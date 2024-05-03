use std::cmp::PartialEq;
use std::fmt::format;
use std::ops::Deref;
use lang::format_error;
use lang::util::SCACHE;
use crate::ast::{AssignData, AstData, AstNode, ConsData, DefFuncData, DefVarData, FuncCallData, IfData, InnerFuncCallData, ListAccData, LiteralCallData, MultiExprData, OpData, WhileData};
use crate::environment::{Context, MetaSpace, ResData};
use crate::op_codes::{decode, OpCode};
use crate::token::Op;
use crate::types::Type;


#[derive(Debug)]
pub struct CompUnit<'a> {
    pub meta_space: &'a mut MetaSpace,
    pub curr_ns: u16,
    pub ns_code: Vec<u8>,
}

// TODO: right now code outside of functions allocates/access global vars for the namespace
//  with out any scope checks

impl<'a> CompUnit<'a> {
    pub fn push_op_code(&mut self, op: OpCode) {
        self.ns_code.push(op as u8)
    }

    // pub fn push_operand(&mut self, val: u8) {
    //     self.ns_code.push(val);
    // }
    // 
    // pub fn push_wide_inst(&mut self, val: u16) {
    //     self.ns_code.push((val & 0xFF) as u8);
    //     self.ns_code.push(((val >> 8) & 0xFF) as u8);
    // }

    pub fn push_code_gen(&mut self, mut other: GenData) {
        self.ns_code.append(&mut other.code);
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct GenData {
    pub code: Vec<u8>,
    typ: u16,
}


impl GenData {
    fn append_op_code(&mut self, op: OpCode) {
        self.code.push(op as u8)
    }

    fn append_operand(&mut self, operand: u8) {
        self.code.push(operand)
    }

    fn append_gen_data(&mut self, mut other: GenData) {
        self.code.append(&mut other.code);
    }

    pub fn append_wide_operand(&mut self, val: u16) {
        self.code.push((val & 0xFF) as u8);
        self.code.push(((val >> 8) & 0xFF) as u8);
    }

    pub fn patch_wide_operand(&mut self, offset: usize, val: u16) {
        self.code[offset] = (val & 0xFF) as u8;
        self.code[offset + 1] = ((val >> 8) & 0xFF) as u8;
    }

    fn new(typ: u16) -> GenData {
        GenData {
            code: Vec::<u8>::with_capacity(0),
            typ,
        }
    }
}


// Code that is not in the body of a class or function will be returned to here to be added to the
// top level name space code.
// TODO should top level code even be allowed other than definitions, other than in the main ns?
pub fn code_gen(program_nodes: Vec<AstNode>, comp_unit: &mut CompUnit) -> Result<(), String> {
    for node in program_nodes {
        let line_char = node.line_char;

        match gen_node(node, comp_unit) {
            Ok(code) => { comp_unit.push_code_gen(code); }
            Err(err) => { println!("{}", format_error!(line_char, err)); }
        }
    }
    let mut ns = comp_unit.meta_space.get_ns_by_id(comp_unit.curr_ns);
    println!("Comp unit code: {:?}", decode(&comp_unit.ns_code));
    ns.code.append(&mut comp_unit.ns_code);
    println!("Namespace code: {:?}", decode(&ns.code));
    Ok(())
}


fn gen_node(node: AstNode, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let res_data = if let Some(res_data) = node.res_data {
        res_data
    } else {
        return Err(format!("Fatal: Missing res data for resolved node {:?}", node));
    };
    match *node.node_data {
        AstData::DefVariable(data) => gen_define_variable(data, res_data, comp_unit),
        AstData::DefLambda(_) => todo!(),
        AstData::DefFunction(data) => gen_define_func(data, res_data, comp_unit),
        AstData::DefStruct(_) => todo!(),
        AstData::DefClass(_) => todo!(),
        AstData::ExprAssignment(data) => gen_assignment(data, res_data, comp_unit),
        AstData::ExprMulti(data) => gen_multi_expr(data, res_data, comp_unit),
        AstData::ExprPrint(_) => todo!(),
        AstData::ExprIf(data) => gen_if_expr(data, res_data, comp_unit),
        AstData::ExprCond(_) => todo!(),
        AstData::ExprWhileLoop(data) => gen_while_loop(data, res_data, comp_unit),
        AstData::ExprCons(data) => gen_cons(data, res_data, comp_unit),
        AstData::ExprPairList(data) => gen_list_new(data, res_data, comp_unit),
        AstData::ExprArray(data) => gen_array_new(data, res_data, comp_unit),
        AstData::ExprListAccess(data) => gen_list_access(data, res_data, comp_unit),
        AstData::ExprFuncCall(data) => gen_func_call(data, res_data, comp_unit),
        AstData::ExprFuncCalInner(_) => todo!(),
        AstData::ExprObjectCall(_) => todo!(),
        AstData::ExprLiteralCall(data) => gen_literal_call(data, res_data, comp_unit),
        AstData::ExprObjectAssignment(_) => todo!(),
        AstData::ExprGenRand(_) => todo!(),
        AstData::ExprDirectInst(_) => todo!(),
        AstData::ExprInitInst(_) => todo!(),
        AstData::Operation(data) => gen_operation(data, res_data, comp_unit),
        AstData::LitInteger(data) => gen_numerical_lit(node.node_data, res_data, comp_unit),
        AstData::LitFloat(data) => gen_numerical_lit(node.node_data, res_data, comp_unit),
        AstData::LitBoolean(data) => gen_numerical_lit(node.node_data, res_data, comp_unit),
        AstData::LitString(_) => todo!(),
        AstData::LitQuote => todo!(),
        AstData::LitObject => todo!(),
        AstData::LitStruct() => todo!(),
        AstData::LitNil => todo!(),
        AstData::LitArray => todo!(),
        AstData::LitPair => todo!(),
        AstData::LitLambda => todo!(),
    }
}

// TODO push modifiers as well as a byte vec where each byte = mod (allows up to 8 modifiers)?

fn gen_define_variable(data: DefVarData, res_data: ResData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let ctx = if let Context::Symbol(symbol) = &res_data.self_ctx {
        symbol
    } else { return Err("Fatal: Invalid codegen context for variable definition expected symbol.".to_string()); };

    let mut code = GenData::new(res_data.type_data.type_id);
    let value = gen_node(data.value, comp_unit)?;
    code.append_gen_data(value);

    if ctx.class.is_none() && ctx.func.is_none() {
        code.append_op_code(OpCode::StoreVarN);
        code.append_wide_operand(ctx.ns);
    } else if ctx.func.is_some() {
        code.append_op_code(OpCode::StoreVarL)
    } else if ctx.class.is_some() {
        todo!();
    } else {
        return Err("Fatal: Invalid codegen resolution data for variable definition".to_string());
    }

    code.append_wide_operand(ctx.index);

    Ok(code)
}


fn gen_define_func(data: DefFuncData, res_data: ResData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let ctx = if let Context::Symbol(symbol) = &res_data.self_ctx {
        symbol
    } else { return Err("Fatal: Invalid codegen context for variable definition expected symbol.".to_string()); };

    let mut code = gen_node(data.lambda.body, comp_unit)?;
    println!("Func ctx: {:?}", ctx);
    code.append_op_code(if ctx.rtn_val { OpCode::ReturnVal } else { OpCode::Return });


    comp_unit.meta_space.push_func_code(ctx, &mut code.code);

    let nil_code = GenData::new(code.typ);
    Ok(nil_code)
}


fn gen_heap_store(typ: u16, size: u16) -> Result<GenData, String> {
    todo!("Unneeded?")
    // let mut code = GenData::new();
    // code.typ = typ;
    // code.append_op_code(OpCode::HeapStore);
    // code.append_wide_operand(typ);
    // code.append_wide_operand(size);
    // Ok(code)
}


fn gen_literal_call(data: LiteralCallData, res_data: ResData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let self_ctx = if let Context::Expr(expr) = &res_data.self_ctx {
        expr
    } else { return Err(format!("Fatal: Missing self context for literal call: {:?}", SCACHE.resolve(data.name))); };

    let target_ctx = if let Some(Context::Symbol(symbol)) = &res_data.target_ctx {
        symbol
    } else { return Err(format!("Fatal:  Missing target context for literal call: {:?}", SCACHE.resolve(data.name))); };

    let mut code = GenData::new(res_data.type_data.type_id);


    if self_ctx.class.is_none() && self_ctx.func.is_none() {
        code.append_op_code(OpCode::LoadVarN);
        code.append_wide_operand(self_ctx.ns);
    } else if self_ctx.func.is_some() {
        code.append_op_code(OpCode::LoadVarL)
    } else if self_ctx.class.is_some() {
        todo!();
    } else {
        return Err(format!("Invalid resolution for literal call: {:?}", SCACHE.resolve(data.name)));
    }

    code.append_wide_operand(target_ctx.index);
    Ok(code)
}


fn gen_assignment(data: AssignData, res_data: ResData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let self_ctx = if let Context::Expr(expr) = &res_data.self_ctx {
        expr
    } else { return Err(format!("Fatal: Missing self context for assignment: {:?}", SCACHE.resolve(data.name))); };

    let target_ctx = if let Some(Context::Symbol(symbol)) = &res_data.target_ctx {
        symbol
    } else { return Err(format!("Fatal: Missing target context for assignment: {:?}", SCACHE.resolve(data.name))); };

    let mut code = GenData::new(res_data.type_data.type_id);
    let value = gen_node(data.value, comp_unit)?;
    code.append_gen_data(value);

    if self_ctx.class.is_none() && self_ctx.func.is_none() {
        code.append_op_code(OpCode::StoreVarN);
        code.append_wide_operand(self_ctx.ns);
    } else if self_ctx.func.is_some() {
        code.append_op_code(OpCode::StoreVarL)
    } else if self_ctx.class.is_some() {
        todo!();
    } else {
        return Err(format!("Invalid resolution for assignment: {:?}", SCACHE.resolve(data.name)));
    }

    code.append_wide_operand(target_ctx.index);

    Ok(code)
}


// fn gen_name_load(name: u64, comp_unit: &mut CompUnit) -> Result<GenData, String> {
//     let mut code = GenData::new();
//
//     let name_index = if let Some(str) = comp_unit.existing_str.get(&name) {
//         *str
//     } else {
//         let idx = comp_unit.push_constant(&name);
//         comp_unit.existing_str.insert(name, idx);
//         idx
//     };
//
//     if name_index > u8::MAX as u16 {
//         code.append_op_code(OpCode::LoadConstW);
//         code.append_wide_operand(name_index);
//     } else {
//         code.append_op_code(OpCode::LoadConst);
//         code.append_operand(name_index as u8);
//     }
//     Ok(code)
// }

fn gen_cons(data: ConsData, res_data: ResData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let car_code = gen_node(data.car, comp_unit)?;
    // let car_code = append_heap_store_if_needed(car_code, comp_unit)?;
    let cdr_code = gen_node(data.cdr, comp_unit)?;
    // let cdr_code = append_heap_store_if_needed(cdr_code, comp_unit)?;

    let mut code = GenData::new(comp_unit.meta_space.types.pair);

    code.append_gen_data(cdr_code);
    code.append_gen_data(car_code);
    code.append_op_code(OpCode::Cons);
    Ok(code)
}


fn gen_list_new(data: OpData, res_data: ResData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let mut operands = Vec::<GenData>::with_capacity(data.operands.len());
    let mut code = GenData::new(comp_unit.meta_space.types.pair);

    for op in data.operands {
        let gen_data = gen_node(op, comp_unit)?;
        operands.push(gen_data);
        //operands.push(append_heap_store_if_needed(gen_data, comp_unit)?);
    }

    code.append_gen_data(operands.pop().unwrap());
    code.append_op_code(OpCode::IConst0); // 0 == nil ref index
    code.append_op_code(OpCode::Cons);

    while let Some(next) = operands.pop() {
        code.append_gen_data(next);
        code.append_op_code(OpCode::Cons);
    }
    Ok(code)
}


fn gen_array_new(data: OpData, res_data: ResData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let size = data.operands.len();
    if size > u16::MAX as usize {
        return Err("Too many array literals (> 65,535".to_string());
    }


    // let ctx = if let Context::Symbol(symbol) = &res_data.self_ctx {
    //     symbol
    // } else { return Err("Invalid/Missing context for instancing array".to_string()); };

    let mut code = GenData::new(res_data.type_data.type_id);

    for op in data.operands {
        let gen_data = gen_node(op, comp_unit)?;
        code.append_gen_data(gen_data);
    }

    code.append_op_code(OpCode::NewArray);
    code.append_wide_operand(code.typ);
    code.append_wide_operand(size as u16);
    Ok(code)
}


fn gen_list_access(data: ListAccData, res_data: ResData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let mut code = GenData::new(res_data.type_data.type_id);

    let list_code = gen_node(data.list, comp_unit)?;
    code.append_gen_data(list_code);

    if let Some(pattern) = data.pattern {
        for acc in pattern {
            code.append_op_code(if acc == 0 { OpCode::Car } else { OpCode::Cdr })
        }
    } else if let Some(index) = data.index_expr {
        let index_code = gen_node(index, comp_unit)?;
        code.append_gen_data(index_code);
        code.append_op_code(OpCode::Aacc);
    }
    Ok(code)
}


fn gen_inner_func_call(data: InnerFuncCallData, res_data: ResData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    todo!()
}


fn gen_func_call(data: FuncCallData, res_data: ResData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let self_ctx = if let Context::Expr(ctx) = res_data.self_ctx {
        ctx
    } else { return Err("Fatal: Missing self context for function call".to_string()); };

    let target_ctx = if let Some(Context::Symbol(target)) = res_data.target_ctx {
        target
    } else { return Err("Fatal: Missing target context for function call".to_string()); };

    let (param_types, rtn_type) = {
        let func_meta = comp_unit.meta_space.get_func(&target_ctx);
        (func_meta.param_types.clone(), func_meta.rtn_type)
    };

    let mut code = GenData::new(rtn_type);

    if param_types.len() > 0 {
        if let Some(args) = data.arguments {
            if args.len() != param_types.len() {
                return Err(format!("Invalid argument count for function call: {:?} | Found: {}, Expected: {}",
                    SCACHE.resolve(data.name), args.len(), param_types.len()));
            }

            for (arg, param_type) in args.into_iter().zip(param_types.iter()) {
                let resolved_arg = gen_node(arg.value, comp_unit)?;
                if resolved_arg.typ != *param_type {
                    return Err(format!("Invalid argument type for function call: {:?} |Found: {:?}, Expected: {:?}",
                        SCACHE.resolve(data.name),
                        comp_unit.meta_space.types.get_type_by_id(resolved_arg.typ),
                        comp_unit.meta_space.types.get_type_by_id(*param_type)
                    ));
                }
                code.append_gen_data(resolved_arg);
            }
        } else {
            return Err(format!("Invalid argument count for function call: {:?} | Found: 0, Expected: {}",
                SCACHE.resolve(data.name), param_types.len()));
        }
    }

    if self_ctx.class.is_some() {
        todo!("Class not implemented")
    } else {
        code.append_op_code(OpCode::InvokeN);
        code.append_wide_operand(target_ctx.ns);
        code.append_wide_operand(target_ctx.index);
    }
    Ok(code)
}


// fn append_heap_store_if_needed(mut data: GenData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
//     match comp_unit.meta_space.types.get_type_by_id(data.typ) {
//         Type::Integer | Type::Float | Type::Boolean => {
//             data.append_gen_data(gen_heap_store(data.typ, 8)?);
//         }
//         Type::Array(_) | Type::String | Type::Pair | Type::Nil
//         | Type::Quote | Type::Object(_) | Type::Lambda(_) => {}
//         _ => panic!("Need match for type")
//     }
//     Ok(data)
// }
//

fn gen_multi_expr(mut data: MultiExprData, res_data: ResData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let mut code = GenData::new(res_data.type_data.type_id);
    while let Some(expr) = data.expressions.pop() {
        let result = gen_node(expr, comp_unit)?;
        code.append_gen_data(result)
    }
    Ok(code)
}


fn gen_if_expr(data: IfData, res_data: ResData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let mut code = GenData::new(res_data.type_data.type_id);

    let cond_data = gen_node(data.if_branch.cond_node, comp_unit)?;
    code.append_gen_data(cond_data);

    let then_jump = emit_jump_empty(OpCode::JumpFalse, &mut code);
    let then_data = gen_node(data.if_branch.then_node, comp_unit)?;
    code.append_gen_data(then_data);


    let else_jump = emit_jump_empty(OpCode::JumpFWd, &mut code);
    patch_jump(then_jump, &mut code);

    if let Some(els) = data.else_branch {
        let else_data = gen_node(els, comp_unit)?;
        code.append_gen_data(else_data);
        patch_jump(else_jump, &mut code);
    }

    Ok(code)
}


fn gen_while_loop(data: WhileData, res_data: ResData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let mut code = GenData::new(res_data.type_data.type_id);

    let body = gen_node(data.body, comp_unit)?;
    if data.is_do {
        code.append_gen_data(body.clone());
    }
    let loop_start = code.code.len();
    let loop_cond = gen_node(data.condition, comp_unit)?;
    code.append_gen_data(loop_cond);
    let jump_index = emit_jump_empty(OpCode::JumpFalse, &mut code);

    code.append_gen_data(body);
    emit_jump(OpCode::JumpBack, code.code.len() - loop_start, &mut code);
    patch_jump(jump_index, &mut code);
    Ok(code)
}


fn gen_numerical_lit(node: Box<AstData>, res_data: ResData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let ctx = if let Context::Expr(expr) = &res_data.self_ctx {
        expr
    } else { return Err("Invalid context".to_string()); };


    let value = match *node {
        AstData::LitInteger(value) => (comp_unit.meta_space.add_constant(&value, ctx), Type::Integer),
        AstData::LitFloat(value) => (comp_unit.meta_space.add_constant(&value, ctx), Type::Float),
        AstData::LitBoolean(value) => (comp_unit.meta_space.add_constant(&value, ctx), Type::Boolean),
        _ => return Err("Fatal: Invalid literal in gen_node".to_string())
    };


    let mut code = GenData::new(comp_unit.meta_space.types.get_type_id(&value.1));

    if ctx.class.is_none() && ctx.func.is_none() {
        if value.0 < u8::MAX as u16 {
            code.append_op_code(OpCode::LoadConstN);
            code.append_wide_operand(ctx.ns);
            code.append_operand(value.0 as u8);
        } else {
            code.append_op_code(OpCode::LoadConstNWide);
            code.append_wide_operand(ctx.ns);
            code.append_wide_operand(value.0);
        }
    } else if ctx.func.is_some() {
        if value.0 < u8::MAX as u16 {
            code.append_op_code(OpCode::LoadConstL);
            code.append_operand(value.0 as u8);
        } else {
            code.append_op_code(OpCode::LoadConstLWide);
            code.append_wide_operand(value.0);
        }
    } else if ctx.class.is_some() {
        todo!();
    } else {
        return Err("Fatal: Invalid resolution data passed to codegen".to_string());
    }
    Ok(code)
}


fn gen_operation(data: OpData, res_data: ResData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let mut operands = Vec::<GenData>::with_capacity(data.operands.len());
    let mut is_float = false;

    for operand in data.operands {
        let gen_data = gen_node(operand, comp_unit)?;
        if gen_data.typ == comp_unit.meta_space.types.float {
            is_float = true
        }
        operands.push(gen_data)
    }

    for operand in &mut operands {
        if is_float && operand.typ != comp_unit.meta_space.types.float {
            if operand.typ == comp_unit.meta_space.types.int || operand.typ == comp_unit.meta_space.types.bool {
                operand.append_op_code(OpCode::I64ToF64)
            } else { return Err(format!("Unexpected node type for operation: {:?}", operand.typ).to_string()); }
        }
    }

    let capacity = operands.len() * 2;

    let code = GenData {
        code: Vec::<u8>::with_capacity(capacity),
        typ: if is_float { comp_unit.meta_space.types.float } else { comp_unit.meta_space.types.int },
    };

    match &data.operation {
        Op::List => todo!(),
        Op::And | Op::Or | Op::Nor | Op::Xor | Op::Nand | Op::Xnor => {
            gen_boolean_logic(data.operation, operands, code)
        }

        Op::Negate => {
            gen_boolean_negate(operands, code)
        }

        Op::Plus | Op::Minus | Op::Asterisk | Op::Slash | Op::Caret | Op::Percent => {
            gen_arithmetic(data.operation, operands, code, is_float)
        }

        Op::PlusPlus | Op::MinusMinus => {
            gen_inc_dec(data.operation, operands, code, is_float)
        }

        Op::Greater | Op::Less | Op::GreaterEqual | Op::LessEqual | Op::Equals => {
            gen_comparison(data.operation, operands, code, is_float)
        }

        Op::BangEquals | Op::RefEqual => todo!()
    }
}


fn gen_boolean_logic(operation: Op, mut operands: Vec<GenData>, mut code: GenData,
) -> Result<GenData, String> {
    if operands.len() > 255 { return Err("Unsupported operand amount (> 255)".to_string()); }

    // This reverses the operands for proper order on stack removal
    let size = operands.len();
    while let Some(operand) = operands.pop() {
        code.append_gen_data(operand);
    }

    match operation {
        Op::And | Op::Nand => code.append_op_code(OpCode::LogicAnd),
        Op::Or | Op::Nor => code.append_op_code(OpCode::LogicOr),
        Op::Xor | Op::Xnor => code.append_op_code(OpCode::LogicOr),
        _ => panic!("Fatal: Wrong logic operation")
    };
    code.append_operand(size as u8);

    if operation == Op::Nand || operation == Op::Nor || operation == Op::Xnor {
        code.append_op_code(OpCode::LogicNegate);
    }
    Ok(code)
}


fn gen_boolean_negate(mut operands: Vec<GenData>, mut code: GenData,
) -> Result<GenData, String> {
    if operands.len() != 1 { return Err("Negation is a unary operation".to_string()); }

    if let Some(operand) = operands.pop() {
        code.append_gen_data(operand);
    }
    code.append_op_code(OpCode::LogicNegate);
    Ok(code)
}


fn gen_arithmetic(operation: Op, mut operands: Vec<GenData>, mut code: GenData, is_float: bool,
) -> Result<GenData, String> {
    if operands.len() < 2 { return Err("Expected at least 2 operands for arithmetic operation".to_string()); }

    let op_code = get_arithmetic_op(operation, is_float)?;
    let size = operands.len() - 1;


    while let Some(operand) = operands.pop() {
        code.append_gen_data(operand);
    }

    for _ in 0..size {
        code.append_op_code(op_code);
    }


    Ok(code)
}


fn get_arithmetic_op(op: Op, is_float: bool) -> Result<OpCode, String> {
    match op {
        Op::Plus => if is_float { Ok(OpCode::AddF64) } else { Ok(OpCode::AddI64) },
        Op::Minus => if is_float { Ok(OpCode::SubF64) } else { Ok(OpCode::SubI64) }
        Op::Asterisk => if is_float { Ok(OpCode::MulF64) } else { Ok(OpCode::MulI64) }
        Op::Slash => if is_float { Ok(OpCode::DivF64) } else { Ok(OpCode::DivI64) }
        Op::Caret => if is_float { Ok(OpCode::PowF64) } else { Ok(OpCode::PowI64) }
        Op::Percent => if is_float { Ok(OpCode::ModF64) } else { Ok(OpCode::ModI64) }
        _ => Err("Fatal: Invalid call to get_arithmetic_op".to_string())
    }
}


fn gen_inc_dec(operation: Op, mut operands: Vec<GenData>, mut code: GenData, is_float: bool,
) -> Result<GenData, String> {
    if operands.len() != 1 { return Err("Inc/Dec is a unary operation".to_string()); }

    if let Some(operand) = operands.pop() {
        code.append_gen_data(operand);
    }

    code.append_op_code(if operation == Op::PlusPlus { OpCode::IConst1 } else { OpCode::IConstM1 });
    code.append_op_code(if is_float { OpCode::AddF64 } else { OpCode::AddI64 });
    Ok(code)
}


fn gen_comparison(operation: Op, mut operands: Vec<GenData>, mut code: GenData, is_float: bool,
) -> Result<GenData, String> {
    if operands.len() < 2 { return Err("Expected at least 2 operands for comparison operation".to_string()); }
    if operands.len() > 255 { return Err("Unsupported operand amount (> 255)".to_string()); }

    let size = operands.len() as u8;
    // This reverses the operands to proper order on stack
    while let Some(operand) = operands.pop() {
        code.append_gen_data(operand);
    }

    if is_float {
        if size > 2 {
            code.append_op_code(OpCode::CompF64N);
            code.append_operand(size);
        } else {
            code.append_op_code(OpCode::CompF64);
        }
    } else if size > 2 {
        code.append_op_code(OpCode::CompI64N);
        code.append_operand(size);
    } else {
        code.append_op_code(OpCode::CompI64);
    }

    let comp_op = match &operation {
        Op::Greater => if size > 2 { OpCode::CompGtN } else { OpCode::CompGt }
        Op::Less => if size > 2 { OpCode::CompLtN } else { OpCode::CompLt }
        Op::GreaterEqual => if size > 2 { OpCode::CompGtEqN } else { OpCode::CompGtEq }
        Op::LessEqual => if size > 2 { OpCode::CompLtEqN } else { OpCode::CompLtEq }
        Op::Equals => if size > 2 { OpCode::CompEqN } else { OpCode::CompEq }
        _ => panic!("Fatal: Wrong comparison operation")
    };

    code.append_op_code(comp_op);
    if size > 2 { code.append_operand(size) }
    Ok(code)
}


fn emit_jump_empty(op_code: OpCode, gen_data: &mut GenData) -> usize {
    gen_data.append_op_code(op_code);
    gen_data.append_wide_operand(0);
    gen_data.code.len() - 2
}


fn emit_jump(op_code: OpCode, offset: usize, gen_data: &mut GenData) {
    gen_data.append_op_code(op_code);
    if offset > u16::MAX as usize {
        panic!("Too large of jump encountered (> 65,535 instructions)")
    }
    // + 3 to include op_code(1 byte) + wide operand (2 bytes)
    gen_data.append_wide_operand((offset + 3) as u16);
}


fn patch_jump(offset: usize, gen_data: &mut GenData) {
    let jump = gen_data.code.len() - offset - 2;
    if jump > u16::MAX as usize {
        panic!("Too large of jump encountered (> 65,535 instructions)")
    }
    gen_data.patch_wide_operand(offset, jump as u16);
}