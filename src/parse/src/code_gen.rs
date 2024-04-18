#![allow(E0004)]


use std::any::Any;
use std::cmp::PartialEq;
use std::fmt::format;
use lang::types::Type;


use crate::ast::{AssignData, AstNode, ConsData, DefVarData, IfData, ListAccData, LiteralCallData, MultiExprData, OpData, WhileData};
use crate::op_codes::OpCode;
use crate::token::Op;
use crate::{op_codes, util};
use crate::token::Op::PlusPlus;
use crate::util::{CompUnit, SCACHE};


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

    pub fn patch_wide_inst(&mut self, offset: usize, val: u16) {
        self.code[offset] = (val & 0xFF) as u8;
        self.code[offset + 1] = ((val >> 8) & 0xFF) as u8;
    }

    fn empty() -> GenData {
        GenData {
            code: Vec::<u8>::with_capacity(0),
            typ: 0, // 0 = #nil
        }
    }
}


pub fn code_gen(program_nodes: Vec<AstNode>, comp_unit: &mut CompUnit) -> Result<(), String> {
    for node in program_nodes {
        let code = gen_node(node, comp_unit)?;
        comp_unit.push_code_gen(code);
    }
    Ok(())
}


fn gen_node(node: AstNode, mut comp_unit: &mut CompUnit) -> Result<GenData, String> {
    match node {
        AstNode::DefVariable(data) => gen_define_variable(data, comp_unit),
        AstNode::DefLambda(_) => todo!(),
        AstNode::DefFunction(_) => todo!(),
        AstNode::DefStruct(_) => todo!(),
        AstNode::DefClass(_) => todo!(),
        AstNode::ExprAssignment(data) => gen_assignment(data, comp_unit),
        AstNode::ExprMulti(data) => gen_multi_expr(data, comp_unit),
        AstNode::ExprPrint(_) => todo!(),
        AstNode::ExprIf(data) => {
            gen_if_expr(data, comp_unit)
        }
        AstNode::ExprCond(_) => todo!(),
        AstNode::ExprWhileLoop(data) => {
            gen_while_loop(data, comp_unit)
        }
        AstNode::ExprCons(data) => gen_cons(data, comp_unit),
        AstNode::ExprPairList(data) => gen_list_new(data, comp_unit),
        AstNode::ExprArray(data) => gen_array_new(data, comp_unit),
        AstNode::ExprListAccess(data) => gen_list_access(data, comp_unit),
        AstNode::ExprFuncCall(_) => todo!(),
        AstNode::ExprFuncCalInner(_) => todo!(),
        AstNode::ExprObjectCall(_) => todo!(),
        AstNode::ExprLiteralCall(data) => gen_literal_call(data, comp_unit),
        AstNode::ExprObjectAssignment(_) => todo!(),
        AstNode::ExprGenRand(_) => todo!(),
        AstNode::ExprDirectInst(_) => todo!(),
        AstNode::ExprInitInst(_) => todo!(),
        AstNode::Operation(op_data) => {
            gen_operation(op_data, comp_unit)
        }
        AstNode::LitInteger(_) | AstNode::LitFloat(_) | AstNode::LitBoolean(_) => {
            gen_numerical_lit(node, comp_unit)
        }
        AstNode::LitString(_) => todo!(),
        AstNode::LitQuote => todo!(),
        AstNode::LitObject => todo!(),
        AstNode::LitStruct() => todo!(),
        AstNode::LitNil => todo!(),
        AstNode::LitVector => todo!(),
        AstNode::LitPair => todo!(),
        AstNode::LitLambda => todo!(),
    }
}

// TODO push modifiers as well as a byte vec where each byte = mod (allows up to 8 modifiers)

fn gen_define_variable(data: Box<DefVarData>, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let ctx = data.ctx.unwrap();

    let mut code = GenData::empty();
    code.typ = ctx.typ;

    //Is Global
    if ctx.scope < 100 {// todo fix this
        let name_load = gen_name_load(data.name, comp_unit)?;
        let value = gen_node(data.value, comp_unit)?;
        let heap_store = gen_heap_store(ctx.typ, 8_u16)?;
        code.append_gen_data(value); // calc value to stack
        code.append_gen_data(heap_store); // store value on heap, push ref to stack
        code.append_gen_data(name_load); // push name to stack
        code.append_op_code(OpCode::DefGlobal);
    } else {
        todo!()
    }
    Ok(code)
}


fn gen_heap_store(typ: u16, size: u16) -> Result<GenData, String> {
    let mut code = GenData::empty();
    code.typ = typ;
    code.append_op_code(OpCode::HeapStore);
    code.append_wide_operand(typ);
    code.append_wide_operand(size);
    Ok(code)
}


fn gen_literal_call(data: LiteralCallData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
   // let ctx = data.ctx.unwrap(); // FIXME FIXME
    let mut code = GenData::empty();
    code.typ = 0; // TODO, types should be provided with calls as well for generation?
   // if ctx.scope < 100 {// todo fix this
        let name_load = gen_name_load(data.name, comp_unit)?;
        code.append_gen_data(name_load);
        code.append_op_code(OpCode::LoadGlobal);
  //  } else {
    //    todo!()
   // }
    Ok(code)
}


fn gen_assignment(data: Box<AssignData>, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let ctx = data.ctx.unwrap();

    let mut code = GenData::empty();
    code.typ = 0;

    if ctx.scope < 100 {// todo fix this
        let name_load = gen_name_load(data.name, comp_unit)?;
        let value = gen_node(data.value, comp_unit)?;
        let heap_store = gen_heap_store(0, 8)?;
        code.append_gen_data(value);
        code.append_gen_data(heap_store);
        code.append_gen_data(name_load);
        code.append_op_code(OpCode::AssignGlobal);
    } else {
        println!("ctx: {:?}", ctx);
        todo!()
    }
    Ok(code)
}


fn gen_name_load(name: u64, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let mut code = GenData::empty();

    let name_index = if let Some(str) = comp_unit.existing_str.get(&name) {
        *str
    } else {
        let idx = comp_unit.push_constant(&name);
        comp_unit.existing_str.insert(name, idx);
        idx
    };

    if name_index > u8::MAX as u16 {
        code.append_op_code(OpCode::LoadConstW);
        code.append_wide_operand(name_index);
    } else {
        code.append_op_code(OpCode::LoadConst);
        code.append_operand(name_index as u8);
    }
    Ok(code)
}


fn gen_cons(data: Box<ConsData>, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let car_code = gen_node(data.car, comp_unit)?;
   // let car_code = append_heap_store_if_needed(car_code, comp_unit)?;
    let cdr_code = gen_node(data.cdr, comp_unit)?;
   // let cdr_code = append_heap_store_if_needed(cdr_code, comp_unit)?;

    let mut code = GenData::empty();
    code.typ = comp_unit.ctx.types.pair;

    code.append_gen_data(cdr_code);
    code.append_gen_data(car_code);

    code.append_op_code(OpCode::Cons);
    Ok(code)
}


fn gen_list_new(data: OpData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let mut operands = Vec::<GenData>::with_capacity(data.operands.len());
    let mut code = GenData::empty();
    code.typ = comp_unit.ctx.types.pair;

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

fn gen_array_new(data: OpData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let size = data.operands.len();
    if size > u16::MAX as usize {
        return Err("Too many array literals (> 65,535".to_string());
    }
    let mut code = GenData::empty();
    code.typ = comp_unit.ctx.types.get_type_id(&data.typ);

    for op in data.operands {
        let gen_data = gen_node(op, comp_unit)?;
       code.append_gen_data(gen_data);
    }
    
    code.append_op_code(OpCode::NewArray);
    code.append_wide_operand(code.typ);
    code.append_wide_operand(size as u16);
    Ok(code)
}


fn gen_list_access(data: Box<ListAccData>, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let mut code = GenData::empty();
    code.typ = comp_unit.ctx.types.nil;
    
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


fn append_heap_store_if_needed(mut data: GenData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    match comp_unit.ctx.types.get_type_by_id(data.typ) {
        Type::Integer | Type::Float | Type::Boolean => {
            data.append_gen_data(gen_heap_store(data.typ, 8)?);
        }
        Type::Array(_) | Type::String | Type::Pair | Type::Nil
        | Type::Quote | Type::Object(_) | Type::Lambda(_) => {}
        _ => panic!("Need match for type")
    }
    Ok(data)
}


fn gen_multi_expr(mut data: MultiExprData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let mut code = GenData::empty();
    while let Some(expr) = data.expressions.pop() {
        let result = gen_node(expr, comp_unit)?;
        code.typ = result.typ.clone();
        code.append_gen_data(result)
    }
    Ok(code)
}


fn gen_if_expr(data: Box<IfData>, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let mut code = GenData::empty();
    code.typ = comp_unit.ctx.types.get_type_id(&data.if_branch.typ);

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


fn gen_while_loop(data: Box<WhileData>, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let mut code = GenData::empty();
    code.typ = comp_unit.ctx.types.get_type_id(&Type::Boolean);

    let body = gen_node(data.body, comp_unit)?;
    println!("Body: {:?}", body);
    if data.is_do {
        code.append_gen_data(body.clone());
    }
    let loop_start = code.code.len();
    println!("Loop Start{}", loop_start);

    let loop_cond = gen_node(data.condition, comp_unit)?;
    code.append_gen_data(loop_cond);
    let jump_index = emit_jump_empty(OpCode::JumpFalse, &mut code);

    code.append_gen_data(body);
    emit_jump(OpCode::JumpBack, code.code.len() - loop_start, &mut code);
    patch_jump(jump_index, &mut code);
    Ok(code)
}


fn gen_numerical_lit(node: AstNode, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let value = match node {
        AstNode::LitInteger(value) => (comp_unit.push_constant(&value), Type::Integer),
        AstNode::LitFloat(value) => (comp_unit.push_constant(&value), Type::Float),
        AstNode::LitBoolean(value) => (comp_unit.push_constant(&value), Type::Boolean),
        _ => return Err("Fatal: Invalid literal in gen_node".to_string())
    };

    let code = if value.0 > u8::MAX as u16 {
        let bytes = util::get_wide_bytes(value.0 as u16);
        vec![OpCode::LoadConstW as u8, bytes.0, bytes.1]
    } else {
        vec![OpCode::LoadConst as u8, value.0 as u8]
    };

    let data = GenData {
        code,
        typ: comp_unit.ctx.types.get_type_id(&value.1),
    };
    Ok(data)
}


fn gen_operation(data: OpData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let mut operands = Vec::<GenData>::with_capacity(data.operands.len());
    let mut is_float = false;

    for operand in data.operands {
        let gen_data = gen_node(operand, comp_unit)?;
        if gen_data.typ == comp_unit.ctx.types.float {
            is_float = true
        }
        operands.push(gen_data)
    }

    for operand in &mut operands {
        if is_float && operand.typ != comp_unit.ctx.types.float {
            if operand.typ == comp_unit.ctx.types.int || operand.typ == comp_unit.ctx.types.bool {
                operand.append_op_code(OpCode::I64ToF64)
            } else { return Err(format!("Unexpected node type for operation: {:?}", operand.typ).to_string()); }
        }
    }

    let capacity = operands.len() * 2;

    let code = GenData {
        code: Vec::<u8>::with_capacity(capacity),
        typ: if is_float { comp_unit.ctx.types.float } else { comp_unit.ctx.types.int },
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
    gen_data.patch_wide_inst(offset, jump as u16);
}