#![allow(E0004)]


use std::cmp::PartialEq;
use std::fmt::format;
use lang::types::Type;


use crate::ast::{AstNode, DefVarData, IfData, OpData, WhileData};
use crate::environment::Context;
use crate::op_codes::OpCode;
use crate::token::Op;
use crate::{op_codes, util};
use crate::util::{CompUnit, SCACHE};


#[derive(Clone, Debug, PartialEq)]
pub struct GenData {
    pub code: Vec<u8>,
    typ: Type,
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

    pub fn append_wide_inst(&mut self, val: u16) {
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
            typ: Type::Unresolved,
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
        AstNode::DefVariable(data) => todo!(),
        AstNode::DefLambda(_) => todo!(),
        AstNode::DefFunction(_) => todo!(),
        AstNode::DefStruct(_) => todo!(),
        AstNode::DefClass(_) => todo!(),
        AstNode::ExprAssignment(_) => todo!(),
        AstNode::ExprMulti(_) => todo!(),
        AstNode::ExprPrint(_) => todo!(),
        AstNode::ExprIf(data) => {
            gen_if_expr(data, comp_unit)
        }
        AstNode::ExprCond(_) => todo!(),
        AstNode::ExprWhileLoop(data) => {
            gen_while_loop(data, comp_unit)
        }
        AstNode::ExprCons(_) => todo!(),
        AstNode::ExprPairList(_) => todo!(),
        AstNode::ExprListAccess(_) => todo!(),
        AstNode::ExprFuncCall(_) => todo!(),
        AstNode::ExprFuncCalInner(_) => todo!(),
        AstNode::ExprObjectCall(_) => todo!(),
        AstNode::ExprLiteralCall(_) => todo!(),
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


fn gen_define_variable(data: Box<DefVarData>, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let ctx = data.ctx.unwrap();
    
    todo!()
    // // Is Global
    // if ctx.scope == 0 {
    //     let name_index = if let Some(spur) = comp_unit.existing_spurs.get(data.name) {
    //         *spur
    //     } else{
    //         comp_unit.push_constant(data.name)
    //     };
    // }
    // Err()
}


fn gen_if_expr(data: Box<IfData>, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let mut code = GenData::empty();
    code.typ = data.if_branch.typ;

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
    code.typ = Type::Boolean;

    let body = gen_node(data.body, comp_unit)?;
    if data.is_do {
        code.append_gen_data(body.clone());
    }
    let loop_start = code.code.len();

    let loop_cond = gen_node(data.condition, comp_unit)?;
    code.append_gen_data(loop_cond);
    let jump_index = emit_jump_empty(OpCode::JumpFalse, &mut code);

    code.append_gen_data(body);
    emit_jump(OpCode::JumpBack, comp_unit.code.len() - loop_start, &mut code);
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

    let code = if value.0 > u8::MAX as usize {
        let bytes = util::get_wide_bytes(value.0 as u16);
        vec![OpCode::LoadConstW as u8, bytes.0, bytes.1]
    } else {
        vec![OpCode::LoadConst as u8, value.0 as u8]
    };

    let data = GenData {
        code,
        typ: value.1,
    };
    Ok(data)
}


fn gen_operation(data: OpData, comp_unit: &mut CompUnit) -> Result<GenData, String> {
    let mut operands = Vec::<GenData>::with_capacity(data.operands.len());
    let mut is_float = false;

    for operand in data.operands {
        let gen_data = gen_node(operand, comp_unit)?;
        if gen_data.typ == Type::Float {
            is_float = true
        }
        operands.push(gen_data)
    }

    for operand in &mut operands {
        if is_float && operand.typ != Type::Float {
            if matches!(operand.typ, Type::Integer | Type::Boolean) {
                operand.append_op_code(OpCode::I64ToF64)
            } else { return Err(format!("Unexpected node type for operation: {:?}", operand.typ).to_string()); }
        }
    }

    let capacity = operands.len() * 2;

    let code = GenData {
        code: Vec::<u8>::with_capacity(capacity),
        typ: if is_float { Type::Float } else { Type::Integer },
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
    gen_data.append_wide_inst(0);
    gen_data.code.len() - 2
}


fn emit_jump(op_code: OpCode, offset: usize, gen_data: &mut GenData) {
    gen_data.append_op_code(op_code);
    if offset > u16::MAX as usize {
        panic!("Too large of jump encountered (> 65,535 instructions)")
    }
    gen_data.append_wide_inst(offset as u16);
}


fn patch_jump(offset: usize, gen_data: &mut GenData) {
    let jump = gen_data.code.len() - offset - 2;
    if jump > u16::MAX as usize {
        panic!("Too large of jump encountered (> 65,535 instructions)")
    }
    gen_data.patch_wide_inst(offset, jump as u16);
}