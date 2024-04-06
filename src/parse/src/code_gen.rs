use std::cmp::PartialEq;
use lang::types::Type;


use crate::ast::{AstNode, OpData};
use crate::environment::Context;
use crate::op_codes::OpCode;
use crate::parser::CompUnit;
use crate::token::Op;
use crate::util;


pub struct GenData {
    pub(crate) code: Vec<u8>,
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
}


pub fn resolve_types(mut program_nodes: &mut Vec<AstNode>, mut context: Context) -> bool {
    let mut fully_resolved = true;
    for _ in 0..2 {
        for node in program_nodes.iter_mut() {
            if resolve(node, &mut context) == Ok(Type::Unresolved) {
                fully_resolved = false;
            }
        }
    }
    fully_resolved
}


fn resolve(node: &mut AstNode, context: &mut Context) -> Result<Type, String> {
    match node {
        AstNode::DefVariable(data) => {
            let resolved_type = resolve(&mut data.value, context)?;
            if resolved_type == Type::Unresolved { return Ok(Type::Unresolved); }

            if let Some(d_type) = data.d_type {
                let resolved_d_type = context.validate_type(d_type);
                if resolved_type == Type::Unresolved {
                    Ok(Type::Unresolved)
                } else {
                    Ok(resolved_d_type)
                }
            } else { Ok(resolved_type) }
        }

        AstNode::DefLambda(_) => todo!(),
        AstNode::DefFunction(_) => todo!(),
        AstNode::DefStruct(_) => todo!(),
        AstNode::DefClass(_) => todo!(),
        AstNode::ExprAssignment(data) => {
            let resolved_type = resolve(&mut data.value, context)?;
            if resolved_type == Type::Unresolved {
                return Ok(Type::Unresolved);
            } else {
                Ok(context.get_symbol_type(data.name))
            }
        }

        AstNode::ExprMulti(data) => {
            context.pushScope();
            let mut resolved_type = Type::Unresolved;
            for expr in data.expressions.iter_mut() {
                resolved_type = resolve(expr, context)?;
            }
            context.popScope();
            Ok(resolved_type)
        }

        AstNode::ExprPrint(data) => {
            resolve(data, context)
        }

        AstNode::ExprIf(data) => {
            // TODO handle resolution when used as assignment or as argument ( multi branches)
            context.pushScope();
            let if_type = resolve(&mut data.if_branch.then_node, context)?;
            context.popScope();

            if let Some(mut els) = data.else_branch {
                context.pushScope();
                resolve(&mut els, context)?;
                context.popScope();
                Ok(Type::Unresolved) // FIXME
            } else {
                Ok(Type::Unresolved) // FIXME
            }
        }
        
        AstNode::ExprCond(data) => todo!(),
        AstNode::ExprWhileLoop(_) => todo!(),
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

        AstNode::Operation(ref mut data) => {
            let mut typ = Type::Integer;
            for op in data.operands.iter_mut() {
                let op_typ = resolve(op, context)?;
                match op_typ {
                    Type::Float => typ = Type::Float,
                    Type::Integer | Type::Boolean => continue,
                    Type::Unresolved => return Ok(Type::Unresolved),
                    _ => return Err("Invalid type in operation expression".to_string()),
                }
            }
            data.typ = typ.clone();
            Ok(typ)
        }

        AstNode::LitInteger(_) => Ok(Type::Integer),
        AstNode::LitFloat(_) => Ok(Type::Float),
        AstNode::LitBoolean(_) => Ok(Type::Boolean),
        AstNode::LitString(_) => Ok(Type::String),
        AstNode::LitQuote => todo!(),
        AstNode::LitObject => todo!(),
        AstNode::LitStruct() => todo!(),
        AstNode::LitNil => Ok(Type::Nil),
        AstNode::LitVector => todo!(),
        AstNode::LitPair => todo!(),
        AstNode::LitLambda => todo!(),
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
        AstNode::DefVariable(_) => todo!(),
        AstNode::DefLambda(_) => todo!(),
        AstNode::DefFunction(_) => todo!(),
        AstNode::DefStruct(_) => todo!(),
        AstNode::DefClass(_) => todo!(),
        AstNode::ExprAssignment(_) => todo!(),
        AstNode::ExprMulti(_) => todo!(),
        AstNode::ExprPrint(_) => todo!(),
        AstNode::ExprIf(_) => todo!(),
        AstNode::ExprCond(_) => todo!(),
        AstNode::ExprWhileLoop(_) => todo!(),
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
        AstNode::Operation(op_data) => gen_operation(op_data, comp_unit),

        AstNode::LitInteger(_) | AstNode::LitFloat(_) | AstNode::LitBoolean(_) => {
            let value = match node {
                AstNode::LitInteger(value) => (comp_unit.push_constant(&value), Type::Integer),
                AstNode::LitFloat(value) => (comp_unit.push_constant(&value), Type::Float),
                AstNode::LitBoolean(value) => (comp_unit.push_constant(&value), Type::Boolean),
                _ => return Err("Fatal: Invalid literal in gen_node".to_string())
            };

            let code = if value.0 > u8::MAX as usize {
                let bytes = util::get_wide_bytes(value.0 as u16);
                vec![OpCode::LdcW as u8, bytes.0, bytes.1]
            } else {
                vec![OpCode::Ldc as u8, value.0 as u8]
            };

            let data = GenData {
                code,
                typ: value.1,
            };
            Ok(data)
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

    for mut operand in &mut operands {
        if is_float && operand.typ != Type::Float {
            if matches!(operand.typ, Type::Integer | Type::Boolean) {
                operand.append_op_code(OpCode::I64ToF64)
            } else { return Err(format!("Unexpected node type for operation: {:?}", operand.typ).to_string()); }
        }
    }

    let capacity = (operands.len() as f32 * 1.5) as usize;

    let mut code = GenData {
        code: Vec::<u8>::with_capacity(capacity),
        typ: if is_float { Type::Float } else { Type::Integer },
    };


    match data.operation {
        Op::List => todo!(),
        Op::And => todo!(),
        Op::Or => todo!(),
        Op::Nor => todo!(),
        Op::Xor => todo!(),
        Op::Xnor => todo!(),
        Op::Nand => todo!(),
        Op::Negate => todo!(),
        Op::Plus | Op::Minus | Op::Asterisk | Op::Slash | Op::Caret | Op::Percent => {
            if operands.len() < 2 {
                return Err("Expected at least 2 operands for arithmetic operation".to_string());
            }

            let op_code = get_arithmetic_op(data.operation, is_float)?;
            let size = operands.len() - 1;

            while let Some(operand) = operands.pop() {
                code.append_gen_data(operand);
            }

            for i in 0..size {
                code.append_op_code(op_code);
            }
            Ok(code)
        }
        Op::PlusPlus => todo!(),
        Op::MinusMinus => todo!(),
        Op::Greater => todo!(),
        Op::Less => todo!(),
        Op::GreaterEqual => todo!(),
        Op::LessEqual => todo!(),
        Op::Equals => todo!(),
        Op::BangEquals => todo!(),
        Op::RefEqual => todo!(),
    }
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