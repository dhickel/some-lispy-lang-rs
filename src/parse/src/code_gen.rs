#![allow(E0004)]


use std::cmp::PartialEq;
use std::fmt::format;
use lang::types::Type;


use crate::ast::{AstNode, OpData};
use crate::environment::Context;
use crate::op_codes::OpCode;
use crate::parser::CompUnit;
use crate::token::Op;
use crate::{op_codes, util};
use crate::util::SCACHE;


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
            typ: Type::Unresolved
        }
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

        AstNode::DefLambda(data) => {
            let resolved_type = resolve(&mut data.body, context)?;
            if let Some(d_type) = data.d_type {
                if resolved_type != context.validate_type(d_type) {
                    return Err("Declared type does not match actual type for lambda".to_string());
                }
            }
            Ok(resolved_type)
        }
        AstNode::DefFunction(_) => todo!(),
        AstNode::DefStruct(_) => todo!(),
        AstNode::DefClass(_) => todo!(),
        AstNode::ExprAssignment(data) => {
            let resolved_type = resolve(&mut data.value, context)?;
            if resolved_type == Type::Unresolved {
                return Ok(Type::Unresolved);
            } else {
                match &data.value {
                    AstNode::ExprIf(expr) => {
                        if !expr.all_types_same() {
                            Err("All branches in assignment must have same type".to_string())
                        } else {
                            let var_type = context.get_symbol_type(data.name);
                            if var_type == &resolved_type {
                                Ok(resolved_type)
                            } else {
                                Err(format!(
                                    "Attempted to assign incorrect type for var: {}",
                                    SCACHE.resolve(&data.name)))
                            }
                        }
                    }
                    AstNode::ExprCond(expr) => {
                        if !expr.all_types_same() {
                            Err("All branches in assignment must have same type".to_string())
                        } else {
                            let var_type = context.get_symbol_type(data.name);
                            if var_type == &resolved_type {
                                Ok(resolved_type)
                            } else {
                                Err(format!(
                                    "Attempted to assign incorrect type for var: {}",
                                    SCACHE.resolve(&data.name)))
                            }
                        }
                    }
                    _ => Ok(context.get_symbol_type(data.name).clone())
                }
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
            context.pushScope();

            // let cond_type = resolve(&mut data.if_branch.cond_node, context)?;
            // if cond_type != Type::Unresolved {
            //     
            // }

            let if_type = resolve(&mut data.if_branch.then_node, context)?;
            if if_type != Type::Unresolved {
                data.if_branch.typ = if_type.clone();
            }
            context.popScope();

            if let Some(ref mut els) = data.else_branch {
                context.pushScope();
                let else_type = resolve(els, context)?;
                if else_type != Type::Unresolved {
                    data.else_type = else_type.clone();
                }
                context.popScope();

                if if_type != Type::Unresolved && else_type != Type::Unresolved {
                    Ok(else_type) // Fixme, find a better way to represent conditional types as they vary
                } else { Ok(Type::Unresolved) }
            } else { Ok(if_type) }
        }

        AstNode::ExprCond(data) => {
            let mut unresolved = false;
            let mut cond_type = Type::Unresolved;
            for branch in data.cond_branches.iter_mut() {
                context.pushScope();
                let branch_type = resolve(&mut branch.then_node, context)?;

                if branch_type != Type::Unresolved {
                    branch.typ = branch_type.clone();
                } else { unresolved = true; }

                cond_type = branch_type;
                context.popScope();
            }

            if let Some(ref mut els) = data.else_branch {
                context.pushScope();
                let else_type = resolve(els, context)?;
                if else_type != Type::Unresolved {
                    data.else_type = else_type.clone();
                }
                context.popScope();

                if !unresolved && else_type != Type::Unresolved {
                    Ok(else_type) // Fixme, find a better way to represent conditional types as they vary
                } else {
                    Ok(Type::Unresolved)
                }
            } else if unresolved {
                Ok(Type::Unresolved)
            } else {
                Ok(cond_type)
            }
        }

        AstNode::ExprWhileLoop(_) => Ok(Type::Boolean), //TODO handle return assignment
        AstNode::ExprCons(_) => Ok(Type::Pair),
        AstNode::ExprPairList(_) => Ok(Type::Pair),
        AstNode::ExprListAccess(data) => Ok(Type::Pair), // TODO this will need to be runtime checked
        AstNode::ExprFuncCall(data) => Ok(context.get_symbol_type(data.name).clone()),
        AstNode::ExprFuncCalInner(data) => Ok(resolve(&mut data.expr, context)?),
        AstNode::ExprObjectCall(data) => todo!(), // TODO will need to resolve accessor to their type
        AstNode::ExprLiteralCall(_) => todo!(), // TODO will need to resolve accessor to their type
        AstNode::ExprObjectAssignment(_) => todo!(), // TODO will need to resolve accessor to their type
        AstNode::ExprGenRand(data) => if data.is_float { Ok(Type::Float) } else { Ok(Type::Integer) },
        AstNode::ExprDirectInst(data) => Ok(context.get_symbol_type(data.name).clone()),
        AstNode::ExprInitInst(data) => Ok(context.get_symbol_type(data.name).clone()),

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
        AstNode::ExprIf(data) => {
     
            let mut code = GenData::empty();
            code.typ = data.if_branch.typ;
            
            let cond_data = gen_node(data.if_branch.cond_node, comp_unit)?;
            // if cond_data.typ != Type::Boolean {
            //     return Err(format!(
            //         "Condition for if expression must evaluate to boolean, found: {:?}",
            //         cond_data.typ));
            // }

            code.append_gen_data(cond_data);

            let then_jump = emit_jump_inst(OpCode::JumpFalse, &mut code);
            
            let then_data = gen_node(data.if_branch.then_node, comp_unit)?;
            code.append_gen_data(then_data);
            
            let else_jump = emit_jump_inst(OpCode::JumpFWd, &mut code);
            patch_jump(then_jump, &mut code);


       
            patch_jump(then_jump, &mut code);

            if let Some(els) = data.else_branch {
                let else_data = gen_node(els, comp_unit)?;
                code.append_gen_data(else_data);
                patch_jump(else_jump, &mut code);
            }
            Ok(code)
        }
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

    for operand in &mut operands {
        if is_float && operand.typ != Type::Float {
            if matches!(operand.typ, Type::Integer | Type::Boolean) {
                operand.append_op_code(OpCode::I64ToF64)
            } else { return Err(format!("Unexpected node type for operation: {:?}", operand.typ).to_string()); }
        }
    }

    let capacity = operands.len() * 2;

    let mut code = GenData {
        code: Vec::<u8>::with_capacity(capacity),
        typ: if is_float { Type::Float } else { Type::Integer },
    };


    match &data.operation {
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

            for _ in 0..size {
                code.append_op_code(op_code);
            }
            Ok(code)
        }

        Op::PlusPlus | Op::MinusMinus => {
            if operands.len() != 1 {
                return Err("Inc/Dec is a unary operation".to_string());
            }

            if let Some(operand) = operands.pop() {
                code.append_gen_data(operand);
            }

            code.append_op_code(if data.operation == Op::PlusPlus { OpCode::IConst1 } else { OpCode::IConstM1 });
            code.append_op_code(if is_float { OpCode::AddF64 } else { OpCode::AddI64 });
            Ok(code)
        }

        Op::Greater | Op::Less | Op::GreaterEqual | Op::LessEqual | Op::Equals => {
            if operands.len() < 2 {
                return Err("Expected at least 2 operands for comparison operation".to_string());
            }
            if operands.len() > 256 {
                return Err("Unsupported operand amount (> 256)".to_string());
            }

            let size = operands.len() as u8;
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

            let comp_op = match &data.operation {
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

        Op::BangEquals | Op::RefEqual => todo!()
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


fn emit_jump_inst(op_code: OpCode, gen_data:&mut GenData) -> usize {
    gen_data.append_op_code(op_code);
    gen_data.append_wide_inst(0);
    gen_data.code.len() - 2
}


fn patch_jump(offset: usize, gen_data: &mut GenData) {
    let jump = gen_data.code.len() - offset - 2;

    if jump > u16::MAX as usize {
        panic!("Too large of jump encountered (> 65,535 instructions)")
    }
    gen_data.patch_wide_inst(offset, jump as u16);
    
}