use vm::environment::Environment;
use crate::types::Type;
use lang::util::SCACHE;
use crate::ast::AstNode;
use crate::environment::Environment;
use crate::parser::ParseResult;



// TODO notes: add better resolution for conditional statements so they can always be used
//  as inner expression, only allow cond. to either return void for branching, or a single type.
//  only non void types can be used for assignment, and all multi types should be flagged as 
//  the type void for that expression (even if it has non void branches, as some expression
//  return a value that is ignored in many context, if statements shouldnt be forced to have
//  type cohesion across branches, just when being used inside other expressions


// TODO skip over already resolved nodes 2nd loop

pub fn resolve_types(mut parse_result: &mut ParseResult, mut context: Environment) -> bool {
    let mut fully_resolved = true;
    for _ in 0..2 {
        fully_resolved = true;
        for node in parse_result.root_expressions.iter_mut() {
            if resolve(node, &mut context) == Ok(Type::Unresolved) {
                fully_resolved = false;
            }
        }
    }
    fully_resolved
}


fn resolve(node: &mut AstNode, context: &mut Environment) -> Result<Type, String> {
    match node {
        AstNode::DefVariable(data) => {
            let resolved_type = resolve(&mut data.value, context)?;
            if resolved_type == Type::Unresolved { return Ok(Type::Unresolved); }


            if let Some(d_type) = data.d_type {
                let resolved_d_type = context.validate_type(d_type);
                if resolved_type == Type::Unresolved {
                    return Ok(Type::Unresolved);
                } else if resolved_type != *resolved_d_type {
                    return Err(format!(
                        "Resolved type: {:?} does not equal declared type: {:?}",
                        resolved_type, resolved_d_type));
                }
            }

            context.add_symbol(data.name, resolved_type.clone())?;

            let ctx = context.get_symbol_ctx(data.name);
            data.ctx = ctx;
            Ok(resolved_type)
        }

        AstNode::DefLambda(data) => {
            let resolved_type = resolve(&mut data.body, context)?;
            if let Some(d_type) = data.d_type {
                if resolved_type != *context.validate_type(d_type) {
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
                println!("Resolved false");
                return Ok(Type::Unresolved);
            } else {
                data.ctx = Some(context.get_scope_ctx());
                println!("Assign OCntext added");
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
                                    SCACHE.resolve(data.name)))
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
                                    SCACHE.resolve(data.name)))
                            }
                        }
                    }
                    _ => Ok(context.get_symbol_type(data.name).clone())
                }
            }
        }

        AstNode::ExprMulti(data) => {
            context.push_scope();
            let mut resolved_type = Type::Unresolved;
            for expr in data.expressions.iter_mut() {
                resolved_type = resolve(expr, context)?;
            }
            context.pop_scope();
            Ok(resolved_type)
        }

        AstNode::ExprPrint(data) => {
            resolve(data, context)
        }

        AstNode::ExprIf(data) => {
            context.push_scope();

            let cond_type = resolve(&mut data.if_branch.cond_node, context)?;
            if cond_type == Type::Unresolved {
                return Ok(Type::Unresolved);
            }

            let if_type = resolve(&mut data.if_branch.then_node, context)?;
            if if_type != Type::Unresolved {
                data.if_branch.typ = if_type.clone();
            }
            context.pop_scope();

            if let Some(ref mut els) = data.else_branch {
                context.push_scope();
                let else_type = resolve(els, context)?;
                if else_type != Type::Unresolved {
                    data.else_type = else_type.clone();
                }
                context.pop_scope();

                if if_type != Type::Unresolved && else_type != Type::Unresolved {
                    Ok(else_type) // Fixme, find a better way to represent conditional types as they vary
                } else { Ok(Type::Unresolved) }
            } else { Ok(if_type) }
        }

        AstNode::ExprCond(data) => {
            let mut unresolved = false;
            let mut cond_type = Type::Unresolved;
            for branch in data.cond_branches.iter_mut() {
                context.push_scope();
                let branch_type = resolve(&mut branch.then_node, context)?;

                if branch_type != Type::Unresolved {
                    branch.typ = branch_type.clone();
                } else { unresolved = true; }

                cond_type = branch_type;
                context.pop_scope();
            }

            if let Some(ref mut els) = data.else_branch {
                context.push_scope();
                let else_type = resolve(els, context)?;
                if else_type != Type::Unresolved {
                    data.else_type = else_type.clone();
                }
                context.pop_scope();

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

        AstNode::ExprWhileLoop(data) => {
            let cond_type = resolve(&mut data.condition, context)?;
            if cond_type == Type::Unresolved {
                return Ok(Type::Unresolved);
            }

            let body_type = resolve(&mut data.body, context)?;
            if body_type == Type::Unresolved {
                Ok(Type::Unresolved)
            } else {
                Ok(body_type)
            }
        } //TODO handle return assignment
        AstNode::ExprCons(_) => Ok(Type::Pair),
        AstNode::ExprPairList(_) => Ok(Type::Pair),
        AstNode::ExprArray(data) => {
            let mut resolved = true;
            let mut typ: Type = Type::Unresolved;
            for i in 0..data.operands.len() {
                let r_type = resolve(data.operands.get_mut(i).unwrap(), context)?;
                if  Type::Unresolved == r_type {
                   resolved = false;
                    continue
                } else if i == 0 {
                    typ = r_type
                } else if typ != r_type {
                   return  Err(format!("Array type mismatch, expected: {:?}, found: {:?}", typ, resolved))
                }
            }
            if !resolved {
                return Ok(Type::Unresolved)
            } else {
                let arr_type = Type::Array(Box::new(typ.clone()));
                context.define_type(arr_type.clone());
                data.typ = arr_type.clone();
                Ok(arr_type)
            }
        }
        AstNode::ExprListAccess(data) => {
            resolve(&mut data.list, context)?;
            Ok(Type::Nil)
        }
        AstNode::ExprFuncCall(data) => Ok(context.get_symbol_type(data.name).clone()),
        AstNode::ExprFuncCalInner(data) => Ok(resolve(&mut data.expr, context)?),
        AstNode::ExprObjectCall(data) => todo!(), // TODO will need to resolve accessor to their type
        AstNode::ExprLiteralCall(data) => {
            data.ctx = Some(context.get_scope_ctx());
            Ok(context.get_symbol_type(data.name).clone())
        }
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
