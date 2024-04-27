use std::ffi::c_void;
use crate::types::Type;
use lang::util::SCACHE;
use crate::ast::{AssignData, AstNode, CondData, ConsData, DefClassData, DefFuncData, DefLambdaData, DefStructData, DefVarData, DirectInst, FuncCallData, GenRandData, IfData, InnerFuncCallData, ListAccData, LiteralCallData, MultiExprData, ObjectAssignData, ObjectCallData, OpData, WhileData};
use crate::environment::{Context, Environment, ResData, TypeData};
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


fn resolve(node: &mut AstNode, env: &mut Environment) -> Result<Type, String> {
    if let Some(typ) = node.resolved_type() { return Ok(typ); }
    match node {
        AstNode::DefVariable(data) => resolve_def_var(data, env),
        AstNode::DefLambda(data) => resolve_def_lambda(data, env),
        AstNode::DefFunction(data) => resolve_def_func(data, env),
        AstNode::DefStruct(data) => resolve_def_struct(data, env),
        AstNode::DefClass(data) => resolve_def_class(data, env),
        AstNode::ExprAssignment(data) => resolve_expr_assign(data, env),
        AstNode::ExprMulti(data) => resolve_expr_multi(data, env),
        AstNode::ExprPrint(data) => resolve_expr_print(data, env),
        AstNode::ExprIf(data) => resolve_expr_if(data, env),
        AstNode::ExprCond(data) => resolve_expr_cond(data, env),
        AstNode::ExprWhileLoop(data) => resolve_expr_while(data, env), //TODO handle return assignment
        AstNode::ExprCons(data) => resolve_expr_cons(data, env),
        AstNode::ExprPairList(data) => resolve_expr_pair_list(data, env),
        AstNode::ExprArray(data) => resolve_expr_array(data, env),
        AstNode::ExprListAccess(data) => resolve_expr_list_acc(data, env),
        AstNode::ExprFuncCall(data) => resolve_func_call(data, env),
        AstNode::ExprFuncCalInner(data) => resolve_func_call_inner(data, env),
        AstNode::ExprObjectCall(data) => resolve_obj_call(data, env), // TODO will need to resolve accessor to their type
        AstNode::ExprLiteralCall(data) => resolve_literal_call(data, env),
        AstNode::ExprObjectAssignment(data) => resolve_obj_assign(data, env), // TODO will need to resolve accessor to their type
        AstNode::ExprGenRand(data) => resolve_gen_rand(data, env),
        AstNode::ExprDirectInst(data) => resolve_direct_inst(data, env),
        AstNode::ExprInitInst(data) => resolve_init_inst(data, env),
        AstNode::Operation(data) => resolve_operation(data, env),
        AstNode::LitInteger(_) => Ok(Type::Integer),
        AstNode::LitFloat(_) => Ok(Type::Float),
        AstNode::LitBoolean(_) => Ok(Type::Boolean),
        AstNode::LitString(_) => Ok(Type::String),
        AstNode::LitQuote => todo!(),
        AstNode::LitObject => todo!(),
        AstNode::LitStruct() => todo!(),
        AstNode::LitNil => Ok(Type::Nil),
        AstNode::LitArray => todo!(),
        AstNode::LitPair => todo!(),
        AstNode::LitLambda => todo!(),
    }
}


pub fn resolve_def_var(data: &mut Box<DefVarData>, env: &mut Environment) -> Result<Type, String> {
    let resolved_type = resolve(&mut data.value, env)?;
    if resolved_type == Type::Unresolved { return Ok(Type::Unresolved); }


    if let Some(d_type) = data.d_type {
        let resolved_d_type = env.validate_type(d_type);
        if resolved_type == Type::Unresolved {
            return Ok(Type::Unresolved);
        } else if resolved_type != *resolved_d_type {
            return Err(format!("Resolved type: {:?} does not equal declared type: {:?}", resolved_type, resolved_d_type));
        }
    }

    let mods = data.modifiers.as_ref().unwrap_or_else(|| &vec![]);
    let res_data = env.add_var_symbol(data.name, resolved_type.clone(), mods)?;
    data.res_data = Some(res_data.clone());
    Ok(resolved_type)
}


pub fn resolve_def_lambda(data: &mut Box<DefLambdaData>, env: &mut Environment) -> Result<Type, String> {
    let resolved_type = resolve(&mut data.body, env)?;
    if let Some(d_type) = data.d_type {
        if resolved_type != *env.validate_type(d_type) {
            return Err("Declared type does not match actual type for lambda".to_string());
        }
    }
    Ok(resolved_type)
}


pub fn resolve_def_func(data: &mut Box<DefFuncData>, env: &mut Environment) -> Result<Type, String> {
    todo!()
}


pub fn resolve_def_struct(data: &mut Box<DefStructData>, env: &mut Environment) -> Result<Type, String> {
    todo!()
}


pub fn resolve_def_class(data: &mut Box<DefClassData>, env: &mut Environment) -> Result<Type, String> {
    todo!()
}


pub fn resolve_expr_assign(data: &mut Box<AssignData>, env: &mut Environment) -> Result<Type, String> {
    let resolved_type = resolve(&mut data.value, env)?;
    if resolved_type == Type::Unresolved {
        return Ok(Type::Unresolved);
    } else {
        let ctx = env.get_symbol_ctx(data.name);
        let type_id = env.meta_space.types.get_type_id(&resolved_type);
        if let Some(target_res) = ctx {
            let res_data = ResData {
                target_ctx: Some(target_res.self_ctx.clone()),
                self_ctx: Context::Expr(env.get_env_ctx()),
                type_data: TypeData { typ: resolved_type, type_id },
            };

            data.res_data = Some(res_data);
        } else {
            return Err(format!("Failed to resolve symbol: {}", SCACHE.resolve(data.name)));
        }

        println!("Assign Context added");
        match &data.value {
            AstNode::ExprIf(expr) => {
                if expr.res_data.unwrap().type_data.typ == Type::Void { // Above code ensure this will be resolved
                    Err("All branches in assignment must have same type".to_string())
                } else {
                    let var_type = env.get_symbol_type(data.name);
                    if var_type == &resolved_type {
                        Ok(resolved_type.clone())
                    } else {
                        Err(format!("Attempted to assign incorrect type for var: {}", SCACHE.resolve(data.name)))
                    }
                }
            }
            AstNode::ExprCond(expr) => {
                if expr.res_data.unwrap().type_data.typ == Type::Void {// Above code ensure this will be resolved
                    Err("All branches in assignment must have same type".to_string())
                } else {
                    let var_type = env.get_symbol_type(data.name);
                    if var_type == &resolved_type {
                        Ok(resolved_type.clone())
                    } else {
                        Err(format!("Attempted to assign incorrect type for var: {}", SCACHE.resolve(data.name)))
                    }
                }
            }
            _ => Ok(env.get_symbol_type(data.name).clone())
        }
    }
}


pub fn resolve_expr_multi(data: &mut MultiExprData, env: &mut Environment) -> Result<Type, String> {
    env.push_scope();
    let mut resolved_type = Type::Unresolved;
    for expr in data.expressions.iter_mut() {
        resolved_type = resolve(expr, env)?;
    }

    let ctx = env.get_env_ctx();
    let type_id = env.meta_space.types.get_or_define_type(resolved_type);
    let type_data = TypeData { typ: resolved_type.clone(), type_id };
    let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
    data.res_data = Some(res_data);
    
    env.pop_scope();
    
    Ok(resolved_type.clone())
}


pub fn resolve_expr_print(data: &mut Box<AstNode>, env: &mut Environment) -> Result<Type, String> {
    resolve(data, env)
}


pub fn resolve_expr_if(data: &mut Box<IfData>, env: &mut Environment) -> Result<Type, String> {
    env.push_scope();

    let cond_type = resolve(&mut data.if_branch.cond_node, env)?;
    if cond_type == Type::Unresolved {
        return Ok(Type::Unresolved);
    }

    let if_type = resolve(&mut data.if_branch.then_node, env)?;
    let if_type_id = env.meta_space.types.get_type_id(&if_type);
    if if_type != Type::Unresolved {
        let ctx = env.get_env_ctx();
        let type_data = TypeData { typ: if_type.clone(), type_id: if_type_id };
        let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
        data.if_branch.res_data = Some(res_data);
    }

    env.pop_scope();

    if let Some(ref mut els) = data.else_branch {
        env.push_scope();

        let else_type = resolve(els, env)?;
        if else_type != Type::Unresolved {
            data.else_type = Some(else_type.clone())
        }

        env.pop_scope();

        if if_type != Type::Unresolved && else_type != Type::Unresolved {
            let ctx = env.get_env_ctx();

            if else_type != if_type {
                let type_data = TypeData { typ: Type::Void, type_id: env.meta_space.types.void };
                let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
                data.res_data = Some(res_data);
                return Ok(Type::Void);
            } // Fall through to end of func return if types are same, or no else branch
        } else { return Ok(Type::Unresolved); }
    }

    let ctx = env.get_env_ctx();
    let type_data = TypeData { typ: if_type.clone(), type_id: if_type_id };
    let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
    data.res_data = Some(res_data);
    Ok(if_type)
}


pub fn resolve_expr_cond(data: &mut Box<CondData>, env: &mut Environment) -> Result<Type, String> {
    let mut unresolved = false;

    for branch in data.cond_branches.iter_mut() {
        if branch.res_data.is_some() { continue; }
        env.push_scope();

        let branch_type = resolve(&mut branch.then_node, env)?;
        if branch_type == Type::Unresolved {
            unresolved = true;
            env.pop_scope(); // pop for early continue
            continue;
        }
        let branch_type_id = env.meta_space.types.get_type_id(&branch_type);
        branch.typ = Some(branch_type.clone());

        let ctx = env.get_env_ctx();
        let type_data = TypeData { typ: branch_type.clone(), type_id: branch_type_id };
        let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
        branch.res_data = Some(res_data);

        env.pop_scope();
    }

    if let Some(ref mut els) = data.else_branch {
        if els.resolved_type().is_none() {
            env.push_scope();

            let else_type = resolve(els, env)?;
            if else_type == Type::Unresolved {
                env.pop_scope(); // pop for early return
                return Ok(Type::Unresolved);
            }

            env.pop_scope();
        }
    }

    if unresolved { return Ok(Type::Unresolved); }

    let base_type = if let Some(els) = &data.else_branch {
        els.resolved_type().unwrap() // safe to unwrap due to resolution branch above
    } else if let Some(&branch) = data.cond_branches.get(0) {
        branch.typ.unwrap()
    } else { panic!("Fatal: If expression with no logic") };

    let all_match = data.cond_branches.iter().all(|branch| {
        if let Some(typ) = &branch.typ {
            *typ == base_type
        } else { false }
    });

    let ctx = env.get_env_ctx();

    if all_match {
        let type_id = env.meta_space.types.get_type_id(&base_type);
        let type_data = TypeData { typ: base_type.clone(), type_id };
        let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
        data.res_data = Some(res_data);
        Ok(base_type)
    } else {
        let type_id = env.meta_space.types.get_type_id(&Type::Void);
        let type_data = TypeData { typ: Type::Void, type_id };
        let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
        data.res_data = Some(res_data);
        Ok(Type::Void)
    }
}


pub fn resolve_expr_while(data: &mut Box<WhileData>, env: &mut Environment) -> Result<Type, String> {
    let cond_type = resolve(&mut data.condition, env)?;
    if cond_type == Type::Unresolved {
        return Ok(Type::Unresolved);
    }

    let body_type = resolve(&mut data.body, env)?;
    if body_type == Type::Unresolved {
        Ok(Type::Unresolved)
    } else {
        Ok(body_type)
    }
}


pub fn resolve_expr_cons(data: &mut Box<ConsData>, env: &mut Environment) -> Result<Type, String> {
    todo!()
}


pub fn resolve_expr_pair_list(data: &mut OpData, env: &mut Environment) -> Result<Type, String> {
    todo!()
}


pub fn resolve_expr_array(data: &mut OpData, env: &mut Environment) -> Result<Type, String> {
    let mut resolved = true;
    let mut typ: Option<Type> = None;

    for i in 0..data.operands.len() {
        let r_type = resolve(data.operands.get_mut(i).unwrap(), env)?;
        if Type::Unresolved == r_type {
            resolved = false;
            continue;
        } else if typ.is_none() {
            typ = Some(r_type)
        } else if let Some(typ) = &typ {
            if *typ != r_type {
                return Err(format!("Array type mismatch, expected: {:?}, found: {:?}", typ, resolved));
            }
        }
    }
    if !resolved {
        return Ok(Type::Unresolved);
    } else {
        let ctx = env.get_env_ctx();
        let typ = typ.unwrap();
        let arr_type = Type::Array(Box::new(typ));
        let type_id = env.define_type(arr_type.clone());
        let type_data = TypeData { typ: arr_type.clone(), type_id };
        env.define_type(arr_type.clone());
        let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
        data.res_data = Some(res_data);
        Ok(arr_type)
    }
}


pub fn resolve_expr_list_acc(data: &mut Box<ListAccData>, env: &mut Environment) -> Result<Type, String> {
    resolve(&mut data.list, env)?;
    Ok(Type::Void)
}


pub fn resolve_func_call(data: &mut Box<FuncCallData>, env: &mut Environment) -> Result<Type, String> {
    Ok(env.get_symbol_type(data.name).clone())
}


pub fn resolve_func_call_inner(data: &mut Box<InnerFuncCallData>, env: &mut Environment) -> Result<Type, String> {
    Ok(resolve(&mut data.expr, env)?)
}


pub fn resolve_obj_call(data: &mut Box<ObjectCallData>, env: &mut Environment) -> Result<Type, String> {
    todo!()
}


pub fn resolve_literal_call(data: &mut LiteralCallData, env: &mut Environment) -> Result<Type, String> {
    // If symbol is not found it is non-existent or out of scope
    if let Some(target_ctx) = env.get_symbol_ctx(data.name) {
        let ctx = env.get_env_ctx();
        let type_data = target_ctx.type_data.clone();
        let typ = type_data.typ.clone();
        let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: Some(target_ctx.self_ctx.clone()), type_data };
        data.res_data = Some(res_data);
        Ok(typ)
    } else {
        return Err(format!("Failed to resolve symbol: {}", SCACHE.resolve(data.name)));
    }
}


pub fn resolve_obj_assign(data: &mut Box<ObjectAssignData>, env: &mut Environment) -> Result<Type, String> {
    todo!()
}


pub fn resolve_gen_rand(data: &mut Box<GenRandData>, env: &mut Environment) -> Result<Type, String> {
    if data.is_float { Ok(Type::Float) } else { Ok(Type::Integer) }
}


pub fn resolve_direct_inst(data: &mut Box<DirectInst>, env: &mut Environment) -> Result<Type, String> {
    Ok(env.get_symbol_type(data.name).clone())
}


pub fn resolve_init_inst(data: &mut Box<FuncCallData>, env: &mut Environment) -> Result<Type, String> {
    Ok(env.get_symbol_type(data.name).clone())
}


pub fn resolve_operation(data: &mut OpData, env: &mut Environment) -> Result<Type, String> {
    let mut typ = Type::Integer;
    for op in data.operands.iter_mut() {
        let op_typ = resolve(op, env)?;
        match op_typ {
            Type::Float => typ = Type::Float,
            Type::Integer | Type::Boolean => continue,
            Type::Unresolved => return Ok(Type::Unresolved),
            _ => return Err("Invalid type in operation expression".to_string()),
        }
    }
    let ctx = env.get_env_ctx();
    let type_id = env.meta_space.types.get_type_id(&typ);
    let type_data = TypeData { typ: typ.clone(), type_id };
    let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
    data.res_data = Some(res_data);
    Ok(typ)
}