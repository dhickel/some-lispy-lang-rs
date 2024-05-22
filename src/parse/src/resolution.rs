use std::fmt::format;
use std::ops::Deref;
use lang::format_error;
use crate::types::{FuncType, Type};
use lang::util::{IString, SCACHE};
use crate::ast::{AssignData, AstData, AstNode, CondData, ConsData, DefClassData, DefFuncData,
    DefLambdaData, DefStructData, DefVarData, DirectInst, FuncCallData, GenRandData, IfData,
    InnerFuncCallData, ListAccData, LiteralCallData, MultiExprData, ObjectAssignData, ObjectCallData,
    OpData, WhileData};
use crate::code_gen::GenData;
use crate::environment::{Context, Environment, ResData, TypeData};
use crate::parser::ParseResult;
use crate::token::Op;


// TODO handle type hierarchies, current type checking check for the concrete type only, inference
//  for hierarchical types and conversions needs implemented (add a matching function to Type?)  


pub fn resolve_types(mut parse_result: &mut ParseResult, mut env: Environment) -> bool {
    // Need a starting scope for resolution to properly work, also add a quick depth check as
    // this will signal resolutions errors where a scope is not popped
    let start_depth = env.get_env_ctx().depth;
    env.push_scope();

    let mut fully_resolved = true;
    for _ in 0..1 {
        //fully_resolved = true;
        for node in parse_result.root_expressions.iter_mut() {
            match resolve(node, &mut env) {
                Ok(typ) => {
                    match typ {
                        Type::Unresolved => { fully_resolved = false; }
                        _ => { println!("{:?}", typ) }
                    }
                }
                Err(err) => {
                    fully_resolved = false;
                    println!("{}", format_error!(node.line_char, err));
                }
            }
        }
    }

    env.pop_scope();
    let end_depth = env.get_env_ctx().depth;
    if start_depth != end_depth {
        let msg = format!(
            "Fatal: Compilation Error |  Ending scope dpeth: ({}) != starting scope depth: ({})",
            start_depth, end_depth
        );
        panic!("{:?}", msg);
    }

    fully_resolved
}


fn resolve(node: &mut AstNode, env: &mut Environment) -> Result<Type, String> {
    if let Some(typ) = node.resolved_type() { return Ok(typ); }
    let res_data = match &mut *node.node_data {
        AstData::DefVariable(data) => resolve_def_var(data, env),
        AstData::DefLambda(data) => {
            resolve_def_lambda(
                data,
                SCACHE.intern(format!("Lambda:{}:{}:{}", env.curr_ns, node.line_char.0, node.line_char.1).to_string()),
                env,
            )
        }
        AstData::DefFunction(data) => resolve_def_func(data, env),
        AstData::DefStruct(data) => resolve_def_struct(data, env),
        AstData::DefClass(data) => resolve_def_class(data, env),
        AstData::ExprAssignment(data) => resolve_expr_assign(data, env),
        AstData::ExprMulti(data) => resolve_expr_multi(data, env),
        AstData::ExprPrint(data) => resolve_expr_print(data, env),
        AstData::ExprIf(data) => resolve_expr_if(data, env),
        AstData::ExprCond(data) => resolve_expr_cond(data, env),
        AstData::ExprWhileLoop(data) => resolve_expr_while(data, env), //TODO handle return assignment
        AstData::ExprCons(data) => resolve_expr_cons(data, env),
        AstData::ExprPairList(data) => resolve_expr_pair_list(data, env),
        AstData::ExprArray(data) => resolve_expr_array(data, env),
        AstData::ExprListAccess(data) => resolve_expr_list_acc(data, env),
        AstData::ExprFuncCall(data) => resolve_func_call(data, env),
        AstData::ExprFuncCalInner(data) => resolve_func_call_inner(data, env),
        AstData::ExprObjectCall(data) => resolve_obj_call(data, env), // TODO will need to resolve accessor to their type
        AstData::ExprLiteralCall(data) => resolve_literal_call(data, env),
        AstData::ExprObjectAssignment(data) => resolve_obj_assign(data, env), // TODO will need to resolve accessor to their type
        AstData::ExprGenRand(data) => resolve_gen_rand(data, env),
        AstData::ExprDirectInst(data) => resolve_direct_inst(data, env),
        AstData::ExprInitInst(data) => resolve_init_inst(data, env),
        AstData::Operation(data) => resolve_operation(data, env),
        AstData::LitInteger(_) => resolve_primitive(Type::Integer, env),
        AstData::LitFloat(_) => resolve_primitive(Type::Float, env),
        AstData::LitBoolean(_) => resolve_primitive(Type::Boolean, env),
        AstData::LitString(_) => resolve_primitive(Type::String, env),
        AstData::LitQuote => todo!(),
        AstData::LitObject => todo!(),
        AstData::LitStruct() => todo!(),
        AstData::LitNil => resolve_primitive(Type::Nil, env),
        AstData::LitArray => todo!(),
        AstData::LitPair => todo!(),
        AstData::LitLambda => todo!(),
    };

    if let Some(data) = res_data? {
        let typ = data.type_data.rtn_type.clone();
        node.res_data = Some(data);
        Ok(typ)
    } else { Ok(Type::Unresolved) }
}

// FIXME need to refactor defs to take modifiers as an optional, vs initing empty arrays

pub fn resolve_def_var(data: &mut DefVarData, env: &mut Environment) -> Result<Option<ResData>, String> {
    println!("resovling def Var for {:?}", SCACHE.resolve(data.name));


    // FIXME, currently this messy match is used to handle the case of an existing function being assigned to new variable
    if let AstData::ExprLiteralCall(ref lit) = *data.value.node_data {
        if let Some(symbol) = env.meta_space.get_symbol(env.curr_ns, env.get_curr_scope(), lit.name.value) {
            if let Context::Symbol(sym) = &symbol.self_ctx {
                if sym.func.is_some() {
                    let res_data = env.meta_space.add_symbol(env.curr_ns, env.get_curr_scope(), data.name, symbol.clone());
                    return Ok(Some(res_data.clone()));
                }
            }
        } else { return Err(format!("Failed to find symbol for assignment {:?}", SCACHE.resolve(lit.name))); }
    }

    let resolved_type = resolve(&mut data.value, env)?;
    if resolved_type == Type::Unresolved { return Ok(None); }


    if let Some(d_type) = &data.d_type {
        if resolved_type != *d_type {
            return Err(format!("Resolved type: {:?} does not equal declared type: {:?}", resolved_type, d_type));
        }
    }

    let res_data = env.add_var_symbol(data.name, resolved_type.clone(), data.modifiers.clone())?;
    Ok(Some(res_data.clone()))
}


pub fn resolve_def_lambda(data: &mut DefLambdaData, name: IString, env: &mut Environment) -> Result<Option<ResData>, String> {
    let resolved_type = resolve(&mut data.body, env)?;
    if resolved_type == Type::Unresolved { return Ok(None); }

    let func_type = if let Some(Type::Lambda(func_type)) = &data.d_type {
        func_type
    } else { return Err(format!("Invalid type for function definition: {:?}", &data)); };


    if resolved_type != *func_type.rtn_type {
        println!("Here ....\n\n");
        return Err("Declared type {:?} does not match actual type {:?} for lambda".to_string());
    }


    let res_data = env.add_func_symbol(name, data, func_type)?.clone();
    env.pop_func();
    Ok(Some(res_data))
}


pub fn resolve_def_func(data: &mut DefFuncData, env: &mut Environment) -> Result<Option<ResData>, String> {
    let func_type = if let Some(Type::Lambda(func_type)) = &data.d_type {
        func_type
    } else { return Err(format!("Invalid type for function definition: {:?}", &data)); };

    // let d_type = env.meta_space.types.get_type_by_name(data.d_type)
    //     .ok_or_else(|| "Failed to resolve declared return type".to_string())?.clone();

    let res_data = env.add_func_symbol(data.name, &data.lambda, func_type)?.clone();
    let resolved_type = resolve(&mut data.lambda.body, env)?;
    env.pop_func();

    if *func_type.rtn_type != resolved_type {
        println!("\n\nname:{:?} \n\n", SCACHE.resolve(data.name));
        return Err(format!("Declared type: {:?} does not match resolved type: {:?} for function",
            func_type.rtn_type,
            resolved_type).to_string()
        );
    }

    Ok(Some(res_data))
}


pub fn resolve_def_struct(data: &mut DefStructData, env: &mut Environment) -> Result<Option<ResData>, String> {
    todo!()
}


pub fn resolve_def_class(data: &mut DefClassData, env: &mut Environment) -> Result<Option<ResData>, String> {
    todo!()
}


pub fn resolve_expr_assign(data: &mut AssignData, env: &mut Environment) -> Result<Option<ResData>, String> {
    let resolved_type = resolve(&mut data.value, env)?;

    if resolved_type == Type::Unresolved {
        return Ok(None);
    } else if resolved_type == Type::Void {
        return Err("Assign data must have non void type".to_string());
    }

    let type_id = env.meta_space.types.get_or_define_type(&resolved_type);
    let ctx = env.get_symbol_ctx(data.name);

    if let Some(target_res) = ctx {
        println!("target ctx: {:?}", target_res);
        let res_data = ResData {
            target_ctx: Some(target_res.self_ctx.clone()),
            self_ctx: Context::Expr(env.get_env_ctx()),
            type_data: TypeData::from_type(&resolved_type, &mut env.meta_space.types),
        };
        Ok(Some(res_data))
    } else {
        Err(format!("Failed to resolve assign target symbol: {}", SCACHE.resolve(data.name)))
    }
}


pub fn resolve_expr_multi(data: &mut MultiExprData, env: &mut Environment) -> Result<Option<ResData>, String> {
    env.push_scope();

    let mut resolved_type = Type::Unresolved;
    let mut is_resolved = true;
    for expr in data.expressions.iter_mut() {
        resolved_type = resolve(expr, env)?;
        if resolved_type == Type::Unresolved {
            is_resolved = false;
        }
    }

    env.pop_scope();

    if resolved_type == Type::Unresolved || !is_resolved {
        Ok(None)
    } else {
        let ctx = env.get_env_ctx();
        let type_id = env.meta_space.types.get_or_define_type(&resolved_type);
        let type_data = TypeData::from_type(&resolved_type, &mut env.meta_space.types);
        let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
        Ok(Some(res_data))
    }
}


pub fn resolve_expr_print(data: &mut AstNode, env: &mut Environment) -> Result<Option<ResData>, String> {
    let resolved_type = resolve(data, env)?;
    if resolved_type == Type::Unresolved {
        Ok(None)
    } else {
        let ctx = env.get_env_ctx();
        let type_id = env.meta_space.types.void;
        let type_data = TypeData::from_type(&resolved_type, &mut env.meta_space.types);
        let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
        Ok(Some(res_data))
    }
}


pub fn resolve_expr_if(data: &mut IfData, env: &mut Environment) -> Result<Option<ResData>, String> {
    env.push_scope();
    let cond_type = resolve(&mut data.if_branch.cond_node, env)?;
    env.pop_scope();

    if cond_type == Type::Unresolved {
        return Ok(None);
    }
    if cond_type != Type::Boolean {
        return Err(format!("If condition resolved to: {:?}, instead of boolean", cond_type).to_string());
    }


    env.push_scope();
    let then_type = resolve(&mut data.if_branch.then_node, env)?;
    env.pop_scope();

    if then_type == Type::Unresolved { return Ok(None); }

    let expr_type = if let Some(ref mut els) = data.else_branch {
        env.push_scope();
        let else_type = resolve(els, env)?;
        env.pop_scope();

        if else_type == Type::Unresolved {
            return Ok(None);
        } else if else_type != then_type {
            Type::Void
        } else {
            else_type
        }
    } else { then_type };

    let ctx = env.get_env_ctx();
    let type_id = env.meta_space.types.get_or_define_type(&expr_type);
    let type_data =  TypeData::from_type(&expr_type, &mut env.meta_space.types);
    let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
    Ok(Some(res_data))
}

// FixMe, Some of these calls to resolve could result in useless scopes being pushed if the nodes are
//  already resolved, this won't affect resolution, but increase memory usage some. resolve_type()
//  could be called first to see if they are resolved and avoid duplicate resolve calls and scopes
//  being pushed. This happens in several of these resolution functions.

pub fn resolve_expr_cond(data: &mut CondData, env: &mut Environment) -> Result<Option<ResData>, String> {
    let mut unresolved = false;
    let mut types = Vec::<Type>::with_capacity(data.cond_branches.len() + 1);

    for branch in data.cond_branches.iter_mut() {
        env.push_scope();
        let cond_type = resolve(&mut branch.cond_node, env)?;
        env.pop_scope();

        if cond_type == Type::Unresolved { unresolved = true }
        if cond_type != Type::Boolean {
            return Err("Condition did not resolve to boolean".to_string());
        }

        env.push_scope();
        let branch_type = resolve(&mut branch.then_node, env)?;
        env.pop_scope();

        if branch_type == Type::Unresolved { unresolved = true; }
        types.push(branch_type);

        if let Some(ref mut els) = data.else_branch {
            env.push_scope();
            let else_type = resolve(els, env)?;
            env.pop_scope();
            if else_type == Type::Unresolved { unresolved = true }
        }
    }

    if unresolved { return Ok(None); }

    // Will always have at least 1 element, so unwrap is safe
    let base_type = types.get(0).unwrap();
    let all_match = types.iter().all(|t| t == base_type);


    let ctx = env.get_env_ctx();
    if all_match {
        let type_id = env.meta_space.types.get_or_define_type(&base_type);
        let type_data = TypeData::from_type(base_type, &mut env.meta_space.types);
        let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
        Ok(Some(res_data))
    } else {
        let type_id = env.meta_space.types.get_or_define_type(&Type::Void);
        let type_data = TypeData::from_type(base_type, &mut env.meta_space.types);
        let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
        Ok(Some(res_data))
    }
}


pub fn resolve_expr_while(data: &mut WhileData, env: &mut Environment) -> Result<Option<ResData>, String> {
    env.push_scope();
    let cond_type = resolve(&mut data.condition, env)?;
    env.pop_scope();

    if cond_type == Type::Unresolved { return Ok(None); }
    if cond_type != Type::Boolean {
        return Err("While condition did not resolve to boolean".to_string());
    }

    // No need to push scope, as while bodies are parsed with an implicit multi-expr
    let body_type = resolve(&mut data.body, env)?;
    if body_type == Type::Unresolved {
        Ok(None)
    } else {
        let ctx = env.get_env_ctx();
        let type_id = env.meta_space.types.get_or_define_type(&body_type);
        let type_data = TypeData::from_type(&body_type, &mut env.meta_space.types);
        let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
        Ok(Some(res_data))
    }
}


// TODO annotate pair data and runtime representation with member type info?
pub fn resolve_expr_cons(data: &mut ConsData, env: &mut Environment) -> Result<Option<ResData>, String> {
    let ctx = env.get_env_ctx();
    let type_data = TypeData::from_type(&Type::Pair, &mut env.meta_space.types);
    let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
    Ok(Some(res_data))
}


pub fn resolve_expr_pair_list(data: &mut OpData, env: &mut Environment) -> Result<Option<ResData>, String> {
    let ctx = env.get_env_ctx();
    let type_data = TypeData::from_type(&Type::Pair, &mut env.meta_space.types);
    let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
    Ok(Some(res_data))
}


pub fn resolve_expr_array(data: &mut OpData, env: &mut Environment) -> Result<Option<ResData>, String> {
    let mut resolved = true;
    let mut types = Vec::<Type>::with_capacity(data.operands.len());

    for op in data.operands.iter_mut() {
        let resolved_type = resolve(op, env)?;
        if resolved_type == Type::Unresolved { resolved = false; }
        types.push(resolved_type);
    }
    if !resolved { return Ok(None); }


    let base_type = types.get(0).unwrap();
    let all_match = types.iter().all(|t| t == base_type);

    let ctx = env.get_env_ctx();
    if !all_match {
        Err("Non-homogeneous array types".to_string())
    } else {
        let type_id = env.meta_space.types.get_or_define_type(&base_type);
        let type_data = TypeData::from_type(base_type, &mut env.meta_space.types);
        let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
        Ok(Some(res_data))
    }
}


// Fixme Need to either makes cons list only contain the same type, or add sometime of runtime inference
pub fn resolve_expr_list_acc(data: &mut ListAccData, env: &mut Environment) -> Result<Option<ResData>, String> {
    let index_type = if let Some(expr) = &mut data.index_expr {
        resolve(expr, env)?
    } else {
        return Err("No access pattern/index specified".to_string());
    };

    if index_type == Type::Unresolved { return Ok(None); }

    let list_type = resolve(&mut data.list, env)?;
    if list_type == Type::Unresolved {
        return Ok(None);
    } else if matches!(list_type,  Type::Pair | Type::Array(_)) {
        return Err("Attempted access on non list/pair item".to_string());
    }


    let ctx = env.get_env_ctx();
    let type_data = TypeData::from_type(&Type::Void, &mut env.meta_space.types);
    let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
    Ok(Some(res_data))
}


// TODO need to type check args = param here

pub fn resolve_func_call(data: &mut FuncCallData, env: &mut Environment) -> Result<Option<ResData>, String> {
    if let Some(args) = &mut data.arguments {
        args.iter_mut().for_each(|arg| {
            let _ = resolve(&mut arg.value, env);
        });
    }

    if let Some(target_ctx) = env.get_symbol_ctx(data.name) {
        let ctx = env.get_env_ctx();

        // let type_data = if let Type::Lambda(func_type) = &target_ctx.type_data.typ {
        //     *func_type.return_type.clone()
        // } else {
        //     return Err(format!("Resolved function call identifier: {:?} not a function", SCACHE.resolve(data.name)));
        // };

        let type_data = target_ctx.type_data.clone();
        let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: Some(target_ctx.self_ctx.clone()), type_data };
        println!("\n\n{:?}", res_data);
        Ok(Some(res_data))
    } else {
        Err(format!("Failed to resolve func call symbol: {}", SCACHE.resolve(data.name)))
    }
}


pub fn resolve_func_call_inner(data: &mut InnerFuncCallData, env: &mut Environment) -> Result<Option<ResData>, String> {
    let ctx = env.get_env_ctx();
    let typ = resolve(&mut data.expr, env)?;
    let type_id = env.meta_space.types.get_type_id(&typ);
    let type_data = TypeData::from_type(&typ, &mut env.meta_space.types);
    let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
    Ok(Some(res_data))
}


pub fn resolve_obj_call(data: &mut ObjectCallData, env: &mut Environment) -> Result<Option<ResData>, String> {
    todo!()
}


pub fn resolve_literal_call(data: &mut LiteralCallData, env: &mut Environment) -> Result<Option<ResData>, String> {
    if let Some(target_ctx) = env.get_symbol_ctx(data.name) {
        let ctx = env.get_env_ctx();
        let type_data = target_ctx.type_data.clone();

        let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: Some(target_ctx.self_ctx.clone()), type_data };
        Ok(Some(res_data))
    } else {
        return Err(format!("Failed to resolve literal symbol: {}", SCACHE.resolve(data.name)));
    }
}


pub fn resolve_obj_assign(data: &mut ObjectAssignData, env: &mut Environment) -> Result<Option<ResData>, String> {
    todo!()
}


pub fn resolve_gen_rand(data: &mut GenRandData, env: &mut Environment) -> Result<Option<ResData>, String> {
    let ctx = env.get_env_ctx();
    let typ = if data.is_float { Type::Float } else { Type::Integer };
    let type_id = if data.is_float { env.meta_space.types.float } else { env.meta_space.types.int };
    let type_data = TypeData::from_type(&typ, &mut env.meta_space.types);
    let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
    Ok(Some(res_data))
}


pub fn resolve_direct_inst(data: &mut DirectInst, env: &mut Environment) -> Result<Option<ResData>, String> {
    todo!()
}


pub fn resolve_init_inst(data: &mut FuncCallData, env: &mut Environment) -> Result<Option<ResData>, String> {
    todo!()
}


pub fn resolve_operation(data: &mut OpData, env: &mut Environment) -> Result<Option<ResData>, String> {
    let mut resolved = true;
    let mut op_infer = Type::Integer;

    for op in data.operands.iter_mut() {
        let op_typ = resolve(op, env)?;
        match op_typ {
            Type::Float => op_infer = Type::Float,
            Type::Integer | Type::Boolean => continue,
            Type::Unresolved => resolved = false,
            _ => {}
        }
        if op_typ == Type::Unresolved {
            resolved = false;
        }
    }

    let typ = match data.operation {
        Op::List => Type::Pair,
        Op::And | Op::Or | Op::Nor | Op::Xor | Op::Xnor | Op::Nand | Op::Negate | Op::Greater
        | Op::Less | Op::GreaterEqual | Op::LessEqual | Op::Equals | Op::BangEquals | Op::RefEqual => {
            Type::Boolean
        }
        Op::Plus | Op::Minus | Op::Asterisk | Op::Slash | Op::Caret
        | Op::Percent | Op::PlusPlus | Op::MinusMinus => {
            op_infer
        }
    };

    let ctx = env.get_env_ctx();
    let type_id = env.meta_space.types.get_type_id(&typ);
    let type_data = TypeData::from_type(&typ, &mut env.meta_space.types);
    let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };

    Ok(Some(res_data))
}


pub fn resolve_primitive(typ: Type, env: &mut Environment) -> Result<Option<ResData>, String> {
    let ctx = env.get_env_ctx();
    let type_id = env.meta_space.types.get_type_id(&typ);
    let type_data = TypeData::from_type(&typ, &mut env.meta_space.types);
    let res_data = ResData { self_ctx: Context::Expr(ctx), target_ctx: None, type_data };
    Ok(Some(res_data))
}