use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::LinkedList;
use std::ops::Deref;
use std::rc;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};
use AstNode::LiteralNode;
use crate::eval::class_loader::{ClassDef, ClassLoader};
use crate::eval::environment::{Environment};
use crate::eval::operation_eval;
use crate::lang::datatypes::{Binding, ObjectAccess, StructMetaData};
use crate::parse;
use crate::parse::ast_nodes::{Accessor, AssignData, AST_FALSE_LIT, AST_NIL_LIT, AST_TRUE_LIT, AstNode, CondData, ConsData, DefNode, ExprFuncCallData, ExprNode, FuncArg, FuncCallData, IfData, InstArgs, LambdaValue, ListAccData, LitNode, ObjectAssignData, ObjectCallData, ObjectValue, OpNode, PairValue, Param, WhileData};
use crate::parse::ast_nodes::AstNode::{DefinitionNode, ExpressionNode, OperationNode, ProgramNode};



macro_rules! nano_time {
    () => {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_nanos()
    };
}


pub fn repl_eval(env: &Rc<RefCell<Environment>>, loader: &RefCell<ClassLoader>, input: String) -> String {
    let start = nano_time!();
    let proc_time;
    let eval_time;
    let total_time;
    let tokens = parse::lexer::process(input);
    println!("\n{:?}\n", tokens);
    let ast = match tokens {
        Ok(tokens) => parse::parser::process(tokens),
        Err(msg) => return msg
    };


    println!("\n{:?}\n", ast);
    proc_time = nano_time!() - start;
    let eval_start = nano_time!();
    let result = match ast {
        Ok(ast) => eval(env, loader, ast).join("\n"),
        Err(error) => return error
    };
    eval_time = nano_time!() - eval_start;
    total_time = nano_time!() - start;
    println!("Processing: {}", proc_time);
    println!("Eval: {}", eval_time);
    println!("Total: {}", total_time);
    result
}


pub fn eval(env: &Rc<RefCell<Environment>>, loader: &RefCell<ClassLoader>, input: Vec<AstNode>) -> Vec<String> {
    let mut evaled = Vec::<String>::new();
    for node in input {
        match eval_node(env, loader, &node) {
            Ok(rtn) => match &*rtn {
                LiteralNode(any) => evaled.push(any.value().as_string()),
                invalid => evaled.push(format!("Fatal: Invalid Eval Return :{:?}", invalid))
            },
            Err(msg) => evaled.push(msg.clone())
        }
    }
    evaled
}


fn eval_node<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    node: &'a AstNode,
) -> Result<Cow<'a, AstNode>, String> {
    match node {
        ExpressionNode(expr) => eval_expression(env, loader, expr),
        LiteralNode(_) => Ok(Cow::Borrowed(node)),
        OperationNode(op) => eval_operation(env, loader, op),
        DefinitionNode(def) => eval_definition(env, loader, *def.clone()),
        ProgramNode(_) => Err("Fatal: Found Nested Program Node".to_string())
    }
}


fn eval_operation<'a>(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    node: &OpNode,
) -> Result<Cow<'a, AstNode>, String> {
    match node {
        OpNode::Addition(op_nodes) => Ok(operation_eval::add(eval_operands(env, loader, op_nodes)?)),
        OpNode::Subtraction(op_nodes) => Ok(operation_eval::subtract(eval_operands(env, loader, op_nodes)?)),
        OpNode::Multiplication(op_nodes) => Ok(operation_eval::multiply(eval_operands(env, loader, op_nodes)?)),
        OpNode::Division(op_nodes) => Ok(operation_eval::divide(eval_operands(env, loader, op_nodes)?)),
        OpNode::Modulo(op_nodes) => Ok(operation_eval::modulo(eval_operands(env, loader, op_nodes)?)),
        OpNode::Exponentiate(op_nodes) => Ok(operation_eval::exponentiate(eval_operands(env, loader, op_nodes)?)),
        OpNode::Increment(op_nodes) => Ok(operation_eval::increment(eval_operands(env, loader, op_nodes)?)),
        OpNode::Decrement(op_nodes) => Ok(operation_eval::decrement(eval_operands(env, loader, op_nodes)?)),
        OpNode::Or(op_nodes) => Ok(operation_eval::or(eval_operands(env, loader, op_nodes)?)),
        OpNode::And(op_nodes) => Ok(operation_eval::and(eval_operands(env, loader, op_nodes)?)),
        OpNode::Xor(op_nodes) => Ok(operation_eval::xor(eval_operands(env, loader, op_nodes)?)),
        OpNode::Nor(op_nodes) => Ok(operation_eval::nor(eval_operands(env, loader, op_nodes)?)),
        OpNode::Xnor(op_nodes) => Ok(operation_eval::xnor(eval_operands(env, loader, op_nodes)?)),
        OpNode::Nand(op_nodes) => Ok(operation_eval::nand(eval_operands(env, loader, op_nodes)?)),
        OpNode::Negate(op_nodes) => Ok(operation_eval::negate(eval_operands(env, loader, op_nodes)?)),
        OpNode::GreaterThan(op_nodes) => Ok(operation_eval::greater_than(eval_operands(env, loader, op_nodes)?)),
        OpNode::GreaterThanEqual(op_nodes) => Ok(operation_eval::greater_than_eq(eval_operands(env, loader, op_nodes)?)),
        OpNode::LessThan(op_nodes) => Ok(operation_eval::less_than(eval_operands(env, loader, op_nodes)?)),
        OpNode::LessThanEqual(op_nodes) => Ok(operation_eval::less_than_eq(eval_operands(env, loader, op_nodes)?)),
        OpNode::Equality(op_nodes) => Ok(operation_eval::ref_equality(eval_operands(env, loader, op_nodes)?)),
        OpNode::RefEquality(op_nodes) => Ok(operation_eval::value_equality(eval_operands(env, loader, op_nodes)?)),
        OpNode::RefNonEquality(op_nodes) => Ok(operation_eval::value_non_equality(eval_operands(env, loader, op_nodes)?))
    }
}


fn eval_operands(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    op_nodes: &Vec<AstNode>,
) -> Result<(bool, Vec<LitNode>), String> {
    let mut operands = Vec::<LitNode>::with_capacity(op_nodes.len());
    let mut is_float = false;
    for node in op_nodes {
        match eval_node(env, loader, &node)? {
            Cow::Borrowed(LiteralNode(val)) => {
                if matches!(val, LitNode::Float(_)) { is_float = true; }
                operands.push(val.clone());
            }
            Cow::Owned(LiteralNode(val)) => {
                if matches!(val, LitNode::Float(_)) { is_float = true; }
                operands.push(val);
            }
            _ => return Err("Error evaluating operands".to_string())
        }
    }
    Ok((is_float, operands))
}


fn eval_definition<'a>(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    node: DefNode,
) -> Result<Cow<'a, AstNode>, String> {
    match node {
        DefNode::Variable(var) => {
            let evaled = eval_node(env, loader, &var.value);
            if let Ok(evaled_var) = evaled {
                let binding = Binding::new_binding(evaled_var.deref(), &var.modifiers);
                let bind = env.borrow_mut().create_binding(var.name.clone(), binding?);
                if let Err(s) = bind {
                    Err(s.clone())
                } else { Ok(Cow::Owned(AstNode::new_bool_lit(true))) }
            } else { Err(format!("Binding did not eval to literal{:?}", evaled)) }
        }
        DefNode::Lambda(lam) => {
            Ok(Cow::Owned(AstNode::new_lambda_lit(lam, Rc::clone(&env))))
        }
        DefNode::Function(fun) => {
            let mods = fun.lambda.modifiers.clone();
            let lambda_lit = AstNode::new_lambda_lit(fun.lambda, Rc::clone(&env));
            let binding = Binding::new_binding(&lambda_lit, &mods);
            let bind = env.borrow_mut().create_binding(fun.name.clone(), binding?);
            if let Err(s) = bind {
                Err(s.clone())
            } else { Ok(Cow::Owned(AstNode::new_bool_lit(true))) }
        }
        DefNode::StructDef(def) => {
            let struct_def = StructMetaData::new_declaration(def.fields)?;
            loader.borrow_mut().new_class_def(def.name, ClassDef::Struct(struct_def))?;
            Ok(Cow::Owned(AstNode::new_bool_lit(true)))
        }
        DefNode::InstanceDef(inst) => {
            let evaled_args = if let Some(args) = inst.args {
                let mut vec = Vec::<InstArgs>::with_capacity(args.len());
                for arg in args {
                    let evaled_arg = match eval_node(env, loader, &arg.value)? {
                        Cow::Borrowed(b) => b.clone(),
                        Cow::Owned(o) => o
                    };
                    vec.push(InstArgs { name: arg.name, value: evaled_arg });
                }
                Some(vec)
            } else { None };

            let loader_ref = loader.borrow();
            let def = loader_ref.get_class_def(&inst.name)?;
            match def {
                ClassDef::Struct(s) => {
                    let inst = s.new_instance(inst.name, evaled_args)?;
                    return Ok(Cow::Owned(inst));
                }
                ClassDef::Class => { todo!() }
            }
        }
    }
}


fn eval_expression<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    node: &'a ExprNode,
) -> Result<Cow<'a, AstNode>, String> {
    match node {
        ExprNode::Assignment(data) => eval_assignment(env, loader, data),
        ExprNode::MultiExpr(data) => eval_multi_expr(env, loader, data),
        ExprNode::PrintExpr(data) => eval_print_expr(env, loader, data),
        ExprNode::IfExpr(data) => eval_if_expr(env, loader, &data),
        ExprNode::CondExpr(data) => eval_cond_expr(env, loader, data),
        ExprNode::WhileLoop(data) => eval_while_expr(env, loader, data),
        ExprNode::ConsExpr(data) => eval_cons_expr(env, loader, data),
        ExprNode::PairList(data) => eval_pair_list_expr(env, loader, data),
        ExprNode::ListAccess(data) => eval_list_acc_expr(env, loader, data),
        ExprNode::FuncCall(data) => eval_func_call_expr(env, loader, data),
        ExprNode::LiteralCall(data) => eval_lit_call_expr(env, loader, data),
        ExprNode::ExprFuncCal(data) => eval_expr_func_call(env, loader, data),
        ExprNode::ObjectCall(data) => eval_object_call(env, loader, data),
        ExprNode::ObjectAssignment( data) => { eval_object_assignment(env, loader, data) }
    }
}


fn eval_assignment<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    expr: &'a AssignData,
) -> Result<Cow<'a, AstNode>, String> {
    let evaled = eval_node(env, loader, &expr.value)?;
    let rtn = env.borrow_mut().update_binding(&expr.name, evaled.deref())?;
    Ok(Cow::Owned(rtn))
}


fn eval_object_assignment<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    expr: &'a  ObjectAssignData,
) -> Result<Cow<'a, AstNode>, String> {
    todo!()
    // let value = match eval_node(env, loader, &expr.value)? {
    //     Cow::Borrowed(b) => b.clone(),
    //     Cow::Owned(o) => o
    // };
    // 
    // let mut obj =
    // match  env.borrow().get_literal(&expr.access.name)?.borrow().deref().clone() {
    //     LiteralNode(LitNode::Object(mut obj)) => {
    //         recur_object_assignment(env, loader, &mut obj, &mut expr.access.accessors.clone(), value)
    //     }
    //     _ => { return Err("dfsdfsdfsdf".to_string()); }
    // };
    // Ok(Cow::Owned(AST_TRUE_LIT))
}




fn recur_object_assignment<'a>(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    call:  &mut ObjectValue,
    accessors: &mut LinkedList<Accessor>,
    evaled_value: AstNode,
) -> Result<Cow<'a, crate::parse::ast_nodes::AstNode>, String> {
    todo!()
    // let acc = accessors.pop_front().ok_or_else(|| "LookupFailed".to_string())?;
    // 
    // if acc.is_field {
    //     if accessors.is_empty() {
    //         call.set_field(&acc.name, &evaled_value)?;
    //         return Ok(Cow::Owned(evaled_value));
    //     } else {
    //         match Rc::clone(call.get_field(&acc.name)?.borrow()) {
    //             LiteralNode(LitNode::Object(ref mut obj)) => {
    //                 return recur_object_assignment(env, loader, &mut obj, accessors, evaled_value);
    //             }
    //             _ => Err("dfsdf".to_string())
    //       
    //         }
    //     }
    // } else {
    //     let method = call.get_method(&acc.name)?;
    //     if let LiteralNode(LitNode::Lambda(lambda)) = method.deref() {
    //         let result = eval_lambda_call(env, loader, &lambda, acc.args)?;
    //         match result {
    //             Cow::Borrowed(b) => Ok(Cow::Owned(b.clone())),
    //             Cow::Owned(o) => Ok(Cow::Owned(o))
    //         }
    //     } else {
    //         Err("Fatal: Expected lambda from method lookup".to_string())
    //     }
    // }
}


fn eval_multi_expr<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    expr: &'a Vec<AstNode>,
) -> Result<Cow<'a, AstNode>, String> {
    if expr.is_empty() { return Ok(Cow::Owned(AST_NIL_LIT)); }
    let mut iter = expr.iter().peekable();

    let result = loop {
        let next = iter.next().unwrap();
        if iter.peek() == None {
            break eval_node(env, loader, next)?;
        } else { eval_node(env, loader, next)?; }
    };
    Ok(result)
}


fn eval_print_expr<'a>(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    expr: &AstNode,
) -> Result<Cow<'a, AstNode>, String> {
    match eval_node(env, loader, &expr)? {
        Cow::Borrowed(LiteralNode(lit)) => {
            println!("{}", lit.value().as_string());
            Ok(Cow::Borrowed(&AST_NIL_LIT))
        }
        Cow::Owned(LiteralNode(lit)) => {
            println!("{}", lit.value().as_string());
            Ok(Cow::Borrowed(&AST_NIL_LIT))
        }
        _ => Err("Print value not literal".to_string())
    }
}


fn eval_if_expr<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    expr: &'a IfData,
) -> Result<Cow<'a, AstNode>, String> {
    if let Ok(LiteralNode(lit)) = eval_node(env, loader, &expr.if_branch.cond_node).as_deref() {
        if lit.value().as_bool() {
            eval_node(env, loader, &expr.if_branch.then_node)
        } else if expr.else_branch.is_some() {
            match eval_node(env, loader, expr.else_branch.as_ref().unwrap())? {
                Cow::Borrowed(b) => Ok(Cow::Owned(b.clone())),
                Cow::Owned(o) => Ok(Cow::Owned(o))
            }
        } else { Ok(Cow::Borrowed(&AST_TRUE_LIT)) }
    } else { Err("If condition did not evaluate to boolean".to_string()) }
}


fn eval_cond_expr<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    expr: &'a CondData,
) -> Result<Cow<'a, AstNode>, String> {
    for e in &expr.cond_branches {
        if let Ok(LiteralNode(lit)) = eval_node(env, loader, &e.cond_node).as_deref() {
            if lit.value().as_bool() {
                return eval_node(env, loader, &e.then_node);
            } else { continue; }
        } else { return Err("Cond condition did  not evaluate to boolean".to_string()); }
    }

    if let Some(else_branch) = &expr.else_branch {
        eval_node(env, loader, &else_branch)
    } else { Ok(Cow::Borrowed(&AST_FALSE_LIT)) }
}


fn eval_while_expr<'a>(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    expr: &WhileData,
) -> Result<Cow<'a, AstNode>, String> {
    if expr.is_do { eval_node(env, loader, &expr.body)?; }
    loop {
        match eval_node(env, loader, &expr.condition).as_deref() {
            Ok(LiteralNode(lit)) if lit.value().as_bool() => { eval_node(env, loader, &expr.body)?; }
            Ok(_) => break,
            Err(err) => { return Err(err.clone()); }
        }
    }
    Ok(Cow::Borrowed(&AST_TRUE_LIT))
}


fn eval_cons_expr<'a>(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    expr: &ConsData,
) -> Result<Cow<'a, AstNode>, String> {
    let car = eval_node(env, loader, &expr.car)?;
    let cdr = eval_node(env, loader, &expr.cdr)?;
    Ok(Cow::Owned(PairValue::from_ast(car.as_ref().clone(), cdr.as_ref().clone())?))
}


fn eval_pair_list_expr<'a>(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    expr: &Vec<AstNode>,
) -> Result<Cow<'a, AstNode>, String> {
    let mut head = AST_NIL_LIT;
    for element in expr.into_iter().rev() {
        match eval_node(env, loader, &element)? {
            Cow::Borrowed(borrowed) => head = PairValue::from_ast(borrowed.clone(), head)?,
            Cow::Owned(owned) => head = PairValue::from_ast(owned, head)?
        };
    }
    Ok(Cow::Owned(head))
}


fn eval_list_acc_expr<'a>(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    expr: &ListAccData,
) -> Result<Cow<'a, AstNode>, String> {
    let evaled_node = eval_node(env, loader, &expr.list);
    let list = if let LiteralNode(LitNode::Pair(pair)) = evaled_node.as_deref()? {
        pair
    } else { return Err("Attempted list access on non-list literal".to_string()); };

    if expr.index_expr.is_some() {
        let index = if let LiteralNode(lit)
            = eval_node(env, loader, &expr.index_expr.as_ref().unwrap()).as_deref()?
        {
            lit.value().as_int()
        } else { return Err("Index did not evaluate to literal".to_string()); };
        if index == 0 { return Ok(Cow::Owned(LiteralNode(*list.car.clone()))); }

        let mut result = list;
        for _ in 0..index { // TODO fix this
            result = if let LitNode::Pair(pair) = &*result.cdr {
                &pair
            } else { return Err("Index not contained in list".to_string()); }
        }
        return Ok(Cow::Owned(LiteralNode(*result.car.clone())));
    }

    if let Some(pattern) = &expr.pattern {
        let mut curr: &PairValue = &list;
        let mut chars = pattern.chars().peekable();

        while let Some(acc) = chars.next() {
            match (acc, &*curr.car, &*curr.cdr) {
                ('f', LitNode::Pair(pair), _) if chars.peek().is_some() => curr = pair,
                ('r', _, LitNode::Pair(pair)) if chars.peek().is_some() => curr = pair,
                ('f', _, _) if chars.peek().is_none() => return Ok(Cow::Owned(LiteralNode(*curr.car.clone()))),
                ('r', _, _) if chars.peek().is_none() => return Ok(Cow::Owned(LiteralNode(*curr.cdr.clone()))),
                _ => return Err("Invalid access pattern or non-pair encountered before the end".to_string()),
            }
        }
        Err("Pattern empty or did not lead to a valid access.".to_string())
    } else {
        Err("Access pattern required.".to_string())
    }
}


fn eval_func_call_expr<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    call: &'a FuncCallData,
) -> Result<Cow<'a, AstNode>, String> {
    if let Ok(binding) = env.borrow().get_literal(&call.name) {
        if let LiteralNode(LitNode::Lambda(lambda)) = &binding.deref() {
            let new_env = Environment::of(Rc::clone(&lambda.env));
            if lambda.def.parameters.is_some() && call.arguments.is_some() {
                map_param_to_env(
                    env,
                    loader,
                    lambda.def.parameters.as_ref().unwrap(),
                    &call.arguments.as_ref().unwrap(),
                    &new_env,
                )?;
            } else if lambda.def.parameters.is_none() != call.arguments.is_none() {
                return Err("Parameter-argument mismatch".to_string());
            }

            match eval_node(&new_env, loader, &lambda.def.body)? {
                Cow::Borrowed(b) => Ok(Cow::Owned(b.clone())),
                Cow::Owned(o) => Ok(Cow::Owned(o))
            }
        } else { Err(format!("Binding is not a lambda: {:?}", binding)) }
    } else { Err(format!("Binding not found: {}", call.name)) }
}


fn eval_object_call<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    call: &ObjectCallData,
) -> Result<Cow<'a, AstNode>, String> {
    if let Ok(binding) = env.borrow().get_literal(&call.name) {
        if let LiteralNode(LitNode::Object(obj)) = &binding.deref() {
            recur_object_call(env, loader, obj, &mut call.accessors.clone())
        } else { Err(format!("Accessors applied to non-object literal: {}", &call.name)) }
    } else { Err(format!("Failed to locate binding for: {}", call.name)) }
}


fn eval_expr_func_call<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    call: &'a ExprFuncCallData,
) -> Result<Cow<'a, AstNode>, String> {
    if let LiteralNode(LitNode::Lambda(lambda)) = eval_node(env, loader, &call.expr).as_deref()? {
        let new_env = Environment::of(Rc::clone(&lambda.env));

        if lambda.def.parameters.is_some() && call.arguments.is_some() {
            map_param_to_env(
                env,
                loader,
                lambda.def.parameters.as_ref().unwrap(),
                &call.arguments.as_ref().unwrap(),
                &new_env,
            )?;
        } else if lambda.def.parameters.is_none() != call.arguments.is_none() {
            return Err("Parameter-argument mismatch".to_string());
        }

        match eval_node(&new_env, loader, &lambda.def.body)? {
            Cow::Borrowed(b) => Ok(Cow::Owned(b.clone())),
            Cow::Owned(o) => Ok(Cow::Owned(o))
        }
    } else { Err(format!("Expression at start of call did not evaluate to a lambda: {:?}", &call.expr)) }
}


fn eval_lambda_call<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    lambda: &'a LambdaValue,
    args: Option<Vec<FuncArg>>,
) -> Result<Cow<'a, AstNode>, String> {
    let new_env = Environment::of(Rc::clone(&lambda.env));
    if lambda.def.parameters.is_some() && args.is_some() {
        map_param_to_env(
            env,
            loader,
            lambda.def.parameters.as_ref().unwrap(),
            &args.as_ref().unwrap(),
            &new_env, // new_env is now directly passed as Rc<RefCell<Environment>>
        )?;
    } else if lambda.def.parameters.is_none() != args.is_none() {
        return Err("Parameter-argument mismatch".to_string());
    }

    match eval_node(&new_env, loader, &lambda.def.body)? {
        Cow::Borrowed(b) => Ok(Cow::Owned(b.clone())),
        Cow::Owned(o) => Ok(Cow::Owned(o))
    }
}


fn recur_object_call<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    call: &dyn ObjectAccess,
    accessors: &mut LinkedList<Accessor>,
) -> Result<Cow<'a, AstNode>, String> {
    todo!()
    // let acc = if let Some(acc) = accessors.pop_front() {
    //     acc
    // } else { return Err("LookupFailed".to_string()); };
    // 
    // if acc.is_field {
    //     let field = call.get_field(&acc.name)?;
    //     return if accessors.is_empty() {
    //         Ok(Cow::Owned(field.clone()))
    //     } else if let LiteralNode(LitNode::Object(obj)) = field.borrow().deref() {
    //         recur_object_call(env, loader, obj, accessors)
    //     } else { Err(format!("Accessors cannot be applied to non-object: {}", &acc.name)) };
    // } else if let LiteralNode(LitNode::Lambda(lambda)) = call.get_method(&acc.name)?.deref() {
    //     match eval_lambda_call(env, loader, &lambda, acc.args)? {
    //         Cow::Borrowed(b) => Ok(Cow::Owned(b.clone())),
    //         Cow::Owned(o) => Ok(Cow::Owned(o))
    //     }
    // } else { Err("Fatal: Expected lambda from method lookup".to_string()) }
}


fn map_param_to_env(
    curr_env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    params: &Vec<Param>,
    args: &Vec<FuncArg>,
    func_env: &Rc<RefCell<Environment>>,
) -> Result<(), String> {
    if params.len() == args.len() {
        for i in 0..args.len() {
            let arg = &args[i].value;
            let ast = eval_node(curr_env, loader, arg)?;
            let binding = Binding::new_binding(&ast, &None)?;
            func_env.borrow_mut().create_binding(params[i].name.clone(), binding)?;
        }
        Ok(())
    } else { Err("Arg len mismatch".to_string()) }
}


fn eval_lit_call_expr<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    name: &String,
) -> Result<Cow<'a, AstNode>, String> {
    if let Ok(lit) = env.borrow().get_literal(name) {
        Ok(Cow::Owned(lit.deref().clone()))
    } else {
        Err(format!("Failed to find binding for: {}", name))
    }
}