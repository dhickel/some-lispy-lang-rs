use std::borrow::Cow;
use std::cell::{RefCell, UnsafeCell};
use std::collections::LinkedList;
use std::fs;
use std::ops::Deref;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};
use lasso::{Spur};
use rand::{Rng, thread_rng};
use AstNode::LiteralNode;
use crate::eval::class_loader::{ClassDef, ClassLoader};
use crate::eval::environment::{Environment};
use crate::eval::operation_eval;
use crate::lang::datatypes::{Binding, ClassMetaData, ObjectAccess, StructMetaData};
use crate::parse;
use crate::parse::ast_nodes::{Accessor, AssignData, AstNode, CondData, ConsData, DefNode, DirectInst, ExprFuncCallData, ExprNode, FloatValue, FuncArg, FuncCallData, IfData, InstArgs, IntValue, LambdaValue, ListAccData, LitNode, ObjectAssignData, ObjectCallData, ObjectValue, OpNode, PairValue, Param, WhileData};
use crate::parse::ast_nodes::AstNode::{DefinitionNode, ExpressionNode, OperationNode, ProgramNode};
use crate::parse::util;



macro_rules! nano_time {
    () => {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_nanos()
    };
}

pub fn file_eval(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    file_path: &String)
    -> String {
    let time = nano_time!();
    match fs::read_to_string(file_path) {
        Ok(content) => {
            println!("content:{}", content);
            repl_eval(env, loader, content);
            format!("Loaded file: {}, Processing Time: {}", &file_path, nano_time!() - time)
        }

        Err(s) => {
            println!("error");
            s.to_string()
        }
    }
}


pub fn repl_eval(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    input: String,
) -> String {
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


pub fn eval(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    input: Vec<AstNode>,
) -> Vec<String> {
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


pub fn eval_node<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    node: &'a AstNode,
) -> Result<Cow<'a, AstNode>, String> {
    match node {
        ExpressionNode(expr) => eval_expression(env, loader, expr),
        LiteralNode(_) => Ok(Cow::Borrowed(node)),
        OperationNode(op) => eval_operation(env, loader, op),
        DefinitionNode(def) => eval_definition(env, loader, def.deref().clone()),
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
) -> Result<(bool, Vec<Rc<LitNode>>), String> {
    let mut operands = Vec::<Rc<LitNode>>::with_capacity(op_nodes.len());
    let mut is_float = false;

    for node in op_nodes {
        match eval_node(env, loader, &node)? {
            Cow::Borrowed(LiteralNode(val)) => {
                if matches!(val.as_ref(), LitNode::Float(_)) { is_float = true; }
                operands.push(Rc::clone(val));
            }
            Cow::Owned(LiteralNode(val)) => {
                if matches!(val.as_ref(), LitNode::Float(_)) { is_float = true; }
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
                let binding = Binding::new_binding_ast(
                    evaled_var.deref(), &var.modifiers, false, true,
                );
                let bind = env.borrow_mut().create_binding(var.name, binding?)?;
                Ok(Cow::Owned(bind))
            } else { Err(format!("Binding did not eval to literal: {:?}", &evaled)) }
        }

        DefNode::Lambda(lam) => {
            Ok(Cow::Owned(AstNode::new_lambda_lit(lam, Rc::clone(&env))))
        }

        DefNode::Function(fun) => {
            let mods = fun.lambda.modifiers.clone();
            let lambda_lit = AstNode::new_lambda_lit(fun.lambda, Rc::clone(&env));
            let binding = Binding::new_binding_ast(&lambda_lit, &mods, false, true);
            let bind = env.borrow_mut().create_binding(fun.name, binding?)?;
            Ok(Cow::Owned(bind))
        }

        DefNode::StructDef(def) => {
            let struct_def = StructMetaData::new_declaration(def.fields)?;
            loader.borrow_mut().new_class_def(def.name, ClassDef::Struct(struct_def))?;
            Ok(Cow::Owned(AstNode::new_bool_lit(true)))
        }

        DefNode::ClassDef(def) => {
            let class_def = ClassMetaData::new_declaration(def.clone())?;
            loader.borrow_mut().new_class_def(def.name, ClassDef::Class(class_def))?;
            Ok(Cow::Owned(AstNode::new_bool_lit(true)))
        }
    }
}


fn eval_direct_instance<'a>(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    inst: &DirectInst,
) -> Result<Cow<'a, AstNode>, String> {
    let evaled_args = if let Some(args) = &inst.args {
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

    return match def {
        ClassDef::Struct(s) => {
            let inst = s.new_instance(inst.name, evaled_args)?;
            Ok(Cow::Owned(inst))
        }
        ClassDef::Class(c) => {
            let inst = c.new_instance(evaled_args, &None, env, loader)?;
            Ok(Cow::Owned(inst))
        }
    };
}


fn eval_instance<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    node: &'a FuncCallData,
) -> Result<Cow<'a, AstNode>, String> {

    let loader_ref = loader.borrow();
    let def = loader_ref.get_class_def(&node.name)?;

    return match def {
        ClassDef::Struct(s) => {
           todo!("Stuct init method not implemented")
        }
        ClassDef::Class(c) => {
            let inst = c.new_instance(None, &node.arguments, env, loader)?;
            Ok(Cow::Owned(inst))
        }
    };
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
        ExprNode::LiteralCall(data) => eval_lit_call_expr(env, data),
        ExprNode::ExprFuncCal(data) => eval_expr_func_call(env, loader, data),
        ExprNode::ObjectCall(data) => eval_object_call(env, loader, data),
        ExprNode::ObjectAssignment(data) => eval_object_assignment(env, loader, data),
        ExprNode::GenRand(is_float, lower, upper) => eval_rand(env, loader, *is_float, lower, upper),
        ExprNode::DirectInst(data) => eval_direct_instance(env, loader, data),
        ExprNode::InitInst(data) => eval_instance(env, loader, data) 
    }
}


fn eval_rand<'a>(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    is_float: bool,
    lower: &AstNode,
    upper: &AstNode,
) -> Result<Cow<'a, AstNode>, String> {
    let l_eval = eval_node(env, loader, lower)?;
    let u_eval = eval_node(env, loader, upper)?;

    let l_val = if let LiteralNode(lit) = &l_eval.as_ref() {
        lit.value()
    } else { return Err("Expected literal value".to_string()); };

    let u_val = if let LiteralNode(lit) = &u_eval.as_ref() {
        lit.value()
    } else { return Err("Expected literal value".to_string()); };

    if is_float {
        let val = thread_rng().gen_range(l_val.as_float()..=u_val.as_float());
        Ok(Cow::Owned(LiteralNode(Rc::new(LitNode::Float(FloatValue(val))))))
    } else {
        let val = thread_rng().gen_range(l_val.as_int()..=u_val.as_int());
        Ok(Cow::Owned(LiteralNode(Rc::new(LitNode::Integer(IntValue(val))))))
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
    expr: &'a ObjectAssignData,
) -> Result<Cow<'a, AstNode>, String> {
    let value = match eval_node(env, loader, &expr.value)? {
        Cow::Borrowed(b) => b.clone(),
        Cow::Owned(o) => o
    };

    let binding = env.borrow().get_literal(&expr.access.name)?;
    if let LitNode::Object(obj) = binding.as_ref() {
        if expr.access.accessors.len() == 1 {
            let name = if let Some(acc) = expr.access.accessors.front() {
                &acc.name
            } else {
                return Err(format!("Expected at least one accessor to set for: {:?}", &expr.access.name));
            };
            set_field_unsafe(binding, name, value)
        } else {
            recur_object_assignment(env, loader, obj, &mut expr.access.accessors.clone(), value)
        }
    } else { Err(format!("Accessors applied to non-object literal: {:?}", &&expr.access.name)) }
}


// Unsafe: This needs mutability only in the context of setting the field. This avoids using 
// a refcell since the bindings need to be also wrapped in an rc for performance reasons to avoid cloning.
fn set_field_unsafe<'a>(
    object: Rc<LitNode>,
    name: &Spur,
    value: AstNode,
) -> Result<Cow<'a, AstNode>, String> {
    let field_mut_ptr = unsafe {
        // Transmute the Rc to Rc<UnsafeCell<LitNode>> to get a mutable pointer
        let field_uc: &Rc<UnsafeCell<LitNode>> = std::mem::transmute(&object);
        // Get a mutable reference from UnsafeCell
        &mut *field_uc.get()
    };

    if let LitNode::Object(obj) = field_mut_ptr {
        let rtn = obj.set_field(name, &value)?;
        Ok(Cow::Owned(LiteralNode(rtn)))
    } else { Err(format!("Access did not evaluate to object: {:?}", field_mut_ptr)) }
}


fn recur_object_assignment<'a>(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    call: &ObjectValue,
    accessors: &mut LinkedList<Accessor>,
    evaled_value: AstNode,
) -> Result<Cow<'a, crate::parse::ast_nodes::AstNode>, String> {
    let acc = if let Some(acc) = accessors.pop_front() {
        acc
    } else { return Err("LookupFailed".to_string()); };


    if accessors.len() == 1 || accessors.is_empty() {
        return if let Ok(field) = call.get_field(&acc.name) {
            let set_name = accessors.pop_front().unwrap().name;
            set_field_unsafe(field, &set_name, evaled_value)
        } else { Err(format!("Failed to find field: {:?}", acc.name)) };
    }

    if acc.is_field {
        return match call.get_field(&acc.name) {
            Ok(field) => {
                if accessors.is_empty() {
                    Err("Fatal: we should reach this its bad code lol".to_string())
                } else if let LitNode::Object(obj) = field.as_ref() {
                    recur_object_assignment(env, loader, obj, accessors, evaled_value)
                } else {
                    Err(format!("Accessors cannot be applied to non-object: {:?}", &acc.name))
                }
            }
            _ => { Err(format!("Field not found: {:?}", &acc.name)) }
        };
    } else if let LitNode::Lambda(lambda) = call.get_method(&acc.name)? {
        match eval_lambda_call(env, loader, &lambda, acc.args)? {
            Cow::Borrowed(b) => Ok(Cow::Owned(b.clone())),
            Cow::Owned(o) => Ok(Cow::Owned(o))
        }
    } else { Err("Fatal: Expected lambda from method lookup".to_string()) }
}


fn eval_multi_expr<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    expr: &'a [AstNode],
) -> Result<Cow<'a, AstNode>, String> {
    if expr.is_empty() { return Ok(Cow::Owned(AstNode::new_nil_lit())); }
    let mut iter = expr.iter().peekable();

    let result = loop {
        let next = iter.next().unwrap();
        if iter.peek().is_none() {
            break eval_node(env, loader, next)?;
        } else {
            eval_node(env, loader, next)?;
        }
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
            Ok(Cow::Owned(AstNode::new_nil_lit()))
        }
        Cow::Owned(LiteralNode(lit)) => {
            println!("{}", lit.value().as_string());
            Ok(Cow::Owned(AstNode::new_nil_lit()))
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
        } else { Ok(Cow::Owned(AstNode::new_bool_lit(false))) }
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
        eval_node(env, loader, else_branch)
    } else { Ok(Cow::Owned(AstNode::new_bool_lit(false))) }
}


fn eval_while_expr<'a>(
    env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    expr: &WhileData,
) -> Result<Cow<'a, AstNode>, String> {
    if expr.is_do { eval_node(env, loader, &expr.body)?; }

    let mut cond = match eval_node(env, loader, &expr.condition)?.as_ref() {
        LiteralNode(cond) => cond.value().as_bool(),
        _ => { return Err(format!("Expression in while loop did not evaluate to literal: {:?}", expr.condition)); }
    };

    if !expr.is_do && !cond { return Ok(Cow::Owned(AstNode::new_bool_lit(false))); }

    while cond {
        eval_node(env, loader, &expr.body)?;
        cond = match eval_node(env, loader, &expr.condition)?.as_ref() {
            LiteralNode(cond) => cond.value().as_bool(),
            _ => { return Err(format!("Expression in while loop did not evaluate to literal: {:?}", expr.condition)); }
        };
    }
    Ok(Cow::Owned(AstNode::new_bool_lit(true)))
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
    let mut head = AstNode::new_nil_lit();
    for element in expr.into_iter().rev() {
        match eval_node(env, loader, element)? {
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
    let evaled_node = eval_node(env, loader, &expr.list)?;
    let list = if let LiteralNode(rc_lit_node) = evaled_node.as_ref() {
        match rc_lit_node.as_ref() {
            LitNode::Pair(pair) => pair,
            _ => return Err("Attempted list access on non-list literal".to_string())
        }
    } else { return Err("Attempted list access on non-list literal".to_string()); };


    if expr.index_expr.is_some() {
        let index = match eval_node(env, loader, expr.index_expr.as_ref().unwrap()).as_deref()? {
            LiteralNode(lit) => lit.value().as_int(),
            _ => { return Err("Index did not evaluate to literal".to_string()); }
        };

        if index == 0 { return Ok(Cow::Owned(LiteralNode(Rc::clone(&list.car)))); }

        let mut result = list;
        for _ in 0..index { // TODO fix this
            result = if let LitNode::Pair(pair) = &*result.cdr {
                &pair
            } else { return Err("Index not contained in list".to_string()); }
        }
        return Ok(Cow::Owned(LiteralNode(Rc::clone(&result.car))));
    }

    if let Some(pattern) = &expr.pattern {
        let mut curr: &PairValue = &list;
        let resolved = util::SCACHE.resolve(pattern);
        let mut chars = resolved.chars().peekable();

        while let Some(acc) = chars.next() {
            match (acc, &*curr.car, &*curr.cdr) {
                ('f', LitNode::Pair(pair), _) if chars.peek().is_some() => curr = pair,
                ('r', _, LitNode::Pair(pair)) if chars.peek().is_some() => curr = pair,
                ('f', _, _) if chars.peek().is_none() => return Ok(Cow::Owned(LiteralNode(Rc::clone(&curr.car)))),
                ('r', _, _) if chars.peek().is_none() => return Ok(Cow::Owned(LiteralNode(Rc::clone(&curr.cdr)))),
                _ => return Err("Invalid access pattern or non-pair encountered before the end".to_string()),
            }
        }
        Err("Pattern empty or did not lead to a valid access.".to_string())
    } else { Err("Access pattern required.".to_string()) }
}


fn eval_func_call_expr<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    call: &'a FuncCallData,
) -> Result<Cow<'a, AstNode>, String> {
    if let Ok(binding) = env.borrow().get_literal(&call.name) {
        if let LitNode::Lambda(lambda) = binding.as_ref() {
            let new_env = Environment::of_nested(Rc::clone(&lambda.env));

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
    } else { Err(format!("Binding not found: {:?}", call.name)) }
}


fn eval_object_call<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    call: &ObjectCallData,
) -> Result<Cow<'a, AstNode>, String> {
    if let Ok(binding) = env.borrow().get_literal(&call.name) {
        if let LitNode::Object(obj) = binding.as_ref() {
            recur_object_call(env, loader, obj, &mut call.accessors.clone())
        } else {
            Err(format!("Accessors applied to non-object literal: {:?}", &call.name))
        }
    } else { Err(format!("Failed to locate binding for: {:?}", call.name)) }
}


fn eval_expr_func_call<'a>(
    env: &'a Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    call: &'a ExprFuncCallData,
) -> Result<Cow<'a, AstNode>, String> {
    if let Some(LitNode::Lambda(lambda)) = eval_node(env, loader, &call.expr)
        .ok()
        .as_deref()
        .and_then(|node| match node {
            LiteralNode(lit) => Some(lit.as_ref()),
            _ => None,
        })
    {
        let new_env = Environment::of_nested(Rc::clone(&lambda.env));
        if lambda.def.parameters.is_some() && call.arguments.is_some() {
            map_param_to_env(
                env,
                loader,
                lambda.def.parameters.as_ref().unwrap(),
                call.arguments.as_ref().unwrap(),
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
    let new_env = Environment::of_nested(Rc::clone(&lambda.env));
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
    let acc = if let Some(acc) = accessors.pop_front() {
        acc
    } else { return Err("LookupFailed".to_string()); };

    if acc.is_field {
        if let Ok(field) = call.get_field(&acc.name) {
            return if accessors.is_empty() {
                Ok(Cow::Owned(LiteralNode(field)))
            } else if let LitNode::Object(obj) = field.as_ref() {
                recur_object_call(env, loader, obj, accessors)
            } else {
                Err(format!("Accessors cannot be applied to non-object: {:?}", &acc.name))
            };
        } else { Err(format!("Field not found: {:?}", &acc.name)) }
    } else if let LitNode::Lambda(lambda) = call.get_method(&acc.name)? {
        match eval_lambda_call(env, loader, &lambda, acc.args)? {
            Cow::Borrowed(b) => Ok(Cow::Owned(b.clone())),
            Cow::Owned(o) => Ok(Cow::Owned(o))
        }
    } else { Err("Fatal: Expected lambda from method lookup".to_string()) }
}


pub fn map_param_to_env(
    curr_env: &Rc<RefCell<Environment>>,
    loader: &RefCell<ClassLoader>,
    params: &[Param],
    args: &[FuncArg],
    func_env: &Rc<RefCell<Environment>>,
) -> Result<(), String> {
    if params.len() == args.len() {
        for i in 0..args.len() {
            let arg = &args[i].value;
            let ast = eval_node(curr_env, loader, arg)?;
            let binding = Binding::new_binding_ast(&ast, &None, false, true)?;
            func_env.borrow_mut().create_binding(params[i].name, binding)?;
        }
        Ok(())
    } else { Err("Arg len mismatch".to_string()) }
}


fn eval_lit_call_expr<'a>(
    env: &'a Rc<RefCell<Environment>>,
    name: &Spur,
) -> Result<Cow<'a, AstNode>, String> {
    if let Ok(lit) = env.borrow().get_literal(name) {
        Ok(Cow::Owned(LiteralNode(lit)))
    } else {
        Err(format!("Failed to find binding for: {:?}", name))
    }
}