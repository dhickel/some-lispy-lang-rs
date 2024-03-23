use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};
use AstNode::LiteralNode;
use crate::eval::environment::{Binding, Environment};
use crate::eval::operation_eval;
use crate::parse;
use crate::parse::ast_nodes::{AssignData, AST_FALSE_LIT, AST_NIL_LIT, AST_TRUE_LIT, AstNode, CondData, ConsData, DefLambdaData, DefNode, ExprNode, FuncArg, FuncCallData, IfData, LambdaValue, ListAccData, LitNode, OpNode, PairValue, Param, WhileData};
use crate::parse::ast_nodes::AstNode::{DefinitionNode, ExpressionNode, OperationNode, ProgramNode};



macro_rules! nano_time {
    () => {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_nanos() // Returns the number of nanoseconds since the Unix epoch
    };
}


pub fn repl_eval(env: &Rc<RefCell<Environment>>, input: String) -> String {
    let start = nano_time!();
    let proc_time;
    let eval_time;
    let total_time;
    let tokens = parse::lexer::process(input);

    let ast = match tokens {
        Ok(tokens) => parse::parser::process(tokens),
        Err(msg) => return msg
    };

    println!("\n{:?}\n", ast);
    proc_time = nano_time!() - start;
    let eval_start = nano_time!();
    let result = match ast {
        Ok(ast) => eval(env, ast).join("\n"),
        Err(error) => return error
    };
    eval_time = nano_time!() - eval_start;
    total_time = nano_time!() - start;
    println!("Processing: {}", proc_time);
    println!("Eval: {}", eval_time);
    println!("Total: {}", total_time);
    result
}

pub fn eval(env: &Rc<RefCell<Environment>>, input: Vec<AstNode>) -> Vec<String> {
    let mut evaled = Vec::<String>::new();
    for node in input {
        match eval_node(env, &node) {
            Ok(rtn) => match &*rtn {
                AstNode::LiteralNode(any) => evaled.push(any.value().as_string()),
                _ => evaled.push("Fatal: Invalid Eval Return".to_string())
            },
            Err(msg) => evaled.push(msg.clone())
        }
    }
    evaled
}

fn eval_node<'a>(env: &'a Rc<RefCell<Environment>>, node: &'a AstNode) -> Result<Cow<'a, AstNode>, String> {
    match node {
        ExpressionNode(expr) => eval_expression(env, expr),
        LiteralNode(_) => Ok(Cow::Borrowed(node)),
        OperationNode(op) => eval_operation(env, op.clone()),
        DefinitionNode(def) => eval_definition(env, *def.to_owned()),
        ProgramNode(_) => Err("Fatal: Found Nested Program Node".to_string())
    }
}

fn eval_operation<'a>(env: &Rc<RefCell<Environment>>, node: OpNode) -> Result<Cow<'a, AstNode>, String> {
    match node {
        OpNode::Addition(op_nodes) => Ok(operation_eval::add(eval_operands(env, op_nodes)?)),
        OpNode::Subtraction(op_nodes) => Ok(operation_eval::subtract(eval_operands(env, op_nodes)?)),
        OpNode::Multiplication(op_nodes) => Ok(operation_eval::multiply(eval_operands(env, op_nodes)?)),
        OpNode::Division(op_nodes) => Ok(operation_eval::divide(eval_operands(env, op_nodes)?)),
        OpNode::Modulo(op_nodes) => Ok(operation_eval::modulo(eval_operands(env, op_nodes)?)),
        OpNode::Exponentiate(op_nodes) => Ok(operation_eval::exponentiate(eval_operands(env, op_nodes)?)),
        OpNode::Increment(op_nodes) => Ok(operation_eval::increment(eval_operands(env, op_nodes)?)),
        OpNode::Decrement(op_nodes) => Ok(operation_eval::decrement(eval_operands(env, op_nodes)?)),
        OpNode::Or(op_nodes) => Ok(operation_eval::or(eval_operands(env, op_nodes)?)),
        OpNode::And(op_nodes) => Ok(operation_eval::and(eval_operands(env, op_nodes)?)),
        OpNode::Xor(op_nodes) => Ok(operation_eval::xor(eval_operands(env, op_nodes)?)),
        OpNode::Nor(op_nodes) => Ok(operation_eval::nor(eval_operands(env, op_nodes)?)),
        OpNode::Xnor(op_nodes) => Ok(operation_eval::xnor(eval_operands(env, op_nodes)?)),
        OpNode::Nand(op_nodes) => Ok(operation_eval::nand(eval_operands(env, op_nodes)?)),
        OpNode::Negate(op_nodes) => Ok(operation_eval::negate(eval_operands(env, op_nodes)?)),
        OpNode::GreaterThan(op_nodes) => Ok(operation_eval::greater_than(eval_operands(env, op_nodes)?)),
        OpNode::GreaterThanEqual(op_nodes) => Ok(operation_eval::greater_than_eq(eval_operands(env, op_nodes)?)),
        OpNode::LessThan(op_nodes) => Ok(operation_eval::less_than(eval_operands(env, op_nodes)?)),
        OpNode::LessThanEqual(op_nodes) => Ok(operation_eval::less_than_eq(eval_operands(env, op_nodes)?)),
        OpNode::Equality(op_nodes) => Ok(operation_eval::ref_equality(eval_operands(env, op_nodes)?)),
        OpNode::RefEquality(op_nodes) => Ok(operation_eval::value_equality(eval_operands(env, op_nodes)?)),
        OpNode::RefNonEquality(op_nodes) => Ok(operation_eval::value_non_equality(eval_operands(env, op_nodes)?))
    }
}

fn eval_operands<'a>(env: &Rc<RefCell<Environment>>, op_nodes: Vec<AstNode>) -> Result<(bool, Vec<LitNode>), String> {
    let mut operands = Vec::<LitNode>::with_capacity(op_nodes.len());
    let mut is_float = false;
    for node in op_nodes {
        match eval_node(env, &node)? {
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


fn eval_definition<'a>(env: &Rc<RefCell<Environment>>, node: DefNode) -> Result<Cow<'a, AstNode>, String> {
    match node {
        DefNode::Variable(var) => {
            if let Ok(evaled_var) = eval_node(env, &var.value).as_deref() {
                let binding = Binding::new_binding(evaled_var, &var.modifiers);
                let bind = env.borrow_mut().create_binding(var.name, binding?);
                if let Err(s) = bind {
                    Err(s.clone())
                } else { Ok(Cow::Owned(AstNode::new_bool_lit(true))) }
            } else { Err("Binding did not eval to literal".to_string()) }
        }
        DefNode::Lambda(lam) => {
            Ok(Cow::Owned(AstNode::new_lambda_lit(lam, Rc::clone(&env))))
        }
        DefNode::Function(fun) => {
            let mods = fun.lambda.modifiers.clone();
            let lambda_lit = AstNode::new_lambda_lit(fun.lambda, Rc::clone(&env));
            let binding = Binding::new_binding(&lambda_lit, &mods);
            let bind = env.borrow_mut().create_binding(fun.name, binding?);
            if let Err(s) = bind {
                Err(s.clone())
            } else { Ok(Cow::Owned(AstNode::new_bool_lit(true))) }
        }
    }
}

fn eval_expression<'a>(env: &'a Rc<RefCell<Environment>>, node: &'a ExprNode) -> Result<Cow<'a, AstNode>, String> {
    match node {
        ExprNode::Assignment(data) => eval_assignment(env, data),
        ExprNode::MultiExpr(data) => eval_multi_expr(env, data),
        ExprNode::PrintExpr(data) => eval_print_expr(env, data),
        ExprNode::IfExpr(data) => eval_if_expr(env, &data),
        ExprNode::CondExpr(data) => eval_cond_expr(env, data),
        ExprNode::WhileLoop(data) => eval_while_expr(env, data),
        ExprNode::ConsExpr(data) => eval_cons_expr(env, data),
        ExprNode::PairList(data) => eval_pair_list_expr(env, data),
        ExprNode::ListAccess(data) => eval_list_acc_expr(env, data),
        ExprNode::FuncCall(data) => eval_func_call_expr(env, data),
        ExprNode::LiteralCall(data) => eval_lit_call_expr(env, data),
    }
}

fn eval_assignment<'a>(env: &'a Rc<RefCell<Environment>>, expr: &AssignData) -> Result<Cow<'a, AstNode>, String> {
    if let Ok(LiteralNode(lit)) = eval_node(env, &expr.value).as_deref() {
        let rtn = env.borrow_mut().update_binding(&expr.name, lit)?;
        Ok(Cow::Owned(rtn))
    } else { Err("Assigned value did not evaluate to literal".to_string()) }
}

fn eval_multi_expr<'a>(env: &'a Rc<RefCell<Environment>>, expr: &'a Vec<AstNode>) -> Result<Cow<'a, AstNode>, String> {
    let mut result: Cow<'a, AstNode> = Cow::Owned(AST_NIL_LIT);
    for e in expr {
        result = eval_node(env, &*e)?;
    }
    Ok(result)
}

fn eval_print_expr<'a>(env: &Rc<RefCell<Environment>>, expr: &AstNode) -> Result<Cow<'a, AstNode>, String> {
    match eval_node(env, &expr)? {
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

fn eval_if_expr<'a>(env: &'a Rc<RefCell<Environment>>, expr: &'a IfData) -> Result<Cow<'a, AstNode>, String> {
    if let Ok(LiteralNode(lit)) = eval_node(env, &expr.if_branch.cond_node).as_deref() {
        if lit.value().as_bool() {
            eval_node(env, &expr.if_branch.then_node)
        } else if expr.else_branch.is_some() {
            match eval_node(env, &expr.to_owned().else_branch.unwrap())? {
                Cow::Borrowed(b) => Ok(Cow::Owned(b.to_owned())),
                Cow::Owned(o) => Ok(Cow::Owned(o))
            }
        } else { Ok(Cow::Borrowed(&AST_TRUE_LIT)) }
    } else { Err("If condition did not evaluate to boolean".to_string()) }
}

fn eval_cond_expr<'a>(env: &'a Rc<RefCell<Environment>>, expr: &'a CondData) -> Result<Cow<'a, AstNode>, String> {
    for e in &expr.cond_branches {
        if let Ok(LiteralNode(lit)) = eval_node(env, &e.cond_node).as_deref() {
            if lit.value().as_bool() {
                return eval_node(env, &e.then_node);
            } else { continue; }
        } else { return Err("Cond condition did  not evaluate to boolean".to_string()); }
    }
    if let Some(else_branch) = &expr.else_branch {
        eval_node(env, &else_branch)
    } else { Ok(Cow::Borrowed(&AST_FALSE_LIT)) }
}


fn eval_while_expr<'a>(env: &Rc<RefCell<Environment>>, expr: &WhileData) -> Result<Cow<'a, AstNode>, String> {
    if expr.is_do { eval_node(env, &expr.body)?; }
    loop {
        match eval_node(env, &expr.condition).as_deref() {
            Ok(LiteralNode(lit)) if lit.value().as_bool() => { eval_node(env, &expr.body)?; }
            Ok(_) => break,
            Err(err) => { return Err(err.to_owned()); }
        }
    }
    Ok(Cow::Borrowed(&AST_TRUE_LIT))
}


fn eval_cons_expr<'a>(env: &Rc<RefCell<Environment>>, expr: &ConsData) -> Result<Cow<'a, AstNode>, String> {
    let car = eval_node(env, &expr.car)?;
    let cdr = eval_node(env, &expr.cdr)?;
    Ok(Cow::Owned(PairValue::from_ast(car.as_ref().to_owned(), cdr.as_ref().to_owned())?))
}

fn eval_pair_list_expr<'a>(env: &Rc<RefCell<Environment>>, expr: &Vec<AstNode>) -> Result<Cow<'a, AstNode>, String> {
    let mut head = AST_NIL_LIT; // Ensure this is the correct way to get an owned Cow of AST_NIL_LIT
    for element in expr.into_iter().rev() {
        let evaled = match eval_node(env, &element)? {
            Cow::Borrowed(borrowed) => head = PairValue::from_ast(borrowed.to_owned(), head)?,
            Cow::Owned(owned) => head = PairValue::from_ast(owned, head)?
        };

        // Depending on PairValue::from_ast implementation, you might need to clone `evaled` if it expects owned value
        // Adjust accordingly if PairValue::from_ast can take a reference
    }
    Ok(Cow::Owned(head))
}


fn eval_list_acc_expr<'a>(env: &Rc<RefCell<Environment>>, expr: &ListAccData) -> Result<Cow<'a, AstNode>, String> {
    let evaled_node = eval_node(env, &expr.list);
    let list = if let LiteralNode(LitNode::Pair(pair)) = evaled_node.as_deref()? {
        pair
    } else { return Err("Attempted list access on non-list literal".to_string()); };

    if expr.index_expr.is_some() {
        let mut index = if let LiteralNode(lit) = eval_node(env, &expr.to_owned().index_expr.unwrap()).as_deref()? {
            lit.value().as_int()
        } else { return Err("Index did not evaluate to literal".to_string()); };
        if index == 0 { return Ok(Cow::Owned(LiteralNode(*list.car.to_owned()))); }

        let mut result = list;
        for _ in 0..index {
            // result = if let LitNode::Pair(pair) = *result.cdr.to_owned() {
            //     &pair.clone().to_owned()
            // } else { return Err("Index not contained in list".to_string()); }
        }
        return Ok(Cow::Owned(LiteralNode(*result.car.to_owned())));
    }

    if let Some(pattern) = &expr.pattern {
        let mut curr: &PairValue = &list;
        let mut chars = pattern.chars().peekable();

        while let Some(acc) = chars.next() {
            match (acc, &*curr.car, &*curr.cdr) {
                ('f', LitNode::Pair(pair), _) if chars.peek().is_some() => curr = pair,
                ('r', _, LitNode::Pair(pair)) if chars.peek().is_some() => curr = pair,
                ('f', _, _) if chars.peek().is_none() => return Ok(Cow::Owned(LiteralNode(*curr.car.to_owned()))),
                ('r', _, _) if chars.peek().is_none() => return Ok(Cow::Owned(LiteralNode(*curr.cdr.to_owned()))),
                _ => return Err("Invalid access pattern or non-pair encountered before the end".to_string()),
            }
        }
        Err("Pattern empty or did not lead to a valid access.".to_string())
    } else {
        Err("Access pattern required.".to_string())
    }
}

fn eval_func_call_expr<'a>(env: &'a Rc<RefCell<Environment>>, call: &'a FuncCallData) -> Result<Cow<'a, AstNode>, String> {
    if let Some(binding) = env.borrow().get_literal(&call.name) {
        if let LitNode::Lambda(lambda) = &binding.borrow().value {
            let new_env = Environment::of(Rc::clone(&lambda.env));

            if lambda.def.parameters.is_some() && call.arguments.is_some() {
                map_param_to_env(
                    env,
                    lambda.def.parameters.as_ref().unwrap(),
                    &mut call.to_owned().arguments.unwrap(),
                    &new_env, // new_env is now directly passed as Rc<RefCell<Environment>>
                )?;
            } else if lambda.def.parameters.is_none() != call.arguments.is_none() {
                return Err("Parameter-argument mismatch".to_string());
            }
            match eval_node(&new_env, &lambda.def.body)? {
                Cow::Borrowed(b) => Ok(Cow::Owned(b.to_owned())),
                Cow::Owned(o) => Ok(Cow::Owned(o))
            }
        } else { Err("Binding is not a lambda".to_string()) }
    } else { Err("Binding is not a lambda".to_string()) }
}

fn map_param_to_env(
    curr_env: &Rc<RefCell<Environment>>,
    params: &Vec<Param>,
    args: &mut Vec<FuncArg>,
    func_env: &Rc<RefCell<Environment>>,
) -> Result<(), String> {
    if params.len() == args.len() {
        for i in 0..args.len() {
            let arg = args.remove(i);
            let ast = eval_node(curr_env, &arg.value)?;
            let binding = Binding::new_binding(&ast, &None)?;
            func_env.borrow_mut().create_binding(params[i].name.clone(), binding)?;
        }
        Ok(())
    } else { Err("Arg len mismatch".to_string()) }
}

fn eval_lit_call_expr<'a>(env: &Rc<RefCell<Environment>>, name: &String) -> Result<Cow<'a, AstNode>, String> {
    if let Some(lit) = env.borrow().get_literal(name) {
        Ok(Cow::Owned(LiteralNode(lit.borrow().value.clone())))
    } else {
        Err(format!("Failed to find binding for: {}", name))
    }
}
