use std::cell::RefCell;
use std::mem;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};
use AstNode::LiteralNode;
use crate::eval::environment::{Binding, Environment};
use crate::eval::operation_eval;
use crate::parse;
use crate::parse::ast_nodes::{AssignData, AstNode, CondData, ConsData, DefLambdaData, DefNode, ExprNode, FuncArg, FuncCallData, IfData, LambdaValue, ListAccData, LitNode, OpNode, Param, WhileData};
use crate::parse::ast_nodes::AstNode::{DefinitionNode, ExpressionNode, OperationNode, ProgramNode};


macro_rules! nano_time {
    () => {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_nanos() // Returns the number of nanoseconds since the Unix epoch
    };
}


pub struct Interp {
    pub env: Rc<RefCell<Environment>>,
}


pub fn repl_eval(env: &Rc<RefCell<Environment>>, input: String) -> String {
    let start = nano_time!();
    let proc_time;
    let eval_time;
    let total_time;
    let tokens = parse::lexer::process(input);
    println!("{:?}", tokens);
    let ast = match tokens {
        Ok(tokens) => parse::parser::process(tokens),
        Err(msg) => return msg
    };
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
        match eval_node(env, node) {
            Ok(rtn) => if let LiteralNode(any) = rtn {
                evaled.push(any.value().as_string())
            } else { evaled.push("Fatal: Invalid Eval Return".to_string()) }
            Err(msg) => evaled.push(msg.clone())
        }
    }
    evaled
}


fn eval_node(env: &Rc<RefCell<Environment>>, node: AstNode) -> Result<AstNode, String> {
    match node {
        ExpressionNode(expr) => eval_expression(env, *expr),
        LiteralNode(_) => Ok(node),
        OperationNode(op) => eval_operation(env, op),
        DefinitionNode(def) => eval_definition(env, *def),
        ProgramNode(_) => Err("Fatal: Found Nested Program Node".to_string())
    }
}

fn eval_operation(env: &Rc<RefCell<Environment>>, node: OpNode) -> Result<AstNode, String> {
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
        OpNode::Equality(op_nodes) => Ok(operation_eval::ref_eqaulity(eval_operands(env, op_nodes)?)),
        OpNode::RefEquality(op_nodes) => Ok(operation_eval::value_equality(eval_operands(env, op_nodes)?)),
        OpNode::RefNonEquality(op_nodes) => Ok(operation_eval::value_non_equality(eval_operands(env, op_nodes)?))
    }
}

fn eval_operands(env: &Rc<RefCell<Environment>>, op_nodes: Vec<AstNode>) -> Result<(bool, Vec<LitNode>), String> {
    let mut operands = Vec::<LitNode>::with_capacity(op_nodes.len());
    let mut is_float = false;
    for node in op_nodes {
        match eval_node(env, node)? {
            LiteralNode(val) => {
                if matches!(val, LitNode::Float(_)) {
                    is_float = true;
                }
                operands.push(val);
            }
            _ => return Err("Operand did not evaluate to literal".to_string()),
        }
    }
    Ok((is_float, operands))
}

fn eval_definition(env: &Rc<RefCell<Environment>>, node: DefNode) -> Result<AstNode, String> {
    match node {
        DefNode::Variable(var) => {
            if let Ok(evaled_var) = eval_node(env, *var.value) {
                let binding = Binding::new_binding(evaled_var, var.modifiers);
                let bind = env.borrow_mut().create_binding(var.name, binding?);
                if let Err(s) = bind {
                    Err(s.clone())
                } else { Ok(AstNode::new_bool_lit(true)) }
            } else { Err("Binding did not eval to literal".to_string()) }
        }
        DefNode::Lambda(lam) => {
            Ok(AstNode::new_lambda_lit(lam, Rc::clone(&env)))
        }
        DefNode::Function(fun) => {
            let mods = fun.lambda.modifiers.clone();
            let lambda_lit = AstNode::new_lambda_lit(fun.lambda, Rc::clone(&env));
            let binding = Binding::new_binding(lambda_lit, mods);
            let bind = env.borrow_mut().create_binding(fun.name, binding?);
            if let Err(s) = bind {
                Err(s.clone())
            } else { Ok(AstNode::new_bool_lit(true)) }
        }
    }
}


fn eval_expression(env: &Rc<RefCell<Environment>>, node: ExprNode) -> Result<AstNode, String> {
    match node {
        ExprNode::Assignment(data) => eval_assignment(env, data),
        ExprNode::MultiExpr(data) => eval_multi_expr(env, data),
        ExprNode::PrintExpr(data) => eval_print_expr(env, data),
        ExprNode::IfExpr(data) => eval_if_expr(env, data),
        ExprNode::CondExpr(data) => eval_cond_expr(env, data),
        ExprNode::WhileLoop(data) => eval_while_expr(env, data),
        ExprNode::ConsExpr(data) => eval_cons_expr(env, data),
        ExprNode::PairList(data) => eval_pair_list_expr(env, data),
        ExprNode::ListAccess(data) => eval_list_acc_expr(env, data),
        ExprNode::FuncCall(data) => eval_func_call_expr(env, data),
        ExprNode::LiteralCall(data) => eval_lit_call_expr(env, data),
    }
}

fn eval_assignment(env: &Rc<RefCell<Environment>>, expr: AssignData) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_multi_expr(env: &Rc<RefCell<Environment>>, expr: Vec<AstNode>) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_print_expr(env: &Rc<RefCell<Environment>>, expr: AstNode) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_if_expr(env: &Rc<RefCell<Environment>>, expr: IfData) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_cond_expr(env: &Rc<RefCell<Environment>>, expr: CondData) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_while_expr(env: &Rc<RefCell<Environment>>, expr: WhileData) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_cons_expr(env: &Rc<RefCell<Environment>>, expr: ConsData) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_pair_list_expr(env: &Rc<RefCell<Environment>>, expr: Vec<AstNode>) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_list_acc_expr(env: &Rc<RefCell<Environment>>, expr: ListAccData) -> Result<AstNode, String> {
    panic!("Not Implemented")
}


fn eval_func_call_expr(env: &Rc<RefCell<Environment>>, call: FuncCallData) -> Result<AstNode, String> {
    if let Some((LitNode::Lambda( lambda))) = env.borrow_mut().get_literal(&call.name) {
        let mut new_env = Environment::of(Rc::clone(&lambda.env));
        if lambda.def.parameters.is_some() && call.arguments.is_some() {
            map_param_to_env(
                &mut new_env,
                lambda.def.parameters.as_ref().unwrap(),
                call.arguments.unwrap(),
            )?;
        } else {
            return Err("Parameter-argument mismatch".to_string());
        }

        let result = eval_node(&mut new_env, lambda.def.body);
        // env = prev_env; // restore the old active one 
        result
    } else {
        Err("Binding is not a lambda".to_string())
    }
}


fn map_param_to_env(env: &Rc<RefCell<Environment>>, mut params: &Vec<Param>, mut args: Vec<FuncArg>) -> Result<(), String> {
    println!("At map params");
    println!("params len:{}", params.len());
    if params.len() == args.len() {
        for i in 0..args.len() {
            let arg = args.remove(i);
            let ast = eval_node(env, arg.value)?;
            let binding = Binding::new_binding(ast, None)?;
            println!("Created binding for: {}", &params[i].name);
            env.borrow_mut().create_binding(params[i].name.clone(), binding)?;
        }
        Ok(())
    } else {
        panic!();
        Err("Arg len mismatch".to_string())
    }
}

fn eval_lit_call_expr(env: &Rc<RefCell<Environment>>, name: String) -> Result<AstNode, String> {
    if let Some(lit) = env.borrow_mut().get_literal(&name) {
        Ok(LiteralNode(lit.clone()))
    } else { Err(format!("Failed to find binding for: {}", name)) }
}
