use std::time::{SystemTime, UNIX_EPOCH};
use AstNode::LiteralNode;
use crate::eval::operation_eval;
use crate::parse;
use crate::parse::ast_nodes::{AssignData, AstNode, CondData, ConsData, DefNode, ExprNode, FuncCallData, IfData, ListAccData, LitNode, OpNode, WhileData};
use crate::parse::ast_nodes::AstNode::{DefinitionNode, ExpressionNode, OperationNode, ProgramNode};

macro_rules! nano_time {
    () => {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_nanos() // Returns the number of nanoseconds since the Unix epoch
    };
}

pub fn repl_eval(input: String) -> String {
    let start = nano_time!();
    let proc_time;
    let eval_time;
    let total_time;
    let tokens = parse::lexer::process(input);
    let ast = match tokens {
        Ok(tokens) => parse::parser::process(tokens),
        Err(msg) => return msg
    };
    proc_time = nano_time!() - start;
    let eval_start = nano_time!();
    let result = match ast {
        Ok(ast) => eval(ast).join("\n"),
        Err(error) => return error
    };
    eval_time = nano_time!() - eval_start;
    total_time = nano_time!() - start;
    println!("Processing: {}", proc_time);
    println!("Eval: {}", eval_time);
    println!("Total: {}", total_time);
    result
}

pub fn eval(input: Vec<AstNode>) -> Vec<String> {
    let mut evaled = Vec::<String>::new();
    for node in input {
        match eval_node(node) {
            Ok(rtn) => if let LiteralNode(any) = rtn {
                evaled.push(any.value().as_string())
            } else { evaled.push("Fatal: Invalid Eval Return".to_string()) }
            Err(msg) => evaled.push(msg)
        }
    }
    evaled
}


fn eval_node(node: AstNode) -> Result<AstNode, String> {
    match node {
        ExpressionNode(expr) => eval_expression(*expr),
        LiteralNode(_) => Ok(node),
        OperationNode(op) => eval_operation(op),
        DefinitionNode(def) => eval_definition(*def),
        ProgramNode(_) => Err("Fatal: Found Nested Program Node".to_string())
    }
}

fn eval_operation(node: OpNode) -> Result<AstNode, String> {
    // let mut arr: [MaybeUninit<LitNode>; SIZE] = unsafe { MaybeUninit::uninit().assume_init() };

    return match node {
        OpNode::Addition(op_nodes) => Ok(operation_eval::add(eval_operands(op_nodes)?)),
        OpNode::Subtraction(op_nodes) => Ok(operation_eval::subtract(eval_operands(op_nodes)?)),
        OpNode::Multiplication(op_nodes) => Ok(operation_eval::multiply(eval_operands(op_nodes)?)),
        OpNode::Division(op_nodes) => Ok(operation_eval::divide(eval_operands(op_nodes)?)),
        OpNode::Modulo(op_nodes) => Ok(operation_eval::modulo(eval_operands(op_nodes)?)),
        OpNode::Exponentiate(op_nodes) => Ok(operation_eval::exponentiate(eval_operands(op_nodes)?)),
        OpNode::Increment(op_nodes) => Ok(operation_eval::increment(eval_operands(op_nodes)?)),
        OpNode::Decrement(op_nodes) => Ok(operation_eval::decrement(eval_operands(op_nodes)?)),
        OpNode::Or(op_nodes) => panic!("Not implemented"),
        OpNode::And(op_nodes) => panic!("Not implemented"),
        OpNode::Xor(op_nodes) => panic!("Not implemented"),
        OpNode::Nor(op_nodes) => panic!("Not implemented"),
        OpNode::Xnor(op_nodes) => panic!("Not implemented"),
        OpNode::Nand(op_nodes) => panic!("Not implemented"),
        OpNode::Negate(op_nodes) => panic!("Not implemented"),
        OpNode::GreaterThan(op_nodes) => panic!("Not implemented"),
        OpNode::GreaterThanEqual(op_nodes) => panic!("Not implemented"),
        OpNode::LessThan(op_nodes) => panic!("Not implemented"),
        OpNode::LessThanEqual(op_nodes) => panic!("Not implemented"),
        OpNode::Equality(op_nodes) => panic!("Not implemented"),
        OpNode::RefEquality(op_nodes) => panic!("Not implemented"),
        OpNode::RefNonEquality(op_nodes) => panic!("Not implemented"),
    };
    pub fn eval_operands(op_nodes: Vec<AstNode>) -> Result<(bool, Vec<LitNode>),
        String> {
        let mut operands = Vec::<LitNode>::with_capacity(op_nodes.len());
        let mut is_float = false;
        for node in op_nodes {
            match eval_node(node) { // Assuming eval_node takes a reference
                Ok(LiteralNode(val)) => {
                    // Check if the value inside the Box is a float and update is_float accordingly
                    if matches!(val, LitNode::Float(_)) {
                        is_float = true;
                    }
                    // Push the dereferenced Box<LitNode> into operands
                    operands.push(val);
                },
                _ => return Err("Operand did not evaluate to literal".to_string()),
            }
        }

        Ok((is_float, operands))
    }
}


fn eval_definition(node: DefNode) -> Result<AstNode, String> {
    panic!("Not implemented")
}

fn eval_expression(node: ExprNode) -> Result<AstNode, String> {
    match node {
        ExprNode::Assignment(data) => eval_assignment(data),
        ExprNode::MultiExpr(data) => eval_multi_expr(data),
        ExprNode::PrintExpr(data) => eval_print_expr(data),
        ExprNode::IfExpr(data) => eval_if_expr(data),
        ExprNode::CondExpr(data) => eval_cond_expr(data),
        ExprNode::WhileLoop(data) => eval_while_expr(data),
        ExprNode::ConsExpr(data) => eval_cons_expr(data),
        ExprNode::PairList(data) => eval_pair_list_expr(data),
        ExprNode::ListAccess(data) => eval_list_acc_expr(data),
        ExprNode::FuncCall(data) => eval_func_call_expr(data),
        ExprNode::LiteralCall(data) => eval_lit_call_expr(data),
    }
}

fn eval_assignment(expr: AssignData) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_multi_expr(expr: Vec<AstNode>) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_print_expr(expr: AstNode) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_if_expr(expr: IfData) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_cond_expr(expr: CondData) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_while_expr(expr: WhileData) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_cons_expr(expr: ConsData) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_pair_list_expr(expr: Vec<AstNode>) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_list_acc_expr(expr: ListAccData) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_func_call_expr(expr: FuncCallData) -> Result<AstNode, String> {
    panic!("Not Implemented")
}

fn eval_lit_call_expr(expr: String) -> Result<AstNode, String> {
    panic!("Not Implemented")
}