use crate::parse::ast_nodes::{AstNode, FloatValue, IntValue, LitNode, VectorValue};
use crate::parse::ast_nodes::AstNode::LiteralNode;


// bool args are if operation should be a float, kept packed for brevity when calling 
pub fn add(data: (bool, Vec<LitNode>)) -> AstNode {
    if data.0 {
        let mut result: f64 = 0.0;
        for op in data.1 { result += op.value().as_float(); }
        AstNode::new_float_lit(result)
    } else {
        let mut result: i64 = 0;
        for op in data.1 { result += op.value().as_int(); }
        AstNode::new_int_lit(result)
    }
}


pub fn subtract(data: (bool, Vec<LitNode>)) -> AstNode {
    if data.0 {
        let mut result: f64 = 0.0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_float();
                continue;
            }
            result -= data.1[i].value().as_float();
        }
        AstNode::new_float_lit(result)
    } else {
        let mut result: i64 = 0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_int();
                continue;
            }
            result -= data.1[i].value().as_int()
        }
        AstNode::new_int_lit(result)
    }
}


pub fn multiply(data: (bool, Vec<LitNode>)) -> AstNode {
    if data.0 {
        let mut result: f64 = 0.0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_float();
                continue;
            }
            result *= data.1[i].value().as_float();
        }
        AstNode::new_float_lit(result)
    } else {
        let mut result: i64 = 0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_int();
                continue;
            }
            result *= data.1[i].value().as_int()
        }
        AstNode::new_int_lit(result)
    }
}


pub fn divide(data: (bool, Vec<LitNode>)) -> AstNode {
    if data.0 {
        let mut result: f64 = 0.0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_float();
                continue;
            }
            result /= data.1[i].value().as_float();
        }
        AstNode::new_float_lit(result)
    } else {
        let mut result: i64 = 0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_int();
                continue;
            }
            result /= data.1[i].value().as_int()
        }
        AstNode::new_int_lit(result)
    }
}


pub fn modulo(data: (bool, Vec<LitNode>)) -> AstNode {
    if data.0 {
        let mut result: f64 = 0.0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_float();
                continue;
            }
            result %= data.1[i].value().as_float();
        }
        AstNode::new_float_lit(result)
    } else {
        let mut result: i64 = 0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_int();
                continue;
            }
            result %= data.1[i].value().as_int()
        }
        AstNode::new_int_lit(result)
    }
}


pub fn exponentiate(data: (bool, Vec<LitNode>)) -> AstNode {
    let mut result: f64 = 0.0;
    for i in 0..data.1.len() {
        if i == 0 {
            result = data.1[i].value().as_float();
            continue;
        }
        result = result.powf(data.1[i].value().as_float())
    }
    AstNode::new_float_lit(result)
}


pub fn increment(data: (bool, Vec<LitNode>)) -> AstNode {
    if data.0 {
        let result = data.1.into_iter()
            .map(|item| LitNode::Float(FloatValue(item.value().as_float() + 1.0)))
            .collect();
        LiteralNode(LitNode::Vector(result))
    } else {
        let result = data.1.into_iter()
            .map(|item| LitNode::Integer(IntValue(item.value().as_int() + 1)))
            .collect();
        LiteralNode(LitNode::Vector(result))
    }
}


pub fn decrement(data: (bool, Vec<LitNode>)) -> AstNode {
    if data.0 {
        let result = data.1.into_iter()
            .map(|item| LitNode::Float(FloatValue(item.value().as_float() + 1.0)))
            .collect();
        LiteralNode(LitNode::Vector(result))
    } else {
        let result = data.1.into_iter()
            .map(|item| LitNode::Integer(IntValue(item.value().as_int() + 1)))
            .collect();
        LiteralNode(LitNode::Vector(result))
    }
}

// pub fn or(data: (bool, Vec<&LitNode>)) -> AstNode {
//     for op in data.1 {
//         if op.
//     }
// }






