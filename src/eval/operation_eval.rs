use crate::parse::ast_nodes::{AstNode, BoolValue, FloatValue, IntValue, LitNode, VectorValue};
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
        let mut result: VectorValue = data.1.into_iter()
            .map(|item| LitNode::Float(FloatValue(item.value().as_float() + 1.0)))
            .collect();

        if result.0.len() == 1 {
            LiteralNode(result.0.remove(0))
        } else { LiteralNode(LitNode::Vector(result)) }
    } else {
        let mut result: VectorValue = data.1.into_iter()
            .map(|item| LitNode::Integer(IntValue(item.value().as_int() + 1)))
            .collect();

        if result.0.len() == 1 {
            LiteralNode(result.0.remove(0))
        } else { LiteralNode(LitNode::Vector(result)) }
    }
}


pub fn decrement(data: (bool, Vec<LitNode>)) -> AstNode {
    if data.0 {
        let mut result: VectorValue = data.1.into_iter()
            .map(|item| LitNode::Float(FloatValue(item.value().as_float() - 1.0)))
            .collect();

        if result.0.len() == 1 {
            LiteralNode(result.0.remove(0))
        } else { LiteralNode(LitNode::Vector(result)) }
    } else {
        let mut result: VectorValue = data.1.into_iter()
            .map(|item| LitNode::Integer(IntValue(item.value().as_int() - 1)))
            .collect();

        if result.0.len() == 1 {
            LiteralNode(result.0.remove(0))
        } else { LiteralNode(LitNode::Vector(result)) }
    }
}


pub fn or(data: (bool, Vec<LitNode>)) -> AstNode {
    for op in data.1 {
        if op.value().as_bool() { return AstNode::new_bool_lit(true); }
    }
    AstNode::new_bool_lit(false)
}


pub fn and(data: (bool, Vec<LitNode>)) -> AstNode {
    for op in data.1 {
        if !op.value().as_bool() { return AstNode::new_bool_lit(false); }
    }
    AstNode::new_bool_lit(true)
}


pub fn xor(data: (bool, Vec<LitNode>)) -> AstNode {
    let mut truths = 0;
    for op in data.1 {
        if op.value().as_bool() { truths += 1 }
    }
    AstNode::new_bool_lit(truths % 2 == 1)
}


pub fn nand(data: (bool, Vec<LitNode>)) -> AstNode {
    for op in data.1 {
        if !op.value().as_bool() { return AstNode::new_bool_lit(true); }
    }
    AstNode::new_bool_lit(false)
}


pub fn nor(data: (bool, Vec<LitNode>)) -> AstNode {
    for op in data.1 {
        if op.value().as_bool() { return AstNode::new_bool_lit(false); }
    }
    AstNode::new_bool_lit(true)
}


pub fn xnor(data: (bool, Vec<LitNode>)) -> AstNode {
    let mut truths = 0;
    for op in data.1 {
        if op.value().as_bool() { truths += 1 }
    }
    AstNode::new_bool_lit(truths % 2 == 0)
}


pub fn negate(data: (bool, Vec<LitNode>)) -> AstNode {
    let result: VectorValue = data.1.into_iter()
        .map(|item| LitNode::Boolean(BoolValue(!item.value().as_bool())))
        .collect();

    if result.0.len() == 1 {
        if result.0[0].value().as_bool() {
            AstNode::new_bool_lit(true)
        } else { AstNode::new_bool_lit(false) }
    } else { LiteralNode(LitNode::Vector(result)) }
}


pub fn greater_than(data: (bool, Vec<LitNode>)) -> AstNode {
    let operands = data.1;
    if data.0 {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_float() > operands[i].value().as_float()) {
                return AstNode::new_bool_lit(false);
            }
        }
        AstNode::new_bool_lit(true)
    } else {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_int() > operands[i].value().as_int()) {
                return AstNode::new_bool_lit(false);
            }
        }
        AstNode::new_bool_lit(true)
    }
}


pub fn greater_than_eq(data: (bool, Vec<LitNode>)) -> AstNode {
    let operands = data.1;
    if data.0 {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_float() >= operands[i].value().as_float()) {
                return AstNode::new_bool_lit(false);
            }
        }
        AstNode::new_bool_lit(true)
    } else {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_int() >= operands[i].value().as_int()) {
                return AstNode::new_bool_lit(false);
            }
        }
        AstNode::new_bool_lit(true)
    }
}


pub fn less_than(data: (bool, Vec<LitNode>)) -> AstNode {
    let operands = data.1;
    if data.0 {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_float() < operands[i].value().as_float()) {
                return AstNode::new_bool_lit(false);
            }
        }
        AstNode::new_bool_lit(true)
    } else {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_int() < operands[i].value().as_int()) {
                return AstNode::new_bool_lit(false);
            }
        }
        AstNode::new_bool_lit(true)
    }
}


pub fn less_than_eq(data: (bool, Vec<LitNode>)) -> AstNode {
    let operands = data.1;
    if data.0 {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_float() <= operands[i].value().as_float()) {
                return AstNode::new_bool_lit(false);
            }
        }
        AstNode::new_bool_lit(true)
    } else {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_int() <= operands[i].value().as_int()) {
                return AstNode::new_bool_lit(false);
            }
        }
        AstNode::new_bool_lit(true)
    }
}


pub fn value_equality(data: (bool, Vec<LitNode>)) -> AstNode {
    let operands = data.1;
    for i in 1..operands.len() {
        if !operands[i - 1].value().equal_to(&operands[i]) {
            return AstNode::new_bool_lit(false);
        }
    }
    AstNode::new_bool_lit(true)
}


pub fn value_non_equality(data: (bool, Vec<LitNode>)) -> AstNode {
    let operands = data.1;
    for i in 1..operands.len() {
        if operands[i - 1].value().equal_to(&operands[i]) {
            return AstNode::new_bool_lit(false);
        }
    }
    AstNode::new_bool_lit(true)
}


pub fn ref_eqaulity(data: (bool, Vec<LitNode>)) -> AstNode {
    let operands = data.1;
    for i in 1..operands.len() {
        if !std::ptr::eq(&operands[i - 1], &operands[i]) {
            return AstNode::new_bool_lit(false);
        }
    }
    AstNode::new_bool_lit(true)
}






