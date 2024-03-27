use std::borrow::Cow;
use std::rc::Rc;
use crate::parse::ast_nodes::{AstNode, BoolValue, FloatValue, IntValue, LitNode, VectorValue};
use crate::parse::ast_nodes::AstNode::LiteralNode;


// bool args are if operation should be a float, kept packed for brevity when calling 
pub fn add<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    if data.0 {
        let mut result: f64 = 0.0;
        for op in data.1 { result += op.value().as_float(); }
        Cow::Owned(AstNode::new_float_lit(result))
    } else {
        let mut result: i64 = 0;
        for op in data.1 { result += op.value().as_int(); }
        Cow::Owned(AstNode::new_int_lit(result))
    }
}


pub fn subtract<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    if data.0 {
        let mut result: f64 = 0.0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_float();
                continue;
            }
            result -= data.1[i].value().as_float();
        }
        Cow::Owned(AstNode::new_float_lit(result))
    } else {
        let mut result: i64 = 0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_int();
                continue;
            }
            result -= data.1[i].value().as_int()
        }
        Cow::Owned(AstNode::new_int_lit(result))
    }
}


pub fn multiply<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    if data.0 {
        let mut result: f64 = 0.0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_float();
                continue;
            }
            result *= data.1[i].value().as_float();
        }
        Cow::Owned(AstNode::new_float_lit(result))
    } else {
        let mut result: i64 = 0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_int();
                continue;
            }
            result *= data.1[i].value().as_int()
        }
        Cow::Owned(AstNode::new_int_lit(result))
    }
}


pub fn divide<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    if data.0 {
        let mut result: f64 = 0.0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_float();
                continue;
            }
            result /= data.1[i].value().as_float();
        }
        Cow::Owned(AstNode::new_float_lit(result))
    } else {
        let mut result: i64 = 0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_int();
                continue;
            }
            result /= data.1[i].value().as_int()
        }
        Cow::Owned(AstNode::new_int_lit(result))
    }
}


pub fn modulo<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    if data.0 {
        let mut result: f64 = 0.0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_float();
                continue;
            }
            result %= data.1[i].value().as_float();
        }
        Cow::Owned(AstNode::new_float_lit(result))
    } else {
        let mut result: i64 = 0;
        for i in 0..data.1.len() {
            if i == 0 {
                result = data.1[i].value().as_int();
                continue;
            }
            result %= data.1[i].value().as_int()
        }
        Cow::Owned(AstNode::new_int_lit(result))
    }
}


pub fn exponentiate<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    let mut result: f64 = 0.0;
    for i in 0..data.1.len() {
        if i == 0 {
            result = data.1[i].value().as_float();
            continue;
        }
        result = result.powf(data.1[i].value().as_float())
    }
    Cow::Owned(AstNode::new_float_lit(result))
}


pub fn increment<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    if data.0 {
        let mut result: VectorValue = data.1.into_iter()
            .map(|item| LitNode::Float(FloatValue(item.value().as_float() + 1.0)))
            .collect();

        if result.0.len() == 1 {
            Cow::Owned(LiteralNode(Rc::new(result.0.remove(0))))
        } else { Cow::Owned(LiteralNode(Rc::new(LitNode::Vector(result)))) }
    } else {
        let mut result: VectorValue = data.1.into_iter()
            .map(|item| LitNode::Integer(IntValue(item.value().as_int() + 1)))
            .collect();

        if result.0.len() == 1 {
            Cow::Owned(LiteralNode(Rc::new(result.0.remove(0))))
        } else { Cow::Owned(LiteralNode(Rc::new(LitNode::Vector(result)))) }
    }
}


pub fn decrement<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    if data.0 {
        let mut result: VectorValue = data.1.into_iter()
            .map(|item| LitNode::Float(FloatValue(item.value().as_float() - 1.0)))
            .collect();

        if result.0.len() == 1 {
            Cow::Owned(LiteralNode(Rc::new(result.0.remove(0))))
        } else { Cow::Owned(LiteralNode(Rc::new(LitNode::Vector(result)))) }
    } else {
        let mut result: VectorValue = data.1.into_iter()
            .map(|item| LitNode::Integer(IntValue(item.value().as_int() - 1)))
            .collect();

        if result.0.len() == 1 {
            Cow::Owned(LiteralNode(Rc::new(result.0.remove(0))))
        } else { Cow::Owned(LiteralNode(Rc::new(LitNode::Vector(result)))) }
    }
}


pub fn or<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    for op in data.1 {
        if op.value().as_bool() { return Cow::Owned(AstNode::new_bool_lit(true)); }
    }
    Cow::Owned(AstNode::new_bool_lit(false))
}


pub fn and<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    for op in data.1 {
        if !op.value().as_bool() { return Cow::Owned(AstNode::new_bool_lit(false)); }
    }
    Cow::Owned(AstNode::new_bool_lit(true))
}


pub fn xor<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    let mut truths = 0;
    for op in data.1 {
        if op.value().as_bool() { truths += 1 }
    }
    Cow::Owned(AstNode::new_bool_lit(truths % 2 == 1))
}


pub fn nand<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    for op in data.1 {
        if !op.value().as_bool() { return Cow::Owned(AstNode::new_bool_lit(true)); }
    }
    Cow::Owned(AstNode::new_bool_lit(false))
}


pub fn nor<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    for op in data.1 {
        if op.value().as_bool() { return Cow::Owned(AstNode::new_bool_lit(false)); }
    }
    Cow::Owned(AstNode::new_bool_lit(true))
}


pub fn xnor<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    let mut truths = 0;
    for op in data.1 {
        if op.value().as_bool() { truths += 1 }
    }
    Cow::Owned(AstNode::new_bool_lit(truths % 2 == 0))
}


pub fn negate<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    let result: VectorValue = data.1.into_iter()
        .map(|item| LitNode::Boolean(BoolValue(!item.value().as_bool())))
        .collect();

    if result.0.len() == 1 {
        if result.0[0].value().as_bool() {
            Cow::Owned(AstNode::new_bool_lit(true))
        } else { Cow::Owned(AstNode::new_bool_lit(false)) }
    } else { Cow::Owned(LiteralNode(Rc::new(LitNode::Vector(result)))) }
}


pub fn greater_than<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    let operands = data.1;
    if data.0 {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_float() > operands[i].value().as_float()) {
                return Cow::Owned(AstNode::new_bool_lit(false));
            }
        }
        Cow::Owned(AstNode::new_bool_lit(true))
    } else {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_int() > operands[i].value().as_int()) {
                return Cow::Owned(AstNode::new_bool_lit(false));
            }
        }
        Cow::Owned(AstNode::new_bool_lit(true))
    }
}


pub fn greater_than_eq<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    let operands = data.1;
    if data.0 {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_float() >= operands[i].value().as_float()) {
                return Cow::Owned(AstNode::new_bool_lit(false));
            }
        }
        Cow::Owned(AstNode::new_bool_lit(true))
    } else {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_int() >= operands[i].value().as_int()) {
                return Cow::Owned(AstNode::new_bool_lit(false));
            }
        }
        Cow::Owned(AstNode::new_bool_lit(true))
    }
}


pub fn less_than<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    let operands = data.1;
    if data.0 {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_float() < operands[i].value().as_float()) {
                return Cow::Owned(AstNode::new_bool_lit(false));
            }
        }
        Cow::Owned(AstNode::new_bool_lit(true))
    } else {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_int() < operands[i].value().as_int()) {
                return Cow::Owned(AstNode::new_bool_lit(false));
            }
        }
        Cow::Owned(AstNode::new_bool_lit(true))
    }
}


pub fn less_than_eq<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    let operands = data.1;
    if data.0 {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_float() <= operands[i].value().as_float()) {
                return Cow::Owned(AstNode::new_bool_lit(false));
            }
        }
        Cow::Owned(AstNode::new_bool_lit(true))
    } else {
        for i in 1..operands.len() {
            if !(operands[i - 1].value().as_int() <= operands[i].value().as_int()) {
                return Cow::Owned(AstNode::new_bool_lit(false));
            }
        }
        Cow::Owned(AstNode::new_bool_lit(true))
    }
}


pub fn value_equality<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    let operands = data.1;
    for i in 1..operands.len() {
        if !operands[i - 1].value().equal_to(&operands[i]) {
            return Cow::Owned(AstNode::new_bool_lit(false));
        }
    }
    Cow::Owned(AstNode::new_bool_lit(true))
}


pub fn value_non_equality<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    let operands = data.1;
    for i in 1..operands.len() {
        if operands[i - 1].value().equal_to(&operands[i]) {
            return Cow::Owned(AstNode::new_bool_lit(false));
        }
    }
    Cow::Owned(AstNode::new_bool_lit(true))
}


pub fn ref_equality<'a>(data: (bool, Vec<Rc<LitNode>>)) -> Cow<'a, AstNode> {
    let operands = data.1;
    for i in 1..operands.len() {
        if !std::ptr::eq(&operands[i - 1], &operands[i]) {
            return Cow::Owned(AstNode::new_bool_lit(false));
        }
    }
    Cow::Owned(AstNode::new_bool_lit(true))
}






