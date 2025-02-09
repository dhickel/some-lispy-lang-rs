use std::fmt::{Debug, Display};
use lang::ast::Value;
use lang::types::LangType;

pub mod code_gen;
pub mod resolution;
pub mod environment;
mod ir;
mod tests;

#[derive(Debug, Clone)]
pub enum Warning {
    Arithmetic(String),
    Empty(String),
}


impl Warning {
    pub fn return_coercion(line_char: (u32, u32), v1: &LangType) -> Warning {
        Self::Arithmetic(format!("Line: {}, Char: {}, coercion of primitive type: {:?}", line_char.0, line_char.1, v1))
    }

    pub fn empty_block(line_char: (u32, u32)) -> Warning {
        Self::Arithmetic(format!("Line: {}, Char: {}, Empty block expression", line_char.0, line_char.1))
    }
}


#[derive(PartialOrd, PartialEq, Ord, Eq)]
enum ValuePrecedence {
    Bool = 0,
    U8 = 1,
    U16 = 2,
    I32 = 3,
    U32 = 4,
    F32 = 5,
    I64 = 6,
    U64 = 7,
    F64 = 8,
}


impl ValuePrecedence {
    pub fn get_precedence_value(typ: &LangType) -> ValuePrecedence {
        match typ {
            LangType::Unresolved(_) => panic!("Fatal<Internal>: Attempted to get precedence of unresolved value"),
            LangType::Integer => ValuePrecedence::I64,
            LangType::Float => ValuePrecedence::F64,
            LangType::Boolean => ValuePrecedence::Bool,
            _ => panic!("Fatal<Internal>: Attempted to get precedence of non-primitive type [{:?}]", typ),
        }
    }

    pub fn return_value_coercion(values: &[LangType]) -> (bool, &LangType) {
        assert!(!values.is_empty());
        let mut max: (ValuePrecedence, &LangType) = (Self::get_precedence_value(&values[0]), &values[0]);
        let mut made_change = false;
        
        for value in values.iter().skip(1) {
            let precedence = Self::get_precedence_value(value);
            if precedence > max.0 {
                max = (precedence, value);
                made_change = true;
            }
        }
        (made_change, max.1)
    }
}