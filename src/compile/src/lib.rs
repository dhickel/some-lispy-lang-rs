use std::fmt::{Debug, Display};
use lang::ast::Value;


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
    pub fn return_coercion(line_char: (u32, u32), v1: &Value) -> Warning {
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
    pub fn get_precedence(value: &Value) -> Result<ValuePrecedence, String> {
        match value {
            Value::I32(_) => Ok(Self::I32),
            Value::I64(_) => Ok(Self::I64),
            Value::U8(_) => Ok(Self::U8),
            Value::U16(_) => Ok(Self::U16),
            Value::U32(_) => Ok(Self::U32),
            Value::U64(_) => Ok(Self::U64),
            Value::F32(_) => Ok(Self::F32),
            Value::F64(_) => Ok(Self::F64),
            Value::Boolean(_) => Ok(Self::Bool),
            other => Err(format!("Non-primitive type in arithmetic/boolean operation: {:?}", other))
        }
    }

    pub fn primitive_operation_return_coercion(values: &[Value]) -> Result<(bool, &Value), String> {
        assert!(!values.is_empty());
        let mut max: (ValuePrecedence, &Value) = (Self::get_precedence(&values[0])?, &values[0]);
        let mut made_change = false;
        for value in values.iter().skip(1) {
            let precedence = Self::get_precedence(value)?;
            if precedence > max.0 {
                let max = (precedence, value);
                made_change = true;
            }
        }
        Ok((made_change, max.1))
    }
}