use std::mem;
use crate::parser::CompUnit;


#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OpCode {
    Exit,
    RtnI64,
    RtnF64,
    RtnBool,
    RtnRef,
    Ldc,
    LdcW,
    AddI64,
    AddF64,
    SubI64,
    SubF64,
    MulI64,
    MulF64,
    DivI64,
    DivF64,
    PowI64,
    PowF64,
    ModI64,
    ModF64,
    NegI64,
    NegF64,
    NegBool,
    I64ToF64,
    F64ToI64,
    ConstT,
    ConstF,
    CompI64,
    CompI64N,
    CompF64,
    CompF64N,
    CompOr,
    CompAnd,
    CompNot,
    CompGrtrN,
    JumpFalse,
    JumpTrue,
}


impl OpCode {
    pub fn from_byte(byte: u8) -> Self {
        unsafe {
            mem::transmute(byte)
        }
    }
}


pub fn decode(comp_unit: CompUnit) -> Vec<String> {
    let mut iter = comp_unit.code.iter();
    let mut decoded = Vec::<String>::with_capacity(comp_unit.code.len());

    while let Some(next) = iter.next() {
        match OpCode::from_byte(*next) {
            OpCode::Exit => decoded.push("Exit".to_string()),
            OpCode::RtnI64 => decoded.push("RtnI64".to_string()),
            OpCode::RtnF64 => decoded.push("RtnF64".to_string()),
            OpCode::RtnBool => decoded.push("RtnBool".to_string()),
            OpCode::RtnRef => decoded.push("RtnRef".to_string()),
            OpCode::Ldc => {
                let index = iter.next().unwrap();
                decoded.push(format!("Ldc: {}", index))
            }
            OpCode::LdcW => {
                // TODO decode wide operands
                let index = iter.next().unwrap().to_string();
                decoded.push(format!("LdcW: {}", index))
            }
            OpCode::AddI64 => decoded.push("AddI64".to_string()),
            OpCode::AddF64 => decoded.push("AddF64".to_string()),
            OpCode::SubI64 => decoded.push("SubI64".to_string()),
            OpCode::SubF64 => decoded.push("SubF64".to_string()),
            OpCode::MulI64 => decoded.push("MulI64".to_string()),
            OpCode::MulF64 => decoded.push("MulF64".to_string()),
            OpCode::DivI64 => decoded.push("DivI64".to_string()),
            OpCode::DivF64 => decoded.push("DivF64".to_string()),
            OpCode::PowI64 => decoded.push("PowI64".to_string()),
            OpCode::PowF64 => decoded.push("PowF64".to_string()),
            OpCode::ModI64 => decoded.push("ModI64".to_string()),
            OpCode::ModF64 => decoded.push("ModF64".to_string()),
            OpCode::NegI64 => decoded.push("NegI64".to_string()),
            OpCode::NegF64 => decoded.push("NegF64".to_string()),
            OpCode::NegBool => decoded.push("NegBool".to_string()),
            OpCode::I64ToF64 => decoded.push("I64ToF64".to_string()),
            OpCode::F64ToI64 => decoded.push("F64ToI64".to_string()),
            OpCode::ConstT => decoded.push("ConstT".to_string()),
            OpCode::ConstF => decoded.push("ConstF".to_string()),
            OpCode::CompI64 => decoded.push("CompI64".to_string()),
            OpCode::CompI64N => {
                let n = iter.next().unwrap();
                decoded.push(format!("CompI64N: {}", n))
            },
            OpCode::CompF64N => {
                let n = iter.next().unwrap();
                decoded.push(format!("CompF64N: {}", n))
            },
            OpCode::CompF64 => decoded.push("CompF64".to_string()),
            OpCode::CompOr => decoded.push("CompOr".to_string()),
            OpCode::CompAnd => decoded.push("CompAnd".to_string()),
            OpCode::CompNot => decoded.push("CompNot".to_string()),
            OpCode::CompGrtrN => {
                let n = iter.next().unwrap();
                decoded.push(format!("CompI64N: {}", n))
            }
            
            OpCode::JumpTrue => decoded.push("JumpTrue".to_string()),
            OpCode::JumpFalse => decoded.push("JumpFalse".to_string()),
        }
    }
    decoded
}

