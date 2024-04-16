use std::mem;
use std::process::id;
use lasso::Key;
use crate::util;
use crate::util::{CompUnit, SCache, SCACHE};


#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OpCode {
    Exit,
    RtnI64,
    RtnF64,
    RtnBool,
    RtnRef,
    LoadConst,
    LoadConstW,
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
    LogicOr,
    LogicAnd,
    LogicXor,
    LogicNegate,
    CompGt,
    CompGtN,
    CompGtEq,
    CompGtEqN,
    CompEq,
    CompEqN,
    CompLt,
    CompLtN,
    CompLtEq,
    CompLtEqN,
    JumpFalse,
    JumpTrue,
    JumpFWd,
    JumpBack,
    IConstM1,
    IConst0,
    IConst1,
    IConst2,
    IConst3,
    IConst4,
    IConst5,
    Pop,
    AssignGlobal,
    DefGlobal,
    LoadGlobal,
    HeapStore,
}


impl OpCode {
    pub fn from_byte(byte: u8) -> Self {
        unsafe {
            mem::transmute(byte)
        }
    }
}


pub fn decode( code: &[u8]) -> Vec<String> {
    let mut iter = code.iter();
    let mut decoded = Vec::<String>::with_capacity(code.len());

    while let Some(next) = iter.next() {
        match OpCode::from_byte(*next) {
            OpCode::Exit => decoded.push("Exit".to_string()),
            OpCode::RtnI64 => decoded.push("RtnI64".to_string()),
            OpCode::RtnF64 => decoded.push("RtnF64".to_string()),
            OpCode::RtnBool => decoded.push("RtnBool".to_string()),
            OpCode::RtnRef => decoded.push("RtnRef".to_string()),
            OpCode::LoadConst => {
                let index = iter.next().unwrap();
                decoded.push(format!("Ldc: {}", index))
            }
            OpCode::LoadConstW => {
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
            }
            OpCode::CompF64N => {
                let n = iter.next().unwrap();
                decoded.push(format!("CompF64N: {}", n))
            }
            OpCode::CompF64 => decoded.push("CompF64".to_string()),
            OpCode::LogicOr => {
                let n = iter.next().unwrap();
                decoded.push(format!("LogicOr: {}", n))
            }
            OpCode::LogicAnd => {
                let n = iter.next().unwrap();
                decoded.push(format!("LogicAnd: {}", n))
            }

            OpCode::LogicXor => {
                let n = iter.next().unwrap();
                decoded.push(format!("LogicXor: {}", n))
            }

            OpCode::LogicNegate => decoded.push("LogicNegate".to_string()),
            OpCode::CompGt => decoded.push("CompGrtr".to_string()),
            OpCode::CompGtN => {
                let n = iter.next().unwrap();
                decoded.push(format!("CompI64N: {}", n))
            }
            OpCode::CompGtEq => decoded.push("CompGtEq".to_string()),
            OpCode::CompGtEqN => {
                let n = iter.next().unwrap();
                decoded.push(format!("CompGtEqN: {}", n))
            }
            OpCode::CompEq => decoded.push("CompEq".to_string()),
            OpCode::CompEqN => {
                let n = iter.next().unwrap();
                decoded.push(format!("CompEqN: {}", n))
            }
            OpCode::CompLt => decoded.push("CompLt".to_string()),
            OpCode::CompLtN => {
                let n = iter.next().unwrap();
                decoded.push(format!("CompLtN: {}", n))
            }
            OpCode::CompLtEq => decoded.push("CompLtEq".to_string()),
            OpCode::CompLtEqN => {
                let n = iter.next().unwrap();
                decoded.push(format!("CompLtEqN: {}", n))
            }
            OpCode::JumpTrue => decoded.push("JumpTrue".to_string()),
            OpCode::JumpFalse => {
                let val1 = iter.next().unwrap();
                let val2 = iter.next().unwrap();
                decoded.push(format!("JumpFalse: {}", util::read_wide_bytes(*val1, *val2)))
            }
            OpCode::JumpFWd => {
                let val1 = iter.next().unwrap();
                let val2 = iter.next().unwrap();
                decoded.push(format!("JumpFWd: {}", util::read_wide_bytes(*val1, *val2)))
            }
            OpCode::JumpBack => {
                let val1 = iter.next().unwrap();
                let val2 = iter.next().unwrap();
                decoded.push(format!("JumpBack: {}", util::read_wide_bytes(*val1, *val2)))
            }
            OpCode::IConstM1 => decoded.push("IConstM1".to_string()),
            OpCode::IConst0 => decoded.push("IConst0".to_string()),
            OpCode::IConst1 => decoded.push("IConst1".to_string()),
            OpCode::IConst2 => decoded.push("IConst2".to_string()),
            OpCode::IConst3 => decoded.push("IConst3".to_string()),
            OpCode::IConst4 => decoded.push("IConst4".to_string()),
            OpCode::IConst5 => decoded.push("IConst5".to_string()),
            OpCode::Pop => decoded.push("Pop".to_string()),
            OpCode::AssignGlobal => {
                decoded.push(format!("AssignGlobal"))
            },
            OpCode::DefGlobal => {
                decoded.push(format!("DefGlobal"))
            }
            OpCode::LoadGlobal => {
   
                decoded.push(format!("LoadGlobal"))
            },

            OpCode::HeapStore => {
                let val1 = iter.next().unwrap();
                let val2 = iter.next().unwrap();
                decoded.push(format!("HeapStore | Size_Multi: {}", util::read_wide_bytes(*val1, *val2)))
            }
        }
    }
    decoded
}

