use std::fmt::{Debug, Display};
use std::str::FromStr;
use bitflags::bitflags;
use crate::token::Mod;
use crate::types::LangType;


pub mod util;
pub mod ast;
pub mod types;
pub mod op_codes;
pub mod token;
pub mod module;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]

// TODO identify between mutable and immutable objects
pub enum ValueType {
    Primitive(PrimType),
    Array,
    Tuple,
    Object,
    String,
    Function,
    Type,
    Quote,
    Definition(DefType),
    Nil,
}


// FIXME switch to object type once it is finalized
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DefType {
    Namespace,
    Interface,
    Record,
    Struct,
    AbsClass,
    Class,
}


#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]

pub enum PrimType {
    U8,
    U16,
    U32,
    U64,
    I32,
    I64,
    F32,
    F64,
    Bool,
}


impl From<&LangType> for ValueType {
    fn from(value: &LangType) -> Self {
        match value {
            LangType::Undefined(_) => panic!("Fatal<internal>: Attempted to convert unresolved Type to ValueType"),
            LangType::Integer => ValueType::Primitive(PrimType::I64),
            LangType::Float => ValueType::Primitive(PrimType::F64),
            LangType::Boolean => ValueType::Primitive(PrimType::Bool),
            LangType::Array(_) => ValueType::Array,
            LangType::String => ValueType::String,
            LangType::Tuple(_) => ValueType::Tuple,
            // FIXME there is a nil value type, but I would like to avoid acknowledging
            //  nil values this deep internally
            LangType::Nil => panic!("Fatal<internal>: Attempted to cover Nil to ValueType"),
            LangType::Quote => ValueType::Quote,
            // FIXME, this need to map to the class definition, symbols should also be values?
            LangType::Object(_) => ValueType::Object,
            LangType::Lambda(_) => ValueType::Function
        }
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ModifierFlags: u16 {
        const NONE          = 0b00000000;
        const FINAL         = 0b00000001;
        const OPTIONAL      = 0b00000010;
        const STATIC        = 0b00000100;
        const MUTABLE       = 0b00001000;
        const PUBLIC        = 0b00010000;
    }
}

impl ModifierFlags {
    pub fn from_mods(mods: &[Mod]) -> ModifierFlags {
        let mut flag = ModifierFlags::NONE;

        mods.iter().for_each(|modd| {
            match modd {
                Mod::Mutable => flag |= ModifierFlags::MUTABLE,
                Mod::Public => flag |= ModifierFlags::PUBLIC,
                Mod::Const => flag |= ModifierFlags::FINAL,
                Mod::Optional => flag |= ModifierFlags::OPTIONAL
            }
        });

        flag
    }
}


#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Sha256Hash {
    hash: [u8; 32],
}

impl Sha256Hash {
    pub fn new(hash: [u8; 32]) -> Self {
        Self { hash }
    }

    pub fn as_bytes(&self) -> &[u8; 32] {
        &self.hash
    }

    pub fn to_hex_string(&self) -> String {
        self.hash.iter().fold(String::with_capacity(64), |mut acc, &byte| {
            acc.push_str(&format!("{:02x}", byte));
            acc
        })
    }

    pub fn from_hex_string(s: &str) -> Result<Self, String> {
        if s.len() != 64 {
            return Err("Invalid SHA256 hash length".to_string());
        }
        let mut hash = [0u8; 32];
        for (i, chunk) in s.as_bytes().chunks(2).enumerate() {
            hash[i] = u8::from_str_radix(std::str::from_utf8(chunk).map_err(|e| e.to_string())?, 16)
                .map_err(|e| e.to_string())?;
        }
        Ok(Self { hash })
    }
}

impl Display for Sha256Hash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_hex_string())
    }
}

impl FromStr for Sha256Hash {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_hex_string(s)
    }
}

impl Debug for Sha256Hash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{}", self.to_hex_string())
    }
}

#[test]
fn test_bitflags() {}