use bitflags::bitflags;
use crate::token::Mod;
use crate::types::Type;


pub mod util;
pub mod ast;
pub mod types;
pub mod op_codes;
pub mod token;


#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueType {
    Primitive(PrimType),
    Array,
    Tuple,
    Object,
    Function,
    Type,
    Quote,
    Definition(DefType),
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
    String,
}


impl From<&Type> for ValueType {
    fn from(value: &Type) -> Self {
        match value {
            Type::Unresolved(_) => panic!("Fatal<internal>: Attempted to convert unresolved Type to ValueType"),
            Type::Integer => ValueType::Primitive(PrimType::I64),
            Type::Float => ValueType::Primitive(PrimType::F64),
            Type::Boolean => ValueType::Primitive(PrimType::Bool),
            Type::Array(_) => ValueType::Array,
            Type::String => ValueType::Primitive(PrimType::String),
            Type::Tuple(_) => ValueType::Tuple,
            Type::Nil => panic!("Fatal<internal>: Attempted to cover Nil to ValueType"),
            Type::Quote => ValueType::Quote,
            Type::Object(_) => ValueType::Definition(DefType::Class), // FIXME, this need to map the type of object
            Type::Lambda(_) => ValueType::Function
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


#[test]
fn test_bitflags() {}