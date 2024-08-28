use bitflags::bitflags;
use crate::token::Mod;
use crate::types::Type;


pub mod util;
pub mod ast;
pub mod types;
pub mod op_codes;
pub mod token;


#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum ValueType {
    Primitive,
    Object,
    Namespace,
    Symbol,
    Type,
    Function,
    Interface,
    Record,
    Struct,
    AbsClass,
    Class,
    Quote,
}


impl From<&Type> for ValueType {
    fn from(value: &Type) -> Self {
        match value {
            Type::Unresolved(_) => panic!("Fatal<internal>: Attempted to convert unresolved Type to ValueType"),
            Type::Integer => ValueType::Primitive,
            Type::Float => ValueType::Primitive,
            Type::Boolean => ValueType::Primitive,
            Type::Array(_) => ValueType::Primitive,
            Type::String => ValueType::Primitive,
            Type::Tuple => ValueType::Primitive,
            Type::Nil => panic!("Fatal<internal>: Attempted to cover Nil to ValueType"),
            Type::Quote => ValueType::Quote,
            Type::Object(_) => ValueType::Class, // FIXME, this need to map the type of object
            Type::Lambda(_) => ValueType::Function
        }
    }
}




bitflags! {
    /// Represents a set of flags.
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
    pub fn from_mods(mods: Option<&[Mod]>) -> ModifierFlags {
        let mut flag = ModifierFlags::NONE;
        if let Some(mods) = mods {
            mods.iter().for_each(|modd| {
                match modd {
                    Mod::Mutable => flag |= ModifierFlags::MUTABLE,
                    Mod::Public => flag |= ModifierFlags::PUBLIC,
                    Mod::Const => flag |= ModifierFlags::FINAL,
                    Mod::Optional => flag |= ModifierFlags::OPTIONAL
                }
            });
        }
        flag
    }
}


#[test]
fn test_bitflags() {}