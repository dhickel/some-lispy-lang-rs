use std::collections::{BTreeSet, HashSet};



#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unresolved,
    Integer,
    Float,
    Boolean,
    Vector(Box<Type>),
    String,
    Pair,
    Nil,
    Quote,
    Object(ObjType),
    Lambda(FuncType),
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjType {
    pub super_types: Vec<Type>,
    pub name: u64,
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    pub return_type: Box<Type>,
    pub arg_typ: Vec<Type>,
}



