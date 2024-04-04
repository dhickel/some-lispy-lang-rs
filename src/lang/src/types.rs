use lasso::Spur;


#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    Float,
    Boolean,
    Vector(Box<Type>),
    String,
    Pair,
    Nil,
    Quote,
    Object(ObjType),
    Lambda(FuncType)
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjType {
    pub super_types: Vec<Type>,
    pub name: Spur,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncType {
    pub return_type: Box<Type>,
    pub arg_typ: Vec<Type>
}