
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    Float,
    Boolean,
    Vector,
    String,
    Pair,
    Nil,
    Quote,
    Object,
    Lambda,
    Custom,
    Struct,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDef {
    pub super_types: Vec<Type>,
    pub name: String,
}

