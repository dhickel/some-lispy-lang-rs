use std::collections::LinkedList;

use lang::util::IString;
use crate::environment::{ExprContext, ResData, SymbolCtx};
use crate::token::{Mod, Op};
use crate::types::{Type, TypeCheck};


#[derive(Debug, Clone, PartialEq)]
pub struct AstData<T> {
    pub expr_type: Type,
    pub node_data: Box<T>,
    pub line_char: (u32, u32),
    pub res_data: Option<ResData>,
}


impl<T> AstData<T> {
    pub fn new(data: T, line_char: (u32, u32), typ: Option<Type>) -> Self {
        Self {
            expr_type: typ.unwrap_or(Type::default()),
            node_data: Box::new(data),
            line_char,
            res_data: None,
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Statement(Statement),
    Expression(Expression),
}


impl AstNode {}


////////////////
// Statements //
////////////////


#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(AstData<LetData>),
    Assign(AstData<AssignData>),
}


impl Into<AstNode> for Statement {
    fn into(self) -> AstNode {
        AstNode::Statement(self)
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct AssignData {
    pub namespace: Option<IString>,
    pub identifier: IString,
    pub value: Expression,
}


#[derive(Debug, Clone, PartialEq)]
pub struct LetData {
    pub identifier: IString,
    pub modifiers: Option<Vec<Mod>>,
    pub assignment: Expression,
}


#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    SCall(AstData<SCallData>),
    FCall(AstData<Vec<FExprData>>),
    Value(Value),
    OpCall(AstData<OpCallData>),
    Block(AstData<Vec<AstNode>>),
    Predicate(AstData<PredicateData>),
    Lambda(AstData<LambdaData>),
}


#[derive(Debug, Clone, PartialEq)]
pub struct SCallData {
    pub operation_expr: Expression,
    pub operand_exprs: Option<Vec<Expression>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct LambdaData {
    pub parameters: Option<Vec<Parameter>>,
    pub expr: Expression,
}


#[derive(Debug, Clone, PartialEq)]
pub enum FExprData {
    MCall { method: Option<IString>, arguments: Option<Vec<Argument>> },
    MAccess { identifier: IString, m_type: MType },
}


#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub modifiers: Option<Vec<Mod>>,
    pub expr: Expression,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub modifiers: Option<Vec<Mod>>,
    pub identifier: IString,
    pub typ: Option<Type>,
}


#[derive(Debug, Clone, PartialEq)]
pub enum MType {
    Namespace,
    MethodCall,
    Field,
    Identifier,
}


#[derive(Debug, Clone, PartialEq)]
pub struct OpCallData {
    pub operation: Op,
    pub operands: Option<Vec<Expression>>,
}


impl Into<AstNode> for Expression {
    fn into(self) -> AstNode {
        AstNode::Expression(self)
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct PredicateData {
    pub pred_expr: Expression,
    pub then_expr: Expression,
    pub else_expr: Option<Expression>,
}


#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    Boolean(bool),
    Quote(Box<AstNode>),
    Object,
    Nil(()),
    Array,
    String,
    Tuple,
    Identifier(IString),
}



