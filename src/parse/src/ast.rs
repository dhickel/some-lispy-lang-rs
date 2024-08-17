use std::collections::LinkedList;

use lang::util::IString;
use crate::environment::{ExprContext, ResData, SymbolCtx};
use crate::ParseError;
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
    pub fn new<T>(data: T, line_char: (u32, u32), typ: Option<Type>) -> Self {
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
    pub name: IString,
    pub namespace: Option<IString>,
    pub value: AstData<AstNode>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct LetData {
    pub name: IString,
    pub modifiers: Vec<Mod>,
    pub assignment: AstData<Expression>,
}


#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Operation(AstData<OpData>),
    Block(AstData<Vec<AstNode>>),
    Predicate(AstData<PredicateData>),
}


impl Into<AstNode> for Expression {
    fn into(self) -> AstNode {
        AstNode::Expression(self)
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct OpData {
    pub operation: Op,
    pub operands: Vec<AstData<Expression>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct PredicateData {
    pub pred_expr: AstData<Expression>,
    pub then_expr: Option<AstData<Expression>>,
    pub else_expr: Option<AstData<Expression>>,
}


#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    I8(AstData<i8>),
    I16(AstData<i16>),
    I32(AstData<i32>),
    I64(AstData<i64>),
    U8(AstData<u8>),
    U16(AstData<u16>),
    U32(AstData<u32>),
    U64(AstData<u64>),
    F32(AstData<f32>),
    F64(AstData<f64>),
    Boolean(AstData<bool>),
    Quote(AstData<AstNode>),
    Object,
    Nil(AstData<()>),
    Array,
    String,
    Tuple,
    Identifier(Identifier),
}


#[derive(Debug, Clone, PartialEq)]
pub enum Identifier {
    LocalSelf { name: IString }
}


// Nodes

#[derive(Debug, Clone, PartialEq)]
pub enum AstDdsata {
    // Definition
    DefVariable(DefVarData),
    DefLambda(DefLambdaData),
    DefFunction(DefFuncData),
    DefStruct(DefStructData),
    DefClass(DefClassData),

    // Expressions
    ExprAssignment(AssignData),
    ExprMulti(MultiExprData),
    ExprPrint(AstData),
    ExprIf(IfData),
    ExprCond(CondData),
    ExprWhileLoop(WhileData),
    ExprCons(ConsData),
    ExprPairList(OpData),
    ExprArray(OpData),
    ExprListAccess(ListAccData),
    ExprFuncCall(FuncCallData),
    ExprFuncCalInner(InnerFuncCallData),
    ExprObjectCall(ObjectCallData),
    ExprLiteralCall(LiteralCallData),
    ExprObjectAssignment(ObjectAssignData),
    ExprGenRand(GenRandData),
    ExprDirectInst(DirectInst),
    ExprInitInst(FuncCallData),

    // Operations
    Operation(OpData),

    // Literals
    LitInteger(i64),
    LitFloat(f64),
    LitBoolean(bool),
    LitString(String),
    LitQuote,
    LitObject,
    LitStruct(),
    LitNil,
    LitArray,
    LitPair,
    LitLambda,

}


// TODO Fix this ugly mess refactor all to use a Node Type with {AstNode, ResData}


// Definition Data

#[derive(Debug, Clone, PartialEq)]
pub struct DefVarData {
    pub name: IString,
    pub modifiers: Option<Vec<Mod>>,
    pub value: AstData,
    pub d_type: Option<Type>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefLambdaData {
    pub parameters: Option<Vec<Param>>,
    pub body: AstData,
    pub d_type: Option<Type>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefFuncData {
    pub name: IString,
    pub d_type: Option<Type>,
    pub lambda: DefLambdaData,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: IString,
    pub modifiers: Option<Vec<Mod>>,
    pub d_type: Option<Type>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefStructData {
    pub name: IString,
    pub fields: Option<Vec<Field>>,

}


#[derive(Debug, Clone, PartialEq)]
pub struct DefClassData {
    pub name: IString,
    pub params: Option<Vec<Mod>>,
    pub fields: Option<Vec<Field>>,
    pub init: Option<Vec<DefLambdaData>>,
    pub methods: Option<Vec<DefFuncData>>,
    pub pre_init: Option<AstData>,
    pub post_init: Option<AstData>,
    pub fin: Option<AstData>,
    pub validate: Option<AstData>,
}


impl DefClassData {
    pub fn empty_def(name: IString) -> DefClassData {
        DefClassData {
            name,
            params: None,
            fields: None,
            init: None,
            methods: None,
            pre_init: None,
            post_init: None,
            fin: None,
            validate: None,
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct DirectInst {
    pub name: IString,
    pub namespace: Option<IString>,
    pub args: Option<Vec<InstArgs>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: IString,
    pub modifiers: Option<Vec<Mod>>,
    pub p_type: Option<Type>,
    pub default_value: Option<AstData>,
}


// Expression Data
#[derive(Debug, Clone, PartialEq)]
pub struct MultiExprData {
    pub expressions: Vec<AstData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct CondBranch {
    pub cond_node: AstData,
    pub then_node: AstData,
}


#[derive(Debug, Clone, PartialEq)]
pub struct IfData {
    pub if_branch: CondBranch,
    pub else_branch: Option<AstData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct CondData {
    pub cond_branches: Vec<CondBranch>,
    pub else_branch: Option<AstData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct WhileData {
    pub condition: AstData,
    pub body: AstData,
    pub is_do: bool,

}


#[derive(Debug, Clone, PartialEq)]
pub struct ConsData {
    pub car: AstData,
    pub cdr: AstData,

}


#[derive(Debug, Clone, PartialEq)]
pub struct ListAccData {
    pub index_expr: Option<AstData>,
    pub pattern: Option<Vec<u8>>,
    pub list: AstData,

}


#[derive(Debug, Clone, PartialEq)]
pub struct FuncCallData {
    pub name: IString,
    pub namespace: Option<IString>,
    pub arguments: Option<Vec<FuncArg>>,

}


#[derive(Debug, Clone, PartialEq)]
pub struct ObjectCallData {
    pub name: IString,
    pub namespace: Option<IString>,
    pub accessors: LinkedList<Accessor>,

}


#[derive(Debug, Clone, PartialEq)]
pub struct LiteralCallData {
    pub name: IString,
    pub namespace: Option<IString>,

}


#[derive(Debug, Clone, PartialEq)]
pub struct ObjectAssignData {
    pub access: ObjectCallData,
    pub namespace: Option<IString>,
    pub value: AstData,

}


#[derive(Debug, Clone, PartialEq)]
pub struct InnerFuncCallData {
    pub expr: AstData,
    pub namespace: Option<IString>,
    pub accessors: Option<Vec<Accessor>>,
    pub arguments: Option<Vec<FuncArg>>,

}


#[derive(Debug, Clone, PartialEq)]
pub struct Accessor {
    pub name: IString,
    pub is_field: bool,
    pub args: Option<Vec<FuncArg>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct FuncArg {
    pub value: AstData,
    pub name: Option<IString>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct InstArgs {
    pub name: IString,
    pub value: AstData,
}


#[derive(Debug, Clone, PartialEq)]
pub struct GenRandData {
    pub is_float: bool,
    pub lower: AstData,
    pub upper: AstData,
}