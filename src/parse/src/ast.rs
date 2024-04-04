use std::collections::LinkedList;
use lasso::Spur;
use lang::types::Type;
use crate::token::Mod;


// Nodes


#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    // Definition
    DefVariable(Box<DefVarData>),
    DefLambda(Box<DefLambdaData>),
    DefFunction(Box<DefFuncData>),
    DefStruct(Box<DefStructData>),
    DefClass(Box<DefClassData>),
    
    // Expressions
    ExprAssignment(Box<AssignData>),
    ExprMulti(Vec<AstNode>),
    ExprPrint(Box<AstNode>),
    ExprIf(Box<IfData>),
    ExprCond(Box<CondData>),
    ExprWhileLoop(Box<WhileData>),
    ExprCons(Box<ConsData>),
    ExprPairList(Vec<AstNode>),
    ExprListAccess(Box<ListAccData>),
    ExprFuncCall(Box<FuncCallData>),
    ExprFuncCalInner(Box<ExprFuncCallData>),
    ExprObjectCall(Box<ObjectCallData>),
    ExprLiteralCall(Spur),
    ExprObjectAssignment(Box<ObjectAssignData>),
    ExprGenRand(Box<GenRandData>),
    ExprDirectInst(Box<DirectInst>),
    ExprInitInst(Box<FuncCallData>),
    
    // Operations
    OpAddition(Vec<AstNode>),
    OpSubtraction(Vec<AstNode>),
    OpMultiplication(Vec<AstNode>),
    OpDivision(Vec<AstNode>),
    OpModulo(Vec<AstNode>),
    OpExponentiate(Vec<AstNode>),
    OpIncrement(Vec<AstNode>),
    OpDecrement(Vec<AstNode>),
    OpOr(Vec<AstNode>),
    OpAnd(Vec<AstNode>),
    OpXor(Vec<AstNode>),
    OpNor(Vec<AstNode>),
    OpXnor(Vec<AstNode>),
    OpNand(Vec<AstNode>),
    OpNegate(Vec<AstNode>),
    OpGreaterThan(Vec<AstNode>),
    OpGreaterThanEqual(Vec<AstNode>),
    OpLessThan(Vec<AstNode>),
    OpLessThanEqual(Vec<AstNode>),
    OpEquality(Vec<AstNode>),
    OpRefEquality(Vec<AstNode>),
    OpRefNonEquality(Vec<AstNode>),
    
    // Literals
    LitInteger(i64),
    LitFloat(f64),
    LitBoolean(bool),
    LitString(String),
    LitQuote,
    LitObject,
    LitStruct(),
    LitNil,
    LitVector,
    LitPair,
    LitLambda,

}


// Definition Data

#[derive(Debug, Clone, PartialEq)]
pub struct DefVarData {
    pub name: Spur,
    pub modifiers: Option<Vec<Mod>>,
    pub value: AstNode,
    pub var_type: Option<Spur>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefLambdaData {
    pub modifiers: Option<Vec<Mod>>,
    pub parameters: Option<Vec<Param>>,
    pub body: AstNode,
    pub p_type: Option<Spur>,
    pub c_type: Option<Type>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefFuncData {
    pub name: Spur,
    pub lambda: DefLambdaData,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: Spur,
    pub p_type: Option<Spur>,
    pub optional: bool,
    pub default_value: Option<AstNode>,
    pub dynamic: bool,
    pub mutable: bool,
    pub c_type: Option<Type>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefStructData {
    pub name: Spur,
    pub fields: Option<Vec<Field>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefClassData {
    pub name: Spur,
    pub params: Option<Vec<Mod>>,
    pub fields: Option<Vec<Field>>,
    pub init: Option<Vec<DefLambdaData>>,
    pub methods: Option<Vec<DefFuncData>>,
    pub pre_init: Option<AstNode>,
    pub post_init: Option<AstNode>,
    pub fin: Option<AstNode>,
    pub validate: Option<AstNode>,
}


impl DefClassData {
    pub fn empty_def(name: Spur) -> DefClassData {
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
    pub name: Spur,
    pub args: Option<Vec<InstArgs>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Spur,
    pub modifiers: Option<Vec<Mod>>,
    pub p_type: Option<Spur>,
    pub default_value: Option<AstNode>,
    pub c_type: Option<Type>,
}


// Expression Data

#[derive(Debug, Clone, PartialEq)]
pub struct CondBranch {
    pub cond_node: AstNode,
    pub then_node: AstNode,
}


#[derive(Debug, Clone, PartialEq)]
pub struct AssignData {
    pub name: Spur,
    pub value: AstNode,
}


#[derive(Debug, Clone, PartialEq)]
pub struct IfData {
    pub if_branch: CondBranch,
    pub else_branch: Option<AstNode>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct CondData {
    pub cond_branches: Vec<CondBranch>,
    pub else_branch: Option<AstNode>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct WhileData {
    pub condition: AstNode,
    pub body: AstNode,
    pub is_do: bool,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ConsData {
    pub car: AstNode,
    pub cdr: AstNode,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ListAccData {
    pub index_expr: Option<AstNode>,
    pub pattern: Option<Spur>,
    pub list: AstNode,
}


#[derive(Debug, Clone, PartialEq)]
pub struct FuncCallData {
    pub name: Spur,
    pub arguments: Option<Vec<FuncArg>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ObjectCallData {
    pub name: Spur,
    pub accessors: LinkedList<Accessor>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ObjectAssignData {
    pub access: ObjectCallData,
    pub value: AstNode,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ExprFuncCallData {
    pub expr: AstNode,
    pub accessors: Option<Vec<Accessor>>,
    pub arguments: Option<Vec<FuncArg>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Accessor {
    pub name: Spur,
    pub is_field: bool,
    pub args: Option<Vec<FuncArg>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct FuncArg {
    pub value: AstNode,
    pub name: Option<Spur>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct InstArgs {
    pub name: Spur,
    pub value: AstNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenRandData {
    pub is_float: bool,
    pub lower: AstNode,
    pub upper: AstNode
}