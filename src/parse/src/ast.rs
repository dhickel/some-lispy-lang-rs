use std::collections::LinkedList;
use lang::types::Type;
use crate::environment::{ScopeCtx, SymbolCtx};
use crate::token::{Mod, Op};


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
    ExprMulti(MultiExprData),
    ExprPrint(Box<AstNode>),
    ExprIf(Box<IfData>),
    ExprCond(Box<CondData>),
    ExprWhileLoop(Box<WhileData>),
    ExprCons(Box<ConsData>),
    ExprPairList(OpData),
    ExprArray(OpData),
    ExprListAccess(Box<ListAccData>),
    ExprFuncCall(Box<FuncCallData>),
    ExprFuncCalInner(Box<InnerFuncCallData>),
    ExprObjectCall(Box<ObjectCallData>),
    ExprLiteralCall(LiteralCallData),
    ExprObjectAssignment(Box<ObjectAssignData>),
    ExprGenRand(Box<GenRandData>),
    ExprDirectInst(Box<DirectInst>),
    ExprInitInst(Box<FuncCallData>),

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
    LitVector,
    LitPair,
    LitLambda,

}


trait CodeGen {
    fn is_rectified(&self) -> bool;
    fn get_code(&self, byte_code: &mut Vec<u8>) -> &Vec<u8>;
}


// Definition Data

#[derive(Debug, Clone, PartialEq)]
pub struct DefVarData {
    pub name: u64,
    pub modifiers: Option<Vec<Mod>>,
    pub value: AstNode,
    pub d_type: Option<u64>,
    pub ctx: Option<SymbolCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefLambdaData {
    pub modifiers: Option<Vec<Mod>>,
    pub parameters: Option<Vec<Param>>,
    pub body: AstNode,
    pub d_type: Option<u64>,
    pub typ: Type,
    pub ctx: Option<SymbolCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefFuncData {
    pub name: u64,
    pub lambda: DefLambdaData,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: u64,
    pub optional: bool,
    pub default_value: Option<AstNode>,
    pub dynamic: bool,
    pub mutable: bool,
    pub d_type: Option<u64>,
    pub c_type: Type,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefStructData {
    pub name: u64,
    pub fields: Option<Vec<Field>>,
    pub ctx: Option<SymbolCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefClassData {
    pub name: u64,
    pub params: Option<Vec<Mod>>,
    pub fields: Option<Vec<Field>>,
    pub init: Option<Vec<DefLambdaData>>,
    pub methods: Option<Vec<DefFuncData>>,
    pub pre_init: Option<AstNode>,
    pub post_init: Option<AstNode>,
    pub fin: Option<AstNode>,
    pub validate: Option<AstNode>,
    pub ctx: Option<SymbolCtx>,
}


impl DefClassData {
    pub fn empty_def(name: u64) -> DefClassData {
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
            ctx: None,
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct DirectInst {
    pub name: u64,
    pub args: Option<Vec<InstArgs>>,
    pub ctx: Option<ScopeCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: u64,
    pub modifiers: Option<Vec<Mod>>,
    pub p_type: Option<u64>,
    pub default_value: Option<AstNode>,
    pub c_type: Type,
}


// Operation Data
#[derive(Debug, Clone, PartialEq)]
pub struct OpData {
    pub operation: Op,
    pub operands: Vec<AstNode>,
    pub typ: Type,

}


// Expression Data
#[derive(Debug, Clone, PartialEq)]
pub struct MultiExprData {
    pub expressions: Vec<AstNode>,
    pub typ: Type,
}


#[derive(Debug, Clone, PartialEq)]
pub struct AssignData {
    pub name: u64,
    pub value: AstNode,
    pub ctx: Option<ScopeCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct CondBranch {
    pub cond_node: AstNode,
    pub then_node: AstNode,
    pub typ: Type,
}


#[derive(Debug, Clone, PartialEq)]
pub struct IfData {
    pub if_branch: CondBranch,
    pub else_branch: Option<AstNode>,
    pub else_type: Type,
}


impl IfData {
    pub fn all_types_same(&self) -> bool {
        self.if_branch.typ == self.else_type
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct CondData {
    pub cond_branches: Vec<CondBranch>,
    pub else_branch: Option<AstNode>,
    pub else_type: Type,
}


impl CondData {
    //todo ignore void for method call branching when implemented
    pub fn all_types_same(&self) -> bool {
        for branch in &self.cond_branches {
            if branch.typ != self.else_type {
                return false;
            }
        }
        true
    }
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
    pub pattern: Option<Vec<u8>>,
    pub list: AstNode,
}


#[derive(Debug, Clone, PartialEq)]
pub struct FuncCallData {
    pub name: u64,
    pub arguments: Option<Vec<FuncArg>>,
    pub ctx: Option<ScopeCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ObjectCallData {
    pub name: u64,
    pub accessors: LinkedList<Accessor>,
    pub ctx: Option<ScopeCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct LiteralCallData {
    pub name: u64,
    pub ctx: Option<ScopeCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ObjectAssignData {
    pub access: ObjectCallData,
    pub value: AstNode,
    pub ctx: Option<SymbolCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct InnerFuncCallData {
    pub expr: AstNode,
    pub accessors: Option<Vec<Accessor>>,
    pub arguments: Option<Vec<FuncArg>>,
    pub ctx: Option<ScopeCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Accessor {
    pub name: u64,
    pub is_field: bool,
    pub args: Option<Vec<FuncArg>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct FuncArg {
    pub value: AstNode,
    pub name: Option<u64>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct InstArgs {
    pub name: u64,
    pub value: AstNode,
}


#[derive(Debug, Clone, PartialEq)]
pub struct GenRandData {
    pub is_float: bool,
    pub lower: AstNode,
    pub upper: AstNode,
}