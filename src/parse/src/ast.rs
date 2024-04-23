use std::collections::LinkedList;
use lang::environment::SymbolCtx;
use lang::types::Type;
use lang::util::IString;
use crate::types::Type;
use crate::environment::{ScopeCtx, SymbolCtx};
use crate::token::{Mod, Op};
use crate::util::IString;


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
    pub name: IString,
    pub modifiers: Option<Vec<Mod>>,
    pub value: AstNode,
    pub d_type: Option<IString>,
    pub ctx: Option<SymbolCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefLambdaData {
    pub modifiers: Option<Vec<Mod>>,
    pub parameters: Option<Vec<Param>>,
    pub body: AstNode,
    pub d_type: Option<IString>,
    pub typ: Type,
    pub ctx: Option<SymbolCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefFuncData {
    pub name: IString,
    pub lambda: DefLambdaData,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: IString,
    pub optional: bool,
    pub default_value: Option<AstNode>,
    pub dynamic: bool,
    pub mutable: bool,
    pub d_type: Option<IString>,
    pub c_type: Type,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefStructData {
    pub name: IString,
    pub fields: Option<Vec<Field>>,
    pub ctx: Option<SymbolCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefClassData {
    pub name: IString,
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
            ctx: None,
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct DirectInst {
    pub name: IString,
    pub args: Option<Vec<InstArgs>>,
    pub ctx: Option<ScopeCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: IString,
    pub modifiers: Option<Vec<Mod>>,
    pub p_type: Option<IString>,
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
    pub name: IString,
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
    pub name: IString,
    pub arguments: Option<Vec<FuncArg>>,
    pub ctx: Option<ScopeCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ObjectCallData {
    pub name: IString,
    pub accessors: LinkedList<Accessor>,
    pub ctx: Option<ScopeCtx>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct LiteralCallData {
    pub name: IString,
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
    pub name: IString,
    pub is_field: bool,
    pub args: Option<Vec<FuncArg>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct FuncArg {
    pub value: AstNode,
    pub name: Option<IString>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct InstArgs {
    pub name: IString,
    pub value: AstNode,
}


#[derive(Debug, Clone, PartialEq)]
pub struct GenRandData {
    pub is_float: bool,
    pub lower: AstNode,
    pub upper: AstNode,
}