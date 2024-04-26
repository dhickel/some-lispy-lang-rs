use std::collections::LinkedList;

use lang::util::IString;
use crate::environment::{ExprContext, ResData, SymbolCtx};
use crate::token::{Mod, Op};
use crate::types::Type;


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
    LitArray,
    LitPair,
    LitLambda,

}


// TODO Fix this ugly mess refactor all to use a Node Type with {AstNode, ResData}
impl AstNode {
    pub fn resolved_type(&self) -> Option<Type> {
        match self {
            AstNode::DefVariable(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::DefLambda(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::DefFunction(data) => {
                if let Some(res_data) = data.lambda.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::DefStruct(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::DefClass(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::ExprAssignment(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::ExprMulti(data) => {
                if let Some(res_data) = &data.res_data {
                    Some(res_data.type_data.typ.clone())
                } else { None }
            }
            AstNode::ExprPrint(data) => data.resolved_type(),
            AstNode::ExprIf(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::ExprCond(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::ExprWhileLoop(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::ExprCons(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::ExprPairList(data) => {
                if let Some(res_data) = &data.res_data {
                    Some(res_data.type_data.typ.clone())
                } else { None }
            }
            AstNode::ExprArray(data) => {
                if let Some(res_data) = &data.res_data {
                    Some(res_data.type_data.typ.clone())
                } else { None }
            }
            AstNode::ExprListAccess(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::ExprFuncCall(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::ExprFuncCalInner(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::ExprObjectCall(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::ExprLiteralCall(data) => {
                if let Some(res_data) = &data.res_data {
                    Some(res_data.type_data.typ.clone())
                } else { None }
            }
            AstNode::ExprObjectAssignment(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::ExprGenRand(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::ExprDirectInst(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::ExprInitInst(data) => {
                if let Some(res_data) = data.res_data {
                    Some(res_data.type_data.typ)
                } else { None }
            }
            AstNode::Operation(data) => {
                if let Some(res_data) = &data.res_data {
                    Some(res_data.type_data.typ.clone())
                } else { None }
            }
            AstNode::LitInteger(data) => Some(Type::Integer),
            AstNode::LitFloat(data) => Some(Type::Float),
            AstNode::LitBoolean(data) => Some(Type::Boolean),
            AstNode::LitString(data) => Some(Type::String),
            AstNode::LitQuote => Some(Type::Unresolved),
            AstNode::LitObject => Some(Type::Unresolved),
            AstNode::LitStruct() => Some(Type::Unresolved),
            AstNode::LitNil => Some(Type::Nil),
            AstNode::LitArray => Some(Type::Unresolved),
            AstNode::LitPair => Some(Type::Unresolved),
            AstNode::LitLambda => Some(Type::Unresolved),
        }
    }
}


// Definition Data

#[derive(Debug, Clone, PartialEq)]
pub struct DefVarData {
    pub name: IString,
    pub modifiers: Option<Vec<Mod>>,
    pub value: AstNode,
    pub d_type: Option<IString>,
    pub res_data: Option<ResData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefLambdaData {
    pub modifiers: Option<Vec<Mod>>,
    pub parameters: Option<Vec<Param>>,
    pub body: AstNode,
    pub d_type: Option<IString>,
    pub res_data: Option<ResData>,
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
    pub c_type: Type,
    // TODO this need resolved to ensure existence
    pub d_type: Option<IString>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefStructData {
    pub name: IString,
    pub fields: Option<Vec<Field>>,
    pub res_data: Option<ResData>,
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
    pub res_data: Option<ResData>,
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
            res_data: None,
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct DirectInst {
    pub name: IString,
    pub namespace: Option<IString>,
    pub args: Option<Vec<InstArgs>>,
    pub res_data: Option<ResData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: IString,
    pub modifiers: Option<Vec<Mod>>,
    pub p_type: Option<IString>,
    pub default_value: Option<AstNode>,
}


// Operation Data
#[derive(Debug, Clone, PartialEq)]
pub struct OpData {
    pub operation: Op,
    pub operands: Vec<AstNode>,
    pub res_data: Option<ResData>,

}


// Expression Data
#[derive(Debug, Clone, PartialEq)]
pub struct MultiExprData {
    pub expressions: Vec<AstNode>,
    pub res_data: Option<ResData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct AssignData {
    pub name: IString,
    pub namespace: Option<IString>,
    pub value: AstNode,
    pub res_data: Option<ResData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct CondBranch {
    pub cond_node: AstNode,
    pub then_node: AstNode,
    pub typ: Option<Type>,
    pub res_data: Option<ResData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct IfData {
    pub if_branch: CondBranch,
    pub else_type: Option<Type>,
    pub else_branch: Option<AstNode>,
    pub res_data: Option<ResData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct CondData {
    pub cond_branches: Vec<CondBranch>,
    pub else_branch: Option<AstNode>,
    pub res_data: Option<ResData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct WhileData {
    pub condition: AstNode,
    pub body: AstNode,
    pub is_do: bool,
    pub res_data: Option<ResData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ConsData {
    pub car: AstNode,
    pub cdr: AstNode,
    pub res_data: Option<ResData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ListAccData {
    pub index_expr: Option<AstNode>,
    pub pattern: Option<Vec<u8>>,
    pub list: AstNode,
    pub res_data: Option<ResData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct FuncCallData {
    pub name: IString,
    pub namespace: Option<IString>,
    pub arguments: Option<Vec<FuncArg>>,
    pub res_data: Option<ResData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ObjectCallData {
    pub name: IString,
    pub namespace: Option<IString>,
    pub accessors: LinkedList<Accessor>,
    pub res_data: Option<ResData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct LiteralCallData {
    pub name: IString,
    pub namespace: Option<IString>,
    pub res_data: Option<ResData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ObjectAssignData {
    pub access: ObjectCallData,
    pub namespace: Option<IString>,
    pub value: AstNode,
    pub res_data: Option<ResData>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct InnerFuncCallData {
    pub expr: AstNode,
    pub namespace: Option<IString>,
    pub accessors: Option<Vec<Accessor>>,
    pub arguments: Option<Vec<FuncArg>>,
    pub res_data: Option<ResData>,
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
    pub res_data: Option<ResData>,
}