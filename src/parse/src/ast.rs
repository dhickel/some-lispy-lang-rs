use std::collections::LinkedList;

use lang::util::IString;
use crate::environment::{ExprContext, ResData, SymbolCtx};
use crate::token::{Mod, Op};
use crate::types::Type;


#[derive(Debug, Clone, PartialEq)]
pub struct AstNode {
    pub node_data: Box<AstData>,
    pub line_char: (u32, u32),
    pub res_data: Option<ResData>,
}


impl AstNode {
    pub fn resolved_type(&self) -> Option<Type> {
        self.res_data.as_ref().map(|res_data| res_data.type_data.typ.clone())
    }
}


impl AstNode {
    pub fn new(node_data: AstData, line_char: (u32, u32)) -> Self {
        AstNode {
            node_data: Box::new(node_data),
            line_char,
            res_data: None,
        }
    }
}

// Nodes

#[derive(Debug, Clone, PartialEq)]
pub enum AstData {
    // Definition
    DefVariable(DefVarData),
    DefLambda(DefLambdaData),
    DefFunction(DefFuncData),
    DefStruct(DefStructData),
    DefClass(DefClassData),

    // Expressions
    ExprAssignment(AssignData),
    ExprMulti(MultiExprData),
    ExprPrint(AstNode),
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
    pub value: AstNode,
    pub d_type: Option<IString>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefLambdaData {
    pub modifiers: Option<Vec<Mod>>,
    pub parameters: Option<Vec<Param>>,
    pub body: AstNode,
    pub d_type: Option<IString>,
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
    pub p_type: Option<IString>,
    pub default_value: Option<AstNode>,
}


// Operation Data
#[derive(Debug, Clone, PartialEq)]
pub struct OpData {
    pub operation: Op,
    pub operands: Vec<AstNode>,
}


// Expression Data
#[derive(Debug, Clone, PartialEq)]
pub struct MultiExprData {
    pub expressions: Vec<AstNode>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct AssignData {
    pub name: IString,
    pub namespace: Option<IString>,
    pub value: AstNode,
}


#[derive(Debug, Clone, PartialEq)]
pub struct CondBranch {
    pub cond_node: AstNode,
    pub then_node: AstNode,
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
    pub pattern: Option<Vec<u8>>,
    pub list: AstNode,

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
    pub value: AstNode,

}


#[derive(Debug, Clone, PartialEq)]
pub struct InnerFuncCallData {
    pub expr: AstNode,
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