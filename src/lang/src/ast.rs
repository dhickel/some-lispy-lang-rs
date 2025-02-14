use crate::ast::Value::U32;
use crate::ModifierFlags;
use crate::util::IString;
use crate::token::{Mod, Op};
use crate::types::{LangType, TypeEntry, TypeError, TypeId, TypeTable};


#[derive(Debug, Clone, PartialEq)]
pub struct AstData<T> {
    pub resolve_state: ResolveState,
    pub node_data: Box<T>,
    pub line_char: (u32, u32),
}





#[derive(Debug, Clone, PartialEq)]
pub enum ResolveState {
    Unresolved(LangType),
    Resolved(ResolveData),
}


impl ResolveState {
    pub fn is_resolved(&self) -> bool { !matches!(self, Self::Unresolved(_)) }

    pub fn get_type(&self) -> &LangType {
        match self {
            ResolveState::Unresolved(typ) => typ,
            ResolveState::Resolved(res) => &res.typ
        }
    }

    pub fn get_type_id(&self) -> Option<TypeId> {
        if let ResolveState::Resolved(res) = self {
            Some(res.type_id)
        } else { None }
    }

    pub fn get_type_and_id(&self) -> (&LangType, Option<TypeId>) {
        (self.get_type(), self.get_type_id())
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct ResolveData {
    pub ns_id: u16,
    pub scope_id: u32,
    pub type_entry: TypeEntry,
    pub meta_data: Option<MetaData>,
}


impl ResolveData {
   pub fn with_meta_data(mut self, meta: MetaData) -> Self {
       self.meta_data = Some(meta);
       self
   }
    
    pub fn new(ns_id: u16, scope_id: u32, type_entry: TypeEntry) -> Self {
        Self { ns_id, scope_id, type_entry, meta_data: None }
    }
}


#[derive(Debug, Clone, PartialEq, Default)]
pub enum MetaData {
    #[default]
    None,
    Primitive,
    Function(Option<Vec<FuncParam>>), // TODO add function classifications?
}

#[derive(Default, Debug)]

pub struct FuncMeta {
    pub params: Option<Vec<FuncParam>>,
    pub locals: Vec<TypeId>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam {
    pub name: Symbol,
    pub modifier_flags: ModifierFlags,
    pub typ: Option<LangType>,
    pub type_id: Option<TypeId>,
}


impl<T> AstData<T> {
    pub fn new(data: T, line_char: (u32, u32), typ: Option<LangType>) -> Self {
        Self {
            resolve_state: ResolveState::Unresolved(typ.unwrap_or_default()),
            node_data: Box::new(data),
            line_char,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Symbol {
    Definition { name: IString, is_defined: bool},
    Reference(IString),
}

impl Symbol {
    pub fn new_definition(name: IString) -> Self {
        Symbol::Definition { name, is_defined: false }
    }
    
    pub fn new_reference(name: IString) -> Self {
       Symbol::Reference(name) 
    }
    
    pub fn name(&self) -> IString {
        match self {
            Symbol::Definition { name, .. } | Symbol::Reference(name) => *name
        }
    }
    
}


#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Statement(StmntVariant),
    Expression(ExprVariant),
}


impl AstNode {}


////////////////
// Statements //
////////////////


#[derive(Debug, Clone, PartialEq)]
pub enum StmntVariant {
    Let(AstData<LetData>),
    Assign(AstData<AssignData>),
}


impl Into<AstNode> for StmntVariant {
    fn into(self) -> AstNode {
        AstNode::Statement(self)
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct AssignData {
    pub namespace: Option<IString>,
    pub identifier: Symbol,
    pub value: ExprVariant,
}


#[derive(Debug, Clone, PartialEq)]
pub struct LetData {
    pub identifier: Symbol,
    pub modifiers: Option<Vec<Mod>>,
    pub assignment: ExprVariant,
}


impl LetData {
    pub fn get_mod_sliced(&self) -> Option<&[Mod]> {
        if let Some(mods) = &self.modifiers {
            Some(mods)
        } else { None }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum ExprVariant {
    SCall(AstData<SCallData>),
    FCall(AstData<Vec<FExprData>>),
    Value(AstData<Value>),
    OpCall(AstData<OpCallData>),
    Block(AstData<Vec<AstNode>>),
    Predicate(AstData<PredicateData>),
    Lambda(AstData<LambdaData>),
}


impl ExprVariant {
    pub fn get_line_char(&self) -> (u32, u32) {
        match self {
            ExprVariant::SCall(data) => data.line_char,
            ExprVariant::FCall(data) => data.line_char,
            ExprVariant::Value(data) => data.line_char,
            ExprVariant::OpCall(data) => data.line_char,
            ExprVariant::Block(data) => data.line_char,
            ExprVariant::Predicate(data) => data.line_char,
            ExprVariant::Lambda(data) => data.line_char,
        }
    }

    pub fn get_resolve_state(&self) -> &ResolveState {
        match self {
            ExprVariant::SCall(data) => &data.resolve_state,
            ExprVariant::FCall(data) => &data.resolve_state,
            ExprVariant::Value(data) => &data.resolve_state,
            ExprVariant::OpCall(data) => &data.resolve_state,
            ExprVariant::Block(data) => &data.resolve_state,
            ExprVariant::Predicate(data) => &data.resolve_state,
            ExprVariant::Lambda(data) => &data.resolve_state,
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct SCallData {
    pub operation_expr: ExprVariant,
    pub operand_exprs: Option<Vec<ExprVariant>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct LambdaData {
    pub parameters: Option<Vec<Parameter>>,
    pub body_expr: ExprVariant,
    pub is_form: bool,
}



#[derive(Debug, Clone, PartialEq)]
pub enum FExprData {
    FCall { method: Option<Symbol>, arguments: Option<Vec<Argument>> },
    FAccess { identifier: Symbol, m_type: MType },
}


#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub modifiers: Option<Vec<Mod>>,
    pub expr: ExprVariant,
}


#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub modifiers: Option<Vec<Mod>>,
    pub identifier: Symbol,
    pub typ: Option<LangType>,
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
    pub operands: Option<Vec<ExprVariant>>,
}


impl Into<AstNode> for ExprVariant {
    fn into(self) -> AstNode {
        AstNode::Expression(self)
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct PredicateData {
    pub pred_expr: ExprVariant,
    pub then_expr: Option<ExprVariant>,
    pub else_expr: Option<ExprVariant>,
}


#[derive(Debug, Clone, PartialEq)]
pub enum Value {
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
    Identifier(Symbol),
}







