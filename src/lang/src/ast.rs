use crate::ast::Value::U32;
use crate::ModifierFlags;
use crate::util::IString;
use crate::token::{Mod, Op};
use crate::types::{Type, TypeError, TypeId, TypeTable};


#[derive(Debug, Clone, PartialEq)]
pub struct AstData<T> {
    pub resolve_state: ResolveState,
    pub node_data: Box<T>,
    pub line_char: (u32, u32),
}


impl<T> AstData<T> {
    pub fn update_resolve_state(&mut self, state: ResolveState) -> &ResolveState {
        self.resolve_state = state;
        &self.resolve_state
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum ResolveState {
    Unresolved(Type),
    Resolved(ResolveData),
}


impl ResolveState {
    pub fn is_resolved(&self) -> bool { matches!(self, Self::Unresolved(_)) }

    pub fn get_type(&self) -> &Type {
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

    pub fn get_type_and_id(&self) -> (&Type, Option<TypeId>) {
        (self.get_type(), self.get_type_id())
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct ResolveData {
    pub type_id: TypeId,
    pub ns_id: u16,
    pub scope_id: u32,
    pub typ: Type,
    pub meta_data: Option<MetaData>,
}


impl ResolveData {
    pub fn new(ns_id: u16, scope_id: u32, type_id: TypeId, typ: Type) -> Self {
        if matches!(typ, Type::Unresolved(_)) {
            panic!("Fatal<internal>: Constructed resolution data with unresolved type")
        }
        Self { type_id, ns_id, scope_id, typ, meta_data: None }
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
    pub name: IString,
    pub modifier_flags: ModifierFlags,
    pub typ: Option<Type>,
    pub type_id: Option<TypeId>,
}


impl<T> AstData<T> {
    pub fn new(data: T, line_char: (u32, u32), typ: Option<Type>) -> Self {
        Self {
            resolve_state: ResolveState::Unresolved(typ.unwrap_or_default()),
            node_data: Box::new(data),
            line_char,
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
    pub identifier: IString,
    pub value: ExprVariant,
}


#[derive(Debug, Clone, PartialEq)]
pub struct LetData {
    pub identifier: IString,
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
}


#[derive(Debug, Clone, PartialEq)]
pub enum FExprData {
    FCall { method: Option<IString>, arguments: Option<Vec<Argument>> },
    FAccess { identifier: IString, m_type: MType },
}


#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub modifiers: Option<Vec<Mod>>,
    pub expr: ExprVariant,
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
    Identifier(IString),
}


impl Value {
    pub fn get_type_info_if_primitive(&self) -> Option<(Type, TypeId)> {
        match self {
            Value::I32(_) | Value::I64(_) | Value::U8(_) |
            Value::U16(_) | Value::U32(_) | Value::U64(_) => Some((Type::Integer, TypeTable::INT)),
            Value::F32(_) | Value::F64(_) => Some((Type::Float, TypeTable::FLOAT)),
            Value::Boolean(_) => Some((Type::Boolean, TypeTable::BOOL)),
            Value::String => Some((Type::String, TypeTable::STRING)),
            _ => None
        }
    }
}






