use std::any::Any;
use crate::ModifierFlags;
use crate::util::IString;
use crate::token::{Mod, Op};
use crate::types::{LangType, PrimitiveType, TypeEntry, TypeError, TypeId, TypeTable};


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
    pub fn is_node_resolved(&self) -> bool { !matches!(self, Self::Unresolved(_)) }

    pub fn get_type_entry(&self) -> Option<&TypeEntry> {
        if let ResolveState::Resolved(res) = self {
            Some(&res.type_entry)
        } else { None }
    }

    pub fn get_lang_type(&self) -> &LangType {
        match self {
            ResolveState::Unresolved(typ) => typ,
            ResolveState::Resolved(res_data) => res_data.type_entry.lang_type()
        }
    }
}

#[derive(Copy, Debug, Clone, PartialEq)]
pub struct ScopeContext {
    pub ns_id: u16,
    pub scope_id: u32,
    pub depth: u32,
}

#[derive(Debug, Copy, Clone, PartialEq, Default)]
pub enum TypeConversion {
    #[default]
    None,
    Primitive(PrimitiveType),
    Custom(TypeId),
}


impl TypeConversion {
    // pub fn conv_type_id(&self) -> TypeId {
    //     match self {
    //         TypeConversion::None => panic!("TypeConversion::conv_type_id called on None"),
    //         TypeConversion::Primitive(prim) => {
    //             match prim {
    //                 PrimitiveType::U8 => TypeTable::U8.id(),
    //                 PrimitiveType::U16 => TypeTable::U16.id(),
    //                 PrimitiveType::U32 => TypeTable::U32.id(),
    //                 PrimitiveType::U64 => TypeTable::U64.id(),
    //                 PrimitiveType::I32 => TypeTable::I32.id(),
    //                 PrimitiveType::I64 => TypeTable::I64.id(),
    //                 PrimitiveType::F32 => TypeTable::F32.id(),
    //                 PrimitiveType::F64 => TypeTable::F64.id(),
    //                 PrimitiveType::Bool => TypeTable::BOOL.id(),
    //                 PrimitiveType::Nil => TypeTable::NIL.id(),
    //             }
    //         }
    //         TypeConversion::Custom(id) => *id
    //     }
    // }

    pub fn is_some(&self) -> bool { !matches!(self, TypeConversion::None) }
}


#[derive(Debug, Clone, PartialEq)]
pub struct ResolveData {
    pub scope_context: ScopeContext,
    pub type_entry: TypeEntry,
    pub type_conversion: TypeConversion,
}



impl ResolveData {
    pub fn with_type_conversion(mut self, conversion: TypeConversion) -> Self {
        self.type_conversion = conversion;
        self
    }

    pub fn new(scope_context: ScopeContext, type_entry: TypeEntry) -> Self {
        Self { scope_context, type_entry, type_conversion: TypeConversion::None }
    }

    pub fn conversion_needed(&self) -> bool { self.type_conversion != TypeConversion::None }
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


impl<T:> AstData<T> {
    pub fn new(data: T, line_char: (u32, u32), typ: Option<LangType>) -> Self {
        Self {
            resolve_state: ResolveState::Unresolved(typ.unwrap_or(LangType::Undefined)),
            node_data: Box::new(data),
            line_char,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Symbol {
    Definition { name: IString, is_defined: bool },
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

impl From<Symbol> for IString {
    fn from(symbol: Symbol) -> Self { symbol.name() }
}


#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Statement(StmntVariant),
    Expression(ExprVariant),
}

impl AstNode {
    pub fn get_inner_resolve_state(&self) -> ResolveState {
        match self {
            AstNode::Statement(stmt) => {}
            AstNode::Expression(expr) => {}
        }
    }
}




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
    pub expr: ExprVariant,
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

    pub fn get_resolve_state_mut(&mut self) -> &mut ResolveState {
        match self {
            ExprVariant::SCall(data) => &mut data.resolve_state,
            ExprVariant::FCall(data) => &mut data.resolve_state,
            ExprVariant::Value(data) => &mut data.resolve_state,
            ExprVariant::OpCall(data) => &mut data.resolve_state,
            ExprVariant::Block(data) => &mut data.resolve_state,
            ExprVariant::Predicate(data) => &mut data.resolve_state,
            ExprVariant::Lambda(data) => &mut data.resolve_state,
        }
    }


    // FIXME: this should be atleast be unsafe, or maybe switch to an internal error idk?
    pub fn add_type_conversion(&mut self, conversion: TypeConversion) {
        let resolve_state = self.get_resolve_state_mut();

        if let ResolveState::Resolved(resolve_data) = resolve_state {
            resolve_data.type_conversion = conversion;
        } else {
            panic!("Fatal<Internal>: Attempted to add type conversion to unresolved node, this path should not occur")
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
    pub parameters: Vec<Parameter>,
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
    pub then_expr: ExprVariant,
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







