use crate::lang::datatypes::{Pair};
use crate::eval::environment::Env;
use crate::parse::ast_nodes::AstNode::LiteralNode;
use crate::parse::{Lit, Mod};

const NIL_LIT: AstNode = LiteralNode(LitNode::Nil(NilValue()));

#[derive(Debug)]
pub enum AstNode {
    DefinitionNode(Box<DefNode>),
    ExpressionNode(Box<ExprNode>),
    LiteralNode(LitNode),
    OperationNode(OpNode),
    ProgramNode(Vec<AstNode>),
}

impl AstNode {
    pub fn new_int_lit(i: i64) -> AstNode {
        LiteralNode(LitNode::Integer(IntValue(i)))
    }

    pub fn new_float_lit(f: f64) -> AstNode {
        LiteralNode(LitNode::Float(FloatValue(f)))
    }

    pub fn new_bool_lit(b: bool) -> AstNode {
        LiteralNode(LitNode::Boolean(BoolValue(b)))
    }

    pub fn new_string_lit(s: String) -> AstNode {
        LiteralNode(LitNode::String(StringValue(s)))
    }

    pub fn new_quote_lit(n: AstNode) -> AstNode {
        LiteralNode(LitNode::Quote(QuoteValue(Box::new(n))))
    }

    pub fn new_object_lit() -> AstNode {
        todo!()
    }

    pub fn new_nil_lit() -> AstNode { NIL_LIT }

    pub fn new_vector_lit(v: Vec<LitNode>) -> AstNode {
        LiteralNode(LitNode::Vector(VectorValue(v)))
    }

    pub fn new_pair_lit(p: Pair) -> AstNode {
        LiteralNode(LitNode::Pair(PairValue(Box::new(p))))
    }

    pub fn new_lambda_lit(def: DefNode, envs: Env) -> AstNode {
        LiteralNode(LitNode::Lambda(LambdaValue { def: Box::new(def), env: Box::new(envs) }))
    }
}


#[derive(Debug)]
pub enum DefNode {
    Variable(DefVarData),
    Lambda(DefLambdaData),
    Function(DefFuncData),
}

#[derive(Debug)]
pub struct DefVarData {
    pub name: String,
    pub modifiers: Option<Vec<Mod>>,
    pub value: Box<AstNode>,
    pub var_type: Option<String>,
}

#[derive(Debug)]
pub struct DefLambdaData {
    pub modifiers: Option<Vec<Mod>>,
    pub parameters: Option<Vec<Param>>,
    pub body: AstNode,
    pub rtn_type: Option<String>,
}

#[derive(Debug)]
pub struct DefFuncData {
    pub name: String,
    pub lambda: AstNode,
}

#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub typ: Option<String>,
    pub optional: bool,
    pub default_value: Option<AstNode>,
    pub dynamic: bool,
    pub mutable: bool,
}

#[derive(Debug)]
pub enum ExprNode {
    Assignment(AssignData),
    MultiExpr(Vec<AstNode>),
    PrintExpr(AstNode),
    IfExpr(IfData),
    CondExpr(CondData),
    WhileLoop(WhileData),
    ConsExpr(ConsData),
    PairList(Vec<AstNode>),
    ListAccess(ListAccData),
    FuncCall(FuncCallData),
    LiteralCall(String),
}

#[derive(Debug)]
pub struct CondBranch {
    pub cond_node: AstNode,
    pub then_node: AstNode,
}

#[derive(Debug)]
pub struct AssignData {
    pub name: String,
    pub value: AstNode,
}

#[derive(Debug)]
pub struct IfData {
    pub if_branch: CondBranch,
    pub else_branch: Option<AstNode>,
}

#[derive(Debug)]
pub struct CondData {
    pub cond_branches: Vec<CondBranch>,
    pub else_branch: Option<AstNode>,
}

#[derive(Debug)]
pub struct WhileData {
    pub condition: AstNode,
    pub body: AstNode,
    pub is_do: bool,
}

#[derive(Debug)]
pub struct ConsData {
    pub car: AstNode,
    pub cdr: AstNode,
}

#[derive(Debug)]
pub struct ListAccData {
    pub index_expr: Option<AstNode>,
    pub pattern: Option<String>,
}

#[derive(Debug)]
pub struct FuncCallData {
    pub name: String,
    pub accessors: Option<Vec<Accessor>>,
    pub arguments: Option<Vec<FuncArg>>,
}


#[derive(Debug, PartialEq)]
pub struct Accessor {
    pub name: String,
    pub is_field: bool,
}

#[derive(Debug)]
pub struct FuncArg {
    pub value: AstNode,
    pub name: Option<String>,
}


#[derive(Debug)]
pub enum OpNode {
    Addition(Vec<AstNode>),
    Subtraction(Vec<AstNode>),
    Multiplication(Vec<AstNode>),
    Division(Vec<AstNode>),
    Modulo(Vec<AstNode>),
    Exponentiate(Vec<AstNode>),
    Increment(Vec<AstNode>),
    Decrement(Vec<AstNode>),
    Or(Vec<AstNode>),
    And(Vec<AstNode>),
    Xor(Vec<AstNode>),
    Nor(Vec<AstNode>),
    Xnor(Vec<AstNode>),
    Nand(Vec<AstNode>),
    Negate(Vec<AstNode>),
    GreaterThan(Vec<AstNode>),
    GreaterThanEqual(Vec<AstNode>),
    LessThan(Vec<AstNode>),
    LessThanEqual(Vec<AstNode>),
    Equality(Vec<AstNode>),
    RefEquality(Vec<AstNode>),
    RefNonEquality(Vec<AstNode>),
}

#[derive(Debug, PartialEq)]
pub enum OpType {
    Arithmetic,
    Boolean,
    Comparison,
    Equality,
}

#[derive(Debug)]
pub enum LitNode {
    Integer(IntValue),
    Float(FloatValue),
    Boolean(BoolValue),
    String(StringValue),
    Quote(QuoteValue),
    Object(ObjectValue),
    // TODO figure this out
    Nil(NilValue),
    Vector(VectorValue),
    Pair(PairValue),
    Lambda(LambdaValue),
}

impl LitNode {
   pub fn value(&self) -> &dyn EvalResult {
        match self {
            LitNode::Integer(val) => val,
            LitNode::Float(val) => val,
            LitNode::Boolean(val) => val,
            LitNode::String(val) => val,
            LitNode::Quote(val) => val,
            LitNode::Object(val) => val,
            LitNode::Nil(val) => val,
            LitNode::Vector(val) => val,
            LitNode::Pair(val) => val,
            LitNode::Lambda(val) => val,
        }
    }
}

pub trait EvalResult {
    fn as_int(&self) -> i64;
    fn as_float(&self) -> f64;
    fn as_bool(&self) -> bool;
    fn as_string(&self) -> String;
    fn get_type(&self) -> String;
}

#[derive(Debug)]
pub struct IntValue(pub i64);

impl EvalResult for IntValue {
    fn as_int(&self) -> i64 { self.0 }
    fn as_float(&self) -> f64 { self.0 as f64 }
    fn as_bool(&self) -> bool { if self.0 == 0 { true } else { false } }
    fn as_string(&self) -> String { self.0.to_string() }
    fn get_type(&self) -> String { "int".to_string() }
}

#[derive(Debug)]
pub struct FloatValue(pub f64);

impl EvalResult for FloatValue {
    fn as_int(&self) -> i64 { self.0 as i64 }
    fn as_float(&self) -> f64 { self.0 }
    fn as_bool(&self) -> bool { if self.0 == 0.0 { true } else { false } }
    fn as_string(&self) -> String { self.0.to_string() }
    fn get_type(&self) -> String { "float".to_string() }
}

#[derive(Debug)]
pub struct BoolValue(pub bool);

impl EvalResult for BoolValue {
    fn as_int(&self) -> i64 { if self.0 { 1 } else { 0 } }
    fn as_float(&self) -> f64 { if self.0 { 1.0 } else { 0.0 } }
    fn as_bool(&self) -> bool { self.0 }
    fn as_string(&self) -> String { if self.0 { "#t".to_string() } else { "#f".to_string() } }
    fn get_type(&self) -> String { "Boolean".to_string() }
}

#[derive(Debug)]
pub struct StringValue(pub String);

impl EvalResult for StringValue {
    fn as_int(&self) -> i64 { self.0.len() as i64 }
    fn as_float(&self) -> f64 { self.0.len() as f64 }
    fn as_bool(&self) -> bool { if self.0.is_empty() { false } else { true } }
    fn as_string(&self) -> String { self.0.to_string() }
    fn get_type(&self) -> String { "String".to_string() }
}

#[derive(Debug)]
pub struct QuoteValue(pub Box<AstNode>);

impl EvalResult for QuoteValue {
    fn as_int(&self) -> i64 { 1 }
    fn as_float(&self) -> f64 { 1.0 }
    fn as_bool(&self) -> bool { true }
    fn as_string(&self) -> String { "$Quote".to_string() }
    fn get_type(&self) -> String { "Quote".to_string() }
}

#[derive(Debug)]
pub struct ObjectValue(pub ());

impl EvalResult for ObjectValue {
    fn as_int(&self) -> i64 { 1 }
    fn as_float(&self) -> f64 { 1.0 }
    fn as_bool(&self) -> bool { true }
    fn as_string(&self) -> String { "$Object".to_string() }
    fn get_type(&self) -> String { "Object".to_string() }
}

#[derive(Debug)]
pub struct NilValue();

impl EvalResult for NilValue {
    fn as_int(&self) -> i64 { 0 }
    fn as_float(&self) -> f64 { 0.0 }
    fn as_bool(&self) -> bool { false }
    fn as_string(&self) -> String { "$Nil".to_string() }
    fn get_type(&self) -> String { "Nil".to_string() }
}

#[derive(Debug)]
pub struct VectorValue(pub Vec<LitNode>);

impl EvalResult for VectorValue {
    fn as_int(&self) -> i64 { self.0.len() as i64 }
    fn as_float(&self) -> f64 { self.0.len() as f64 }
    fn as_bool(&self) -> bool { false }
    fn as_string(&self) -> String { "{:?}".to_string() }
    fn get_type(&self) -> String { "Vector<Type>".to_string() }
}

impl FromIterator<LitNode> for VectorValue {
    fn from_iter<I: IntoIterator<Item=LitNode>>(iter: I) -> Self {
        let mut vector = Vec::new();
        for item in iter {
            vector.push(item);
        }
        VectorValue(vector)
    }
}

#[derive(Debug)]
pub struct PairValue(pub Box<Pair>);

impl EvalResult for PairValue {
    fn as_int(&self) -> i64 { 1 }
    fn as_float(&self) -> f64 { 1.0 }
    fn as_bool(&self) -> bool { true }
    fn as_string(&self) -> String { "Pair".to_string() }
    fn get_type(&self) -> String { "Pair".to_string() }
}

#[derive(Debug)]
pub struct LambdaValue {
    pub def: Box<DefNode>,
    pub env: Box<Env>,
}

impl EvalResult for LambdaValue {
    fn as_int(&self) -> i64 { 1 }
    fn as_float(&self) -> f64 { 1.0 }
    fn as_bool(&self) -> bool { true }
    fn as_string(&self) -> String { "$Lambda".to_string() }
    fn get_type(&self) -> String { "Lambda".to_string() }
}



