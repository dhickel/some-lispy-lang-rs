use crate::lang::datatypes::Pair;
use crate::eval::environment::Env;
use crate::parse::ast_nodes::AstNode::LiteralNode;
use crate::parse::Mod;


#[derive(Debug)]
pub enum AstNode {
    DefinitionNode(Box<DefNode>),
    ExpressionNode(Box<ExprNode>),
    LiteralNode(Box<LitNode>),
    OperationNode(OpNode),
    ProgramNode(Vec<AstNode>),
}

#[derive(Debug)]
pub enum DefNode {
    Variable {
        name: String,
        modifiers: Option<Vec<Mod>>,
        value: Box<AstNode>,
        var_type: Option<String>,
    },
    Lambda { modifiers: Option<Vec<Mod>>, parameters: Option<Vec<Param>>, body: AstNode, rtn_type: Option<String> },
    Function { name: String, lambda: AstNode },
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
    Assignment { name: String, value: AstNode },
    MultiExpr(Vec<AstNode>),
    SingleExpr(AstNode),
    PrintExpr(AstNode),
    IfExpr { if_branch: CondBranch, else_branch: Option<AstNode> },
    CondExpr { cond_branches: Vec<CondBranch>, else_branch: Option<AstNode> },
    WhileLoop { condition: AstNode, body: AstNode, is_do: bool },
    ConsExpr { car: AstNode, cdr: AstNode },
    PairList(Vec<AstNode>),
    ListAccess { index_expr: Option<AstNode>, pattern: Option<String> },
    FuncCall { name: String, accessors: Option<Vec<Accessor>>, arguments: Option<Vec<FuncArg>> },
    LiteralCall(String),
}

#[derive(Debug)]
pub struct CondBranch {
    pub cond_node: AstNode,
    pub then_node: AstNode,
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
    Addition { op_type: OpType, operands: Vec<AstNode> },
    Subtraction { op_type: OpType, operands: Vec<AstNode> },
    Multiplication { op_type: OpType, operands: Vec<AstNode> },
    Division { op_type: OpType, operands: Vec<AstNode> },
    Modulo { op_type: OpType, operands: Vec<AstNode> },
    Exponentiate { op_type: OpType, operands: Vec<AstNode> },
    Increment { op_type: OpType, operands: Vec<AstNode> },
    Decrement { op_type: OpType, operands: Vec<AstNode> },
    Or { op_type: OpType, operands: Vec<AstNode> },
    And { op_type: OpType, operands: Vec<AstNode> },
    Xor { op_type: OpType, operands: Vec<AstNode> },
    Nor { op_type: OpType, operands: Vec<AstNode> },
    Xnor { op_type: OpType, operands: Vec<AstNode> },
    Nand { op_type: OpType, operands: Vec<AstNode> },
    Negate { op_type: OpType, operands: Vec<AstNode> },
    GreaterThan { op_type: OpType, operands: Vec<AstNode> },
    GreaterThanEqual { op_type: OpType, operands: Vec<AstNode> },
    LessThan { op_type: OpType, operands: Vec<AstNode> },
    LessThanEqual { op_type: OpType, operands: Vec<AstNode> },
    Equality { op_type: OpType, operands: Vec<AstNode> },
    RefEquality { op_type: OpType, operands: Vec<AstNode> },
    RefNonEquality { op_type: OpType, operands: Vec<AstNode> },
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
    Integer(i64),
    Float(f64),
    String(String),
    Quote(AstNode),
    Boolean(bool),
    Object(()),
    // TODO figure this out
    Nil,
    Vector(Vec<LitNode>),
    Pair(Box<Pair>),
    Lambda { def: DefNode, env: Env },
}

#[derive(Debug, PartialEq)]
pub struct LitValue {
    pub int_value: i64,
    pub float_value: f64,
    pub bool_value: bool,
    pub string_value: String,
    pub obj_value: (),
}


impl LitNode {
    pub fn int_value(&self) -> i64 {
        match self {
            LitNode::Integer(val) => *val,
            LitNode::Float(val) => *val as i64,
            LitNode::String(val) => if val.is_empty() { 0 } else { 1 }
            LitNode::Quote(_) => 1,
            LitNode::Boolean(val) => if *val == true { 1 } else { 0 },
            LitNode::Object(_) => 1,
            LitNode::Nil => 0,
            LitNode::Vector(vec) => if vec.is_empty() { 0 } else { 1 }
            LitNode::Pair(_) => 1,
            LitNode::Lambda { .. } => 1
        }
    }

    pub fn float_value(&self) -> f64 {
        match self {
            LitNode::Integer(val) => *val as f64,
            LitNode::Float(val) => *val,
            LitNode::String(val) => if val.is_empty() { 0.0 } else { 1.0 }
            LitNode::Quote(_) => 1.0,
            LitNode::Boolean(val) => if *val == true { 1.0 } else { 0.0 },
            LitNode::Object(_) => 1.0,
            LitNode::Nil => 0.0,
            LitNode::Vector(vec) => if vec.is_empty() { 0.0 } else { 1.0 }
            LitNode::Pair(_) => 1.0,
            LitNode::Lambda { .. } => 1.0
        }
    }

    // pub fn string_value(&self) -> &str {
    //     match self {
    //         LitNode::IntLiteral(val) => &val.to_string(),
    //         LitNode::FloatLiteral(val) => &val.to_string(),
    //         LitNode::StringLiteral(val) => &val,
    //         LitNode::QuoteLiteral(val) => "quote", // todo implement to_string on nodes
    //         LitNode::BooleanLiteral(val) => if *val { "#t" } else { "#f" },
    //         LitNode::ObjectLiteral(_) => "object",
    //         LitNode::NilLiteral() => "#nil",
    //         LitNode::VectorLiteral(vec) => { // TODO better impl
    //             let mut string = String::new();
    //             string.push('[');
    //             for val in vec {
    //                 string.push_str(val.string_value());
    //                 string.push(',');
    //             }
    //             &string
    //         }
    //         LitNode::PairLiteral(_) => "pair",
    //         LitNode::LambdaLiteral { .. } => "lambda"
    //     }
    // }
}


