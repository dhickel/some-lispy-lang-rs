use std::cell::{Ref, RefCell};
use std::collections::LinkedList;
use std::rc::Rc;
use std::vec;
use lasso::Spur;
use crate::eval::environment::Environment;
use crate::lang::datatypes::{ClassData, ClassMetaData, ObjectAccess, StructData};
use crate::lang::types::Type;
use crate::parse::ast_nodes::AstNode::LiteralNode;
use crate::parse::{Lit, Mod};


const NIL_LIT: LitNode = LitNode::Nil(NilValue());
const TRUE_LIT: LitNode = LitNode::Boolean(BoolValue(true));
const FALSE_LIT: LitNode = LitNode::Boolean(BoolValue(false));


#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    DefinitionNode(Box<DefNode>),
    ExpressionNode(Box<ExprNode>),
    LiteralNode(Rc<LitNode>),
    OperationNode(OpNode),
    ProgramNode(Vec<AstNode>),
}


impl AstNode {
    pub fn new_int_lit(i: i64) -> AstNode {
        LiteralNode(Rc::new(LitNode::Integer(IntValue(i))))
    }

    pub fn new_float_lit(f: f64) -> AstNode {
        LiteralNode(Rc::new(LitNode::Float(FloatValue(f))))
    }

    pub fn new_bool_lit(b: bool) -> AstNode {
        if b { LiteralNode(Rc::new(TRUE_LIT)) } else { LiteralNode(Rc::new(FALSE_LIT)) }
    }

    pub fn new_string_lit(s: String) -> AstNode {
        LiteralNode(Rc::new(LitNode::String(StringValue(s))))
    }

    pub fn new_quote_lit(n: AstNode) -> AstNode {
        LiteralNode(Rc::new(LitNode::Quote(QuoteValue(Box::new(n)))))
    }

    pub fn new_object_lit() -> AstNode {
        todo!()
    }

    pub fn new_nil_lit() -> AstNode { LiteralNode(Rc::new(NIL_LIT)) }


    pub fn new_vector_lit(v: Vec<LitNode>) -> AstNode {
        LiteralNode(Rc::new(LitNode::Vector(VectorValue(v))))
    }

    // pub fn new_pair_lit(car: LitNode, cdr: LitNode) -> AstNode {
    //     LiteralNode(Rc::new(LitNode::Pair(PairValue { car: Box::new(car), cdr: Box::new(cdr) })))
    // }

    // pub fn new_raw_pair(car: LitNode, cdr: LitNode) -> Rc<LitNode> {
    //     Rc::new(LitNode::Pair(PairValue { car: Box::new(car), cdr: Box::new(cdr) }))
    // }

    pub fn new_lambda_lit(def: DefLambdaData, envs: Rc<RefCell<Environment>>) -> AstNode {
        LiteralNode(Rc::new(LitNode::Lambda(LambdaValue { def: Box::new(def), env: envs })))
    }

    pub fn new_struct_lit(data: StructData) -> AstNode {
        LiteralNode(Rc::new(LitNode::Struct(data)))
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum DefNode {
    Variable(DefVarData),
    Lambda(DefLambdaData),
    Function(DefFuncData),
    StructDef(DefStructData),
    ClassDef(DefClassData),
    InstanceDef(DefStructInst),
}


#[derive(Debug, Clone, PartialEq)]
pub struct DefVarData {
    pub name: Spur,
    pub modifiers: Option<Vec<Mod>>,
    pub value: Box<AstNode>,
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
    pub init: Option<Vec<DefFuncData>>,
    pub methods: Option<Vec<DefFuncData>>,
    pub pre_init: Option<Vec<AstNode>>,
    pub post_init: Option<Vec<AstNode>>,
    pub fin : Option<Vec<AstNode>>
}

impl DefClassData {
    pub fn empty_def(name : Spur) -> DefClassData{
        DefClassData{
            name,
            params: None,
            fields : None,
            init: None,
            methods: None,
            pre_init: None,
            post_init: None,
            fin: None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefStructInst {
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


#[derive(Debug, Clone, PartialEq)]
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
    ExprFuncCal(ExprFuncCallData),
    ObjectCall(ObjectCallData),
    LiteralCall(Spur),
    ObjectAssignment(ObjectAssignData),
}


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


#[derive(Debug, Clone, PartialEq)]
pub enum LitNode {
    Integer(IntValue),
    Float(FloatValue),
    Boolean(BoolValue),
    String(StringValue),
    Quote(QuoteValue),
    Object(ObjectValue),
    Struct(StructData),
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
            LitNode::Lambda(val) => val,
            LitNode::Object(val) => val,
            LitNode::Vector(val) => val,
            LitNode::Struct(val) => val,
            LitNode::Pair(val) => val,
            LitNode::String(val) => val,
            LitNode::Quote(val) => val,
            LitNode::Nil(val) => val,
        }
    }
}


pub trait EvalResult {
    fn as_int(&self) -> i64;
    fn as_float(&self) -> f64;
    fn as_bool(&self) -> bool;
    //   fn as_vector(&self) -> Vec<LitNode>;
    fn as_string(&self) -> String;
    fn equal_to(&self, other: &LitNode) -> bool;
    fn get_type(&self) -> Type;
}


#[derive(Debug, Clone, PartialEq)]
pub struct IntValue(pub i64);


impl EvalResult for IntValue {
    fn as_int(&self) -> i64 { self.0 }
    fn as_float(&self) -> f64 { self.0 as f64 }
    fn as_bool(&self) -> bool { if self.0 == 0 { false } else { true } }
    fn as_string(&self) -> String { self.0.to_string() }
    //   fn as_vector(&self) -> Vec<LitNode> { vec![LitNode::Integer(IntValue(self.0))] }
    fn equal_to(&self, other: &LitNode) -> bool {
        match other {
            LitNode::Integer(val) => { val.0 == self.0 }
            LitNode::Float(val) => { val.0 == self.0 as f64 }
            _ => false
        }
    }
    fn get_type(&self) -> Type { Type::Integer }
}


#[derive(Debug, Clone, PartialEq)]
pub struct FloatValue(pub f64);


impl EvalResult for FloatValue {
    fn as_int(&self) -> i64 { self.0 as i64 }
    fn as_float(&self) -> f64 { self.0 }
    fn as_bool(&self) -> bool { if self.0 == 0.0 { false } else { true } }
    fn as_string(&self) -> String { self.0.to_string() }
    //  fn as_vector(&self) -> Vec<LitNode> { vec![LitNode::Float(FloatValue(self.0))] }
    fn equal_to(&self, other: &LitNode) -> bool {
        match other {
            LitNode::Integer(val) => { val.0 as f64 == self.0 }
            LitNode::Float(val) => { val.0 == self.0 }
            _ => false
        }
    }
    fn get_type(&self) -> Type { Type::Float }
}


#[derive(Debug, Clone, PartialEq)]
pub struct BoolValue(pub bool);


impl EvalResult for BoolValue {
    fn as_int(&self) -> i64 { if self.0 { 1 } else { 0 } }
    fn as_float(&self) -> f64 { if self.0 { 1.0 } else { 0.0 } }
    fn as_bool(&self) -> bool { self.0 }
    fn as_string(&self) -> String { if self.0 { "#t".to_string() } else { "#f".to_string() } }
    //   fn as_vector(&self) -> Vec<LitNode> { vec![LitNode::Boolean(BoolValue(self.0))] }
    fn equal_to(&self, other: &LitNode) -> bool { other.value().as_bool() == self.0 }
    fn get_type(&self) -> Type { Type::Boolean }
}


#[derive(Debug, Clone, PartialEq)]
pub struct StringValue(pub String);


impl EvalResult for StringValue {
    fn as_int(&self) -> i64 { self.0.len() as i64 }
    fn as_float(&self) -> f64 { self.0.len() as f64 }
    fn as_bool(&self) -> bool { if self.0.is_empty() { false } else { true } }
    fn as_string(&self) -> String { self.0.to_string() }
    //    fn as_vector(&self) -> Vec<LitNode> { vec![LitNode::String(StringValue(self.0.to_string()))] }
    fn equal_to(&self, other: &LitNode) -> bool { other.value().as_string() == self.0 }
    fn get_type(&self) -> Type { Type::String }
}


#[derive(Debug, Clone, PartialEq)]
pub struct QuoteValue(pub Box<AstNode>);


impl EvalResult for QuoteValue {
    fn as_int(&self) -> i64 { 1 }
    fn as_float(&self) -> f64 { 1.0 }
    fn as_bool(&self) -> bool { true }
    fn as_string(&self) -> String { "$Quote".to_string() }
    //   fn as_vector(&self) -> Vec<LitNode> { vec![LitNode::Quote(QuoteValue(self.0.clone()))] }
    fn equal_to(&self, other: &LitNode) -> bool {
        if let LitNode::Quote(val) = other {
            val.0 == self.0
        } else { false }
    }
    fn get_type(&self) -> Type { Type::Quote }
}


#[derive(Debug, Clone, PartialEq)]
pub enum ObjectValue {
    Struct(StructData),
    Class(ClassData),
}


impl ObjectAccess for ObjectValue {
    fn get_field(&self, name: &Spur) -> Result<Rc<LitNode>, String> {
        match self {
            ObjectValue::Struct(s) => s.get_field(name),
            ObjectValue::Class(c) => c.get_field(name),
        }
    }

    fn get_method(&self, name: &Spur) -> Result<LitNode, String> {
        match self {
            ObjectValue::Struct(s) => s.get_method(name),
            ObjectValue::Class(c) => c.get_method(name),
        }
    }

    fn set_field(&mut self, name: &Spur, value: &AstNode) -> Result<Rc<LitNode>, String> {
        match self {
            ObjectValue::Struct(s) => s.set_field(name, value),
            ObjectValue::Class(c) => c.set_field(name, value),
        }
    }
}


impl EvalResult for ObjectValue {
    fn as_int(&self) -> i64 { 1 }
    fn as_float(&self) -> f64 { 1.0 }
    fn as_bool(&self) -> bool { true }
    fn as_string(&self) -> String { "$Object".to_string() }
    //   fn as_vector(&self) -> Vec<LitNode> { vec![LitNode::Object(ObjectValue(()))] }
    fn equal_to(&self, other: &LitNode) -> bool {
        if let LitNode::Object(val) = other {
            true // FIXME
        } else { false }
    }
    // TODO type data should come from the actual held value
    fn get_type(&self) -> Type { Type::Object }
}


#[derive(Debug, Clone, PartialEq)]
pub struct NilValue();


impl EvalResult for NilValue {
    fn as_int(&self) -> i64 { 0 }
    fn as_float(&self) -> f64 { 0.0 }
    fn as_bool(&self) -> bool { false }
    fn as_string(&self) -> String { "()".to_string() }
    //   fn as_vector(&self) -> Vec<LitNode> { vec![NIL_LIT] }
    fn equal_to(&self, other: &LitNode) -> bool { matches!(other, LitNode::Nil(_)) }
    fn get_type(&self) -> Type { Type::Nil }
}


#[derive(Debug, Clone, PartialEq)]
pub struct VectorValue(pub Vec<LitNode>);


impl EvalResult for VectorValue {
    fn as_int(&self) -> i64 { self.0.len() as i64 }
    fn as_float(&self) -> f64 { self.0.len() as f64 }
    fn as_bool(&self) -> bool { false }
    //   fn as_vector(&self) -> Vec<LitNode> { self.0.clone() }
    fn as_string(&self) -> String { format!("{:?}", self).to_string() }
    fn equal_to(&self, other: &LitNode) -> bool {
        if let LitNode::Vector(val) = other {
            val.0 == self.0
        } else { false }
    }
    fn get_type(&self) -> Type { Type::Vector }
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


#[derive(Debug, Clone, PartialEq)]
pub struct PairValue {
    pub car: Rc<LitNode>,
    pub cdr: Rc<LitNode>,
}


impl PairValue {
    pub fn as_string(&self) -> String {
        self.to_string_recur().trim_end_matches(", ").to_string()
    }

    fn to_string_recur(&self) -> String {
        let car_str = self.car.value().as_string();
        match *self.cdr {
            LitNode::Nil(_) => car_str, // Stop and return if cdr is Nil
            LitNode::Pair(ref pair) => format!("{}, {}", car_str, pair.to_string_recur()),
            ref lit_node => format!("{}, {}", car_str, lit_node.value().as_string()),
        }
    }

    pub fn from_ast(car_val: AstNode, cdr_val: AstNode) -> Result<AstNode, String> {
        let car = if let LiteralNode(lit) = car_val {
            Rc::clone(&lit)
        } else { return Err("Invalid, car value not a literal".to_string()); };
        let cdr = if let LiteralNode(lit) = cdr_val {
            Rc::clone(&lit)
        } else { return Err("Invalid, car value not a literal".to_string()); };
        Ok(LiteralNode(Rc::new(LitNode::Pair(PairValue { car, cdr }))))
    }

    pub fn from_ast_as_lit(car_val: AstNode, cdr_val: AstNode) -> Result<LitNode, String> {
        let car = if let LiteralNode(lit) = car_val {
            Rc::clone(&lit)
        } else { return Err("Invalid, car value not a literal".to_string()); };
        let cdr = if let LiteralNode(lit) = cdr_val {
            Rc::clone(&lit)
        } else { return Err("Invalid, car value not a literal".to_string()); };
        Ok(LitNode::Pair(PairValue { car, cdr }))
    }

    pub fn from_lit(car: Rc<LitNode>, cdr: Rc<LitNode>) -> AstNode {
        LiteralNode(Rc::new(LitNode::Pair(PairValue { car: Rc::clone(&car), cdr: Rc::clone(&cdr) })))
    }

    pub fn from_lit_as_lit(car: Rc<LitNode>, cdr: Rc<LitNode>) -> LitNode {
        LitNode::Pair(PairValue { car: Rc::clone(&car), cdr: Rc::clone(&cdr) })
    }
}


impl EvalResult for PairValue {
    fn as_int(&self) -> i64 { 1 }
    fn as_float(&self) -> f64 { 1.0 }
    fn as_bool(&self) -> bool { true }
    fn as_string(&self) -> String { self.as_string() }
    // fn as_vector(&self) -> Vec<LitNode> { vec![LitNode::Pair(self.clone())] }
    fn equal_to(&self, other: &LitNode) -> bool {
        if let LitNode::Pair(val) = other {
            val.car == self.car && val.cdr == self.cdr
        } else { false }
    }
    fn get_type(&self) -> Type { Type::Pair }
}


#[derive(Debug, Clone, PartialEq)]
pub struct LambdaValue {
    pub def: Box<DefLambdaData>,
    pub env: Rc<RefCell<Environment>>,
}


impl EvalResult for LambdaValue {
    fn as_int(&self) -> i64 { 1 }
    fn as_float(&self) -> f64 { 1.0 }
    fn as_bool(&self) -> bool { true }
    // fn as_vector(&self) -> Vec<LitNode> { vec![LitNode::Lambda(self.clone())] }
    fn as_string(&self) -> String { "$Lambda".to_string() }
    fn equal_to(&self, other: &LitNode) -> bool {
        if let LitNode::Lambda(val) = other {
            val.def == self.def && val.env == self.env
        } else { false }
    }
    fn get_type(&self) -> Type { Type::Lambda }
}
