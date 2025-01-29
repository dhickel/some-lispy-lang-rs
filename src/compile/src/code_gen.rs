use lang::ast::{AstData, AstNode, ExprVariant, LetData, StmntVariant};
use lang::op_codes::OpCode;
use crate::environment::SubEnvironment;


pub struct CodeGenError {}


pub struct CompUnit {
    pub code: Vec<u8>,
   // pub const_pool:
}


impl CompUnit {
    pub fn push_op_code(&mut self, op: OpCode) {
        self.code.push(op as u8)
    }

    pub fn push_gen_data(&mut self, mut data: GenData) {
        self.code.append(&mut data.code)
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct GenData {
    pub code: Vec<u8>,
    typ: u16,
}


impl GenData {
    fn append_op_code(&mut self, op: OpCode) {
        self.code.push(op as u8)
    }

    fn append_operand(&mut self, operand: u8) {
        self.code.push(operand)
    }

    fn append_gen_data(&mut self, mut other: GenData) {
        self.code.append(&mut other.code);
    }

    pub fn append_wide_operand(&mut self, val: u16) {
        self.code.push((val & 0xFF) as u8);
        self.code.push(((val >> 8) & 0xFF) as u8);
    }

    pub fn patch_wide_operand(&mut self, offset: usize, val: u16) {
        self.code[offset] = (val & 0xFF) as u8;
        self.code[offset + 1] = ((val >> 8) & 0xFF) as u8;
    }

    fn new(typ: u16) -> GenData {
        GenData {
            code: Vec::<u8>::with_capacity(0),
            typ,
        }
    }
}


pub fn gen_byte_code_from_nodes() {}


pub fn gen_node(node: &AstNode, comp_unit: &mut CompUnit) -> Result<GenData, GenData> {
    match node {
        AstNode::Statement(stmnt) => match stmnt {
            StmntVariant::Let(let_data) => {}
            StmntVariant::Assign(_) => {}
        }
        AstNode::Expression(expr) => match expr {
            ExprVariant::SCall(_) => {}
            ExprVariant::FCall(_) => {}
            ExprVariant::Value(_) => {}
            ExprVariant::OpCall(_) => {}
            ExprVariant::Block(_) => {}
            ExprVariant::Predicate(_) => {}
            ExprVariant::Lambda(_) => {}
        }
    }
    todo!()
}

// 
// pub fn gen_let_statement(
//     data: &AstData<LetData>,
//     comp_unit: &mut CompUnit
// ) -> Result<GenData, String> {
//     
//     
// }

