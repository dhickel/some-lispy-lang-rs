use std::fmt;
use std::fmt::format;
use crate::parse::ast_nodes::{AstNode, LitNode, NilValue};


// const NIL_PAIR: Pair = Pair {
//     car: LitNode::Nil(NilValue()),
//     cdr: LitNode::Nil(NilValue()),
// };
// 
// 
// #[derive(Debug, Clone, PartialEq)]
// pub struct Pair {
//     pub car: LitNode,
//     pub cdr: LitNode,
// }
// 
// impl Pair {
//     pub fn is_nil(&self) -> bool {
//         matches!(self.cdr, LitNode::Nil(_))
//     }
// 
//     pub fn new_nil() -> Pair { NIL_PAIR }
// }
//     
// 
