
use crate::parse::ast_nodes::LitNode;

#[derive(Debug)]
pub struct Pair {
    car: LitNode,
    cdr: LitNode,
}