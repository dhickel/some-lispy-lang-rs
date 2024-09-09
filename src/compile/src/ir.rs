use lang::ast::{AstData, AstNode, ExprVariant, LetData, StmntVariant};
use lang::token::Op;
use lang::types::TypeId;
use crate::environment::{ConstantPool, SubEnvironment};


pub enum LoadOp {
    LoadConst,
    LoadIdentifier,
}


pub enum IROp {
    Operation { operation: Op, operands: Vec<IROp> },
    PredicateBranch(Box<IRPredicate>),
    InvokeStatic(u16),
    InvokeVirtual(u16),
    InvokeInterface(u16),
    AccessStatic(u16),
    AccessMember(u16),
    AccessLocal(u16),
    LoadConst(u16),
    StoreStatic(u16),
    StoreMember(u16),
    StoreLocal(u16),
}



pub struct IRPredicate {
    pred: IROp,
    then: Option<IROp>,
    els: Option<IROp>,
}


#[derive(Default)]
pub struct NameSpaceIR {
    obj_defs: Vec<()>, // Fixme
    static_fields: Vec<()>,
    static_funcs: Vec<()>,
    constant_pool: ConstantPool,
}


pub struct ObjectIR {
    fields: Vec<()>,
    functions: Vec<()>,
    constant_pool: ConstantPool,
}


pub struct FunctionIR {
    locals: Vec<TypeId>,
    param_count: usize,
}


impl FunctionIR {
    pub fn add_local(&mut self, typ: TypeId) -> Result<usize, LoweringError> {
        if self.param_count < u16::MAX as usize {
            let index = self.locals.len();
            self.locals.push(typ);
            Ok(index)
        } else { Err(LoweringError::ExceededLimit("Exceeded 256 parameters".to_string())) }
    }
}


pub enum LoweringError {
    ExceededLimit(String),
    Unexpected(String),
}


impl LoweringError {}


#[derive(Default)]
pub enum LState {
    #[default]
    Namespace,
    NamespaceFunc,
    Object,
    ObjectFunc,
}


pub struct IRLower {
    env: SubEnvironment,
    ast_nodes: Vec<AstNode>,
    ns_ir: NameSpaceIR,
    l_state: LState,
    curr_func: Option<FunctionIR>,
}


impl IRLower {
    pub fn lower(&mut self) {
        for node in &mut self.ast_nodes {
            match node {
                AstNode::Statement(stmnt) => {}
                AstNode::Expression(_) => {}
            }
        }
    }


    pub fn lower_statement(&mut self, stmnt: &mut StmntVariant) -> Result<(), LoweringError> {
        match stmnt {
            StmntVariant::Let(let_data) => {}
            StmntVariant::Assign(assign_data) => {}
        }
    }

    pub fn lower_expression(&mut self, expr: &mut ExprVariant) -> Result<(), LoweringError> {}

    pub fn lower_let_statement(&mut self, let_data: &mut AstData<LetData>) -> Result<(), LoweringError> {
        match self.l_state {
            LState::NamespaceFunc | LState::ObjectFunc => {
                if let Some(curr_func) = &mut self.curr_func {
                    let store_idx = curr_func.add_local(let_data.resolve_state.get_type_id().unwrap())?;
                    let assign_expr = self.lower_expression(&mut let_data.node_data.assignment)?;


                    Ok(())
                } else { panic!("Fatal<internal>: Expected function in focus") }
            }
            LState::Object | LState::Namespace => panic!(
                "Fatal<internal>: Let statement in wrong position, should be validation error."
            )
        }
    }
}