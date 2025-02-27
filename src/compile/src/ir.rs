use lang::ast::{AstData, AstNode, ExprVariant, LetData, SCallData, StmntVariant, Value};
use lang::types::TypeId;
use crate::environment::{ConstantPool, SubEnvironment};


pub enum LoadOp {
    LoadConst,
    LoadIdentifier,
}
// Load locals by offset table stored in constant pool, also include general load instructions
// for common sizes with an offset field behind


/* 16 kb stack size limit | Loacal Load and stores can be pre calculated for offsets into arrays
    LoadLocalN <local_offset> <size>
    StoreLocalN <local_offset> <size>
    LoadMember <const_index> *requires ref on top stack
    LoadMemberS <const_index> *uses self ref at param 0
    LoadStatic <const_index>.// can resolve to direct ptr
    StoreMember <const_index>
    InvokeMember <const_index> *arguments on stack, compiler should self ref
    InvokeStatic <Const index> // can resolve to direct ptr
    InvokeInterface <something>

 */

pub enum IROp {
    Store(Box<IRStore>),
    Load(Box<IRLoad>),
    Invoke(Box<IRInvoke>),
}


pub enum IRStore {
    StoreMember { data_expr: IROp },
    StoreLocal { data_expr: IROp },
}


pub enum IRLoad {
    LoadLocal { offset: u16, size: u16 },
    LoadMember { c_pool_index: u16 },
    LoadStatic { c_pool_index: u16 },
}


pub enum IRInvoke {
    // cpool holds index of class/method, will be resolved to pointer, self ref loads reference to call method on
    Member { cpool_idx: u16, self_ref: IRLoad, args: Vec<IROp> },
    // cpool holds pointer to method, no ref needed for static
    Static { cpool_idx: u16, args: Vec<IROp> },
    // TODO interfaces
    Interface,

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
        if self.param_count < u16::MAX as usize && self.locals.len() >= u16::MAX as usize {
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


// LState represents the scope state for lower, as in if we are inside a namespace,object or nested?

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
            StmntVariant::Let(let_data) => {todo!()}
            StmntVariant::Assign(assign_data) => {todo!()}
        }
    }


    pub fn lower_let_statement(&mut self, let_data: &mut AstData<LetData>) -> Result<IROp, LoweringError> {
        match self.l_state {
            LState::NamespaceFunc | LState::ObjectFunc => {
                if let Some(curr_func) = &mut self.curr_func {
                    let store_idx = curr_func.add_local(let_data.resolve_state.get_type_entry().unwrap().id())?;
                    let assign_expr = self.lower_expression(&mut let_data.node_data.assignment)?;
                    Ok(IROp::Store(Box::new(IRStore::StoreLocal { data_expr: assign_expr })))
                } else { panic!("Fatal<internal>: Expected function in focus") }
            }
            LState::Object | LState::Namespace => panic!(
                "Fatal<internal>: Let statement in wrong position, should be validation error."
            )
        }
    }

    pub fn lower_expression(&mut self, expr: &mut ExprVariant) -> Result<IROp, LoweringError> {
        match expr {
            ExprVariant::SCall(s_call) => self.lower_s_call(s_call),
            ExprVariant::FCall(f_call) => {todo!()}
            ExprVariant::Value(val) => {todo!()}
            ExprVariant::OpCall(op_call) => {todo!()}
            ExprVariant::Block(block_data) => {todo!()}
            ExprVariant::Predicate(pred_data) => {todo!()}
            ExprVariant::Lambda(func_data) => {todo!()}
        }
    }

    pub fn lower_s_call(&mut self, expr: &mut AstData<SCallData>) -> Result<IROp, LoweringError> {
        // should be pointer to constant poll
        let op_expr = self.lower_expression(&mut expr.node_data.operation_expr)?;
        // let operand_exprs: Vec<IROp> = expr.node_data.operand_exprs.and_then(|mut operands| {
        //     Some(operands.iter_mut()
        //         .map(|mut e| Ok(self.lower_expression(&mut e)?))
        //         .collect())
        // }).unwrap_or(vec![]);

        // need to identify when parsing whether is static or dynamic when resolving or from load value op

       // IROp::Invoke()
        todo!()
    }
    
    pub fn lower_value(&mut self, value: &mut AstData<Value>) -> Result<IROp, LoweringError> {
        
        todo!()
    }
}