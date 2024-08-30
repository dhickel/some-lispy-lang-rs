use std::any::Any;
use lang::ast::{Argument, AssignData, AstData, AstNode, ExprVariant, FExprData, LetData, OpCallData, PredicateData, ResolveData, ResolveState, SCallData, StmntVariant, Value};
use lang::{ModifierFlags, ValueType};
use lang::types::{Type, TypeId, TypeTable, UnresolvedType};
use lang::types::UnresolvedType::Unknown;
use lang::util::{IString, SCACHE};
use parser::ParseError;
use crate::environment2::{EnvError, SubEnvironment};
use crate::{ValuePrecedence, Warning};

macro_rules! wrap_scope {
    ($self:expr, $($func:tt)*) => {{
        $self.env.push_scope();
        let result = $($func)*;
        $self.env.pop_scope();
        result
    }}
}


pub enum ResolveError {
    EnvError(String),
    InvalidAssignment(String),
    InvalidOperation(String),
    InvalidArgument(String),
}


impl ResolveError {
    pub fn invalid_assignment<T>(line_char: (u32, u32), t1: &Type, t2: &Type) -> Result<T, ResolveError> {
        Err(Self::InvalidAssignment(
            format!(
                "Line: {}, Char: {}, Cannot assign type: {:?}, to symbol of type: {:?}",
                line_char.0, line_char.1, t1, t2)))
    }

    pub fn invalid_argument<T>(line_char: (u32, u32), msg: String) -> Result<T, ResolveError> {
        Err(Self::InvalidAssignment(
            format!("Line: {}, Char: {}, Invalid argument: {:?}", line_char.0, line_char.1, msg))
        )
    }

    pub fn env_error<T>(line_char: (u32, u32), env_error: EnvError) -> Result<T, ResolveError> {
        Err(Self::EnvError(format!("Line: {}, Char: {}, {:?}", line_char.0, line_char.1, env_error)))
    }

    pub fn invalid_operation<T>(line_char: (u32, u32), info: &str, symbol: Option<IString>) -> Result<T, ResolveError> {
        let symbol = if let Some(symbol) = symbol {
            format!(", Symbol: {:?}", SCACHE.resolve(symbol))
        } else { "".to_string() };

        Err(Self::InvalidOperation(
            format!("Line: {}, Char: {}, Invalid operation{:?}: {:?}", line_char.0, line_char.1, symbol, info))
        )
    }
}


pub enum ResolutionResult {}


pub struct Resolver {
    fully_resolved: bool,
    ast_nodes: Vec<AstNode>,
    env: SubEnvironment,
    warnings: Vec<Warning>,
}


impl Resolver {
    pub fn new(mut ast_nodes: Vec<AstNode>, env: SubEnvironment) -> Self {
        Self { fully_resolved: false, ast_nodes, env, warnings: vec![] }
    }

    pub fn resolve(&mut self, attempts: u32) -> ResolutionResult {
        for _ in 0..attempts {
            self.fully_resolved = true; // Set true at start, flag anytime resolution fails to false
            for node in self.ast_nodes {
                let x = self.resolve_top_node(node);
            }
        }
        todo!()
    }


    fn resolve_top_node(&mut self, node: &mut AstNode) -> Result<bool, ResolveError> {
        let result = match node {
            AstNode::Statement(stmnt) => self.resolve_statement(stmnt),
            AstNode::Expression(expr) => self.resolve_expression(expr)
        };
        todo!()
    }


    fn resolve_statement(&mut self, stmnt: &mut StmntVariant) -> Result<bool, ResolveError> {
        match stmnt {
            StmntVariant::Let(let_data) => self.resolve_let_stmnt(let_data),
            StmntVariant::Assign(assign_data) => self.resolve_assign_stmnt(assign_data)
        }
    }


    fn resolve_expression(&mut self, expr: &mut ExprVariant) -> Result<bool, ResolveError> {
        match expr {
            ExprVariant::SCall(s_call_data) => self.resolve_s_expr(s_call_data),
            ExprVariant::FCall(f_call_data) => { todo!() }
            ExprVariant::Value(value_data) => self.resolve_value(value_data),
            ExprVariant::OpCall(op_call_data) => self.resolve_op_expr(op_call_data),
            ExprVariant::Block(block_expr_data) => self.resolve_block_expression(block_expr_data),
            ExprVariant::Predicate(predicate_data) => { todo!() }
            ExprVariant::Lambda(lambda_data) => { todo!() }
        }
    }


    ////////////////
    // Statements //
    ////////////////

    // TODO type table insertions for user types

    fn resolve_let_stmnt(&mut self, data: &mut AstData<LetData>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { return Ok(true); }

        let assign_type = if self.resolve_expression(&mut data.node_data.assignment)? {
            data.resolve_state.get_type()
        } else { return Ok(false); };

        let symbol_state = &data.resolve_state;

        if assign_type.compatible_with(symbol_state.get_type()) {
            let symbol_state = ResolveState::Resolved(
                self.env.get_resolve_data(symbol_state.get_type().clone(), symbol_state.get_type_id().unwrap())
            );

            // Update the resolve state, and then borrow it back to make borrow checker happy.
            data.resolve_state = symbol_state;
            let symbol_state = &data.resolve_state;

            let name = data.node_data.identifier;
            let modifiers = if let Some(modifiers) = &data.node_data.modifiers {
                ModifierFlags::from_mods(modifiers)
            } else { ModifierFlags::NONE };

            if let Err(err) = self.env.add_symbol(name, symbol_state.get_type_id().unwrap(), modifiers) {
                return ResolveError::env_error(data.line_char, err)?;
            } else { Ok(true) }
        } else { ResolveError::invalid_assignment(data.line_char, assign_type, &symbol_state.get_type()) }
    }


    fn resolve_assign_stmnt(&mut self, data: &mut AstData<AssignData>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { return Ok(true); }

        let identifier = data.node_data.identifier;

        if let Some(symbol) = self.env.find_symbol_in_scope(identifier) {
            if symbol.mod_flags.contains(ModifierFlags::MUTABLE) {
                let value_type_id = if self.resolve_expression(&mut data.node_data.value)? {
                    data.resolve_state.get_type_id().unwrap()
                } else { return Ok(false); };

                if self.env.are_types_compatible(value_type_id, symbol.type_id) {
                    Ok(true)
                } else {
                    ResolveError::invalid_assignment(data.line_char, &data.resolve_state.get_type(), data.resolve_state.get_type())
                }
            } else { Ok(false) }
        } else { ResolveError::invalid_operation(data.line_char, "Symbol is immutable", Some(identifier)) }
    }


    /////////////////
    // Expressions //
    /////////////////

    fn resolve_s_expr(&mut self, data: &mut AstData<SCallData>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { return Ok(true); }

        let (op_resolved, op_type, op_type_id)
            = if self.resolve_expression(&mut data.node_data.operation_expr)? {
            (true, data.resolve_state.get_type(), Some(data.resolve_state.get_type_id().unwrap()))
        } else { (false, &Type::Unresolved(Unknown), None) };

        if let Some(exprs) = data.node_data.operand_exprs.as_mut() {
            let operands_resolved = exprs.iter_mut()
                .try_fold(true, |acc, expr| {
                    let result = self.resolve_expression(expr)?;
                    Ok(acc && result)
                })?;

            if op_resolved && operands_resolved {
                let res_data = self.env.get_resolve_data(op_type.clone(), op_type_id.unwrap());
                data.resolve_state = ResolveState::Resolved(res_data);
                Ok(true)
            } else { Ok(false) }
        } else { Ok(true) }
    }


    fn resolve_f_expr(&mut self, data: &mut AstData<FExprData>) -> Result<bool, ResolveError> {
        todo!()
    }


    fn resolve_value(&mut self, data: &mut AstData<Value>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { return Ok(true); } else {
            panic!("Currently every this should be resolved at run time (primitives")
        }

        // TODO: non-primitive types will need resolved here, but we need to object system for that
    }


    fn resolve_op_expr(&mut self, data: &mut AstData<OpCallData>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { return Ok(true); }

        if let Some(exprs) = data.node_data.operands.as_mut() {
            let operands_resolved = exprs.iter_mut()
                .try_fold(true, |acc, expr| {
                    let result = self.resolve_expression(expr)?;
                    Ok(acc && result)
                })?;


            if operands_resolved {
                let expr_values = exprs.iter().enumerate().map(|(i, expr)| {
                    if let ExprVariant::Value(val) = expr {
                        Ok(*val.node_data.clone())
                    } else {
                        ResolveError::invalid_argument(
                            data.line_char,
                            format!("Operation: {:?}, index: {}", data.node_data.operation, i),
                        )
                    }
                }).collect::<Result<Vec<Value>, ResolveError>>()?;

                let (made_change, return_val) = match ValuePrecedence::primitive_operation_return_coercion(&expr_values) {
                    Ok(val) => val,
                    Err(err) => return ResolveError::invalid_argument(
                        data.line_char,
                        format!("Operation: {:?}, {}", data.node_data.operation, err).to_string(),
                    ),
                };

                if made_change { self.warnings.push(Warning::return_coercion(data.line_char, return_val)) }

                let (typ, type_id) = return_val.get_type_info_if_primitive().unwrap();
                let res_data = self.env.get_resolve_data(typ, type_id);
                data.resolve_state = ResolveState::Resolved(res_data);
                Ok(true)
            } else { return Ok(false); }
        } else { Err(ResolveError::InvalidArgument("Operation expression requires arguments".to_string())) }
    }

    fn resolve_block_expression(&mut self, data: &mut AstData<Vec<AstNode>>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { return Ok(true); }

        if data.node_data.is_empty() {
            self.warnings.push(Warning::empty_block(data.line_char));
            data.resolve_state = ResolveState::Resolved(self.env.get_nil_resolve());
            return Ok(true);
        }

        let block_resolved = data.node_data.iter_mut()
            .try_fold(true, |acc, node| {
                let result = self.resolve_top_node(node)?;
                Ok(acc && result)
            })?;

        if block_resolved {
            match data.node_data.last() {
                Some(AstNode::Statement(_)) => data.resolve_state = ResolveState::Resolved(self.env.get_nil_resolve()),
                Some(AstNode::Expression(expr)) => {
                    let last_expr = expr.get_resolve_state();
                    let res_data = self.env.get_resolve_data(
                        last_expr.get_type().clone(), last_expr.get_type_id().unwrap(),
                    );
                    data.resolve_state = ResolveState::Resolved(res_data);
                }
                None => panic!("Fatal<internal>: Branch should not be reach due to pre-validation")
            }
            Ok(true)
        } else { Ok(false) }
    }

    fn resolve_predicate_expression(&mut self, data: &mut AstData<PredicateData>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { return Ok(true); }

        let pred_resolve = self.resolve_expression(&mut data.node_data.pred_expr)?;

        if pred_resolve && !matches!(data.node_data.pred_expr.get_resolve_state().get_type(), Type::Boolean) {
            return ResolveError::invalid_operation(
                data.line_char, "Non-boolean expression as predicate condition", None,
            );
        }

        let then_resolve = if let Some(then) = &mut data.node_data.then_expr {
            Some(self.resolve_expression(then)?).clone()
        } else { None };


        let else_resolve = if let Some(els) = &mut data.node_data.else_expr {
            Some(self.resolve_expression(els)?)
        } else { None };

        if let (Some(then_resolve), Some(else_resolve)) = (then_resolve, else_resolve) {
            if !(then_resolve && else_resolve) { return Ok(false); }

            if let (Some(then_state), Some(else_state))
                = (&data.node_data.then_expr, &data.node_data.else_expr)
            {
                let (then_type, then_id) = then_state.get_resolve_state().get_type_and_id();
                let else_type = else_state.get_resolve_state().get_type();

                if else_type.compatible_with(then_type) {
                    let res_data = self.env.get_resolve_data(then_type.clone(), then_id.unwrap());
                    data.resolve_state = ResolveState::Resolved(res_data);
                    Ok(true)
                } else {
                    data.resolve_state = ResolveState::Resolved(self.env.get_nil_resolve());
                    Ok(true)
                }
            } else { Ok(false) }
        } else if let Some(then_resolve) = then_resolve {
            if !then_resolve { return Ok(false); }

            if let Some(then_state) = &data.node_data.then_expr {
                let (typ, typ_id) = then_state.get_resolve_state().get_type_and_id();
                let res_data = self.env.get_resolve_data(typ.clone(), typ_id.unwrap());
                data.resolve_state = ResolveState::Resolved(res_data);
                Ok(true)
            } else { panic!("Fatal<internal>: Failed to match expected branch, shouldn't happen") }
        } else if let Some(else_resolve) = else_resolve {
            if !else_resolve { return Ok(false); }

            if let Some(else_state) = &data.node_data.else_expr {
                let (typ, typ_id) = else_state.get_resolve_state().get_type_and_id();
                let res_data = self.env.get_resolve_data(typ.clone(), typ_id.unwrap());
                data.resolve_state = ResolveState::Resolved(res_data);
                Ok(true)
            } else { panic!("Fatal<internal>: Failed to match expected branch, shouldn't happen") }
        } else { ResolveError::invalid_operation(data.line_char, "Predicate with no branches", None) }
    }
}

