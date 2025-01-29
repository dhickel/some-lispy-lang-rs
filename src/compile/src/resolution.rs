use std::any::Any;
use std::io::stdout;
use lang::ast::{Argument, AssignData, AstData, AstNode, ExprVariant, FExprData, FuncMeta, FuncParam, LambdaData, LetData, MetaData, MType, OpCallData, PredicateData, ResolveData, ResolveState, SCallData, StmntVariant, Value};
use lang::ModifierFlags;
use lang::types::{Type, TypeId, TypeTable, UnresolvedType};
use lang::types::UnresolvedType::Unknown;
use lang::util::{IString, SCACHE};
use crate::environment::{EnvError, SubEnvironment};
use crate::{ValuePrecedence, Warning};

macro_rules! with_scope {
    ($self:expr, $($func:tt)*) => {{
        $self.env.push_scope();
        let result = $($func)*;
        $self.env.pop_scope();
        result
    }}
}


#[derive(Debug)]
pub enum ResolveError {
    EnvError(String),
    InvalidAssignment(String),
    InvalidParameter(String),
    InvalidOperation(String),
    InvalidArgument(String),
}

// TODO dont allow statements inside any expression except a block expression

impl ResolveError {
    pub fn invalid_assignment<T>(line_char: (u32, u32), t1: &Type, t2: &Type) -> Result<T, ResolveError> {
        Err(Self::InvalidAssignment(
            format!(
                "Line: {}, Char: {}, Cannot assign type: {:?}, to symbol of type: {:?}",
                line_char.0, line_char.1, t1, t2)))
    }

    pub fn invalid_argument<T>(line_char: (u32, u32), msg: &str) -> Result<T, ResolveError> {
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

    pub fn invalid_parameter<T>(line_char: (u32, u32), msg: &str) -> Result<T, ResolveError> {
        Err(Self::InvalidParameter(format!("Line: {}, Char: {}, Invalid parameter: {:?}", line_char.0, line_char.1, msg)))
    }
}


pub enum ResolutionResult {}


#[derive(Debug)]
pub struct Resolver<'a> {
    fully_resolved: bool,
    ast_nodes: Vec<AstNode>,
    env: &'a mut SubEnvironment,
    warnings: Vec<Warning>,
}


impl<'a> Resolver<'a> {
    pub fn new(ast_nodes: Vec<AstNode>, env: &'a mut SubEnvironment) -> Self {
        Self { fully_resolved: false, ast_nodes, env, warnings: vec![] }
    }

    pub fn resolve(&mut self, attempts: u32) -> Result<bool, ResolveError> {
        for _ in 0..attempts {
            let mut cloned_ast = self.ast_nodes.clone(); // FIXME, working around BC
            let resolved = cloned_ast.iter_mut().try_fold(true, |acc, node| {
                let result = self.resolve_top_node(node)?;
                println!("Resolved Top Node: {:?}", result);
                Ok(acc && result)
            })?;
            self.ast_nodes = cloned_ast; // FIXME, working around BC

            if resolved {
                self.fully_resolved = true;
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub fn is_resolved(&self) -> bool { self.fully_resolved }


    fn resolve_top_node(&mut self, node: &mut AstNode) -> Result<bool, ResolveError> {
        match node {
            AstNode::Statement(stmnt) => self.resolve_statement(stmnt),
            AstNode::Expression(expr) => self.resolve_expression(expr)
        }
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
            ExprVariant::FCall(f_call_data) => self.resolve_f_expr(f_call_data),
            ExprVariant::Value(value_data) => self.resolve_value(value_data),
            ExprVariant::OpCall(op_call_data) => self.resolve_op_expr(op_call_data),
            ExprVariant::Block(block_expr_data) => with_scope!(self, self.resolve_block_expression(block_expr_data)),
            ExprVariant::Predicate(predicate_data) => self.resolve_predicate_expression(predicate_data),
            ExprVariant::Lambda(lambda_data) => with_scope!(self, self.resolve_lambda_expression(lambda_data)),
        }
    }


    ////////////////
    // Statements //
    ////////////////

    // TODO type table insertions for user types

    fn resolve_let_stmnt(&mut self, data: &mut AstData<LetData>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { return Ok(true); }

        println!("Here");
        let assign_type = if self.resolve_expression(&mut data.node_data.assignment)? {
            data.resolve_state.get_type()
        } else {
            println!("Returning false");
            return Ok(false);
        };

   

        let symbol_state = &data.resolve_state;

        println!("Curr assign_type: {:?}", assign_type);
        if assign_type.compatible_with(symbol_state.get_type()) {
            let symbol_state = ResolveState::Resolved(
                self.env.get_resolve_data_by_type_id(symbol_state.get_type_id().unwrap())
            );
            println!("Here3");

            // Update the resolve state, and then borrow it back to make borrow checker happy.
            data.resolve_state = symbol_state;
            let symbol_state = &data.resolve_state;

            let name = data.node_data.identifier;
            let modifiers = if let Some(modifiers) = &data.node_data.modifiers {
                ModifierFlags::from_mods(modifiers)
            } else { ModifierFlags::NONE };

            // Handle different types of assignments (functions need special handling)
            let meta_data = if let ResolveState::Resolved(res)
                = &data.node_data.assignment.get_resolve_state() {
                res.meta_data.clone()
            } else { panic!("Fatal<internal>: Assignment expected to be resolved") };

            if let Err(err) = self.env.add_symbol(name, symbol_state.get_type_id().unwrap(), modifiers, meta_data) {
                ResolveError::env_error(data.line_char, err)?
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
            let operands_resolved = exprs.iter_mut().try_fold(true, |acc, expr| {
                let result = self.resolve_expression(expr)?;
                Ok(acc && result)
            })?;

            if op_resolved && operands_resolved {
                let res_data = self.env.get_resolve_data_by_type_id(op_type_id.unwrap());
                data.resolve_state = ResolveState::Resolved(res_data);
                Ok(true)
            } else { Ok(false) }
        } else { Ok(true) }
    }


    fn resolve_f_expr(&mut self, data: &mut AstData<Vec<FExprData>>) -> Result<bool, ResolveError> {
        // let prior ;
        let first = true;
        for expr_data in data.node_data.iter_mut() {
            match expr_data {
                FExprData::FCall { method, arguments } => {
                    if let Some(method) = method {
                        // this is a local call
                        // TODO local calls should be callable as both ::<func> and <func>, currently only ::<func> works
                        if first {
                            self.env.find_symbol_in_scope(*method).is_some();
                        }
                        if let Some(args) = arguments {}
                    } else {
                        // This handles when an object is called with ::[] on itself
                        todo!()
                    }
                }
                FExprData::FAccess { identifier, m_type } => {
                    match m_type {
                        MType::Namespace => {}
                        MType::MethodCall => {}
                        MType::Field => {}
                        MType::Identifier => {}
                    }
                    todo!()
                }
            }
        }
        todo!()
    }


    // FIXME, need a more detailed representation for numerical types in type system, currently
    //  simplfied to focus on bootstrapping the language.
    fn resolve_value(&mut self, data: &mut AstData<Value>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { Ok(true) } else {
            match *data.node_data {
                Value::I32(_) | Value::I64(_) | Value::U8(_)
                | Value::U16(_) | Value::U32(_) | Value::U64(_) => {
                    data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type(&Type::Integer));
                    Ok(true)
                }
                Value::F32(_) | Value::F64(_) => {
                    data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type(&Type::Float));
                    Ok(true)
                }
                Value::Boolean(_) => {
                    data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type(&Type::Boolean));
                    Ok(true)
                }
                Value::Quote(_) => {
                    data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type(&Type::Quote));
                    Ok(true)
                }
                Value::Object => todo!("Objects type resolution WIP"),
                Value::Nil(_) => {
                    data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type(&Type::Nil));
                    Ok(true)
                }
                Value::Array => todo!("Array type resolution WIP"),
                Value::String => todo!("String type resolution WIP"),
                Value::Tuple => todo!("Tuple Type Resolution WIP"),
                Value::Identifier(ident) => {
                    if let Some(symbol_context) = self.env.find_symbol_in_scope(ident) {
                        data.resolve_state = ResolveState::Resolved(symbol_context.into());
                        Ok(true)
                    } else { Ok(false) }
                }
            }
        }

        // TODO: non-primitive types will need resolved here, but we need to object system for that
    }


    fn resolve_op_expr(&mut self, data: &mut AstData<OpCallData>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { return Ok(true); }

        if let Some(exprs) = data.node_data.operands.as_mut() {
            let operands_resolved = exprs.iter_mut().try_fold(true, |acc, expr| {
                let result = self.resolve_expression(expr)?;
                Ok(acc && result)
            })?;


            if operands_resolved {
                let expr_values = exprs.iter().enumerate().map(|(i, expr)| {
                    if let ExprVariant::Value(val) = expr {
                        Ok(*val.node_data.clone())
                    } else {
                        ResolveError::invalid_argument(
                            data.line_char, &format!("Operation: {:?}, index: {}", data.node_data.operation, i),
                        )
                    }
                }).collect::<Result<Vec<Value>, ResolveError>>()?;

                let (made_change, return_val) = match ValuePrecedence::primitive_operation_return_coercion(&expr_values) {
                    Ok(val) => val,
                    Err(err) => return ResolveError::invalid_argument(
                        data.line_char, &format!("Operation: {:?}, {}", data.node_data.operation, err).to_string(),
                    ),
                };

                if made_change { self.warnings.push(Warning::return_coercion(data.line_char, return_val)) }

                let (typ, type_id) = return_val.get_type_info_if_primitive().unwrap();
                let res_data = self.env.get_resolve_data_by_type_id(type_id);
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

        let block_resolved = data.node_data.iter_mut().try_fold(true, |acc, node| {
            let result = self.resolve_top_node(node)?;
            Ok(acc && result)
        })?;

        if block_resolved {
            match data.node_data.last() {
                Some(AstNode::Statement(_)) => data.resolve_state = ResolveState::Resolved(self.env.get_nil_resolve()),
                Some(AstNode::Expression(expr)) => {
                    let last_expr = expr.get_resolve_state();
                    let res_data = self.env.get_resolve_data_by_type_id(last_expr.get_type_id().unwrap());
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

            if let (Some(then_state), Some(else_state)) = (&data.node_data.then_expr, &data.node_data.else_expr)
            {
                let (then_type, then_id) = then_state.get_resolve_state().get_type_and_id();
                let else_type = else_state.get_resolve_state().get_type();

                if else_type.compatible_with(then_type) {
                    let res_data = self.env.get_resolve_data_by_type_id(then_id.unwrap());
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
                let res_data = self.env.get_resolve_data_by_type_id(typ_id.unwrap());
                data.resolve_state = ResolveState::Resolved(res_data);
                Ok(true)
            } else { panic!("Fatal<internal>: Failed to match expected branch, shouldn't happen") }
        } else if let Some(else_resolve) = else_resolve {
            if !else_resolve { return Ok(false); }

            if let Some(else_state) = &data.node_data.else_expr {
                let (typ, typ_id) = else_state.get_resolve_state().get_type_and_id();
                let res_data = self.env.get_resolve_data_by_type_id(typ_id.unwrap());
                data.resolve_state = ResolveState::Resolved(res_data);
                Ok(true)
            } else { panic!("Fatal<internal>: Failed to match expected branch, shouldn't happen") }
        } else { ResolveError::invalid_operation(data.line_char, "Predicate with no branches", None) }
    }


    fn resolve_lambda_expression(&mut self, data: &mut AstData<LambdaData>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { return Ok(true); }

        let body_resolved = self.resolve_expression(&mut data.node_data.body_expr)?;

        let params_resolved = if let Some(params) = &mut data.node_data.parameters {
            if params.is_empty() {
                true
            } else {
                let mut resolved_types = Vec::with_capacity(params.len());
                let mut fully_resolved = true;
                // FIXME, currently no type inference is support so just check for types
                for p in params.iter() {
                    if let Some(typ) = &p.typ {
                        if let Some(id) = self.env.get_id_for_type(typ) {
                            resolved_types.push(id)
                        } else {
                            fully_resolved = false
                        }
                    } else {
                        return ResolveError::invalid_parameter(data.line_char, "Parameters must be typed");
                    }
                }

                if fully_resolved {
                    for (param, id) in params.iter().zip(resolved_types.into_iter()) {
                        if let Err(err) = self.env.add_symbol(
                            param.identifier,
                            id, // FIXME need to keep from cloning this
                            ModifierFlags::from_mods(&param.modifiers.clone().unwrap_or(vec![])),
                            None,
                        ) {
                            return ResolveError::invalid_parameter(
                                data.line_char, format!("Error in lambda parameters: {:?}", err).as_str(),
                            );
                        };
                    }
                    true
                } else { false }
            }
        } else { true };


        if params_resolved && body_resolved {
            let params = data.node_data.parameters.as_ref().map(|params| params.iter().map(|p| {
                    let (typ, type_id) = self.env.get_type_and_id_by_name(p.identifier).map_or_else(|| (None, None), |t| (Some(t.0), Some(t.1)));

                    FuncParam {
                        name: p.identifier, // FIXME clone, this all could be cleaned up really
                        modifier_flags: ModifierFlags::from_mods(&p.modifiers.clone().unwrap_or_default()),
                        typ,
                        type_id,
                    }
                }).collect::<Vec<FuncParam>>());

            let body_type_and_id = data.node_data.body_expr.get_resolve_state().get_type_and_id();
            let mut res_data = self.env.get_resolve_data_by_type(body_type_and_id.0);
            res_data.meta_data = Some(MetaData::Function(params));
            data.resolve_state = ResolveState::Resolved(res_data);
            Ok(true)
        } else { Ok(false) }
    }
}
