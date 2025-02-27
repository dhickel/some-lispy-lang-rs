use lang::ast::{AssignData, AstData, AstNode, ExprVariant, FExprData, LambdaData, LetData, MType, OpCallData, Parameter, PredicateData, ResolveData, ResolveState, SCallData, StmntVariant, TypeConversion, Value};
use lang::{ModifierFlags, ValueType};
use lang::types::{CompositeType, LangType, TypeTable};
use lang::util::{IString, SCACHE};
use crate::environment::{EnvError, SubEnvironment};
use crate::{Warning};

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
    pub fn invalid_assignment<T>(line_char: (u32, u32), t1: &LangType, t2: &LangType) -> Result<T, ResolveError> {
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
            self.env.reset_scope_for_next_iter();
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
        if expr.get_resolve_state().is_resolved() { return Ok(true); }
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

        // We want to  abort if the assign type is no resolved, as we need it to validate the let resolution
        // clippy warning is not identical blocks as the resolve_expression mutates and changes the data if true
        let assign_type = if data.node_data.assignment.get_resolve_state().is_resolved()
            || self.resolve_expression(&mut data.node_data.assignment)? {
            data.node_data.assignment.get_resolve_state().get_type_entry().unwrap().lang_type()
        } else { return Ok(false); };

        println!("Data Resolve State: {:?}", data.resolve_state);
        let symbol_state = if data.resolve_state.is_resolved() {
            data.resolve_state.clone()
        } else if let Some(resolved_data) = self.env.get_resolve_data_by_type(
            data.resolve_state.get_type_entry().unwrap().lang_type()
        ) {
            let r_state = ResolveState::Resolved(resolved_data);
            println!("Resolved data: {:?}", r_state);
            data.resolve_state = r_state.clone();
            r_state
        } else {
            // Can return now as if the symbol is let to resolvable the assignment type is not as well
            println!("Returning false");
            return Ok(false);
        };


        println!("Curr assign_type: {:?}", assign_type);
        println!("Assign Node: {:?}", data.node_data.assignment);
        println!("Symbol State: {:?}", symbol_state);

        let assign_type_id = self.env.get_type_entry_by_type(assign_type).unwrap().id();
        let symbol_type_id = symbol_state.get_type_entry().unwrap().id();

        let (type_ids_compatible, conversion_needed)
            = self.env.are_type_ids_compatible(symbol_type_id, assign_type_id);

        if !type_ids_compatible {
            return ResolveError::invalid_assignment(
                data.line_char, assign_type, symbol_state.get_type_entry().unwrap().lang_type(),
            );
        }

        let symbol_state = ResolveState::Resolved(
            self.env.get_resolve_data_by_type_id(symbol_state.get_type_entry().unwrap().id())
        );
        println!("Here3");


        // Update the resolve state, and then borrow it back to make borrow checker happy.
        data.resolve_state = symbol_state.clone();
        if let ResolveState::Resolved(resolved_data) = &mut data.resolve_state {
            resolved_data.type_conversion = conversion_needed;
        } else { panic!("Fatal<Internal>: Path should never be taken") }


        let name = data.node_data.identifier;
        let modifiers = if let Some(modifiers) = &data.node_data.modifiers {
            ModifierFlags::from_mods(modifiers)
        } else { ModifierFlags::NONE };

        // Handle different types of assignments (functions need special handling)
        if let Err(err) = self.env.add_symbol(name, symbol_type_id, modifiers) {
            ResolveError::env_error(data.line_char, err)?
        } else { Ok(true) }
    }


    fn resolve_assign_stmnt(&mut self, data: &mut AstData<AssignData>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { return Ok(true); }

        let identifier = data.node_data.identifier;

        if let Some(symbol) = self.env.find_symbol_in_scope(identifier) {
            if !symbol.mod_flags.contains(ModifierFlags::MUTABLE) {
                return ResolveError::invalid_operation(
                    data.line_char, "Assignment to immutable symbol", Some(identifier.name()),
                );
            }

            let value_type_id = if self.resolve_expression(&mut data.node_data.expr)? {
                data.resolve_state.get_type_entry().unwrap().id()
            } else { return Ok(false); };

            let (types_compatible, conversion_needed)
                = self.env.are_type_ids_compatible(value_type_id, symbol.type_entry.id());

            if !types_compatible {
                return ResolveError::invalid_assignment(
                    data.line_char,
                    data.resolve_state.get_type_entry().unwrap().lang_type(),
                    symbol.type_entry.lang_type(),
                );
            }

            match self.env.are_type_ids_compatible(value_type_id, symbol.type_entry.id()) {
                (is_compatible, conversion_needed) if is_compatible => {
                    if conversion_needed != TypeConversion::None {
                        if let ResolveState::Resolved(res_data) = &mut data.resolve_state {
                            res_data.type_conversion = conversion_needed;
                        } else { panic!("Fatal<Internal>: Path shouldn't be take, value should be pre-resolved") }
                    }
                    Ok(true)
                }
                _ => ResolveError::invalid_operation(data.line_char, "Assigned type incompatible", Some(identifier.name()))
            }
        } else {
            ResolveError::invalid_operation(data.line_char, "Failed to find symbol to assign to", Some(identifier.name()))
        }
    }


    /////////////////
    // Expressions //
    /////////////////

    // Resolves function call S-Expressions Operations (+-*/...) use resolve_op_expr
    fn resolve_s_expr(&mut self, data: &mut AstData<SCallData>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { return Ok(true); }
        let operation = &mut data.node_data.operation_expr;
        let operands = &mut data.node_data.operand_exprs;


        // Check and resolve operation expression if needed
        let operation_resolved = if !operation.get_resolve_state().is_resolved() {
            self.resolve_expression(operation)?
        } else { true };

        // Iter and attempt to resolve any unresolved operands, then check if all are resolved
        let operands_resolved = if let Some(ops) = operands {
            let mut is_resolved = true;
            for o in ops.iter_mut() {
                if !o.get_resolve_state().is_resolved() {
                    match self.resolve_expression(o) {
                        Ok(resolved) => if !resolved { is_resolved = false },
                        Err(e) => return Err(e),
                    }
                }
            };
            is_resolved
        } else { true };


        // return early if resolution of inner expression unsuccessful
        if !(operation_resolved && operands_resolved) { return Ok(false); }


        // Built in operations will take the path to resolve_op_expr, which calculates type from operands
        // Here it is needed to verify that the signature of the arguments match and assign the operations
        // return type to the nodes resolve data
        let op_type = data.node_data.operation_expr.get_resolve_state().get_type_entry()
            .expect("Fatal<internal>: Operation expected to be resolved, path should no occur");


        match op_type.lang_type() {
            LangType::Composite(CompositeType::Function(func_type)) => {
                if let Some(ops) = operands {
                    if ops.len() != func_type.param_types.len() {
                        return ResolveError::invalid_argument(
                            data.line_char,
                            format!("Arguments({:?}) count mismatch parameters({:?})",
                                    ops.len(), func_type.param_types.len()).as_str(),
                        );
                    }

                    for (arg, param) in ops.iter_mut().zip(func_type.param_types.iter()) {
                        let param_id = self.env.get_type_entry_by_type(param)
                            .expect("Fatal<internal>: Attempted to look up unresolved param type, this path shouldn't occur")
                            .id();

                        let arg_id = arg.get_resolve_state().get_type_entry()
                            .expect("Fatal<internal>: Attempted to look up unresolved arg type, this path shouldn't occur")
                            .id();

                        let (are_compat, cast_needed) = self.env.are_type_ids_compatible(arg_id, param_id);

                        // Exit and return error if an incompatible arg type is found.
                        if !are_compat {
                            return ResolveError::invalid_argument(
                                data.line_char,
                                format!("Argument Type: {:?}, incompatible with parameter type: {:?}",
                                        arg.get_resolve_state().get_type_entry().unwrap().lang_type(), param
                                ).as_str());
                        }

                        // Add type conversion to args ResolveState if needed for code gen
                        if cast_needed != TypeConversion::None { arg.add_type_conversion(cast_needed); }
                    }
                }
                let res_data = ResolveData::new(self.env.get_scope_context(), op_type.clone());
                data.resolve_state = ResolveState::Resolved(res_data);
                Ok(true)
            } // As of now the only correct operations are built in and functions, so error on rest 
            // (built in ops have their own node to simplify things)
            _ => {
                ResolveError::invalid_operation(data.line_char, format!(
                    "S-Expression operation position must resolve to a function/lambda, found: {:?}", op_type
                ).as_str(), None)
            }
        }
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


    // FIXME: need a more detailed representation for numerical types in type system, currently
    //  simplified to focus on bootstrapping the language, so all numerics are 8 bytes until stack rep is figured out.
    fn resolve_value(&mut self, data: &mut AstData<Value>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { Ok(true) } else {
            match *data.node_data {
                Value::I32(_) | Value::I64(_) | Value::U8(_)
                | Value::U16(_) | Value::U32(_) | Value::U64(_) => {
                    data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type(&LangType::I64).unwrap());
                    Ok(true)
                }
                Value::F32(_) | Value::F64(_) => {
                    data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type(&LangType::F64).unwrap());
                    Ok(true)
                }
                Value::Boolean(_) => {
                    data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type(&LangType::BOOL).unwrap());
                    Ok(true)
                }
                Value::Quote(_) => {
                    data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type(&LangType::QUOTE).unwrap());
                    Ok(true)
                }
                Value::Object => todo!("Objects type resolution WIP"),
                Value::Nil(_) => {
                    data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type(&LangType::NIL).unwrap());
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

        println!("Resolving Operand Expression");
        let operands = if let Some(oprs) = data.node_data.operands.as_mut() {
            oprs
        } else {
            return ResolveError::invalid_operation(data.line_char, "Operation expects one or more operands", None)
        };


        // First operands must be resolved
        let mut ops_resolved = true;
        for o in operands.iter_mut() {
            if o.get_resolve_state().is_resolved() { continue; }

            let resolved = self.resolve_expression(o)?;
            if !resolved { ops_resolved = false; }


            if let ResolveState::Resolved(res_data) = o.get_resolve_state() {
                if {
                    let t = res_data.type_entry.lang_type();
                    !t.is_primitive() || t.is_nil()
                } {
                    return ResolveError::invalid_operation(data.line_char, "Non-primitive value(s) in operation arguments", None);
                }
            } else { panic!("Fatal<Internal>: Path should not be reached") }
        }
        // Exit early if not, we need them all resolved to continue
        if !ops_resolved { return Ok(false); }

        // Calculate any type conversions needed to support the widest type
        let operand_types: Vec<LangType> = operands.iter()
            .map(|o| o.get_resolve_state().get_type_entry().expect("Prechecked").lang_type().clone())
            .collect();

        let return_value = if let TypeConversion::Primitive(conv_type)
            = LangType::get_widest_prim_type_needed(&operand_types)
        {
            for (opr_node, opr_type) in operands.iter_mut().zip(operand_types) {
                if let LangType::Primitive(opr_type) = opr_type {
                    if opr_type != conv_type {
                        if let ResolveState::Resolved(res_data) = opr_node.get_resolve_state_mut() {
                            res_data.type_conversion = TypeConversion::Primitive(conv_type);
                        } else { panic!("Fatal<Internal>: Shouldn't Occur, Resolution should be pre-checked") }
                    }
                } else { panic!("Fatal<Internal>: Should always be a primitive type due to logic/guards") }
            }
            conv_type
        } else { panic!("Fatal<Internal>: Return value should always be a primitive, and pre-guarded") };


        let res_data = self.env.get_resolve_data_by_type(&LangType::Primitive(return_value))
            .expect("Resolution data should always be returned");

        data.resolve_state = ResolveState::Resolved(res_data);
        Ok(true)
    }


    fn resolve_block_expression(&mut self, data: &mut AstData<Vec<AstNode>>) -> Result<bool, ResolveError> {
        println!("Resolving Block Expression");
        if data.resolve_state.is_resolved() { return Ok(true); }


        if data.node_data.is_empty() {
            self.warnings.push(Warning::empty_block(data.line_char));
            data.resolve_state = ResolveState::Resolved(self.env.get_nil_resolve());
            return Ok(true);
        }

        let block_resolved = data.node_data.iter_mut().try_fold(true, |acc, node| {
            let result = self.resolve_top_node(node)?;
            println!("BLock Node: {:?}, Resolved: {:?}", node, result);
            Ok(acc && result)
        })?;

        if block_resolved {
            println!("\n\nResolved Block Expression");
            match data.node_data.last() {
                Some(AstNode::Statement(_)) => data.resolve_state = ResolveState::Resolved(self.env.get_nil_resolve()),
                Some(AstNode::Expression(expr)) => {
                    let last_expr = expr.get_resolve_state();
                    let res_data = self.env.get_resolve_data_by_type_id(
                        last_expr.get_type_entry().expect("Fatal<Internal>: Path should pre-validate resolved").id()
                    );
                    data.resolve_state = ResolveState::Resolved(res_data);
                }
                None => panic!("Fatal<internal>: Branch should not be reach due to pre-validation")
            }
            Ok(true)
        } else {
            println!("\n\nDidn't Resolve Block Expression");
            Ok(false)
        }
    }

    // FIXME messy AF all these lower ones need cleaned up as they didnt need much of a logic re-write and exist as mehyuck
    fn resolve_predicate_expression(&mut self, data: &mut AstData<PredicateData>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { return Ok(true); }

        let pred_resolve = self.resolve_expression(&mut data.node_data.pred_expr)?;
        if pred_resolve && !matches!(
            data.node_data.pred_expr.get_resolve_state().get_type_entry().unwrap().lang_type(),
            &LangType::BOOL
        ) {
            return ResolveError::invalid_operation(data.line_char, "Non-boolean expression as predicate condition", None);
        }

        let then_resolved = self.resolve_expression(&mut data.node_data.then_expr)?;
        if !then_resolved { return Ok(false); }

        let else_resolve = if let Some(els) = &mut data.node_data.else_expr {
            Some(self.resolve_expression(els)?)
        } else { None };

        if let Some(else_resolve) = else_resolve {
            if !else_resolve { return Ok(false); }
        }

        // Now its known then branch is resolved along with the else branch if it exists
        let then_type = data.node_data.then_expr.get_resolve_state().get_type_entry().unwrap();
        let else_type = if let Some(els) = &data.node_data.else_expr {
            Some(els.get_resolve_state().get_type_entry().unwrap().clone())
        } else { None };

        if else_type.is_none() {
            let res_data = self.env.get_resolve_data_by_type_id(then_type.id());
            data.resolve_state = ResolveState::Resolved(res_data);
            return Ok(true);
        }

        // if there is an else brancn then comparability need to be checked;
        let else_type = else_type.unwrap();

        // TODO/FIXME this will need better calculation logic, this is kind of important
        // Check that else branch returns a type compatible with the then branch
        let (branches_compat, conv_need) = self.env.are_type_ids_compatible(else_type.id(), then_type.id());

        let else_expr = if let Some(expr) = data.node_data.else_expr.as_mut() {
            expr
        } else { panic!("Fatal<Internal>: This path should not occur, else_expr should always exist") };

        // If a conversion is need on the else branch, and branches are compatible, 
        // apply it to the else and return as resolved
        if branches_compat && conv_need != TypeConversion::None {
            else_expr.add_type_conversion(conv_need);
            data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type_id(then_type.id()));
            Ok(true)
        } else {
            // If branches are not compatible than the predicate expression is forced to return nil
            // this will only be an issue, if it is placed in an assignment or place expecting a result
            // which will result in an error as it should
            data.resolve_state = ResolveState::Resolved(self.env.get_nil_resolve());
            Ok(true)
        }
    }


    fn resolve_lambda_expression(&mut self, data: &mut AstData<LambdaData>) -> Result<bool, ResolveError> {
        println!("Resolving lambda expression");
        if data.resolve_state.is_resolved() { return Ok(true); }


        let body_resolved = self.resolve_expression(&mut data.node_data.body_expr)?;

        // Return early cant do anymore work until body fully resolved
        if !body_resolved { return Ok(false); }

        println!("Lambda BOdy Expression: {:?}", &data.node_data.body_expr);
        println!("Lambda BOdy Resolved: {:?}", body_resolved);

        // TODO we need to make sure to parse alternative type declarations, as types can be on symbol or
        //  in the lambda signature or both.

        let params = &data.node_data.parameters;

        let mut resolved_params: Vec<ResolveData> = Vec::with_capacity(params.len());

        let params_resolved = params.iter().all(|p| {
            let param_type = &p.clone().typ.expect("Param must be type in actual sig atm");
            // If resolved is some than param type is resolved
            if let Some(res_data) = self.env.get_resolve_data_by_type(param_type) {
                resolved_params.push(res_data.clone());
                true
            } else { false }
        });

        // Return early cant do anymore work until they are fully resolved
        if !params_resolved { return Ok(false); }

        for (param, res_data) in params.iter().zip(resolved_params.iter()) {
            println!("Adding Param Symbol for: {:?}", param);
            if let Err(err) = self.env.add_symbol(
                param.identifier,
                res_data.type_entry.id(), // FIXME need to keep from cloning this
                ModifierFlags::from_mods(&param.modifiers.clone().unwrap_or(vec![])),
            ) {
                return ResolveError::invalid_parameter(
                    data.line_char, format!("Error in lambda parameters: {:?}", err).as_str(),
                );
            };
        }

        // Set resolution data for the node and return resolved
        let return_type = &data.node_data.body_expr.get_resolve_state().get_type_entry()
            .expect("Fatal<Internal>: Body should already be pre-validated as resolved");

        let res_data = self.env.get_resolve_data_by_type_id(return_type.id());
        data.resolve_state = ResolveState::Resolved(res_data);
        Ok(true)
    }
}
