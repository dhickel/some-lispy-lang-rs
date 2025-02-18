use lang::ast::{AssignData, AstData, AstNode, ExprVariant, FExprData, LambdaData, LetData, MType,
                OpCallData, PredicateData, ResolveState, SCallData, StmntVariant, Value};
use lang::{ModifierFlags, ValueType};
use lang::types::{LangType};
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
            data.node_data.assignment.get_resolve_state().get_type()
        } else { return Ok(false); };

        println!("Data Resolve State: {:?}", data.resolve_state);
        let symbol_state = if data.resolve_state.is_resolved() {
            data.resolve_state.clone()
        } else if let Some(resolved_data) = self.env.get_resolve_data_by_type(data.resolve_state.get_type()) {
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
            } else {
                panic!("Fatal<internal>: Assignment expected to be resolved")
            };

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

                if self.env.are_types_compatible(value_type_id, symbol.type_entry.id()) {
                    Ok(true)
                } else {
                    ResolveError::invalid_assignment(data.line_char, &data.resolve_state.get_type(), data.resolve_state.get_type())
                }
            } else { Ok(false) }
        } else { ResolveError::invalid_operation(data.line_char, "Symbol is immutable", Some(identifier.name())) }
    }


    /////////////////
    // Expressions //
    /////////////////

    
    // Resolves function call S-Expressions Operations (+-*/...) use resolve_op_expr
    fn resolve_s_expr(&mut self, data: &mut AstData<SCallData>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { return Ok(true); }
        let mut operation = &data.node_data.operation_expr;
        let mut operands = &data.node_data.operand_exprs;

        // Check and resolve operation expression if needed
        let operation_resolved = if !operation.get_resolve_state().is_resolved() {
            self.resolve_expression(&mut operation)?
        } else { true };

        // Iter and attempt to resolve any unresolved operands, then check if all are resolved
        let operands_resolved = if let Some(mut operands) = operands {
            operands.iter_mut().for_each(|o| {
                if !o.get_resolve_state().is_resolved() { self.resolve_expression(o)?; }
            });
            operands.iter().all(|o| o.get_resolve_state().is_resolved())
        } else { true };
        
        // Built in operations will take the path to resolve_op_expr, which calculates type from operands
        // Here it is needed to verify that the signature of the arguments match and assign the operations
        // return type to the nodes resolve data
         
        if operation_resolved && operands_resolved {
            let op_type = data.node_data.operation_expr.get_resolve_state().get_type();
            
            let arguments_valid = if let Some(mut operands) = &operands {
                if let Expr
                
                for operand in operands.iter_mut() {
                    let (valid, conversion) = self.env.are_types_compatible()
                }
            } else { true};


            let res_data = self.env.get_resolve_data_by_type_id(op_type_id.unwrap());
            data.resolve_state = ResolveState::Resolved(res_data);
            Ok(true)
        } else { Ok(false) }
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
                    data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type(&LangType::Integer).unwrap());
                    Ok(true)
                }
                Value::F32(_) | Value::F64(_) => {
                    data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type(&LangType::Float).unwrap());
                    Ok(true)
                }
                Value::Boolean(_) => {
                    data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type(&LangType::Boolean).unwrap());
                    Ok(true)
                }
                Value::Quote(_) => {
                    data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type(&LangType::Quote).unwrap());
                    Ok(true)
                }
                Value::Object => todo!("Objects type resolution WIP"),
                Value::Nil(_) => {
                    data.resolve_state = ResolveState::Resolved(self.env.get_resolve_data_by_type(&LangType::Nil).unwrap());
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

        if let Some(exprs) = data.node_data.operands.as_mut() {
            let operands_resolved = exprs.iter_mut().try_fold(true, |acc, expr| {
                let result = self.resolve_expression(expr)?;
                println!("Resolving Operation Expr: {:?}, Resolved: {:?}", expr, result);
                Ok(acc && result)
            })?;

            if !operands_resolved {
                println!("Returning False Operands Unresolved");
                return Ok(false);
            }

            let expr_types = exprs.iter().map(|expr| {
                expr.get_resolve_state().get_type().clone()
            }).collect::<Vec<LangType>>();

            if !expr_types.iter().all(|t| matches!(t.into(), ValueType::Primitive(_))) {
                return ResolveError::invalid_argument(data.line_char, "Non-primitive value(s) in operation arguments");
            }

            let (made_change, rtn_type) = ValuePrecedence::return_value_coercion(&expr_types);
            if made_change { self.warnings.push(Warning::return_coercion(data.line_char, rtn_type)) }

            let res_data = self.env.get_resolve_data_by_type(rtn_type).unwrap();
            data.resolve_state = ResolveState::Resolved(res_data);
            Ok(true)
        } else { ResolveError::invalid_argument(data.line_char, "Operation requires arguments") }
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
                    let res_data = self.env.get_resolve_data_by_type_id(last_expr.get_type_id().unwrap());
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


    fn resolve_predicate_expression(&mut self, data: &mut AstData<PredicateData>) -> Result<bool, ResolveError> {
        if data.resolve_state.is_resolved() { return Ok(true); }

        let pred_resolve = self.resolve_expression(&mut data.node_data.pred_expr)?;

        if pred_resolve && !matches!(data.node_data.pred_expr.get_resolve_state().get_type(), LangType::Boolean) {
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
        println!("Resolving lambda expression");
        if data.resolve_state.is_resolved() { return Ok(true); }


        let body_resolved = self.resolve_expression(&mut data.node_data.body_expr)?;

        println!("Lambda BOdy Expression: {:?}", data.node_data.body_expr);
        println!("Lambda BOdy Resolved: {:?}", body_resolved);


        // TODO we need to make sure to parse alternative type declarations, as types can be on symbol or
        //  in the lambda signature or both.


        let params_resolved = match &data.node_data.parameters {
            None => true,
            Some(params) => {
                if !params.iter().all(|p| p.typ.is_some())
            }
        }


        if let Some(params) = &mut data.node_data.parameters {
            if params.is_empty() {
                true
            } else {
                let mut params_resolved = true;
                // FIXME, currently no type inference is support so just check for types
                for p in params.iter_mut() {
                    if let Some(typ) = &p.typ {
                        if !typ.is_resolved() {
                            params_resolved = false;
                            break;
                        }
                    } else { return ResolveError::invalid_parameter(data.line_char, "Parameters must be typed"); }
                }
                params_resolved
            };
        };
        // TODO RESUME make sure param symbols are properly added and able to be properly looked up,
        //   need to track when if they have been added to the symbol table or not 

        if params_resolved {
            for (param, id) in params.iter().zip(resolved_types.into_iter()) {
                println!("Adding Parama Symbol for: {:?}", param);
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


// TODO: here and else where need to implement atleast some type inference where either the actual symbol can be typed
//  and or the parameters and the return value, this needs implemented ina few other places as well. No full inference 
//  atm but any place where it can be dual notated need handled.

if params_resolved & & body_resolved {
println!("\n\nResolved BOdy: {:?}", data.node_data.body_expr);
let params = data.node_data.parameters.as_ref().map( | params| params.iter().map( | p| {
let (typ, type_id) = self.env.get_type_and_id_by_name(p.typ.unwrap()).map_or_else( | | (None, None), | t | (Some(t.0), Some(t.1)));

FuncParam {
name: p.identifier, // FIXME clone, this all could be cleaned up really
modifier_flags: ModifierFlags::from_mods( & p.modifiers.clone().unwrap_or_default()),
typ,
type_id,
}
}).collect::<Vec<FuncParam> > ());

let body_type_and_id = data.node_data.body_expr.get_resolve_state().get_type_and_id();
let mut res_data = self .env.get_resolve_data_by_type(body_type_and_id.0).unwrap();
res_data.meta_data = Some(MetaData::Function(params));
data.resolve_state = ResolveState::Resolved(res_data);
Ok(true)
} else { Ok(false) }
}
}
