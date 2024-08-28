use std::any::Any;
use lang::ast::{Argument, AssignData, AstData, AstNode, Expression, LetData, SCallData, Statement};
use lang::{ModifierFlags, ValueType};
use lang::types::{Type, UnresolvedType};
use lang::types::Type::Lambda;
use lang::util::{IString, SCACHE};
use parser::ParseError;
use parser::ParseError::Parsing;
use parser::parser::;
use crate::environment2::{EnvError, SubEnvironment};
use crate::environment2::SymbolContext;


pub enum ResolutionError {
    EnvError(String),
    InvalidAssignment(String),
    InvalidOperation(String),
}


impl ResolutionError {
    pub fn invalid_assignment(line_char: (u32, u32), t1: &Type, t2: &Type) -> Err<ResolutionError> {
        Err(Self::InvalidAssignment(
            format!(
                "Line: {}, Char: {}, Cannot assign type: {:?}, to symbol of type: {:?}",
                line_char.0, line_char.1, t1, t2))
        )
    }
    pub fn env_error(line_char: (u32, u32), env_error: EnvError) -> Err<ResolutionError> {
        Err(Self::EnvError(format!("Line: {}, Char: {}, {:?}", line_char.0, line_char.1, env_error)))
    }

    pub fn invalid_operation(line_char: (u32, u32), info: &str, symbol: Option<IString>) -> Err<ResolutionError> {
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
}


impl Resolver {
    pub fn new(mut ast_nodes: Vec<AstNode>, env: SubEnvironment) -> Self {
        Self { fully_resolved: false, ast_nodes, env }
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


    fn resolve_top_node(&mut self, node: &mut AstNode) -> Result<bool, ResolutionResult> {
        let result = match node {
            AstNode::Statement(stmnt) => self.resolve_statement(stmnt),
            AstNode::Expression(expr) => self.resolve_expression(expr)
        };
        todo!()
    }


    fn resolve_statement(&mut self, stmnt: &mut Statement) -> Result<bool, ResolutionError> {
        match stmnt {
            AstNode::Statement(statement) => {
                match statement {
                    Statement::Let(let_data) => self.resolve_let_stmnt(let_data),
                    Statement::Assign(assign_data) => self.resolve_assign_stmnt(assign_data)
                }
            }
        }
    }


    fn resolve_expression(&mut self, expr: &mut Expression) -> Result<&Type, ResolutionError> {
        match expr {
            AstNode::Expression(expression) => {
                match expression {
                    Expression::SCall(s_call_data) => { todo!() }
                    Expression::FCall(f_call_data) => { todo!() }
                    Expression::Value(value_data) => { todo!() }
                    Expression::OpCall(op_call_data) => { todo!() }
                    Expression::Block(block_expr_data) => { todo!() }
                    Expression::Predicate(predicate_data) => { todo!() }
                    Expression::Lambda(lambda_data) => { todo!() }
                }
            }
        }
    }

    ////////////////
    // Statements //
    ////////////////

    fn resolve_let_stmnt(&mut self, data: &mut AstData<LetData>) -> Result<bool, ResolutionError> {
        let symbol_type = &data.expr_type;
        let assign_type = self.resolve_expression(&mut data.node_data.assignment)?;

        if !symbol_type.is_resolved() { panic!("Fatal<internal>: Let statements with out resolved type identifier") }
        if !assign_type.is_resolved() {
            return Ok(false);
        }

        if *symbol_type == *assign_type {
            let name = data.node_data.identifier;
            let modifiers = ModifierFlags::from_mods(data.node_data.modifiers.into());
            self.env.add_symbol(name, symbol_type.clone(), modifiers)
                .map_err(|err| ResolutionError::env_error(data.line_char, err))?;

            Ok(true)
        } else { ResolutionError::invalid_assignment(data.line_char, &assign_type, &symbol_type) }
    }


    fn resolve_assign_stmnt(&mut self, data: &mut AstData<AssignData>) -> Result<bool, ResolutionError> {
        let identifier = data.node_data.identifier;

        if let Some(symbol) = self.env.find_symbol_in_scope(identifier) {
            if symbol.mod_flags.contains(ModifierFlags::MUTABLE) {
                if let assign_type = self.resolve_expression(&mut data.node_data.value)? {
                    if !assign_type.is_resolved() {
                        return Ok(false);
                    }

                    if symbol.typ != *assign_type {
                        ResolutionError::invalid_assignment(data.line_char, &assign_type, &symbol.typ)
                    } else {
                        Ok(true)
                    }
                } else { Ok(false) }
            } else { ResolutionError::invalid_operation(data.line_char, "Symbol is immutable", Some(identifier)) }
        } else { Ok(false) }
    }


    /////////////////
    // Expressions //
    /////////////////

    fn resolve_s_expr(&mut self, data: &mut AstData<SCallData>) -> Result<&Type, ResolutionError> {
        if data.expr_type.is_resolved() { return Ok(&data.expr_type); }

        let operation_type = self.resolve_expression(&mut data.node_data.operation_expr)?;

        if let Some(mut exprs) = data.node_data.operand_exprs {
            let mut unresolved = None;
            for expr in &mut exprs {
                let typ = self.resolve_expression(expr)?;
                if !typ.is_resolved() { unresolved = Some(typ) }
            }

            if let Some(unresolved) = unresolved { return Ok(unresolved); }
        };
        Ok(&operation_type)
    }
}  

