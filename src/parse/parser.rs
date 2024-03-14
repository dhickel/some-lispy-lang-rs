use std::io::BufRead;

use crate::parse::{Def, Expr, Lex, Lit, Mod, Op, Syn, Token, TokenData, TokenType};
use crate::parse::ast_nodes::{AstNode, CondBranch, DefNode, ExprNode, FuncArg, LitNode, OpNode, OpType, Param};
use crate::parse::ast_nodes::AstNode::{DefinitionNode, ExpressionNode, LiteralNode, OperationNode};
use crate::parse::ast_nodes::ExprNode::{CondExpr, ConsExpr, IfExpr, ListAccess, MultiExpr, PairList, PrintExpr, SingleExpr, WhileLoop};


use crate::parse::TokenType::{Definition, Expression, Lexical, Literal, Operation, Syntactic, Modifier};

struct ParserState {
    pub tokens: Vec<Token>,
    pub current: usize,
    pub end: usize,
    pub depth: i32,
    pub warnings: Vec<String>,
}

impl ParserState {
    pub fn new(tokens: Vec<Token>) -> ParserState {
        let len = tokens.len();
        ParserState {
            tokens,
            current: 0,
            end: len,
            depth: 0,
            warnings: Vec::<String>::new(),
        }
    }

    pub fn process(&mut self) -> Result<Vec<AstNode>, String> {
        let mut root_expressions = Vec::<AstNode>::new();
        while self.have_next() {
            root_expressions.push(self.parse_expr_data()?);
        }
        Ok(root_expressions)
    }

    pub fn push_warning(&mut self, warning: String) {
        self.warnings.push(warning);
    }

    pub fn have_next(&mut self) -> bool {
        self.current + 1 < self.end
    }

    pub fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    pub fn peek_n(&self, n: usize) -> &Token {
        &self.tokens[self.current + (n - 1)]
    }

    pub fn previous(&mut self) -> &Token {
        self.tokens.get(std::cmp::max(0, self.current - 1)).unwrap()
    }

    pub fn previous_n(&mut self, n: usize) -> &Token {
        self.tokens.get(std::cmp::max(0, self.current - n)).unwrap()
    }

    pub fn check(&mut self, token_type: &TokenType) -> bool {
        if !self.have_next() {
            false
        } else { &self.peek().token_type == token_type }
    }

    pub fn advance(&mut self) -> Result<&Token, String> {
        if !self.have_next() { return Err("Advanced past end".to_string()); }

        if matches!(self.peek().token_type, Lexical(Lex::LeftParen) | Lexical(Lex::RightParen)) {
            panic!("Parenthesis should only be advanced via consume paren function");
            return Err("Parenthesis should only be advanced via consume paren function".to_string());
        }
        self.current += 1;
        Ok(&self.tokens[self.current - 1])
    }

    pub fn match_token(&mut self, tokens: &[TokenType]) -> Option<&Token> {
        self.tokens.iter().find(|token| tokens.contains(&token.token_type))
    }

    pub fn consume(&mut self, token_type: TokenType) -> Result<&Token, String> {
        if !self.have_next() { return Err("Advanced past end".to_string()); }

        if matches!(self.peek().token_type, Lexical(Lex::LeftParen) | Lexical(Lex::RightParen)) {
            panic!("Parenthesis should only be advanced via consume paren function");
            return Err("Parenthesis should only be advanced via consume paren function".to_string());
        }
        if !self.check(&token_type) {
            panic!("Expected: {:?}, Found: {:?}", token_type, self.peek());
            return Err(format!("Expected: {:?}, Found: {:?}", token_type, self.peek()));
        }
        self.advance()
    }

    pub fn consume_left_paren(&mut self) -> Result<(), String> {
        if !self.check(&Lexical(Lex::LeftParen)) {
            panic!("Expected: LeftParen, Found: {:?}", self.peek());
            return Err(format!("Expected: LeftParen, Found: {:?}", self.peek()));
        }
        self.current += 1;
        self.depth += 1;
        Ok(())
    }

    pub fn consume_right_paren(&mut self) -> Result<(), String> {
        if !self.check(&Lexical(Lex::RightParen)) {
            panic!("Expected: RightParent, Found: {:?}", self.peek());
            return Err(format!("Expected: LeftParen, Found: {:?}", self.peek()));
        }
        self.current += 1;
        self.depth -= 1;
        Ok(())
    }

    pub fn parse_expr_data(&mut self) -> Result<AstNode, String> {
        if (self.current > 1) {
            println!("prev: {:?}", self.previous());
            println!("prev: {:?}", self.peek());
        }
        match &self.peek().token_type {
            Lexical(Lex::LeftParen) => self.parse_s_expr(),
            Lexical(Lex::RightParen) => {
                // Needed, this function is entered by parse_s_expr, but could be null list lit
                if matches!(self.previous().token_type, Lexical(Lex::LeftParen)) {
                    Ok(LiteralNode(Box::new(LitNode::Nil)))
                } else {
                    panic!();
                    Err(format!("Right paren with no matching open, line: {}", self.peek().line))
                }
            }
            Operation(_) => self.parse_operation(),
            Literal(_) => self.parse_literal(),
            Syntactic(Syn::Grave) => self.parse_quote(),
            Expression(_) => self.parse_exact_expr(),
            Definition(Def::Define) => self.parse_define(),
            Definition(Def::DefineFunc) => self.parse_func(),
            Definition(Def::Lambda) => self.parse_lambda(false),
            Syntactic(_) => Err(format!("Unexpected token: {:?}", &self.peek())), // TODO implement quote
            _ => Err(format!("Unexpected token: {:?}", &self.peek()))
        }
    }

    pub fn parse_s_expr(&mut self) -> Result<AstNode, String> {
        self.consume_left_paren()?;
        let expression = self.parse_expr_data();
        // if matches!(&self.peek().token_type, SyntacticToken(Syntactic::Colon)) {
        //     parse object call
        // }
        let expr = &expression;
        if let Ok((s)) = expr {
            println!("expr: {:?}", s)
        }
        println!("Prev: {:?}", self.previous());


        self.consume_right_paren()?;
        expression
    }

    pub fn parse_operation(&mut self) -> Result<AstNode, String> {
        let operation = &self.peek().token_type.clone();
        self.advance()?;

        let mut operands = Vec::<AstNode>::new();
        while self.have_next() && self.peek().token_type != Lexical(Lex::RightParen) {
            operands.push(self.parse_expr_data()?);
        }

        match operation {
            Operation(Op::And) => Ok(OperationNode(OpNode::And { op_type: OpType::Boolean, operands })),
            Operation(Op::Or) => Ok(OperationNode(OpNode::Or { op_type: OpType::Boolean, operands })),
            Operation(Op::Nor) => Ok(OperationNode(OpNode::Nor { op_type: OpType::Boolean, operands })),
            Operation(Op::Xor) => Ok(OperationNode(OpNode::Xor { op_type: OpType::Boolean, operands })),
            Operation(Op::Xnor) => Ok(OperationNode(OpNode::Xnor { op_type: OpType::Boolean, operands })),
            Operation(Op::Nand) => Ok(OperationNode(OpNode::Nand { op_type: OpType::Boolean, operands })),
            Operation(Op::Negate) => Ok(OperationNode(OpNode::Negate { op_type: OpType::Boolean, operands })),
            Operation(Op::Plus) => Ok(OperationNode(OpNode::Addition { op_type: OpType::Arithmetic, operands })),
            Operation(Op::Minus) => Ok(OperationNode(OpNode::Subtraction { op_type: OpType::Arithmetic, operands })),
            Operation(Op::Asterisk) => Ok(OperationNode(OpNode::Multiplication { op_type: OpType::Arithmetic, operands })),
            Operation(Op::Slash) => Ok(OperationNode(OpNode::Division { op_type: OpType::Arithmetic, operands })),
            Operation(Op::Caret) => Ok(OperationNode(OpNode::Exponentiate { op_type: OpType::Arithmetic, operands })),
            Operation(Op::Percent) => Ok(OperationNode(OpNode::Modulo { op_type: OpType::Arithmetic, operands })),
            Operation(Op::PlusPlus) => Ok(OperationNode(OpNode::Increment { op_type: OpType::Arithmetic, operands })),
            Operation(Op::MinusMinus) => Ok(OperationNode(OpNode::Decrement { op_type: OpType::Arithmetic, operands })),
            Operation(Op::Greater) => Ok(OperationNode(OpNode::GreaterThan { op_type: OpType::Comparison, operands })),
            Operation(Op::Less) => Ok(OperationNode(OpNode::LessThan { op_type: OpType::Comparison, operands })),
            Operation(Op::GreaterEqual) => Ok(OperationNode(OpNode::GreaterThanEqual { op_type: OpType::Comparison, operands })),
            Operation(Op::LessEqual) => Ok(OperationNode(OpNode::LessThanEqual { op_type: OpType::Comparison, operands })),
            Operation(Op::Equals) => Ok(OperationNode(OpNode::Equality { op_type: OpType::Equality, operands })),
            Operation(Op::BangEquals) => Ok(OperationNode(OpNode::RefNonEquality { op_type: OpType::Equality, operands })),
            Operation(Op::RefEqual) => Ok(OperationNode(OpNode::RefEquality { op_type: OpType::Equality, operands })),
            _ => Err(format!("Fatal, Expected Operation received {:?}", operation))
        }
    }

    pub fn parse_literal(&mut self) -> Result<AstNode, String> {
        let token = self.advance()?;

        match &token.token_type {
            Literal(lit) => {
                match lit {
                    Lit::True => Ok(LiteralNode(Box::new(LitNode::Boolean(true)))),
                    Lit::False => Ok(LiteralNode(Box::new(LitNode::Boolean(false)))),
                    Lit::String => {
                        if let Some(TokenData::String(value)) = &token.data {
                            Ok(LiteralNode(Box::new(LitNode::String(value.to_string()))))
                        } else { Err(format!("Invalid data for string literal: {:?}", token.data)) }
                    }
                    Lit::Int => {
                        if let Some(TokenData::Integer(value)) = &token.data {
                            Ok(LiteralNode(Box::new(LitNode::Integer(*value))))
                        } else { Err(format!("Invalid data for integer literal: {:?}", token.data)) }
                    }
                    Lit::Float => {
                        if let Some(TokenData::Float(value)) = &token.data {
                            Ok(LiteralNode(Box::new(LitNode::Float(*value))))
                        } else { Err(format!("Invalid data for float literal: {:?}", token.data)) }
                    }
                    Lit::Identifier => {
                        if let Some(TokenData::String(value)) = &token.data {
                            Ok(LiteralNode(Box::new(LitNode::String(value.to_string()))))
                        } else { Err(format!("Invalid data for identifier: {:?}", token.data)) }
                    }
                    Lit::Nil => Ok(LiteralNode(Box::new(LitNode::Nil)))
                }
            }
            _ => Err(format!("Expected literal value found: {:?}", token))
        }
    }

    pub fn parse_exact_expr(&mut self) -> Result<AstNode, String> {
        let expression = self.advance()?;
        match expression.token_type {
            Expression(Expr::Assign) => self.parse_assign(),
            Expression(Expr::If) => self.parse_if(),
            Expression(Expr::Cond) => self.parse_cond(),
            Expression(Expr::Begin) => self.parse_multi_expr(),
            Expression(Expr::Print) => self.parse_print(),
            Expression(Expr::List) => self.parse_list(),
            Expression(Expr::Lacc) => self.parse_list_access(),
            Expression(Expr::While) => self.parse_while(),
            Expression(Expr::Cons) => self.parse_cons(),
            Expression(Expr::Car) => {
                Ok(ExpressionNode(Box::new(ListAccess { index_expr: None, pattern: Some("f".to_string()) })))
            }
            Expression(Expr::Cdr) => {
                Ok(ExpressionNode(Box::new(ListAccess { index_expr: None, pattern: Some("r".to_string()) })))
            }
            _ => Err(format!("Expected expression found {:?}", expression))
        }
    }

    pub fn parse_assign(&mut self) -> Result<AstNode, String> {
        self.advance()?;
        self.parse_expr_data()
    }

    pub fn parse_if(&mut self) -> Result<AstNode, String> {
        let condition = self.parse_expr_data()?;
        let then = self.parse_expr_data()?;
        let if_branch = CondBranch { cond_node: condition, then_node: then };
        let else_branch = if self.peek().token_type == Lexical(Lex::LeftParen) {
            Some(self.parse_expr_data()?)
        } else { None };
        Ok(ExpressionNode(Box::new(IfExpr { if_branch, else_branch })))
    }

    pub fn parse_cond(&mut self) -> Result<AstNode, String> {
        let mut cond_branches = Vec::<CondBranch>::new();
        let mut else_branch = None;

        while self.peek().token_type != Lexical(Lex::RightParen) {
            if self.peek_n(2).token_type == Syntactic(Syn::Else) {
                self.consume_left_paren()?;
                self.consume(Syntactic(Syn::Else))?;
                else_branch = Some(self.parse_expr_data()?);
                self.consume_right_paren()?;
                break;
            }
            cond_branches.push(self.parse_cond_branch()?)
        }

        if cond_branches.is_empty() {
            Err(format!("Cond expression must have at least one branch, line: {}", self.peek().line))
        } else { Ok(ExpressionNode(Box::new(CondExpr { cond_branches, else_branch }))) }
    }

    pub fn parse_cond_branch(&mut self) -> Result<CondBranch, String> {
        self.consume_left_paren()?;

        let cond_node = self.parse_expr_data()?;
        let then_node = self.parse_expr_data()?;
        self.consume_right_paren()?;
        Ok(CondBranch { cond_node, then_node })
    }

    pub fn parse_multi_expr(&mut self) -> Result<AstNode, String> {
        let mut expressions = Vec::<AstNode>::new();
        while self.peek().token_type != Lexical(Lex::RightParen) {
            expressions.push(self.parse_expr_data()?);
        }

        if expressions.is_empty() {
            Err(format!("Expected one or more expressions, line: {}", self.peek().line))
        } else if expressions.len() == 1 {
            Ok(ExpressionNode(Box::new(SingleExpr(expressions.pop().unwrap()))))
        } else { Ok(ExpressionNode(Box::new(MultiExpr(expressions)))) }
    }

    // Todo add some checks for define nodes and things that would get evaled
    //  but shouldn't be in a print statement
    pub fn parse_print(&mut self) -> Result<AstNode, String> {
        let expr = self.parse_expr_data()?;
        Ok(ExpressionNode(Box::new(PrintExpr(expr))))
    }

    pub fn parse_list(&mut self) -> Result<AstNode, String> {
        let mut elements = Vec::<AstNode>::new();
        while self.peek().token_type != Lexical(Lex::RightParen) {
            elements.push(self.parse_expr_data()?);
        }

        if elements.is_empty() {
            Ok(LiteralNode(Box::new(LitNode::Nil)))
        } else { Ok(ExpressionNode(Box::new(PairList(elements)))) }
    }

    pub fn parse_list_head(&mut self) -> Result<AstNode, String> {
        let head = self.parse_expr_data()?;
        if self.peek().token_type != Lexical(Lex::RightParen) {
            Err(format!("Invalid argument count, line: {}", self.peek().line))
        } else { Ok(head) }
    }

    pub fn parse_list_access(&mut self) -> Result<AstNode, String> {
        if self.peek().token_type == Syntactic(Syn::Grave) {
            self.advance()?;
            let token_data = &self.consume(Literal(Lit::Identifier))?.data;

            if let Some(TokenData::String(s)) = token_data {
                Ok(ExpressionNode(Box::new(ListAccess { index_expr: None, pattern: Some(s.clone()) })))
            } else { Err("Expected fr... access pattern".to_string()) }
        } else {
            let index = self.parse_expr_data()?;
            Ok(ExpressionNode(Box::new(ListAccess { index_expr: Some(index), pattern: None })))
        }
    }

    pub fn parse_while(&mut self) -> Result<AstNode, String> {
        let is_do = if self.peek().token_type == Modifier(Mod::Do) {
            self.advance()?;
            true
        } else { false };

        let condition = self.parse_expr_data()?;
        let body = self.parse_multi_expr()?;
        Ok(ExpressionNode(Box::new(WhileLoop { condition, body, is_do })))
    }

    pub fn parse_cons(&mut self) -> Result<AstNode, String> {
        let car = self.parse_expr_data()?;
        let cdr = self.parse_expr_data()?;

        if self.peek().token_type == Lexical(Lex::RightParen) {
            Err("Cons expression may only have 2 arguments".to_string())
        } else { Ok(ExpressionNode(Box::new(ConsExpr { car, cdr }))) }
    }

    pub fn parse_type_if_exists(&mut self) -> Result<Option<String>, String> {
        let typ = if let (
            Syntactic(Syn::Type), Some(TokenData::String(s))
        ) = (&self.peek().token_type, &self.peek().data) {
            Some(s.clone())
        } else { None };

        if typ.is_some() { self.advance()?; }
        Ok(typ)
    }

    pub fn parse_define(&mut self) -> Result<AstNode, String> {
        self.consume(Definition(Def::Define))?;

        let name = match &self.consume(Literal(Lit::Identifier))?.data {
            Some(TokenData::String(name)) => name.clone(),
            _ => return Err("Expected a string identifier".to_string())
        };
        let modifiers = self.parse_modifiers()?;
        let var_type = self.parse_type_if_exists()?;

        let definition = match self.peek().token_type {
            Lexical(Lex::LeftParen) => {
                if self.peek_n(2).token_type == Definition(Def::Lambda) {
                    if var_type != None {
                        self.push_warning(format!(
                            "Type specifier: {}, is unused for function definition, line: {}",
                            var_type.unwrap(), self.peek().line))
                    }

                    self.consume_left_paren()?;
                    let lambda = self.parse_lambda(false)?;
                    self.consume_right_paren()?;
                    DefinitionNode(Box::new(DefNode::Function { name, lambda }))
                } else {
                    let value = Box::new(self.parse_expr_data()?);
                    DefinitionNode(Box::new(DefNode::Variable { name, modifiers, value, var_type }))
                }
            }
            Literal(_) => {
                let value = Box::new(self.parse_literal()?);
                DefinitionNode(Box::new(DefNode::Variable { name, modifiers, value, var_type }))
            }
            Syntactic(Syn::Grave) => self.parse_quote()?,
            _ => return Err(format!("Invalid syntax in define, line: {}", self.peek().line))
        };

        return Ok(definition);
    }


    pub fn parse_quote(&mut self) -> Result<AstNode, String> {
        self.consume(Syntactic(Syn::Grave))?;
        Ok(LiteralNode(Box::new(LitNode::Quote(self.parse_expr_data()?))))
    }


    pub fn parse_func(&mut self) -> Result<AstNode, String> {
        self.consume(Definition(Def::DefineFunc))?;

        let name = match &self.consume(Literal(Lit::Identifier))?.data {
            Some(TokenData::String(name)) => name.clone(),
            _ => return Err("Expected name for function".to_string())
        };
        let lambda = self.parse_lambda(true)?;
        println!("exiting defunc with: {:?}", self.peek());
        Ok(DefinitionNode(Box::new(DefNode::Function { name, lambda })))
    }

    pub fn parse_lambda(&mut self, is_defunc: bool) -> Result<AstNode, String> {
        if !is_defunc { self.consume(Definition(Def::Lambda))?; }

        let modifiers = if matches!(self.peek().token_type, Modifier(_)) {
            self.parse_modifiers()?
        } else { None };

        let parameters = self.parse_parameters()?;
        let body = self.parse_multi_expr()?;
        //self.consume_right_paren();


        let rtn_type = self.parse_type_if_exists()?;
        Ok(DefinitionNode(Box::new(DefNode::Lambda { modifiers, parameters, body, rtn_type })))
    }

    pub fn parse_modifiers(&mut self) -> Result<Option<Vec<Mod>>, String> {
        if !matches!(self.peek().token_type, Modifier(_)) { return Ok(None); };

        let mut modifiers = Vec::<Mod>::new();
        while let Modifier(modifier) = &self.peek().token_type {
            modifiers.push(*modifier);
            self.advance()?;
        }
        if modifiers.is_empty() {
            Ok(None)
        } else { Ok(Some(modifiers)) }
    }

    pub fn parse_func_args(&mut self) -> Result<Option<Vec<FuncArg>>, String> {
        let mut func_args = Vec::<FuncArg>::new();
        while self.peek().token_type != Lexical(Lex::RightParen) {
            if self.peek().token_type == Syntactic(Syn::Colon) {
                self.advance()?; // consume colon
                let token_data = self.consume(Literal(Lit::Identifier))?;
                let name = match token_data.data {
                    Some(TokenData::String(ref s)) => s.clone(), // Clone to get owned String
                    _ => return Err("Expected a string identifier".to_string())
                };
                let value = self.parse_expr_data()?;
                func_args.push(FuncArg { name: Some(name.clone()), value });
            } else {
                let value = self.parse_expr_data()?;
                func_args.push(FuncArg { name: None, value });
            }
        }
        Ok(Some(func_args))
    }


    pub fn parse_parameters(&mut self) -> Result<Option<Vec<Param>>, String> {
        self.consume_left_paren()?;
        if self.peek().token_type == Lexical(Lex::RightParen) {
            self.consume_right_paren()?;
            return Ok(None);
        }

        let mut optional = false;
        let mut params = Vec::<Param>::new();

        while self.peek().token_type != Lexical(Lex::RightParen) {
            println!("Peek before modifers:{:?}", self.peek());
            let modifiers = self.parse_modifiers()?;
            println!("Previous:{:?}", self.previous());
            let mut dynamic = false;
            let mut mutable = false;
            println!("Modifiers: {:?}", modifiers);
            for modifier in modifiers.unwrap_or_default() {
                match modifier {
                    Mod::Mutable => mutable = true,
                    Mod::Dynamic => dynamic = true,
                    Mod::Optional => if !optional { optional = true },
                    _ => {}
                };
            };

            println!("Previous:{:?}", self.previous());
            println!("is  optional: {}", optional);
            println!("params: {:?}", params);

            let name = match &self.consume(Literal(Lit::Identifier))?.data {
                Some(TokenData::String(s)) => s.clone(),
                _ => return Err("Expected a variable identifier".to_string())
            };
            println!("Assigned: {}", name);
            println!("Next: {:?}", self.peek());
            let default_value: Option<AstNode> = if optional {
                if self.peek().token_type != Syntactic(Syn::Equal) {
                    return Err(format!(
                        "All parameters after &opt modifier need default assignments, line: {}",
                        self.peek().line));
                };
                self.advance()?; // consume =
                Some(self.parse_literal()?)
            } else { None };

            let typ = self.parse_type_if_exists()?;
            println!("type exists?: {}", typ.is_some());
            println!("Next at end:{:?}", self.peek());
            params.push(Param { name, typ, optional, default_value, dynamic, mutable });
        }
        self.consume_right_paren()?;
        Ok(Some(params))
    }
}

pub fn process(tokens: Vec<Token>) -> Result<Vec<AstNode>, String> {
    let mut state = ParserState::new(tokens);
    state.process()
}



