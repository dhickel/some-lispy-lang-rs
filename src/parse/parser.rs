use std::fmt::format;
use crate::parse::{Def, Lex, Lit, Op, Token, TokenData, TokenType};
use crate::parse::ast_nodes::{AstNode, LitNode, OpNode, OpType};
use crate::parse::ast_nodes::AstNode::{LiteralNode, OperationNode};
use crate::parse::Lex::RightParen;
use crate::parse::TokenType::{Definition, Expression, Lexical, Literal, Operation, Syntactic};

struct ParserState {
    pub tokens: Vec<Token>,
    pub current: usize,
    pub end: usize,
    pub depth: i32,
}

impl ParserState {
    pub fn new(tokens: Vec<Token>) -> ParserState {
        let len = tokens.len();
        ParserState {
            tokens,
            current: 0,
            end: len,
            depth: 0,
        }
    }

    pub fn have_next(&mut self) -> bool {
        self.current + 1 < self.end
    }

    pub fn peek(&mut self) -> &Token {
        &self.tokens[self.current]
    }

    pub fn peek_n(&mut self, n: usize) -> &Token {
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

        if matches!(self.peek().token_type,Lexical(Lex::LeftParen)|Lexical(Lex::RightParen)) {
            return Err("Parenthesis should only be advanced via consume paren function".to_string());
        }
        self.current += 1;
        Ok(&self.tokens[self.current - 1])
    }

    pub fn match_token(&mut self, tokens: &[TokenType]) -> Option<&Token> {
        self.tokens.iter().find(|token| tokens.contains(&token.token_type))
    }

    pub fn consume(&mut self, token_type: &TokenType) -> Result<&Token, String> {
        if !self.have_next() { return Err("Advanced past end".to_string()); }

        if matches!(self.peek().token_type, Lexical(Lex::LeftParen) | Lexical(Lex::RightParen)) {
            return Err("Parenthesis should only be advanced via consume paren function".to_string());
        }
        if !self.check(&token_type) {
            return Err(format!("Expected: {:?}, Found: {:?}", token_type, self.peek()));
        }
        self.current += 1;
        Ok(&self.tokens[self.current])
    }

    pub fn consume_left_paren(&mut self) -> Result<(), String> {
        if !self.check(&Lexical(Lex::LeftParen)) {
            return Err(format!("Expected: LeftParen, Found: {:?}", self.peek()));
        }
        self.depth += 1;
        self.current += 1;
        Ok(())
    }

    pub fn consume_right_paren(&mut self) -> Result<(), String> {
        if !self.check(&Lexical(Lex::RightParen)) {
            return Err(format!("Expected: LeftParen, Found: {:?}", self.peek()));
        }
        self.depth -= 1;
        self.current += 1;
        Ok(())
    }

    pub fn process(&mut self) -> Result<Vec<AstNode>, String> {
        let mut root_expressions = Vec::<AstNode>::new();
        while self.have_next() {
            root_expressions.push(self.parse_expr_data()?);
        }
        Ok(root_expressions)
    }

    pub fn parse_expr_data(&mut self) -> Result<AstNode, String> {
        match &self.peek().token_type {
            Lexical(Lex::LeftParen) => self.parse_s_expr(),
            Lexical(Lex::RightParen) => {
                // Needed, this function is entered by parse_s_expr, but could be null list lit
                if matches!(self.previous().token_type, Lexical(Lex::LeftParen)) {
                    Ok(LiteralNode(Box::new(LitNode::Nil())))
                } else { Err(format!("Right paren with no matching open, line: {}", self.peek().line)) }
            }
            Operation(_) => self.parse_operation(),
            Literal(_) => self.parse_literal(),
            Expression(_) => self.parse_exact_expr(),
            Definition(Def::Define) => self.parse_define(),
            Definition(Def::DefineFunc) => self.parse_func(),
            Definition(Def::Lambda) => self.parse_lambda(),
            Syntactic(_) => Err(format!("Unexpected token: {:?}", &self.peek())), // TODO implement quote
            _ => Err(format!("Unexpected token: {:?}", &self.peek()))
        }
    }

    pub fn parse_s_expr(&mut self) -> Result<AstNode, String> {
        self.consume_left_paren()?;

        if self.peek().token_type == Lexical(RightParen) {// empty list if () is found
            return Ok(LiteralNode(Box::new(LitNode::Nil())));
        }
        let expression = self.parse_expr_data();
        // if matches!(&self.peek().token_type, SyntacticToken(Syntactic::Colon)) {
        //     parse object call
        // }

        self.consume_right_paren()?;
        expression
    }

    pub fn parse_operation(&mut self) -> Result<AstNode, String> {
        let operation = &self.peek().token_type.clone();
        self.advance()?;

        let mut operands = Vec::<AstNode>::new();
        while self.have_next() && !matches!(&self.peek().token_type, Lexical(Lex::RightParen)) {
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
                    Lit::Nil => Ok(LiteralNode(Box::new(LitNode::Nil())))
                }
            }
            _ => Err(format!("Expected literal value found: {:?}", token))
        }
    }

    pub fn parse_exact_expr(&mut self) -> Result<AstNode, String> { panic!("Invalid token should not be reached") }

    pub fn parse_define(&mut self) -> Result<AstNode, String> { panic!("Invalid token should not be reached") }

    pub fn parse_func(&mut self) -> Result<AstNode, String> { panic!("Invalid token should not be reached") }

    pub fn parse_lambda(&mut self) -> Result<AstNode, String> { panic!("Invalid token should not be reached") }
}

pub fn process(tokens: Vec<Token>) -> Result<Vec<AstNode>, String> {
    let mut state = ParserState::new(tokens);
    state.process()
}


