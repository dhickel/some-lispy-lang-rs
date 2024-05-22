use std::collections::LinkedList;
use crate::types::{FuncType, Type};
use lang::{format_error, util};
use lang::util::{IString, SCACHE};

use crate::ast::*;
use crate::ast::AstData::*;
use crate::token::*;
use crate::token::Syn::SemiColon;
use crate::token::TokenType::*;


struct ParserState {
    pub tokens: Vec<Token>,
    pub current: usize,
    pub end: usize,
    pub depth: i32,
    pub warnings: Vec<String>,
    pub name_space: IString,
}


#[derive(Debug)]
pub struct ParseResult {
    pub name_space: IString,
    pub root_expressions: Vec<AstNode>,
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
            name_space: SCACHE.intern("main".to_string()),
        }
    }


    pub fn process(&mut self) -> Result<ParseResult, String> {
        let mut root_expressions = Vec::<AstNode>::new();
        while self.have_next() {
            // root_expressions.push(self.parse_expr_data()?);
            match self.parse_data() {
                Ok(parse_data) => root_expressions.push(parse_data),
                Err(err) => return Err(format_error!(self.line_char(), err))
            }
        }
        Ok(ParseResult { name_space: self.name_space, root_expressions })
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

    pub fn line_char(&self) -> (u32, u32) {
        let peek = self.peek();
        (peek.line, peek.char)
    }

    pub fn peek_n(&self, n: usize) -> &Token {
        &self.tokens[self.current + (n - 1)]
    }

    pub fn previous(&self) -> &Token {
        self.tokens.get(if self.current == 0 { 0 } else { self.current - 1 }).unwrap()
    }

    pub fn previous_n(&self, n: usize) -> &Token {
        self.tokens.get(if self.current < n { 0 } else { self.current - n }).unwrap()
    }

    pub fn check(&mut self, token_type: &TokenType) -> bool {
        if !self.have_next() {
            false
        } else { &self.peek().token_type == token_type }
    }


    pub fn advance(&mut self) -> Result<&Token, String> {
        if !self.have_next() { return Err("Advanced past end".to_string()); }

        if matches!(self.peek().token_type, TSyntactic(Syn::LeftParen) | TSyntactic(Syn::RightParen)) {
            panic!("Parenthesis should  be advanced via consume paren function");
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

        if matches!(self.peek().token_type, TSyntactic(Syn::LeftParen) | TSyntactic(Syn::RightParen)) {
            //panic!("Parenthesis should only be advanced via consume paren function");
            return Err("Parenthesis should only be advanced via consume paren function".to_string());
        }
        if !self.check(&token_type) {
            panic!("Expected: {:?}, Found: {:?}", token_type, self.peek());
            return Err(format!("Expected: {:?}, Found: {:?}", token_type, self.peek()));
        }
        self.advance()
    }


    pub fn consume_left_paren(&mut self) -> Result<(), String> {
        if !self.check(&TSyntactic(Syn::LeftParen)) {
            //  panic!("Expected: Left Paren, Found: {:?}", self.peek());
            return Err(format!("Expected: Left Paren, Found: {:?}", self.peek()));
        }
        self.current += 1;
        self.depth += 1;
        Ok(())
    }


    pub fn consume_right_paren(&mut self) -> Result<(), String> {
        if !self.check(&TSyntactic(Syn::RightParen)) {
            //  panic!("Expected: Right Paren, Found: {:?}", self.peek());
            return Err(format!("Expected: Right Paren, Found: {:?}", self.peek()));
        }
        self.current += 1;
        self.depth -= 1;
        Ok(())
    }

    pub fn consume_left_bracket(&mut self) -> Result<(), String> {
        if !self.check(&TSyntactic(Syn::LeftBracket)) {
            panic!("Expected: Left Bracket, Found: {:?}", self.peek());
            return Err(format!("Expected: Left Bracket, Found: {:?}", self.peek()));
        }
        self.current += 1;
        Ok(())
    }


    pub fn consume_right_bracket(&mut self) -> Result<(), String> {
        if !self.check(&TSyntactic(Syn::RightBracket)) {
            panic!("Expected: Right Bracket, Found: {:?}", self.peek());
            return Err(format!("Expected: Right Bracket, Found: {:?}", self.peek()));
        }
        self.current += 1;
        Ok(())
    }


    pub fn consume_left_brace(&mut self) -> Result<(), String> {
        if !self.check(&TSyntactic(Syn::LeftBrace)) {
            // panic!("Expected: Left Bracket, Found: {:?}", self.peek());
            return Err(format!("Expected: Left Brace, Found: {:?}", self.peek()));
        }
        self.current += 1;
        Ok(())
    }


    pub fn consume_right_brace(&mut self) -> Result<(), String> {
        if !self.check(&TSyntactic(Syn::RightBrace)) {
            //   panic!("Expected: Right Bracket, Found: {:?}", self.peek());
            return Err(format!("Expected: Right Brace, Found: {:?}", self.peek()));
        }
        self.current += 1;
        Ok(())
    }


    pub fn parse_data(&mut self) -> Result<AstNode, String> {
        match &self.peek().token_type {
            TSyntactic(Syn::LeftParen) => {
                self.parse_s_expr()
            }

            TSyntactic(Syn::RightParen) => {
                // Needed, this function is entered by parse_s_expr, but could be null list lit
                if matches!(self.previous().token_type, TSyntactic(Syn::LeftParen)) {
                    Ok(AstNode::new(LitNil, self.line_char()))
                } else {
                    Err(format!("Right paren with no matching open, line: {}", self.peek().line))
                }
            }

            TLiteral(_) => {
                if self.peek_n(2).token_type == TSyntactic(Syn::Assign) {
                    self.parse_assign()
                } else {
                    self.parse_literal()
                }
            }

            TSyntactic(Syn::LeftBrace) => {
                if let Some(m_expr) = self.parse_multi_expr().unwrap() {
                    Ok(m_expr)
                } else {
                    self.parse_data()
                }
            }

            TDefinition(Def::Define) => {
                self.parse_define()
            }

            TOperation(_) => {
                self.parse_operation()
            }

            TSyntactic(Syn::SingleQuote) => {
                self.parse_quote()
            }

            TExpression(_) => {
                self.parse_exact_expr()
            }

            TDefinition(Def::DefineFunc) => {
                let data = self.parse_func(true)?;
                Ok(AstNode::new(DefFunction(data), self.line_char()))
            }

            TDefinition(Def::Lambda) => {
                let data = self.parse_lambda()?;
                Ok(AstNode::new(DefLambda(data), self.line_char()))
            }

            TDefinition(Def::DefineStruct) => {
                self.parse_struct_def()
            }

            TDefinition(Def::DefineClass) => {
                self.parse_class_def()
            }

            TSyntactic(_) => {// TODO implement quote
                panic!("{:?}", &self.peek());
                Err(format!("Unexpected token: {:?}", &self.peek()))
            }

            _ => {
                Err(format!("Unexpected token: {:?}", &self.peek()))
            }
        }
    }


    pub fn parse_s_expr(&mut self) -> Result<AstNode, String> {
        self.consume_left_paren()?;
        let expression = self.parse_data()?;

        // Handle edge case where first expr in an s-expr is something that evals to a lambda call

        if self.peek().token_type != TSyntactic(Syn::RightParen) {
            let args = self.parse_func_args()?;
            self.consume_right_paren()?;

            let data = InnerFuncCallData {
                namespace: None, // FIXME
                expr: expression,
                accessors: None,
                arguments: args,
            };
            return Ok(AstNode::new(ExprFuncCalInner(data), self.line_char()));
        }

        self.consume_right_paren()?;
        Ok(expression)
    }


    pub fn parse_operation(&mut self) -> Result<AstNode, String> {
        let operation = if let TOperation(op) = &self.peek().token_type.clone() {
            op.clone()
        } else { return Err("Expected operation token".to_string()); };

        self.advance()?;

        let mut operands = Vec::<AstNode>::new();
        while self.have_next() && self.peek().token_type != TSyntactic(Syn::RightParen) {
            operands.push(self.parse_data()?);
        }

        let data = OpData { operation, operands };
        Ok(AstNode::new(Operation(data), self.line_char()))
    }


    pub fn parse_literal(&mut self) -> Result<AstNode, String> {
        let token = self.advance().unwrap().token_type;
        let data = &self.previous().data;

        match token {
            TLiteral(lit) => {
                match lit {
                    Lit::True => {
                        Ok(AstNode::new(LitBoolean(true), self.line_char()))
                    }

                    Lit::False => {
                        Ok(AstNode::new(LitBoolean(false), self.line_char()))
                    }

                    Lit::String => {
                        if let Some(TokenData::String(value)) = data {
                            let string = util::SCACHE.resolve(*value).to_string();
                            Ok(AstNode::new(LitString(string), self.line_char()))
                        } else { Err(format!("Invalid data for string literal: {:?}", &self.peek().data)) }
                    }

                    Lit::Int => {
                        if let Some(TokenData::Integer(value)) = data {
                            Ok(AstNode::new(LitInteger(*value), self.line_char()))
                        } else { Err(format!("Invalid data for integer literal: {:?}", &self.peek().data)) }
                    }

                    Lit::Float => {
                        if let Some(TokenData::Float(value)) = data {
                            Ok(AstNode::new(LitFloat(*value), self.line_char()))
                        } else { Err(format!("Invalid data for float literal: {:?}", &self.peek().data)) }
                    }

                    Lit::Identifier => {
                        if let Some(TokenData::String(value)) = data {
                            self.parse_identifier(*value)
                        } else { Err(format!("Invalid data for identifier: {:?}", &self.peek().data)) }
                    }

                    Lit::Instance => {
                        if let Some(TokenData::String(value)) = data {
                            self.parse_instance(*value)
                        } else { Err(format!("Invalid data for identifier: {:?}", &self.peek().data)) }
                    }

                    Lit::Nil => Ok(AstNode::new(LitNil, self.line_char())),
                }
            }
            _ => Err(format!("Expected literal value found: {:?}", token.clone()))
        }
    }


    pub fn parse_exact_expr(&mut self) -> Result<AstNode, String> {
        let expression = self.advance()?;
        match expression.token_type {
            TExpression(Expr::If) => {
                self.parse_if()
            }

            TExpression(Expr::Cond) => {
                self.parse_cond()
            }

            TExpression(Expr::Begin) => {
                match self.parse_multi_expr()? {
                    None => { Err("Expected body for begin, found none".to_string()) }
                    Some(s) => Ok(s)
                }
            }

            TExpression(Expr::Print) => {
                self.parse_print()
            }

            TExpression(Expr::List) => {
                self.parse_list()
            }

            TExpression(Expr::Array) => {
                self.parse_array()
            }

            TExpression(Expr::Lacc) => {
                self.parse_list_access()
            }

            TExpression(Expr::While) => {
                self.parse_while()
            }

            TExpression(Expr::Cons) => {
                self.parse_cons()
            }

            TExpression(Expr::Car) => {
                let data = ListAccData {
                    index_expr: None,
                    pattern: Some(vec![0]),
                    list: self.parse_list_head()?,
                };
                Ok(AstNode::new(ExprListAccess(data), self.line_char()))
            }

            TExpression(Expr::Cdr) => {
                let data = ListAccData {
                    index_expr: None,
                    pattern: Some(vec![1]),
                    list: self.parse_list_head()?,
                };
                Ok(AstNode::new(ExprListAccess(data), self.line_char()))
            }

            TExpression(Expr::Randi) => {
                self.parse_rand(false)
            }

            TExpression(Expr::Randf) => {
                self.parse_rand(true)
            }

            _ => Err(format!("Expected expression found {:?}", expression))
        }
    }


    pub fn parse_rand(&mut self, is_float: bool) -> Result<AstNode, String> {
        let lower = self.parse_data()?;
        let upper = self.parse_data()?;
        let data = GenRandData {
            is_float,
            upper,
            lower,
        };
        Ok(AstNode::new(ExprGenRand(data), self.line_char()))
    }


    pub fn parse_assign(&mut self) -> Result<AstNode, String> {
        match self.peek().token_type {
            TLiteral(Lit::Identifier) => {
                let name = match &self.consume(TLiteral(Lit::Identifier))?.data {
                    Some(TokenData::String(name)) => name.clone(),
                    _ => return Err("Expected a string identifier".to_string())
                };

                self.consume(TSyntactic(Syn::Assign))?;
                let value = self.parse_data()?;

                let data = AssignData { name, value, namespace: None };// FIXME ns
                Ok(AstNode::new(ExprAssignment(data), self.line_char()))
            }

            TSyntactic(Syn::LeftParen) => {
                self.consume_left_paren()?;
                let token = self.advance()?;

                let name = if let (
                    TLiteral(Lit::Identifier), Some(TokenData::String(s))
                ) = (&token.token_type, &token.data) {
                    *s
                } else { return Err(format!("Expected object name to assign to, found: {:?}", token)); };

                let accessors = self.parse_accessors()?;
                self.consume_right_paren()?;
                let value = self.parse_data()?;

                let access = ObjectCallData { name, namespace: None, accessors }; //FIXME ns
                let assign_data = ObjectAssignData { access, value, namespace: None }; // FIXME ns
                Ok(AstNode::new(ExprObjectAssignment(assign_data), self.line_char()))
            }

            _ => Err(format!(
                "Expected identifier or object access to assign to, found: {:?}"
                , self.peek().token_type)
            )
        }
    }


    pub fn parse_if(&mut self) -> Result<AstNode, String> {
        let condition = self.parse_data()?;
        let then = self.parse_data()?;
        let if_branch = CondBranch { cond_node: condition, then_node: then };

        let else_branch = if self.peek().token_type != TSyntactic(Syn::RightParen) {
            Some(self.parse_data()?)
        } else { None };

        let data = IfData { if_branch, else_branch };
        Ok(AstNode::new(ExprIf(data), self.line_char()))
    }


    pub fn parse_cond(&mut self) -> Result<AstNode, String> {
        let mut cond_branches = Vec::<CondBranch>::new();
        let mut else_branch = None;

        while self.peek().token_type != TSyntactic(Syn::RightParen) {
            if self.peek_n(2).token_type == TSyntactic(Syn::Else) {
                self.consume_left_paren()?;

                self.consume(TSyntactic(Syn::Else))?;
                else_branch = Some(self.parse_data()?);

                self.consume_right_paren()?;
                break;
            }
            cond_branches.push(self.parse_cond_branch()?)
        }

        if cond_branches.is_empty() {
            Err(format!("Cond expression must have at least one branch, line: {}", self.peek().line))
        } else {
            let data = CondData { cond_branches, else_branch };
            Ok(AstNode::new(ExprCond(data), self.line_char()))
        }
    }


    pub fn parse_cond_branch(&mut self) -> Result<CondBranch, String> {
        self.consume_left_paren()?;
        let cond_node = self.parse_data()?;
        let then_node = self.parse_data()?;
        self.consume_right_paren()?;
        Ok(CondBranch { cond_node, then_node })
    }


    pub fn parse_multi_expr(&mut self) -> Result<Option<AstNode>, String> {
        self.consume_left_brace()?;
        let mut expressions = Vec::<AstNode>::new();
        while self.peek().token_type != TSyntactic(Syn::RightBrace) {
            expressions.push(self.parse_data()?);
        }

        self.consume_right_brace()?;

        if expressions.is_empty() {
            Ok(None)
        } else if expressions.len() == 1 {
            Ok(Some(expressions.pop().unwrap()))
        } else {
            let data = MultiExprData { expressions };
            Ok(Some(AstNode::new(ExprMulti(data), self.line_char())))
        }
    }


    pub fn parse_print(&mut self) -> Result<AstNode, String> {
        let expr = self.parse_data()?;
        Ok(AstNode::new(ExprPrint(expr), self.line_char()))
    }


    pub fn parse_list(&mut self) -> Result<AstNode, String> {
        let mut elements = Vec::<AstNode>::new();
        while self.peek().token_type != TSyntactic(Syn::RightParen) {
            elements.push(self.parse_data()?);
        }

        if elements.is_empty() {
            Ok(AstNode::new(LitNil, self.line_char()))
        } else {
            let data = OpData { operation: Op::List, operands: elements };
            Ok(AstNode::new(ExprPairList(data), self.line_char()))
        }
    }

    pub fn parse_array(&mut self) -> Result<AstNode, String> {
        let mut elements = Vec::<AstNode>::new();
        while self.peek().token_type != TSyntactic(Syn::RightParen) {
            elements.push(self.parse_data()?);
        }

        if elements.is_empty() {
            Ok(AstNode::new(LitNil, self.line_char()))
        } else {
            let data = OpData { operation: Op::List, operands: elements };
            Ok(AstNode::new(ExprArray(data), self.line_char()))
        }
    }


    pub fn parse_list_head(&mut self) -> Result<AstNode, String> {
        let head = self.parse_data()?;
        if self.peek().token_type != TSyntactic(Syn::RightParen) {
            Err(format!("Invalid argument count, line: {}", self.peek().line))
        } else {
            Ok(head)
        }
    }


    pub fn parse_list_access(&mut self) -> Result<AstNode, String> {
        if self.peek().token_type == TSyntactic(Syn::SingleQuote) {
            self.advance()?;

            let token_data = &self.consume(TLiteral(Lit::Identifier))?.data.clone();
            let list = self.parse_list_head()?;

            if let Some(TokenData::String(s)) = token_data {
                let str = SCACHE.resolve(*s);
                let mut pattern = Vec::<u8>::with_capacity(str.chars().count());

                for char in str.chars() {
                    match char {
                        'f' => pattern.push(0),
                        'r' => pattern.push(1),
                        _ => { return Err(format!("Invalid access pattern char: {}", char)); }
                    }
                }

                let data = ListAccData {
                    index_expr: None,
                    pattern: Some(pattern),
                    list,
                };
                Ok(AstNode::new(ExprListAccess(data), self.line_char()))
            } else {
                Err("Expected fr... access pattern".to_string())
            }
        } else {
            let index = self.parse_data()?;
            let list = self.parse_list_head()?;
            let data = ListAccData {
                index_expr: Some(index),
                pattern: None,
                list,
            };
            Ok(AstNode::new(ExprListAccess(data), self.line_char()))
        }
    }


    pub fn parse_while(&mut self) -> Result<AstNode, String> {
        let is_do = if self.peek().token_type == TModifier(Mod::Do) {
            self.advance()?;
            true
        } else { false };

        let condition = self.parse_data()?;
        let body = match self.parse_multi_expr()? {
            None => { return Err("Expected body for while loop, found none".to_string()); }
            Some(s) => s
        };

        let data = WhileData { condition, body, is_do };
        Ok(AstNode::new(ExprWhileLoop(data), self.line_char()))
    }


    pub fn parse_cons(&mut self) -> Result<AstNode, String> {
        let car = self.parse_data()?;
        let cdr = self.parse_data()?;

        if self.peek().token_type != TSyntactic(Syn::RightParen) {
            Err("Cons expression may only have 2 arguments".to_string())
        } else {
            let data = ConsData { car, cdr };
            Ok(AstNode::new(ExprCons(data), self.line_char()))
        }
    }


    pub fn parse_type_if_exists(&mut self, top_type: bool) -> Result<Option<Type>, String> {
        if top_type {
            if !matches!(self.peek().token_type, TSyntactic(Syn::DoubleColon)) {
                return Ok(None);
            } else { self.consume(TSyntactic(Syn::DoubleColon))?; }
        }

        let mut types = Vec::<Type>::with_capacity(4);
        println!("Parsing: {:?}", self.peek());
        let curr_token = self.advance()?.clone();

        if curr_token.token_type != TLiteral(Lit::Identifier) {
            panic!();
            Err(format!("Expected Type Identifier, Found: {:?}", curr_token).to_string())
        } else if let Some(TokenData::String(string)) = curr_token.data {
            return match string {
                IString { .. } if SCACHE.resolve(string) == "F" => {
                    self.parse_lambda_type()
                }
                IString { .. } if SCACHE.resolve(string) == "Arr" => {
                    self.parse_array_type(string)
                }
                _ => self.parse_basic_type(string)
            };
        } else { return Err("Fatal: Type parse error".to_string()); }
    }


    fn parse_basic_type(&mut self, name: IString) -> Result<Option<Type>, String> {
        let typ = Type::get_basic_type_from_name(name);
        Ok(Some(typ))
    }


    fn parse_lambda_type(&mut self) -> Result<Option<Type>, String> {
        self.consume(TOperation(Op::Less))?;

        let mut arg_types = Vec::<Type>::with_capacity(4);
        let mut rtn_type = Type::Void;

        while self.peek().token_type != TOperation(Op::Greater) {
            let next = self.peek().token_type;

            if next == TSyntactic(SemiColon) {
                self.advance()?;  // skip to identifier after ;
                rtn_type = self.parse_type_if_exists(false)?
                    .ok_or_else(|| format!("Expected Type Identifier, Found: {:?}", self.peek()).to_string())?;

                break; // Break since return is parsed;
            } else {
                let typ = self.parse_type_if_exists(false)?
                    .ok_or_else(|| format!("Expected Type Identifier, Found: {:?}", self.peek()).to_string())?;

                arg_types.push(typ);
                continue;
            }
        }

        self.consume(TOperation(Op::Greater))?;

        // if no args and void return
        let func_type = FuncType {
            rtn_type: Box::new(rtn_type),
            param_types: arg_types,
        };

        Ok(Some(Type::Lambda(func_type)))
    }


    fn parse_array_type(&mut self, base_type: IString) -> Result<Option<Type>, String> {
        self.consume(TOperation(Op::Less))?;
        let arr_type = self.parse_type_if_exists(false)?
            .ok_or_else(|| format!("Expected Type Identifier, Found {:?}", self.peek()).to_string())?;

        self.consume(TOperation(Op::Greater))?;
        Ok(Some(arr_type))
    }


    pub fn parse_identifier(&mut self, identifier: IString) -> Result<AstNode, String> {
        let name = identifier;
        let mut accessors = None;

        let is_func_call = self.previous_n(2).token_type == TSyntactic(Syn::LeftParen);

        if matches!(self.peek().token_type, TSyntactic(Syn::DoubleColon) | TSyntactic(Syn::ColonDot)) {
            accessors = Some(self.parse_accessors()?);
        }

        if is_func_call {
            let arguments = self.parse_func_args()?;

            if accessors.is_some() && arguments.is_some() {
                return Err("Can't call arguments on result of object access pattern, \
                object access should be it's own expression: \
                Ex: ((<Object>::<Method/Field>) <args>)".to_string());
            }

            if let Some(accessors) = accessors {
                let data = ObjectCallData { name, accessors, namespace: None };
                Ok(AstNode::new(ExprObjectCall(data), self.line_char()))
            } else {
                let data = FuncCallData { name, arguments, namespace: None };
                Ok(AstNode::new(ExprFuncCall(data), self.line_char()))
            }
        } else {
            if accessors.is_some() {
                return Err("All object access must be contained inside an expression,\
                Ex: (<Object>:.field) not <Object>:.field".to_string());
            }
            Ok(AstNode::new(ExprLiteralCall(LiteralCallData { name, namespace: None }), self.line_char()))
        }
    }


    fn parse_accessors(&mut self) -> Result<LinkedList<Accessor>, String> {
        let mut accessors = LinkedList::<Accessor>::new();

        while matches!(self.peek().token_type, TSyntactic(Syn::DoubleColon) | TSyntactic(Syn::ColonDot)) {
            let is_field = match self.advance()?.token_type {
                TSyntactic(Syn::DoubleColon) => false,
                TSyntactic(Syn::ColonDot) => true,
                _ => { return Err(format!("Expected accessor pattern(:: | :.) found: {:?}", self.peek())); }
            };

            let name = if let Some(TokenData::String(id)) = &self.advance()?.data {
                *id
            } else { return Err(format!("Expected identifier for accessors, found {:?}", self.peek())); };

            let args = if let TSyntactic(Syn::LeftBracket) = self.peek().token_type {
                self.consume_left_bracket()?;
                let args = self.parse_method_args()?;
                self.consume_right_bracket()?;
                Some(args)
            } else {
                None
            };
            accessors.push_back(Accessor { name, is_field, args })
        }
        Ok(accessors)
    }


    fn parse_method_args(&mut self) -> Result<Vec<FuncArg>, String> {
        let mut args = Vec::<FuncArg>::with_capacity(4);


        while self.peek().token_type != TSyntactic(Syn::RightBracket) {
            let name = if let TLiteral(Lit::Identifier) = self.peek().token_type {
                if let Some(TokenData::String(name)) = &self.advance()?.data {
                    Some(*name)
                } else {
                    return Err("Expected value for identifier".to_string());
                }
            } else { None };

            let value = self.parse_data()?;
            args.push(FuncArg { value, name })
        }
        Ok(args)
    }


    pub fn parse_define(&mut self) -> Result<AstNode, String> {
        self.consume(TDefinition(Def::Define))?;


        let name = match &self.consume(TLiteral(Lit::Identifier))?.data {
            Some(TokenData::String(name)) => name.clone(),
            _ => return Err("Expected a string identifier".to_string())
        };

        let modifiers = self.parse_modifiers()?;
        let var_type = self.parse_type_if_exists(true)?;
        self.consume(TSyntactic(Syn::Equal))?;

        let definition = match self.peek().token_type {
            TSyntactic(Syn::LeftParen) => {
                if self.peek_n(2).token_type == TDefinition(Def::Lambda) {
                    if var_type != None {
                        self.push_warning("Type specifier is unused for function definition".to_string());
                    };

                    self.consume_left_paren()?;
                    let lambda = self.parse_lambda()?;
                    let typ = lambda.d_type.clone();
                    self.consume_right_paren()?;


                    let data = DefFuncData { name, lambda, d_type: typ };
                    AstNode::new(DefFunction(data), self.line_char())
                } else {
                    let value = self.parse_data()?;
                    let data = DefVarData { name, modifiers, value, d_type: var_type };
                    AstNode::new(DefVariable(data), self.line_char())
                }
            }

            TLiteral(_) => {
                let value = self.parse_literal()?;
                let data = DefVarData { name, modifiers, value, d_type: var_type };
                AstNode::new(DefVariable(data), self.line_char())
            }

            TSyntactic(Syn::SingleQuote) => {
                self.parse_quote()?
            }

            _ => return Err(format!("Invalid syntax in let, line: {}", self.peek().line))
        };
        Ok(definition)
    }


    pub fn parse_instance(&mut self, name: IString) -> Result<AstNode, String> {
        if self.previous_n(2).token_type == TSyntactic(Syn::LeftParen) {
            let arguments = self.parse_func_args()?;
            let data = FuncCallData { name, arguments, namespace: None };
            Ok(AstNode::new(ExprInitInst(data), self.line_char()))
        } else {
            self.consume(TSyntactic(Syn::LeftBracket))?; // post-fixed direct call, consume opening bracket
            let args = self.parse_named_args()?;
            self.consume(TSyntactic(Syn::RightBracket))?;

            let data = DirectInst { name, args, namespace: None };
            Ok(AstNode::new(ExprDirectInst(data), self.line_char()))
        }
    }


    pub fn parse_named_args(&mut self) -> Result<Option<Vec<InstArgs>>, String> {
        let mut args = Vec::<InstArgs>::new();

        while self.peek().token_type != TSyntactic(Syn::RightBracket) {
            self.consume(TSyntactic(Syn::Colon))?;

            let name = if let Some(TokenData::String(name))
                = &self.consume(TLiteral(Lit::Identifier))?.data {
                name.clone()
            } else { return Err("Expected name for instance call".to_string()); };

            let value = self.parse_data()?;
            args.push(InstArgs { name, value })
        }
        Ok(Some(args))
    }


    // FIXME
    pub fn parse_quote(&mut self) -> Result<AstNode, String> {
        self.consume(TSyntactic(Syn::SingleQuote))?;
        if self.peek().token_type == TSyntactic(Syn::LeftParen)
            && self.peek_n(2).token_type == TSyntactic(Syn::RightParen)
        {
            self.consume_left_paren()?;
            self.consume_right_paren()?;
            return Ok(AstNode::new(LitNil, self.line_char()));
        }
        Ok(AstNode::new(LitQuote, self.line_char()))
    }


    pub fn parse_struct_def(&mut self) -> Result<AstNode, String> {
        self.consume(TDefinition(Def::DefineStruct))?;

        let name = match &self.consume(TLiteral(Lit::Identifier))?.data {
            Some(TokenData::String(name)) => name.clone(),
            _ => return Err("Expected name for struct".to_string())
        };

        let fields = self.parse_fields()?;
        let data = DefStructData { name, fields };
        Ok(AstNode::new(DefStruct(data), self.line_char()))
    }


    pub fn parse_class_def(&mut self) -> Result<AstNode, String> {
        self.consume(TDefinition(Def::DefineClass))?;

        let name = match &self.consume(TLiteral(Lit::Identifier))?.data {
            Some(TokenData::String(name)) => name.clone(),
            _ => return Err("Expected name for class".to_string())
        };

        let mut class_data = DefClassData::empty_def(name);


        while self.peek().token_type != TSyntactic(Syn::RightParen) {
            self.consume_left_paren()?;
            self.consume(TSyntactic(Syn::Colon))?;

            match &self.peek().data {
                Some(TokenData::String(init)) if init == &util::SCACHE.const_init => {
                    self.advance()?;
                    let mut init_vec = Vec::<DefLambdaData>::with_capacity(4);

                    while self.peek().token_type != TSyntactic(Syn::RightParen) {
                        self.consume_left_paren()?;

                        match self.parse_lambda() {
                            Ok(init) => init_vec.push(init),
                            Err(e) => return Err(e),
                        }

                        self.consume_right_paren()?;
                    }
                    class_data.init = if init_vec.is_empty() { None } else { Some(init_vec) }
                }

                Some(TokenData::String(istring)) if istring == &util::SCACHE.const_func => {
                    self.advance()?;
                    let mut func_vec = Vec::<DefFuncData>::with_capacity(4);

                    while self.peek().token_type != TSyntactic(Syn::RightParen) {
                        self.consume_left_paren()?;

                        match self.parse_func(false) {
                            Ok(func) => func_vec.push(func),
                            Err(e) => return Err(e),
                        }

                        self.consume_right_paren()?;
                    }

                    class_data.methods = if func_vec.is_empty() { None } else { Some(func_vec) }
                }

                Some(TokenData::String(istring)) if istring == &util::SCACHE.const_param => {
                    self.advance()?;
                    class_data.params = self.parse_modifiers()?;
                }

                Some(TokenData::String(istring)) if istring == &util::SCACHE.const_var => {
                    self.advance()?;
                    class_data.fields = self.parse_fields()?
                }

                Some(TokenData::String(istring)) if istring == &util::SCACHE.const_pre => {
                    self.advance()?;
                    class_data.pre_init = self.parse_multi_expr()?;
                }

                Some(TokenData::String(istring))if istring == &util::SCACHE.const_post => {
                    self.advance()?;
                    class_data.post_init = self.parse_multi_expr()?;
                }

                Some(TokenData::String(istring))if istring == &util::SCACHE.const_final => {
                    self.advance()?;
                    class_data.fin = self.parse_multi_expr()?;
                }

                Some(TokenData::String(istring))if istring == &util::SCACHE.const_validate => {
                    self.advance()?;
                    class_data.validate = self.parse_multi_expr()?;
                }

                _ => { return Err(format!("Invalid token in class definition:  {:?}", &self.peek())); }
            }

            self.consume_right_paren()?;
        }
        Ok(AstNode::new(DefClass(class_data), self.line_char()))
    }


    pub fn parse_fields(&mut self) -> Result<Option<Vec<Field>>, String> {
        let mut fields = Vec::<Field>::new();

        while self.peek().token_type != TSyntactic(Syn::RightParen) {
            self.consume_left_paren()?;

            let name = match &self.consume(TLiteral(Lit::Identifier))?.data {
                Some(TokenData::String(name)) => name.clone(),
                _ => return Err("Expected name for struct".to_string())
            };

            let modifiers = if matches!(self.peek().token_type, TModifier(_)) {
                self.parse_modifiers()?
            } else { None };

            let p_type = self.parse_type_if_exists(true)?;

            let default_value: Option<AstNode> = if self.peek().token_type == TSyntactic(Syn::Bar) {
                self.advance()?;
                Some(self.parse_data()?)
            } else { None };

            self.consume_right_paren()?;
            fields.push(Field { name, modifiers, p_type, default_value })
        }
        if fields.is_empty() {
            Ok(None)
        } else { Ok(Some(fields)) }
    }


    pub fn parse_func(&mut self, is_defunc: bool) -> Result<DefFuncData, String> {
        if is_defunc { self.consume(TDefinition(Def::DefineFunc))?; }

        let name = match &self.consume(TLiteral(Lit::Identifier))?.data {
            Some(TokenData::String(name)) => name.clone(),
            _ => return Err("Expected name for function".to_string())
        };

        let d_type = if let Some(typ) = self.parse_type_if_exists(true)? {
            Some(typ)
        } else { return Err("Expected type for function definition".to_string()); };


        let lambda = self.parse_lambda()?;
        Ok(DefFuncData { name, d_type, lambda })
    }


    pub fn parse_lambda(&mut self) -> Result<DefLambdaData, String> {
        self.consume(TDefinition(Def::Lambda))?;

        let rtn_typ = self.parse_type_if_exists(true)?
            .unwrap_or_else(|| Type::Void);

        let parameters = self.parse_parameters()?;

        let body = match self.parse_multi_expr()? {
            None => { return Err("Expected body for lambda, found none".to_string()); }
            Some(s) => s
        };

        let param_types: Option<Vec<Type>> = parameters.as_ref().and_then(|params| {
            params.iter().map(|p| p.d_type.clone()).collect()
        });

        let full_type = param_types
            .map(|p_types|
                Type::Lambda(FuncType { rtn_type: Box::new(rtn_typ), param_types: p_types }));


        Ok(DefLambdaData { parameters, body, d_type: full_type })
    }


    pub fn parse_modifiers(&mut self) -> Result<Option<Vec<Mod>>, String> {
        if !matches!(self.peek().token_type, TModifier(_)) { return Ok(None); };

        let mut modifiers = Vec::<Mod>::new();
        while let TModifier(modifier) = &self.peek().token_type {
            modifiers.push(*modifier);
            self.advance()?;
        }

        if modifiers.is_empty() {
            Ok(None)
        } else { Ok(Some(modifiers)) }
    }


    pub fn parse_func_args(&mut self) -> Result<Option<Vec<FuncArg>>, String> {
        let mut func_args = Vec::<FuncArg>::new();
        let mut at_opt = false;

        let mut token = self.peek().token_type;
        while token != TSyntactic(Syn::RightParen) {
            let peek = self.peek().token_type;

            if peek == TSyntactic(Syn::Colon) {
                self.advance()?; // consume colon
                at_opt = true;

                let token_data = self.consume(TLiteral(Lit::Identifier))?;

                let name = match token_data.data {
                    Some(TokenData::String(ref s)) => *s,
                    _ => return Err("Expected a string identifier".to_string())
                };

                let value = self.parse_data()?;

                func_args.push(FuncArg { name: Some(name.clone()), value });
            } else {
                if at_opt {
                    return Err("All args after named arg, must also be named".to_string());
                }
                let value = self.parse_data()?;
                func_args.push(FuncArg { name: None, value });
            }
            token = self.peek().token_type;
        }
        Ok(if func_args.is_empty() { None } else { Some(func_args) })
    }


    pub fn parse_parameters(&mut self) -> Result<Option<Vec<Param>>, String> {
        self.consume_left_paren()?;

        if self.peek().token_type == TSyntactic(Syn::RightParen) {
            self.consume_right_paren()?;
            return Ok(None);
        }

        let mut params = Vec::<Param>::new();

        while self.peek().token_type != TSyntactic(Syn::RightParen) {
            let modifiers = self.parse_modifiers()?;

            let name = match &self.consume(TLiteral(Lit::Identifier))?.data {
                Some(TokenData::String(s)) => s.clone(),
                _ => return Err("Expected a variable identifier".to_string())
            };

            let d_type = if let Some(typ) = self.parse_type_if_exists(true)? {
                Some(typ)
            } else { return Err("Expected type for parameter".to_string()); };

            params.push(Param {
                name,
                d_type,
                modifiers,
            });
        }

        self.consume_right_paren()?;
        Ok(Some(params))
    }
}


pub fn process(tokens: Vec<Token>) -> Result<ParseResult, String> {
    let mut state = ParserState::new(tokens);
    state.process()
}
