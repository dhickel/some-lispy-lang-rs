use std::collections::LinkedList;
use std::process::id;
use ahash::AHashMap;
use intmap::IntMap;
use lang::types::Type;
use crate::ast::*;
use crate::ast::AstNode::*;
use crate::code_gen::GenData;
use crate::environment::{Binding, Environment};
use crate::op_codes::OpCode;
use crate::token::*;
use crate::token::TokenType::*;
use crate::util;
use crate::util::SCACHE;


struct ParserState {
    pub tokens: Vec<Token>,
    pub current: usize,
    pub end: usize,
    pub depth: i32,
    pub globals: IntMap<Binding>,
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
            globals: IntMap::<Binding>::with_capacity(50),
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

        if matches!(self.peek().token_type, TLexical(Lex::LeftParen) | TLexical(Lex::RightParen)) {
            // panic!("Parenthesis should  be advanced via consume paren function");
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

        if matches!(self.peek().token_type, TLexical(Lex::LeftParen) | TLexical(Lex::RightParen)) {
            //panic!("Parenthesis should only be advanced via consume paren function");
            return Err("Parenthesis should only be advanced via consume paren function".to_string());
        }
        if !self.check(&token_type) {
            // panic!("Expected: {:?}, Found: {:?}", token_type, self.peek());
            return Err(format!("Expected: {:?}, Found: {:?}", token_type, self.peek()));
        }
        self.advance()
    }


    pub fn consume_left_paren(&mut self) -> Result<(), String> {
        if !self.check(&TLexical(Lex::LeftParen)) {
            //  panic!("Expected: Left Paren, Found: {:?}", self.peek());
            return Err(format!("Expected: Left Paren, Found: {:?}", self.peek()));
        }
        self.current += 1;
        self.depth += 1;
        Ok(())
    }


    pub fn consume_right_paren(&mut self) -> Result<(), String> {
        if !self.check(&TLexical(Lex::RightParen)) {
            //  panic!("Expected: Right Paren, Found: {:?}", self.peek());
            return Err(format!("Expected: Right Paren, Found: {:?}", self.peek()));
        }
        self.current += 1;
        self.depth -= 1;
        Ok(())
    }

    pub fn consume_left_bracket(&mut self) -> Result<(), String> {
        if !self.check(&TLexical(Lex::LeftBracket)) {
            // panic!("Expected: Left Bracket, Found: {:?}", self.peek());
            return Err(format!("Expected: Left Bracket, Found: {:?}", self.peek()));
        }
        self.current += 1;
        Ok(())
    }


    pub fn consume_right_bracket(&mut self) -> Result<(), String> {
        if !self.check(&TLexical(Lex::RightBracket)) {
            //   panic!("Expected: Right Bracket, Found: {:?}", self.peek());
            return Err(format!("Expected: Right Bracket, Found: {:?}", self.peek()));
        }
        self.current += 1;
        Ok(())
    }


    pub fn consume_left_brace(&mut self) -> Result<(), String> {
        if !self.check(&TLexical(Lex::LeftBrace)) {
            // panic!("Expected: Left Bracket, Found: {:?}", self.peek());
            return Err(format!("Expected: Left Brace, Found: {:?}", self.peek()));
        }
        self.current += 1;
        Ok(())
    }


    pub fn consume_right_brace(&mut self) -> Result<(), String> {
        if !self.check(&TLexical(Lex::RightBrace)) {
            //   panic!("Expected: Right Bracket, Found: {:?}", self.peek());
            return Err(format!("Expected: Right Brace, Found: {:?}", self.peek()));
        }
        self.current += 1;
        Ok(())
    }


    pub fn parse_expr_data(&mut self) -> Result<AstNode, String> {
        match &self.peek().token_type {
            TLexical(Lex::LeftParen) => {
                self.parse_s_expr()
            }

            TLexical(Lex::RightParen) => {
                // Needed, this function is entered by parse_s_expr, but could be null list lit
                if matches!(self.previous().token_type, TLexical(Lex::LeftParen)) {
                    Ok(LitNil)
                } else {
                    Err(format!("Right paren with no matching open, line: {}", self.peek().line))
                }
            }

            TOperation(_) => {
                self.parse_operation()
            }

            TLiteral(_) => {
                self.parse_literal()
            }

            TLexical(Lex::SingleQuote) => {
                self.parse_quote()
            }

            TExpression(_) => {
                self.parse_exact_expr()
            }

            TDefinition(Def::Define) => {
                self.parse_define()
            }

            TDefinition(Def::DefineFunc) => {
                let data = self.parse_func(true)?;
                Ok(DefFunction(Box::new(data)))
            }

            TDefinition(Def::Lambda) => {
                let data = self.parse_lambda(false, false)?;
                Ok(DefLambda(Box::new(data)))
            }

            TDefinition(Def::DefineStruct) => {
                self.parse_struct_def()
            }

            TDefinition(Def::DefineClass) => {
                self.parse_class_def()
            }

            TSyntactic(_) => {// TODO implement quote
                Err(format!("Unexpected token: {:?}", &self.peek()))
            }

            _ => {
                Err(format!("Unexpected token: {:?}", &self.peek()))
            }
        }
    }


    pub fn parse_s_expr(&mut self) -> Result<AstNode, String> {
        self.consume_left_paren()?;
        let expression = self.parse_expr_data()?;

        // Handle edge case where first expr in an s-expr is something that evals to a lambda call
        if self.peek().token_type != TLexical(Lex::RightParen) {
            let args = self.parse_func_args()?;
            self.consume_right_paren()?;

            let data = InnerFuncCallData {
                expr: expression,
                accessors: None,
                arguments: args,
                ctx: None,
            };
            return Ok(ExprFuncCalInner(Box::new(data)));
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
        while self.have_next() && self.peek().token_type != TLexical(Lex::RightParen) {
            operands.push(self.parse_expr_data()?);
        }

        let data = OpData { operation, operands, typ: Type::Unresolved };
        Ok(Operation(data))
    }


    pub fn parse_literal(&mut self) -> Result<AstNode, String> {
        let token = self.advance().unwrap().token_type;
        let data = &self.previous().data;

        match token {
            TLiteral(lit) => {
                match lit {
                    Lit::True => {
                        Ok(LitBoolean(true))
                    }

                    Lit::False => {
                        Ok(LitBoolean(false))
                    }

                    Lit::String => {
                        if let Some(TokenData::String(value)) = data {
                            let string = util::SCACHE.resolve(*value).to_string();
                            Ok(LitString(string))
                        } else { Err(format!("Invalid data for string literal: {:?}", &self.peek().data)) }
                    }

                    Lit::Int => {
                        if let Some(TokenData::Integer(value)) = data {
                            Ok(LitInteger(*value))
                        } else { Err(format!("Invalid data for integer literal: {:?}", &self.peek().data)) }
                    }

                    Lit::Float => {
                        if let Some(TokenData::Float(value)) = data {
                            Ok(LitFloat(*value))
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

                    Lit::Nil => Ok(LitNil),
                }
            }
            _ => Err(format!("Expected literal value found: {:?}", token.clone()))
        }
    }


    pub fn parse_exact_expr(&mut self) -> Result<AstNode, String> {
        let expression = self.advance()?;
        match expression.token_type {
            TExpression(Expr::Assign) => {
                self.parse_assign()
            }

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
                Ok(ExprListAccess(Box::new(data)))
            }

            TExpression(Expr::Cdr) => {
                let data = ListAccData {
                    index_expr: None,
                    pattern: Some(vec![1]),
                    list: self.parse_list_head()?,
                };
                Ok(ExprListAccess(Box::new(data)))
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
        let lower = self.parse_expr_data()?;
        let upper = self.parse_expr_data()?;
        let data = GenRandData {
            is_float,
            upper,
            lower,
        };
        Ok(ExprGenRand(Box::new(data)))
    }


    pub fn parse_assign(&mut self) -> Result<AstNode, String> {
        match self.peek().token_type {
            TLiteral(Lit::Identifier) => {
                let name = match &self.consume(TLiteral(Lit::Identifier))?.data {
                    Some(TokenData::String(name)) => name.clone(),
                    _ => return Err("Expected a string identifier".to_string())
                };
                let value = self.parse_expr_data()?;
                let data = AssignData { name, value, ctx: None };
                Ok(ExprAssignment(Box::new(data)))
            }

            TLexical(Lex::LeftParen) => {
                self.consume_left_paren()?;
                let token = self.advance()?;

                let name = if let (
                    TLiteral(Lit::Identifier), Some(TokenData::String(s))
                ) = (&token.token_type, &token.data) {
                    *s
                } else { return Err(format!("Expected object name to assign to, found: {:?}", token)); };

                let accessors = self.parse_accessors()?;
                self.consume_right_paren()?;
                let value = self.parse_expr_data()?;

                let access = ObjectCallData { name, accessors, ctx: None };
                let assign_data = ObjectAssignData { access, value, ctx: None };
                Ok(ExprObjectAssignment(Box::new(assign_data)))
            }

            _ => Err(format!(
                "Expected identifier or object access to assign to, found: {:?}"
                , self.peek().token_type)
            )
        }
    }


    pub fn parse_if(&mut self) -> Result<AstNode, String> {
        let condition = self.parse_expr_data()?;
        let then = self.parse_expr_data()?;
        let if_branch = CondBranch { cond_node: condition, then_node: then, typ: Type::Unresolved };

        let else_branch = if self.peek().token_type != TLexical(Lex::RightParen) {
            Some(self.parse_expr_data()?)
        } else { None };

        let data = IfData { if_branch, else_branch, else_type: Type::Unresolved };
        Ok(ExprIf(Box::new(data)))
    }


    pub fn parse_cond(&mut self) -> Result<AstNode, String> {
        let mut cond_branches = Vec::<CondBranch>::new();
        let mut else_branch = None;

        while self.peek().token_type != TLexical(Lex::RightParen) {
            if self.peek_n(2).token_type == TSyntactic(Syn::Else) {
                self.consume_left_paren()?;

                self.consume(TSyntactic(Syn::Else))?;
                else_branch = Some(self.parse_expr_data()?);

                self.consume_right_paren()?;
                break;
            }
            cond_branches.push(self.parse_cond_branch()?)
        }

        if cond_branches.is_empty() {
            Err(format!("Cond expression must have at least one branch, line: {}", self.peek().line))
        } else {
            let data = CondData { cond_branches, else_branch, else_type: Type::Unresolved };
            Ok(ExprCond(Box::new(data)))
        }
    }


    pub fn parse_cond_branch(&mut self) -> Result<CondBranch, String> {
        self.consume_left_paren()?;

        let cond_node = self.parse_expr_data()?;
        let then_node = self.parse_expr_data()?;

        self.consume_right_paren()?;
        Ok(CondBranch { cond_node, then_node, typ: Type::Unresolved })
    }


    pub fn parse_multi_expr(&mut self) -> Result<Option<AstNode>, String> {
        let mut expressions = Vec::<AstNode>::new();
        while self.peek().token_type != TLexical(Lex::RightParen) {
            expressions.push(self.parse_expr_data()?);
        }

        if expressions.is_empty() {
            Ok(None)
        } else if expressions.len() == 1 {
            Ok(Some(expressions.pop().unwrap()))
        } else {
            let data = MultiExprData { expressions, typ: Type::Unresolved };
            Ok(Some(ExprMulti(data)))
        }
    }


    pub fn parse_print(&mut self) -> Result<AstNode, String> {
        let expr = self.parse_expr_data()?;
        Ok(ExprPrint(Box::new(expr)))
    }


    pub fn parse_list(&mut self) -> Result<AstNode, String> {
        let mut elements = Vec::<AstNode>::new();
        while self.peek().token_type != TLexical(Lex::RightParen) {
            elements.push(self.parse_expr_data()?);
        }

        if elements.is_empty() {
            Ok(LitNil)
        } else {
            let data = OpData { operation: Op::List, operands: elements, typ: Type::Unresolved };
            Ok(ExprPairList(data))
        }
    }
    
    pub fn parse_array(&mut self) -> Result<AstNode, String> {
        let mut elements = Vec::<AstNode>::new();
        while self.peek().token_type != TLexical(Lex::RightParen) {
            elements.push(self.parse_expr_data()?);
        }

        if elements.is_empty() {
            Ok(LitNil)
        } else {
            let data = OpData { operation: Op::List, operands: elements, typ: Type::Unresolved };
            Ok(ExprArray(data))
        }
    }


    pub fn parse_list_head(&mut self) -> Result<AstNode, String> {
        let head = self.parse_expr_data()?;
        if self.peek().token_type != TLexical(Lex::RightParen) {
            Err(format!("Invalid argument count, line: {}", self.peek().line))
        } else {
            Ok(head)
        }
    }


    pub fn parse_list_access(&mut self) -> Result<AstNode, String> {
        if self.peek().token_type == TLexical(Lex::SingleQuote) {
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
                Ok(ExprListAccess(Box::new(data)))
            } else {
                Err("Expected fr... access pattern".to_string())
            }
        } else {
            let index = self.parse_expr_data()?;
            let list = self.parse_list_head()?;
            let data = ListAccData {
                index_expr: Some(index),
                pattern: None,
                list,
            };
            Ok(ExprListAccess(Box::new(data)))
        }
    }


    pub fn parse_while(&mut self) -> Result<AstNode, String> {
        let is_do = if self.peek().token_type == TModifier(Mod::Do) {
            self.advance()?;
            true
        } else { false };

        let condition = self.parse_expr_data()?;
        let body = match self.parse_multi_expr()? {
            None => { return Err("Expected body for while loop, found none".to_string()); }
            Some(s) => s
        };

        let data = WhileData { condition, body, is_do };
        Ok(ExprWhileLoop(Box::new(data)))
    }


    pub fn parse_cons(&mut self) -> Result<AstNode, String> {
        let car = self.parse_expr_data()?;
        let cdr = self.parse_expr_data()?;

        if self.peek().token_type != TLexical(Lex::RightParen) {
            Err("Cons expression may only have 2 arguments".to_string())
        } else {
            let data = ConsData { car, cdr };
            Ok(ExprCons(Box::new(data)))
        }
    }


    pub fn parse_type_if_exists(&mut self) -> Result<Option<u64>, String> {
        let typ = if let TSyntactic(Syn::DoubleColon) = &self.peek().token_type {
            self.advance()?;
            let next = self.advance()?;

            if let (TLiteral(Lit::Identifier), Some(TokenData::String(s))) = (&next.token_type, &next.data) {
                Some(*s)
            } else { return Err(format!("Expected type identifier, found: {:?}", next)); }
        } else { None };

        //if typ.is_some() { self.advance()?; } FIXME? Remember to comment why you comment out ...
        Ok(typ)
    }


    pub fn parse_identifier(&mut self, identifier: u64) -> Result<AstNode, String> {
        let name = identifier;
        let mut accessors = None;

        let is_func_call = self.previous_n(2).token_type == TLexical(Lex::LeftParen);

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
                let data = ObjectCallData { name, accessors, ctx: None };
                Ok(ExprObjectCall(Box::new(data)))
            } else {
                let data = FuncCallData { name, arguments, ctx: None };
                Ok(ExprFuncCall(Box::new(data)))
            }
        } else {
            if accessors.is_some() {
                return Err("All object access must be contained inside an expression,\
                Ex: (<Object>:.field) not <Object>:.field".to_string());
            }
            Ok(ExprLiteralCall(LiteralCallData { name, ctx: None }))
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

            let args = if let TLexical(Lex::LeftBracket) = self.peek().token_type {
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


        while self.peek().token_type != TLexical(Lex::RightBracket) {
            let name = if let TLiteral(Lit::Identifier) = self.peek().token_type {
                if let Some(TokenData::String(name)) = &self.advance()?.data {
                    Some(*name)
                } else {
                    return Err("Expected value for identifier".to_string());
                }
            } else { None };

            let value = self.parse_expr_data()?;
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
        let var_type = self.parse_type_if_exists()?;

        let definition = match self.peek().token_type {
            TLexical(Lex::LeftParen) => {
                if self.peek_n(2).token_type == TDefinition(Def::Lambda) {
                    if var_type != None {
                        self.push_warning(format!(
                            "Type specifier: {}, is unused for function definition, line: {}",
                            util::SCACHE.resolve(var_type.unwrap()), self.peek().line))
                    }

                    self.consume_left_paren()?;
                    let lambda = self.parse_lambda(false, false)?;
                    self.consume_right_paren()?;

                    let data = DefFuncData { name, lambda };
                    DefFunction(Box::new(data))
                } else {
                    let value = self.parse_expr_data()?;
                    let data = DefVarData { name, modifiers, value, d_type: var_type, ctx: None };
                    DefVariable(Box::new(data))
                }
            }

            TLiteral(_) => {
                let value = self.parse_literal()?;
                let data = DefVarData { name, modifiers, value, d_type: var_type, ctx: None };
                DefVariable(Box::new(data))
            }

            TLexical(Lex::SingleQuote) => {
                self.parse_quote()?
            }

            _ => return Err(format!("Invalid syntax in define, line: {}", self.peek().line))
        };
        Ok(definition)
    }


    pub fn parse_instance(&mut self, name: u64) -> Result<AstNode, String> {
        if self.previous_n(2).token_type == TLexical(Lex::LeftParen) {
            let arguments = self.parse_func_args()?;
            let data = FuncCallData { name, arguments, ctx: None };
            Ok(ExprInitInst(Box::new(data)))
        } else {
            self.consume(TLexical(Lex::LeftBracket))?; // post-fixed direct call, consume opening bracket
            let args = self.parse_named_args()?;
            self.consume(TLexical(Lex::RightBracket))?;

            let data = DirectInst { name, args, ctx: None };
            Ok(ExprDirectInst(Box::new(data)))
        }
    }


    pub fn parse_named_args(&mut self) -> Result<Option<Vec<InstArgs>>, String> {
        let mut args = Vec::<InstArgs>::new();

        while self.peek().token_type != TLexical(Lex::RightBracket) {
            self.consume(TSyntactic(Syn::Colon))?;

            let name = if let Some(TokenData::String(name))
                = &self.consume(TLiteral(Lit::Identifier))?.data {
                name.clone()
            } else { return Err("Expected name for instance call".to_string()); };

            let value = self.parse_expr_data()?;
            args.push(InstArgs { name, value })
        }
        Ok(Some(args))
    }

    // FIXME
    pub fn parse_quote(&mut self) -> Result<AstNode, String> {
        self.consume(TLexical(Lex::SingleQuote))?;
        if self.peek().token_type == TLexical(Lex::LeftParen)
            && self.peek_n(2).token_type == TLexical(Lex::RightParen)
        {
            self.consume_left_paren()?;
            self.consume_right_paren()?;
            return Ok(LitNil);
        }
        Ok(LitQuote)
    }


    pub fn parse_struct_def(&mut self) -> Result<AstNode, String> {
        self.consume(TDefinition(Def::DefineStruct))?;

        let name = match &self.consume(TLiteral(Lit::Identifier))?.data {
            Some(TokenData::String(name)) => name.clone(),
            _ => return Err("Expected name for struct".to_string())
        };

        let fields = self.parse_fields()?;
        let data = DefStructData { name, fields, ctx: None };
        Ok(DefStruct(Box::new(data)))
    }


    pub fn parse_class_def(&mut self) -> Result<AstNode, String> {
        self.consume(TDefinition(Def::DefineClass))?;

        let name = match &self.consume(TLiteral(Lit::Identifier))?.data {
            Some(TokenData::String(name)) => name.clone(),
            _ => return Err("Expected name for class".to_string())
        };

        let mut class_data = DefClassData::empty_def(name);


        while self.peek().token_type != TLexical(Lex::RightParen) {
            self.consume_left_paren()?;
            self.consume(TSyntactic(Syn::Colon))?;

            match &self.peek().data {
                Some(TokenData::String(u64)) if u64 == &util::SCACHE.const_init => {
                    self.advance()?;
                    let mut init_vec = Vec::<DefLambdaData>::with_capacity(4);

                    while self.peek().token_type != TLexical(Lex::RightParen) {
                        self.consume_left_paren()?;

                        match self.parse_lambda(true, true) {
                            Ok(init) => init_vec.push(init),
                            Err(e) => return Err(e),
                        }

                        self.consume_right_paren()?;
                    }
                    class_data.init = if init_vec.is_empty() { None } else { Some(init_vec) }
                }

                Some(TokenData::String(u64)) if u64 == &util::SCACHE.const_func => {
                    self.advance()?;
                    let mut func_vec = Vec::<DefFuncData>::with_capacity(4);

                    while self.peek().token_type != TLexical(Lex::RightParen) {
                        self.consume_left_paren()?;

                        match self.parse_func(false) {
                            Ok(func) => func_vec.push(func),
                            Err(e) => return Err(e),
                        }

                        self.consume_right_paren()?;
                    }

                    class_data.methods = if func_vec.is_empty() { None } else { Some(func_vec) }
                }

                Some(TokenData::String(u64)) if u64 == &util::SCACHE.const_param => {
                    self.advance()?;
                    class_data.params = self.parse_modifiers()?;
                }

                Some(TokenData::String(u64)) if u64 == &util::SCACHE.const_var => {
                    self.advance()?;
                    class_data.fields = self.parse_fields()?
                }

                Some(TokenData::String(u64)) if u64 == &util::SCACHE.const_pre => {
                    self.advance()?;
                    class_data.pre_init = self.parse_multi_expr()?;
                }

                Some(TokenData::String(u64))if u64 == &util::SCACHE.const_post => {
                    self.advance()?;
                    class_data.post_init = self.parse_multi_expr()?;
                }

                Some(TokenData::String(u64))if u64 == &util::SCACHE.const_final => {
                    self.advance()?;
                    class_data.fin = self.parse_multi_expr()?;
                }

                Some(TokenData::String(u64))if u64 == &util::SCACHE.const_validate => {
                    self.advance()?;
                    class_data.validate = self.parse_multi_expr()?;
                }

                _ => { return Err(format!("Invalid token in class definition:  {:?}", &self.peek())); }
            }

            self.consume_right_paren()?;
        }
        Ok(DefClass(Box::new(class_data)))
    }


    pub fn parse_fields(&mut self) -> Result<Option<Vec<Field>>, String> {
        let mut fields = Vec::<Field>::new();

        while self.peek().token_type != TLexical(Lex::RightParen) {
            self.consume_left_paren()?;

            let name = match &self.consume(TLiteral(Lit::Identifier))?.data {
                Some(TokenData::String(name)) => name.clone(),
                _ => return Err("Expected name for struct".to_string())
            };

            let modifiers = if matches!(self.peek().token_type, TModifier(_)) {
                self.parse_modifiers()?
            } else { None };

            let p_type = self.parse_type_if_exists()?;

            let default_value: Option<AstNode> = if self.peek().token_type == TSyntactic(Syn::Bar) {
                self.advance()?;
                Some(self.parse_expr_data()?)
            } else { None };

            self.consume_right_paren()?;
            fields.push(Field { name, modifiers, p_type, default_value, c_type: Type::Unresolved })
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

        let lambda = self.parse_lambda(true, true)?;
        Ok(DefFuncData { name, lambda })
    }


    pub fn parse_lambda(&mut self, is_defunc: bool, bracketed: bool) -> Result<DefLambdaData, String> {
        if !is_defunc { self.consume(TDefinition(Def::Lambda))?; }

        let modifiers = if matches!(self.peek().token_type, TModifier(_)) {
            self.parse_modifiers()?
        } else { None };

        let parameters = self.parse_parameters(bracketed)?;

        let body = match self.parse_multi_expr()? {
            None => { return Err("Expected body for lambda, found none".to_string()); }
            Some(s) => s
        };

        let rtn_type = self.parse_type_if_exists()?;
        Ok(DefLambdaData { modifiers, parameters, body, d_type: rtn_type, typ: Type::Unresolved, ctx: None })
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
        while token != TLexical(Lex::RightParen) {
            let peek = self.peek().token_type;

            if peek == TSyntactic(Syn::Colon) {
                self.advance()?; // consume colon
                at_opt = true;

                let token_data = self.consume(TLiteral(Lit::Identifier))?;

                let name = match token_data.data {
                    Some(TokenData::String(ref s)) => *s,
                    _ => return Err("Expected a string identifier".to_string())
                };

                let value = self.parse_expr_data()?;

                func_args.push(FuncArg { name: Some(name.clone()), value });
            } else {
                if at_opt {
                    return Err("All args after named arg, must also be named".to_string());
                }
                let value = self.parse_expr_data()?;
                func_args.push(FuncArg { name: None, value });
            }
            token = self.peek().token_type;
        }
        Ok(if func_args.is_empty() { None } else { Some(func_args) })
    }


    pub fn parse_parameters(&mut self, bracketed: bool) -> Result<Option<Vec<Param>>, String> {
        if bracketed {
            self.consume_left_bracket()?;
            if self.peek().token_type == TLexical(Lex::RightBracket) {
                self.consume_right_bracket()?;
                return Ok(None);
            }
        } else {
            self.consume_left_paren()?;
            if self.peek().token_type == TLexical(Lex::RightParen) {
                self.consume_right_paren()?;
                return Ok(None);
            }
        }

        let mut optional = false;
        let mut params = Vec::<Param>::new();

        while self.peek().token_type != if bracketed { TLexical(Lex::RightBracket) } else { TLexical(Lex::RightParen) } {
            let mut dynamic = false;
            let mut mutable = false;

            let modifiers = self.parse_modifiers()?;
            for modifier in modifiers.unwrap_or_default() {
                match modifier {
                    Mod::Mutable => mutable = true,
                    Mod::Dynamic => dynamic = true,
                    Mod::Optional => if !optional { optional = true },
                    _ => {}
                };
            };

            let name = match &self.consume(TLiteral(Lit::Identifier))?.data {
                Some(TokenData::String(s)) => s.clone(),
                _ => return Err("Expected a variable identifier".to_string())
            };

            let default_value: Option<AstNode> = if optional {
                if self.peek().token_type != TSyntactic(Syn::Equal) {
                    return Err(format!(
                        "All parameters after &opt modifier need default assignments, line: {}",
                        self.peek().line)
                    );
                };
                self.advance()?; // consume =
                Some(self.parse_literal()?)
            } else { None };

            let p_type = self.parse_type_if_exists()?;
            params.push(Param {
                name,
                d_type: p_type,
                optional,
                default_value,
                dynamic,
                mutable,
                c_type: Type::Unresolved,
            });
        }
        if bracketed { self.consume_right_bracket()?; } else { self.consume_right_paren()?; }
        Ok(Some(params))
    }
}


pub fn process(tokens: Vec<Token>) -> Result<Vec<AstNode>, String> {
    let mut state = ParserState::new(tokens);
    state.process()
}
