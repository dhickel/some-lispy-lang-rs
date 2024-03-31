use std::collections::LinkedList;
use std::rc::Rc;
use lasso::{Rodeo, Spur};
use crate::parse::{Def, Expr, Lex, Lit, Mod, Op, Syn, Token, TokenData, TokenType, util};
use crate::parse::ast_nodes::{Accessor, AssignData, CondBranch, CondData, ConsData, DefClassData, DefFuncData, DefLambdaData, DefNode, DefStructData, DirectInst, DefVarData, ExprFuncCallData, ExprNode, Field, FuncArg, FuncCallData, IfData, InstArgs, ListAccData, LitNode, ObjectAssignData, ObjectCallData, OpNode, Param, WhileData, AstNode};
use crate::parse::ast_nodes::AstNode::{DefinitionNode, ExpressionNode, OperationNode};
use crate::parse::ast_nodes::DefNode::ClassDef;
use crate::parse::ast_nodes::ExprNode::{Assignment, CondExpr, ConsExpr, ExprFuncCal, FuncCall, GenRand, IfExpr, ListAccess, LiteralCall, MultiExpr, ObjectAssignment, ObjectCall, PairList, PrintExpr, WhileLoop};
use crate::parse::Def::{DefineClass, DefineStruct};
use crate::parse::Lex::{LeftParen, RightBracket, RightParen};


use crate::parse::TokenType::{TDefinition, TExpression, TLexical, TLiteral, TOperation, TSyntactic, TModifier};



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

        if matches!(self.peek().token_type, TLexical(LeftParen) | TLexical(RightParen)) {
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

        if matches!(self.peek().token_type, TLexical(LeftParen) | TLexical(RightParen)) {
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
            TLexical(LeftParen) => self.parse_s_expr(),
            TLexical(RightParen) => { 
                // Needed, this function is entered by parse_s_expr, but could be null list lit
                if matches!(self.previous().token_type, TLexical(Lex::LeftParen)) {
                    Ok(AstNode::new_nil_lit())
                } else { Err(format!("Right paren with no matching open, line: {}", self.peek().line)) }
            }
            TOperation(_) => self.parse_operation(),
            TLiteral(_) => self.parse_literal(),
            TLexical(Lex::SingleQuote) => self.parse_quote(),
            TExpression(_) => self.parse_exact_expr(),
            TDefinition(Def::Define) => self.parse_define(),
            TDefinition(Def::DefineFunc) => {
                Ok(DefinitionNode(Rc::new(DefNode::Function(self.parse_func(true)?))))
            }
            TDefinition(Def::Lambda) => {
                Ok(DefinitionNode(Rc::new(DefNode::Lambda(Rc::new(self.parse_lambda(false, false)?)))))
            }
            TDefinition(DefineStruct) => self.parse_struct_def(),
            TDefinition(DefineClass) => self.parse_class_def(),

            TSyntactic(_) => Err(format!("Unexpected token: {:?}", &self.peek())), // TODO implement quote
            _ =>  Err(format!("Unexpected token: {:?}", &self.peek()))
        }
    }


    pub fn parse_s_expr(&mut self) -> Result<AstNode, String> {
        self.consume_left_paren()?;
        let expression = self.parse_expr_data()?;

        // Handle edge case where first expr in an s-expr is something that evals to a lambda call
        if self.peek().token_type != TLexical(Lex::RightParen) {
            let args = self.parse_func_args()?;
            self.consume_right_paren()?;
            return Ok(ExpressionNode(Rc::new(ExprFuncCal(ExprFuncCallData {
                expr: expression,
                accessors: None,
                arguments: args,
            }))));
        }
        
        self.consume_right_paren()?;
        Ok(expression)
    }


    pub fn parse_operation(&mut self) -> Result<AstNode, String> {
        let operation = &self.peek().token_type.clone();
        self.advance()?;

        let mut operands = Vec::<AstNode>::new();
        while self.have_next() && self.peek().token_type != TLexical(Lex::RightParen) {
            operands.push(self.parse_expr_data()?);
        }


        match operation {
            TOperation(Op::And) => Ok(OperationNode(OpNode::And(operands))),
            TOperation(Op::Or) => Ok(OperationNode(OpNode::Or(operands))),
            TOperation(Op::Nor) => Ok(OperationNode(OpNode::Nor(operands))),
            TOperation(Op::Xor) => Ok(OperationNode(OpNode::Xor(operands))),
            TOperation(Op::Xnor) => Ok(OperationNode(OpNode::Xnor(operands))),
            TOperation(Op::Nand) => Ok(OperationNode(OpNode::Nand(operands))),
            TOperation(Op::Negate) => Ok(OperationNode(OpNode::Negate(operands))),
            TOperation(Op::Plus) => Ok(OperationNode(OpNode::Addition(operands))),
            TOperation(Op::Minus) => Ok(OperationNode(OpNode::Subtraction(operands))),
            TOperation(Op::Asterisk) => Ok(OperationNode(OpNode::Multiplication(operands))),
            TOperation(Op::Slash) => Ok(OperationNode(OpNode::Division(operands))),
            TOperation(Op::Caret) => Ok(OperationNode(OpNode::Exponentiate(operands))),
            TOperation(Op::Percent) => Ok(OperationNode(OpNode::Modulo(operands))),
            TOperation(Op::PlusPlus) => Ok(OperationNode(OpNode::Increment(operands))),
            TOperation(Op::MinusMinus) => Ok(OperationNode(OpNode::Decrement(operands))),
            TOperation(Op::Greater) => Ok(OperationNode(OpNode::GreaterThan(operands))),
            TOperation(Op::Less) => Ok(OperationNode(OpNode::LessThan(operands))),
            TOperation(Op::GreaterEqual) => Ok(OperationNode(OpNode::GreaterThanEqual(operands))),
            TOperation(Op::LessEqual) => Ok(OperationNode(OpNode::LessThanEqual(operands))),
            TOperation(Op::Equals) => Ok(OperationNode(OpNode::Equality(operands))),
            TOperation(Op::BangEquals) => Ok(OperationNode(OpNode::RefNonEquality(operands))),
            TOperation(Op::RefEqual) => Ok(OperationNode(OpNode::RefEquality(operands))),
            _ => Err(format!("Fatal, Expected Operation received {:?}", operation))
        }
    }


    pub fn parse_literal(&mut self) -> Result<AstNode, String> {
        let token = self.advance().unwrap().token_type;
        let data = &self.previous().data;

        match token {
            TLiteral(lit) => {
                match lit {
                    Lit::True => Ok(AstNode::new_bool_lit(true)),
                    Lit::False => Ok(AstNode::new_bool_lit(false)),
                    Lit::String => {
                        if let Some(TokenData::String(value)) = data {
                            let string = util::SCACHE.resolve(value).to_string();
                            Ok(AstNode::new_string_lit(string))
                        } else { Err(format!("Invalid data for string literal: {:?}", &self.peek().data)) }
                    }
                    Lit::Int => {
                        if let Some(TokenData::Integer(value)) = data {
                            Ok(AstNode::new_int_lit(*value))
                        } else { Err(format!("Invalid data for integer literal: {:?}", &self.peek().data)) }
                    }
                    Lit::Float => {
                        if let Some(TokenData::Float(value)) = data {
                            Ok(AstNode::new_float_lit(*value))
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
                    Lit::Nil => Ok(AstNode::new_nil_lit()),
                }
            }
            _ => Err(format!("Expected literal value found: {:?}", token.clone()))
        }
    }


    pub fn parse_exact_expr(&mut self) -> Result<AstNode, String> {
        let expression = self.advance()?;
        match expression.token_type {
            TExpression(Expr::Assign) => self.parse_assign(),
            TExpression(Expr::If) => self.parse_if(),
            TExpression(Expr::Cond) => self.parse_cond(),
            TExpression(Expr::Begin) => match self.parse_multi_expr()? {
                None => { Err("Expected body for begin, found none".to_string()) }
                Some(s) => Ok(s)
            },
            TExpression(Expr::Print) => self.parse_print(),
            TExpression(Expr::List) => self.parse_list(),
            TExpression(Expr::Lacc) => self.parse_list_access(),
            TExpression(Expr::While) => self.parse_while(),
            TExpression(Expr::Cons) => self.parse_cons(),
            TExpression(Expr::Car) => {
                Ok(ExpressionNode(Rc::new(ListAccess(
                    ListAccData {
                        index_expr: None,
                        pattern: Some(util::SCACHE.intern("f")),
                        list: self.parse_list_head()?,
                    }))))
            }
            TExpression(Expr::Cdr) => {
                Ok(ExpressionNode(Rc::new(ListAccess(
                    ListAccData {
                        index_expr: None,
                        pattern: Some(util::SCACHE.intern("r")),
                        list: self.parse_list_head()?,
                    }))))
            }
            TExpression(Expr::Randi) => self.parse_rand(false),
            TExpression(Expr::Randf) => self.parse_rand(true),
            _ => Err(format!("Expected expression found {:?}", expression))
        }
    }


    pub fn parse_rand(&mut self, is_float: bool) -> Result<AstNode, String> {
        let lower = self.parse_expr_data()?;
        let upper = self.parse_expr_data()?;
        Ok(ExpressionNode(Rc::new(GenRand(is_float, lower, upper))))
    }


    pub fn parse_assign(&mut self) -> Result<AstNode, String> {
        match self.peek().token_type {
            TLiteral(Lit::Identifier) => {
                let name = match &self.consume(TLiteral(Lit::Identifier))?.data {
                    Some(TokenData::String(name)) => name.clone(),
                    _ => return Err("Expected a string identifier".to_string())
                };
                let value = self.parse_expr_data()?;
                let data = Rc::new(Assignment(AssignData { name, value }));
                Ok(ExpressionNode(data))
            }

            TLexical(LeftParen) => {
                self.consume_left_paren()?;
                let token = self.advance()?;

                let name = if let (
                    TLiteral(Lit::Identifier), Some(TokenData::String(s))) = (&token.token_type, &token.data) {
                    *s
                } else { return Err(format!("Expected object name to assign to, found: {:?}", token)); };

                let accessors = self.parse_accessors()?;

                self.consume_right_paren()?;

                let value = self.parse_expr_data()?;
                let access = ObjectCallData { name, accessors };
                let assign_data = ObjectAssignData { access, value };
                Ok(ExpressionNode(Rc::new(ObjectAssignment(assign_data))))
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
        let if_branch = CondBranch { cond_node: condition, then_node: then };
        let else_branch = if self.peek().token_type != TLexical(Lex::RightParen) {
            Some(self.parse_expr_data()?)
        } else { None };
        Ok(ExpressionNode(Rc::new(IfExpr(IfData { if_branch, else_branch }))))
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
        } else { Ok(ExpressionNode(Rc::new(CondExpr(CondData { cond_branches, else_branch })))) }
    }


    pub fn parse_cond_branch(&mut self) -> Result<CondBranch, String> {
        self.consume_left_paren()?;

        let cond_node = self.parse_expr_data()?;
        let then_node = self.parse_expr_data()?;
        self.consume_right_paren()?;
        Ok(CondBranch { cond_node, then_node })
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
        } else { Ok(Some(ExpressionNode(Rc::new(MultiExpr(expressions))))) }
    }


    pub fn parse_print(&mut self) -> Result<AstNode, String> {
        let expr = self.parse_expr_data()?;
        Ok(ExpressionNode(Rc::new(PrintExpr(expr))))
    }


    pub fn parse_list(&mut self) -> Result<AstNode, String> {
        let mut elements = Vec::<AstNode>::new();
        while self.peek().token_type != TLexical(Lex::RightParen) {
            elements.push(self.parse_expr_data()?);
        }

        if elements.is_empty() {
            Ok(AstNode::new_nil_lit())
        } else { Ok(ExpressionNode(Rc::new(PairList(elements)))) }
    }


    pub fn parse_list_head(&mut self) -> Result<AstNode, String> {
        let head = self.parse_expr_data()?;
        if self.peek().token_type != TLexical(Lex::RightParen) {
            Err(format!("Invalid argument count, line: {}", self.peek().line))
        } else { Ok(head) }
    }


    pub fn parse_list_access(&mut self) -> Result<AstNode, String> {
        if self.peek().token_type == TLexical(Lex::SingleQuote) {
            self.advance()?;

            let token_data = &self.consume(TLiteral(Lit::Identifier))?.data.clone();
            let list = self.parse_list_head()?;

            if let Some(TokenData::String(s)) = token_data {
                Ok(ExpressionNode(Rc::new(ListAccess(ListAccData { index_expr: None, pattern: Some(s.clone()), list }))))
            } else { Err("Expected fr... access pattern".to_string()) }
        } else {
            let index = self.parse_expr_data()?;
            let list = self.parse_list_head()?;
            Ok(ExpressionNode(Rc::new(ListAccess(ListAccData { index_expr: Some(index), pattern: None, list }))))
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
        Ok(ExpressionNode(Rc::new(WhileLoop(WhileData { condition, body, is_do }))))
    }


    pub fn parse_cons(&mut self) -> Result<AstNode, String> {
        let car = self.parse_expr_data()?;
        let cdr = self.parse_expr_data()?;

        if self.peek().token_type != TLexical(Lex::RightParen) {
            Err("Cons expression may only have 2 arguments".to_string())
        } else { Ok(ExpressionNode(Rc::new(ConsExpr(ConsData { car, cdr })))) }
    }


    pub fn parse_type_if_exists(&mut self) -> Result<Option<Spur>, String> {
        let typ = if let TSyntactic(Syn::DoubleColon) = &self.peek().token_type {
            self.advance()?;
            let next = self.advance()?;

            if let (TLiteral(Lit::Identifier), Some(TokenData::String(s))) = (&next.token_type, &next.data) {
                Some(*s)
            } else { return Err(format!("Expected type identifier, found: {:?}", next)); }
        } else { None };

        //  if typ.is_some() { self.advance()?; }
        Ok(typ)
    }


    pub fn parse_identifier(&mut self, identifier: Spur) -> Result<AstNode, String> {
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
                Ok(ExpressionNode(Rc::new(ObjectCall(ObjectCallData { name, accessors }))))
            } else { Ok(ExpressionNode(Rc::new(FuncCall(FuncCallData { name, arguments })))) }
        } else {
            if accessors.is_some() {
                return Err("All object access must be contained inside an expression,\
                Ex: (<Object>:.field) not <Object>:.field".to_string());
            }
            Ok(ExpressionNode(Rc::new(LiteralCall(name))))
        }
    }


    fn parse_accessors(&mut self) -> Result<LinkedList<Accessor>, String> {
        let mut accessors = LinkedList::<Accessor>::new();
        while matches!(
            self.peek().token_type, TSyntactic(Syn::DoubleColon) | TSyntactic(Syn::ColonDot)
        ) {
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
            } else { None };

            accessors.push_back(Accessor { name, is_field, args })
        }
        Ok(accessors)
    }

    fn parse_method_args(&mut self) -> Result<Vec<FuncArg>, String> {
        let mut args = Vec::<FuncArg>::with_capacity(4);

   
        while self.peek().token_type != TLexical(RightBracket) {
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
                            util::SCACHE.resolve(&var_type.unwrap()), self.peek().line))
                    }

                    self.consume_left_paren()?;
                    let lambda = self.parse_lambda(false, false)?;
                    self.consume_right_paren()?;

                    DefinitionNode(Rc::new(DefNode::Function(DefFuncData { name, lambda: Rc::new(lambda) })))
                } else {
                    let value = Rc::new(self.parse_expr_data()?);
                    DefinitionNode(Rc::new(DefNode::Variable(DefVarData { name, modifiers, value, var_type })))
                }
            }
            
            TLiteral(_) => {
                let value = Rc::new(self.parse_literal()?);
                DefinitionNode(Rc::new(DefNode::Variable(DefVarData { name, modifiers, value, var_type })))
            }
            
            TLexical(Lex::SingleQuote) => self.parse_quote()?,
            
            _ => return Err(format!("Invalid syntax in define, line: {}", self.peek().line))
        };
        Ok(definition)
    }


    pub fn parse_instance(&mut self, name: Spur) -> Result<AstNode, String> {
        if self.previous_n(2).token_type == TLexical(LeftParen) {
            let arguments = self.parse_func_args()?;
            Ok(ExpressionNode(Rc::new(ExprNode::InitInst(FuncCallData { name, arguments }))))
        } else {
            self.consume(TLexical(Lex::LeftBracket))?; // post-fixed direct call, consume opening bracket
            let args = self.parse_named_args()?;
            self.consume(TLexical(Lex::RightBracket))?;
            Ok(ExpressionNode(Rc::new(ExprNode::DirectInst(DirectInst { name, args }))))
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
            && self.peek_n(2).token_type == TLexical(Lex::RightParen) {
            self.consume_left_paren()?;
            self.consume_right_paren()?;
            return Ok(AstNode::new_nil_lit());
        }
        Ok(AstNode::new_quote_lit(self.parse_expr_data()?))
    }


    pub fn parse_struct_def(&mut self) -> Result<AstNode, String> {
        self.consume(TDefinition(DefineStruct))?;

        let name = match &self.consume(TLiteral(Lit::Identifier))?.data {
            Some(TokenData::String(name)) => name.clone(),
            _ => return Err("Expected name for struct".to_string())
        };
        let fields = self.parse_fields()?;
        Ok(DefinitionNode(Rc::new(DefNode::StructDef(DefStructData { name, fields }))))
    }


    pub fn parse_class_def(&mut self) -> Result<AstNode, String> {
        self.consume(TDefinition(DefineClass))?;

        let name = match &self.consume(TLiteral(Lit::Identifier))?.data {
            Some(TokenData::String(name)) => name.clone(),
            _ => return Err("Expected name for class".to_string())
        };

        let mut class_data = DefClassData::empty_def(name);


        while self.peek().token_type != TLexical(RightParen) {
            self.consume_left_paren()?;
            self.consume(TSyntactic(Syn::Colon))?;

            match &self.peek().data {
                Some(TokenData::String(spur)) if spur == &util::SCACHE.const_init => {
                    self.advance()?;
                    let mut init_vec = Vec::<DefLambdaData>::with_capacity(4);

                    while self.peek().token_type != TLexical(RightParen) {
                        self.consume_left_paren()?;
                        match self.parse_lambda(true, true) {
                            Ok(init) => init_vec.push(init),
                            Err(e) => return Err(e),
                        }
                        self.consume_right_paren()?;
                    }

                    class_data.init = if init_vec.is_empty() { None } else { Some(init_vec) }
                }

                Some(TokenData::String(spur)) if spur == &util::SCACHE.const_func => {
                    self.advance()?;
                    let mut func_vec = Vec::<DefFuncData>::with_capacity(4);

                    while self.peek().token_type != TLexical(RightParen) {
                        self.consume_left_paren()?;
                        match self.parse_func(false) {
                            Ok(func) => func_vec.push(func),
                            Err(e) => return Err(e),
                        }
                        self.consume_right_paren()?;
                    }

                    class_data.methods = if func_vec.is_empty() { None } else { Some(func_vec) }
                }

                Some(TokenData::String(spur)) if spur == &util::SCACHE.const_param => {
                    self.advance()?;
                    class_data.params = self.parse_modifiers()?;
                }

                Some(TokenData::String(spur)) if spur == &util::SCACHE.const_var => {
                    self.advance()?;
                    class_data.fields = self.parse_fields()?
                }

                Some(TokenData::String(spur)) if spur == &util::SCACHE.const_pre => {
                    self.advance()?;
                    class_data.pre_init = self.parse_multi_expr()?;
                }

                Some(TokenData::String(spur))if spur == &util::SCACHE.const_post => {
                    self.advance()?;
                    class_data.post_init = self.parse_multi_expr()?;
                }

                Some(TokenData::String(spur))if spur == &util::SCACHE.const_final => {
                    self.advance()?;
                    class_data.fin = self.parse_multi_expr()?;
                }

                Some(TokenData::String(spur))if spur == &util::SCACHE.const_validate => {
                    self.advance()?;
                    class_data.validate = self.parse_multi_expr()?;
                }

                _ => { return Err(format!("Invalid token in class definition:  {:?}", &self.peek())); }
            }

            self.consume_right_paren()?;
        }
        Ok(DefinitionNode(Rc::new(ClassDef(class_data))))
    }


    pub fn parse_fields(&mut self) -> Result<Option<Vec<Field>>, String> {
        let mut fields = Vec::<Field>::new();

        while self.peek().token_type != TLexical(RightParen) {
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
            fields.push(Field { name, modifiers, p_type, default_value, c_type: None })
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
        Ok(DefFuncData { name, lambda: Rc::new(lambda) })
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
        Ok(DefLambdaData { modifiers, parameters, body, p_type: rtn_type, c_type: None })
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

        while self.peek().token_type != if bracketed { TLexical(RightBracket) } else { TLexical(RightParen) } {
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
                p_type,
                optional,
                default_value,
                dynamic,
                mutable,
                c_type: None,
            }
            );
        }
        if bracketed { self.consume_right_bracket()?; } else { self.consume_right_paren()?; }
        Ok(Some(params))
    }
}


pub fn process(tokens: Vec<Token>) -> Result<Vec<AstNode>, String> {
    let mut state = ParserState::new(tokens);
    state.process()
}
