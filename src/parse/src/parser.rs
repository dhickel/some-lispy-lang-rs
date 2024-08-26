use lang::types::{FuncType, ObjType, Type, UnresolvedType};
use lang::util::{IString, SCACHE};

use lang::ast::*;
use crate::grammar::{Arg, AssignStmntPattern, BlockExprPattern, CondExprPattern, ExprPattern,
    FExprPattern, LambdaExprPattern, LambdaFormPattern, LetStmntPattern, MemberAccess, Operation,
    Param, ParseMatch, PredicateForm, SExprPattern, StmntPattern, SubParser};
use crate::{grammar, ParseError};
use lang::token::*;
use lang::token::TokenType::*;
use lang::types::Type::Unresolved;


pub struct ParserState {
    pub tokens: Vec<Token>,
    pub current: usize,
    pub end: usize,
    pub depth: i32,
    pub warnings: Vec<String>,
    pub name_space: IString,
}


pub enum ExprType {
    SExpr,
    FExpr,
    NExpr,
    LitExpr,
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


    pub fn process(&mut self) -> Result<ParseResult, ParseError> {
        let mut root_expressions = Vec::<AstNode>::with_capacity(10);

        while self.have_next() {
            let grammar_match = {
                let mut sub_parser = SubParser::new(|idx| self.peek_n(idx));
                grammar::find_next_match(&mut sub_parser)?
            };

            println!("\nParse Match: {:?}\n", grammar_match);
            let parsed_data = self.parse_grammar_pattern(grammar_match)?;
            root_expressions.push(parsed_data)
        }

        Ok(ParseResult { name_space: self.name_space, root_expressions })
    }


    pub fn push_warning(&mut self, warning: String) {
        self.warnings.push(warning);
    }

    pub fn have_next(&self) -> bool {
        self.current + 1 < self.end
    }

    pub fn have_next_error(&self) -> Result<(), ParseError> {
        if self.current + 1 < self.end {
            Ok(())
        } else { ParseError::parse_error("Advanced past end".to_string()) }
    }

    pub fn peek(&self) -> Result<&Token, ParseError> {
        self.have_next_error()?;
        Ok(&self.tokens[self.current])
    }

    pub fn line_char(&self) -> Result<(u32, u32), ParseError> {
        let peek = self.peek()?;
        Ok((peek.line, peek.char))
    }

    pub fn peek_n(&self, n: usize) -> Result<&Token, ParseError> {
        self.have_next_error()?;
        Ok(&self.tokens[self.current + (n - 1)])
    }

    pub fn peek_token_type_n(&self, n: usize) -> Result<TokenType, ParseError> {
        self.have_next_error()?;
        Ok(self.tokens[self.current + (n - 1)].token_type)
    }

    pub fn previous(&self) -> &Token {
        self.tokens.get(if self.current == 0 { 0 } else { self.current - 1 }).unwrap()
    }

    pub fn previous_n(&self, n: usize) -> &Token {
        self.tokens.get(if self.current < n { 0 } else { self.current - n }).unwrap()
    }

    pub fn check(&mut self, token_type: &TokenType) -> bool {
        if self.have_next() {
            let peek_token = self.peek().unwrap().token_type;
            peek_token == *token_type
        } else { false }
    }

    pub fn open_container_token_check(&self) -> Result<(), ParseError> {
        let token = self.peek()?.token_type;
        if matches!(token, 
            TSyntactic(Syn::LeftParen) | TSyntactic(Syn::RightParen) |
            TSyntactic(Syn::LeftBracket) | TSyntactic(Syn::RightBracket) |
            TSyntactic(Syn::LeftBrace) | TSyntactic(Syn::RightBrace)
        ) {
            ParseError::parse_error(
                "\"(, [, {, }, ], )\" should only be advanced via related consumer function".to_string()
            )
        } else {
            Ok(())
        }
    }

    pub fn advance_past_end_check(&self) -> Result<(), ParseError> {
        if !self.have_next() {
            Err(ParseError::Parsing("Advanced past end".to_string()))
        } else { Ok(()) }
    }

    pub fn advance(&mut self) -> Result<Token, ParseError> {
        self.advance_past_end_check()?;
        self.open_container_token_check()?;

        self.current += 1;
        Ok(self.tokens[self.current - 1])
    }

    pub fn match_token(&mut self, tokens: &[TokenType]) -> Option<&Token> {
        self.tokens.iter().find(|token| tokens.contains(&token.token_type))
    }

    pub fn consume(&mut self, token_type: TokenType) -> Result<Token, ParseError> {
        self.advance_past_end_check()?;
        self.open_container_token_check()?;

        if !self.check(&token_type) {
            ParseError::parse_error(format!("Expected: {:?}, Found: {:?}", token_type, self.peek()?))?
        }
        self.advance()
    }

    pub fn consume_left_paren(&mut self) -> Result<(), ParseError> {
        if !self.check(&TSyntactic(Syn::LeftParen)) {
            ParseError::parse_error(format!("Expected: Left Paren, Found: {:?}", self.peek()?))?
        }
        self.current += 1;
        self.depth += 1;
        Ok(())
    }

    pub fn consume_right_paren(&mut self) -> Result<(), ParseError> {
        if !self.check(&TSyntactic(Syn::RightParen)) {
            ParseError::parse_error(format!("Expected: Right Paren, Found: {:?}", self.peek()?))?
        }
        self.current += 1;
        self.depth -= 1;
        Ok(())
    }

    pub fn consume_left_bracket(&mut self) -> Result<(), ParseError> {
        if !self.check(&TSyntactic(Syn::LeftBracket)) {
            ParseError::parse_error(format!("Expected: Left Bracket, Found: {:?}", self.peek()?))?
        }
        self.current += 1;
        Ok(())
    }

    pub fn consume_right_bracket(&mut self) -> Result<(), ParseError> {
        if !self.check(&TSyntactic(Syn::RightBracket)) {
            ParseError::parse_error(format!("Expected: Right Bracket, Found: {:?}", self.peek()?))?
        }
        self.current += 1;
        Ok(())
    }

    pub fn consume_left_brace(&mut self) -> Result<(), ParseError> {
        if !self.check(&TSyntactic(Syn::LeftBrace)) {
            ParseError::parse_error(format!("Expected: Left Brace, Found: {:?}", self.peek()?))?
        }
        self.current += 1;
        Ok(())
    }

    pub fn consume_right_brace(&mut self) -> Result<(), ParseError> {
        if !self.check(&TSyntactic(Syn::RightBrace)) {
            ParseError::parse_error(format!("Expected: Right Brace, Found: {:?}", self.peek()?))?
        }
        self.current += 1;
        Ok(())
    }


    ///////////////
    // Top Parse //
    ///////////////

    pub fn parse_grammar_pattern(&mut self, pattern: ParseMatch) -> Result<AstNode, ParseError> {
        match pattern {
            ParseMatch::Expression(expr) => {
                let expr = self.parse_expression(expr)?;
                Ok(expr.into())
            }
            ParseMatch::Statement(stmnt) => {
                let stmnt = self.parse_statement(stmnt)?;
                Ok(stmnt.into())
            }
        }
    }

    pub fn parse_statement(&mut self, stmnt: StmntPattern) -> Result<Statement, ParseError> {
        match stmnt {
            StmntPattern::Let(let_pat) => self.parse_let_statement(let_pat),
            StmntPattern::Assign(assign_pat) => self.parse_assignment_statement(assign_pat)
        }
    }

    pub fn parse_expression(&mut self, expr: ExprPattern) -> Result<Expression, ParseError> {
        match expr {
            ExprPattern::SExpr(pat) => self.parse_s_expression(pat),
            ExprPattern::VExpr => self.parse_v_expression(),
            ExprPattern::FExpr(pat) => self.parse_f_expression(pat),
            ExprPattern::BExpr => todo!(),
            ExprPattern::BlockExpr(pat) => self.parse_block_expression(pat),
            ExprPattern::CondExpr(pat) => self.parse_condition_expression(pat),
            ExprPattern::LambdaExpr(pat) => self.parse_lambda_expression(pat),
            ExprPattern::LambdaFormExpr(pat) => self.parse_lambda_form(pat),
            ExprPattern::MatchExpr => todo!()
        }
    }


    /////////////
    // UTILITY //
    /////////////
    pub fn parse_identifier(&mut self) -> Result<IString, ParseError> {
        match self.consume(TLiteral(Lit::Identifier))?.data {
            Some(TokenData::String(token)) => Ok(token),
            other => {
                ParseError::parsing_error(
                    self.peek()?,
                    &format!("Expected Identifier(IString), found: {:?}", other))
            }
        }
    }

    pub fn parse_modifiers(&mut self, count: u32) -> Result<Option<Vec<Mod>>, ParseError> {
        if count == 0 { return Ok(None); };

        let mut modifiers = Vec::<Mod>::with_capacity(count as usize);
        for _ in 0..count {
            if let TokenType::TModifier(modd) = self.advance()?.token_type {
                modifiers.push(modd);
            } else { ParseError::parsing_error(self.peek()?, "Expected modifier")? }
        }
        Ok(Some(modifiers))
    }

    pub fn parse_type(&mut self, consume_colon: bool) -> Result<Type, ParseError> {
        if consume_colon {
            // ::= ':'
            self.consume(TSyntactic(Syn::Colon))?;
        }


        if matches!(self.peek()?.token_type, TokenType::FN) {
            self.advance()?;
            Ok(self.parse_func_type()?)
        } else if let Some(TokenData::String(str)) = self.consume(TokenType::IDENTIFIER)?.data {
            if matches!(self.peek()?.token_type, TokenType::ANGLE_BRACKET_LEFT) {
                self.advance()?;
                let typ = self.parse_type(false)?;
                self.consume(TokenType::ANGLE_BRACKET_RIGHT)?;
                Ok(typ)
            } else {
                Ok(Type::parse_type_from_string(str))
            }
        } else { ParseError::parsing_error(self.peek()?, "Expected Identifier for type") }
    }

    // ::= '<' { Identifier } ';' Identifier } '>'
    pub fn parse_func_type(&mut self) -> Result<Type, ParseError> {
        let mut func_type = FuncType::default();

        // ::= '<'
        self.consume(TOperation(Op::Less))?;


        // ::= { Identifier }
        while matches!(self.peek()?.token_type, TokenType::IDENTIFIER) {
            let typ = self.parse_type(false)?;
            func_type.add_param_type(typ);
        }
        // ::= ';'
        self.consume(TSyntactic(Syn::SemiColon))?;

        // ::= Identifier
        let typ = self.parse_type(false)?;
        func_type.set_return_type(typ)
            .map_err(|err| { ParseError::TypeChecking(format!("{:?}", err)) })?;

        // ::= '>'

        self.consume(TOperation(Op::Greater))?;

        Ok(Type::Lambda(func_type))
    }

    pub fn parse_array_type(&mut self) -> Result<Type, ParseError> {
        let typ = self.parse_type(false)?;
        Ok(Type::Array(Box::new(typ)))
    }

    ////////////////
    // Statements //
    ////////////////

    // ::= 'let' Identifier { Modifier } [ ':' Type ] '=' Expr
    pub fn parse_let_statement(&mut self, pattern: LetStmntPattern) -> Result<Statement, ParseError> {
        let line_char = self.line_char()?;

        // ::= let
        self.consume(TDefinition(Def::DefineLet))?;

        // ::= Identifier
        let identifier = self.parse_identifier()?;

        // ::= [ ':' Type ]
        let typ = if pattern.has_type {
            self.parse_type(true)?
        } else { UnresolvedType::Unknown.into() };

        // ::= { Modifier }
        let modifiers = self.parse_modifiers(pattern.modifier_count)?;

        // ::= '='
        self.consume(TSyntactic(Syn::Equal))?;

        // ::= Expr
        let assignment = self.parse_expression(pattern.expr)?;

        let let_data = LetData { identifier, modifiers, assignment };
        Ok(Statement::Let(AstData::new(let_data, line_char, Some(typ))))
    }

    // ::= Identifier ':=' Expr
    pub fn parse_assignment_statement(&mut self, pattern: AssignStmntPattern) -> Result<Statement, ParseError> {
        let line_char = self.line_char()?;

        // ::= Identifier
        let identifier = self.parse_identifier()?;

        // ::= ':='
        self.consume(TSyntactic(Syn::ColonEqual))?;

        // ::= Expr
        let value = self.parse_expression(pattern.expr)?;

        let assign_data = AssignData { identifier, namespace: None, value };
        Ok(Statement::Assign(AstData::new(assign_data, line_char, None)))
    }

    // ::= '(' Expr | Operation { Expr }
    pub fn parse_s_expression(&mut self, pattern: SExprPattern) -> Result<Expression, ParseError> {
        let line_char = self.line_char()?;

        // ::= '('
        self.consume_left_paren()?;

        // ::= Expr | Operation
        let (op, expr_op) = match pattern.operation {
            Operation::Expr(expr) => (None, Some(self.parse_expression(*expr)?)),
            Operation::Op => (Some(self.parser_operator()?), None)
        };

        // ::= { Expr } 
        let operands = if let Some(operands) = pattern.operands {
            let mut operand_expr = Vec::<Expression>::with_capacity(operands.len());
            for expr in operands { operand_expr.push(self.parse_expression(expr)?) }
            Some(operand_expr)
        } else { None };

        // ::= ')'
        self.consume_right_paren()?;

        if let Some(op) = op {
            let data = OpCallData { operation: op, operands };
            Ok(Expression::OpCall(AstData::new(data, line_char, None)))
        } else if let Some(expr) = expr_op {
            let data = SCallData { operation_expr: expr, operand_exprs: operands };
            Ok(Expression::SCall(AstData::new(data, line_char, None)))
        } else { ParseError::parsing_error(self.peek()?, "Proper Expression") }
    }

    //::=  [ NamespaceAccess ] [ Identifier ] [ MemberAccessChain ]
    pub fn parse_f_expression(&mut self, pattern: FExprPattern) -> Result<Expression, ParseError> {
        let line_char = self.line_char()?;

        let mut sub_exprs = Vec::<FExprData>::with_capacity(pattern.namespace_count as usize + 3);

        // ::= [ NamespaceAccess ]
        if let Some(mut ns) = self.parse_namespace(pattern.namespace_count)? { sub_exprs.append(&mut ns) };

        // ::= Identifier
        if pattern.has_identifier {
            let identifier = self.parse_identifier()?;
            sub_exprs.push(FExprData::MAccess { identifier, m_type: MType::Identifier })
        }

        // ::= [ MemberAccessChain ]
        if let Some(mut ac) = self.parse_member_access(pattern.access_chain)? { sub_exprs.append(&mut ac); }

        Ok(Expression::FCall(AstData::new(sub_exprs, line_char, None)))
    }

    pub fn parse_v_expression(&mut self) -> Result<Expression, ParseError> {
        let line_char = self.line_char()?;
        let token = self.advance()?;

        if let TokenType::TLiteral(lit) = token.token_type {
            let (typ, value) = match lit {
                Lit::True => (Some(Type::Boolean), Value::Boolean(true)),
                Lit::False => (Some(Type::Boolean), Value::Boolean(false)),
                Lit::String => (Some(Type::String), Value::String), // FIXME, need to actual implement strings
                Lit::Int => {
                    if let Some(TokenData::Integer(val)) = token.data {
                        (Some(Type::Integer), Value::I64(val))
                    } else { ParseError::parsing_error(self.peek()?, "Expected Integer Value")? }
                }
                Lit::Float => {
                    if let Some(TokenData::Float(val)) = token.data {
                        (Some(Type::Float), Value::F64(val))
                    } else { ParseError::parsing_error(self.peek()?, "Expected Float Value")? }
                }
                Lit::Identifier => {
                    if let Some(TokenData::String(val)) = token.data {
                        (Some(UnresolvedType::Unknown.into()), Value::Identifier(val))
                    } else { ParseError::parsing_error(self.peek()?, "Expected Identifier")? }
                }
                Lit::Nil => (Some(Type::Nil), Value::Nil(())),
            };
            Ok(Expression::Value(AstData::new(value, line_char, typ)))
        } else { ParseError::parsing_error(self.peek()?, "Expected value expression") }
    }

    // ::= '{' { Expr | Stmnt } '}'
    pub fn parse_block_expression(&mut self, pattern: BlockExprPattern) -> Result<Expression, ParseError> {
        let line_char = self.line_char()?;

        // ::= '{'
        self.consume_left_brace()?;

        let expr = if let Some(members) = pattern.members {
            let members = members.into_iter()
                .map(|pat| Ok(self.parse_grammar_pattern(pat)?))
                .collect::<Result<Vec<AstNode>, ParseError>>()?;

            Expression::Block(AstData::new(members, line_char, None))
        } else { Expression::Block(AstData::new(vec![], line_char, None)) };

        // ::= '}'
        self.consume_right_brace()?;
        Ok(expr)
    }

    pub fn parse_lambda_expression(&mut self, pattern: LambdaExprPattern) -> Result<Expression, ParseError> {
        let line_char = self.line_char()?;

        // ::=  '=>'
        self.consume(TokenType::LAMBDA_ARROW)?;

        // ::= [ (':' Type) ]
        let typ = if pattern.has_type {
            Some(self.parse_type(true)?)
        } else { None };

        // ::= '|' { Parameter } '|' Expr
        let lambda = self.parse_lambda(pattern.form)?;

        Ok(Expression::Lambda(AstData::new(lambda, line_char, typ)))
    }

    pub fn parse_lambda_form(&mut self, pattern: LambdaFormPattern) -> Result<Expression, ParseError> {
        let line_char = self.line_char()?;
        let lambda = self.parse_lambda(pattern)?;
        Ok(Expression::Lambda(AstData::new(lambda, line_char, None)))
    }

    // ::= '|' { Parameter } '|' Expr
    pub fn parse_lambda(&mut self, pattern: LambdaFormPattern) -> Result<LambdaData, ParseError> {
        // ::= '|'
        self.consume(TokenType::BAR)?;
        // ::= { Parameter }
        let parameters = self.parse_parameters(pattern.parameters)?;
        // ::= '|'
        self.consume(TokenType::BAR)?;

        let expr = self.parse_expression(*pattern.expr)?;

        Ok(LambdaData { parameters, expr })
    }


    // ::= '(' Expr '->' Expr [ ':' Expr ] ')'
    pub fn parse_condition_expression(&mut self, pattern: CondExprPattern) -> Result<Expression, ParseError> {
        let line_char = self.line_char()?;
        // ::= '('
        self.consume_left_paren()?;

        // ::= Expr
        let pred_expr = self.parse_expression(*pattern.pred_expr)?;

        // ::= '->' Expr [ ':' Expr ]
        let (then_expr, else_expr) = self.parse_predicate_form(*pattern.pred_form)?;

        // ::= ')'
        self.consume_right_paren()?;

        let data = PredicateData { pred_expr, then_expr, else_expr };
        Ok(Expression::Predicate(AstData::new(data, line_char, None)))
    }

    // ::= '->' Expr [ ':' Expr ]
    pub fn parse_predicate_form(
        &mut self,
        pattern: PredicateForm,
    ) -> Result<(Option<Expression>, Option<Expression>), ParseError> {


        // ::= '->' Expr
        let then_expr = if let Some(then_pattern) = pattern.then_form {
            self.consume(TokenType::RIGHT_ARROW)?;
            Some(self.parse_expression(then_pattern)?)
        } else {
            None
        };

        let else_expr = if let Some(else_pattern) = pattern.else_form {
            // ::= [ ':' Expr ] 
            self.consume(TokenType::TSyntactic(Syn::Colon))?;
            Some(self.parse_expression(else_pattern)?)
        } else { None };

        Ok((then_expr, else_expr))
    }

    pub fn parse_namespace(&mut self, count: u32) -> Result<Option<Vec<FExprData>>, ParseError> {
        if count == 0 { return Ok(None); };

        let mut namespaces = Vec::<FExprData>::with_capacity(count as usize);
        for _ in 0..count {
            if let Some(TokenData::String(str)) = self.advance()?.data {
                namespaces.push(FExprData::MAccess { identifier: str, m_type: MType::Namespace });
                self.consume(TokenType::RIGHT_ARROW)?;
            } else { ParseError::parsing_error(self.peek()?, "Namespace(s)")? }
        }
        Ok(Some(namespaces))
    }

    pub fn parse_member_access(
        &mut self,
        members: Option<Vec<MemberAccess>>,
    ) -> Result<Option<Vec<FExprData>>, ParseError> {
        if let Some(members) = members {
            let exprs = members.into_iter().map(|mem| {
                match mem {
                    MemberAccess::Field => {
                        // ::= ':.'
                        self.consume(TokenType::FIELD_SPACE_ACCESS)?;
                        let identifier = self.parse_identifier()?;
                        Ok(FExprData::MAccess { identifier, m_type: MType::Field })
                    }
                    MemberAccess::MethodCall { arguments } => {
                        // ::=  '::'
                        self.consume(TokenType::METHOD_SPACE_ACCESS)?;
                        // ::=  [ Identifier ]
                        let identifier = if matches!(self.peek()?. token_type, TokenType::IDENTIFIER) {
                            Some(self.parse_identifier()?)
                        } else { None };


                        // ::= '['
                        self.consume_left_bracket()?;
                        // ::= { Argument }
                        let arguments = self.parse_arguments(arguments)?;
                        // ::= ']'
                        self.consume_right_bracket()?;
                        Ok(FExprData::MCall { method: identifier, arguments })
                    }
                    MemberAccess::MethodAccess => {
                        // ::=  '::'
                        self.consume(TokenType::METHOD_SPACE_ACCESS)?;
                        let identifier = self.parse_identifier()?;
                        Ok(FExprData::MAccess { identifier, m_type: MType::Identifier })
                    }
                }
            }).collect::<Result<Vec<FExprData>, ParseError>>()?;
            Ok(Some(exprs))
        } else { Ok(None) }
    }

    pub fn parse_arguments(&mut self, arguments: Option<Vec<Arg>>) -> Result<Option<Vec<Argument>>, ParseError> {
        if let Some(args) = arguments {
            let parsed_args = args.into_iter()
                .map(|arg| {
                    let modifiers = self.parse_modifiers(arg.modifier_count)?;
                    let expr = self.parse_expression(arg.expr)?;
                    Ok(Argument { modifiers, expr })
                })
                .collect::<Result<Vec<Argument>, ParseError>>()?;
            Ok(Some(parsed_args))
        } else { Ok(None) }
    }

    pub fn parse_parameters(&mut self, params: Option<Vec<Param>>) -> Result<Option<Vec<Parameter>>, ParseError> {
        if let Some(params) = params {
            let parameters = params.into_iter()
                .map(|param| {
                    let modifiers = self.parse_modifiers(param.modifier_count)?;
                    let identifier = self.parse_identifier()?;
                    let typ = if param.has_type {
                        Some(self.parse_type(true)?)
                    } else { None };
                    Ok(Parameter { modifiers, identifier, typ })
                }).collect::<Result<Vec<Parameter>, ParseError>>()?;
            Ok(Some(parameters))
        } else { Ok(None) }
    }


    pub fn parser_operator(&mut self) -> Result<Op, ParseError> {
        if let TOperation(op) = self.advance()?.token_type {
            Ok(op)
        } else { ParseError::parsing_error(self.peek()?, "Operation Token") }
    }
} 