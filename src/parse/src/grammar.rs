use std::cmp::PartialEq;
use std::fmt::Debug;
use crate::ParseError;
use crate::grammar::ExprPattern::{FExpr, LambdaExpr, LambdaFormExpr, VExpr};
use lang::token::{Def, Syn, Token, TokenType};

#[macro_export]
macro_rules! match_fn {
    ($variant:pat) => {|t: &Token| matches!(t.token_type, $variant)};
}

const MATCH_METHOD_ACCESS: fn(&Token) -> bool = match_fn!(TokenType::METHOD_SPACE_ACCESS);
const MATCH_FIELD_ACCESS: fn(&Token) -> bool = match_fn!(TokenType::FIELD_SPACE_ACCESS);
const MATCH_LEFT_PAREN: fn(&Token) -> bool = match_fn!(TokenType::LEFT_PAREN);
const MATCH_RIGHT_PAREN: fn(&Token) -> bool = match_fn!(TokenType::RIGHT_PAREN);
const MATCH_LEFT_BRACKET: fn(&Token) -> bool = match_fn!(TokenType::LEFT_BRACKET);
const MATCH_RIGHT_BRACKET: fn(&Token) -> bool = match_fn!(TokenType::RIGHT_BRACKET);
const MATCH_LEFT_BRACE: fn(&Token) -> bool = match_fn!(TokenType::LEFT_BRACE);
const MATCH_RIGHT_BRACE: fn(&Token) -> bool = match_fn!(TokenType::RIGHT_BRACE);
const MATCH_LAMBDA_ARROW: fn(&Token) -> bool = match_fn!(TokenType::LAMBDA_ARROW);
const MATCH_RIGHT_ARROW: fn(&Token) -> bool = match_fn!(TokenType::RIGHT_ARROW);
const MATCH_IDENTIFIER: fn(&Token) -> bool = match_fn!(TokenType::IDENTIFIER);
const MATCH_VALUE: fn(&Token) -> bool = match_fn!(TokenType::TLiteral(_));
const MATCH_MODIFIER: fn(&Token) -> bool = match_fn!(TokenType::TModifier(_));
const MATCH_ASSIGN: fn(&Token) -> bool = match_fn!(TokenType::ASSIGNMENT);
const MATCH_LET: fn(&Token) -> bool = match_fn!(TokenType::TDefinition(Def::DefineLet));
const MATCH_COLON: fn(&Token) -> bool = match_fn!(TokenType::TSyntactic(Syn::Colon));
const MATCH_EQUAL: fn(&Token) -> bool = match_fn!(TokenType::TSyntactic(Syn::Equal));
const MATCH_BAR: fn(&Token) -> bool = match_fn!(TokenType::TSyntactic(Syn::Bar));
const MATCH_OPERATION: fn(&Token) -> bool = match_fn!(TokenType::TOperation(_));


pub fn match_tokens<F>(
    parser: &mut SubParser,
    matcher_fns: &[F],
) -> Result<bool, ParseError>
where
    F: Fn(&Token) -> bool,
{
    for (func, index) in matcher_fns.iter().zip(1..=matcher_fns.len()) {
        let token = parser.peek_n(index)?;
        if !func(token) { return Ok(false); }
    }
    // Consume all tokens after full match
    parser.consume_n(matcher_fns.len());
    Ok(true)
}


pub fn match_multiple<F>(
    parser: &mut SubParser,
    matcher_fns: &[F],
) -> Result<u32, ParseError>
where
    F: Fn(&Token) -> bool,
{
    let mut count = 0;
    while match_tokens(parser, matcher_fns)? {
        count += 1;
    }
    Ok(count)
}


fn validation_error<T, E>(token: &Token, expected_str: E) -> Result<T, ParseError>
where
    E: Debug,
{
    ParseError::validation_error(
        format!("Expected: {:?} , Found: {:?}", expected_str, token.token_type),
        token.line,
        token.char,
    )?
}


pub struct SubParser<'a, 'b> {
    idx: usize,
    supplier: Box<dyn Fn(usize) -> Result<&'a Token, ParseError> + 'b>,
}


impl<'a, 'b> SubParser<'a, 'b> {
    pub fn new<F>(supplier: F) -> Self
    where
        F: Fn(usize) -> Result<&'a Token, ParseError> + 'b,
    {
        Self {
            idx: 0,
            supplier: Box::new(supplier),
        }
    }

    pub fn advance(&mut self) -> Result<&'a Token, ParseError> {
        self.idx += 1;
        (self.supplier)(self.idx - 1)
    }

    pub fn peek_n(&self, n: usize) -> Result<&'a Token, ParseError> {
        (self.supplier)(self.idx + n)
    }

    pub fn consume_n(&mut self, n: usize) {
        self.idx += n;
    }
}


#[derive(Debug)]
pub enum Operation {
    Expr(Box<ExprPattern>),
    Op,
}


#[derive(Debug)]
pub struct SExprPattern {
    pub operation: Operation,
    pub operands: Option<Vec<ExprPattern>>,
}


#[derive(Debug)]
pub struct FExprPattern {
    pub namespace_count: u32,
    pub has_identifier: bool,
    pub access_chain: Option<Vec<MemberAccess>>,
}


#[derive(Debug)]
pub struct BlockExprPattern {
    pub members: Option<Vec<ParseMatch>>,
}


#[derive(Debug)]
pub struct CondExprPattern {
    pub pred_expr: Box<ExprPattern>,
    pub pred_form: Box<PredicateForm>,
}


#[derive(Debug)]
pub struct LambdaExprPattern {
    pub has_type: bool,
    pub form: LambdaFormPattern,
}


#[derive(Debug)]
pub struct LambdaFormPattern {
    pub parameters: Option<Vec<Param>>,
    pub expr: Box<ExprPattern>,
}


#[derive(Debug)]
pub enum ExprPattern {
    SExpr(SExprPattern),
    VExpr,
    FExpr(FExprPattern),
    BExpr,
    BlockExpr(BlockExprPattern),
    CondExpr(CondExprPattern),
    LambdaExpr(LambdaExprPattern),
    LambdaFormExpr(LambdaFormPattern),
    MatchExpr,
}


#[derive(Debug)]
pub struct LetStmntPattern {
    pub has_type: bool,
    pub modifier_count: u32,
    pub expr: ExprPattern,
}


#[derive(Debug)]
pub struct AssignStmntPattern {
    pub expr: ExprPattern,
}


#[derive(Debug)]
pub enum StmntPattern {
    Let(LetStmntPattern),
    Assign(AssignStmntPattern),
}


#[derive(Debug, PartialEq)]
enum BuiltIn {
    Match { is_bound: bool },
    Iter { is_range: bool },
}


#[derive(Debug)]
pub struct Param {
    pub modifier_count: u32,
    pub has_type: bool,
}


#[derive(Debug)]
pub struct Arg {
    pub modifier_count: u32,
    pub expr: ExprPattern,
}


#[derive(Debug)]

// FIXME, predicate form should atleast always have a then form
pub struct PredicateForm {
    pub then_form: Option<ExprPattern>,
    pub else_form: Option<ExprPattern>,
}


#[derive(Debug)]
pub enum MemberAccess {
    Field,
    MethodCall { arguments: Option<Vec<Arg>> },
    MethodAccess,
}


#[derive(Debug)]
pub enum ParseMatch {
    Expression(ExprPattern),
    Statement(StmntPattern),
}


pub fn find_next_match(parser: &mut SubParser) -> Result<ParseMatch, ParseError> {
    if let Some(statement) = is_statement(parser)? { return Ok(ParseMatch::Statement(statement)); };
    if let Some(expr) = is_expression(parser)? { return Ok(ParseMatch::Expression(expr)); };
    println!("Nex TOken: {:?}", parser.peek_n(1)?);
    validation_error(parser.peek_n(1)?, " Statement Or Expression: Parse Error")
}

///////////////////////
// Statement Grammar //
///////////////////////

pub fn is_statement(parser: &mut SubParser) -> Result<Option<StmntPattern>, ParseError> {
    println!("Now at statement");
    if let Some(let_stmnt) = is_let_statement(parser)? { return Ok(Some(let_stmnt)); }
    if let Some(assign_stmnt) = is_assign_statement(parser)? { return Ok(Some(assign_stmnt)); }
    Ok(None)
}


// ::= 'let' Identifier [ (':' Type) ] { Modifier } '=' Expr
pub fn is_let_statement(parser: &mut SubParser) -> Result<Option<StmntPattern>, ParseError> {
    let mut has_type = false;

    // ::= 'let'
    if !match_tokens(parser, &[MATCH_LET])? { return Ok(None); }

    println!("At let statement");

    // ::= Identifier
    if !match_tokens(parser, &[MATCH_IDENTIFIER])? {
        validation_error(parser.peek_n(1)?, "Identifier")?
    }

    // ::= (':' Type)
    if match_tokens(parser, &[MATCH_COLON])? {
        if is_type(parser)? {
            has_type = true;
        } else { return validation_error(parser.peek_n(1)?, "Type"); }
    } else {
        return validation_error(parser.peek_n(1)?, ":");
    }

    // ::= { Modifier }
    let modifier_count = is_modifiers(parser)?;
    
    // ::= '='
    if !match_tokens(parser, &[MATCH_EQUAL])? {
        validation_error(parser.peek_n(1)?, "=")?
    }

    // ::= Expr
    println!("Next statement token: {:?}", parser.peek_n(1)?);
    if let Some(expr) = is_expression(parser)? {
        println!("Exit let statement");       
        Ok(Some(StmntPattern::Let(LetStmntPattern { has_type, modifier_count, expr })))
    } else { validation_error(parser.peek_n(1)?, "Expr") }
}


// ::= Identifier ':=' Expr // TODO better error when = is used for assignment on accident
pub fn is_assign_statement(parser: &mut SubParser) -> Result<Option<StmntPattern>, ParseError> {
    // ::= Identifier ':='
    if !match_tokens(parser, &[MATCH_IDENTIFIER, MATCH_ASSIGN])? { return Ok(None); }

    // ::= Expr
    if let Ok(Some(expr)) = is_expression(parser) {
        Ok(Some(StmntPattern::Assign(AssignStmntPattern { expr })))
    } else {  validation_error(parser.peek_n(1)?, "Expr") }
}


////////////////////////
// Expression Grammar //
////////////////////////


// NOTE the order of these matter
const EXPR_CHECKS: [fn(&mut SubParser) -> Result<Option<ExprPattern>, ParseError>; 7] = [
    is_block_expression, is_lambda_expression, is_lambda_form, is_b_expression,
    is_s_expression, is_v_expression, is_f_expression,
];


pub fn is_expression(parser: &mut SubParser) -> Result<Option<ExprPattern>, ParseError> {
    println!("Now at expression with token: {:?}", parser.peek_n(1)?);
    for expr_func in EXPR_CHECKS {
        println!("Loop Token: {:?}", parser.peek_n(1)?);
        if let Some(expr) = expr_func(parser)? { return Ok(Some(expr)); }
    }
    Ok(None)
}


pub fn is_operator(parser: &mut SubParser) -> Result<bool, ParseError> {
    match_tokens(parser, &[MATCH_OPERATION])
}


// ::= { Modifier } Identifier [ Type ]
pub fn is_parameter(parser: &mut SubParser) -> Result<Option<Param>, ParseError> {
    // ::= { Modifier }
    let modifier_count = match_multiple(parser, &[MATCH_MODIFIER])?;

    // ::= Identifier
    if !match_tokens(parser, &[MATCH_IDENTIFIER])? {
        if modifier_count == 0 {
            return Ok(None);
        } else {
            validation_error(parser.peek_n(1)?, "{Modifier}- followed by Identifier")?
        }
    }

    // ::= [ (':' Type) ]
    let has_type = if match_tokens(parser, &[MATCH_COLON])? {
        is_type(parser)?;
        true
    } else { false };

    Ok(Some(Param { modifier_count, has_type }))
}


// ::= { Parameter }
pub fn is_parameters(parser: &mut SubParser) -> Result<Option<Vec<Param>>, ParseError> {
    let mut parameters = Vec::new();
    while let Some(param) = is_parameter(parser)? { parameters.push(param) }
    if parameters.is_empty() { Ok(None) } else { Ok(Some(parameters)) }
}


// ::= { Modifier } Expr
pub fn is_argument(parser: &mut SubParser) -> Result<Option<Arg>, ParseError> {
    // ::= { Modifier }
    let modifier_count = match_multiple(parser, &[MATCH_MODIFIER])?;

    // ::= Expr
    if let Some(expr) = is_expression(parser)? {
        Ok(Some(Arg { modifier_count, expr }))
    } else { Ok(None) }
}


pub fn is_arguments(parser: &mut SubParser) -> Result<Option<Vec<Arg>>, ParseError> {
    let mut arguments = Vec::new();
    while let Some(arg) = is_argument(parser)? { arguments.push(arg); }
    if arguments.is_empty() { Ok(None) } else { Ok(Some(arguments)) }
}


// ::=  '=>' [ (':' Type) ] '|' { Parameters } '|' Expr
pub fn is_lambda_expression(parser: &mut SubParser) -> Result<Option<ExprPattern>, ParseError> {
    println!("lambda test");

    if !match_tokens(parser, &[MATCH_LEFT_PAREN, MATCH_LAMBDA_ARROW])? {
        return Ok(None);
    }
   
    
 
    println!("At lambda Expr");

    // ::= (':' Type)
    let has_type = if match_tokens(parser, &[MATCH_COLON])? {
        if is_type(parser)? {
            true
        } else { return validation_error(parser.peek_n(1)?, "Type"); }
    } else { false };

    // ::= ('|' { Parameter } '|')
    if let Some(LambdaFormExpr(mut expr)) = is_lambda_form(parser)? {
        // ::= ')'
        if !match_tokens(parser, &[MATCH_RIGHT_PAREN])? {
            validation_error(parser.peek_n(1)?, ")")?
        }
        
        Ok(Some(LambdaExpr(LambdaExprPattern {
            has_type,
            form: LambdaFormPattern {
                parameters: expr.parameters,
                expr: Box::new(*expr.expr),
            },
        })))
    } else { validation_error(parser.peek_n(1)?, "Expr") }
}


// ::= ('|' { Parameter } '|')
pub fn is_lambda_form(parser: &mut SubParser) -> Result<Option<ExprPattern>, ParseError> {
    println!("lambda form test");

    if !match_tokens(parser, &[MATCH_BAR])? {
        return Ok(None);
    }

    println!("At lambda Form");


    let parameters = is_parameters(parser)?;
    if !match_tokens(parser, &[MATCH_BAR])? {
        return validation_error(parser.peek_n(1)?, "|");
    }
    println!("ext token: {:?}", parser.peek_n(1)?);
    if let Some(expr) = is_expression(parser)? {
       
        Ok(Some(LambdaFormExpr(
            LambdaFormPattern {
                parameters,
                expr: Box::new(expr),
            }))
        )
    } else {  validation_error(parser.peek_n(1)?, "Expr") }
    
    
}


// : '(' Expr | Operation  { Expr } ')';
pub fn is_s_expression(parser: &mut SubParser) -> Result<Option<ExprPattern>, ParseError> {
    // ::= '('
    println!("test s Expr");
    if !match_tokens(parser, &[MATCH_LEFT_PAREN])? { 
        return Ok(None);
    }
    println!("At s Expr");


    // ::= Expr | Operation
    let operation = if is_operator(parser)? {
        Operation::Op
    } else if let Some(expr) = is_expression(parser)? {
        Operation::Expr(Box::new(expr))
    } else { validation_error(parser.peek_n(1)?, "Expr")? };


    // Check if predicate form ::= '->' Expr [ ':' Expr ]
    if let Some(pred_form) = is_predicate_form(parser)? {
        if let Operation::Expr(expr) = operation {
            return Ok(Some(ExprPattern::CondExpr(CondExprPattern { pred_expr: expr, pred_form: Box::new(pred_form) })));
        } else {
            validation_error(parser.peek_n(1)?, "PredicateForm cannot be used with operators")?
        }
    }

    // ::= { Expr }
    let mut operands = Vec::new();
    while let Some(expr) = is_expression(parser)? {
        operands.push(expr)
    }


    // ::= ')'
    if !match_tokens(parser, &[MATCH_RIGHT_PAREN])? {
        validation_error(parser.peek_n(1)?, ")")?
    }

    Ok(Some(ExprPattern::SExpr(SExprPattern {
        operation,
        operands: if operands.is_empty() { None } else { Some(operands) },
    })))
}


// ::= {[ NamespaceChain ] [ Identifier ] [ MemberAccessChain ]}- 
pub fn is_f_expression(parser: &mut SubParser) -> Result<Option<ExprPattern>, ParseError> {
    println!("FExpr test");

    let namespace_count = is_namespace_chain(parser)?;
    let has_identifier = match_tokens(parser, &[MATCH_IDENTIFIER])?;
    let access_chain = is_member_access_chain(parser)?;

    if namespace_count > 0 || access_chain.is_some() {
        println!("At F Expr");

        Ok(Some(FExpr(FExprPattern { namespace_count, has_identifier, access_chain })))
    } else { Ok(None) }
}


// ::= Identifier | Value
pub fn is_v_expression(parser: &mut SubParser) -> Result<Option<ExprPattern>, ParseError> {
    println!("Value test");

    let t1 = parser.peek_n(1)?.token_type;
    let t2 = parser.peek_n(2)?.token_type;
    if matches!(t1, TokenType::TLiteral(_)) && !matches!(t2,
        TokenType::RIGHT_ARROW | TokenType::METHOD_SPACE_ACCESS
        | TokenType::FIELD_SPACE_ACCESS | TokenType::LEFT_BRACKET)
    {
        parser.consume_n(1);
        println!("At V Expr");
        Ok(Some(VExpr))
    } else { Ok(None) }
}


// ::= '{' { Expr | Stmnt } '}'
pub fn is_block_expression(parser: &mut SubParser) -> Result<Option<ExprPattern>, ParseError> {
    println!("Block test");
    if !match_tokens(parser, &[MATCH_LEFT_BRACE])? {
        return Ok(None);
    }

    println!("At block expression");
    let mut block_members = Vec::<ParseMatch>::with_capacity(5);

    while !match_tokens(parser, &[MATCH_RIGHT_BRACE])? {
        block_members.push(find_next_match(parser)?)
    }
    

    Ok(Some(ExprPattern::BlockExpr(BlockExprPattern {
        members: if block_members.is_empty() {
            None
        } else { Some(block_members) }
    })))
}


pub fn is_b_expression(parser: &mut SubParser) -> Result<Option<ExprPattern>, ParseError> {
    // TODO implement this
    Ok(None)
}


// ::= Identifier '->'
pub fn is_namespace_access(parser: &mut SubParser) -> Result<bool, ParseError> {
    match_tokens(parser, &[MATCH_IDENTIFIER, MATCH_RIGHT_ARROW])
}


// ::= ':." Identifier
pub fn is_field_access(parser: &mut SubParser) -> Result<bool, ParseError> {
    match_tokens(parser, &[MATCH_FIELD_ACCESS, MATCH_IDENTIFIER])
}


// ::= '::'[Identifier]
pub fn is_method_access(parser: &mut SubParser) -> Result<bool, ParseError> {
    if match_tokens(parser, &[MATCH_METHOD_ACCESS])? {
        match_tokens(parser, &[MATCH_IDENTIFIER])?;
        Ok(true)
    } else { Ok(false) }
}


// { NamespaceAccess }-
pub fn is_namespace_chain(parser: &mut SubParser) -> Result<u32, ParseError> {
    let mut count = 0;
    while is_namespace_access(parser)? { count += 1; }
    Ok(count)
}


//  { { FieldAccess | MethodCall } [ MethodAccess ] }-
pub fn is_member_access_chain(
    parser: &mut SubParser
) -> Result<Option<Vec<MemberAccess>>, ParseError> {
    let mut access_chain = Vec::new();
    loop {
        if is_field_access(parser)? {
            access_chain.push(MemberAccess::Field);
            continue;
        }

        if is_method_access(parser)? {
            if match_tokens(parser, &[MATCH_LEFT_BRACKET])? {
                let arguments = is_arguments(parser)?;
                if !match_tokens(parser, &[MATCH_RIGHT_BRACKET])? {
                    validation_error(parser.peek_n(1)?, "]")?
                }
                access_chain.push(MemberAccess::MethodCall { arguments });
                continue;
            } else {
                if is_method_access(parser)? || is_field_access(parser)? {
                    validation_error(
                        parser.peek_n(1)?,
                        "End of F-Expr (Method identity calls must come last)",
                    )?
                }
                access_chain.push(MemberAccess::MethodAccess);
                break;
            }
        }
        break;
    }
    if access_chain.is_empty() { Ok(None) } else { Ok(Some(access_chain)) }
}

///////////////////
// Special Forms //
///////////////////


pub fn is_predicate_form(parser: &mut SubParser) -> Result<Option<PredicateForm>, ParseError> {
    let then_form = if match_tokens(parser, &[MATCH_RIGHT_ARROW])? {
        if let Some(expr) = is_expression(parser)? {
            Some(expr)
        } else { validation_error(parser.peek_n(1)?, "Expr")? }
    } else { None };

    let else_form = if match_tokens(parser, &[MATCH_COLON])? {
        if let Some(expr) = is_expression(parser)? {
            Some(expr)
        } else { validation_error(parser.peek_n(1)?, "Expr")? }
    } else { None };

    if then_form.is_some() || else_form.is_some() {
        Ok(Some(PredicateForm { then_form, else_form }))
    } else { Ok(None) }
}


//////////////////
// Type Grammar //
//////////////////

pub fn is_type(parser: &mut SubParser) -> Result<bool, ParseError> {
    let t1 = parser.peek_n(1)?;

    if matches!(t1.token_type, TokenType::IDENTIFIER) {
        parser.consume_n(1);
    } else if matches!(t1.token_type, TokenType::FN) {
        parser.consume_n(1);
        if !is_func_type(parser)? {
            validation_error(parser.peek_n(1)?, "Type<Fn>")?;
        } else { return Ok(true); }
    } else { return Ok(false); }

    let _ = is_array_type(parser)?;
    Ok(true)
}


pub fn is_array_type(parser: &mut SubParser) -> Result<bool, ParseError> {
    let mut t1 = parser.peek_n(1)?;
    if matches!(t1.token_type, TokenType::ANGLE_BRACKET_LEFT) {
        parser.consume_n(1);
        if let has_type = is_type(parser)? {
            if !has_type { return validation_error(t1, "Type"); }
        }
    } else { return Ok(false); }

    t1 = parser.peek_n(1)?;
    if matches!(t1.token_type, TokenType::ANGLE_BRACKET_RIGHT) {
        parser.consume_n(1);
        Ok(true)
    } else { validation_error(t1, '>') }
}


pub fn is_func_type(parser: &mut SubParser) -> Result<bool, ParseError> {
    let mut t1 = parser.peek_n(1)?;

    if matches!(t1.token_type, TokenType::ANGLE_BRACKET_LEFT) {
        parser.consume_n(1)
    } else { return validation_error(t1, "Func !!! '<' !!! ({Type}- | '_') ';' Type '>'"); }

    while matches!(is_type(parser)?, true) { /* Spin past parameter types */ }

    t1 = parser.peek_n(1)?;
    if matches!(t1.token_type, TokenType::TSyntactic(Syn::SemiColon)) {
        parser.consume_n(1);
        if !is_type(parser)? { return validation_error(t1, "Type"); }
    } else { return validation_error(t1, "Func '<' ({Type}- | '_') !!! ';' !!! Type '>'"); }

    t1 = parser.peek_n(1)?;
    if matches!(t1.token_type, TokenType::ANGLE_BRACKET_RIGHT) {
        parser.consume_n(1);
        Ok(true)
    } else { validation_error(t1, "Func '<' ({Type}- | '_') ';' Type !!! '>' !!!") }
}


pub fn is_modifiers(parser: &mut SubParser) -> Result<u32, ParseError> {
    match_multiple(parser, &[|t1| matches!(t1.token_type, TokenType::TModifier(_))])
}

