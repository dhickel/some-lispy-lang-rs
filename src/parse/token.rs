use std::string::String;
use crate::parse::token::Lex::*;
use crate::parse::token::Lit::*;
use crate::parse::token::Op::*;
use crate::parse::token::Syn::*;
use crate::parse::token::Expr::*;
use crate::parse::token::Def::*;
use crate::parse::token::Mod::*;
use crate::parse::TokenType::*;

pub fn match_single_token(input: char) -> Option<TokenType> {
    match input {
        '(' => Some(Lexical(LeftParen)),
        ')' => Some(Lexical(RightParen)),
        '{' => Some(Lexical(LeftBrace)),
        '}' => Some(Lexical(RightBrace)),
        '[' => Some(Lexical(LeftBracket)),
        ']' => Some(Lexical(RightBracket)),
        ',' => Some(Lexical(Comma)),
        '\\' => Some(Lexical(BackSlash)),
        '\'' => Some(Lexical(SingleQuote)),
        '"' => Some(Lexical(DoubleQuote)),
        '.' => Some(Syntactic(Period)),
        '&' => Some(Syntactic(Ampersand)),
        '`' => Some(Syntactic(Grave)),
        ':' => Some(Syntactic(Colon)),
        ';' => Some(Syntactic(SemiColon)),
        '#' => Some(Syntactic(Pound)),
        '$' => Some(Syntactic(Cache)),
        '@' => Some(Syntactic(At)),
        '|' => Some(Syntactic(Bar)),
        '~' => Some(Syntactic(Tilde)),
        '=' => Some(Syntactic(Equal)),
        '+' => Some(Operation(Plus)),
        '-' => Some(Operation(Minus)),
        '*' => Some(Operation(Asterisk)),
        '/' => Some(Operation(Slash)),
        '^' => Some(Operation(Caret)),
        '%' => Some(Operation(Percent)),
        '>' => Some(Operation(Greater)),
        '<' => Some(Operation(Less)),
        _ => None
    }
}

pub fn match_double_token(input: (char, char)) -> Option<TokenType> {
    match input {
        (':', ':') => Some(Syntactic(Type)),
        ('+', '+') => Some(Operation(PlusPlus)),
        ('-', '-') => Some(Operation(MinusMinus)),
        ('>', '=') => Some(Operation(GreaterEqual)),
        ('<', '=') => Some(Operation(LessEqual)),
        ('!', '=') => Some(Operation(BangEquals)),
        ('=', '=') => Some(Operation(RefEqual)),
        ('=', '>') => Some(Syntactic(EqualGreater)),
        (':', '=') => Some(Expression(Assign)),
        ('#', 't') => Some(Literal(True)),
        ('#', 'f') => Some(Literal(False)),
        ('i', 'f') => Some(Expression(If)),
        _ => None,
    }
}

pub fn match_word_token(input: &str) -> Option<TokenType> {
    match input {
        "else" => Some(Syntactic(Else)),
        "and" => Some(Operation(And)),
        "or" => Some(Operation(Or)),
        "nor" => Some(Operation(Nor)),
        "xor" => Some(Operation(Xor)),
        "xnor" => Some(Operation(Xnor)),
        "nand" => Some(Operation(Nand)),
        "not" => Some(Operation(Negate)),
        "equals" => Some(Operation(Equals)),
        "#nil" => Some(Literal(Nil)),
        "cond" => Some(Expression(Cond)),
        "print" => Some(Expression(Print)),
        "begin" => Some(Expression(Begin)),
        "fori" => Some(Expression(ForI)),
        "foreach" => Some(Expression(ForEach)),
        "while" => Some(Expression(While)),
        "cons" => Some(Expression(Cons)),
        "car" => Some(Expression(Car)),
        "cdr" => Some(Expression(Cdr)),
        "list" => Some(Expression(List)),
        "lacc" => Some(Expression(Lacc)),
        "define" => Some(Definition(Define)),
        "defunc" => Some(Definition(DefineFunc)),
        "def-class" => Some(Definition(DefineClass)),
        "def-record" => Some(Definition(DefineRecord)),
        "lambda" => Some(Definition(Lambda)),
        _ => None
    }
}

pub fn match_modifier_token(input: &str) -> Option<TokenType> {
    match input {
        "&mut" => Some(Modifier(Mutable)),
        "&pub" => Some(Modifier(Public)),
        "&priv" => Some(Modifier(Private)),
        "&stat" => Some(Modifier(Static)),
        "&dyn" => Some(Modifier(Dynamic)),
        "&opt" => Some(Modifier(Optional)),
        "&do" => Some(Modifier(Do)),
        _ => None
    }
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    Lexical(Lex),
    Syntactic(Syn),
    Operation(Op),
    Literal(Lit),
    Expression(Expr),
    Definition(Def),
    Modifier(Mod),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Lex {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    BackSlash,
    SingleQuote,
    DoubleQuote,
    EOF,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Syn {
    Period,
    Ampersand,
    Grave,
    Colon,
    SemiColon,
    Type,
    Pound,
    Cache,
    At,
    Bar,
    Tilde,
    Equal,
    Else,
    EqualGreater,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Op {
    And,
    Or,
    Nor,
    Xor,
    Xnor,
    Nand,
    Negate,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Caret,
    Percent,
    PlusPlus,
    MinusMinus,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Equals,
    BangEquals,
    RefEqual,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Lit {
    True,
    False,
    String,
    Int,
    Float,
    Identifier,
    Nil,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Expr {
    Assign,
    If,
    Cond,
    Print,
    Begin,
    ForI,
    ForEach,
    While,
    Cons,
    Car,
    Cdr,
    List,
    Lacc,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Def {
    Define,
    DefineFunc,
    DefineClass,
    DefineRecord,
    Lambda,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Mod {
    Mutable,
    Public,
    Private,
    Static,
    Dynamic,
    Optional,
    Do,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenData {
    String(String),
    Integer(i64),
    Float(f64),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub data: Option<TokenData>,
    pub line: i32,
    pub char: i32,
}