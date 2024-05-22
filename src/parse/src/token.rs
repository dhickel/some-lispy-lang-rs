use lang::util::IString;
use crate::token::Mod::*;
use crate::token::Syn::*;
use crate::token::Op::*;
use crate::token::Expr::*;
use crate::token::Def::*;
use crate::token::Lit::*;
use crate::token::TokenType::*;


pub fn match_single_token(input: char) -> Option<TokenType> {
    match input {
        '(' => Some(TSyntactic(LeftParen)),
        ')' => Some(TSyntactic(RightParen)),
        '{' => Some(TSyntactic(LeftBrace)),
        '}' => Some(TSyntactic(RightBrace)),
        '[' => Some(TSyntactic(LeftBracket)),
        ']' => Some(TSyntactic(RightBracket)),
        ',' => Some(TSyntactic(Comma)),
        '\\' => Some(TSyntactic(BackSlash)),
        '\'' => Some(TSyntactic(SingleQuote)),
        '"' => Some(TSyntactic(DoubleQuote)),
        '.' => Some(TSyntactic(Period)),
        '&' => Some(TSyntactic(Ampersand)),
        '`' => Some(TSyntactic(Grave)),
        ':' => Some(TSyntactic(Colon)),
        ';' => Some(TSyntactic(SemiColon)),
        //  '#' => Some(Syntactic(Pound)),
        '$' => Some(TSyntactic(Cache)),
        '@' => Some(TSyntactic(At)),
        '|' => Some(TSyntactic(Bar)),
        '~' => Some(TSyntactic(Tilde)),
        '=' => Some(TSyntactic(Equal)),
        '+' => Some(TOperation(Plus)),
        '-' => Some(TOperation(Minus)),
        '*' => Some(TOperation(Asterisk)),
        '/' => Some(TOperation(Slash)),
        '^' => Some(TOperation(Caret)),
        '%' => Some(TOperation(Percent)),
        '>' => Some(TOperation(Greater)),
        '<' => Some(TOperation(Less)),
        _ => None
    }
}


pub fn match_double_token(input: (char, char)) -> Option<TokenType> {
    match input {
        (':', ':') => Some(TSyntactic(DoubleColon)),
        (':', '.') => Some(TSyntactic(ColonDot)),
        ('+', '+') => Some(TOperation(PlusPlus)),
        ('-', '-') => Some(TOperation(MinusMinus)),
        ('>', '=') => Some(TOperation(GreaterEqual)),
        ('<', '=') => Some(TOperation(LessEqual)),
        ('!', '=') => Some(TOperation(BangEquals)),
        ('=', '=') => Some(TOperation(RefEqual)),
        (':', '=') => Some(TSyntactic(Assign)),
        ('#', 't') => Some(TLiteral(True)),
        ('#', 'f') => Some(TLiteral(False)),
        ('i', 'f') => Some(TExpression(If)),
        ('=', '>') => Some(TDefinition(Lambda)),
        ('-', '>') => Some(TSyntactic(Arrow)),
        _ => None,
    }
}


pub fn match_word_token(input: &str) -> Option<TokenType> {
    match input {
        "else" => Some(TSyntactic(Else)),
        "and" => Some(TOperation(And)),
        "or" => Some(TOperation(Or)),
        "nor" => Some(TOperation(Nor)),
        "xor" => Some(TOperation(Xor)),
        "xnor" => Some(TOperation(Xnor)),
        "nand" => Some(TOperation(Nand)),
        "not" => Some(TOperation(Negate)),
        "equals" => Some(TOperation(Equals)),
        "#nil" => Some(TLiteral(Nil)),
        "cond" => Some(TExpression(Cond)),
        "print" => Some(TExpression(Print)),
        "begin" => Some(TExpression(Begin)),
        "fori" => Some(TExpression(ForI)),
        "foreach" => Some(TExpression(ForEach)),
        "while" => Some(TExpression(While)),
        "cons" => Some(TExpression(Cons)),
        "car" => Some(TExpression(Car)),
        "cdr" => Some(TExpression(Cdr)),
        "list" => Some(TExpression(Expr::List)),
        "array" => Some(TExpression(Expr::Array)),
        "lacc" => Some(TExpression(Lacc)),
        "let" => Some(TDefinition(Define)),
        "class" => Some(TDefinition(DefineClass)),
        "struct" => Some(TDefinition(DefineStruct)),
        "randi" => Some(TExpression(Randi)),
        "randf" => Some(TExpression(Randf)),
        _ => None
    }
}


// Modifiers are imported from lang::environment
pub fn match_modifier_token(input: &str) -> Option<TokenType> {
    match input {
        "&mut" => Some(TModifier(Mutable)),
        "&pub" => Some(TModifier(Public)),
        "&null" => Some(TModifier(Nullable)),
        "&stat" => Some(TModifier(Static)),
        "&dyn" => Some(TModifier(Dynamic)),
        "&opt" => Some(TModifier(Optional)),
        "&do" => Some(TModifier(Do)),
        _ => None
    }
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    TSyntactic(Syn),
    TOperation(Op),
    TExpression(Expr),
    TDefinition(Def),
    TLiteral(Lit),
    TModifier(Mod),
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Syn {
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
    Period,
    Ampersand,
    Grave,
    Colon,
    SemiColon,
    DoubleColon,
    ColonDot,
    Pound,
    Cache,
    At,
    Bar,
    Tilde,
    Equal,
    Else,
    EqualGreater,
    Arrow,
    Assign,
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Op {
    List,
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
    Instance,
    Nil,
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Expr {
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
    Randi,
    Randf,
    Array,
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Def {
    Define,
    DefineFunc,
    DefineClass,
    DefineStruct,
    Lambda,
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Mod {
    Mutable,
    Public,
    Nullable,
    Static,
    Dynamic,
    Optional,
    Do,
}


#[derive(Debug, PartialEq, Clone)]
pub enum TokenData {
    String(IString),
    Integer(i64),
    Float(f64),
}


#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub data: Option<TokenData>,
    pub line: u32,
    pub char: u32,
}