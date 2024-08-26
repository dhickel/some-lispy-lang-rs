use crate::token::TokenType::{TBuiltIn, TDefinition, TLiteral, TModifier, TOperation, TSyntactic};
use crate::token::BuiltIn::*;
use crate::token::Def::*;
use crate::token::Lit::*;
use crate::token::Mod::*;
use crate::token::Op::*;
use crate::token::Syn::*;
use crate::util::IString;


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
        ('!', '=') => Some(TOperation(BangEqual)),
        ('=', '=') => Some(TOperation(EqualEqual)),
        (':', '=') => Some(TSyntactic(ColonEqual)),
        ('#', 't') => Some(TLiteral(True)),
        ('#', 'f') => Some(TLiteral(False)),
        ('i', 'f') => Some(TBuiltIn(If)),
        ('=', '>') => Some(TBuiltIn(Lambda)),
        ('-', '>') => Some(TSyntactic(Arrow)),
        ('F', 'n') => Some(TBuiltIn(Fn)),
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
        "cond" => Some(TBuiltIn(Cond)),
        "print" => Some(TBuiltIn(Print)),
        "begin" => Some(TBuiltIn(Begin)),
        "fori" => Some(TBuiltIn(ForI)),
        "foreach" => Some(TBuiltIn(ForEach)),
        "while" => Some(TBuiltIn(While)),
        "cons" => Some(TBuiltIn(Cons)),
        "car" => Some(TBuiltIn(Car)),
        "cdr" => Some(TBuiltIn(Cdr)),
        "list" => Some(TBuiltIn(BuiltIn::List)),
       // "array" => Some(TBuiltIn(BuiltIn::Array)),
        "lacc" => Some(TBuiltIn(Lacc)),
        "let" => Some(TDefinition(DefineLet)),
        "func" => Some(TDefinition(DefineFunc)),
        "class" => Some(TDefinition(DefineClass)),
        "struct" => Some(TDefinition(DefineStruct)),
        "randi" => Some(TBuiltIn(Randi)),
        "randf" => Some(TBuiltIn(Randf)),
        _ => None
    }
}


// Modifiers are imported from lang::environment
pub fn match_modifier_token(input: &str) -> Option<TokenType> {
    match input {
        "@mut" => Some(TModifier(Mutable)),
        "@pub" => Some(TModifier(Public)),
        "@const" => Some(TModifier(Const)),
        "@opt" => Some(TModifier(Optional)),
        _ => None
    }
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    TSyntactic(Syn),
    TOperation(Op),
    TBuiltIn(BuiltIn),
    TDefinition(Def),
    TLiteral(Lit),
    TModifier(Mod),
}


impl TokenType {
    pub const LEFT_PAREN: TokenType = TSyntactic(LeftParen);
    pub const RIGHT_PAREN: TokenType = TSyntactic(RightParen);

    pub const LEFT_BRACE: TokenType = TSyntactic(LeftBrace);
    pub const RIGHT_BRACE: TokenType = TSyntactic(RightBrace);

    pub const LEFT_BRACKET: TokenType = TSyntactic(LeftBracket);
    pub const RIGHT_BRACKET: TokenType = TSyntactic(RightBracket);

    pub const ANGLE_BRACKET_LEFT: TokenType = TOperation(Op::Less);
    pub const ANGLE_BRACKET_RIGHT: TokenType = TOperation(Op::Greater);
    
    pub const FN: TokenType = TBuiltIn(BuiltIn::Fn);
    pub const LAMBDA_ARROW: TokenType = TBuiltIn(BuiltIn::Lambda);

    pub const ASSIGNMENT: TokenType = TSyntactic(Syn::ColonEqual);
    pub const IDENTIFIER: TokenType = TLiteral(Identifier);
    pub const NAMESPACE_ACCESS: TokenType = TSyntactic(Arrow);
    pub const METHOD_SPACE_ACCESS: TokenType = TSyntactic(DoubleColon);
    pub const FIELD_SPACE_ACCESS: TokenType = TSyntactic(ColonDot);
    pub const RIGHT_ARROW: TokenType = TSyntactic(Syn::Arrow);

    pub const BAR: TokenType = TSyntactic(Syn::Bar);
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
    ColonEqual,
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
    BangEqual,
    EqualEqual,
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
pub enum BuiltIn {
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
   // Array,
    Lambda,

    Fn,
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Def {
    DefineLet,
    DefineFunc,
    DefineClass,
    DefineStruct,
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Mod {
    Mutable,
    Public,
    Const,
    Optional,
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenData {
    String(IString),
    Integer(i64),
    Float(f64),
}


#[derive(Debug, PartialEq, Clone, Copy, )]
pub struct Token {
    pub token_type: TokenType,
    pub data: Option<TokenData>,
    pub line: u32,
    pub char: u32,
}