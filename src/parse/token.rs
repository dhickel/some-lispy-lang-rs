use crate::parse::token::Lexical::*;
use crate::parse::token::Literal::*;
use crate::parse::token::Operation::*;
use crate::parse::token::Syntactic::*;
use crate::parse::token::Expression::*;
use crate::parse::token::Definition::*;
use crate::parse::token::Modifier::*;
use crate::parse::TokenType::*;

pub fn match_single_token(input: &str) -> Option<TokenType> {
    match input {
        "(" => Some(LexicalToken(LeftParen)),
        ")" => Some(LexicalToken(RightParen)),
        "{" => Some(LexicalToken(LeftBrace)),
        "}" => Some(LexicalToken(RightBrace)),
        "[" => Some(LexicalToken(LeftBracket)),
        "]" => Some(LexicalToken(RightBracket)),
        "," => Some(LexicalToken(Comma)),
        "\\" => Some(LexicalToken(BackSlash)),
        "'" => Some(LexicalToken(SingleQuote)),
        "\"" => Some(LexicalToken(DoubleQuote)),
        "." => Some(SyntacticToken(Period)),
        "&" => Some(SyntacticToken(Ampersand)),
        "`" => Some(SyntacticToken(Grave)),
        ":" => Some(SyntacticToken(Colon)),
        ";" => Some(SyntacticToken(SemiColon)),
        "#" => Some(SyntacticToken(Pound)),
        "$" => Some(SyntacticToken(Cache)),
        "@" => Some(SyntacticToken(At)),
        "|" => Some(SyntacticToken(Bar)),
        "~" => Some(SyntacticToken(Tilde)),
        "=" => Some(SyntacticToken(Equal)),
        "+" => Some(OperationToken(Plus)),
        "-" => Some(OperationToken(Minus)),
        "*" => Some(OperationToken(Asterisk)),
        "/" => Some(OperationToken(Slash)),
        "^" => Some(OperationToken(Caret)),
        "%" => Some(OperationToken(Percent)),
        ">" => Some(OperationToken(Greater)),
        "<" => Some(OperationToken(Less)),
        _ => None
    }
}

pub fn match_double_token(input: &str) -> Option<TokenType> {
    match input {
        "::" => Some(SyntacticToken(Type)),
        "++" => Some(OperationToken(PlusPlus)),
        "--" => Some(OperationToken(MinusMinus)),
        ">=" => Some(OperationToken(GreaterEqual)),
        "<=" => Some(OperationToken(LessEqual)),
        "!=" => Some(OperationToken(BangEquals)),
        "==" => Some(OperationToken(RefEqual)),
        "=>" => Some(SyntacticToken(EqualGreater)),
        "#t" => Some(LiteralToken(True)),
        "#f" => Some(LiteralToken(False)),
        ":=" => Some(ExpressionToken(Assign)),
        "if" => Some(ExpressionToken(If)),
        _ => None
    }
}

pub fn match_word_token(input: &str) -> Option<TokenType> {
    match input {
        "else" => Some(SyntacticToken(Else)),
        "and" => Some(OperationToken(And)),
        "or" => Some(OperationToken(Or)),
        "nor" => Some(OperationToken(Nor)),
        "xor" => Some(OperationToken(Xor)),
        "xnor" => Some(OperationToken(Xnor)),
        "nand" => Some(OperationToken(Nand)),
        "not" => Some(OperationToken(Negate)),
        "equals" => Some(OperationToken(Equals)),
        "#nil" => Some(LiteralToken(Nil)),
        "cond" => Some(ExpressionToken(Cond)),
        "print" => Some(ExpressionToken(Print)),
        "begin" => Some(ExpressionToken(Begin)),
        "fori" => Some(ExpressionToken(ForI)),
        "foreach" => Some(ExpressionToken(ForEach)),
        "while" => Some(ExpressionToken(While)),
        "cons" => Some(ExpressionToken(Cons)),
        "car" => Some(ExpressionToken(Car)),
        "cdr" => Some(ExpressionToken(Cdr)),
        "list" => Some(ExpressionToken(List)),
        "lacc" => Some(ExpressionToken(Lacc)),
        "define" => Some(DefinitionToken(Define)),
        "defunc" => Some(DefinitionToken(DefineFunc)),
        "def-class" => Some(DefinitionToken(DefineRecord)),
        "def-record" => Some(DefinitionToken(DefineRecord)),
        "lambda" => Some(DefinitionToken(Lambda)),
        "&mut" => Some(ModifierToken(Mutable)),
        "&pub" => Some(ModifierToken(Public)),
        "&priv" => Some(ModifierToken(Private)),
        "&stat" => Some(ModifierToken(Static)),
        "&dyn" => Some(ModifierToken(Dynamic)),
        "&opt" => Some(ModifierToken(Optional)),
        "&do" => Some(ModifierToken(Do)),
        _ => None
    }
}

pub enum TokenType {
    LexicalToken(Lexical),
    SyntacticToken(Syntactic),
    OperationToken(Operation),
    LiteralToken(Literal),
    ExpressionToken(Expression),
    DefinitionToken(Definition),
    ModifierToken(Modifier),
}

pub enum Lexical {
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

pub enum Syntactic {
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
    EqualGreater
}

pub enum Operation {
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

pub enum Literal {
    True,
    False,
    String,
    Int,
    Float,
    Identifier,
    Nil,
}

pub enum Expression {
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

pub enum Definition {
    Define,
    DefineFunc,
    DefineClass,
    DefineRecord,
    Lambda,
}

pub enum Modifier {
    Mutable,
    Public,
    Private,
    Static,
    Dynamic,
    Optional,
    Do,
}

pub enum TokenData {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
}

pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub data: TokenData,
    pub line: i32,
}