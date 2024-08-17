pub mod token;
pub mod lexer;
pub mod parser;
pub mod ast;

pub mod code_gen;
pub mod op_codes;
pub mod resolution;
pub mod types;
pub mod environment;


pub enum ParseError {
    TypeChecking(String),
    TypeParsing(String),
    Parsing(String),
}


impl ParseError {
    pub fn parse_error(str: String) -> Err<Self> {
        Err(Self::Parsing(str))
    }
}

