use lang::token::Token;


pub mod lexer;
pub mod parser;

pub mod grammar;
mod tests;


#[derive(Debug)]
pub enum ParseError {
    TypeChecking(String),
    TypeParsing(String),
    Parsing(String),
    Validation(String),
}


impl ParseError {
    pub fn parse_error<T>(str: String) -> Result<T, ParseError> {
        Err(Self::Parsing(str))
    }

    pub fn validation_error<T>(str: String, line: u32, char: u32) -> Result<T, ParseError> {
        Err(Self::Validation(format!("Line: {}, Char: {}, {:?}", line, char, str)))
    }

    pub fn parsing_error<T>(token: &Token, error_msg: &str) -> Result<T, ParseError> {
        Err(Self::Parsing(format!("Line: {}, Char: {}, {:?}", token.line, token.char, error_msg)))
    }
}

