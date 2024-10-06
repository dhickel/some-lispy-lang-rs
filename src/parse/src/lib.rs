use std::fs;
use std::path::{Path, PathBuf};
use sha2::Sha256;
use lang::token::Token;
use crate::parser::ParseResult;


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


pub struct ProjectData {
    pub org_name: String,
    pub modules: ModuleData
}


#[derive(Debug)]
pub struct ModuleData {
    pub name: String,
    pub files: Vec<SourceFile>,
    pub sub_modules: Vec<ModuleData>,
}

#[derive(Debug)]
pub struct SourceFile {
    pub path: PathBuf,
    pub hash: Sha256,
    pub parsed_ast: Option<ParseResult>
}


impl ModuleData {
    pub fn new(name: String) -> ModuleData {
        ModuleData {
            name,
            files: vec![],
            sub_modules: vec![],
        }
    }
}

fn map_project_dir(dir: &Path) -> ModuleData {
    if dir.is_dir() {
        let mut module = ModuleData::new(dir.file_name().unwrap().to_str().unwrap().to_string());
        for entry in fs::read_dir(dir).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();

            if path.is_dir() {
                module.sub_modules.push(map_project_dir(&path));
            } else {
                module.files.push(path);
            }
        }
        module
    } else { panic!("Not Directory") }
}