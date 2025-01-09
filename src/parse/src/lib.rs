use std::fs;
use std::path::{Path, PathBuf};
use lang::{module, Sha256Hash};
use sha2::Sha256;
use lang::ast::AstNode;
use lang::token::Token;
use lang::util::{get_sha256, read_file, IString, SCACHE};
use crate::parser::{ParseResult, ParserState};


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
    IO(std::io::Error),
    Lex(String),
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


#[derive(Debug)]
pub struct ModuleData {
    pub name: IString,
    files: Vec<SourceFile>,
    sub_modules: Vec<ModuleData>,
}

#[derive(Debug)]
pub struct SourceFile {
    pub path: PathBuf,
    pub hash: Sha256Hash,
    pub ast_nodes: Option<Vec<AstNode>>,
}

impl SourceFile {
    pub fn parse(&mut self) -> Result<(), ParseError> {
        let source_str = read_file(&self.path)
            .map_err(ParseError::IO)?;

        let source_tokens = lexer::process(&source_str)
            .map_err(ParseError::Lex)?;

        let mut parser_state = ParserState::new(source_tokens);
        let ast = parser_state.process().map(|r| r.root_expressions)?;
        self.ast_nodes = Some(ast);
        Ok(())
    }
}

impl ModuleData {
    pub fn new(name: String) -> ModuleData {
        let name = SCACHE.intern(name);
        ModuleData {
            name,
            files: vec![],
            sub_modules: vec![],
        }
    }

    pub fn get_sub_module(&self, name: IString) -> Option<&ModuleData> {
        self.sub_modules.iter().find(|sm| sm.name == name)
    }

    pub fn get_sub_module_mut(&mut self, name: IString) -> Option<&mut ModuleData> {
        self.sub_modules.iter_mut().find(|sm| sm.name == name)
    }

    pub fn parse_module(&mut self) -> Result<(), ParseError> {
        for file in &mut self.files {
            file.parse()?;
        }
        for module in &mut self.sub_modules {
            module.parse_module()?;
        }
        Ok(())
    }

    pub fn get_module_ast(&self) -> Vec<AstNode> {
        let mut ast_nodes = vec![];
        for file in &self.files {
            if let Some(ast) = &file.ast_nodes {
                ast_nodes.append(&mut ast.clone())
            }
        }
        ast_nodes
    }
}

impl SourceFile {
    pub fn has_new_hash(&mut self) -> (bool, Sha256Hash) {
        let hash = get_sha256(&self.path).unwrap(); //FIxme
        let has_changed = self.hash != hash;
        self.hash = hash;
        (has_changed, hash)
    }
}

pub fn map_project_dir(dir: &Path) -> ModuleData {
    if dir.is_dir() {
        let mut module = ModuleData::new(dir.file_name().unwrap().to_str().unwrap().to_string()); //FIXME
        for entry in fs::read_dir(dir).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();

            if path.is_dir() {
                let module_data = map_project_dir(&path);
                let exists = module.sub_modules.iter().any(|sm| sm.name == module_data.name);
                if !exists {
                    module.sub_modules.push(module_data);
                }
            } else {
                let exists = module.files.iter().any(|f| f.path == path);
                if !exists {
                    let source_hash = get_sha256(&path).unwrap(); // FIXME
                    let source = SourceFile {
                        path,
                        hash: source_hash,
                        ast_nodes: None,
                    };
                    module.files.push(source);
                }
            }
        }
        module
    } else { panic!("Not a Directory") }
}

pub fn load_project(org_name: IString, path: &Path) {
    let mut project_data = map_project_dir(path);
    project_data.parse_module().unwrap();
    println!("Project: {:?}", project_data)
}

