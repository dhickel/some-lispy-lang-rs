use std::cell::RefCell;
use std::io;
use std::io::Write;
use lasso::Rodeo;
use crate::eval::class_loader::ClassLoader;
use crate::eval::environment::Environment;
use vm;
use vm::op_codes::{OpCode, SmallInst};
use vm::op_codes::OpCode::OpReturn;
use vm::vm::{CompUnit, Vm};


pub mod parse;
pub mod lang;
pub mod eval;




// TODO init this better


#[allow(dead_code)]
fn main() {
    let mut chunk = CompUnit {
        code: Vec::<u8>::new()
    };
    
    chunk.code.push(OpReturn as u8);
    let mut vm = Vm::new(&mut chunk);
    vm.test();
    
    
    // let mut env = Environment::new();
    // let mut loader = RefCell::new(ClassLoader::default());
    // let mut string_cache = Rodeo::default();
    // 
    // println!("Size of Stuct: {}", std::mem::size_of::<SmallInst>());
    // 
    // loop {
    //     print!("> ");
    //     let mut input = String::new();
    //     match io::stdin().read_line(&mut input) {
    //         Ok(n) => {
    //             if input.trim() == "exit" { break; }
    //             if input.starts_with("$load-file") {
    //                 let file_name = input.replace("$load-file", "").trim().to_string();
    //                 println!("{}", eval::interpreter::file_eval(&env, &loader, &file_name))
    //             } else {
    //                 println!("{}", eval::interpreter::repl_eval(&env, &loader,  input))
    //             }
    //         }
    //         // }
    //         Err(error) => println!("error: {}", error),
    // 
    //         // match parse::lexer::process(input) {
    //         //     Ok(tokens) => {
    //         //         println!("Tokens: {:?}\n", tokens);
    //         //         let ast = parse::parser::process(tokens);
    //         //         println!("\nSyntax Tree: {:?}\n", ast)
    //         //
    //         //     }
    //         //     Err(s) => println!("Error Processing: {}", s)
    //         // }
    //     }
    // }
}