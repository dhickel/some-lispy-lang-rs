use std::io;
use std::io::Write;
use std::time::SystemTime;
use crate::eval::environment::Environment;

pub mod parse;
pub mod lang;
pub mod eval;

#[allow(dead_code)]
fn main() {
    let mut env = Environment::new();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();

        let duration_since_epoch = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap();
        let t = duration_since_epoch.as_nanos();
        match io::stdin().read_line(&mut input) {
            Ok(n) => {
                if input.trim() == "exit" {
                    break;
                }
                println!("{}", eval::interpreter::repl_eval(&mut env, input))
            }
            // }
            Err(error) => println!("error: {}", error),

            // match parse::lexer::process(input) {
            //     Ok(tokens) => {
            //         println!("Tokens: {:?}\n", tokens);
            //         let ast = parse::parser::process(tokens);
            //         println!("\nSyntax Tree: {:?}\n", ast)
            //         
            //     }
            //     Err(s) => println!("Error Processing: {}", s)
            // }
        }
    }
}