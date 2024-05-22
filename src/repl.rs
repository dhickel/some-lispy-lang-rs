extern crate core;


use std::time::{SystemTime, UNIX_EPOCH};
use lang::util::SCACHE;
use parser::code_gen::CompUnit;
use parser::environment::{Environment, MetaSpace};

use parser::token::{Lit, TokenData, TokenType};


use vm;

use vm::vm::*;


macro_rules! nano_time {
    () => {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("")
            .as_nanos()
    };
}


#[allow(dead_code)]
fn main() {
    let mut meta_space = MetaSpace::default();
    let env = Environment::new(&mut meta_space);
    // let input = "(define x 10) (define val 0) (while (> x  0) (:= x (-- x)) (:= val (+ val x)) (define meh val)) meh".to_string();
    let input = "(defunc fib ::F<int; int>  [n ::int] )))  (fib 10) ".to_string();
    //let input = "(define x &mut 1) (define y  &mut 10) (while (> y 0) (:= y (-- y)) (:= x (* x  2))) x";
    //let input = "(defunc even ::int [x ::int] (if (equals (% x 2) 0) 1 0)) (even 7)";
   //let input = "(defunc dbl ::int [n ::int] (if (> n 20) n (dbl (* n 2)))) (define a ::int (dbl 2)) a ";
  // let input = "(defunc dbl ::F<int; int> [n ::int] (* n 2)) (dbl 11)".to_string();
   // let input = "(defunc dbl ::int [n ::int] (* n 2)) (define dlb2 dlb)".to_string(); 
    let input = "(defunc dbl ::F<int; int> [n ::int] (* n 2)) (defunc dbl2 ::F<int; int>  [func ::F<int; int> val ::int] (func(func(val)))) (dbl2 dbl 10)"; 
   // let input = "(define x ::int 10) (define y x) (:= x 11) y";
    //let input = "(define x ::int::int->::int (=> (x ::int y ::int) (* x y))) (define y ::int::int->::int::int->::int (=> (f ::int::int->::int val ::int) (f val)) (y 10)".to_string();
   // let input = "(define x ::Arr<int> (10 20 30)) ";
    let input = "let fib = (=> ::int (n ::int){ (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))) }) let x = (fib 10) x";
    
    

    let t = nano_time!();
    let tokens = parser::lexer::process(input.to_string()).expect("Token Err");


    
    println!("tokens: {:?}", tokens);

    let mut ast = parser::parser::process(tokens).expect("Parse Err");
    println!("\nast: {:?}", &ast);

    let resolved = parser::resolution::resolve_types(&mut ast, env);

    println!("Resolved: {}", resolved);

    let mut comp_unit = CompUnit {
        meta_space: &mut meta_space,
        curr_ns: 0,
        ns_code: Vec::<u8>::with_capacity(100),
    };

    parser::code_gen::code_gen(ast.root_expressions, &mut comp_unit).expect("gen Err");
    //comp_unit.ns_code.push(OpCode::ReturnVal as u8);

    let mut vm = Vm::new(meta_space);
    vm.run();
    vm.print_stack()
    


    // let mut chunk = CompUnit {
    //     code: Vec::<u8>::new(),
    //     constants: Vec::<u8>::new(),
    // };
    //
    //
    //
    // let v1_idx = chunk.push_constant(&1001_i64) as u8;
    // let v2_idx = chunk.push_constant(&123123123_i64) as u8;
    // let v3_idx = chunk.push_constant(&6456_i64) as u8;
    // let v4_idx = chunk.push_constant(&6323_i64) as u8;
    // let v5_idx = chunk.push_constant(&1001_i64) as u8;
    // let v6_idx = chunk.push_constant(&5645641_i64) as u8;
    // let v7_idx = chunk.push_constant(&1001_i64) as u8;
    // let v8_idx = chunk.push_constant(&001_i64) as u8;
    //
    // chunk.write_operand(OpCode::Ldc as u8);
    // chunk.write_operand(v8_idx);
    // chunk.write_op_code(OpCode::RtnBool);
    //
    //
    //

    //
    //
    // vm.run();
    //
    // let mut chunk = CompUnit {
    //     code: Vec::<u8>::new()
    // };
    //
    // chunk.code.push(Constant as u8);
    // chunk.code.push(Return as u8);
    // chunk.code.push(Exit as u8);
    // let mut vm = Vm::new(&mut chunk);
    // vm.run();


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
    //         //         let ast = parse::parse::process(tokens);
    //         //         println!("\nSyntax Tree: {:?}\n", ast)
    //         //
    //         //     }
    //         //     Err(s) => println!("Error Processing: {}", s)
    //         // }
    //     }
    // }
}