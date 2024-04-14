extern crate core;


use std::time::{SystemTime, UNIX_EPOCH};
use ahash::AHashMap;

use parser::environment::Context;
use parser::op_codes::OpCode;

use parser::util::{CompUnit, SCACHE};
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
    let mut comp_unit = CompUnit {
        code: vec![],
        constants: Vec::<[u8; 8]>::with_capacity(100),
        existing_u64s: AHashMap::<u64, u16>::with_capacity(50)
    };

    let u64 = SCACHE.const_float;


    println!("size of u64{}", std::mem::size_of_val(&u64));

    let context = Context::default();
    let input = "(+ 2 30 40 (+ 20 20))".to_string();

    let t = nano_time!();
    let tokens = parser::lexer::process(input).expect("Token Err");

    let mut ast = parser::parser::process(tokens).expect("Parse Err");

    let resolved = parser::resolution::resolve_types(&mut ast, context);
    println!("Resolved: {}", resolved);
    parser::code_gen::code_gen(ast, &mut comp_unit).expect("gen Err");


    comp_unit.write_op_code(OpCode::RtnI64);
    //comp_unit.write_op_code(OpCode::RtnI64);
   // comp_unit.write_op_code(OpCode::Exit);
    
  //  comp_unit.write_op_code(OpCode::Exit);

    //comp_unit.write_op_code(OpCode::Exit);
    println!("Proc Time: {} ns", nano_time!() - t);
    println!("comp unit: {:?}", comp_unit);
    println!("Decoded: {:?}", parser::op_codes::decode(comp_unit.clone()));


    let mut vm = Vm::new(&mut comp_unit);
    vm.run();
    vm.print_stack();


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