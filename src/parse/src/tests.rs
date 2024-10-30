use std::path::Path;
use lang::util::{IString, SCACHE};
use crate::{lexer, load_project};
use crate::parser::ParserState;


// Test are not to ensure full correctness, but to ensure that the majority of grammar patterns
// at least match and parse successfully

const S_EXPR: &str = "(test_func 1 2.0 30)";
const S_EXPR_OP: &str = "(- 10 20 30 (* (+ 10 10) (+ 20 -20)))";
const PRED_EXPR: &str = "((> 10 4) -> 420 : (* 6 9))";
const PRED_ELSE_ONLY: &str = "let x : int = ((fake_function) : 10)";
const LAMBDA_EXPR: &str = "(=> |x| (* 10 x))";
const LAMBDA_EXPR_TYPES: &str = "(=> : int | x: int y: float| ((> x y) -> 1 : 0))";
const LAMBDA_EXPR_FORM: &str = "(|x| (* x 20))";
const VALUES: &str = "10 Identifier (* 20 30) 20 test";
const NAME_SPACE: &str = " namespace->::Test[function call]";
const NAME_SPACE2: &str = " namespace->::Test[function call]:.field";
const F_EXPR: &str = " ::Test[function call]:.field::func_call2[10 20 30]";
const S_F_EXPR: &str = " (::Test[function call]:.field::func_call2[10 20 30] 10 20 30)";
const LET: &str = " let x : int  = (::Test[function call]:.field::func_call2[10 20 30] 10 20 30)";
const ASSIGN: &str = "x  := (* 10 20)";
const BLOCK_EXPR: &str = "\
{
    let x: int =  10
    x := (+ x 10)
    (test 20 (* x 10 30))
}";
const BLOCK_EXPR2: &str = "\
let x :int = {
    let x: int =  10
    x := (+ x 10)
    (test 20 (* x 10 30))
}";

const LAMBDA_BLOCK : &str = "
let x : Fn<int;int> = (=> |y| {
    let x: int =  10
    x := (+ x 10)
    (* x y 10 30)
})";
const FUNC_LET: &str = "let x: Fn<int int; int> = (=> |x y| (* x y))";


const FORMS: [&str; 18] = [
    S_EXPR,
    S_EXPR_OP,
    PRED_EXPR,
    LAMBDA_EXPR,
    LAMBDA_EXPR_TYPES,
    LAMBDA_EXPR_FORM,
    PRED_ELSE_ONLY,
    VALUES,
    NAME_SPACE,
    NAME_SPACE2,
    F_EXPR,
    S_F_EXPR,
    LET,
    ASSIGN,
    BLOCK_EXPR,
    BLOCK_EXPR2,
    LAMBDA_BLOCK,
    FUNC_LET
];

const TEST_PROJECT: &str = "/home/mindspice/code/rust/some-lispy-lang-rs/test_project";


#[test]
fn parser_integration() {
   // FORMS.iter().for_each(|s| parse_instance(s))
    
    // parse_instance("(:.member obj 10 20 30)");
    // parse_instance("(NS->member obj 10 20 30)");
    // parse_instance("(member obj 10 20 30)");

    let path = Path::new("/home/hickelpickle/Documents/TestFolder");
    load_project(SCACHE.const_string, Path::new(TEST_PROJECT))
}


fn parse_instance(input: &str) {
    let tokens = lexer::process(input).unwrap();
    println!("Tokens: ");
    tokens.iter().for_each(|t| println!("{:?}", t));
    let mut parser = ParserState::new(tokens);
    let ast = parser.process().unwrap();
    println!("Ast: {:?}\n\n", ast)
}

