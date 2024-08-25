use crate::lexer;
use crate::parser::ParserState;


// Test are not to ensure full correctness, but to ensure that the majority of grammar patterns
// at least match and parse successfully

const S_EXPR: &str = "(test_func 1 2.0 30)";
const S_EXPR_OP: &str = "(- 10 20 30 (* (+ 10 10) (+ 20 -20)))";
const PRED_EXPR: &str = "((> 10 4) -> 420 : (* 6 9))";
const PRED_ELSE_ONLY: &str = "let x : int = ((fake_function) : 10)";
const LAMBDA_EXPR: &str = "(=> |x| (* 10 x))";
const LAMBDA_EXPR_TYPES: &str = "(=> : int |x: int y: float| ((> x y) -> 1 : 0))";
const LAMBDA_EXPR_FORM: &str = "(|x| (* x 20))";
const VALUES: &str = "10 Identifier (* 20 30) 20 test";
const NAME_SPACE: &str = " namespace->::Test[function call]";
const NAME_SPACE2: &str = " namespace->::Test[function call]:.field";
const FEXPR: &str = " ::Test[function call]:.field::func_call2[10 20 30]";
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


const FORMS: [&str; 16] = [
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
    FEXPR,
    S_F_EXPR,
    LET,
    ASSIGN,
    BLOCK_EXPR,
    BLOCK_EXPR2,
];


#[test]
fn parser_integration() {
    FORMS.iter().for_each(|s| parse_instance(s))
}


fn parse_instance(input: &str) {
    let tokens = lexer::process(input).unwrap();
    println!("Tokens: ");
    tokens.iter().for_each(|t| println!("{:?}", t));
    let mut parser = ParserState::new(tokens);
    let ast = parser.process().unwrap();
    println!("Ast: {:?}\n\n", ast)
}

