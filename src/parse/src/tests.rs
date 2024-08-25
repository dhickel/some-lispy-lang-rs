use crate::lexer;
use crate::parser::ParserState;


// Test are not to ensure full correctness, but to ensure that all grammar patterns at least match and
// parse successfully
const S_EXPR: &str = "(test_func 1 2.0 30)";
const S_EXPR_OP: &str = "(- 10 20 30 (* (+ 10 10) (+ 20 -20)))";
const PRED_EXPR: &str = "((> 10 4) -> 420 : (* 6 9))";
const LAMBDA_EXPR: &str = "(=> |x| (* 10 x))";
const LAMBDA_EXPR_TYPES: &str = "(=> : int | x : int y : float | ((> x y) -> 1 : 0))";
const LAMBDA_EXPR_FORM: &str = "(|x| (* x 20))";


const FORMS: [&str; 6] = [
    S_EXPR,
    S_EXPR_OP,
    PRED_EXPR,
    LAMBDA_EXPR,
    LAMBDA_EXPR_TYPES,
    LAMBDA_EXPR_FORM,
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

