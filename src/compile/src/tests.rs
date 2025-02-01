use parser::lexer;
use parser::parser::ParserState;
use crate::environment::{Environment, SubEnvironment};
use crate::resolution::Resolver;

const PRED_ELSE_ONLY: &str = "\
    let x: Int = 10
    x := (+ x 10)
";

const BLOCK_EXPR2: &str = "\
let x :Int = {
    let x : Int =  10
    x := (+ x 10)
}";

const S_EXPR_OP: &str = "\
let y: Int  = 11 \
let x : Int = (- 10 20 30 (* (+ 10  y) (+ 20 -20)))\
";

const LAMBDA_BLOCK : &str = "
let x : Fn<Int;Int> = (=> |y : Int| {
    let x: Int =  10
    (* x y 10 30)
})";


#[test]
fn test_compile() {
    let tokens = lexer::process(LAMBDA_BLOCK).unwrap();
    let mut parser = ParserState::new(tokens);
    let ast = parser.process().unwrap();

    println!("Ast: {:?}", ast);

    let env = Environment::default();
    let mut sub_env = env.new_sub_env(0);
    let mut resolved = Resolver::new(ast.root_expressions, &mut sub_env);
    resolved.resolve(4).expect("Failed to resolve");
    println!("Resolver:{:?} ", resolved);
    assert!(resolved.is_resolved(), "Failed to full resolve test code");
}