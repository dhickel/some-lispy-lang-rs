use parser::lexer;
use parser::parser::ParserState;
use crate::environment::{Environment, SubEnvironment};
use crate::resolution::Resolver;

const PRED_ELSE_ONLY: &str = "\
    let x: int = 10
    x := (+ x 10)
";

const BLOCK_EXPR2: &str = "\
let x :int = {
    let x: int =  10
    x := (+ x 10)
    (test 20 (* x 10 30))
}";


#[test]
fn test_compile() {
    let tokens = lexer::process(BLOCK_EXPR2).unwrap();
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