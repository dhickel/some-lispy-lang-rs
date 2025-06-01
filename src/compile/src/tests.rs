use lang::ast::Resolvable;
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

const LAMBDA_BLOCK: &str = "
let x :Fn<I64;I64> = (=> :I64 |y :I64| {
    let x: I64 = 10
    (* x y 10 30)
})";


const PARAM_TEST: &str = "
let x: I64 = 10
(+ x x)
";

#[test]
fn test_compile() {
    let tokens = lexer::process(PARAM_TEST).unwrap();
    let mut parser = ParserState::new(tokens);
    let ast = parser.process().unwrap();

    println!("Ast In: {:?}", ast);

    let env = Environment::default();
    let mut sub_env = env.new_sub_env(0);
    let mut resolved = Resolver::new(ast.root_expressions, &mut sub_env);
    let (res, asts) = resolved.resolve(10).expect("Failed to resolve");
    
    let mut r_vec = vec![];
    let mut ur_vec = vec![];

    asts.iter().for_each(|ast| {
       if ast.is_resolved() {  r_vec.push(ast); } else { ur_vec.push(ast) }
    });

    println!("\n\nResolved: {:?}", r_vec);
    println!("\n\nUn Resolved: {:?}", ur_vec);
    
    assert!(res, "Failed to full resolve test code");
    
    
    

}