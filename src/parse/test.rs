use crate::parse::lexer;

//  fn main(){
//      test_number_lexing();
// }

pub fn test_number_lexing() -> bool {
    let input  = "(* 2000.00  100 200 1 99 2.0 0)".to_string();
    let result = lexer::process(input).expect("TODO: panic message");
    println!("{:?}", result);
    true
}