use crate::parse::{Def, Expr, lexer, Lit, Syn, TokenData};
use crate::parse::TokenType::{TDefinition, TExpression, TLiteral, TSyntactic};

//  fn main(){
//      test_number_lexing();
// }
// #[allow(dead_code)]
// pub fn run_tests() {
//     test_number_lexing();
//     test_string_lexing();
//     test_keyword_identity_lexing();
//     test_type_lexing();
// }
// pub fn test_number_lexing() {
//     let input = "(* 2000.00  100 1 -99 2.0 0 0.0)".to_string();
//     let result = lexer::process(input).expect("Lexing Failed");
//     let result1 = result.get(2).unwrap();
//     assert!(matches!(result1.token_type, Literal(Lit::Float)));
//     assert!(matches!(result1.data, Some(TokenData::Float(value)) if value == 2000.0));
// 
//     let result2 = result.get(3).unwrap();
//     assert!(matches!(result2.token_type, Literal(Lit::Int)));
//     assert!(matches!(result2.data, Some(TokenData::Integer(value)) if value == 100));
// 
// 
//     let result3 = result.get(4).unwrap();
//     assert!(matches!(result3.token_type, Literal(Lit::Int)));
//     assert!(matches!(result3.data, Some(TokenData::Integer(value)) if value == 1));
// 
//     let result4 = result.get(5).unwrap();
//     assert!(matches!(result4.token_type, Literal(Lit::Int)));
//     assert!(matches!(result4.data, Some(TokenData::Integer(value)) if value == -99));
// 
//     let result5 = result.get(6).unwrap();
//     assert!(matches!(result5.token_type, Literal(Lit::Float)));
//     assert!(matches!(result5.data, Some(TokenData::Float(value)) if value == 2.0));
// 
//     let result6 = result.get(7).unwrap();
//     assert!(matches!(result6.token_type, Literal(Lit::Int)));
//     assert!(matches!(result6.data, Some(TokenData::Integer(value)) if value == 0));
// 
//     let result7 = result.get(8).unwrap();
//     assert!(matches!(result7.token_type, Literal(Lit::Float)));
//     assert!(matches!(result7.data, Some(TokenData::Float(value)) if value == 0.0));
// 
// }
// 
// pub fn test_string_lexing() {
//     let input = "(define x \"String Literal\")".to_string();
//     let result = lexer::process(input).expect("Lexing Failed");
//     let result1 = result.get(3).unwrap();
//     assert!(matches!(result1.data, Some(TokenData::String(ref value)) if value == "String Literal"));
// }
// 
// pub fn test_keyword_identity_lexing(){
//     let input = "(define x 10)
//     (fori 0 10 2)
//     (TestFunction1)
//     (test_Function2)
//     (test-Function3)".to_string();
// 
//     let result = lexer::process(input).expect("Lexing Failed");
//     assert!(matches!(result.get(1).unwrap().token_type, Definition(Def::Define)));
//     assert!(matches!(result.get(2).unwrap().token_type, Literal(Lit::Identifier)));
//     assert!(matches!(result.get(6).unwrap().token_type, Expression(Expr::ForI)));
//     assert!(matches!(result.get(12).unwrap().token_type, Literal(Lit::Identifier)));
//     assert!(matches!(result.get(15).unwrap().token_type, Literal(Lit::Identifier)));
//     assert!(matches!(result.get(18).unwrap().token_type, Literal(Lit::Identifier)));
// 
// }
// 
// pub fn test_type_lexing(){
//     let input = "(define x ::int 10)".to_string();
//     let result = lexer::process(input).expect("Lexing Failed");
//     assert!(matches!(result.get(3).unwrap().token_type, Syntactic(Syn::DoubleColon)));
//     assert!(matches!(result.get(3).unwrap().data, Some(TokenData::String(ref value)) if value == "int"));
// }
// 
// pub fn test_quote_lexing() {
//     let input = "define x `(* 10 30 (+ 30 20))".to_string();
//     let result = lexer::process(input).expect("Lexing Failed");
// }