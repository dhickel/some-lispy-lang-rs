mod parse;
use std::io::Write;

fn main() {
    parse::test::test_number_lexing();
    
 


    // loop {
    //     print!("> ");
    //     io::stdout().flush().unwrap();
    // 
    //     let mut input = String::new();
    // 
    //     match io::stdin().read_line(&mut input) {
    //         Ok(n) => {
    //             if input.trim() == "exit" {
    //                 break;
    //             }
    // 
    //             println!("You entered: {}", input);
    //         }
    //         Err(error) => println!("error: {}", error),
    //     }
    // }
}