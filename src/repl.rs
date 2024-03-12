mod parse;

#[allow(dead_code)]
fn main() {
    parse::test::run_tests();
    
 


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