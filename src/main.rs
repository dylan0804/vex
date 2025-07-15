use crate::lexer::Lexer;

mod lexer;

fn main() {
    let mut lexer = Lexer::new("3 + 3".to_string());
    let token = lexer.tokenize().unwrap();
    println!("tokens {:?}", token);
}
