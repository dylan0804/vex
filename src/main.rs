use crate::{lexer::Lexer, parser::Parser};

mod lexer;
mod parser;
mod interpreter;

fn main() {
    let mut lexer = Lexer::new("let x = 5".to_string());
    let tokens = lexer.tokenize().unwrap();
    // let mut parser = Parser::new(tokens);
    // let a = parser.parse_expression().unwrap();

    // let res = interpreter::evaluate(&a).unwrap();
    // println!("res is {}", res);
    println!("tokens is {:?}", tokens);
}
