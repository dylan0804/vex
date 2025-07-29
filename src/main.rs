use crate::{interpreter::Interpreter, lexer::Lexer, parser::Parser};

mod interpreter;
mod lexer;
mod parser;

fn main() {
    let mut lexer = Lexer::new("print(\"{}\", 6 >= 5)".to_string());
    let tokens = lexer.tokenize().unwrap();
    println!("tokens is {:?}", tokens);
    let mut parser = Parser::new(tokens);
    let statements = parser.parse_statement().unwrap();
    println!("stmt is {:?}", statements);
    let mut int = Interpreter::new();
    let res = int.calculate(statements).unwrap();
    println!("{:?}", res);
}
