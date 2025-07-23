use crate::{interpreter::Interpreter, lexer::Lexer, parser::Parser};

mod lexer;
mod parser;
mod interpreter;

fn main() {
    let mut lexer = Lexer::new("let x = 5\n2 + 3".to_string());
    let tokens = lexer.tokenize().unwrap();
    let mut parser = Parser::new(tokens);
    let statements = parser.parse_statement().unwrap();
    println!("stmt is {:?}", statements);
    let mut int = Interpreter::new();
    let res = int.calculate(statements);
}
