use crate::{interpreter::Interpreter, lexer::Lexer, parser::Parser};

mod errors;
mod interpreter;
mod lexer;
mod parser;

fn main() {
    let mut lexer = Lexer::new(

            "let x = 7\nif x > 10 { print(\"big\") } else if x > 5 { print(\"medium\") } else { print(\"small\") }"
        .to_string());
    let tokens = lexer.tokenize().unwrap();
    println!("tokens is {:?}\n", tokens);
    let mut parser = Parser::new(tokens);
    let statements = parser.parse_statement().unwrap();
    println!("stmt is {:#?}", statements);
    let mut int = Interpreter::new();
    let res = int.calculate(statements).unwrap();
    println!("{:?}", res);
}
