use crate::{interpreter::Interpreter, lexer::Lexer, parser::Parser};

mod errors;
mod interpreter;
mod lexer;
mod parser;

fn main() {
    let mut lexer = Lexer::new(
        "let x = 3\n if x < 3 { let y = 5 print(\"bitch is {}\", y)} else if y < 5 { print(\"hey is\") print(\"hey is\") print(\"hey is\") else if y < 5 { print(\"hey is\") print(\"hey is\") print(\"hey is\") } else { print(\"hey\") }}"

            .to_string(),
    );
    let tokens = lexer.tokenize().unwrap();
    println!("tokens is {:?}\n", tokens);
    let mut parser = Parser::new(tokens);
    let statements = parser.parse_statement().unwrap();
    println!("stmt is {:#?}", statements);
    // let mut int = Interpreter::new();
    // let res = int.calculate(statements).unwrap();
    // println!("{:?}", res);
}
