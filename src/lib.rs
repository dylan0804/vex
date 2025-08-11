use wasm_bindgen::prelude::*;

use crate::{interpreter::Interpreter, lexer::Lexer, parser::Parser};

pub mod errors;
pub mod interpreter;
pub mod lexer;
pub mod parser;

#[wasm_bindgen]
pub fn run(code: &str) -> Result<String, JsValue> {
    let mut lexer = Lexer::new(code);
    let tokens = lexer
        .tokenize()
        .map_err(|e| JsValue::from_str(&e.to_string()))?;
    let mut parser = Parser::new(tokens);
    let statements = parser
        .parse_statement()
        .map_err(|e| JsValue::from(&e.to_string()))?;

    let mut interpreter = Interpreter::new();
    interpreter
        .execute(statements)
        .map_err(|e| JsValue::from_str(&e.to_string()))?;

    let output = interpreter.get_output();

    serde_json::to_string(&output).map_err(|e| JsValue::from_str(&e.to_string()))
}
