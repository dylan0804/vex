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
    let result = interpreter
        .calculate(statements)
        .map_err(|e| JsValue::from_str(&e.to_string()))?;

    serde_json::to_string(&result).map_err(|e| JsValue::from_str(&e.to_string()))
}
