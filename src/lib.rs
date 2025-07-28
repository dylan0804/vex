use wasm_bindgen::prelude::*;

pub mod lexer;
pub mod parser;
pub mod interpreter;

#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}