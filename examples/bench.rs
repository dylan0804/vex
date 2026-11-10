use vex::lexer::Lexer;

fn main() {
    let code = "suppose x = 1 + 2 * 3\n".repeat(50_000);

    let mut lexer = Lexer::new(&code);
    let tokens = lexer.tokenize().unwrap();
    println!("{} tokens", tokens.len());
}
