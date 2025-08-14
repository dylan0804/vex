use std::collections::HashMap;

use anyhow::{anyhow, Context, Result};

#[derive(Debug, PartialEq, Clone)]
pub enum PrintType {
    Whisper,
    Shout,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Number(f64),
    String(String),
    Add,
    Subtract,
    Multiply,
    Divide,
    LeftParent,
    RightParent,
    LeftBrace,
    RightBrace,
    RightBracket,
    LeftBracket,
    Suppose,
    Identifier(String),
    Assign,
    Print(PrintType),
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Comma,
    True,
    False,
    Maybe,
    Perhaps,
    Wander,
    Contemplate,
    SupposeIts,
    Nah,
    Equal,
    Pipeline,
    Eof, // add more operators later
}

pub struct Lexer {
    position: usize,
    input: String,
    lexeme: String,
    tokens: Vec<Token>,
    reserved_keywords: HashMap<String, Token>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let input = input.trim().to_string();
        let reserved_keywords = get_reserved_keywords();

        Self {
            position: 0,
            input,
            reserved_keywords,
            lexeme: String::new(),
            tokens: Vec::<Token>::new(),
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        let input = std::mem::take(&mut self.input);
        while self.position < input.len() {
            let c = input.as_bytes()[self.position] as char;
            match c {
                '0'..='9' | '.' => {
                    self.lexeme.push(c);
                    self.advance_position();
                }
                '+' => {
                    self.handle_operator(Token::Add)?;
                }
                '-' => {
                    self.handle_operator(Token::Subtract)?;
                }
                '*' => {
                    self.handle_operator(Token::Multiply)?;
                }
                '/' => {
                    self.handle_operator(Token::Divide)?;
                }
                '(' => {
                    self.handle_operator(Token::LeftParent)?;
                }
                ')' => {
                    self.handle_operator(Token::RightParent)?;
                }
                '{' => {
                    self.handle_operator(Token::LeftBrace)?;
                }
                '}' => {
                    self.handle_operator(Token::RightBrace)?;
                }
                '[' => {
                    self.handle_operator(Token::LeftBracket)?;
                }
                ']' => {
                    self.handle_operator(Token::RightBracket)?;
                }
                ',' => {
                    self.handle_operator(Token::Comma)?;
                }
                '|' => match self.peek(&input) {
                    Some('>') => {
                        self.advance_position();
                        self.handle_operator(Token::Pipeline)?;
                    }
                    // add more | operators later on
                    _ => return Err(anyhow!("Invalid use of '|'")),
                },
                '<' => self.check_ahead(&input, '=', Token::LessThanOrEqual, Token::LessThan)?,
                '>' => {
                    self.check_ahead(&input, '=', Token::GreaterThanOrEqual, Token::GreaterThan)?
                }
                ' ' | '\n' => {
                    self.flush_tokens()?;
                    self.advance_position();
                }
                '=' => self.check_ahead(&input, '=', Token::Equal, Token::Assign)?,
                '"' => {
                    self.advance_position();
                    let str = self.read_content(&input)?;
                    self.tokens.push(Token::String(str));
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let word = self.read_word(&input);
                    if let Some(token) = self.reserved_keywords.get(&word) {
                        self.tokens.push(token.clone());
                    } else {
                        self.tokens.push(Token::Identifier(word));
                    }
                }
                _ => {
                    return Err(anyhow!(format!(
                        "Unexpected error at: {}, character: {}",
                        self.position, c
                    )));
                }
            }
        }

        if !self.lexeme.is_empty() {
            let num = self
                .lexeme
                .parse::<f64>()
                .with_context(|| "Invalid number format")?;
            self.tokens.push(Token::Number(num));
        }

        Ok(std::mem::take(&mut self.tokens))
    }

    fn check_ahead(
        &mut self,
        input: &str,
        expected: char,
        expected_token: Token,
        fallback_token: Token,
    ) -> Result<()> {
        if let Some(c) = self.peek(&input) {
            if c == expected {
                self.advance_position();
                self.handle_operator(expected_token)?;
            } else {
                self.handle_operator(fallback_token)?;
            }
        } else {
            self.handle_operator(fallback_token)?;
        }

        Ok(())
    }

    fn handle_operator(&mut self, op: Token) -> Result<()> {
        self.flush_tokens()?;
        self.tokens.push(op);
        self.advance_position();
        Ok(())
    }

    fn flush_tokens(&mut self) -> Result<()> {
        if !self.lexeme.is_empty() {
            let num = self
                .lexeme
                .parse::<f64>()
                .with_context(|| "Invalid number format")?;
            self.tokens.push(Token::Number(num));
            self.lexeme.clear();
        }

        Ok(())
    }

    fn read_word(&mut self, input: &str) -> String {
        let mut word = String::new();
        while self.position < input.len() {
            let ch = input.as_bytes()[self.position] as char;
            if ch.is_ascii_alphanumeric() || ch == '_' {
                word.push(ch);
                self.advance_position();
            } else {
                break;
            }
        }

        word
    }

    fn read_content(&mut self, input: &str) -> Result<String> {
        let mut content = String::new();
        while self.position < input.len() {
            let ch = input.as_bytes()[self.position] as char;
            if ch == '"' {
                self.advance_position();
                return Ok(content);
            }
            content.push(ch);
            self.advance_position();
        }

        Err(anyhow!("Expected a closing \""))
    }

    fn advance_position(&mut self) {
        self.position += 1
    }

    fn peek(&self, input: &str) -> Option<char> {
        let mut next = 1;
        while self.position + next < input.len() {
            if let Some(c) = input.chars().nth(self.position + next) {
                if !c.is_whitespace() {
                    return Some(c);
                }
            }
            next += 1;
        }

        None
    }
}

fn get_reserved_keywords() -> HashMap<String, Token> {
    HashMap::from([
        ("suppose".to_string(), Token::Suppose),
        ("shout".to_string(), Token::Print(PrintType::Shout)),
        ("whisper".to_string(), Token::Print(PrintType::Whisper)),
        ("true".to_string(), Token::True),
        ("false".to_string(), Token::False),
        ("maybe".to_string(), Token::Maybe),
        ("perhaps".to_string(), Token::Perhaps),
        ("nah".to_string(), Token::Nah),
        ("wander".to_string(), Token::Wander),
        ("contemplate".to_string(), Token::Contemplate),
        ("suppose_its".to_string(), Token::SupposeIts),
    ])
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn test_basic_addition() {
        let mut lexer = Lexer::new("2 + 3");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![Token::Number(2.0), Token::Add, Token::Number(3.0)]
        );
    }

    #[test]
    fn test_string_declaration() {
        let mut lexer = Lexer::new("suppose s = \"yo\"");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Suppose,
                Token::Identifier("s".to_string()),
                Token::Assign,
                Token::String("yo".to_string())
            ]
        );
    }

    #[test]
    fn test_multi_digit_numbers() {
        let mut lexer = Lexer::new("25 + 137");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![Token::Number(25.0), Token::Add, Token::Number(137.0)]
        );
    }

    #[test]
    fn test_equals_operator() {
        let mut lexer = Lexer::new("maybe x == 5 {}");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Maybe,
                Token::Identifier("x".to_string()),
                Token::Equal,
                Token::Number(5.0),
                Token::LeftBrace,
                Token::RightBrace
            ]
        )
    }

    #[test]
    fn test_decimal_numbers() {
        let mut lexer = Lexer::new("3.15 * 2.5");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![Token::Number(3.15), Token::Multiply, Token::Number(2.5)]
        );
    }

    #[test]
    fn test_no_spaces() {
        let mut lexer = Lexer::new("25+3*4");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Number(25.0),
                Token::Add,
                Token::Number(3.0),
                Token::Multiply,
                Token::Number(4.0)
            ]
        );
    }

    #[test]
    fn test_all_operators() {
        let mut lexer = Lexer::new("1 + 2 - 3 * 4 / 5");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Number(1.0),
                Token::Add,
                Token::Number(2.0),
                Token::Subtract,
                Token::Number(3.0),
                Token::Multiply,
                Token::Number(4.0),
                Token::Divide,
                Token::Number(5.0)
            ]
        );
    }

    #[test]
    fn test_parentheses() {
        let mut lexer = Lexer::new("(2 + 3) * 4");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::LeftParent,
                Token::Number(2.0),
                Token::Add,
                Token::Number(3.0),
                Token::RightParent,
                Token::Multiply,
                Token::Number(4.0)
            ]
        );
    }

    #[test]
    fn test_double_operators() {
        let mut lexer = Lexer::new("2 + + 3");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Number(2.0),
                Token::Add,
                Token::Add,
                Token::Number(3.0)
            ]
        );
    }

    #[test]
    fn test_extra_spaces() {
        let mut lexer = Lexer::new("  2   +   3  ");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![Token::Number(2.0), Token::Add, Token::Number(3.0)]
        );
    }

    #[test]
    fn test_single_number() {
        let mut lexer = Lexer::new("42");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens, vec![Token::Number(42.0)]);
    }

    #[test]
    fn test_empty_string() {
        let mut lexer = Lexer::new("");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens, vec![]);
    }

    #[test]
    fn test_invalid_number() {
        let mut lexer = Lexer::new("3.14.5");
        let result = lexer.tokenize();

        assert!(result.is_err());
        assert_eq!(result.unwrap_err().to_string(), "Invalid number format");
    }

    #[test]
    fn test_expression_with_identifier() {
        let mut lexer = Lexer::new("suppose x = 5");
        let result = lexer.tokenize().unwrap();

        assert_eq!(
            result,
            vec![
                Token::Suppose,
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::Number(5.0)
            ]
        );
    }

    #[test]
    fn test_let_assignment() {
        let mut lexer = Lexer::new("suppose variable = 42.5");
        let result = lexer.tokenize().unwrap();

        assert_eq!(
            result,
            vec![
                Token::Suppose,
                Token::Identifier("variable".to_string()),
                Token::Assign,
                Token::Number(42.5)
            ]
        );
    }

    #[test]
    fn test_expression_with_variables() {
        let mut lexer = Lexer::new("x + y * 2");
        let result = lexer.tokenize().unwrap();

        assert_eq!(
            result,
            vec![
                Token::Identifier("x".to_string()),
                Token::Add,
                Token::Identifier("y".to_string()),
                Token::Multiply,
                Token::Number(2.0)
            ]
        );
    }

    #[test]
    fn test_underscore_identifier() {
        let mut lexer = Lexer::new("_test = 10");
        let result = lexer.tokenize().unwrap();

        assert_eq!(
            result,
            vec![
                Token::Identifier("_test".to_string()),
                Token::Assign,
                Token::Number(10.0)
            ]
        );
    }

    #[test]
    fn test_mixed_case_identifier() {
        let mut lexer = Lexer::new("myVar123 = 100");
        let result = lexer.tokenize().unwrap();

        assert_eq!(
            result,
            vec![
                Token::Identifier("myVar123".to_string()),
                Token::Assign,
                Token::Number(100.0)
            ]
        );
    }

    #[test]
    fn test_unexpected_character_error() {
        let mut lexer = Lexer::new("2 @ 3");
        let result = lexer.tokenize();

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Unexpected error"));
    }

    #[test]
    fn test_complex_nested_parentheses() {
        let mut lexer = Lexer::new("((2 + 3) * (4 - 1))");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::LeftParent,
                Token::LeftParent,
                Token::Number(2.0),
                Token::Add,
                Token::Number(3.0),
                Token::RightParent,
                Token::Multiply,
                Token::LeftParent,
                Token::Number(4.0),
                Token::Subtract,
                Token::Number(1.0),
                Token::RightParent,
                Token::RightParent
            ]
        );
    }

    #[test]
    fn test_let_assignment_no_spaces() {
        let mut lexer = Lexer::new("letx=42");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Identifier("letx".to_string()),
                Token::Assign,
                Token::Number(42.0)
            ]
        );
    }

    #[test]
    fn test_print_with_string() {
        let mut lexer = Lexer::new("whisper(\"hello world\")");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Print(PrintType::Whisper),
                Token::LeftParent,
                Token::String("hello world".to_string()),
                Token::RightParent
            ]
        );
    }

    #[test]
    fn test_perhaps_combination() {
        let mut lexer = Lexer::new("perhaps");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens, vec![Token::Perhaps]);
    }

    #[test]
    fn test_nah_keyword() {
        let mut lexer = Lexer::new("nah");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens, vec![Token::Nah]);
    }

    #[test]
    fn test_maybe_perhaps_nah_chain() {
        let mut lexer = Lexer::new("maybe perhaps nah");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens, vec![Token::Maybe, Token::Perhaps, Token::Nah]);
    }

    #[test]
    fn test_separate_nah_and_maybe() {
        let mut lexer = Lexer::new("} nah { } maybe");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::RightBrace,
                Token::Nah,
                Token::LeftBrace,
                Token::RightBrace,
                Token::Maybe
            ]
        );
    }

    #[test]
    fn test_maybe_perhaps_chain() {
        let mut lexer = Lexer::new("maybe perhaps perhaps");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens, vec![Token::Maybe, Token::Perhaps, Token::Perhaps]);
    }

    #[test]
    fn test_standalone_nah() {
        let mut lexer = Lexer::new("} nah {");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![Token::RightBrace, Token::Nah, Token::LeftBrace]
        );
    }

    #[test]
    fn test_standalone_maybe() {
        let mut lexer = Lexer::new("maybe condition");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![Token::Maybe, Token::Identifier("condition".to_string())]
        );
    }

    #[test]
    fn test_array_declaration() {
        let mut lexer = Lexer::new("suppose x = [1, 2, 3, 5]");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Suppose,
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::LeftBracket,
                Token::Number(1.0),
                Token::Comma,
                Token::Number(2.0),
                Token::Comma,
                Token::Number(3.0),
                Token::Comma,
                Token::Number(5.0),
                Token::RightBracket
            ]
        );
    }

    #[test]
    fn test_array_indexing() {
        let mut lexer = Lexer::new("arr[0]");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Identifier("arr".to_string()),
                Token::LeftBracket,
                Token::Number(0.0),
                Token::RightBracket
            ]
        );
    }

    #[test]
    fn test_array_indexing_with_expression() {
        let mut lexer = Lexer::new("numbers[i + 1]");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Identifier("numbers".to_string()),
                Token::LeftBracket,
                Token::Identifier("i".to_string()),
                Token::Add,
                Token::Number(1.0),
                Token::RightBracket
            ]
        );
    }

    #[test]
    fn test_chained_array_indexing() {
        let mut lexer = Lexer::new("matrix[0][1]");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Identifier("matrix".to_string()),
                Token::LeftBracket,
                Token::Number(0.0),
                Token::RightBracket,
                Token::LeftBracket,
                Token::Number(1.0),
                Token::RightBracket
            ]
        );
    }

    #[test]
    fn test_array_indexing_in_print() {
        let mut lexer = Lexer::new("whisper(\"Value: {}\", arr[2])");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Print(PrintType::Whisper),
                Token::LeftParent,
                Token::String("Value: {}".to_string()),
                Token::Comma,
                Token::Identifier("arr".to_string()),
                Token::LeftBracket,
                Token::Number(2.0),
                Token::RightBracket,
                Token::RightParent
            ]
        );
    }

    #[test]
    fn test_function_declaration() {
        let mut lexer = Lexer::new("contemplate add(a, b) { suppose_its a + b }");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Contemplate,
                Token::Identifier("add".to_string()),
                Token::LeftParent,
                Token::Identifier("a".to_string()),
                Token::Comma,
                Token::Identifier("b".to_string()),
                Token::RightParent,
                Token::LeftBrace,
                Token::SupposeIts,
                Token::Identifier("a".to_string()),
                Token::Add,
                Token::Identifier("b".to_string()),
                Token::RightBrace
            ]
        );
    }

    #[test]
    fn test_function_call() {
        let mut lexer = Lexer::new("add(5, 3)");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Identifier("add".to_string()),
                Token::LeftParent,
                Token::Number(5.0),
                Token::Comma,
                Token::Number(3.0),
                Token::RightParent
            ]
        );
    }

    #[test]
    fn test_function_with_no_params() {
        let mut lexer = Lexer::new("contemplate greet() { shout(\"Hello!\") }");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Contemplate,
                Token::Identifier("greet".to_string()),
                Token::LeftParent,
                Token::RightParent,
                Token::LeftBrace,
                Token::Print(PrintType::Shout),
                Token::LeftParent,
                Token::String("Hello!".to_string()),
                Token::RightParent,
                Token::RightBrace
            ]
        );
    }

    #[test]
    fn test_function_call_in_expression() {
        let mut lexer = Lexer::new("whisper(\"{}\", add(x, y) * 2)");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Print(PrintType::Whisper),
                Token::LeftParent,
                Token::String("{}".to_string()),
                Token::Comma,
                Token::Identifier("add".to_string()),
                Token::LeftParent,
                Token::Identifier("x".to_string()),
                Token::Comma,
                Token::Identifier("y".to_string()),
                Token::RightParent,
                Token::Multiply,
                Token::Number(2.0),
                Token::RightParent
            ]
        );
    }

    #[test]
    fn test_nested_function_calls() {
        let mut lexer = Lexer::new("max(add(1, 2), multiply(3, 4))");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Identifier("max".to_string()),
                Token::LeftParent,
                Token::Identifier("add".to_string()),
                Token::LeftParent,
                Token::Number(1.0),
                Token::Comma,
                Token::Number(2.0),
                Token::RightParent,
                Token::Comma,
                Token::Identifier("multiply".to_string()),
                Token::LeftParent,
                Token::Number(3.0),
                Token::Comma,
                Token::Number(4.0),
                Token::RightParent,
                Token::RightParent
            ]
        );
    }

    #[test]
    fn test_pipe_operator() {
        let mut lexer = Lexer::new("5 |> add(3)");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Number(5.0),
                Token::Pipeline,
                Token::Identifier("add".to_string()),
                Token::LeftParent,
                Token::Number(3.0),
                Token::RightParent
            ]
        );
    }

    #[test]
    fn test_chained_pipe_operations() {
        let mut lexer = Lexer::new("10 |> add(5) |> multiply(2) |> whisper(\"{}\")");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Number(10.0),
                Token::Pipeline,
                Token::Identifier("add".to_string()),
                Token::LeftParent,
                Token::Number(5.0),
                Token::RightParent,
                Token::Pipeline,
                Token::Identifier("multiply".to_string()),
                Token::LeftParent,
                Token::Number(2.0),
                Token::RightParent,
                Token::Pipeline,
                Token::Print(PrintType::Whisper),
                Token::LeftParent,
                Token::String("{}".to_string()),
                Token::RightParent
            ]
        );
    }

    #[test]
    fn test_pipe_with_variable() {
        let mut lexer = Lexer::new("x |> max(10)");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Identifier("x".to_string()),
                Token::Pipeline,
                Token::Identifier("max".to_string()),
                Token::LeftParent,
                Token::Number(10.0),
                Token::RightParent
            ]
        );
    }
}
