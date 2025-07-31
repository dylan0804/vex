use std::{collections::HashMap, hash::Hash};

use anyhow::{anyhow, Context, Result};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Number(f64),
    Add,
    Subtract,
    Multiply,
    Divide,
    LeftParent,
    RightParent,
    LeftBrace,
    RightBrace,
    Let,
    Identifier(String),
    String(String),
    Assign,
    Print,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Comma,
    True,
    False,
    If,
    ElseIf,
    Else,
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
    pub fn new(input: String) -> Self {
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
                ',' => {
                    self.handle_operator(Token::Comma)?;
                }
                '<' => {
                    if let Some(c) = self.peek(&input) {
                        if c == '=' {
                            self.advance_position();
                            self.handle_operator(Token::LessThanOrEqual)?;
                        } else {
                            self.handle_operator(Token::LessThan)?;
                        }
                    } else {
                        self.handle_operator(Token::LessThan)?;
                    }
                }
                '>' => {
                    if let Some(c) = self.peek(&input) {
                        if c == '=' {
                            self.advance_position();
                            self.handle_operator(Token::GreaterThanOrEqual)?;
                        } else {
                            self.handle_operator(Token::GreaterThan)?;
                        }
                    } else {
                        self.handle_operator(Token::GreaterThan)?;
                    }
                }
                ' ' | '\n' => {
                    self.flush_tokens()?;
                    self.advance_position();
                }
                '=' => {
                    self.handle_operator(Token::Assign)?;
                }
                '"' => {
                    self.advance_position();
                    let str = self.read_content(&input)?;
                    self.tokens.push(Token::String(str));
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let word = self.read_word(&input);
                    if let Some(token) = self.reserved_keywords.get(&word) {
                        if word == "if" && self.top() == Token::Else {
                            self.tokens.pop();
                            self.tokens.push(Token::ElseIf);
                        } else {
                            self.tokens.push(token.clone());
                        }
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

        self.tokens.push(Token::Eof);

        Ok(std::mem::take(&mut self.tokens))
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

    fn top(&self) -> Token {
        self.tokens.last().unwrap_or(&Token::Eof).clone()
    }
}

fn get_reserved_keywords() -> HashMap<String, Token> {
    HashMap::from([
        ("let".to_string(), Token::Let),
        ("print".to_string(), Token::Print),
        ("true".to_string(), Token::True),
        ("false".to_string(), Token::False),
        ("if".to_string(), Token::If),
        ("else if".to_string(), Token::ElseIf),
        ("else".to_string(), Token::Else),
    ])
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn test_basic_addition() {
        let mut lexer = Lexer::new("2 + 3".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![Token::Number(2.0), Token::Add, Token::Number(3.0)]
        );
    }

    #[test]
    fn test_multi_digit_numbers() {
        let mut lexer = Lexer::new("25 + 137".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![Token::Number(25.0), Token::Add, Token::Number(137.0)]
        );
    }

    #[test]
    fn test_decimal_numbers() {
        let mut lexer = Lexer::new("3.15 * 2.5".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![Token::Number(3.15), Token::Multiply, Token::Number(2.5)]
        );
    }

    #[test]
    fn test_no_spaces() {
        let mut lexer = Lexer::new("25+3*4".to_string());
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
        let mut lexer = Lexer::new("1 + 2 - 3 * 4 / 5".to_string());
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
        let mut lexer = Lexer::new("(2 + 3) * 4".to_string());
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
        let mut lexer = Lexer::new("2 + + 3".to_string());
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
        let mut lexer = Lexer::new("  2   +   3  ".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![Token::Number(2.0), Token::Add, Token::Number(3.0)]
        );
    }

    #[test]
    fn test_single_number() {
        let mut lexer = Lexer::new("42".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens, vec![Token::Number(42.0)]);
    }

    #[test]
    fn test_empty_string() {
        let mut lexer = Lexer::new("".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens, vec![]);
    }

    #[test]
    fn test_invalid_number() {
        let mut lexer = Lexer::new("3.14.5".to_string());
        let result = lexer.tokenize();

        assert!(result.is_err());
        assert_eq!(result.unwrap_err().to_string(), "Invalid number format");
    }

    #[test]
    fn test_expression_with_identifier() {
        let mut lexer = Lexer::new("let x = 5".to_string());
        let result = lexer.tokenize().unwrap();

        assert_eq!(
            result,
            vec![
                Token::Let,
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::Number(5.0)
            ]
        );
    }

    #[test]
    fn test_let_assignment() {
        let mut lexer = Lexer::new("let variable = 42.5".to_string());
        let result = lexer.tokenize().unwrap();

        assert_eq!(
            result,
            vec![
                Token::Let,
                Token::Identifier("variable".to_string()),
                Token::Assign,
                Token::Number(42.5)
            ]
        );
    }

    #[test]
    fn test_expression_with_variables() {
        let mut lexer = Lexer::new("x + y * 2".to_string());
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
        let mut lexer = Lexer::new("_test = 10".to_string());
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
        let mut lexer = Lexer::new("myVar123 = 100".to_string());
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
        let mut lexer = Lexer::new("2 @ 3".to_string());
        let result = lexer.tokenize();

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Unexpected error"));
    }

    #[test]
    fn test_complex_nested_parentheses() {
        let mut lexer = Lexer::new("((2 + 3) * (4 - 1))".to_string());
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
        let mut lexer = Lexer::new("letx=42".to_string());
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
        let mut lexer = Lexer::new("print(\"hello world\")".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Print,
                Token::LeftParent,
                Token::String("hello world".to_string()),
                Token::RightParent
            ]
        );
    }

    #[test]
    fn test_else_if_combination() {
        let mut lexer = Lexer::new("else if".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens, vec![Token::ElseIf]);
    }

    #[test]
    fn test_else_if_with_whitespace() {
        let mut lexer = Lexer::new("else   if".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens, vec![Token::ElseIf]);
    }

    #[test]
    fn test_else_if_with_newline() {
        let mut lexer = Lexer::new("else\nif".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens, vec![Token::ElseIf]);
    }

    #[test]
    fn test_separate_else_and_if() {
        let mut lexer = Lexer::new("} else { } if".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::RightBrace,
                Token::Else,
                Token::LeftBrace,
                Token::RightBrace,
                Token::If
            ]
        );
    }

    #[test]
    fn test_if_else_if_chain() {
        let mut lexer = Lexer::new("if else if else if".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens, vec![Token::If, Token::ElseIf, Token::ElseIf]);
    }

    #[test]
    fn test_standalone_else() {
        let mut lexer = Lexer::new("} else {".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![Token::RightBrace, Token::Else, Token::LeftBrace]
        );
    }

    #[test]
    fn test_standalone_if() {
        let mut lexer = Lexer::new("if condition".to_string());
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![Token::If, Token::Identifier("condition".to_string())]
        );
    }
}
