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
    Let,
    Identifier(String),
    Assign,
    Eof, // add more operators later
}

pub struct Lexer {
    position: usize,
    input: String,
    lexeme: String,
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let input = input.trim().to_string();
        Self {
            position: 0,
            input,
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
                ' ' => {
                    self.flush_tokens()?;
                    self.advance_position();
                },
                '=' => {
                    self.handle_operator(Token::Assign)?;
                },
                'a'..='z' | 'A'..='Z' | '_' => {
                    let word = self.read_word(&input);
                    if word == "let" {
                        self.handle_operator(Token::Let)?;
                    } else {
                        self.handle_operator(Token::Identifier(word))?;
                    }
                },
                _ => return Err(anyhow!(format!("Unexpected error at: {}, character: {}", self.position, c)))
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
                println!("position is {}", self.position);
            } else {
                break;
            }
        }

        word
    }

    fn advance_position(&mut self) {
        self.position += 1
    }
}

#[cfg(test)]
mod tests {
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
}
