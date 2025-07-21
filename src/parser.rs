use anyhow::{anyhow, Result};

use crate::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Number(f64),
    BinaryOp {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0
        }
    }

    fn current_token(&self) -> Token {
        if self.current >= self.tokens.len() {
            Token::Eof
        } else {
            self.tokens[self.current]
        }
    }

    fn advance(&mut self) {
        self.current += 1
    }

    // USES RECURSIVE DESCENT
    // logic:
    // 3 + 2 * 4
    // need left side, calls parse_term()
    // need left side again calls parse_factor()
    // if number then return, advance current (now at +)
    // current is + (not * or /) go back to parse_expr
    // left is Number(3), current is plus, return Number(3)
    // now its addition, go to parse_term()
    // inside parse_term() need right side, call parse_factor()
    // ...

    // handles lowest precedence e.g (+,-)
    pub fn parse_expression(&mut self) -> Result<Expr> {
        let mut left = self.parse_term()?;
        while matches!(self.current_token(), Token::Add | Token::Subtract) {
            let current_token = self.current_token();
            self.advance();
            let right = self.parse_term()?;
            left = Expr::BinaryOp {
                left: Box::new(left),
                op: current_token,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    // handles higher precedence e.g (*,/)
    fn parse_term(&mut self) -> Result<Expr> {
        let mut left = self.parse_factor()?;
        while matches!(self.current_token(), Token::Multiply | Token::Divide) {
            let current_token = self.current_token();
            self.advance();
            let right = self.parse_factor()?;
            left = Expr::BinaryOp {
                left: Box::new(left),
                op: current_token,
                right: Box::new(right)
            };
        }
        Ok(left)
    }

    // handles number and parentheses
    fn parse_factor(&mut self) -> Result<Expr> {
        match self.current_token() {
            Token::Number(n) => {
                self.advance();
                return Ok(Expr::Number(n));
            },
            Token::LeftParent => {
                self.advance();
                let expr = self.parse_expression()?;

                if matches!(self.current_token(), Token::RightParent) {
                    self.advance();
                    return Ok(expr);
                } else {
                    return Err(anyhow!("Expected ')' after expression"))
                }
            }
            _ => {}
        }
        Err(anyhow!("Expected number or '('"))
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;

    use super::*;

    #[test]
    fn test_addition_expression() {
        let mut lexer = Lexer::new("3 + 3".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(parsed_expr, Expr::BinaryOp {
            left: Box::new(Expr::Number(
                3.0,
            )),
            op: Token::Add,
            right: Box::new(Expr::Number(
                3.0,
            )),
         });
    }

    #[test]
    fn test_subtraction_expression() {
        let mut lexer = Lexer::new("5 - 2".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(parsed_expr, Expr::BinaryOp {
            left: Box::new(Expr::Number(5.0)),
            op: Token::Subtract,
            right: Box::new(Expr::Number(2.0)),
        });
    }

    #[test]
    fn test_multiplication_expression() {
        let mut lexer = Lexer::new("4 * 6".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(parsed_expr, Expr::BinaryOp {
            left: Box::new(Expr::Number(4.0)),
            op: Token::Multiply,
            right: Box::new(Expr::Number(6.0)),
        });
    }

    #[test]
    fn test_division_expression() {
        let mut lexer = Lexer::new("8 / 2".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(parsed_expr, Expr::BinaryOp {
            left: Box::new(Expr::Number(8.0)),
            op: Token::Divide,
            right: Box::new(Expr::Number(2.0)),
        });
    }

    #[test]
    fn test_operator_precedence() {
        let mut lexer = Lexer::new("2 + 3 * 4".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(parsed_expr, Expr::BinaryOp {
            left: Box::new(Expr::Number(2.0)),
            op: Token::Add,
            right: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::Number(3.0)),
                op: Token::Multiply,
                right: Box::new(Expr::Number(4.0)),
            }),
        });
    }

    #[test]
    fn test_left_associativity() {
        let mut lexer = Lexer::new("10 - 5 - 2".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(parsed_expr, Expr::BinaryOp {
            left: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::Number(10.0)),
                op: Token::Subtract,
                right: Box::new(Expr::Number(5.0)),
            }),
            op: Token::Subtract,
            right: Box::new(Expr::Number(2.0)),
        });
    }

    #[test]
    fn test_parentheses_expression() {
        let mut lexer = Lexer::new("(2 + 3) * 4".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(parsed_expr, Expr::BinaryOp {
            left: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::Number(2.0)),
                op: Token::Add,
                right: Box::new(Expr::Number(3.0)),
            }),
            op: Token::Multiply,
            right: Box::new(Expr::Number(4.0)),
        });
    }

    #[test]
    fn test_nested_parentheses() {
        let mut lexer = Lexer::new("((2 + 3) * 4)".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(parsed_expr, Expr::BinaryOp {
            left: Box::new(Expr::BinaryOp {
                left: Box::new(Expr::Number(2.0)),
                op: Token::Add,
                right: Box::new(Expr::Number(3.0)),
            }),
            op: Token::Multiply,
            right: Box::new(Expr::Number(4.0)),
        });
    }

    #[test]
    fn test_single_number() {
        let mut lexer = Lexer::new("42".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(parsed_expr, Expr::Number(42.0));
    }

    #[test]
    fn test_single_number_with_parentheses() {
        let mut lexer = Lexer::new("(42)".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(parsed_expr, Expr::Number(42.0));
    }
}
