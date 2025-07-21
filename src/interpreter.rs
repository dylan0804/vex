use anyhow::{Result, anyhow};

use crate::{lexer::Token, parser::Expr};

pub fn evaluate(expr: &Expr) -> Result<f64> {
    match expr {
        Expr::Number(n) => Ok(*n),
        Expr::BinaryOp { left, op, right } => {
            let left_val = evaluate(left)?;
            let right_val = evaluate(right)?;

            match op {
                Token::Add => Ok(left_val + right_val),
                Token::Subtract => Ok(left_val - right_val),
                Token::Multiply => Ok(left_val * right_val),
                Token::Divide => {
                    if right_val == 0.0 {
                        Err(anyhow!("Division by zero"))
                    } else {
                        Ok(left_val / right_val)
                    }
                }
                _ => Err(anyhow!(format!("Unknown operator {:?}", op))),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn test_addition() {
        let mut lexer = Lexer::new("3 + 2 * 3".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();
        let res = evaluate(&parsed_expr).unwrap();

        assert_eq!(res, 9.0);
    }

    #[test]
    fn test_subtraction() {
        let mut lexer = Lexer::new("10 - 4".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();
        let res = evaluate(&parsed_expr).unwrap();

        assert_eq!(res, 6.0);
    }

    #[test]
    fn test_multiplication() {
        let mut lexer = Lexer::new("4 * 5".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();
        let res = evaluate(&parsed_expr).unwrap();

        assert_eq!(res, 20.0);
    }

    #[test]
    fn test_division() {
        let mut lexer = Lexer::new("15 / 3".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();
        let res = evaluate(&parsed_expr).unwrap();

        assert_eq!(res, 5.0);
    }

    #[test]
    fn test_parentheses() {
        let mut lexer = Lexer::new("(2 + 3) * 4".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();
        let res = evaluate(&parsed_expr).unwrap();

        assert_eq!(res, 20.0);
    }

    #[test]
    fn test_complex_expression() {
        let mut lexer = Lexer::new("2 + 3 * 4 - 1".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();
        let res = evaluate(&parsed_expr).unwrap();

        assert_eq!(res, 13.0);
    }

    #[test]
    fn test_nested_parentheses() {
        let mut lexer = Lexer::new("((2 + 3) * 4) / 2".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();
        let res = evaluate(&parsed_expr).unwrap();

        assert_eq!(res, 10.0);
    }

    #[test]
    fn test_decimal_numbers() {
        let mut lexer = Lexer::new("3.5 + 2.25".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();
        let res = evaluate(&parsed_expr).unwrap();

        assert_eq!(res, 5.75);
    }

    #[test]
    fn test_single_number() {
        let mut lexer = Lexer::new("42".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();
        let res = evaluate(&parsed_expr).unwrap();

        assert_eq!(res, 42.0);
    }

    #[test]
    fn test_division_by_zero() {
        let mut lexer = Lexer::new("5 / 0".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();
        let res = evaluate(&parsed_expr);

        assert!(res.is_err());
        assert_eq!(res.unwrap_err().to_string(), "Division by zero");
    }
}
