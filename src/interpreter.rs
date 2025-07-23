use std::collections::HashMap;

use anyhow::{Result, anyhow};

use crate::{lexer::Token, parser::{Expr, Statement}};

pub struct Interpreter {
    variables: HashMap<String, f64>
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new()
        }
    }

    pub fn calculate(&mut self, statements: Vec<Statement>) -> Result<Vec<f64>> {
        let mut results = Vec::new();
        for stmt in statements {
            match stmt {
                Statement::LetDeclaration { name, expr } => {
                    let value = self.evaluate(&expr)?;
                    self.variables.insert(name,value);
                },
                Statement::Expr(expr) => {
                    results.push(self.evaluate(&expr)?);
                }
            }
        }
        Ok(results)
    }
    
    pub fn evaluate(&self, expr: &Expr) -> Result<f64> {
        match expr {
            Expr::Number(n) => Ok(*n),
            Expr::Variable(i) => {
                let value = self.variables
                    .get(i)
                    .ok_or_else(|| anyhow!("Variable not found {}", i))?;
                Ok(*value)
            },
            Expr::BinaryOp { left, op, right } => {
                let left_val = self.evaluate(left)?;
                let right_val = self.evaluate(right)?;

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
}

    
#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};
    use super::*;

    // Basic arithmetic tests
    #[test]
    fn test_addition() {
        let mut lexer = Lexer::new("3 + 2".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![5.0]);
    }

    #[test]
    fn test_operator_precedence() {
        let mut lexer = Lexer::new("3 + 2 * 4".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![11.0]); // 3 + (2 * 4) = 11
    }

    #[test]
    fn test_subtraction() {
        let mut lexer = Lexer::new("10 - 4".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![6.0]);
    }

    #[test]
    fn test_multiplication() {
        let mut lexer = Lexer::new("4 * 5".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![20.0]);
    }

    #[test]
    fn test_division() {
        let mut lexer = Lexer::new("15 / 3".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![5.0]);
    }

    #[test]
    fn test_parentheses_precedence() {
        let mut lexer = Lexer::new("(3 + 2) * 4".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![20.0]); // (3 + 2) * 4 = 20
    }

    #[test]
    fn test_complex_expression() {
        let mut lexer = Lexer::new("2 + 3 * 4 - 1".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![13.0]); // 2 + (3 * 4) - 1 = 13
    }

    #[test]
    fn test_left_associativity() {
        let mut lexer = Lexer::new("10 - 3 - 2".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![5.0]); // (10 - 3) - 2 = 5
    }

    #[test]
    fn test_nested_parentheses() {
        let mut lexer = Lexer::new("((2 + 3) * 4) / 2".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![10.0]); // ((2 + 3) * 4) / 2 = 10
    }

    #[test]
    fn test_decimal_numbers() {
        let mut lexer = Lexer::new("3.5 + 2.25".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![5.75]);
    }

    #[test]
    fn test_single_number() {
        let mut lexer = Lexer::new("42".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![42.0]);
    }

    // Variable tests
    #[test]
    fn test_simple_variable_declaration() {
        let mut lexer = Lexer::new("let x = 5".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![]); // Let declarations don't return values
    }

    #[test]
    fn test_variable_usage() {
        let mut lexer = Lexer::new("let x = 5\nx + 3".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![8.0]); // x(5) + 3 = 8
    }

    #[test]
    fn test_multiple_variables() {
        let mut lexer = Lexer::new("let x = 10\nlet y = 5\nx * y".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![50.0]); // x(10) * y(5) = 50
    }

    #[test]
    fn test_variable_in_let_expression() {
        let mut lexer = Lexer::new("let x = 5\nlet y = x + 10\ny".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![15.0]); // y = x(5) + 10 = 15
    }

    #[test]
    fn test_variable_redeclaration() {
        let mut lexer = Lexer::new("let x = 5\nlet x = 10\nx".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![10.0]); // x gets redeclared to 10
    }

    #[test]
    fn test_complex_variable_expression() {
        let mut lexer = Lexer::new("let a = 2\nlet b = 3\nlet c = 4\na + b * c".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![14.0]); // a(2) + b(3) * c(4) = 2 + 12 = 14
    }

    // Error handling tests
    #[test]
    fn test_undefined_variable_error() {
        let mut lexer = Lexer::new("x + 5".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr);

        assert!(res.is_err());
        assert!(res.unwrap_err().to_string().contains("Variable not found"));
    }

    #[test]
    fn test_division_by_zero() {
        let mut lexer = Lexer::new("5 / 0".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr);

        assert!(res.is_err());
        assert_eq!(res.unwrap_err().to_string(), "Division by zero");
    }

    // Multiple statement tests
    #[test]
    fn test_multiple_expressions() {
        let mut lexer = Lexer::new("5 + 3\n2 * 4\n10 - 1".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![8.0, 8.0, 9.0]); // Results of each expression
    }

    #[test]
    fn test_mixed_statements() {
        let mut lexer = Lexer::new("let x = 10\n20 + 5\nlet y = x * 2\ny".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();    
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec![25.0, 20.0]); // Only expressions return values
    }
}
