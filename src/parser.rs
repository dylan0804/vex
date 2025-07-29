use anyhow::{anyhow, Result};

use crate::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum Statement {
    LetDeclaration { name: String, expr: Expr },
    Print { format_str: String, args: Vec<Expr> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
}

impl Value {
    pub fn as_number(&self) -> Result<f64> {
        match self {
            Value::Number(n) => Ok(*n),
            Value::Boolean(_) => Err(anyhow!("Expected number, got boolean")),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Value::Number(n) => n.to_string(),
            Value::Boolean(b) => b.to_string(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Variable(String),
    Value(Value),
    BinaryOp {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn current_token(&self) -> Token {
        if self.current >= self.tokens.len() {
            Token::Eof
        } else {
            self.tokens[self.current].clone()
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

    pub fn parse_statement(&mut self) -> Result<Vec<Statement>> {
        let mut statements = Vec::new();
        while self.current < self.tokens.len() {
            let current_token = self.current_token();
            match current_token {
                Token::Let => {
                    self.advance();
                    let stmt = self.parse_variable_declaration()?;
                    statements.push(stmt);
                }
                Token::Print => {
                    self.advance();
                    if !matches!(self.current_token(), Token::LeftParent) {
                        return Err(anyhow!("Expected an opening ( after print statement"));
                    }

                    self.advance();
                    let print_stmt = self.parse_format_str()?;

                    // check if closing parentheses is there
                    if !matches!(self.current_token(), Token::RightParent) {
                        return Err(anyhow!("Expected a closing ) in a print statement"));
                    }

                    self.advance();
                    statements.push(print_stmt);
                }
                Token::If => {
                    self.advance();
                    let expr = self.parse_expression()?;

                    if !matches!(self.current_token(), Token::LeftBrace) {
                        return Err(anyhow!("Expected '{{' after if condition"));
                    }
                    self.advance();

                    while !matches!(self.current_token(), Token::RightBrace) {
                        let stmt = self.parse_statement()?;
                        println!("statements inside if {:?}", stmt);
                    }
                }
                _ => break,
            }
        }
        Ok(statements)
    }

    fn parse_format_str(&mut self) -> Result<Statement> {
        if let Token::String(content) = self.current_token() {
            let args_count = validate_string(&content)?;
            self.advance();

            let mut args = Vec::new();

            while self.current_token() == Token::Comma {
                self.advance();
                let expr = self.parse_comparison()?;
                args.push(expr);
            }

            if args.len() != args_count {
                return Err(anyhow!(
                    "Incorrect arguments was passed. Found {}, expected {}",
                    args.len(),
                    args_count
                ));
            }

            Ok(Statement::Print {
                format_str: content,
                args,
            })
        } else {
            Err(anyhow!("Invalid format after print statement"))
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<Statement> {
        if let Token::Identifier(i) = self.current_token() {
            self.advance(); // move past the identifier
            if self.current_token() == Token::Assign {
                // check if the next token is of type Assign
                self.advance();
                let expr = self.parse_comparison()?;
                Ok(Statement::LetDeclaration {
                    name: i.to_string(),
                    expr,
                })
            } else {
                Err(anyhow!("Expected '=' after variable name"))
            }
        } else {
            Err(anyhow!("Expected indentifier after 'let'"))
        }
    }

    fn parse_comparison(&mut self) -> Result<Expr> {
        let mut left = self.parse_expression()?;
        while matches!(
            self.current_token(),
            Token::GreaterThan
                | Token::LesserThan
                | Token::GreaterAndEqualThan
                | Token::LesserAndEqualThan
        ) {
            let current_token = self.current_token();
            self.advance();
            let right = self.parse_expression()?;
            left = Expr::BinaryOp {
                left: Box::new(left),
                op: current_token,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    // handles lowest precedence e.g (+,-)
    fn parse_expression(&mut self) -> Result<Expr> {
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
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    // handles number and parentheses
    fn parse_factor(&mut self) -> Result<Expr> {
        match self.current_token() {
            Token::Number(n) => {
                self.advance();
                return Ok(Expr::Value(Value::Number(n)));
            }
            Token::Identifier(i) => {
                self.advance();
                return Ok(Expr::Variable(i));
            }
            Token::True => {
                self.advance();
                return Ok(Expr::Value(Value::Boolean(true)));
            }
            Token::False => {
                self.advance();
                return Ok(Expr::Value(Value::Boolean(false)));
            }
            Token::LeftParent => {
                self.advance();
                let expr = self.parse_expression()?;

                if matches!(self.current_token(), Token::RightParent) {
                    self.advance();
                    return Ok(expr);
                } else {
                    return Err(anyhow!("Expected ')' after expression"));
                }
            }
            _ => {}
        }
        Err(anyhow!(format!(
            "Unexpected token '{:?}' found",
            self.current_token()
        )))
    }
}

fn validate_string(content: &str) -> Result<usize> {
    let mut stack = Vec::new();
    let mut args_count = 0;

    for c in content.chars() {
        match c {
            '{' => {
                stack.push(c);
            }
            '}' => {
                if let Some(_) = stack.last() {
                    stack.pop();
                    args_count += 1;
                } else {
                    return Err(anyhow!("Invalid '}}' in format string"));
                }
            }
            _ => {}
        }
    }

    if stack.len() != 0 {
        return Err(anyhow!("Invalid '{{' in format string"));
    }

    Ok(args_count)
}

#[cfg(test)]
mod parser_tests {
    use crate::lexer::Lexer;

    use super::*;

    #[test]
    fn test_parse_multi_statement() {
        let mut lexer = Lexer::new("let x = 5\nprint(\"Result: {}\", 2 + 3)".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![
                Statement::LetDeclaration {
                    name: "x".to_string(),
                    expr: Expr::Number(5.0)
                },
                Statement::Print {
                    format_str: "Result: {}".to_string(),
                    args: vec![Expr::BinaryOp {
                        left: Box::new(Expr::Number(2.0)),
                        op: Token::Add,
                        right: Box::new(Expr::Number(3.0))
                    }]
                }
            ]
        )
    }

    #[test]
    fn test_multiple_let_declarations() {
        let mut lexer = Lexer::new("let x = 10\nlet y = 20".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![
                Statement::LetDeclaration {
                    name: "x".to_string(),
                    expr: Expr::Number(10.0)
                },
                Statement::LetDeclaration {
                    name: "y".to_string(),
                    expr: Expr::Number(20.0)
                }
            ]
        )
    }

    #[test]
    fn test_let_with_complex_expression() {
        let mut lexer = Lexer::new("let result = 5 * 3\nprint(\"Answer: {}\", 10 + 2)".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![
                Statement::LetDeclaration {
                    name: "result".to_string(),
                    expr: Expr::BinaryOp {
                        left: Box::new(Expr::Number(5.0)),
                        op: Token::Multiply,
                        right: Box::new(Expr::Number(3.0))
                    }
                },
                Statement::Print {
                    format_str: "Answer: {}".to_string(),
                    args: vec![Expr::BinaryOp {
                        left: Box::new(Expr::Number(10.0)),
                        op: Token::Add,
                        right: Box::new(Expr::Number(2.0))
                    }]
                }
            ]
        )
    }

    #[test]
    fn test_three_statements() {
        let mut lexer = Lexer::new("let a = 1\nlet b = 2\nprint(\"Sum: {}\", 7 + 3)".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![
                Statement::LetDeclaration {
                    name: "a".to_string(),
                    expr: Expr::Number(1.0)
                },
                Statement::LetDeclaration {
                    name: "b".to_string(),
                    expr: Expr::Number(2.0)
                },
                Statement::Print {
                    format_str: "Sum: {}".to_string(),
                    args: vec![Expr::BinaryOp {
                        left: Box::new(Expr::Number(7.0)),
                        op: Token::Add,
                        right: Box::new(Expr::Number(3.0))
                    }]
                }
            ]
        )
    }

    #[test]
    fn test_single_let_statement() {
        let mut lexer = Lexer::new("let value = 42".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::LetDeclaration {
                name: "value".to_string(),
                expr: Expr::Number(42.0)
            }]
        )
    }

    #[test]
    fn test_variable_in_expression() {
        let mut lexer = Lexer::new("let x = 5\nprint(\"x + 3 = {}\", x + 3)".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![
                Statement::LetDeclaration {
                    name: "x".to_string(),
                    expr: Expr::Number(5.0)
                },
                Statement::Print {
                    format_str: "x + 3 = {}".to_string(),
                    args: vec![Expr::BinaryOp {
                        left: Box::new(Expr::Variable("x".to_string())),
                        op: Token::Add,
                        right: Box::new(Expr::Number(3.0))
                    }]
                }
            ]
        )
    }

    #[test]
    fn test_two_variables_expression() {
        let mut lexer =
            Lexer::new("let a = 10\nlet b = 20\nprint(\"a * b = {}\", a * b)".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![
                Statement::LetDeclaration {
                    name: "a".to_string(),
                    expr: Expr::Number(10.0)
                },
                Statement::LetDeclaration {
                    name: "b".to_string(),
                    expr: Expr::Number(20.0)
                },
                Statement::Print {
                    format_str: "a * b = {}".to_string(),
                    args: vec![Expr::BinaryOp {
                        left: Box::new(Expr::Variable("a".to_string())),
                        op: Token::Multiply,
                        right: Box::new(Expr::Variable("b".to_string()))
                    }]
                }
            ]
        )
    }

    #[test]
    fn test_let_with_variable_expression() {
        let mut lexer = Lexer::new("let x = 5\nlet y = x + 10".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![
                Statement::LetDeclaration {
                    name: "x".to_string(),
                    expr: Expr::Number(5.0)
                },
                Statement::LetDeclaration {
                    name: "y".to_string(),
                    expr: Expr::BinaryOp {
                        left: Box::new(Expr::Variable("x".to_string())),
                        op: Token::Add,
                        right: Box::new(Expr::Number(10.0))
                    }
                }
            ]
        )
    }

    #[test]
    fn test_complex_variable_expression() {
        let mut lexer = Lexer::new("let result = x * 2 + y / 3".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::LetDeclaration {
                name: "result".to_string(),
                expr: Expr::BinaryOp {
                    left: Box::new(Expr::BinaryOp {
                        left: Box::new(Expr::Variable("x".to_string())),
                        op: Token::Multiply,
                        right: Box::new(Expr::Number(2.0))
                    }),
                    op: Token::Add,
                    right: Box::new(Expr::BinaryOp {
                        left: Box::new(Expr::Variable("y".to_string())),
                        op: Token::Divide,
                        right: Box::new(Expr::Number(3.0))
                    })
                }
            }]
        )
    }

    #[test]
    fn test_variable_with_parentheses() {
        let mut lexer = Lexer::new("print(\"Result: {}\", (x + y) * z)".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::Print {
                format_str: "Result: {}".to_string(),
                args: vec![Expr::BinaryOp {
                    left: Box::new(Expr::BinaryOp {
                        left: Box::new(Expr::Variable("x".to_string())),
                        op: Token::Add,
                        right: Box::new(Expr::Variable("y".to_string()))
                    }),
                    op: Token::Multiply,
                    right: Box::new(Expr::Variable("z".to_string()))
                }]
            }]
        )
    }

    #[test]
    fn test_single_variable_expression() {
        let mut lexer = Lexer::new("print(\"Value: {}\", myVar)".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::Print {
                format_str: "Value: {}".to_string(),
                args: vec![Expr::Variable("myVar".to_string())]
            }]
        )
    }

    #[test]
    fn test_addition_expression() {
        let mut lexer = Lexer::new("3 + 3".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(
            parsed_expr,
            Expr::BinaryOp {
                left: Box::new(Expr::Number(3.0,)),
                op: Token::Add,
                right: Box::new(Expr::Number(3.0,)),
            }
        );
    }

    #[test]
    fn test_subtraction_expression() {
        let mut lexer = Lexer::new("5 - 2".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(
            parsed_expr,
            Expr::BinaryOp {
                left: Box::new(Expr::Number(5.0)),
                op: Token::Subtract,
                right: Box::new(Expr::Number(2.0)),
            }
        );
    }

    #[test]
    fn test_multiplication_expression() {
        let mut lexer = Lexer::new("4 * 6".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(
            parsed_expr,
            Expr::BinaryOp {
                left: Box::new(Expr::Number(4.0)),
                op: Token::Multiply,
                right: Box::new(Expr::Number(6.0)),
            }
        );
    }

    #[test]
    fn test_division_expression() {
        let mut lexer = Lexer::new("8 / 2".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(
            parsed_expr,
            Expr::BinaryOp {
                left: Box::new(Expr::Number(8.0)),
                op: Token::Divide,
                right: Box::new(Expr::Number(2.0)),
            }
        );
    }

    #[test]
    fn test_operator_precedence() {
        let mut lexer = Lexer::new("2 + 3 * 4".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(
            parsed_expr,
            Expr::BinaryOp {
                left: Box::new(Expr::Number(2.0)),
                op: Token::Add,
                right: Box::new(Expr::BinaryOp {
                    left: Box::new(Expr::Number(3.0)),
                    op: Token::Multiply,
                    right: Box::new(Expr::Number(4.0)),
                }),
            }
        );
    }

    #[test]
    fn test_left_associativity() {
        let mut lexer = Lexer::new("10 - 5 - 2".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(
            parsed_expr,
            Expr::BinaryOp {
                left: Box::new(Expr::BinaryOp {
                    left: Box::new(Expr::Number(10.0)),
                    op: Token::Subtract,
                    right: Box::new(Expr::Number(5.0)),
                }),
                op: Token::Subtract,
                right: Box::new(Expr::Number(2.0)),
            }
        );
    }

    #[test]
    fn test_parentheses_expression() {
        let mut lexer = Lexer::new("(2 + 3) * 4".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(
            parsed_expr,
            Expr::BinaryOp {
                left: Box::new(Expr::BinaryOp {
                    left: Box::new(Expr::Number(2.0)),
                    op: Token::Add,
                    right: Box::new(Expr::Number(3.0)),
                }),
                op: Token::Multiply,
                right: Box::new(Expr::Number(4.0)),
            }
        );
    }

    #[test]
    fn test_nested_parentheses() {
        let mut lexer = Lexer::new("((2 + 3) * 4)".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(
            parsed_expr,
            Expr::BinaryOp {
                left: Box::new(Expr::BinaryOp {
                    left: Box::new(Expr::Number(2.0)),
                    op: Token::Add,
                    right: Box::new(Expr::Number(3.0)),
                }),
                op: Token::Multiply,
                right: Box::new(Expr::Number(4.0)),
            }
        );
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
