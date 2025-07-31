use anyhow::{anyhow, Result};

use crate::{errors::ParserError, lexer::Token};

#[derive(Debug, PartialEq)]
pub enum Statement {
    Declaration {
        name: String,
        expr: Expr,
    },
    Print {
        format_str: String,
        args: Vec<Expr>,
    },
    If {
        condition: Expr,
        then_block: Vec<Statement>,
        else_ifs: Vec<(Expr, Vec<Statement>)>,
        else_block: Option<Vec<Statement>>,
    },
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

#[derive(Debug, PartialEq, Clone)]
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

    pub fn parse_statement(&mut self) -> Result<Vec<Statement>, ParserError> {
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
                        return Err(ParserError::ExpectedOpeningParentAfterPrint);
                    }

                    self.advance();
                    let print_stmt = self.parse_format_str()?;

                    // check if closing parentheses is there
                    if !matches!(self.current_token(), Token::RightParent) {
                        return Err(ParserError::ExpectedClosingParentInPrint);
                    }

                    self.advance();
                    statements.push(print_stmt);
                }
                Token::If => {
                    self.advance();

                    // if block
                    let expr = self.parse_comparison()?;
                    let then_block = self.parse_block_with_braces()?;

                    // else if block
                    let mut else_ifs = Vec::new();
                    while matches!(self.current_token(), Token::ElseIf) {
                        self.advance();
                        let expr = self.parse_comparison()?;
                        let stmts = self.parse_block_with_braces()?;
                        else_ifs.push((expr, stmts));
                    }

                    // else block
                    let else_block = if matches!(self.current_token(), Token::Else) {
                        self.advance();
                        let stmts = self.parse_block_with_braces()?;
                        Some(stmts)
                    } else {
                        None
                    };

                    let if_statement = Statement::If {
                        condition: expr,
                        then_block,
                        else_ifs,
                        else_block,
                    };

                    statements.push(if_statement);
                }
                _ => break,
            }
        }

        Ok(statements)
    }

    fn parse_block_with_braces(&mut self) -> Result<Vec<Statement>, ParserError> {
        if !matches!(self.current_token(), Token::LeftBrace) {
            return Err(ParserError::ExpectedOpeningBraceAfterIf);
        }
        self.advance();

        let mut stmts = Vec::new();
        self.parse_if_block(&mut stmts)?;
        if !matches!(self.current_token(), Token::RightBrace) {
            return Err(ParserError::ExpectedClosingBraceAfterIf);
        }

        self.advance();

        Ok(stmts)
    }

    fn parse_if_block(&mut self, stmts: &mut Vec<Statement>) -> Result<(), ParserError> {
        while !matches!(self.current_token(), Token::RightBrace) {
            if !self.is_statement_token() {
                return Err(ParserError::MissingClosingBrace);
            }

            let stmt = self.parse_statement()?;

            stmts.extend(stmt);
        }
        Ok(())
    }

    fn parse_format_str(&mut self) -> Result<Statement, ParserError> {
        if let Token::String(content) = self.current_token() {
            let args_count = validate_args_count(&content)?;
            self.advance();

            let mut args = Vec::new();

            while self.current_token() == Token::Comma {
                self.advance();
                let expr = self.parse_comparison()?;
                args.push(expr);
            }

            if args.len() != args_count {
                return Err(ParserError::IncorrectArgumentCount {
                    found: args_count,
                    expected: args.len(),
                });
            }

            Ok(Statement::Print {
                format_str: content,
                args,
            })
        } else {
            Err(ParserError::InvalidFormatString)
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<Statement, ParserError> {
        if let Token::Identifier(i) = self.current_token() {
            self.advance(); // move past the identifier
            if self.current_token() == Token::Assign {
                // check if the next token is of type Assign
                self.advance();
                let expr = self.parse_comparison()?;
                Ok(Statement::Declaration {
                    name: i.to_string(),
                    expr,
                })
            } else {
                return Err(ParserError::ExpectedAssignmentAfterVariable);
            }
        } else {
            return Err(ParserError::ExpectedIdentifierAfterLet);
        }
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParserError> {
        let mut left = self.parse_expression()?;
        while matches!(
            self.current_token(),
            Token::GreaterThan
                | Token::LessThan
                | Token::GreaterThanOrEqual
                | Token::LessThanOrEqual
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
    fn parse_expression(&mut self) -> Result<Expr, ParserError> {
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
    fn parse_term(&mut self) -> Result<Expr, ParserError> {
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
    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
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
                    return Err(ParserError::ExpectedClosingParentAfterExpression);
                }
            }
            _ => Err(ParserError::UnexpectedToken(self.current_token())),
        }
    }

    pub fn is_statement_token(&self) -> bool {
        matches!(self.current_token(), Token::Let | Token::If | Token::Print)
    }
}

fn validate_args_count(content: &str) -> Result<usize, ParserError> {
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
                    return Err(ParserError::InvalidClosingBrace);
                }
            }
            _ => {}
        }
    }

    if stack.len() != 0 {
        return Err(ParserError::InvalidOpeningBrace);
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
                Statement::Declaration {
                    name: "x".to_string(),
                    expr: Expr::Value(Value::Number(5.0))
                },
                Statement::Print {
                    format_str: "Result: {}".to_string(),
                    args: vec![Expr::BinaryOp {
                        left: Box::new(Expr::Value(Value::Number(2.0))),
                        op: Token::Add,
                        right: Box::new(Expr::Value(Value::Number(3.0)))
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
                Statement::Declaration {
                    name: "x".to_string(),
                    expr: Expr::Value(Value::Number(10.0))
                },
                Statement::Declaration {
                    name: "y".to_string(),
                    expr: Expr::Value(Value::Number(20.0))
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
                Statement::Declaration {
                    name: "result".to_string(),
                    expr: Expr::BinaryOp {
                        left: Box::new(Expr::Value(Value::Number(5.0))),
                        op: Token::Multiply,
                        right: Box::new(Expr::Value(Value::Number(3.0)))
                    }
                },
                Statement::Print {
                    format_str: "Answer: {}".to_string(),
                    args: vec![Expr::BinaryOp {
                        left: Box::new(Expr::Value(Value::Number(10.0))),
                        op: Token::Add,
                        right: Box::new(Expr::Value(Value::Number(2.0)))
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
                Statement::Declaration {
                    name: "a".to_string(),
                    expr: Expr::Value(Value::Number(1.0))
                },
                Statement::Declaration {
                    name: "b".to_string(),
                    expr: Expr::Value(Value::Number(2.0))
                },
                Statement::Print {
                    format_str: "Sum: {}".to_string(),
                    args: vec![Expr::BinaryOp {
                        left: Box::new(Expr::Value(Value::Number(7.0))),
                        op: Token::Add,
                        right: Box::new(Expr::Value(Value::Number(3.0)))
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
            vec![Statement::Declaration {
                name: "value".to_string(),
                expr: Expr::Value(Value::Number(42.0))
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
                Statement::Declaration {
                    name: "x".to_string(),
                    expr: Expr::Value(Value::Number(5.0))
                },
                Statement::Print {
                    format_str: "x + 3 = {}".to_string(),
                    args: vec![Expr::BinaryOp {
                        left: Box::new(Expr::Variable("x".to_string())),
                        op: Token::Add,
                        right: Box::new(Expr::Value(Value::Number(3.0)))
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
                Statement::Declaration {
                    name: "a".to_string(),
                    expr: Expr::Value(Value::Number(10.0))
                },
                Statement::Declaration {
                    name: "b".to_string(),
                    expr: Expr::Value(Value::Number(20.0))
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
                Statement::Declaration {
                    name: "x".to_string(),
                    expr: Expr::Value(Value::Number(5.0))
                },
                Statement::Declaration {
                    name: "y".to_string(),
                    expr: Expr::BinaryOp {
                        left: Box::new(Expr::Variable("x".to_string())),
                        op: Token::Add,
                        right: Box::new(Expr::Value(Value::Number(10.0)))
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
            vec![Statement::Declaration {
                name: "result".to_string(),
                expr: Expr::BinaryOp {
                    left: Box::new(Expr::BinaryOp {
                        left: Box::new(Expr::Variable("x".to_string())),
                        op: Token::Multiply,
                        right: Box::new(Expr::Value(Value::Number(2.0)))
                    }),
                    op: Token::Add,
                    right: Box::new(Expr::BinaryOp {
                        left: Box::new(Expr::Variable("y".to_string())),
                        op: Token::Divide,
                        right: Box::new(Expr::Value(Value::Number(3.0)))
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
                left: Box::new(Expr::Value(Value::Number(3.0))),
                op: Token::Add,
                right: Box::new(Expr::Value(Value::Number(3.0))),
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
                left: Box::new(Expr::Value(Value::Number(5.0))),
                op: Token::Subtract,
                right: Box::new(Expr::Value(Value::Number(2.0))),
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
                left: Box::new(Expr::Value(Value::Number(4.0))),
                op: Token::Multiply,
                right: Box::new(Expr::Value(Value::Number(6.0))),
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
                left: Box::new(Expr::Value(Value::Number(8.0))),
                op: Token::Divide,
                right: Box::new(Expr::Value(Value::Number(2.0))),
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
                left: Box::new(Expr::Value(Value::Number(2.0))),
                op: Token::Add,
                right: Box::new(Expr::BinaryOp {
                    left: Box::new(Expr::Value(Value::Number(3.0))),
                    op: Token::Multiply,
                    right: Box::new(Expr::Value(Value::Number(4.0))),
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
                    left: Box::new(Expr::Value(Value::Number(10.0))),
                    op: Token::Subtract,
                    right: Box::new(Expr::Value(Value::Number(5.0))),
                }),
                op: Token::Subtract,
                right: Box::new(Expr::Value(Value::Number(2.0))),
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
                    left: Box::new(Expr::Value(Value::Number(2.0))),
                    op: Token::Add,
                    right: Box::new(Expr::Value(Value::Number(3.0))),
                }),
                op: Token::Multiply,
                right: Box::new(Expr::Value(Value::Number(4.0))),
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
                    left: Box::new(Expr::Value(Value::Number(2.0))),
                    op: Token::Add,
                    right: Box::new(Expr::Value(Value::Number(3.0))),
                }),
                op: Token::Multiply,
                right: Box::new(Expr::Value(Value::Number(4.0))),
            }
        );
    }

    #[test]
    fn test_single_number() {
        let mut lexer = Lexer::new("42".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(parsed_expr, Expr::Value(Value::Number(42.0)));
    }

    #[test]
    fn test_single_number_with_parentheses() {
        let mut lexer = Lexer::new("(42)".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(parsed_expr, Expr::Value(Value::Number(42.0)));
    }

    #[test]
    fn test_simple_if_statement() {
        let mut lexer = Lexer::new("if x > 5 { let y = 10 }".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::If {
                condition: Expr::BinaryOp {
                    left: Box::new(Expr::Variable("x".to_string())),
                    op: Token::GreaterThan,
                    right: Box::new(Expr::Value(Value::Number(5.0)))
                },
                then_block: vec![Statement::Declaration {
                    name: "y".to_string(),
                    expr: Expr::Value(Value::Number(10.0))
                }],
                else_ifs: vec![],
                else_block: None
            }]
        );
    }

    #[test]
    fn test_if_else_statement() {
        let mut lexer = Lexer::new("if x > 5 { let y = 10 } else { let y = 20 }".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::If {
                condition: Expr::BinaryOp {
                    left: Box::new(Expr::Variable("x".to_string())),
                    op: Token::GreaterThan,
                    right: Box::new(Expr::Value(Value::Number(5.0)))
                },
                then_block: vec![Statement::Declaration {
                    name: "y".to_string(),
                    expr: Expr::Value(Value::Number(10.0))
                }],
                else_ifs: vec![],
                else_block: Some(vec![Statement::Declaration {
                    name: "y".to_string(),
                    expr: Expr::Value(Value::Number(20.0))
                }])
            }]
        );
    }

    #[test]
    fn test_if_else_if_statement() {
        let mut lexer =
            Lexer::new("if x > 10 { let y = 1 } else if x > 5 { let y = 2 }".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::If {
                condition: Expr::BinaryOp {
                    left: Box::new(Expr::Variable("x".to_string())),
                    op: Token::GreaterThan,
                    right: Box::new(Expr::Value(Value::Number(10.0)))
                },
                then_block: vec![Statement::Declaration {
                    name: "y".to_string(),
                    expr: Expr::Value(Value::Number(1.0))
                }],
                else_ifs: vec![(
                    Expr::BinaryOp {
                        left: Box::new(Expr::Variable("x".to_string())),
                        op: Token::GreaterThan,
                        right: Box::new(Expr::Value(Value::Number(5.0)))
                    },
                    vec![Statement::Declaration {
                        name: "y".to_string(),
                        expr: Expr::Value(Value::Number(2.0))
                    }]
                )],
                else_block: None
            }]
        );
    }

    #[test]
    fn test_if_else_if_else_statement() {
        let mut lexer = Lexer::new(
            "if x > 10 { let y = 1 } else if x > 5 { let y = 2 } else { let y = 3 }".to_string(),
        );
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::If {
                condition: Expr::BinaryOp {
                    left: Box::new(Expr::Variable("x".to_string())),
                    op: Token::GreaterThan,
                    right: Box::new(Expr::Value(Value::Number(10.0)))
                },
                then_block: vec![Statement::Declaration {
                    name: "y".to_string(),
                    expr: Expr::Value(Value::Number(1.0))
                }],
                else_ifs: vec![(
                    Expr::BinaryOp {
                        left: Box::new(Expr::Variable("x".to_string())),
                        op: Token::GreaterThan,
                        right: Box::new(Expr::Value(Value::Number(5.0)))
                    },
                    vec![Statement::Declaration {
                        name: "y".to_string(),
                        expr: Expr::Value(Value::Number(2.0))
                    }]
                )],
                else_block: Some(vec![Statement::Declaration {
                    name: "y".to_string(),
                    expr: Expr::Value(Value::Number(3.0))
                }])
            }]
        );
    }

    #[test]
    fn test_multiple_else_if_statements() {
        let mut lexer = Lexer::new("if x > 10 { let y = 1 } else if x > 8 { let y = 2 } else if x > 5 { let y = 3 } else { let y = 4 }".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::If {
                condition: Expr::BinaryOp {
                    left: Box::new(Expr::Variable("x".to_string())),
                    op: Token::GreaterThan,
                    right: Box::new(Expr::Value(Value::Number(10.0)))
                },
                then_block: vec![Statement::Declaration {
                    name: "y".to_string(),
                    expr: Expr::Value(Value::Number(1.0))
                }],
                else_ifs: vec![
                    (
                        Expr::BinaryOp {
                            left: Box::new(Expr::Variable("x".to_string())),
                            op: Token::GreaterThan,
                            right: Box::new(Expr::Value(Value::Number(8.0)))
                        },
                        vec![Statement::Declaration {
                            name: "y".to_string(),
                            expr: Expr::Value(Value::Number(2.0))
                        }]
                    ),
                    (
                        Expr::BinaryOp {
                            left: Box::new(Expr::Variable("x".to_string())),
                            op: Token::GreaterThan,
                            right: Box::new(Expr::Value(Value::Number(5.0)))
                        },
                        vec![Statement::Declaration {
                            name: "y".to_string(),
                            expr: Expr::Value(Value::Number(3.0))
                        }]
                    )
                ],
                else_block: Some(vec![Statement::Declaration {
                    name: "y".to_string(),
                    expr: Expr::Value(Value::Number(4.0))
                }])
            }]
        );
    }

    #[test]
    fn test_nested_if_statements() {
        let mut lexer = Lexer::new("if x > 5 { if y > 10 { let z = 1 } }".to_string());
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::If {
                condition: Expr::BinaryOp {
                    left: Box::new(Expr::Variable("x".to_string())),
                    op: Token::GreaterThan,
                    right: Box::new(Expr::Value(Value::Number(5.0)))
                },
                then_block: vec![Statement::If {
                    condition: Expr::BinaryOp {
                        left: Box::new(Expr::Variable("y".to_string())),
                        op: Token::GreaterThan,
                        right: Box::new(Expr::Value(Value::Number(10.0)))
                    },
                    then_block: vec![Statement::Declaration {
                        name: "z".to_string(),
                        expr: Expr::Value(Value::Number(1.0))
                    }],
                    else_ifs: vec![],
                    else_block: None
                }],
                else_ifs: vec![],
                else_block: None
            }]
        );
    }
}
