use anyhow::{anyhow, Result};

use crate::{
    errors::ParserError,
    lexer::{PrintType, Token},
};

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Declaration {
        name: String,
        expr: Expr,
    },
    Assignment {
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
    FunctionDeclaration {
        fn_name: String,
        parameters: Vec<String>,
        body: Vec<Statement>,
    },
    FunctionCall(Expr),
    Return {
        expr: Expr,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueType {
    Number,
    String,
    Boolean,
    Array,
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Array(Vec<Value>),
    None,
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        match self {
            Value::Number(_) => ValueType::Number,
            Value::Boolean(_) => ValueType::Boolean,
            Value::String(_) => ValueType::String,
            Value::Array(_) => ValueType::Array,
            Value::None => ValueType::None,
        }
    }

    pub fn as_number(&self) -> Result<f64> {
        match self {
            Value::Number(n) => Ok(*n),
            Value::Boolean(b) => Err(anyhow!("Expected number, got boolean: {}", b)),
            Value::String(s) => Err(anyhow!("Expected number, got string: {}", s)),
            Value::Array(s) => Err(anyhow!("Expected number, got array: {:?}", s)),
            Value::None => Err(anyhow!("Expected number, got none")),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Value::Number(n) => n.to_string(),
            Value::Boolean(b) => b.to_string(),
            Value::String(s) => s.to_string(),
            Value::None => "".to_string(),
            Value::Array(arr) => {
                let elements = arr.iter().map(|e| e.to_string()).collect::<Vec<String>>();
                format!("[{}]", elements.join(", "))
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Variable(String),
    Value(Value),
    Array(Vec<Expr>),
    Index {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    FunctionCall {
        name: String,
        args: Vec<Expr>,
    },
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
                // variable declaration
                Token::Suppose => {
                    self.advance();
                    let stmt = self.parse_variable_declaration()?;
                    statements.push(stmt);
                }
                Token::Identifier(i) => {
                    match self.tokens[self.current + 1] {
                        Token::Assign => {
                            self.advance(); // consume identifier
                            self.advance(); // consume assign

                            let expr = self.parse_comparison()?;

                            statements.push(Statement::Assignment {
                                name: i.clone(),
                                expr,
                            });
                        }
                        Token::LeftParent => {
                            let expr = self.parse_comparison()?;
                            statements.push(Statement::FunctionCall(expr));
                        }
                        _ => return Err(ParserError::ExpectedAssignmentOrFunctionCall),
                    }
                }
                Token::Contemplate => {
                    self.advance();

                    // get function name
                    let fn_name = if let Token::Identifier(fn_name) = self.current_token() {
                        Ok(fn_name)
                    } else {
                        Err(ParserError::ExpectedFunctionName)
                    }?;
                    self.advance();

                    if !matches!(self.current_token(), Token::LeftParent) {
                        return Err(ParserError::ExpectedOpeningParenthesesAfterFunction);
                    }
                    self.advance();

                    // get parameters
                    let parameters = self.collect_params()?;

                    // consume right parentheses
                    if !matches!(self.current_token(), Token::RightParent) {
                        return Err(ParserError::ExpectedClosingParenthesesAfterFunction);
                    }
                    self.advance();

                    // consume bracket
                    self.advance();

                    // get function body
                    let body = self.parse_statement()?;

                    // consume right brace
                    self.advance();
                    statements.push(Statement::FunctionDeclaration {
                        fn_name,
                        parameters,
                        body,
                    });
                }
                Token::Print(pt) => {
                    self.advance();
                    if !matches!(self.current_token(), Token::LeftParent) {
                        return Err(ParserError::ExpectedOpeningParentAfterPrint);
                    }

                    self.advance();
                    let print_stmt = self.parse_format_str(pt)?;

                    // check if closing parentheses is there
                    if !matches!(self.current_token(), Token::RightParent) {
                        return Err(ParserError::ExpectedClosingParentInPrint);
                    }

                    self.advance();
                    statements.push(print_stmt);
                }
                Token::SupposeIts => {
                    self.advance();
                    let expr = self.parse_comparison()?;
                    statements.push(Statement::Return { expr });
                }
                // if statements
                Token::Maybe => {
                    self.advance();

                    // if block
                    let expr = self.parse_comparison()?;
                    let then_block = self.parse_block_with_braces()?;

                    // else if block
                    let mut else_ifs = Vec::new();
                    while matches!(self.current_token(), Token::Perhaps) {
                        self.advance();
                        let expr = self.parse_comparison()?;
                        let stmts = self.parse_block_with_braces()?;
                        else_ifs.push((expr, stmts));
                    }

                    // else block
                    let else_block = if matches!(self.current_token(), Token::Nah) {
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

    fn collect_params(&mut self) -> Result<Vec<String>, ParserError> {
        let mut parameters = Vec::new();

        // handle empty params
        if matches!(self.current_token(), Token::RightParent) {
            return Ok(parameters);
        }

        loop {
            if let Token::Identifier(param) = self.current_token() {
                parameters.push(param);
                self.advance();
            } else {
                return Err(ParserError::ExpectedParameterName);
            }

            match self.current_token() {
                Token::Comma => {
                    self.advance(); // consume comma
                }
                Token::RightParent => {
                    break;
                }
                _ => {
                    return Err(ParserError::ExpectedCommaOrClosingParen);
                }
            }
        }

        Ok(parameters)
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

    fn parse_format_str(&mut self, pt: PrintType) -> Result<Statement, ParserError> {
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
                format_str: match pt {
                    PrintType::Whisper => content,
                    PrintType::Shout => content + "\n",
                },
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
                let expr = match self.current_token() {
                    // arrays
                    Token::LeftBracket => {
                        self.advance();

                        let mut elements = Vec::new();
                        if !matches!(self.current_token(), Token::RightBracket) {
                            elements.push(self.parse_comparison()?);
                        }

                        while matches!(self.current_token(), Token::Comma) {
                            self.advance();
                            elements.push(self.parse_comparison()?);
                        }

                        if matches!(self.current_token(), Token::RightBracket) {
                            self.advance();
                            Ok(Expr::Array(elements))
                        } else {
                            Err(ParserError::ExpectedClosingBracket)
                        }
                    }
                    _ => self.parse_comparison(),
                }?;

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

    fn parse_function_args(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut args = Vec::new();

        if matches!(self.current_token(), Token::RightParent) {
            return Ok(args);
        }

        loop {
            args.push(self.parse_comparison()?);

            match self.current_token() {
                Token::Comma => {
                    self.advance();
                }
                Token::RightParent => break,
                _ => return Err(ParserError::ExpectedCommaOrClosingParen),
            }
        }

        Ok(args)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParserError> {
        let mut left = self.parse_expression()?;
        while matches!(
            self.current_token(),
            Token::GreaterThan
                | Token::LessThan
                | Token::GreaterThanOrEqual
                | Token::LessThanOrEqual
                | Token::Equal
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

    fn parse_term(&mut self) -> Result<Expr, ParserError> {
        let mut left = self.parse_postfix()?;
        while matches!(self.current_token(), Token::Multiply | Token::Divide) {
            let current_token = self.current_token();
            self.advance();
            let right = self.parse_postfix()?;
            left = Expr::BinaryOp {
                left: Box::new(left),
                op: current_token,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParserError> {
        let mut left = self.parse_factor()?;
        while matches!(self.current_token(), Token::LeftBracket | Token::LeftParent) {
            match self.current_token() {
                // array related
                Token::LeftBracket => {
                    self.advance();
                    let index = self.parse_expression()?;

                    if !matches!(self.current_token(), Token::RightBracket) {
                        return Err(ParserError::ExpectedClosingBracket);
                    }

                    self.advance();

                    left = Expr::Index {
                        array: Box::new(left),
                        index: Box::new(index),
                    }
                }
                // function call
                Token::LeftParent => {
                    self.advance();
                    let args = self.parse_function_args()?;

                    if !matches!(self.current_token(), Token::RightParent) {
                        return Err(ParserError::ExpectedClosingParenthesesAfterFunction);
                    }
                    self.advance();

                    let name = if let Expr::Variable(i) = left {
                        Ok(i)
                    } else {
                        Err(ParserError::InvalidFunctionName)
                    }?;

                    left = Expr::FunctionCall { name, args }
                }
                _ => break,
            }
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        match self.current_token() {
            Token::Number(n) => {
                self.advance();
                Ok(Expr::Value(Value::Number(n)))
            }
            Token::Identifier(i) => {
                self.advance();
                Ok(Expr::Variable(i))
            }
            Token::String(s) => {
                self.advance();
                Ok(Expr::Value(Value::String(s)))
            }
            Token::True => {
                self.advance();
                Ok(Expr::Value(Value::Boolean(true)))
            }
            Token::False => {
                self.advance();
                Ok(Expr::Value(Value::Boolean(false)))
            }
            Token::LeftParent => {
                self.advance();
                let expr = self.parse_expression()?;

                if matches!(self.current_token(), Token::RightParent) {
                    self.advance();
                    Ok(expr)
                } else {
                    Err(ParserError::ExpectedClosingParentAfterExpression)
                }
            }
            _ => Err(ParserError::UnexpectedToken(self.current_token())),
        }
    }

    pub fn is_statement_token(&self) -> bool {
        matches!(
            self.current_token(),
            Token::Suppose | Token::Maybe | Token::Print(_) | Token::SupposeIts
        )
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
        let mut lexer = Lexer::new("suppose x = 5\nwhisper(\"Result: {}\", 2 + 3)");
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
        let mut lexer = Lexer::new("suppose x = 10\nsuppose y = 20");
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
        let mut lexer = Lexer::new("suppose result = 5 * 3\nshout(\"Answer: {}\", 10 + 2)");
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
                    format_str: "Answer: {}\n".to_string(),
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
        let mut lexer = Lexer::new("suppose a = 1\nsuppose b = 2\nwhisper(\"Sum: {}\", 7 + 3)");
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
        let mut lexer = Lexer::new("suppose value = 42");
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
        let mut lexer = Lexer::new("suppose x = 5\nshout(\"x + 3 = {}\", x + 3)");
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
                    format_str: "x + 3 = {}\n".to_string(),
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
            Lexer::new("suppose a = 10\nsuppose b = 20\nwhisper(\"a * b = {}\", a * b)");
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
        let mut lexer = Lexer::new("suppose x = 5\nsuppose y = x + 10");
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
        let mut lexer = Lexer::new("suppose result = x * 2 + y / 3");
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
        let mut lexer = Lexer::new("shout(\"Result: {}\", (x + y) * z)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::Print {
                format_str: "Result: {}\n".to_string(),
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
        let mut lexer = Lexer::new("whisper(\"Value: {}\", myVar)");
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
        let mut lexer = Lexer::new("3 + 3");
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
        let mut lexer = Lexer::new("5 - 2");
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
        let mut lexer = Lexer::new("4 * 6");
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
        let mut lexer = Lexer::new("8 / 2");
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
        let mut lexer = Lexer::new("2 + 3 * 4");
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
        let mut lexer = Lexer::new("10 - 5 - 2");
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
        let mut lexer = Lexer::new("(2 + 3) * 4");
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
        let mut lexer = Lexer::new("((2 + 3) * 4)");
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
        let mut lexer = Lexer::new("42");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(parsed_expr, Expr::Value(Value::Number(42.0)));
    }

    #[test]
    fn test_single_number_with_parentheses() {
        let mut lexer = Lexer::new("(42)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(parsed_expr, Expr::Value(Value::Number(42.0)));
    }

    #[test]
    fn test_simple_if_statement() {
        let mut lexer = Lexer::new("maybe x > 5 { suppose y = 10 }");
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
        let mut lexer = Lexer::new("maybe x > 5 { suppose y = 10 } nah { suppose y = 20 }");
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
            Lexer::new("maybe x > 10 { suppose y = 1 } perhaps x > 5 { suppose y = 2 }");
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
            "maybe x > 10 { suppose y = 1 } perhaps x > 5 { suppose y = 2 } nah { suppose y = 3 }",
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
        let mut lexer = Lexer::new("maybe x > 10 { suppose y = 1 } perhaps x > 8 { suppose y = 2 } perhaps x > 5 { suppose y = 3 } nah { suppose y = 4 }");
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
        let mut lexer = Lexer::new("maybe x > 5 { maybe y > 10 { suppose z = 1 } }");
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

    #[test]
    fn test_simple_array_indexing() {
        let mut lexer = Lexer::new("arr[0]");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(
            parsed_expr,
            Expr::Index {
                array: Box::new(Expr::Variable("arr".to_string())),
                index: Box::new(Expr::Value(Value::Number(0.0)))
            }
        );
    }

    #[test]
    fn test_array_indexing_with_expression() {
        let mut lexer = Lexer::new("numbers[i + 1]");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(
            parsed_expr,
            Expr::Index {
                array: Box::new(Expr::Variable("numbers".to_string())),
                index: Box::new(Expr::BinaryOp {
                    left: Box::new(Expr::Variable("i".to_string())),
                    op: Token::Add,
                    right: Box::new(Expr::Value(Value::Number(1.0)))
                })
            }
        );
    }

    #[test]
    fn test_chained_array_indexing() {
        let mut lexer = Lexer::new("matrix[0][1]");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(
            parsed_expr,
            Expr::Index {
                array: Box::new(Expr::Index {
                    array: Box::new(Expr::Variable("matrix".to_string())),
                    index: Box::new(Expr::Value(Value::Number(0.0)))
                }),
                index: Box::new(Expr::Value(Value::Number(1.0)))
            }
        );
    }

    #[test]
    fn test_array_indexing_in_statement() {
        let mut lexer = Lexer::new("whisper(\"Value: {}\", arr[2])");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::Print {
                format_str: "Value: {}".to_string(),
                args: vec![Expr::Index {
                    array: Box::new(Expr::Variable("arr".to_string())),
                    index: Box::new(Expr::Value(Value::Number(2.0)))
                }]
            }]
        );
    }

    #[test]
    fn test_array_indexing_with_operator_precedence() {
        let mut lexer = Lexer::new("arr[0] + 5");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(
            parsed_expr,
            Expr::BinaryOp {
                left: Box::new(Expr::Index {
                    array: Box::new(Expr::Variable("arr".to_string())),
                    index: Box::new(Expr::Value(Value::Number(0.0)))
                }),
                op: Token::Add,
                right: Box::new(Expr::Value(Value::Number(5.0)))
            }
        );
    }

    #[test]
    fn test_complex_array_indexing_expression() {
        let mut lexer = Lexer::new("data[i * 2][j + 1] * factor");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(
            parsed_expr,
            Expr::BinaryOp {
                left: Box::new(Expr::Index {
                    array: Box::new(Expr::Index {
                        array: Box::new(Expr::Variable("data".to_string())),
                        index: Box::new(Expr::BinaryOp {
                            left: Box::new(Expr::Variable("i".to_string())),
                            op: Token::Multiply,
                            right: Box::new(Expr::Value(Value::Number(2.0)))
                        })
                    }),
                    index: Box::new(Expr::BinaryOp {
                        left: Box::new(Expr::Variable("j".to_string())),
                        op: Token::Add,
                        right: Box::new(Expr::Value(Value::Number(1.0)))
                    })
                }),
                op: Token::Multiply,
                right: Box::new(Expr::Variable("factor".to_string()))
            }
        );
    }

    #[test]
    fn test_simple_function_declaration() {
        let mut lexer = Lexer::new("contemplate add(a, b) { suppose_its a + b }");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::FunctionDeclaration {
                fn_name: "add".to_string(),
                parameters: vec!["a".to_string(), "b".to_string()],
                body: vec![Statement::Return {
                    expr: Expr::BinaryOp {
                        left: Box::new(Expr::Variable("a".to_string())),
                        op: Token::Add,
                        right: Box::new(Expr::Variable("b".to_string()))
                    }
                }]
            }]
        );
    }

    #[test]
    fn test_function_with_no_parameters() {
        let mut lexer = Lexer::new("contemplate greet() { shout(\"Hello!\") }");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::FunctionDeclaration {
                fn_name: "greet".to_string(),
                parameters: vec![],
                body: vec![Statement::Print {
                    format_str: "Hello!\n".to_string(),
                    args: vec![]
                }]
            }]
        );
    }

    #[test]
    fn test_function_with_single_parameter() {
        let mut lexer = Lexer::new("contemplate square(x) { suppose_its x * x }");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::FunctionDeclaration {
                fn_name: "square".to_string(),
                parameters: vec!["x".to_string()],
                body: vec![Statement::Return {
                    expr: Expr::BinaryOp {
                        left: Box::new(Expr::Variable("x".to_string())),
                        op: Token::Multiply,
                        right: Box::new(Expr::Variable("x".to_string()))
                    }
                }]
            }]
        );
    }

    #[test]
    fn test_function_with_multiple_statements() {
        let mut lexer = Lexer::new("contemplate calculate(x, y) { suppose result = x + y\nshout(\"Result: {}\", result)\nsuppose_its result }");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::FunctionDeclaration {
                fn_name: "calculate".to_string(),
                parameters: vec!["x".to_string(), "y".to_string()],
                body: vec![
                    Statement::Declaration {
                        name: "result".to_string(),
                        expr: Expr::BinaryOp {
                            left: Box::new(Expr::Variable("x".to_string())),
                            op: Token::Add,
                            right: Box::new(Expr::Variable("y".to_string()))
                        }
                    },
                    Statement::Print {
                        format_str: "Result: {}\n".to_string(),
                        args: vec![Expr::Variable("result".to_string())]
                    },
                    Statement::Return {
                        expr: Expr::Variable("result".to_string())
                    }
                ]
            }]
        );
    }

    #[test]
    fn test_function_with_conditional_return() {
        let mut lexer = Lexer::new(
            "contemplate max(a, b) { maybe a > b { suppose_its a } nah { suppose_its b } }",
        );
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::FunctionDeclaration {
                fn_name: "max".to_string(),
                parameters: vec!["a".to_string(), "b".to_string()],
                body: vec![Statement::If {
                    condition: Expr::BinaryOp {
                        left: Box::new(Expr::Variable("a".to_string())),
                        op: Token::GreaterThan,
                        right: Box::new(Expr::Variable("b".to_string()))
                    },
                    then_block: vec![Statement::Return {
                        expr: Expr::Variable("a".to_string())
                    }],
                    else_ifs: vec![],
                    else_block: Some(vec![Statement::Return {
                        expr: Expr::Variable("b".to_string())
                    }])
                }]
            }]
        );
    }

    #[test]
    fn test_function_call_expression() {
        let mut lexer = Lexer::new("add(5, 3)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(
            parsed_expr,
            Expr::FunctionCall {
                name: "add".to_string(),
                args: vec![
                    Expr::Value(Value::Number(5.0)),
                    Expr::Value(Value::Number(3.0))
                ]
            }
        );
    }

    #[test]
    fn test_nested_function_calls() {
        let mut lexer = Lexer::new("max(add(1, 2), multiply(3, 4))");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_expression().unwrap();

        assert_eq!(
            parsed_expr,
            Expr::FunctionCall {
                name: "max".to_string(),
                args: vec![
                    Expr::FunctionCall {
                        name: "add".to_string(),
                        args: vec![
                            Expr::Value(Value::Number(1.0)),
                            Expr::Value(Value::Number(2.0))
                        ]
                    },
                    Expr::FunctionCall {
                        name: "multiply".to_string(),
                        args: vec![
                            Expr::Value(Value::Number(3.0)),
                            Expr::Value(Value::Number(4.0))
                        ]
                    }
                ]
            }
        );
    }

    #[test]
    fn test_function_call_in_statement() {
        let mut lexer = Lexer::new("suppose result = add(x, y)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_stmt = parser.parse_statement().unwrap();

        assert_eq!(
            parsed_stmt,
            vec![Statement::Declaration {
                name: "result".to_string(),
                expr: Expr::FunctionCall {
                    name: "add".to_string(),
                    args: vec![
                        Expr::Variable("x".to_string()),
                        Expr::Variable("y".to_string())
                    ]
                }
            }]
        );
    }
}
