use std::collections::HashMap;

use anyhow::{anyhow, Ok, Result};

use crate::{
    lexer::Token,
    parser::{Expr, Statement, Value, ValueType},
};

pub struct Interpreter {
    scopes: Vec<HashMap<String, Value>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) -> Option<HashMap<String, Value>> {
        self.scopes.pop()
    }

    fn get_variable(&mut self, name: &str) -> Option<&Value> {
        // check the current scope first (top of the vec)
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }

        None
    }

    pub fn set_variable(&mut self, name: String, value: Value) {
        if let Some(curr_scope) = self.scopes.last_mut() {
            curr_scope.insert(name, value);
        }
    }

    pub fn update_variable(&mut self, name: String, value: Value) -> Result<()> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(&name) {
                scope.insert(name, value);
                return Ok(());
            }
        }

        Err(anyhow!("Variable \"{}\" not found", name))
    }

    pub fn with_scope<F, T>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.push_scope();
        let result = f(self);
        self.pop_scope();
        result
    }

    pub fn calculate(&mut self, statements: Vec<Statement>) -> Result<Vec<String>> {
        let mut results = Vec::new();
        for stmt in statements {
            match stmt {
                Statement::Declaration { name, expr } => {
                    let value = self.evaluate(&expr)?;
                    self.set_variable(name, value);
                }
                Statement::Assignment { name, expr } => {
                    let value = self.evaluate(&expr)?;
                    self.update_variable(name, value)?;
                }
                Statement::Print { format_str, args } => {
                    let mut formatted_str = format_str.clone();
                    for arg in args {
                        let value = self.evaluate(&arg)?.to_string();
                        formatted_str = formatted_str.replacen("{}", &value, 1);
                    }
                    results.push(formatted_str);
                }
                Statement::If {
                    condition,
                    then_block,
                    else_ifs,
                    else_block,
                } => {
                    let result =
                        self.execute_if_statement(condition, then_block, else_ifs, else_block)?;
                    results.extend(result);
                }
            }
        }
        Ok(results)
    }

    fn execute_if_statement(
        &mut self,
        condition: Expr,
        then_block: Vec<Statement>,
        else_ifs: Vec<(Expr, Vec<Statement>)>,
        else_block: Option<Vec<Statement>>,
    ) -> Result<Vec<String>> {
        if let Value::Boolean(true) = self.evaluate(&condition)? {
            return self.with_scope(|interpreter| interpreter.calculate(then_block));
        }

        for (else_if_expr, else_if_stmt) in else_ifs {
            if let Value::Boolean(true) = self.evaluate(&else_if_expr)? {
                return self.with_scope(|interpreter| interpreter.calculate(else_if_stmt));
            }
        }

        if let Some(else_block) = else_block {
            return self.with_scope(|interpreter| interpreter.calculate(else_block));
        }

        Ok(Vec::new())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Value(val) => Ok(val.clone()),
            Expr::Variable(i) => self
                .get_variable(i)
                .cloned()
                .ok_or_else(|| anyhow!("Variable \"{}\" not found", i)),
            Expr::Array(exprs) => {
                let mut res = Vec::<Value>::new();
                for expr in exprs {
                    let value = self.evaluate(&expr)?;
                    let current_type = value.get_type();

                    if let Some(last_value) = res.last() {
                        if current_type != last_value.get_type() {
                            return Err(anyhow!(
                                "Array elements must be the same type. Expected {:?}, found {:?}",
                                last_value.get_type(),
                                current_type
                            ));
                        }
                        res.push(value);
                    } else {
                        res.push(value);
                    }
                }

                Ok(Value::Array(res))
            }
            Expr::BinaryOp { left, op, right } => {
                let left_val = self.evaluate(left)?;
                let right_val = self.evaluate(right)?;

                let left_type = left_val.get_type();
                let right_type = right_val.get_type();

                match op {
                    Token::Add => match (left_type, right_type) {
                        (ValueType::Number, ValueType::Number) => Ok(Value::Number(
                            left_val.as_number()? + right_val.as_number()?,
                        )),
                        (ValueType::String, _) | (_, ValueType::String) => Ok(Value::String(
                            format!("{}{}", left_val.to_string(), right_val.to_string()),
                        )),
                        _ => Err(anyhow!(
                            "Cannot add {:?}: {:?} and {:?}: {:?}",
                            left_type,
                            left_val.to_string(),
                            right_type,
                            right_val.to_string()
                        )),
                    },
                    Token::Subtract => match (left_type, right_type) {
                        (ValueType::Number, ValueType::Number) => Ok(Value::Number(
                            left_val.as_number()? - right_val.as_number()?,
                        )),
                        _ => Err(anyhow!(
                            "Cannot subtract {:?}: {:?} and {:?}: {:?}",
                            left_type,
                            left_val.to_string(),
                            right_type,
                            right_val.to_string()
                        )),
                    },
                    Token::Multiply => match (left_type, right_type) {
                        (ValueType::Number, ValueType::Number) => Ok(Value::Number(
                            left_val.as_number()? * right_val.as_number()?,
                        )),
                        _ => Err(anyhow!(
                            "Cannot multiply {:?}: {:?} and {:?}: {:?}",
                            left_type,
                            left_val.to_string(),
                            right_type,
                            right_val.to_string()
                        )),
                    },
                    Token::Divide => {
                        if right_val.as_number()? == 0.0 {
                            return Err(anyhow!("Division by zero"));
                        } else {
                            Ok(Value::Number(
                                left_val.as_number()? / right_val.as_number()?,
                            ))
                        }
                    }
                    Token::LessThan => match (left_type, right_type) {
                        (ValueType::Number, ValueType::Number) => Ok(Value::Boolean(
                            left_val.as_number()? < right_val.as_number()?,
                        )),
                        (ValueType::String, ValueType::String) => {
                            Ok(Value::Boolean(left_val.to_string() < right_val.to_string()))
                        }
                        _ => Err(anyhow!(
                            "Cannot compare {:?}: {:?} and {:?}: {:?}",
                            left_type,
                            left_val.to_string(),
                            right_type,
                            right_val.to_string()
                        )),
                    },
                    Token::GreaterThan => match (left_type, right_type) {
                        (ValueType::Number, ValueType::Number) => Ok(Value::Boolean(
                            left_val.as_number()? > right_val.as_number()?,
                        )),
                        (ValueType::String, ValueType::String) => {
                            Ok(Value::Boolean(left_val.to_string() > right_val.to_string()))
                        }
                        _ => Err(anyhow!(
                            "Cannot compare {:?}: {:?} and {:?}: {:?}",
                            left_type,
                            left_val.to_string(),
                            right_type,
                            right_val.to_string()
                        )),
                    },
                    Token::GreaterThanOrEqual => match (left_type, right_type) {
                        (ValueType::Number, ValueType::Number) => Ok(Value::Boolean(
                            left_val.as_number()? >= right_val.as_number()?,
                        )),
                        (ValueType::String, ValueType::String) => Ok(Value::Boolean(
                            left_val.to_string() >= right_val.to_string(),
                        )),
                        _ => Err(anyhow!(
                            "Cannot compare {:?}: {:?} and {:?}: {:?}",
                            left_type,
                            left_val.to_string(),
                            right_type,
                            right_val.to_string()
                        )),
                    },
                    Token::LessThanOrEqual => match (left_type, right_type) {
                        (ValueType::Number, ValueType::Number) => Ok(Value::Boolean(
                            left_val.as_number()? <= right_val.as_number()?,
                        )),
                        (ValueType::String, ValueType::String) => Ok(Value::Boolean(
                            left_val.to_string() <= right_val.to_string(),
                        )),
                        _ => Err(anyhow!(
                            "Cannot compare {:?}: {:?} and {:?}: {:?}",
                            left_type,
                            left_val.to_string(),
                            right_type,
                            right_val.to_string()
                        )),
                    },
                    Token::Equal => match (left_type, right_type) {
                        (ValueType::Number, ValueType::Number) => Ok(Value::Boolean(
                            left_val.as_number()? == right_val.as_number()?,
                        )),
                        (ValueType::String, ValueType::String) => Ok(Value::Boolean(
                            left_val.to_string() == right_val.to_string(),
                        )),
                        (ValueType::Boolean, ValueType::Boolean) => {
                            Ok(Value::Boolean(left_val == right_val))
                        }
                        (ValueType::Array, ValueType::Array) => {
                            Ok(Value::Boolean(left_val == right_val))
                        }
                        _ => Ok(Value::Boolean(false)),
                    },
                    _ => Err(anyhow!(format!("Unknown operator {:?}", op))),
                }
            }
        }
    }
}

#[cfg(test)]
mod interpreter_tests {
    use super::*;
    use crate::{lexer::Lexer, parser::Parser};

    fn parse_and_interpret(input: &str) -> Result<Vec<String>> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let statements = parser.parse_statement().unwrap();
        let mut interpreter = Interpreter::new();
        interpreter.calculate(statements)
    }

    #[test]
    fn test_simple_if_true() {
        let result = parse_and_interpret("maybe true { shout(\"inside if\") }").unwrap();
        assert_eq!(result, vec!["inside if\n"]);
    }

    #[test]
    fn test_simple_if_false() {
        let result = parse_and_interpret("maybe false { shout(\"inside if\") }").unwrap();
        assert_eq!(result, Vec::<String>::new());
    }

    #[test]
    fn test_if_else_true_condition() {
        let result =
            parse_and_interpret("maybe true { whisper(\"in if\") } nah { whisper(\"in else\") }")
                .unwrap();
        assert_eq!(result, vec!["in if"]);
    }

    #[test]
    fn test_if_else_false_condition() {
        let result =
            parse_and_interpret("maybe false { whisper(\"in if\") } nah { whisper(\"in else\") }")
                .unwrap();
        assert_eq!(result, vec!["in else"]);
    }

    #[test]
    fn test_if_else_if_chain() {
        let result = parse_and_interpret(
            "suppose x = 7\nmaybe x > 10 { shout(\"big\") } perhaps x > 5 { shout(\"medium\") } nah { shout(\"small\") }"
        ).unwrap();
        assert_eq!(result, vec!["medium\n"]);
    }

    #[test]
    fn test_multiple_else_if() {
        let result = parse_and_interpret(
            "suppose x = 3\nmaybe x > 10 { shout(\"huge\") } perhaps x > 7 { shout(\"big\") } perhaps x > 5 { shout(\"medium\") } nah { shout(\"small\") }"
        ).unwrap();
        assert_eq!(result, vec!["small\n"]);
    }

    #[test]
    fn test_variable_scoping_inside_if() {
        let result = parse_and_interpret(
            "suppose x = 5\nmaybe x > 3 { suppose y = 10\nwhisper(\"y is {}\", y) }\nshout(\"x is {}\", x)",
        );

        // should work - y exists inside if block, x exists outside
        assert!(result.is_ok());
        let output = result.unwrap();
        assert_eq!(output, vec!["y is 10", "x is 5\n"]);
    }

    #[test]
    fn test_variable_scoping_y_not_available_outside() {
        let result = parse_and_interpret("maybe true { suppose y = 10 }\nwhisper(\"y is {}\", y)");

        // should fail - y doesn't exist outside the if block
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Variable \"y\" not found"));
    }

    #[test]
    fn test_variable_shadowing_in_if() {
        let result = parse_and_interpret(
            "suppose x = 5\nmaybe true { suppose x = 100\nshout(\"inner x: {}\", x) }\nwhisper(\"outer x: {}\", x)"
        ).unwrap();

        assert_eq!(result, vec!["inner x: 100\n", "outer x: 5"]);
    }

    #[test]
    fn test_nested_if_statements() {
        let result = parse_and_interpret(
            "suppose x = 8\nmaybe x > 5 { maybe x > 7 { shout(\"very big\") } nah { shout(\"medium\") } }",
        )
        .unwrap();

        assert_eq!(result, vec!["very big\n"]);
    }

    #[test]
    fn test_nested_scoping() {
        let result = parse_and_interpret(
            "suppose x = 1\nmaybe true { suppose x = 2\nmaybe true { suppose x = 3\nwhisper(\"innermost: {}\", x) }\nshout(\"middle: {}\", x) }\nshout(\"outer: {}\", x)"
        ).unwrap();

        assert_eq!(result, vec!["innermost: 3", "middle: 2\n", "outer: 1\n"]);
    }

    #[test]
    fn test_if_with_expressions() {
        let result = parse_and_interpret(
            "suppose x = 10\nsuppose y = 5\nmaybe x + y > 12 { whisper(\"sum is big\") } nah { whisper(\"sum is small\") }"
        ).unwrap();

        assert_eq!(result, vec!["sum is big"]);
    }

    #[test]
    fn test_multiple_statements_in_if_block() {
        let result = parse_and_interpret(
            "maybe true { suppose a = 1\nsuppose b = 2\nshout(\"a: {}\", a)\nwhisper(\"b: {}\", b) }",
        )
        .unwrap();

        assert_eq!(result, vec!["a: 1\n", "b: 2"]);
    }

    #[test]
    fn test_global_variables_accessible_in_if() {
        let result = parse_and_interpret(
            "suppose global = 42\nmaybe true { shout(\"global is {}\", global) }",
        )
        .unwrap();

        assert_eq!(result, vec!["global is 42\n"]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::Lexer, parser::Parser};

    // basic arithmetic tests
    #[test]
    fn test_addition() {
        let mut lexer = Lexer::new("whisper(\"{}\", 3 + 2)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["5".to_string()]);
    }

    #[test]
    fn test_operator_precedence() {
        let mut lexer = Lexer::new("whisper(\"{}\", 3 + 2 * 4)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["11".to_string()]); // 3 + (2 * 4) = 11
    }

    #[test]
    fn test_subtraction() {
        let mut lexer = Lexer::new("whisper(\"{}\", 10 - 4)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["6".to_string()]);
    }

    #[test]
    fn test_multiplication() {
        let mut lexer = Lexer::new("shout(\"{}\", 4 * 5)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["20\n".to_string()]);
    }

    #[test]
    fn test_division() {
        let mut lexer = Lexer::new("whisper(\"{}\", 15 / 3)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["5".to_string()]);
    }

    #[test]
    fn test_parentheses_precedence() {
        let mut lexer = Lexer::new("whisper(\"{}\", (3 + 2) * 4)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["20".to_string()]); // (3 + 2) * 4 = 20
    }

    #[test]
    fn test_complex_expression() {
        let mut lexer = Lexer::new("shout(\"{}\", 2 + 3 * 4 - 1)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["13\n".to_string()]); // 2 + (3 * 4) - 1 = 13
    }

    #[test]
    fn test_left_associativity() {
        let mut lexer = Lexer::new("shout(\"{}\", 10 - 3 - 2)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["5\n".to_string()]); // (10 - 3) - 2 = 5
    }

    #[test]
    fn test_nested_parentheses() {
        let mut lexer = Lexer::new("whisper(\"{}\", ((2 + 3) * 4) / 2)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["10".to_string()]); // ((2 + 3) * 4) / 2 = 10
    }

    #[test]
    fn test_decimal_numbers() {
        let mut lexer = Lexer::new("whisper(\"{}\", 3.5 + 2.25)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["5.75".to_string()]);
    }

    #[test]
    fn test_single_number() {
        let mut lexer = Lexer::new("shout(\"{}\", 42)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["42\n".to_string()]);
    }

    // variable tests
    #[test]
    fn test_variable_usage() {
        let mut lexer = Lexer::new("suppose x = 5\nwhisper(\"{}\", x + 3)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["8".to_string()]); // x(5) + 3 = 8
    }

    #[test]
    fn test_multiple_variables() {
        let mut lexer = Lexer::new("suppose x = 10\nsuppose y = 5\nwhisper(\"{}\", x * y)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["50".to_string()]); // x(10) * y(5) = 50
    }

    #[test]
    fn test_variable_in_let_expression() {
        let mut lexer = Lexer::new("suppose x = 5\nsuppose y = x + 10\nshout(\"{}\", y)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["15\n".to_string()]); // y = x(5) + 10 = 15
    }

    #[test]
    fn test_variable_redeclaration() {
        let mut lexer = Lexer::new("suppose x = 5\nsuppose x = 10\nshout(\"{}\", x)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["10\n".to_string()]); // x gets redeclared to 10
    }

    #[test]
    fn test_complex_variable_expression() {
        let mut lexer =
            Lexer::new("suppose a = 2\nsuppose b = 3\nsuppose c = 4\nshout(\"{}\", a + b * c)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["14\n".to_string()]); // a(2) + b(3) * c(4) = 2 + 12 = 14
    }

    // error handling tests
    #[test]
    fn test_undefined_variable_error() {
        let mut lexer = Lexer::new("shout(\"{}\", x + 5)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr);

        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .to_string()
            .contains("Variable \"x\" not found"));
    }

    #[test]
    fn test_division_by_zero() {
        let mut lexer = Lexer::new("shout(\"{}\", 5 / 0)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr);

        assert!(res.is_err());
        assert_eq!(res.unwrap_err().to_string(), "Division by zero");
    }

    // multiple statement tests
    #[test]
    fn test_multiple_expressions() {
        let mut lexer =
            Lexer::new("shout(\"{}\", 5 + 3)\nshout(\"{}\", 2 * 4)\nshout(\"{}\", 10 - 1)");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(
            res,
            vec!["8\n".to_string(), "8\n".to_string(), "9\n".to_string()]
        );
        // Results of each print expression
    }

    #[test]
    fn test_mixed_statements() {
        let mut lexer = Lexer::new(
            "suppose x = 10\nwhisper(\"{}\", 20 + 5)\nsuppose y = x * 2\nwhisper(\"{}\", y)",
        );
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let parsed_expr = parser.parse_statement().unwrap();
        let mut int = Interpreter::new();
        let res = int.calculate(parsed_expr).unwrap();

        assert_eq!(res, vec!["25".to_string(), "20".to_string()]); // results from print statements only
    }
}
