use crate::lexer::Token;

#[derive(Debug)]
pub enum ParserError {
    MissingClosingBrace,
    UnexpectedToken(Token),

    // type errors
    ExpectedNumber,

    // print statement errors
    ExpectedOpeningParentAfterPrint,
    ExpectedClosingParentInPrint,
    InvalidFormatString,
    IncorrectArgumentCount { found: usize, expected: usize },

    // if statement errors
    ExpectedOpeningBraceAfterIf,
    ExpectedClosingBraceAfterIf,

    // variable declaration errors
    ExpectedAssignmentAfterVariable,
    ExpectedIdentifierAfterLet,

    // variable assignment errors
    ExpectedAssignmentOperator(String),

    // expression errors
    ExpectedClosingParentAfterExpression,

    // format string errors
    InvalidClosingBrace,
    InvalidOpeningBrace,

    ExpectedClosingBracket,
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::MissingClosingBrace => write!(f, "Missing closing brace"),
            ParserError::UnexpectedToken(token) => write!(f, "Unexpected token {:?}", token),

            // type errors
            ParserError::ExpectedNumber => write!(f, "Expected number, got boolean"),

            // print statement errors
            ParserError::ExpectedOpeningParentAfterPrint => {
                write!(f, "Expected an opening ( after shout statement")
            }
            ParserError::ExpectedClosingParentInPrint => {
                write!(f, "Expected a closing ) in a shout statement")
            }
            ParserError::InvalidFormatString => write!(f, "Invalid format after shout statement"),
            ParserError::IncorrectArgumentCount { found, expected } => write!(
                f,
                "Incorrect arguments was passed. Found {}, expected {}",
                found, expected
            ),

            // if statement errors
            ParserError::ExpectedOpeningBraceAfterIf => {
                write!(f, "Expected '{{' after maybe")
            }
            ParserError::ExpectedClosingBraceAfterIf => {
                write!(f, "Expected a closing '}}' after maybe")
            }

            // variable declaration errors
            ParserError::ExpectedAssignmentAfterVariable => {
                write!(f, "Expected '=' after variable name")
            }
            ParserError::ExpectedIdentifierAfterLet => {
                write!(f, "Expected indentifier after 'suppose'")
            }
            ParserError::ExpectedAssignmentOperator(i) => {
                write!(f, "Expected '=' after identifier '{}'", i)
            }

            // expression errors
            ParserError::ExpectedClosingParentAfterExpression => {
                write!(f, "Expected ')' after expression")
            }

            // format string errors
            ParserError::InvalidClosingBrace => write!(f, "Invalid '}}' in format string"),
            ParserError::InvalidOpeningBrace => write!(f, "Invalid '{{' in format string"),

            // array errors
            ParserError::ExpectedClosingBracket => write!(f, "Expected ']' in array declaration"),
        }
    }
}
