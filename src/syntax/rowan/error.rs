use std::fmt;

use rowan::TextRange;

use super::kind::SyntaxKind;

/// A syntax error detected and recorded during parse
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseError {
    UnexpectedToken {
        expected: SyntaxKind,
        actual: SyntaxKind,
        range: TextRange,
    },
    UnclosedSingleQuote {
        range: TextRange,
    },
    UnclosedDoubleQuote {
        range: TextRange,
    },
    InvalidParenExpr {
        open_paren_range: Option<TextRange>,
        range: TextRange,
    },
    UnterminatedBlock {
        open_brace_range: Option<TextRange>,
        range: TextRange,
    },
    EmptyDeclarationBody {
        range: TextRange,
    },
    MissingDeclarationColon {
        head_range: TextRange,
    },
    MalformedDeclarationHead {
        range: TextRange,
    },
    InvalidFormalParameter {
        head_range: TextRange,
        range: TextRange,
    },
    InvalidOperatorName {
        head_range: TextRange,
        range: TextRange,
    },
    InvalidPropertyName {
        head_range: TextRange,
        range: TextRange,
    },
    SurplusContent {
        range: TextRange,
    },
    ReservedCharacter {
        range: TextRange,
    },
    EmptyExpression {
        range: TextRange,
    },
    UnclosedStringInterpolation {
        range: TextRange,
    },
    InvalidZdtLiteral {
        range: TextRange,
    },
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                expected, actual, ..
            } => write!(f, "expected {expected:?}, found {actual:?}"),
            ParseError::UnclosedSingleQuote { .. } => {
                write!(f, "unterminated single-quoted string")
            }
            ParseError::UnclosedDoubleQuote { .. } => write!(f, "unterminated string literal"),
            ParseError::InvalidParenExpr { .. } => {
                write!(f, "invalid parenthesised expression")
            }
            ParseError::UnterminatedBlock { .. } => write!(f, "unterminated block (missing '}}')"),
            ParseError::EmptyDeclarationBody { .. } => {
                write!(f, "empty declaration body where a value was expected")
            }
            ParseError::MissingDeclarationColon { .. } => {
                write!(f, "missing ':' after declaration head")
            }
            ParseError::MalformedDeclarationHead { .. } => {
                write!(f, "malformed declaration head")
            }
            ParseError::InvalidFormalParameter { .. } => {
                write!(f, "invalid formal parameter in function definition")
            }
            ParseError::InvalidOperatorName { .. } => write!(f, "invalid operator name"),
            ParseError::InvalidPropertyName { .. } => write!(f, "invalid property name"),
            ParseError::SurplusContent { .. } => write!(f, "unexpected content after expression"),
            ParseError::ReservedCharacter { .. } => write!(f, "reserved character"),
            ParseError::EmptyExpression { .. } => {
                write!(f, "empty expression where a value was expected")
            }
            ParseError::UnclosedStringInterpolation { .. } => {
                write!(f, "unterminated string interpolation (missing '}}')")
            }
            ParseError::InvalidZdtLiteral { .. } => write!(f, "invalid date/time literal"),
        }
    }
}
