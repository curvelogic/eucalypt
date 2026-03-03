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
        /// A brief description of why the literal is invalid
        reason: ZdtInvalidReason,
    },
    /// A bracket expression was opened but not closed
    UnclosedBracketExpr {
        range: TextRange,
    },
    /// A bracket expression uses a close bracket that does not match the open
    MismatchedBrackets {
        open_range: TextRange,
        close_range: TextRange,
        /// The character that was expected
        expected_close: char,
        /// The character that was found
        actual_close: char,
    },
    /// An unknown bracket pair was used — not defined as an idiom bracket
    UnknownBracketPair {
        range: TextRange,
        open_char: char,
    },
}

/// The reason a `t"..."` date/time literal is invalid
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ZdtInvalidReason {
    /// The string is empty
    Empty,
    /// The string looks like a date/time (starts with digits in YYYY-MM-DD
    /// form) but the date itself does not exist (e.g. February 30)
    InvalidDate,
    /// The string does not match any recognised date/time format
    Malformed,
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
            ParseError::InvalidZdtLiteral { reason, .. } => match reason {
                ZdtInvalidReason::Empty => write!(
                    f,
                    "date/time literal cannot be empty\n  \
                     help: the t-string syntax requires a valid ISO 8601 date or date-time, \
                     e.g. 2023-01-15 or 2023-01-15T10:30:00Z"
                ),
                ZdtInvalidReason::InvalidDate => write!(
                    f,
                    "date/time literal contains an invalid date\n  \
                     help: check that the day exists in the given month and year \
                     (e.g. February has at most 29 days, and only in a leap year)"
                ),
                ZdtInvalidReason::Malformed => write!(
                    f,
                    "invalid date/time literal\n  \
                     help: the t-string syntax requires ISO 8601 format, \
                     e.g. 2023-01-15 or 2023-01-15T10:30:00Z\n  \
                     help: supported formats: \
                     YYYY-MM-DD, YYYY-MM-DDTHH:MM:SS, YYYY-MM-DDTHH:MM:SSZ, \
                     YYYY-MM-DDTHH:MM:SS+HH:MM"
                ),
            },
            ParseError::UnclosedBracketExpr { .. } => {
                write!(f, "unclosed bracket expression (missing closing bracket)")
            }
            ParseError::MismatchedBrackets {
                expected_close,
                actual_close,
                ..
            } => write!(
                f,
                "mismatched brackets: expected '{expected_close}' but found '{actual_close}'\n  \
                 help: bracket pairs must match, e.g. '⟦' must be closed with '⟧'"
            ),
            ParseError::UnknownBracketPair { open_char, .. } => write!(
                f,
                "unknown bracket pair starting with '{open_char}'\n  \
                 help: bracket pairs must be declared before use, \
                 e.g. '(⟦ x ⟧): my-functor(x)'"
            ),
        }
    }
}
