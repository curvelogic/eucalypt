use crate::syntax::span::HasSpan;
use codespan::{ByteIndex, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::ParseError;
use std::io;
use thiserror::Error;

/// An error forming the AST in semantic action, often wrapped by
/// LALRPOP parse error
#[derive(Eq, PartialEq, Debug, Clone, Error)]
pub enum SyntaxError {
    #[error("unexpected character {1} found in input")]
    UnexpectedCharacter(usize, char, ByteIndex),
    #[error("unexpected end of input")]
    UnexpectedEndOfInput(usize, Span),
    #[error("no item preceding colon to be interpreted as a declaration head")]
    MissingDeclarationHead(usize, Span),
    #[error("item preceding colon could not be interpreted as a declaration head")]
    InvalidDeclarationHead(usize, Span),
    #[error("elements could not be parsed as an expression")]
    InvalidExpression(usize, Span),
    #[error("invalid number literal")]
    InvalidNumber(usize, Span),
    #[error("empty text where expression was expected")]
    EmptyExpression(usize, Span),
    #[error("an extra token was found in the input")]
    ExtraToken(usize, Span),
    #[error("an unrecognised token was found in the input")]
    UnrecognisedToken(usize, Span, Vec<String>),
    #[error("an invalid token was found in the input")]
    InvalidToken(usize, Span),
    #[error("input was not correctly formed: {1}")]
    InvalidInputFormat(usize, String),
    #[error("string literal was an invalid string pattern: {1}")]
    InvalidStringPattern(usize, Span),
}

impl SyntaxError {
    /// Convert a LALRPOP error from string pattern parsing into a
    /// syntax error that can pass through the AST parser.
    pub fn from_string_pattern_error<T>(
        file_id: usize,
        err: ParseError<ByteIndex, T, SyntaxError>,
    ) -> Self {
        match err {
            ParseError::User {
                error: syntax_error,
            } => syntax_error,
            ParseError::ExtraToken { token: (s, _t, e) } => {
                SyntaxError::ExtraToken(file_id, Span::new(s, e))
            }
            ParseError::UnrecognizedToken {
                token: (s, _t, e),
                expected,
            } => SyntaxError::UnrecognisedToken(file_id, Span::new(s, e), expected.to_vec()),
            ParseError::UnrecognizedEOF { location: l, .. } => {
                SyntaxError::UnexpectedEndOfInput(file_id, Span::new(l, l))
            }
            ParseError::InvalidToken { .. } => unreachable!(),
        }
    }
}

impl HasSpan for SyntaxError {
    fn span(&self) -> Span {
        use self::SyntaxError::*;

        match *self {
            UnexpectedCharacter(_f, _c, i) => Span::new(i, i),
            UnexpectedEndOfInput(_f, s) => s,
            MissingDeclarationHead(_f, s) => s,
            InvalidDeclarationHead(_f, s) => s,
            InvalidExpression(_f, s) => s,
            InvalidNumber(_f, s) => s,
            EmptyExpression(_f, s) => s,
            ExtraToken(_f, s) => s,
            UnrecognisedToken(_f, s, _) => s,
            InvalidToken(_f, s) => s,
            InvalidInputFormat(_f, _) => Span::new(ByteIndex::default(), ByteIndex::default()),
            InvalidStringPattern(_f, s) => s,
        }
    }
}

impl SyntaxError {
    pub fn file_id(&self) -> usize {
        use self::SyntaxError::*;

        match *self {
            UnexpectedCharacter(f, _, _) => f,
            UnexpectedEndOfInput(f, _) => f,
            MissingDeclarationHead(f, _) => f,
            InvalidDeclarationHead(f, _) => f,
            InvalidExpression(f, _) => f,
            InvalidNumber(f, _) => f,
            EmptyExpression(f, _) => f,
            ExtraToken(f, _) => f,
            UnrecognisedToken(f, _, _) => f,
            InvalidToken(f, _) => f,
            InvalidInputFormat(f, _) => f,
            InvalidStringPattern(f, _) => f,
        }
    }

    pub fn to_diagnostic(&self) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("{}", self))
            .with_labels(vec![Label::primary(self.file_id(), self.span())])
    }
}

/// A canonicalised error for all parse related errors, parse, IO,
/// AST, free of token references.
#[derive(Debug, Error)]
pub enum ParserError {
    #[error(transparent)]
    Io(io::Error),
    #[error(transparent)]
    Syntax(SyntaxError),
}

/// Freely convert to ParserError
impl ParserError {
    pub fn from_lalrpop<T>(
        file_id: usize,
        err: ParseError<ByteIndex, T, SyntaxError>,
    ) -> ParserError {
        match err {
            ParseError::User {
                error: syntax_error,
            } => ParserError::Syntax(syntax_error),
            ParseError::ExtraToken { token: (s, _t, e) } => {
                ParserError::Syntax(SyntaxError::ExtraToken(file_id, Span::new(s, e)))
            }
            ParseError::UnrecognizedToken {
                token: (s, _t, e),
                expected,
            } => ParserError::Syntax(SyntaxError::UnrecognisedToken(
                file_id,
                Span::new(s, e),
                expected.to_vec(),
            )),
            ParseError::UnrecognizedEOF { location: l, .. } => {
                ParserError::Syntax(SyntaxError::UnexpectedEndOfInput(file_id, Span::new(l, l)))
            }
            ParseError::InvalidToken { .. } => unreachable!(),
        }
    }
}

impl ParserError {
    /// Convert to a diagnostic
    pub fn to_diagnostic(&self) -> Diagnostic<usize> {
        match self {
            ParserError::Syntax(e) => e.to_diagnostic(),
            ParserError::Io(e) => Diagnostic::error().with_message(format!("IO Error: {}", e)),
        }
    }
}
