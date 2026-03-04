use crate::syntax::rowan::ParseError as RowanParseError;
use crate::syntax::span::HasSpan;
use codespan::{ByteIndex, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
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

impl SyntaxError {}

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
            .with_message(format!("{self}"))
            .with_labels(vec![Label::primary(self.file_id(), self.span())])
    }
}

/// Convert a `rowan::TextRange` byte offset to a `codespan::ByteIndex`.
fn text_size_to_byte_index(ts: rowan::TextSize) -> ByteIndex {
    ByteIndex(u32::from(ts))
}

/// Convert a `rowan::TextRange` to a `codespan::Span`.
fn text_range_to_span(range: rowan::TextRange) -> Span {
    Span::new(
        text_size_to_byte_index(range.start()),
        text_size_to_byte_index(range.end()),
    )
}

/// Extract the primary text range from a Rowan parse error.
fn rowan_error_range(error: &RowanParseError) -> Option<rowan::TextRange> {
    use crate::syntax::rowan::error::ParseError::*;
    match error {
        UnexpectedToken { range, .. }
        | UnclosedSingleQuote { range }
        | UnclosedDoubleQuote { range }
        | InvalidParenExpr { range, .. }
        | UnterminatedBlock { range, .. }
        | EmptyDeclarationBody { range }
        | MalformedDeclarationHead { range }
        | InvalidFormalParameter { range, .. }
        | InvalidOperatorName { range, .. }
        | InvalidPropertyName { range, .. }
        | SurplusContent { range }
        | ReservedCharacter { range }
        | EmptyExpression { range }
        | UnclosedStringInterpolation { range }
        | InvalidZdtLiteral { range, .. }
        | InvalidDoubleColon { range }
        | UnclosedBracketExpr { range }
        | UnknownBracketPair { range, .. } => Some(*range),
        MissingDeclarationColon { head_range } => Some(*head_range),
        MismatchedBrackets { open_range, .. } => Some(*open_range),
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
    /// Parse errors from the Rowan parser, with source location information.
    ///
    /// Carries the file id and the list of parse errors with their `TextRange`
    /// positions so that diagnostics can show the actual error location rather
    /// than defaulting to the start of the file.
    #[error("{}", .1.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(", "))]
    ParseErrors(usize, Vec<RowanParseError>),
}

impl ParserError {
    /// Convert to a diagnostic
    pub fn to_diagnostic(&self) -> Diagnostic<usize> {
        match self {
            ParserError::Syntax(e) => e.to_diagnostic(),
            ParserError::Io(e) => Diagnostic::error().with_message(format!("IO Error: {e}")),
            ParserError::ParseErrors(file_id, errors) => {
                if let Some(first) = errors.first() {
                    let message = first.to_string();
                    let mut diag = Diagnostic::error().with_message(message);
                    if let Some(range) = rowan_error_range(first) {
                        diag = diag
                            .with_labels(vec![Label::primary(*file_id, text_range_to_span(range))]);
                    }
                    // Add secondary labels for additional errors if any
                    let secondary_labels: Vec<Label<usize>> = errors
                        .iter()
                        .skip(1)
                        .filter_map(|e| {
                            rowan_error_range(e).map(|r| {
                                Label::secondary(*file_id, text_range_to_span(r))
                                    .with_message(e.to_string())
                            })
                        })
                        .collect();
                    if !secondary_labels.is_empty() {
                        diag = diag.with_labels(secondary_labels);
                    }
                    diag
                } else {
                    Diagnostic::error().with_message("parse error")
                }
            }
        }
    }
}
