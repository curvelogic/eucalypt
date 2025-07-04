//! `SourceError` are parse and format errors for foreign formats
//! (YAML etc.)`
use crate::{
    common::sourcemap::SourceMap,
    core::error::CoreError,
    syntax::{error::ParserError, span::HasSpan},
};
use codespan::Span;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files,
};
use thiserror::Error;

/// An error forming the AST in semantic action, often wrapped by
/// LALRPOP parse error
#[derive(Debug, Error)]
pub enum SourceError {
    #[error("invalid yaml or json syntax {0}")]
    InvalidYaml(String, usize, Span),
    #[error("invalid xml syntax {0}")]
    InvalidXml(String, usize, Span),
    #[error("invalid toml syntax {0}")]
    InvalidToml(String, usize),
    #[error("invalid edn: {0}")]
    InvalidEdn(Box<edn_format::ParserErrorWithContext>, usize),
    #[error("character set error after {0}")]
    CharSetError(String, usize),
    #[error("invalid key {0}")]
    InvalidBlockKey(String, usize, Span),
    #[error("could not parse number {0}")]
    InvalidNumber(String, usize, Span),
    #[error("bad source format: {0}")]
    InvalidSource(String, usize),
    #[error("embedded eucalypt error {0}")]
    EmbeddedCoreError(CoreError, usize, Span),
    #[error("embedded eucalypt parse error {0}")]
    EmbeddedParserError(ParserError, usize, Span),
    #[error("unknown source format {0}")]
    UnknownSourceFormat(String, usize),
    #[error(transparent)]
    Files(#[from] files::Error),
}

impl HasSpan for SourceError {
    fn span(&self) -> Span {
        match *self {
            SourceError::InvalidYaml(_, _, s) => s,
            SourceError::InvalidXml(_, _, s) => s,
            SourceError::InvalidToml(_, _) => Span::default(),
            SourceError::CharSetError(_, _) => Span::default(),
            SourceError::InvalidBlockKey(_, _, s) => s,
            SourceError::InvalidNumber(_, _, s) => s,
            SourceError::InvalidSource(_, _) => Span::default(),
            SourceError::EmbeddedCoreError(_, _, s) => s,
            SourceError::EmbeddedParserError(_, _, s) => s,
            SourceError::Files(_) => Span::default(),
            SourceError::UnknownSourceFormat(_, _) => Span::default(),
            SourceError::InvalidEdn(_, _) => Span::default(),
        }
    }
}

impl SourceError {
    pub fn file_id(&self) -> usize {
        match *self {
            SourceError::InvalidYaml(_, f, _) => f,
            SourceError::InvalidXml(_, f, _) => f,
            SourceError::InvalidToml(_, f) => f,
            SourceError::CharSetError(_, f) => f,
            SourceError::InvalidBlockKey(_, f, _) => f,
            SourceError::InvalidNumber(_, f, _) => f,
            SourceError::InvalidSource(_, f) => f,
            SourceError::EmbeddedCoreError(_, f, _) => f,
            SourceError::EmbeddedParserError(_, f, _) => f,
            SourceError::Files(_) => unreachable!(),
            SourceError::UnknownSourceFormat(_, f) => f,
            SourceError::InvalidEdn(_, f) => f,
        }
    }

    pub fn to_diagnostic(&self, source_map: &SourceMap) -> Diagnostic<usize> {
        match self {
            SourceError::EmbeddedCoreError(e, _, _) => e.to_diagnostic(source_map),
            SourceError::EmbeddedParserError(e, _, _) => e.to_diagnostic(),
            _ => Diagnostic::error()
                .with_message(format!("{self}"))
                .with_labels(vec![Label::primary(self.file_id(), self.span())]),
        }
    }
}
