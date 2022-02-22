//! Overall high-level error type for eucalypt
use crate::common::sourcemap::SourceMap;
use crate::core::error::CoreError;
use crate::eval::error::ExecutionError;
use crate::import::error::SourceError;
use crate::syntax::error::ParserError;
use crate::syntax::error::SyntaxError;
use crate::syntax::import::ImportError;
use codespan_reporting::diagnostic::Diagnostic;
use std::fmt::Display;
use std::io;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum EucalyptError {
    #[error(transparent)]
    Parser(#[from] ParserError),
    #[error(transparent)]
    Syntax(#[from] SyntaxError),
    #[error(transparent)]
    Core(#[from] CoreError),
    #[error(transparent)]
    Import(#[from] ImportError),
    #[error(transparent)]
    Source(#[from] SourceError),
    #[error(transparent)]
    Execution(#[from] ExecutionError),
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error("unknown resource {0}")]
    UnknownResource(String),
    #[error("path {0} could not be read")]
    FileCouldNotBeRead(String),
}

fn default_diagnostic<E>(e: &E) -> Diagnostic<usize>
where
    E: Display,
{
    Diagnostic::error().with_message(format!("{}", e))
}

impl EucalyptError {
    /// Convert to a diagnostic
    pub fn to_diagnostic(&self, source_map: &SourceMap) -> Diagnostic<usize> {
        match self {
            EucalyptError::Parser(e) => e.to_diagnostic(),
            EucalyptError::Core(e) => e.to_diagnostic(source_map),
            EucalyptError::Source(e) => e.to_diagnostic(source_map),
            EucalyptError::Execution(e) => e.to_diagnostic(source_map),
            e => default_diagnostic(e),
        }
    }
}
