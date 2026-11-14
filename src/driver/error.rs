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
    Source(#[from] Box<SourceError>),
    #[error(transparent)]
    Execution(#[from] Box<ExecutionError>),
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error("unknown resource {0}")]
    UnknownResource(String),
    /// A file could not be read.
    ///
    /// The first field is the path. The optional second field is the
    /// OS-level reason (e.g. "No such file or directory", "Permission denied").
    #[error("{}", format_file_could_not_be_read(.0, .1.as_deref()))]
    FileCouldNotBeRead(String, Option<String>),
    /// A file could not be written.
    ///
    /// The first field is the path. The optional second field is the
    /// OS-level reason (e.g. "Permission denied", "No space left on device").
    #[error("{}", format_file_could_not_be_written(.0, .1.as_deref()))]
    FileCouldNotBeWritten(String, Option<String>),
    /// `eu test` found no test files in the inputs it was given.
    ///
    /// The field names the inputs searched.
    #[error("no test files found in {0}")]
    NoTestsFound(String),
}

/// Format a "file could not be read" error message including the OS reason when available.
fn format_file_could_not_be_read(path: &str, reason: Option<&str>) -> String {
    match reason {
        Some(r) => format!("could not read '{path}': {r}"),
        None => format!("could not read '{path}'"),
    }
}

/// Format a "file could not be written" error message including the OS reason when available.
fn format_file_could_not_be_written(path: &str, reason: Option<&str>) -> String {
    match reason {
        Some(r) => format!("could not write '{path}': {r}"),
        None => format!("could not write '{path}'"),
    }
}

fn default_diagnostic<E>(e: &E) -> Diagnostic<usize>
where
    E: Display,
{
    Diagnostic::error().with_message(format!("{e}"))
}

impl EucalyptError {
    /// Whether this error is the reader of our stdout having stopped
    /// listening (EPIPE) — `eu … | head`, or quitting `less`.
    ///
    /// The CLI exits 0 silently on this, as it already does for a closed
    /// pipe during rendering (eu-1tkk.7.25): the consumer has what it
    /// wanted, so nothing went wrong.
    pub fn is_broken_pipe(&self) -> bool {
        match self {
            EucalyptError::Io(e) => e.kind() == io::ErrorKind::BrokenPipe,
            EucalyptError::Execution(e) => e.is_output_stream_closed(),
            _ => false,
        }
    }

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

    /// Convert to a diagnostic, reading `files` for the source evidence
    /// that gates some hints (eu-oaz72.28).
    pub fn to_diagnostic_with_source(
        &self,
        source_map: &SourceMap,
        files: &codespan_reporting::files::SimpleFiles<String, String>,
    ) -> Diagnostic<usize> {
        match self {
            EucalyptError::Execution(e) => e.to_diagnostic_with_source(source_map, files),
            e => e.to_diagnostic(source_map),
        }
    }
}
