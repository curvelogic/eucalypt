//! RenderError

use std::io;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum RenderError {
    #[error(transparent)]
    Io(#[from] io::Error),
    /// A format-specific serialiser refused the document it was given.
    ///
    /// Distinct from `Io`: nothing is wrong with the output stream, the
    /// document could not be turned into bytes (eu-1tkk.7.25).
    #[error("{0}")]
    Serialisation(String),
}

impl RenderError {
    /// Whether this is the reader having stopped listening (EPIPE).
    ///
    /// A broken pipe is not a failure — `eu … | head` closes the pipe once
    /// it has what it wants — so the driver exits quietly on this, while
    /// every other write error becomes a diagnostic (eu-1tkk.7.25).
    pub fn is_broken_pipe(&self) -> bool {
        matches!(self, RenderError::Io(e) if e.kind() == io::ErrorKind::BrokenPipe)
    }
}
