//! RenderError

use std::io;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum RenderError {
    #[error(transparent)]
    Io(#[from] io::Error),
}
