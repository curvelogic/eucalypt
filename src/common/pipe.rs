//! A closed stdout pipe (EPIPE) is not a failure.
//!
//! `eu … | head`, or quitting `less`, closes the reader's end while `eu` is
//! still writing. The reader has what it wanted, so `eu` stops writing and
//! exits as it would have (eu-1tkk.7.25). Every other write failure (a full
//! disk, a read-only target) is still reported. These helpers are the one
//! place that tells the two apart.

use std::io;

/// Whether `error` is the reader of our output having gone away.
pub fn is_broken_pipe(error: &io::Error) -> bool {
    error.kind() == io::ErrorKind::BrokenPipe
}

/// Pass on a write's result, treating a closed pipe as success.
pub fn ignore_broken_pipe(result: io::Result<()>) -> io::Result<()> {
    match result {
        Err(e) if is_broken_pipe(&e) => Ok(()),
        other => other,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_a_broken_pipe_is_ignored() {
        let pipe = || io::Error::from(io::ErrorKind::BrokenPipe);
        let full = || io::Error::other("no space left on device");
        assert!(is_broken_pipe(&pipe()));
        assert!(!is_broken_pipe(&full()));
        assert!(ignore_broken_pipe(Err(pipe())).is_ok());
        assert!(ignore_broken_pipe(Err(full())).is_err());
        assert!(ignore_broken_pipe(Ok(())).is_ok());
    }
}
