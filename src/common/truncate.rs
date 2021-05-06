//! Marker for truncated debug output

/// Marker for truncated output
#[derive(Clone)]
pub struct Truncated<T> {
    content: T,
}

impl<T> Truncated<T> {
    pub fn new(content: T) -> Self {
        Truncated { content }
    }

    pub fn content(&self) -> &T {
        &self.content
    }
}
