//! Provides `HasSpan` trait for extracting code spans from objects.
use codespan::Span;

pub trait HasSpan {
    fn span(&self) -> Span;
}

impl<T: HasSpan> HasSpan for Vec<T> {
    fn span(&self) -> Span {
        match self.split_first() {
            Some((first, rest)) => rest.iter().fold(first.span(), |s, e| s.merge(e.span())),
            None => Span::default(),
        }
    }
}
