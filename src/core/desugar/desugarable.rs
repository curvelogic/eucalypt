//! The Desugarable trait

use crate::core::{error::CoreError, expr::RcExpr};

use super::Desugarer;

/// Various AST types are desugarable into core expressions
pub trait Desugarable {
    fn desugar(&self, _: &mut Desugarer) -> Result<RcExpr, CoreError>;
}

/// Content for desugaring
///
/// Desugaring receives a mix of ASTs which need translating and Core
/// Expression that have been parsed directly from data formats which
/// just need naming and weaving in.
///
/// Both have a `usize` for diagnostics.
/// A means of passing content to the desugar phase
pub struct Content<'a>(usize, &'a dyn Desugarable);

impl<'a> Content<'a> {
    pub fn new(file_id: usize, content: &'a dyn Desugarable) -> Self {
        Content(file_id, content)
    }

    pub fn file_id(&self) -> usize {
        self.0
    }

    pub fn content(&self) -> &dyn Desugarable {
        self.1
    }
}
