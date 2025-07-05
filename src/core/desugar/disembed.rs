//! Support desugaring embedded core syntax
//!
//! This module now redirects to the Rowan AST implementation for core embedding.
//! The legacy AST implementation has been removed as part of eliminating legacy AST usage.

use crate::{
    core::{error::CoreError, expr::*},
    syntax::rowan::ast as rowan_ast,
};

use super::desugarer::Desugarer;
use super::rowan_disembed::core_from_rowan_embedding;

/// Convert Rowan AST expression containing core embedding to core expr
///
/// This is a compatibility wrapper that works with Rowan AST.
/// The legacy AST version has been removed.
pub fn core_from_embedding(
    desugarer: &mut Desugarer,
    soup: &rowan_ast::Soup,
) -> Result<RcExpr, CoreError> {
    core_from_rowan_embedding(desugarer, soup)
}
