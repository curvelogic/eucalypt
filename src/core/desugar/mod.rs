//! Desugaring of concrete AST into initial core representation.
//!
//! This:
//! - transforms AST blocks into a combination of a core let and blocks
//! - extracts targets
//! - splicies in imports at the appropriate places
//!
//! Some content (read from YAML, JSON etc.) may already be in core
//! syntax. Desugaring leaves it unchanged but can splice it in at
//! import points.

pub mod desugarable;
pub mod desugarer;
pub mod disembed;
pub mod escape;
pub mod literal;
pub mod rowan_ast;
pub mod rowan_disembed;

#[cfg(test)]
#[path = "rowan_integration_test.rs"]
mod rowan_integration_test;

#[cfg(test)]
#[path = "rowan_comprehensive_test.rs"]
mod rowan_comprehensive_test;

pub use desugarable::Content;
pub use desugarer::Desugarer;
