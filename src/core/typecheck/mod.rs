//! Gradual type system for eucalypt.
//!
//! This module provides the foundational type infrastructure:
//!
//! - [`types`] — `Type` enum, `TypeScheme`, and `TypeVarId`
//! - [`parse`] — recursive descent parser for type annotation strings
//! - [`env`] — scoped type environment (`TypeEnv`)
//! - [`error`] — `TypeWarning` diagnostic produced by the type checker
//!
//! The type checker itself (bidirectional checking, subtyping, instantiation)
//! lives in downstream beads and is not yet implemented here.
//!
//! Type issues are always reported as warnings — they never prevent evaluation.
//! See `docs/development/gradual-typing-spec.md` for the full specification.

pub mod env;
pub mod error;
pub mod parse;
pub mod subtype;
pub mod types;
