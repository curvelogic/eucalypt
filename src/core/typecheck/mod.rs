//! Gradual type system for eucalypt.
//!
//! This module provides the full type-checking infrastructure:
//!
//! - [`types`] — `Type` enum, `TypeScheme`, and `TypeVarId`
//! - [`parse`] — recursive descent parser for type annotation strings
//! - [`env`] — scoped type environment (`TypeEnv`)
//! - [`error`] — `TypeWarning` diagnostic produced by the type checker
//! - [`subtype`] — subtyping (`<:`) and consistency (`~`) relations
//! - [`check`] — bidirectional type checker over core expressions
//!
//! Type issues are always reported as warnings — they never prevent evaluation.
//! See `docs/development/gradual-typing-spec.md` for the full specification.

pub mod check;
pub mod env;
pub mod error;
pub mod parse;
pub mod subtype;
pub mod types;
