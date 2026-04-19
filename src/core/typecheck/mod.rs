//! Gradual type checker for eucalypt.
//!
//! This module will house the type inference and checking passes that run
//! on simplified core expressions after cooking and verification.
//!
//! Type issues are always reported as warnings — they never prevent evaluation.
//! See `docs/development/gradual-typing-spec.md` for the full specification.
pub mod error;
