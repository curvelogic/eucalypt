//! Custom binding types replacing the `moniker` crate.
//!
//! Provides de Bruijn indexed variable binding with free/bound variable
//! distinction, scopes, and helper operations.

use crate::core::demand::Demand;
use serde::{Deserialize, Serialize};

/// A single let-binding entry: a name, its bound expression, and its demand
/// annotation.
///
/// The `demand` field starts at `Demand::default()` (all `Unknown`) and is
/// refined by analysis passes.  The STG compiler reads it at
/// `take_lambda_form` time to decide Value vs. Thunk.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CoreBinding<T: Clone> {
    /// The binding name (used to resolve `Var::Free` references during scoping).
    pub name: String,
    /// The bound expression.
    pub expr: T,
    /// Demand annotation — populated by analysis passes and the prune pass.
    pub demand: Demand,
}

impl<T: Clone> CoreBinding<T> {
    /// Create a binding with the given name and expression and a default
    /// (conservative, all-`Unknown`) demand annotation.
    pub fn new(name: String, expr: T) -> Self {
        CoreBinding {
            name,
            expr,
            demand: Demand::default(),
        }
    }

    /// Create a binding with an explicit demand annotation.
    pub fn with_demand(name: String, expr: T, demand: Demand) -> Self {
        CoreBinding { name, expr, demand }
    }
}

/// A variable reference — either free (by name) or bound (de Bruijn indexed)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Var {
    Free(String),
    Bound(BoundVar),
}

/// A bound variable with de Bruijn indices
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BoundVar {
    /// How many enclosing scopes to skip (0 = innermost)
    pub scope: u32,
    /// Which binding within that scope (0-indexed)
    pub binder: u32,
    /// Original name, preserved for debugging and pretty-printing
    pub name: Option<String>,
}

/// A scope binding pattern `P` over body `B`
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Scope<P, B> {
    pub pattern: P,
    pub body: B,
}

/// Collect the free variable name from a `Var` reference.
pub fn var_free_name(v: &Var) -> Option<&str> {
    match v {
        Var::Free(n) => Some(n.as_str()),
        Var::Bound(_) => None,
    }
}
