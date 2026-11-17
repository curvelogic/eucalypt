//! Custom binding types replacing the `moniker` crate.
//!
//! Provides de Bruijn indexed variable binding with free/bound variable
//! distinction, scopes, and helper operations.

use crate::core::demand::Demand;
use serde::{Deserialize, Serialize};

/// A single let-binding entry: a name, its bound expression, its demand
/// annotation and, for a lambda, its demand signature.
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
    /// The demand the bound lambda places on each of its parameters, set by
    /// demand analysis on a let-bound lambda. The STG compiler gives a call
    /// whose callee is a bound variable the signature of the binding that
    /// variable names, so two bindings that share a name keep their own
    /// signatures (eu-oaz72.66). Not serialised: it exists only between
    /// demand analysis and STG compilation, so the blob encoding of core is
    /// unchanged.
    #[serde(skip)]
    pub signature: Option<Vec<Demand>>,
}

impl<T: Clone> CoreBinding<T> {
    /// Create a binding with the given name and expression and a default
    /// (conservative, all-`Unknown`) demand annotation.
    pub fn new(name: String, expr: T) -> Self {
        CoreBinding {
            name,
            expr,
            demand: Demand::default(),
            signature: None,
        }
    }

    /// Create a binding with an explicit demand annotation.
    pub fn with_demand(name: String, expr: T, demand: Demand) -> Self {
        CoreBinding {
            name,
            expr,
            demand,
            signature: None,
        }
    }

    /// The same binding over a new expression: name, demand and signature
    /// are kept. Passes that rebuild a binding's expression use this, so
    /// the annotations demand analysis leaves survive them.
    pub fn with_expr<U: Clone>(&self, expr: U) -> CoreBinding<U> {
        CoreBinding {
            name: self.name.clone(),
            expr,
            demand: self.demand,
            signature: self.signature.clone(),
        }
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
