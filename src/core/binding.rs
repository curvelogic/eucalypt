//! Custom binding types replacing the `moniker` crate.
//!
//! Provides de Bruijn indexed variable binding with free/bound variable
//! distinction, scopes, and helper operations.

/// A variable reference — either free (by name) or bound (de Bruijn indexed)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Var {
    Free(String),
    Bound(BoundVar),
}

/// A bound variable with de Bruijn indices
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BoundVar {
    /// How many enclosing scopes to skip (0 = innermost)
    pub scope: u32,
    /// Which binding within that scope (0-indexed)
    pub binder: u32,
    /// Original name, preserved for debugging and pretty-printing
    pub name: Option<String>,
}

/// A scope binding pattern `P` over body `B`
#[derive(Debug, Clone, PartialEq, Eq)]
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
