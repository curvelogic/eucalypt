//! Type unification for eucalypt's gradual type system.
//!
//! Provides first-order unification with occurs check, substitution application,
//! and polymorphic scheme instantiation (freshening).
//!
//! Used by the bidirectional type checker (eu-wq59) to handle polymorphic
//! functions such as `map : (a -> b) -> [a] -> [b]`.

use std::collections::{HashMap, HashSet};

use crate::core::typecheck::types::{Type, TypeScheme, TypeVarId};

/// A mapping from type variable identifiers to their concrete types.
///
/// Built incrementally as arguments are checked against polymorphic function
/// parameter types.
pub type Substitution = HashMap<TypeVarId, Type>;

/// Errors that can arise during unification.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnifyError {
    /// The two types cannot be unified (structural mismatch).
    Mismatch(Type, Type),
    /// A type variable occurs in the type being unified with it (infinite type).
    OccursCheck(TypeVarId, Type),
}

impl std::fmt::Display for UnifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnifyError::Mismatch(t1, t2) => write!(f, "cannot unify {t1} with {t2}"),
            UnifyError::OccursCheck(v, t) => {
                write!(f, "type variable {v} occurs in {t} (infinite type)")
            }
        }
    }
}

/// Attempt to unify `t1` with `t2`, updating `subst` in place.
///
/// At the gradual boundary, `any` unifies with everything (no binding
/// produced).  For type variables, a binding is added to `subst` after an
/// occurs check.
///
/// # Errors
///
/// Returns `UnifyError::Mismatch` when the two types are structurally
/// incompatible, and `UnifyError::OccursCheck` when a type variable would be
/// bound to a type that contains itself.
pub fn unify(t1: &Type, t2: &Type, subst: &mut Substitution) -> Result<(), UnifyError> {
    let t1 = apply_subst(t1, subst);
    let t2 = apply_subst(t2, subst);

    match (&t1, &t2) {
        // Gradual boundary: `any` unifies with everything.
        (Type::Any, _) | (_, Type::Any) => Ok(()),

        // Identical types — no bindings needed.
        (t1, t2) if t1 == t2 => Ok(()),

        // Type variable on the left — bind it.
        (Type::Var(id), rhs) => {
            let id = id.clone();
            let rhs = rhs.clone();
            if occurs(&id, &rhs) {
                return Err(UnifyError::OccursCheck(id, rhs));
            }
            subst.insert(id, rhs);
            Ok(())
        }

        // Type variable on the right — bind it.
        (lhs, Type::Var(id)) => {
            let id = id.clone();
            let lhs = lhs.clone();
            if occurs(&id, &lhs) {
                return Err(UnifyError::OccursCheck(id, lhs));
            }
            subst.insert(id, lhs);
            Ok(())
        }

        // Literal symbol ↔ Symbol: widen to symbol (succeed, no binding).
        (Type::LiteralSymbol(_), Type::Symbol) | (Type::Symbol, Type::LiteralSymbol(_)) => Ok(()),

        // Structural: function types — contravariant in param, covariant in result.
        (Type::Function(a1, b1), Type::Function(a2, b2)) => {
            let (a1, b1, a2, b2) = (a1.clone(), b1.clone(), a2.clone(), b2.clone());
            unify(&a1, &a2, subst)?;
            let b1 = apply_subst(&b1, subst);
            let b2 = apply_subst(&b2, subst);
            unify(&b1, &b2, subst)
        }

        // Structural: list types — covariant in element type.
        (Type::List(e1), Type::List(e2)) => {
            let (e1, e2) = (e1.clone(), e2.clone());
            unify(&e1, &e2, subst)
        }

        // Structural: tuple types — covariant, same arity required.
        (Type::Tuple(elems1), Type::Tuple(elems2)) if elems1.len() == elems2.len() => {
            let pairs: Vec<_> = elems1
                .iter()
                .zip(elems2.iter())
                .map(|(a, b)| (a.clone(), b.clone()))
                .collect();
            for (a, b) in pairs {
                unify(&a, &b, subst)?;
            }
            Ok(())
        }

        // Structural: IO types.
        (Type::IO(inner1), Type::IO(inner2)) => {
            let (i1, i2) = (inner1.clone(), inner2.clone());
            unify(&i1, &i2, subst)
        }

        // Structural: Lens types.
        (Type::Lens(a1, b1), Type::Lens(a2, b2)) => {
            let (a1, b1, a2, b2) = (a1.clone(), b1.clone(), a2.clone(), b2.clone());
            unify(&a1, &a2, subst)?;
            let b1 = apply_subst(&b1, subst);
            let b2 = apply_subst(&b2, subst);
            unify(&b1, &b2, subst)
        }

        // Structural: Traversal types.
        (Type::Traversal(a1, b1), Type::Traversal(a2, b2)) => {
            let (a1, b1, a2, b2) = (a1.clone(), b1.clone(), a2.clone(), b2.clone());
            unify(&a1, &a2, subst)?;
            let b1 = apply_subst(&b1, subst);
            let b2 = apply_subst(&b2, subst);
            unify(&b1, &b2, subst)
        }

        // Structural: record types — unify the common fields; extra fields are
        // allowed for open records.
        (Type::Record { fields: f1, .. }, Type::Record { fields: f2, .. }) => {
            let pairs: Vec<_> = f1
                .iter()
                .filter_map(|(k, v)| f2.get(k).map(|v2| (v.clone(), v2.clone())))
                .collect();
            for (v1, v2) in pairs {
                unify(&v1, &v2, subst)?;
            }
            Ok(())
        }

        // Everything else is a structural mismatch.
        (lhs, rhs) => Err(UnifyError::Mismatch(lhs.clone(), rhs.clone())),
    }
}

/// Apply a substitution to a type, replacing all bound type variables.
///
/// Application is transitive: if the substitution maps `a → _t0` and
/// `_t0 → number`, then `apply_subst(Var(a), subst)` returns `number`.
pub fn apply_subst(ty: &Type, subst: &Substitution) -> Type {
    if subst.is_empty() {
        return ty.clone();
    }
    match ty {
        Type::Var(id) => {
            if let Some(replacement) = subst.get(id) {
                // Apply transitively — the replacement may itself contain vars.
                apply_subst(replacement, subst)
            } else {
                ty.clone()
            }
        }
        Type::List(inner) => Type::List(Box::new(apply_subst(inner, subst))),
        Type::Tuple(elems) => Type::Tuple(elems.iter().map(|e| apply_subst(e, subst)).collect()),
        Type::IO(inner) => Type::IO(Box::new(apply_subst(inner, subst))),
        Type::Lens(a, b) => Type::Lens(
            Box::new(apply_subst(a, subst)),
            Box::new(apply_subst(b, subst)),
        ),
        Type::Traversal(a, b) => Type::Traversal(
            Box::new(apply_subst(a, subst)),
            Box::new(apply_subst(b, subst)),
        ),
        Type::Function(a, b) => Type::Function(
            Box::new(apply_subst(a, subst)),
            Box::new(apply_subst(b, subst)),
        ),
        Type::Record { fields, open } => Type::Record {
            fields: fields
                .iter()
                .map(|(k, v)| (k.clone(), apply_subst(v, subst)))
                .collect(),
            open: *open,
        },
        Type::Union(variants) => {
            Type::Union(variants.iter().map(|v| apply_subst(v, subst)).collect())
        }
        // All other types contain no variables.
        other => other.clone(),
    }
}

/// Instantiate a polymorphic type scheme by replacing each quantified variable
/// with a fresh type variable.
///
/// Fresh variables use the form `_t{counter}` to distinguish them from
/// user-written variables.  `counter` is incremented once per fresh variable.
///
/// For a monomorphic scheme (no `vars`), the body is returned unchanged.
pub fn freshen(scheme: &TypeScheme, counter: &mut u32) -> Type {
    if scheme.vars.is_empty() {
        return scheme.body.clone();
    }

    let mut rename: Substitution = HashMap::new();
    for var in &scheme.vars {
        let fresh = TypeVarId(format!("_t{}", *counter));
        *counter += 1;
        rename.insert(var.clone(), Type::Var(fresh));
    }

    apply_subst(&scheme.body, &rename)
}

/// Lift a `Type` into a `TypeScheme` by collecting all free type variables.
///
/// All `Type::Var` occurrences become quantified variables in the returned
/// scheme, ordered by first appearance (left-to-right, depth-first).
pub fn infer_scheme(ty: Type) -> TypeScheme {
    let mut vars: Vec<TypeVarId> = Vec::new();
    let mut seen: HashSet<TypeVarId> = HashSet::new();
    collect_free_vars(&ty, &mut vars, &mut seen);
    if vars.is_empty() {
        TypeScheme::mono(ty)
    } else {
        TypeScheme::poly(vars, ty)
    }
}

// ── Internal helpers ─────────────────────────────────────────────────────────

/// Collect all free type variables from `ty` in order of first appearance.
fn collect_free_vars(ty: &Type, vars: &mut Vec<TypeVarId>, seen: &mut HashSet<TypeVarId>) {
    match ty {
        Type::Var(id) if !seen.contains(id) => {
            seen.insert(id.clone());
            vars.push(id.clone());
        }
        Type::Var(_) => {}
        Type::List(inner) | Type::IO(inner) => collect_free_vars(inner, vars, seen),
        Type::Lens(a, b) | Type::Traversal(a, b) | Type::Function(a, b) => {
            collect_free_vars(a, vars, seen);
            collect_free_vars(b, vars, seen);
        }
        Type::Tuple(elems) => {
            for e in elems {
                collect_free_vars(e, vars, seen);
            }
        }
        Type::Record { fields, .. } => {
            for v in fields.values() {
                collect_free_vars(v, vars, seen);
            }
        }
        Type::Union(variants) => {
            for v in variants {
                collect_free_vars(v, vars, seen);
            }
        }
        // Primitives and special types have no variables.
        _ => {}
    }
}

/// Returns `true` if `id` appears free in `ty` (for the occurs check).
fn occurs(id: &TypeVarId, ty: &Type) -> bool {
    match ty {
        Type::Var(other) => other == id,
        Type::List(inner) | Type::IO(inner) => occurs(id, inner),
        Type::Lens(a, b) | Type::Traversal(a, b) | Type::Function(a, b) => {
            occurs(id, a) || occurs(id, b)
        }
        Type::Tuple(elems) => elems.iter().any(|e| occurs(id, e)),
        Type::Record { fields, .. } => fields.values().any(|v| occurs(id, v)),
        Type::Union(variants) => variants.iter().any(|v| occurs(id, v)),
        _ => false,
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn var(name: &str) -> Type {
        Type::Var(TypeVarId(name.to_string()))
    }

    fn vid(name: &str) -> TypeVarId {
        TypeVarId(name.to_string())
    }

    // ── unify ────────────────────────────────────────────────────────────────

    #[test]
    fn unify_identical_concrete_types() {
        let mut s = Substitution::new();
        assert!(unify(&Type::Number, &Type::Number, &mut s).is_ok());
        assert!(s.is_empty());
    }

    #[test]
    fn unify_any_with_anything() {
        let mut s = Substitution::new();
        assert!(unify(&Type::Any, &Type::Number, &mut s).is_ok());
        assert!(unify(&Type::String, &Type::Any, &mut s).is_ok());
    }

    #[test]
    fn unify_var_with_concrete() {
        let mut s = Substitution::new();
        assert!(unify(&var("a"), &Type::Number, &mut s).is_ok());
        assert_eq!(s.get(&vid("a")), Some(&Type::Number));
    }

    #[test]
    fn unify_concrete_with_var() {
        let mut s = Substitution::new();
        assert!(unify(&Type::String, &var("b"), &mut s).is_ok());
        assert_eq!(s.get(&vid("b")), Some(&Type::String));
    }

    #[test]
    fn unify_mismatch() {
        let mut s = Substitution::new();
        let err = unify(&Type::Number, &Type::String, &mut s).unwrap_err();
        assert!(matches!(err, UnifyError::Mismatch(_, _)));
    }

    #[test]
    fn unify_function_types_binds_vars() {
        let mut s = Substitution::new();
        let f1 = Type::Function(Box::new(var("a")), Box::new(var("b")));
        let f2 = Type::Function(Box::new(Type::Number), Box::new(Type::String));
        assert!(unify(&f1, &f2, &mut s).is_ok());
        assert_eq!(s.get(&vid("a")), Some(&Type::Number));
        assert_eq!(s.get(&vid("b")), Some(&Type::String));
    }

    #[test]
    fn occurs_check_prevents_infinite_type() {
        let mut s = Substitution::new();
        let infinite = Type::List(Box::new(var("a")));
        let err = unify(&var("a"), &infinite, &mut s).unwrap_err();
        assert!(matches!(err, UnifyError::OccursCheck(_, _)));
    }

    #[test]
    fn unify_list_types() {
        let mut s = Substitution::new();
        let l1 = Type::List(Box::new(var("a")));
        let l2 = Type::List(Box::new(Type::Number));
        assert!(unify(&l1, &l2, &mut s).is_ok());
        assert_eq!(s.get(&vid("a")), Some(&Type::Number));
    }

    #[test]
    fn unify_literal_symbol_with_symbol() {
        let mut s = Substitution::new();
        let ls = Type::LiteralSymbol("active".to_string());
        assert!(unify(&ls, &Type::Symbol, &mut s).is_ok());
        assert!(s.is_empty()); // no bindings — just widening
    }

    #[test]
    fn unify_symbol_with_literal_symbol() {
        let mut s = Substitution::new();
        let ls = Type::LiteralSymbol("active".to_string());
        assert!(unify(&Type::Symbol, &ls, &mut s).is_ok());
        assert!(s.is_empty());
    }

    #[test]
    fn unify_literal_symbol_same() {
        let mut s = Substitution::new();
        let a = Type::LiteralSymbol("x".to_string());
        let b = Type::LiteralSymbol("x".to_string());
        assert!(unify(&a, &b, &mut s).is_ok());
    }

    #[test]
    fn unify_literal_symbol_different_fails() {
        let mut s = Substitution::new();
        let a = Type::LiteralSymbol("x".to_string());
        let b = Type::LiteralSymbol("y".to_string());
        assert!(unify(&a, &b, &mut s).is_err());
    }

    #[test]
    fn unify_var_with_literal_symbol() {
        let mut s = Substitution::new();
        let ls = Type::LiteralSymbol("active".to_string());
        assert!(unify(&var("a"), &ls, &mut s).is_ok());
        assert_eq!(
            s.get(&vid("a")),
            Some(&Type::LiteralSymbol("active".to_string()))
        );
    }

    // ── apply_subst ──────────────────────────────────────────────────────────

    #[test]
    fn apply_empty_subst_is_identity() {
        let s = Substitution::new();
        let ty = Type::Function(Box::new(var("a")), Box::new(Type::Number));
        assert_eq!(apply_subst(&ty, &s), ty);
    }

    #[test]
    fn apply_subst_replaces_var() {
        let mut s = Substitution::new();
        s.insert(vid("a"), Type::String);
        let ty = Type::Function(Box::new(var("a")), Box::new(Type::Number));
        assert_eq!(
            apply_subst(&ty, &s),
            Type::Function(Box::new(Type::String), Box::new(Type::Number))
        );
    }

    #[test]
    fn apply_subst_transitive() {
        let mut s = Substitution::new();
        s.insert(vid("a"), Type::Var(TypeVarId("b".to_string())));
        s.insert(vid("b"), Type::Number);
        assert_eq!(apply_subst(&var("a"), &s), Type::Number);
    }

    // ── freshen ──────────────────────────────────────────────────────────────

    #[test]
    fn freshen_mono_scheme_returns_body_unchanged() {
        let scheme = TypeScheme::mono(Type::Number);
        let mut counter = 0u32;
        assert_eq!(freshen(&scheme, &mut counter), Type::Number);
        assert_eq!(counter, 0);
    }

    #[test]
    fn freshen_poly_scheme_creates_fresh_vars() {
        // forall a b. a -> b
        let scheme = TypeScheme::poly(
            vec![vid("a"), vid("b")],
            Type::Function(Box::new(var("a")), Box::new(var("b"))),
        );
        let mut counter = 0u32;
        let ty = freshen(&scheme, &mut counter);
        assert_eq!(counter, 2);
        assert_eq!(
            ty,
            Type::Function(
                Box::new(Type::Var(TypeVarId("_t0".to_string()))),
                Box::new(Type::Var(TypeVarId("_t1".to_string()))),
            )
        );
    }

    #[test]
    fn freshen_twice_produces_distinct_vars() {
        let scheme = TypeScheme::poly(vec![vid("a")], Type::List(Box::new(var("a"))));
        let mut counter = 0u32;
        let ty1 = freshen(&scheme, &mut counter);
        let ty2 = freshen(&scheme, &mut counter);
        assert_ne!(ty1, ty2);
    }

    // ── infer_scheme ─────────────────────────────────────────────────────────

    #[test]
    fn infer_scheme_from_mono_type_is_mono() {
        let ty = Type::Function(Box::new(Type::Number), Box::new(Type::Number));
        let scheme = infer_scheme(ty.clone());
        assert!(scheme.vars.is_empty());
        assert_eq!(scheme.body, ty);
    }

    #[test]
    fn infer_scheme_from_poly_type_collects_vars() {
        // (a -> b) -> [a] -> [b]
        let ty = Type::Function(
            Box::new(Type::Function(Box::new(var("a")), Box::new(var("b")))),
            Box::new(Type::Function(
                Box::new(Type::List(Box::new(var("a")))),
                Box::new(Type::List(Box::new(var("b")))),
            )),
        );
        let scheme = infer_scheme(ty);
        assert_eq!(scheme.vars, vec![vid("a"), vid("b")]);
    }

    #[test]
    fn infer_scheme_vars_in_order_of_first_appearance() {
        // b -> a (b appears first)
        let ty = Type::Function(Box::new(var("b")), Box::new(var("a")));
        let scheme = infer_scheme(ty);
        assert_eq!(scheme.vars, vec![vid("b"), vid("a")]);
    }
}
