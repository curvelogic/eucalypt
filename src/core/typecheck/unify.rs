//! Type unification for eucalypt's gradual type system.
//!
//! Provides first-order unification with occurs check, substitution application,
//! and polymorphic scheme instantiation (freshening).
//!
//! Used by the bidirectional type checker to handle polymorphic functions such
//! as `map : (a -> b) -> [a] -> [b]`.
//!
//! ## Higher-kinded types (B1)
//!
//! `Con`/`App` decomposition: `unify(App(f1, x1), App(f2, x2))` recurses on
//! both the head and the argument.  This allows `unify(App(Var(m), a),
//! App(Con("List"), number))` to bind `m = Con("List")` and `a = number`.
//!
//! Kind-aware variable binding: when binding `Var(id, kind)` to a type, the
//! type must have the same kind.  A kind mismatch produces a `KindMismatch`
//! error rather than silently binding to an ill-kinded type.
//!
//! `Forall` nodes are instantiated (freshened) before unification.

use std::collections::{HashMap, HashSet};

use crate::core::typecheck::types::{
    kind_of, unfold_mu, Constraint, Kind, Type, TypeScheme, TypeVarId,
};

/// A mapping from type variable identifiers to their concrete types.
pub type Substitution = HashMap<TypeVarId, Type>;

/// Errors that can arise during unification.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnifyError {
    /// The two types cannot be unified (structural mismatch).
    Mismatch(Type, Type),
    /// A type variable occurs in the type being unified with it (infinite type).
    OccursCheck(TypeVarId, Type),
    /// A kind mismatch prevented unification.
    KindMismatch(Kind, Kind),
}

impl std::fmt::Display for UnifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnifyError::Mismatch(t1, t2) => write!(f, "cannot unify {t1} with {t2}"),
            UnifyError::OccursCheck(v, t) => {
                write!(f, "type variable {v} occurs in {t} (infinite type)")
            }
            UnifyError::KindMismatch(k1, k2) => {
                write!(f, "kind mismatch: expected {k1}, got {k2}")
            }
        }
    }
}

/// Attempt to unify `t1` with `t2`, updating `subst` in place.
pub fn unify(t1: &Type, t2: &Type, subst: &mut Substitution) -> Result<(), UnifyError> {
    let t1 = apply_subst(t1, subst);
    let t2 = apply_subst(t2, subst);

    match (&t1, &t2) {
        // Gradual boundary: `any` unifies with everything.
        (Type::Any, _) | (_, Type::Any) => Ok(()),

        // Identical types.
        (t1, t2) if t1 == t2 => Ok(()),

        // Type variable on the left — bind it (kind-aware).
        (Type::Var(id, kind), rhs) => {
            let id = id.clone();
            let kind = kind.clone();
            let rhs = rhs.clone();
            bind_var(id, kind, rhs, subst)
        }

        // Type variable on the right — bind it (kind-aware).
        (lhs, Type::Var(id, kind)) => {
            let id = id.clone();
            let kind = kind.clone();
            let lhs = lhs.clone();
            bind_var(id, kind, lhs, subst)
        }

        // Literal ↔ base widening.
        (Type::LiteralSymbol(_), Type::Symbol) | (Type::Symbol, Type::LiteralSymbol(_)) => Ok(()),
        (Type::LiteralString(_), Type::String) | (Type::String, Type::LiteralString(_)) => Ok(()),

        // Constructor name — must match exactly (handled by `t1 == t2` above for same name;
        // different names are a mismatch).
        (Type::Con(a), Type::Con(b)) if a != b => Err(UnifyError::Mismatch(t1.clone(), t2.clone())),

        // Constructor application — decompose.
        //
        // Special case: `NonEmpty` ↔ `List` widening.  `NonEmpty` refines `List`
        // (i.e. `NonEmpty(a) <: List(a)`), so unification between the two
        // succeeds by unifying their element types.  This mirrors the rule in
        // the old dedicated-variant unifier and is needed to avoid spurious
        // warnings when a non-empty result is passed where a list is expected
        // (or vice-versa) in polymorphic contexts.
        (Type::App(_, _), Type::App(_, _))
            if matches!(
                (t1.as_applied_single(), t2.as_applied_single()),
                (Some(("NonEmpty", _)), Some(("List", _)))
                    | (Some(("List", _)), Some(("NonEmpty", _)))
            ) =>
        {
            let e1 = t1.as_applied_single().unwrap().1.clone();
            let e2 = t2.as_applied_single().unwrap().1.clone();
            unify(&e1, &e2, subst)
        }

        // Constructor application — generic decomposition.
        (Type::App(f1, x1), Type::App(f2, x2)) => {
            let (f1, x1, f2, x2) = (f1.clone(), x1.clone(), f2.clone(), x2.clone());
            unify(&f1, &f2, subst)?;
            let x1 = apply_subst(&x1, subst);
            let x2 = apply_subst(&x2, subst);
            unify(&x1, &x2, subst)
        }

        // Forall: instantiate (freshen) both and unify the bodies.
        (Type::Forall(binders, body), other) => {
            let freshened = freshen_forall(binders, body, &mut 0);
            unify(&freshened, other, subst)
        }
        (other, Type::Forall(binders, body)) => {
            let freshened = freshen_forall(binders, body, &mut 0);
            unify(other, &freshened, subst)
        }

        // Structural: function types.
        (Type::Function(a1, b1), Type::Function(a2, b2)) => {
            let (a1, b1, a2, b2) = (a1.clone(), b1.clone(), a2.clone(), b2.clone());
            unify(&a1, &a2, subst)?;
            let b1 = apply_subst(&b1, subst);
            let b2 = apply_subst(&b2, subst);
            unify(&b1, &b2, subst)
        }

        // Structural: tuple types.
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

        // Structural: record types.
        (
            Type::Record {
                fields: f1,
                open: open1,
                rows: rows1,
            },
            Type::Record {
                fields: f2,
                open: open2,
                rows: rows2,
            },
        ) => {
            let (f1, open1, rows1) = (f1.clone(), *open1, rows1.clone());
            let (f2, open2, rows2) = (f2.clone(), *open2, rows2.clone());

            // Unify common fields.
            let common_keys: Vec<String> =
                f1.keys().filter(|k| f2.contains_key(*k)).cloned().collect();
            for k in &common_keys {
                let v1 = apply_subst(f1.get(k).unwrap(), subst);
                let v2 = apply_subst(f2.get(k).unwrap(), subst);
                unify(&v1, &v2, subst)?;
            }

            let f1_only: Vec<&String> = f1.keys().filter(|k| !f2.contains_key(*k)).collect();
            if !f1_only.is_empty() && rows2.is_empty() {
                return Err(UnifyError::Mismatch(
                    Type::Record {
                        fields: f1.clone(),
                        open: open1,
                        rows: rows1.clone(),
                    },
                    Type::Record {
                        fields: f2.clone(),
                        open: open2,
                        rows: rows2.clone(),
                    },
                ));
            }
            let f2_only: Vec<&String> = f2.keys().filter(|k| !f1.contains_key(*k)).collect();
            if !f2_only.is_empty() && !open1 && rows1.is_empty() {
                return Err(UnifyError::Mismatch(
                    Type::Record {
                        fields: f1.clone(),
                        open: open1,
                        rows: rows1.clone(),
                    },
                    Type::Record {
                        fields: f2.clone(),
                        open: open2,
                        rows: rows2.clone(),
                    },
                ));
            }

            // Greedy row variable absorption.
            for r1 in &rows1 {
                let r1_resolved = apply_subst(&Type::var(r1.clone()), subst);
                if matches!(r1_resolved, Type::Var(_, _)) {
                    let extra: std::collections::BTreeMap<String, Type> = f2
                        .iter()
                        .filter(|(k, _)| !f1.contains_key(*k))
                        .map(|(k, v)| (k.clone(), apply_subst(v, subst)))
                        .collect();
                    let extra_ty = Type::Record {
                        fields: extra,
                        open: !rows2.is_empty(),
                        rows: rows2.clone(),
                    };
                    if !occurs(r1, &extra_ty) {
                        subst.insert(r1.clone(), extra_ty);
                    }
                }
            }
            for r2 in &rows2 {
                let r2_resolved = apply_subst(&Type::var(r2.clone()), subst);
                if matches!(r2_resolved, Type::Var(_, _)) {
                    let extra: std::collections::BTreeMap<String, Type> = f1
                        .iter()
                        .filter(|(k, _)| !f2.contains_key(*k))
                        .map(|(k, v)| (k.clone(), apply_subst(v, subst)))
                        .collect();
                    let extra_ty = Type::Record {
                        fields: extra,
                        open: !rows1.is_empty(),
                        rows: rows1.clone(),
                    };
                    if !occurs(r2, &extra_ty) {
                        subst.insert(r2.clone(), extra_ty);
                    }
                }
            }

            Ok(())
        }

        // Dict unified with a closed record: bind the dict's value type to the
        // union of the record's field types.
        (
            Type::App(dict_head, t),
            Type::Record {
                fields,
                open: false,
                rows,
            },
        ) if matches!(dict_head.as_ref(), Type::Con(n) if n == "Dict") && rows.is_empty() => {
            let t = t.clone();
            let fields = fields.clone();
            let value_union = Type::union(fields.values().cloned());
            unify(&t, &value_union, subst)
        }
        (
            Type::Record {
                fields,
                open: false,
                rows,
            },
            Type::App(dict_head, t),
        ) if matches!(dict_head.as_ref(), Type::Con(n) if n == "Dict") && rows.is_empty() => {
            let t = t.clone();
            let fields = fields.clone();
            let value_union = Type::union(fields.values().cloned());
            unify(&value_union, &t, subst)
        }
        // Dict unified with an open/row-variable record: bind to `any`.
        (Type::App(dict_head, t), Type::Record { .. }) if matches!(dict_head.as_ref(), Type::Con(n) if n == "Dict") =>
        {
            let t = t.clone();
            unify(&t, &Type::Any, subst)
        }
        (Type::Record { .. }, Type::App(dict_head, t)) if matches!(dict_head.as_ref(), Type::Con(n) if n == "Dict") =>
        {
            let t = t.clone();
            unify(&Type::Any, &t, subst)
        }

        // Recursive type (Mu): unfold once and unify.
        (Type::Mu(x, body), other) | (other, Type::Mu(x, body)) => {
            let (x, body) = (x.clone(), body.clone());
            let mu = Type::Mu(x.clone(), body.clone());
            let unfolded = unfold_mu(&x, &body, &mu);
            let other = other.clone();
            unify(&unfolded, &other, subst)
        }

        // Everything else is a structural mismatch.
        (lhs, rhs) => Err(UnifyError::Mismatch(lhs.clone(), rhs.clone())),
    }
}

/// Bind `id` (with expected kind `kind`) to `rhs`, after an occurs check and
/// kind check.
fn bind_var(
    id: TypeVarId,
    kind: Kind,
    rhs: Type,
    subst: &mut Substitution,
) -> Result<(), UnifyError> {
    if occurs(&id, &rhs) {
        return Err(UnifyError::OccursCheck(id, rhs));
    }
    // Kind check: the rhs must have the same kind as the variable.
    let rhs_kind = kind_of(&rhs);
    if rhs_kind != kind {
        return Err(UnifyError::KindMismatch(kind, rhs_kind));
    }
    subst.insert(id, rhs);
    Ok(())
}

/// Apply a substitution to a type, replacing all bound type variables.
pub fn apply_subst(ty: &Type, subst: &Substitution) -> Type {
    if subst.is_empty() {
        return ty.clone();
    }
    match ty {
        Type::Var(id, _) => {
            if let Some(replacement) = subst.get(id) {
                apply_subst(replacement, subst)
            } else {
                ty.clone()
            }
        }
        Type::App(f, inner) => Type::App(
            Box::new(apply_subst(f, subst)),
            Box::new(apply_subst(inner, subst)),
        ),
        Type::Con(_) => ty.clone(),
        Type::Tuple(elems) => Type::Tuple(elems.iter().map(|e| apply_subst(e, subst)).collect()),
        Type::Function(a, b) => Type::Function(
            Box::new(apply_subst(a, subst)),
            Box::new(apply_subst(b, subst)),
        ),
        Type::Record { fields, open, rows } => {
            let mut merged_fields: std::collections::BTreeMap<String, Type> = fields
                .iter()
                .map(|(k, v)| (k.clone(), apply_subst(v, subst)))
                .collect();

            let mut new_rows: Vec<TypeVarId> = Vec::new();
            let mut result_open = *open;
            let mut result_rows_extra: Vec<TypeVarId> = Vec::new();

            for r in rows {
                if let Some(bound) = subst.get(r).cloned() {
                    let bound = apply_subst(&bound, subst);
                    match bound {
                        Type::Record {
                            fields: extra_fields,
                            open: extra_open,
                            rows: extra_rows,
                        } => {
                            for (k, v) in extra_fields {
                                merged_fields.entry(k).or_insert(v);
                            }
                            if extra_open {
                                result_open = true;
                            }
                            result_rows_extra.extend(extra_rows);
                        }
                        Type::Var(new_id, _) => {
                            new_rows.push(new_id);
                        }
                        Type::Any => {
                            result_open = true;
                        }
                        _ => {
                            result_open = true;
                        }
                    }
                } else {
                    match apply_subst(&Type::var(r.clone()), subst) {
                        Type::Var(new_r, _) => new_rows.push(new_r),
                        _ => result_open = true,
                    }
                }
            }

            new_rows.extend(result_rows_extra);

            Type::Record {
                fields: merged_fields,
                open: result_open,
                rows: new_rows,
            }
        }
        Type::Union(variants) => {
            Type::Union(variants.iter().map(|v| apply_subst(v, subst)).collect())
        }
        Type::Mu(x, body) => {
            let mut inner_subst = subst.clone();
            inner_subst.remove(x);
            Type::Mu(x.clone(), Box::new(apply_subst(body, &inner_subst)))
        }
        Type::Forall(binders, body) => {
            // Binders shadow the outer subst for their own ids.
            let mut inner_subst = subst.clone();
            for (id, _) in binders {
                inner_subst.remove(id);
            }
            Type::Forall(binders.clone(), Box::new(apply_subst(body, &inner_subst)))
        }
        other => other.clone(),
    }
}

/// Instantiate a polymorphic type scheme by replacing each quantified variable
/// with a fresh type variable.
pub fn freshen(scheme: &TypeScheme, counter: &mut u32) -> Type {
    if scheme.vars.is_empty() {
        return scheme.body.clone();
    }

    let mut rename: Substitution = HashMap::new();
    for var in &scheme.vars {
        let fresh = TypeVarId(format!("_t{}", *counter));
        *counter += 1;
        rename.insert(var.clone(), Type::var(fresh));
    }

    apply_subst(&scheme.body, &rename)
}

/// Instantiate a polymorphic scheme, renaming type variables to fresh names
/// throughout both the body type **and** any constraints.
///
/// The same rename map is applied to both, so that constraint variables
/// (`a` in `<(a, a) => a -> a -> a`) refer to the same fresh variables as
/// the body parameters.  This is the entry point for schemes that carry
/// operator constraints (B2).
pub fn freshen_with_constraints(scheme: &TypeScheme, counter: &mut u32) -> (Type, Vec<Constraint>) {
    if scheme.vars.is_empty() {
        let body = scheme.body.clone();
        let constraints = scheme.constraints.clone();
        return (body, constraints);
    }

    let mut rename: Substitution = HashMap::new();
    for var in &scheme.vars {
        let fresh = TypeVarId(format!("_t{}", *counter));
        *counter += 1;
        rename.insert(var.clone(), Type::var(fresh));
    }

    let body = apply_subst(&scheme.body, &rename);
    let constraints = scheme
        .constraints
        .iter()
        .map(|c| Constraint {
            function: c.function.clone(),
            args: c.args.iter().map(|a| apply_subst(a, &rename)).collect(),
        })
        .collect();

    (body, constraints)
}

/// Instantiate a `Forall` node by replacing its binders with fresh variables.
///
/// The fresh variables inherit the kind from the binder.  `counter` is used to
/// generate unique names.
pub fn freshen_forall(binders: &[(TypeVarId, Kind)], body: &Type, counter: &mut u32) -> Type {
    let mut rename: Substitution = HashMap::new();
    for (id, kind) in binders {
        let fresh = TypeVarId(format!("_t{counter}"));
        *counter += 1;
        rename.insert(id.clone(), Type::hk_var(fresh, kind.clone()));
    }
    apply_subst(body, &rename)
}

/// Lift a `Type` into a `TypeScheme` by collecting all free type variables.
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
        Type::Var(id, _) if !seen.contains(id) => {
            seen.insert(id.clone());
            vars.push(id.clone());
        }
        Type::Var(_, _) => {}
        Type::App(f, inner) => {
            collect_free_vars(f, vars, seen);
            collect_free_vars(inner, vars, seen);
        }
        Type::Con(_) => {}
        Type::Function(a, b) => {
            collect_free_vars(a, vars, seen);
            collect_free_vars(b, vars, seen);
        }
        Type::Tuple(elems) => {
            for e in elems {
                collect_free_vars(e, vars, seen);
            }
        }
        Type::Record { fields, rows, .. } => {
            for v in fields.values() {
                collect_free_vars(v, vars, seen);
            }
            for r in rows {
                if !seen.contains(r) {
                    seen.insert(r.clone());
                    vars.push(r.clone());
                }
            }
        }
        Type::Union(variants) => {
            for v in variants {
                collect_free_vars(v, vars, seen);
            }
        }
        Type::Mu(x, body) => {
            seen.insert(x.clone());
            collect_free_vars(body, vars, seen);
        }
        Type::Forall(binders, body) => {
            // Binder vars are bound, not free.
            let mut inner_seen = seen.clone();
            for (id, _) in binders {
                inner_seen.insert(id.clone());
            }
            collect_free_vars(body, vars, &mut inner_seen);
        }
        _ => {}
    }
}

/// Returns `true` if `id` appears free in `ty` (for the occurs check).
fn occurs(id: &TypeVarId, ty: &Type) -> bool {
    match ty {
        Type::Var(other, _) => other == id,
        Type::App(f, inner) => occurs(id, f) || occurs(id, inner),
        Type::Con(_) => false,
        Type::Function(a, b) => occurs(id, a) || occurs(id, b),
        Type::Tuple(elems) => elems.iter().any(|e| occurs(id, e)),
        Type::Record { fields, rows, .. } => {
            fields.values().any(|v| occurs(id, v)) || rows.iter().any(|r| r == id)
        }
        Type::Union(variants) => variants.iter().any(|v| occurs(id, v)),
        Type::Mu(x, body) => x != id && occurs(id, body),
        Type::Forall(binders, body) => {
            // id is shadowed if it appears as a binder.
            if binders.iter().any(|(b, _)| b == id) {
                false
            } else {
                occurs(id, body)
            }
        }
        _ => false,
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::typecheck::types::Kind;

    fn var(name: &str) -> Type {
        Type::var(TypeVarId(name.to_string()))
    }

    fn vid(name: &str) -> TypeVarId {
        TypeVarId(name.to_string())
    }

    fn hk_var(name: &str) -> Type {
        Type::hk_var(TypeVarId(name.to_string()), Kind::star_to_star())
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
        let infinite = Type::list(var("a"));
        let err = unify(&var("a"), &infinite, &mut s).unwrap_err();
        assert!(matches!(err, UnifyError::OccursCheck(_, _)));
    }

    #[test]
    fn unify_list_types() {
        let mut s = Substitution::new();
        let l1 = Type::list(var("a"));
        let l2 = Type::list(Type::Number);
        assert!(unify(&l1, &l2, &mut s).is_ok());
        assert_eq!(s.get(&vid("a")), Some(&Type::Number));
    }

    #[test]
    fn unify_literal_symbol_with_symbol() {
        let mut s = Substitution::new();
        let ls = Type::LiteralSymbol("active".to_string());
        assert!(unify(&ls, &Type::Symbol, &mut s).is_ok());
        assert!(s.is_empty());
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
        s.insert(vid("a"), Type::var(TypeVarId("b".to_string())));
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
                Box::new(Type::var(TypeVarId("_t0".to_string()))),
                Box::new(Type::var(TypeVarId("_t1".to_string()))),
            )
        );
    }

    #[test]
    fn freshen_twice_produces_distinct_vars() {
        let scheme = TypeScheme::poly(vec![vid("a")], Type::list(var("a")));
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
        let ty = Type::Function(
            Box::new(Type::Function(Box::new(var("a")), Box::new(var("b")))),
            Box::new(Type::Function(
                Box::new(Type::list(var("a"))),
                Box::new(Type::list(var("b"))),
            )),
        );
        let scheme = infer_scheme(ty);
        assert_eq!(scheme.vars, vec![vid("a"), vid("b")]);
    }

    #[test]
    fn infer_scheme_vars_in_order_of_first_appearance() {
        let ty = Type::Function(Box::new(var("b")), Box::new(var("a")));
        let scheme = infer_scheme(ty);
        assert_eq!(scheme.vars, vec![vid("b"), vid("a")]);
    }

    // ── Row variable greedy absorption ───────────────────────────────────────

    fn rec(fields: &[(&str, Type)], open: bool, rows: &[&str]) -> Type {
        Type::Record {
            fields: fields
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone()))
                .collect(),
            open,
            rows: rows.iter().map(|r| TypeVarId(r.to_string())).collect(),
        }
    }

    #[test]
    fn row_var_absorbs_extra_fields() {
        let lhs = rec(&[("x", Type::Number)], false, &["r"]);
        let rhs = rec(&[("x", Type::Number), ("y", Type::String)], false, &[]);
        let mut s = Substitution::new();
        assert!(unify(&lhs, &rhs, &mut s).is_ok());
        let r_bound = s.get(&vid("r")).expect("r should be bound");
        assert_eq!(r_bound, &rec(&[("y", Type::String)], false, &[]));
    }

    #[test]
    fn row_var_absorbs_empty_when_no_extra_fields() {
        let lhs = rec(&[("x", Type::Number)], false, &["r"]);
        let rhs = rec(&[("x", Type::Number)], false, &[]);
        let mut s = Substitution::new();
        assert!(unify(&lhs, &rhs, &mut s).is_ok());
        let r_bound = s.get(&vid("r")).expect("r should be bound");
        assert_eq!(r_bound, &rec(&[], false, &[]));
    }

    #[test]
    fn apply_subst_expands_row_var() {
        let mut s = Substitution::new();
        s.insert(vid("r"), rec(&[("y", Type::String)], false, &[]));
        let ty = rec(&[("x", Type::Number)], false, &["r"]);
        let result = apply_subst(&ty, &s);
        let expected = rec(&[("x", Type::Number), ("y", Type::String)], false, &[]);
        assert_eq!(result, expected);
    }

    #[test]
    fn occurs_includes_row_var() {
        let ty = rec(&[("x", Type::Number)], false, &["r"]);
        assert!(occurs(&vid("r"), &ty));
        assert!(!occurs(&vid("q"), &ty));
    }

    #[test]
    fn dict_unifies_with_dict() {
        let mut s = Substitution::new();
        assert!(unify(&Type::dict(Type::Number), &Type::dict(Type::Number), &mut s).is_ok());
    }

    #[test]
    fn dict_unifies_value_types() {
        let mut s = Substitution::new();
        let lhs = Type::dict(var("a"));
        let rhs = Type::dict(Type::Number);
        assert!(unify(&lhs, &rhs, &mut s).is_ok());
        assert_eq!(s.get(&vid("a")), Some(&Type::Number));
    }

    #[test]
    fn closed_record_unifies_with_dict() {
        let mut s = Substitution::new();
        let record = rec(&[("x", Type::Number), ("y", Type::Number)], false, &[]);
        let dict = Type::dict(Type::Number);
        assert!(unify(&record, &dict, &mut s).is_ok());
    }

    #[test]
    fn closed_record_unifies_with_dict_binds_var() {
        let mut s = Substitution::new();
        let record = rec(&[("x", Type::Number)], false, &[]);
        let dict = Type::dict(var("a"));
        assert!(unify(&record, &dict, &mut s).is_ok());
        assert_eq!(s.get(&vid("a")), Some(&Type::Number));
    }

    #[test]
    fn two_row_vars_absorb_fields() {
        let lhs = rec(&[("x", Type::Number)], false, &["r", "s"]);
        let rhs = rec(
            &[("x", Type::Number), ("y", Type::String), ("z", Type::Bool)],
            false,
            &[],
        );
        let mut s = Substitution::new();
        assert!(unify(&lhs, &rhs, &mut s).is_ok());
        let mut absorbed_fields: std::collections::BTreeMap<String, Type> = Default::default();
        if let Some(Type::Record { fields, .. }) = s.get(&vid("r")) {
            absorbed_fields.extend(fields.clone());
        }
        if let Some(Type::Record { fields, .. }) = s.get(&vid("s")) {
            absorbed_fields.extend(fields.clone());
        }
        assert!(absorbed_fields.contains_key("y"));
        assert!(absorbed_fields.contains_key("z"));
    }

    // ── Higher-kinded type unification (B1) ───────────────────────────────────

    #[test]
    fn hk_var_binds_to_con() {
        // Var(m, * → *) unified with Con("List") should bind m = Con("List")
        let mut s = Substitution::new();
        let m = hk_var("m");
        let list_con = Type::Con("List".into());
        assert!(unify(&m, &list_con, &mut s).is_ok());
        assert_eq!(s.get(&vid("m")), Some(&list_con));
    }

    #[test]
    fn app_decomposes() {
        // App(Var(m), a) unified with App(Con("List"), number)
        // → m = Con("List"), a = number
        let mut s = Substitution::new();
        let lhs = Type::App(Box::new(hk_var("m")), Box::new(var("a")));
        let rhs = Type::list(Type::Number);
        assert!(unify(&lhs, &rhs, &mut s).is_ok());
        assert_eq!(s.get(&vid("m")), Some(&Type::Con("List".into())));
        assert_eq!(s.get(&vid("a")), Some(&Type::Number));
    }

    #[test]
    fn kind_mismatch_fails() {
        // Trying to bind Var(m, * → *) to Type::Number (kind *) should fail
        let mut s = Substitution::new();
        let m = hk_var("m");
        let err = unify(&m, &Type::Number, &mut s).unwrap_err();
        assert!(matches!(err, UnifyError::KindMismatch(_, _)));
    }

    #[test]
    fn star_var_cannot_bind_to_constructor() {
        // Var(a, *) unified with Con("List") (kind * → *) should fail
        let mut s = Substitution::new();
        let a = var("a"); // kind *
        let list_con = Type::Con("List".into()); // kind * → *
        let err = unify(&a, &list_con, &mut s).unwrap_err();
        assert!(matches!(err, UnifyError::KindMismatch(_, _)));
    }

    #[test]
    fn forall_instantiates_on_unify() {
        // Forall([(a, *)], [a]) unified with [number] should succeed after instantiation
        let mut s = Substitution::new();
        let forall = Type::Forall(vec![(vid("a"), Kind::Star)], Box::new(Type::list(var("a"))));
        let rhs = Type::list(Type::Number);
        // The forall is instantiated to [_t0], then unified with [number]
        assert!(unify(&forall, &rhs, &mut s).is_ok());
    }
}
