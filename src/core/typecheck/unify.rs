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

        // Structural: record types — unify common fields; handle row variables
        // using greedy absorption for gradual semantics.
        (
            Type::Record {
                fields: f1,
                open: open1,
                row: row1,
            },
            Type::Record {
                fields: f2,
                open: open2,
                row: row2,
            },
        ) => {
            let (f1, open1, row1) = (f1.clone(), *open1, row1.clone());
            let (f2, open2, row2) = (f2.clone(), *open2, row2.clone());

            // Unify common fields.
            let common_keys: Vec<String> =
                f1.keys().filter(|k| f2.contains_key(*k)).cloned().collect();
            for k in &common_keys {
                let v1 = apply_subst(f1.get(k).unwrap(), subst);
                let v2 = apply_subst(f2.get(k).unwrap(), subst);
                unify(&v1, &v2, subst)?;
            }

            // Missing-field check: if one side requires a field the other side's
            // known fields don't include, fail unless the other side has a named
            // row variable that can absorb the difference.
            //
            // We intentionally do NOT exempt anonymously-open records (`{..}`)
            // from this check: even though an open record *might* have the field
            // at runtime, the synthesised type only lists what is statically
            // known, and we warn when a required field is absent from that set.
            // This is the gradual-typing decision: prefer actionable warnings over
            // silently allowing unverifiable constraints.
            let f1_only: Vec<&String> = f1.keys().filter(|k| !f2.contains_key(*k)).collect();
            if !f1_only.is_empty() && row2.is_none() {
                // f2 has no row var and is missing fields that f1 requires — fail.
                return Err(UnifyError::Mismatch(
                    Type::Record {
                        fields: f1.clone(),
                        open: open1,
                        row: row1.clone(),
                    },
                    Type::Record {
                        fields: f2.clone(),
                        open: open2,
                        row: row2.clone(),
                    },
                ));
            }
            let f2_only: Vec<&String> = f2.keys().filter(|k| !f1.contains_key(*k)).collect();
            if !f2_only.is_empty() && !open1 && row1.is_none() {
                // f1 is closed and has no row var — it cannot absorb f2's extra fields.
                return Err(UnifyError::Mismatch(
                    Type::Record {
                        fields: f1.clone(),
                        open: open1,
                        row: row1.clone(),
                    },
                    Type::Record {
                        fields: f2.clone(),
                        open: open2,
                        row: row2.clone(),
                    },
                ));
            }

            // Greedy row variable absorption:
            //   If lhs has a row var, bind it to a record of the rhs's extra fields.
            //   If rhs has a row var, bind it to a record of the lhs's extra fields.
            //   Extra = fields present in one side but not the other.
            if let Some(r1) = &row1 {
                // Only absorb if r1 is not already bound.
                let r1_resolved = apply_subst(&Type::Var(r1.clone()), subst);
                if matches!(r1_resolved, Type::Var(_)) {
                    // Collect fields in f2 that are not in f1.
                    let extra: std::collections::BTreeMap<String, Type> = f2
                        .iter()
                        .filter(|(k, _)| !f1.contains_key(*k))
                        .map(|(k, v)| (k.clone(), apply_subst(v, subst)))
                        .collect();
                    let extra_ty = Type::Record {
                        fields: extra,
                        open: open2,
                        row: row2.clone(),
                    };
                    if !occurs(r1, &extra_ty) {
                        subst.insert(r1.clone(), extra_ty);
                    }
                }
            }
            if let Some(r2) = &row2 {
                let r2_resolved = apply_subst(&Type::Var(r2.clone()), subst);
                if matches!(r2_resolved, Type::Var(_)) {
                    let extra: std::collections::BTreeMap<String, Type> = f1
                        .iter()
                        .filter(|(k, _)| !f2.contains_key(*k))
                        .map(|(k, v)| (k.clone(), apply_subst(v, subst)))
                        .collect();
                    let extra_ty = Type::Record {
                        fields: extra,
                        open: open1,
                        row: row1.clone(),
                    };
                    if !occurs(r2, &extra_ty) {
                        subst.insert(r2.clone(), extra_ty);
                    }
                }
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
        Type::Record { fields, open, row } => {
            // Apply substitution to each field type.
            let new_fields: std::collections::BTreeMap<String, Type> = fields
                .iter()
                .map(|(k, v)| (k.clone(), apply_subst(v, subst)))
                .collect();

            // If there is a row variable, look it up in the substitution.
            if let Some(r) = row {
                if let Some(bound) = subst.get(r).cloned() {
                    let bound = apply_subst(&bound, subst);
                    // Merge the bound record's fields into our fields.
                    match bound {
                        Type::Record {
                            fields: extra_fields,
                            open: extra_open,
                            row: extra_row,
                        } => {
                            let mut merged = new_fields;
                            for (k, v) in extra_fields {
                                // Extra fields do not override existing fields.
                                merged.entry(k).or_insert(v);
                            }
                            // Openness after expanding the row var is determined
                            // solely by the bound value: the original `open: true`
                            // was because of the row var itself, which is now gone.
                            Type::Record {
                                fields: merged,
                                open: extra_open,
                                row: extra_row,
                            }
                        }
                        // Row bound to a type variable — this is a rename (e.g.
                        // from a freshen pass: r → _t0).  Preserve the row as a
                        // named row variable with the new name.
                        Type::Var(new_id) => Type::Record {
                            fields: new_fields,
                            open: *open,
                            row: Some(new_id),
                        },
                        // Row bound to any — make open with no named row.
                        Type::Any => Type::Record {
                            fields: new_fields,
                            open: true,
                            row: None,
                        },
                        // Other types — ignore row binding, keep open.
                        _ => Type::Record {
                            fields: new_fields,
                            open: true,
                            row: None,
                        },
                    }
                } else {
                    // Row variable not yet bound — apply subst to it too
                    // (it may have been renamed by a freshen pass).
                    let new_row = match apply_subst(&Type::Var(r.clone()), subst) {
                        Type::Var(new_r) => Some(new_r),
                        _ => None, // resolved to a non-var; drop the row name
                    };
                    Type::Record {
                        fields: new_fields,
                        open: *open,
                        row: new_row,
                    }
                }
            } else {
                Type::Record {
                    fields: new_fields,
                    open: *open,
                    row: None,
                }
            }
        }
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

    // For regular type variables, map Var(id) → Var(_tN).
    // For row variables (which appear as row fields in Records), we also map
    // them via fresh TypeVarId values stored in the same rename substitution
    // — the apply_subst function handles row variable expansion correctly.
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
        Type::Record { fields, row, .. } => {
            for v in fields.values() {
                collect_free_vars(v, vars, seen);
            }
            // Row variables are free variables too.
            if let Some(r) = row {
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
        Type::Record { fields, row, .. } => {
            fields.values().any(|v| occurs(id, v)) || row.as_ref().is_some_and(|r| r == id)
        }
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

    // ── Row variable greedy absorption ───────────────────────────────────────

    fn rec(fields: &[(&str, Type)], open: bool, row: Option<&str>) -> Type {
        Type::Record {
            fields: fields
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone()))
                .collect(),
            open,
            row: row.map(|r| TypeVarId(r.to_string())),
        }
    }

    #[test]
    fn row_var_absorbs_extra_fields() {
        // {x: number, ..r} unified with {x: number, y: string}
        // should bind r → {y: string}
        let lhs = rec(&[("x", Type::Number)], true, Some("r"));
        let rhs = rec(&[("x", Type::Number), ("y", Type::String)], false, None);
        let mut s = Substitution::new();
        assert!(unify(&lhs, &rhs, &mut s).is_ok());
        // r is bound to a record containing only {y: string}
        let r_bound = s.get(&vid("r")).expect("r should be bound");
        assert_eq!(r_bound, &rec(&[("y", Type::String)], false, None));
    }

    #[test]
    fn row_var_absorbs_empty_when_no_extra_fields() {
        // {x: number, ..r} unified with {x: number} (closed)
        // should bind r → {} (empty, closed)
        let lhs = rec(&[("x", Type::Number)], true, Some("r"));
        let rhs = rec(&[("x", Type::Number)], false, None);
        let mut s = Substitution::new();
        assert!(unify(&lhs, &rhs, &mut s).is_ok());
        let r_bound = s.get(&vid("r")).expect("r should be bound");
        assert_eq!(r_bound, &rec(&[], false, None));
    }

    #[test]
    fn apply_subst_expands_row_var() {
        // Given r → {y: string}, {x: number, ..r} should expand to
        // {x: number, y: string}
        let mut s = Substitution::new();
        s.insert(vid("r"), rec(&[("y", Type::String)], false, None));
        let ty = rec(&[("x", Type::Number)], true, Some("r"));
        let result = apply_subst(&ty, &s);
        let expected = rec(&[("x", Type::Number), ("y", Type::String)], false, None);
        assert_eq!(result, expected);
    }

    #[test]
    fn occurs_includes_row_var() {
        // occurs check should detect the row variable
        let ty = rec(&[("x", Type::Number)], true, Some("r"));
        assert!(occurs(&vid("r"), &ty));
        assert!(!occurs(&vid("q"), &ty));
    }
}
