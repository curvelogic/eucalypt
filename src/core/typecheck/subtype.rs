//! Subtyping and consistency relations for eucalypt's gradual type system.
//!
//! ## Subtyping (`<:`)
//!
//! `is_subtype(S, T)` returns `true` when a value of type `S` can be used
//! wherever a value of type `T` is expected.  The relation is reflexive and
//! transitive.
//!
//! Key rules (from spec section 3):
//!
//! - `never <: T <: top` for all `T`
//! - Primitives are flat — no subtyping between `number`, `string`, etc.
//! - Lists are covariant: `[A] <: [B]` iff `A <: B`
//! - Tuples widen to lists and are covariant in each component
//! - Records: width + depth subtyping (more fields <: fewer fields, covariant values)
//! - Functions: contravariant input, covariant output
//! - Unions: `A <: A | B`; `A | B <: C` iff `A <: C` and `B <: C`
//! - `Lens(A, B) <: Traversal(A, B)`; both covariant in `A` and `B`
//! - `IO(A) <: IO(B)` iff `A <: B`
//! - `set`, `vec`, `array` are opaque — no cross-kind subtyping
//!
//! ## Constructor application (B1)
//!
//! For known built-in constructors (`List`, `IO`, `Dict`, `NonEmpty`) the
//! argument is **covariant**.  `Lens`/`Traversal` follow the same rules as
//! before.  For an abstract head — `App(Var(m), a)` — variance is unknown
//! so **invariance** is required.
//!
//! ## Consistency (`~`)
//!
//! `is_consistent(S, T)` is the gradual-typing relation.  `any` is consistent
//! with every type in both directions.  For non-`any` types the relation
//! falls through to subtyping.

use super::types::{unfold_mu, Type};
use super::unify::beta_reduce;

/// Return `true` if type `s` is a subtype of type `t` (`s <: t`).
///
/// Type variables are treated as `any` (consistent with everything) because
/// at the point of subtyping they have not been instantiated.
pub fn is_subtype(s: &Type, t: &Type) -> bool {
    is_subtype_co(s, t, &mut Vec::new())
}

/// Coinductive subtyping with an assumed-pairs set to handle equirecursive types.
fn is_subtype_co(s: &Type, t: &Type, assumed: &mut Vec<(Type, Type)>) -> bool {
    // Beta-reduce App(Lam(x, body), arg) before comparing.
    let s_red;
    let t_red;
    let s = if matches!(s, Type::App(_, _)) {
        s_red = beta_reduce(s);
        &s_red
    } else {
        s
    };
    let t = if matches!(t, Type::App(_, _)) {
        t_red = beta_reduce(t);
        &t_red
    } else {
        t
    };

    // Reflexivity.
    if s == t {
        return true;
    }

    // Coinductive base.
    if assumed.iter().any(|(as_, at)| as_ == s && at == t) {
        return true;
    }

    match (s, t) {
        // ── Mu (equirecursive) ────────────────────────────────────────────────
        (Type::Mu(x, body), _) => {
            let mu = s.clone();
            assumed.push((s.clone(), t.clone()));
            let unfolded = unfold_mu(x, body, &mu);
            let result = is_subtype_co(&unfolded, t, assumed);
            assumed.pop();
            result
        }
        (_, Type::Mu(x, body)) => {
            let mu = t.clone();
            assumed.push((s.clone(), t.clone()));
            let unfolded = unfold_mu(x, body, &mu);
            let result = is_subtype_co(s, &unfolded, assumed);
            assumed.pop();
            result
        }

        // ── Top and Never ─────────────────────────────────────────────────────
        (Type::Never, _) => true,
        (_, Type::Top) => true,
        (Type::Top, _) => false,
        (_, Type::Never) => false,

        // ── Any (gradual) ────────────────────────────────────────────────────
        (Type::Any, _) | (_, Type::Any) => true,

        // ── Type variables ───────────────────────────────────────────────────
        // Uninstantiated type variables are treated as `any`.
        (Type::Var(_, _), _) | (_, Type::Var(_, _)) => true,

        // ── Literal types ────────────────────────────────────────────────────
        (Type::LiteralSymbol(_), Type::Symbol) => true,
        (Type::LiteralString(_), Type::String) => true,

        // ── Primitives ────────────────────────────────────────────────────────
        (Type::Number, Type::Number) => true,
        (Type::String, Type::String) => true,
        (Type::Symbol, Type::Symbol) => true,
        (Type::Bool, Type::Bool) => true,
        (Type::Null, Type::Null) => true,
        (Type::DateTime, Type::DateTime) => true,

        // ── Opaque collection types ───────────────────────────────────────────
        (Type::Set, Type::Set) => true,
        (Type::Vec, Type::Vec) => true,
        (Type::Array, Type::Array) => true,

        // ── Constructor application (B1) ─────────────────────────────────────
        //
        // Known built-in constructors are covariant.
        // Abstract constructor heads (Var) require invariance.
        // Lens(A, B) <: Traversal(A, B).
        (Type::App(_, _), Type::App(_, _)) => is_app_subtype(s, t, assumed),

        // ── Tuple ─────────────────────────────────────────────────────────────
        // Tuples widen to lists.
        (Type::Tuple(elems), _) if s_is_app_con(t, "List") => {
            let elem_ty = t.as_list().unwrap();
            elems.iter().all(|e| is_subtype_co(e, elem_ty, assumed))
        }
        // Tuple <: NonEmpty([U])
        (Type::Tuple(elems), _) if s_is_app_con(t, "NonEmpty") => {
            let elem_ty = t.as_non_empty().unwrap();
            !elems.is_empty() && elems.iter().all(|e| is_subtype_co(e, elem_ty, assumed))
        }
        // Tuple covariance.
        (Type::Tuple(as_), Type::Tuple(bs)) => {
            as_.len() == bs.len()
                && as_
                    .iter()
                    .zip(bs.iter())
                    .all(|(a, b)| is_subtype_co(a, b, assumed))
        }

        // ── Record — width + depth subtyping ──────────────────────────────────
        (
            Type::Record {
                fields: s_fields,
                open: s_open,
                rows: s_rows,
            },
            Type::Record {
                fields: t_fields,
                open: t_open,
                rows: _,
            },
        ) => {
            let s_is_open = *s_open || !s_rows.is_empty();

            let fields_ok = t_fields
                .iter()
                .all(|(name, t_ty)| match s_fields.get(name) {
                    Some(s_ty) => is_subtype_co(s_ty, t_ty, assumed),
                    None => false,
                });

            if !fields_ok {
                return false;
            }

            if !t_open && s_is_open {
                return false;
            }

            true
        }

        // Dict(A) <: Dict(B) iff A <: B
        (s, t) if s_is_app_con(s, "Dict") && s_is_app_con(t, "Dict") => {
            is_subtype_co(s.as_dict().unwrap(), t.as_dict().unwrap(), assumed)
        }

        // Closed record <: Dict(B) when all field values are <: B.
        (
            Type::Record {
                fields: s_fields,
                open: false,
                rows,
            },
            _,
        ) if rows.is_empty() && s_is_app_con(t, "Dict") => {
            let b = t.as_dict().unwrap();
            s_fields.values().all(|v| is_subtype_co(v, b, assumed))
        }

        // Dict(T) <: {..}
        (
            s,
            Type::Record {
                fields, open: true, ..
            },
        ) if fields.is_empty() && s_is_app_con(s, "Dict") => true,

        // ── Function ─────────────────────────────────────────────────────────
        (Type::Function(a, b), Type::Function(c, d)) => {
            is_subtype_co(c, a, assumed) && is_subtype_co(b, d, assumed)
        }

        // ── Union ─────────────────────────────────────────────────────────────
        (Type::Union(variants), t) => {
            let variants = variants.clone();
            variants.iter().all(|v| is_subtype_co(v, t, assumed))
        }
        (s, Type::Union(variants)) => {
            let variants = variants.clone();
            variants.iter().any(|v| is_subtype_co(s, v, assumed))
        }

        // ── Forall — treat as body for subtyping ──────────────────────────────
        (Type::Forall(_, body_s), t) => is_subtype_co(body_s, t, assumed),
        (s, Type::Forall(_, body_t)) => is_subtype_co(s, body_t, assumed),

        // ── Con — bare constructor (only equal to itself, reflexivity above) ─
        (Type::Con(_), _) | (_, Type::Con(_)) => false,

        // ── Lam — type-level lambda ───────────────────────────────────────────
        // Two lambdas: compare bodies with same parameter (alpha-equivalent check).
        (Type::Lam(x, body_s), Type::Lam(y, body_t)) if x == y => {
            is_subtype_co(body_s, body_t, assumed)
        }
        // Lam in isolation (not under App): treat as consistent with `any`.
        (Type::Lam(_, _), _) | (_, Type::Lam(_, _)) => true,

        _ => false,
    }
}

/// Helper: is `ty` an `App(Con(name), _)`?
fn s_is_app_con(ty: &Type, name: &str) -> bool {
    ty.as_applied_single().is_some_and(|(n, _)| n == name)
}

/// Subtyping for `App` types.
fn is_app_subtype(s: &Type, t: &Type, assumed: &mut Vec<(Type, Type)>) -> bool {
    // Two-arg: Lens/Traversal
    if let (Some((sn, sa, sb)), Some((tn, ta, tb))) = (s.as_applied_two(), t.as_applied_two()) {
        match (sn, tn) {
            // Lens(A, B) <: Lens(A', B') — covariant in both
            ("Lens", "Lens") | ("Traversal", "Traversal") => {
                return is_subtype_co(sa, ta, assumed) && is_subtype_co(sb, tb, assumed);
            }
            // Lens(A, B) <: Traversal(A', B')
            ("Lens", "Traversal") => {
                return is_subtype_co(sa, ta, assumed) && is_subtype_co(sb, tb, assumed);
            }
            _ => {}
        }
    }

    // Single-arg constructors.
    if let (Some((sn, si)), Some((tn, ti))) = (s.as_applied_single(), t.as_applied_single()) {
        if sn == tn {
            return match sn {
                // Covariant built-in constructors.
                "List" | "IO" | "Dict" | "NonEmpty" => is_subtype_co(si, ti, assumed),
                // Unknown constructor — invariant.
                _ => si == ti,
            };
        }
        // NonEmpty([A]) <: [B] iff A <: B
        if sn == "NonEmpty" && tn == "List" {
            return is_subtype_co(si, ti, assumed);
        }
    }

    // App with an abstract (Var) head — require the full App types to be
    // identical (invariant).
    s == t
}

/// Return `true` if types `s` and `t` are consistent (`s ~ t`).
pub fn is_consistent(s: &Type, t: &Type) -> bool {
    // Beta-reduce App(Lam(x, body), arg) before checking.
    let s_red;
    let t_red;
    let s = if matches!(s, Type::App(_, _)) {
        s_red = beta_reduce(s);
        &s_red
    } else {
        s
    };
    let t = if matches!(t, Type::App(_, _)) {
        t_red = beta_reduce(t);
        &t_red
    } else {
        t
    };

    // `any` is consistent with everything.
    if matches!(s, Type::Any) || matches!(t, Type::Any) {
        return true;
    }

    // Literal type consistency.
    match (s, t) {
        (Type::LiteralSymbol(_), Type::Symbol)
        | (Type::Symbol, Type::LiteralSymbol(_))
        | (Type::LiteralString(_), Type::String)
        | (Type::String, Type::LiteralString(_)) => {
            return true;
        }
        _ => {}
    }

    // Structural consistency.
    match (s, t) {
        // ── Mu ─────────────────────────────────────────────────────────────
        (Type::Mu(x, body), _) => {
            let mu = s.clone();
            let unfolded = unfold_mu(x, body, &mu);
            is_consistent(&unfolded, t)
        }
        (_, Type::Mu(x, body)) => {
            let mu = t.clone();
            let unfolded = unfold_mu(x, body, &mu);
            is_consistent(s, &unfolded)
        }

        // ── Constructor application ──────────────────────────────────────────
        //
        // When both sides are Apps and the left head is an unresolved HKT
        // variable — treat as gradual.  This avoids false positives when the
        // constructor variable `m` (from `forall (m :: * -> *). m a → m b`)
        // is freshened independently across call sites and remains unbound
        // at the consistency check.  An abstract `m a` (where `m :: * → *` is
        // an unresolved higher-kinded variable) is treated as gradual: it could
        // be ANY concrete type depending on `m`, so it is consistent with
        // everything.  This is the correct gradual-typing treatment of abstract
        // HKT applications.
        (Type::App(f, _), _) if matches!(&**f, Type::Var(_, _)) => true,
        // Symmetric: right head is an unresolved HKT variable.
        (_, Type::App(f, _)) if matches!(&**f, Type::Var(_, _)) => true,
        (Type::App(_, _), Type::App(_, _)) => is_app_consistent(s, t),

        // ── Tuple ─────────────────────────────────────────────────────────────
        (Type::Tuple(as_), Type::Tuple(bs)) if as_.len() == bs.len() => {
            as_.iter().zip(bs.iter()).all(|(a, b)| is_consistent(a, b))
        }
        (Type::Tuple(elems), _) if s_is_app_con(t, "NonEmpty") => {
            let b = t.as_non_empty().unwrap();
            !elems.is_empty() && elems.iter().all(|e| is_consistent(e, b))
        }
        (_, Type::Tuple(elems)) if s_is_app_con(s, "NonEmpty") => {
            let a = s.as_non_empty().unwrap();
            !elems.is_empty() && elems.iter().all(|e| is_consistent(a, e))
        }

        // ── Function ─────────────────────────────────────────────────────────
        (Type::Function(a, b), Type::Function(c, d)) => is_consistent(c, a) && is_consistent(b, d),

        // ── Record ─────────────────────────────────────────────────────────
        (
            Type::Record {
                fields: s_fields,
                open: s_open,
                rows: s_rows,
            },
            Type::Record {
                fields: t_fields,
                open: t_open,
                rows: _,
            },
        ) => {
            let s_is_open = *s_open || !s_rows.is_empty();
            let shared_ok = t_fields
                .iter()
                .all(|(name, t_ty)| match s_fields.get(name) {
                    Some(s_ty) => is_consistent(s_ty, t_ty),
                    None => s_is_open,
                });
            if !shared_ok {
                return false;
            }
            if !t_open && s_fields.len() > t_fields.len() && !s_is_open {
                return false;
            }
            true
        }

        // Dict consistency.
        (s, t) if s_is_app_con(s, "Dict") && s_is_app_con(t, "Dict") => {
            is_consistent(s.as_dict().unwrap(), t.as_dict().unwrap())
        }
        (Type::Record { fields, .. }, t) if s_is_app_con(t, "Dict") => {
            let b = t.as_dict().unwrap();
            fields.values().all(|v| is_consistent(v, b))
        }
        (s, Type::Record { fields, .. }) if s_is_app_con(s, "Dict") => {
            let a = s.as_dict().unwrap();
            fields.values().all(|v| is_consistent(a, v))
        }

        // ── Union ──────────────────────────────────────────────────────────
        (Type::Union(vs), t) => vs.iter().any(|v| is_consistent(v, t)),
        (s, Type::Union(vs)) => vs.iter().any(|v| is_consistent(s, v)),

        // ── Forall ────────────────────────────────────────────────────────
        (Type::Forall(_, body_s), t) => is_consistent(body_s, t),
        (s, Type::Forall(_, body_t)) => is_consistent(s, body_t),

        // ── Lam — type-level lambda ───────────────────────────────────────
        // A Lam in isolation (not beta-reduced away by the App check above)
        // is treated as gradual — consistent with anything.
        (Type::Lam(_, _), _) | (_, Type::Lam(_, _)) => true,

        // Fall through to subtyping.
        _ => is_subtype(s, t),
    }
}

/// Consistency for App types.
fn is_app_consistent(s: &Type, t: &Type) -> bool {
    // Two-arg.
    if let (Some((sn, sa, sb)), Some((tn, ta, tb))) = (s.as_applied_two(), t.as_applied_two()) {
        if sn == tn {
            return is_consistent(sa, ta) && is_consistent(sb, tb);
        }
        if sn == "Lens" && tn == "Traversal" {
            return is_consistent(sa, ta) && is_consistent(sb, tb);
        }
        if sn == "Traversal" && tn == "Lens" {
            return is_consistent(sa, ta) && is_consistent(sb, tb);
        }
    }

    // Single-arg.
    if let (Some((sn, si)), Some((tn, ti))) = (s.as_applied_single(), t.as_applied_single()) {
        if sn == tn {
            return is_consistent(si, ti);
        }
        if (sn == "NonEmpty" && tn == "List") || (sn == "List" && tn == "NonEmpty") {
            return is_consistent(si, ti);
        }
    }

    // Abstract heads.
    is_subtype(s, t)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::typecheck::types::{Kind, Type, TypeVarId};

    fn var(name: &str) -> Type {
        Type::var(TypeVarId(name.to_string()))
    }

    fn list(t: Type) -> Type {
        Type::list(t)
    }

    fn func(a: Type, b: Type) -> Type {
        Type::Function(Box::new(a), Box::new(b))
    }

    fn io(t: Type) -> Type {
        Type::io(t)
    }

    fn lens(a: Type, b: Type) -> Type {
        Type::lens(a, b)
    }

    fn traversal(a: Type, b: Type) -> Type {
        Type::traversal(a, b)
    }

    fn closed(fields: &[(&str, Type)]) -> Type {
        Type::Record {
            fields: fields
                .iter()
                .map(|(k, v)| ((*k).to_string(), v.clone()))
                .collect(),
            open: false,
            rows: vec![],
        }
    }

    fn open(fields: &[(&str, Type)]) -> Type {
        Type::Record {
            fields: fields
                .iter()
                .map(|(k, v)| ((*k).to_string(), v.clone()))
                .collect(),
            open: true,
            rows: vec![],
        }
    }

    fn union(types: Vec<Type>) -> Type {
        Type::Union(types)
    }

    // ── Reflexivity ─────────────────────────────────────────────────────────

    #[test]
    fn reflexivity_primitives() {
        assert!(is_subtype(&Type::Number, &Type::Number));
        assert!(is_subtype(&Type::String, &Type::String));
        assert!(is_subtype(&Type::Symbol, &Type::Symbol));
        assert!(is_subtype(&Type::Bool, &Type::Bool));
        assert!(is_subtype(&Type::Null, &Type::Null));
        assert!(is_subtype(&Type::DateTime, &Type::DateTime));
    }

    #[test]
    fn reflexivity_special() {
        assert!(is_subtype(&Type::Any, &Type::Any));
        assert!(is_subtype(&Type::Top, &Type::Top));
        assert!(is_subtype(&Type::Never, &Type::Never));
    }

    #[test]
    fn reflexivity_collections() {
        assert!(is_subtype(&Type::Set, &Type::Set));
        assert!(is_subtype(&Type::Vec, &Type::Vec));
        assert!(is_subtype(&Type::Array, &Type::Array));
    }

    // ── Top and Never ────────────────────────────────────────────────────────

    #[test]
    fn never_is_bottom() {
        assert!(is_subtype(&Type::Never, &Type::Number));
        assert!(is_subtype(&Type::Never, &Type::Top));
        assert!(is_subtype(&Type::Never, &Type::Any));
        assert!(is_subtype(&Type::Never, &list(Type::String)));
        assert!(is_subtype(&Type::Never, &func(Type::Number, Type::Bool)));
    }

    #[test]
    fn top_is_ceiling() {
        assert!(is_subtype(&Type::Number, &Type::Top));
        assert!(is_subtype(&Type::String, &Type::Top));
        assert!(is_subtype(&list(Type::Bool), &Type::Top));
        assert!(is_subtype(&Type::Never, &Type::Top));
    }

    #[test]
    fn top_is_not_subtype_of_primitives() {
        assert!(!is_subtype(&Type::Top, &Type::Number));
        assert!(!is_subtype(&Type::Top, &Type::String));
        assert!(!is_subtype(&Type::Top, &list(Type::Number)));
    }

    #[test]
    fn primitives_not_subtypes_of_never() {
        assert!(!is_subtype(&Type::Number, &Type::Never));
        assert!(!is_subtype(&Type::String, &Type::Never));
    }

    // ── Primitives ───────────────────────────────────────────────────────────

    #[test]
    fn no_cross_primitive_subtyping() {
        assert!(!is_subtype(&Type::Number, &Type::String));
        assert!(!is_subtype(&Type::String, &Type::Number));
        assert!(!is_subtype(&Type::Bool, &Type::Null));
        assert!(!is_subtype(&Type::Symbol, &Type::String));
        assert!(!is_subtype(&Type::DateTime, &Type::Number));
    }

    // ── Lists — covariant ────────────────────────────────────────────────────

    #[test]
    fn list_covariance() {
        assert!(is_subtype(&list(Type::Never), &list(Type::Number)));
        assert!(is_subtype(&list(Type::Number), &list(Type::Top)));
        assert!(is_subtype(&list(Type::Number), &list(Type::Number)));
    }

    #[test]
    fn list_not_covariant_opposite() {
        assert!(!is_subtype(&list(Type::Number), &list(Type::String)));
        assert!(!is_subtype(&list(Type::Top), &list(Type::Number)));
    }

    #[test]
    fn no_cross_collection_subtyping() {
        assert!(!is_subtype(&Type::Set, &list(Type::Number)));
        assert!(!is_subtype(&Type::Vec, &list(Type::Number)));
        assert!(!is_subtype(&Type::Array, &list(Type::Number)));
        assert!(!is_subtype(&list(Type::Number), &Type::Set));
        assert!(!is_subtype(&Type::Set, &Type::Vec));
        assert!(!is_subtype(&Type::Vec, &Type::Array));
        assert!(!is_subtype(&Type::Array, &Type::Set));
    }

    // ── NonEmpty ─────────────────────────────────────────────────────────────

    #[test]
    fn non_empty_refines_list() {
        let ne = Type::non_empty(Type::Number);
        let l = list(Type::Number);
        assert!(is_subtype(&ne, &l));
        assert!(!is_subtype(&l, &ne));
    }

    #[test]
    fn non_empty_covariant() {
        let ne1 = Type::non_empty(Type::Never);
        let ne2 = Type::non_empty(Type::Number);
        assert!(is_subtype(&ne1, &ne2));
    }

    // ── Tuples ───────────────────────────────────────────────────────────────

    #[test]
    fn tuple_widens_to_list() {
        let t = Type::Tuple(vec![Type::Number, Type::String]);
        let l = list(union(vec![Type::Number, Type::String]));
        assert!(is_subtype(&t, &l));
    }

    #[test]
    fn tuple_widens_to_any_list() {
        let t = Type::Tuple(vec![Type::Number, Type::String]);
        assert!(is_subtype(&t, &list(Type::Any)));
    }

    #[test]
    fn unit_tuple_widens_to_list() {
        let t = Type::Tuple(vec![Type::Number]);
        assert!(is_subtype(&t, &list(Type::Number)));
    }

    #[test]
    fn tuple_covariance() {
        let s = Type::Tuple(vec![Type::Never, Type::String]);
        let t = Type::Tuple(vec![Type::Number, Type::String]);
        assert!(is_subtype(&s, &t));
    }

    #[test]
    fn tuple_length_mismatch() {
        let s = Type::Tuple(vec![Type::Number, Type::String]);
        let t = Type::Tuple(vec![Type::Number]);
        assert!(!is_subtype(&s, &t));
        assert!(!is_subtype(&t, &s));
    }

    // ── Records ──────────────────────────────────────────────────────────────

    #[test]
    fn record_reflexivity() {
        let r = closed(&[("x", Type::Number)]);
        assert!(is_subtype(&r, &r));
    }

    #[test]
    fn record_width_subtyping() {
        let wider = closed(&[("x", Type::Number), ("y", Type::String)]);
        let narrower = closed(&[("x", Type::Number)]);
        assert!(is_subtype(&wider, &narrower));
        assert!(!is_subtype(&narrower, &wider));
    }

    #[test]
    fn record_depth_subtyping() {
        let s = closed(&[("x", Type::Never)]);
        let t = closed(&[("x", Type::Number)]);
        assert!(is_subtype(&s, &t));
        assert!(!is_subtype(&t, &s));
    }

    #[test]
    fn open_record_subtype_of_open_with_fewer_fields() {
        let wider = open(&[("x", Type::Number), ("y", Type::String)]);
        let narrower = open(&[("x", Type::Number)]);
        assert!(is_subtype(&wider, &narrower));
    }

    #[test]
    fn open_record_not_subtype_of_closed() {
        let s = open(&[("x", Type::Number)]);
        let t = closed(&[("x", Type::Number)]);
        assert!(!is_subtype(&s, &t));
    }

    #[test]
    fn closed_subtype_of_open() {
        let s = closed(&[("x", Type::Number)]);
        let t = open(&[("x", Type::Number)]);
        assert!(is_subtype(&s, &t));
    }

    #[test]
    fn missing_required_field() {
        let s = closed(&[("y", Type::Number)]);
        let t = closed(&[("x", Type::Number)]);
        assert!(!is_subtype(&s, &t));
    }

    #[test]
    fn empty_open_record_is_supertype() {
        let any_block = open(&[]);
        let concrete = closed(&[("x", Type::Number)]);
        assert!(is_subtype(&concrete, &any_block));
    }

    // ── Functions ────────────────────────────────────────────────────────────

    #[test]
    fn function_contravariant_input() {
        let s = func(Type::Top, Type::Number);
        let t = func(Type::Number, Type::Number);
        assert!(is_subtype(&s, &t));
        assert!(!is_subtype(&t, &s));
    }

    #[test]
    fn function_covariant_output() {
        let s = func(Type::Number, Type::Never);
        let t = func(Type::Number, Type::String);
        assert!(is_subtype(&s, &t));
        assert!(!is_subtype(&t, &s));
    }

    #[test]
    fn function_full_rule() {
        let s = func(Type::Top, Type::Never);
        let t = func(Type::Number, Type::String);
        assert!(is_subtype(&s, &t));
    }

    // ── IO ───────────────────────────────────────────────────────────────────

    #[test]
    fn io_covariance() {
        assert!(is_subtype(&io(Type::Never), &io(Type::Number)));
        assert!(is_subtype(&io(Type::Number), &io(Type::Number)));
        assert!(is_subtype(&io(Type::Number), &io(Type::Top)));
    }

    #[test]
    fn io_not_subtype_of_plain() {
        assert!(!is_subtype(&io(Type::Number), &Type::Number));
        assert!(!is_subtype(&Type::Number, &io(Type::Number)));
    }

    // ── Lens / Traversal ─────────────────────────────────────────────────────

    #[test]
    fn lens_subtype_of_traversal() {
        let l = lens(Type::Number, Type::String);
        let tr = traversal(Type::Number, Type::String);
        assert!(is_subtype(&l, &tr));
        assert!(!is_subtype(&tr, &l));
    }

    #[test]
    fn lens_covariant() {
        let s = lens(Type::Never, Type::Never);
        let t = lens(Type::Number, Type::String);
        assert!(is_subtype(&s, &t));
    }

    #[test]
    fn traversal_covariant() {
        let s = traversal(Type::Never, Type::Never);
        let t = traversal(Type::Number, Type::String);
        assert!(is_subtype(&s, &t));
    }

    #[test]
    fn lens_not_subtype_of_different_traversal() {
        let l = lens(Type::Number, Type::String);
        let tr = traversal(Type::String, Type::Number);
        assert!(!is_subtype(&l, &tr));
    }

    // ── Unions ───────────────────────────────────────────────────────────────

    #[test]
    fn type_subtype_of_union_containing_it() {
        let u = union(vec![Type::Number, Type::String]);
        assert!(is_subtype(&Type::Number, &u));
        assert!(is_subtype(&Type::String, &u));
    }

    #[test]
    fn type_not_subtype_of_union_not_containing_it() {
        let u = union(vec![Type::Number, Type::String]);
        assert!(!is_subtype(&Type::Bool, &u));
    }

    #[test]
    fn union_subtype_of_type_when_all_variants_are() {
        let u = union(vec![Type::Never, Type::Never]);
        assert!(is_subtype(&u, &Type::Number));
    }

    #[test]
    fn union_not_subtype_when_some_variant_is_not() {
        let u = union(vec![Type::Number, Type::String]);
        assert!(!is_subtype(&u, &Type::Number));
    }

    #[test]
    fn union_subtype_of_wider_union() {
        let u1 = union(vec![Type::Number, Type::String]);
        let u2 = union(vec![Type::Number, Type::String, Type::Bool]);
        assert!(is_subtype(&u1, &u2));
        assert!(!is_subtype(&u2, &u1));
    }

    // ── Transitivity ─────────────────────────────────────────────────────────

    #[test]
    fn transitivity_never_through_number_to_top() {
        assert!(is_subtype(&Type::Never, &Type::Number));
        assert!(is_subtype(&Type::Number, &Type::Top));
        assert!(is_subtype(&Type::Never, &Type::Top));
    }

    #[test]
    fn transitivity_list_covariance() {
        assert!(is_subtype(&list(Type::Never), &list(Type::Number)));
        assert!(is_subtype(&list(Type::Number), &list(Type::Top)));
        assert!(is_subtype(&list(Type::Never), &list(Type::Top)));
    }

    #[test]
    fn transitivity_function() {
        let f1 = func(Type::Top, Type::Never);
        let f2 = func(Type::Number, Type::Bool);
        let f3 = func(Type::Never, Type::Top);
        assert!(is_subtype(&f1, &f2));
        assert!(is_subtype(&f2, &f3));
        assert!(is_subtype(&f1, &f3));
    }

    // ── Any ──────────────────────────────────────────────────────────────────

    #[test]
    fn any_is_subtype_of_everything_in_subtype() {
        assert!(is_subtype(&Type::Any, &Type::Number));
        assert!(is_subtype(&Type::Any, &Type::String));
        assert!(is_subtype(&Type::Any, &Type::Top));
        assert!(is_subtype(&Type::Any, &list(Type::Number)));
    }

    #[test]
    fn everything_is_subtype_of_any_in_subtype() {
        assert!(is_subtype(&Type::Number, &Type::Any));
        assert!(is_subtype(&Type::String, &Type::Any));
        assert!(is_subtype(&list(Type::Bool), &Type::Any));
    }

    // ── Type variables ───────────────────────────────────────────────────────

    #[test]
    fn type_var_consistent_with_everything() {
        assert!(is_subtype(&var("a"), &Type::Number));
        assert!(is_subtype(&Type::Number, &var("a")));
        assert!(is_subtype(&var("a"), &var("b")));
    }

    // ── Consistency ─────────────────────────────────────────────────────────

    #[test]
    fn any_consistent_with_all_types() {
        assert!(is_consistent(&Type::Any, &Type::Number));
        assert!(is_consistent(&Type::Number, &Type::Any));
        assert!(is_consistent(&Type::Any, &Type::String));
        assert!(is_consistent(&Type::String, &Type::Any));
        assert!(is_consistent(&Type::Any, &list(Type::Bool)));
        assert!(is_consistent(&list(Type::Bool), &Type::Any));
        assert!(is_consistent(&Type::Any, &Type::Any));
    }

    #[test]
    fn consistency_not_transitive() {
        assert!(is_consistent(&Type::Number, &Type::Any));
        assert!(is_consistent(&Type::Any, &Type::String));
        assert!(!is_consistent(&Type::Number, &Type::String));
    }

    #[test]
    fn consistency_falls_through_to_subtyping() {
        assert!(is_consistent(&Type::Number, &Type::Number));
        assert!(is_consistent(&Type::Never, &Type::Number));
        assert!(!is_consistent(&Type::Number, &Type::String));
    }

    #[test]
    fn consistency_list_any() {
        assert!(is_consistent(&list(Type::Any), &list(Type::Number)));
        assert!(is_consistent(&list(Type::Number), &list(Type::Any)));
    }

    #[test]
    fn consistency_io_any() {
        assert!(is_consistent(&io(Type::Any), &io(Type::String)));
        assert!(is_consistent(&io(Type::String), &io(Type::Any)));
    }

    #[test]
    fn consistency_function_any() {
        let f_any = func(Type::Any, Type::Any);
        let f_num = func(Type::Number, Type::String);
        assert!(is_consistent(&f_any, &f_num));
        assert!(is_consistent(&f_num, &f_any));
    }

    #[test]
    fn consistency_record_any_field() {
        let s = closed(&[("x", Type::Any)]);
        let t = closed(&[("x", Type::Number)]);
        assert!(is_consistent(&s, &t));
        assert!(is_consistent(&t, &s));
    }

    #[test]
    fn consistency_union_any() {
        let u = union(vec![Type::Number, Type::Any]);
        assert!(is_consistent(&u, &Type::String));
    }

    // ── Literal symbol types ─────────────────────────────────────────────────

    #[test]
    fn literal_symbol_subtype_of_symbol() {
        let ls = Type::LiteralSymbol("x".to_string());
        assert!(is_subtype(&ls, &Type::Symbol));
    }

    #[test]
    fn symbol_not_subtype_of_literal_symbol() {
        let ls = Type::LiteralSymbol("x".to_string());
        assert!(!is_subtype(&Type::Symbol, &ls));
    }

    #[test]
    fn literal_symbol_equal_subtype() {
        let a = Type::LiteralSymbol("x".to_string());
        let b = Type::LiteralSymbol("x".to_string());
        assert!(is_subtype(&a, &b));
    }

    #[test]
    fn literal_symbol_different_not_subtype() {
        let a = Type::LiteralSymbol("x".to_string());
        let b = Type::LiteralSymbol("y".to_string());
        assert!(!is_subtype(&a, &b));
    }

    #[test]
    fn literal_symbol_consistent_with_symbol() {
        let ls = Type::LiteralSymbol("x".to_string());
        assert!(is_consistent(&ls, &Type::Symbol));
        assert!(is_consistent(&Type::Symbol, &ls));
    }

    #[test]
    fn literal_symbol_in_union_subtype() {
        let u = Type::Union(vec![
            Type::LiteralSymbol("active".to_string()),
            Type::LiteralSymbol("inactive".to_string()),
        ]);
        assert!(is_subtype(&u, &Type::Symbol));
    }

    // ── Edge cases ───────────────────────────────────────────────────────────

    #[test]
    fn empty_union_subtype_of_anything() {
        let empty = union(vec![]);
        assert!(is_subtype(&empty, &Type::Number));
        assert!(is_subtype(&empty, &Type::Top));
    }

    #[test]
    fn lens_subtype_of_matching_traversal_with_never() {
        let l = lens(Type::Never, Type::Never);
        let tr = traversal(Type::Number, Type::String);
        assert!(is_subtype(&l, &tr));
    }

    // ── Dict subtyping ───────────────────────────────────────────────────────

    fn dict(t: Type) -> Type {
        Type::dict(t)
    }

    #[test]
    fn dict_covariant() {
        assert!(is_subtype(&dict(Type::Never), &dict(Type::Number)));
        assert!(is_subtype(&dict(Type::Number), &dict(Type::Number)));
        assert!(is_subtype(&dict(Type::Number), &dict(Type::Top)));
        assert!(!is_subtype(&dict(Type::Number), &dict(Type::String)));
    }

    #[test]
    fn closed_record_subtype_of_dict() {
        let r = closed(&[("a", Type::Number), ("b", Type::Number)]);
        assert!(is_subtype(&r, &dict(Type::Number)));
    }

    #[test]
    fn closed_record_not_subtype_of_wrong_dict() {
        let r = closed(&[("a", Type::Number), ("b", Type::String)]);
        assert!(!is_subtype(&r, &dict(Type::Number)));
    }

    #[test]
    fn open_record_not_subtype_of_dict() {
        let r = open(&[("a", Type::Number)]);
        assert!(!is_subtype(&r, &dict(Type::Number)));
    }

    #[test]
    fn dict_subtype_of_empty_open_record() {
        let any_block = open(&[]);
        assert!(is_subtype(&dict(Type::Number), &any_block));
        assert!(is_subtype(&dict(Type::String), &any_block));
    }

    #[test]
    fn dict_not_subtype_of_named_field_record() {
        let r = closed(&[("x", Type::Number)]);
        assert!(!is_subtype(&dict(Type::Number), &r));
    }

    #[test]
    fn dict_consistent_with_same_dict() {
        assert!(is_consistent(&dict(Type::Number), &dict(Type::Number)));
    }

    #[test]
    fn dict_consistent_with_any_dict() {
        assert!(is_consistent(&dict(Type::Any), &dict(Type::Number)));
        assert!(is_consistent(&dict(Type::Number), &dict(Type::Any)));
    }

    #[test]
    fn dict_consistent_with_empty_open_record() {
        let any_block = open(&[]);
        assert!(is_consistent(&dict(Type::Number), &any_block));
        assert!(is_consistent(&any_block, &dict(Type::Number)));
    }

    // ── Row variable subtyping ────────────────────────────────────────────────

    fn row_rec(fields: &[(&str, Type)], row_name: &str) -> Type {
        Type::Record {
            fields: fields
                .iter()
                .map(|(k, v)| ((*k).to_string(), v.clone()))
                .collect(),
            open: false,
            rows: vec![TypeVarId(row_name.to_string())],
        }
    }

    #[test]
    fn row_var_record_is_open_for_subtyping() {
        let with_row = row_rec(&[("x", Type::Number)], "r");
        let open_rec = open(&[("x", Type::Number)]);
        assert!(is_subtype(&with_row, &open_rec));
    }

    #[test]
    fn row_var_record_not_subtype_of_closed() {
        let with_row = row_rec(&[("x", Type::Number)], "r");
        let closed_rec = closed(&[("x", Type::Number)]);
        assert!(!is_subtype(&with_row, &closed_rec));
    }

    // ── Higher-kinded type subtyping (B1) ────────────────────────────────────

    #[test]
    fn hkt_abstract_head_invariant() {
        // App(Var(m, * → *), a) <: App(Var(m, * → *), b) only if a == b
        let m = TypeVarId("m".into());
        let a_ty = Type::Number;
        let b_ty = Type::String;
        let app_m_a = Type::App(
            Box::new(Type::Var(m.clone(), Kind::star_to_star())),
            Box::new(a_ty.clone()),
        );
        let app_m_b = Type::App(Box::new(Type::Var(m, Kind::star_to_star())), Box::new(b_ty));
        // m a <: m a (reflexivity)
        assert!(is_subtype(&app_m_a, &app_m_a));
        // m number NOT <: m string (invariant abstract head)
        assert!(!is_subtype(&app_m_a, &app_m_b));
    }

    #[test]
    fn hkt_list_covariance_via_app() {
        // [never] <: [number] via App covariance
        assert!(is_subtype(&list(Type::Never), &list(Type::Number)));
    }
}
