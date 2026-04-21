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
//! ## Consistency (`~`)
//!
//! `is_consistent(S, T)` is the gradual-typing relation.  `any` is consistent
//! with every type in both directions.  For non-`any` types the relation
//! falls through to subtyping.

use super::types::Type;

/// Return `true` if type `s` is a subtype of type `t` (`s <: t`).
///
/// Type variables are treated as `any` (consistent with everything) because
/// at the point of subtyping they have not been instantiated.
pub fn is_subtype(s: &Type, t: &Type) -> bool {
    // Reflexivity
    if s == t {
        return true;
    }

    match (s, t) {
        // ── Top and Never ─────────────────────────────────────────────────────
        // `never` is the bottom type — subtype of everything.
        (Type::Never, _) => true,
        // `top` is the top type — everything is a subtype of it.
        (_, Type::Top) => true,
        // `top` is only a subtype of itself (reflexivity above) or `any`.
        (Type::Top, _) => false,
        // `never` as a supertype is only satisfied by `never` itself
        // (reflexivity above).
        (_, Type::Never) => false,

        // ── Any (gradual) ────────────────────────────────────────────────────
        // `any` is NOT in the subtype lattice — use `is_consistent` for that.
        // Here we treat `any` as consistent in both positions for subtyping
        // purposes (this matches the gradual semantics where `any` flows freely).
        (Type::Any, _) | (_, Type::Any) => true,

        // ── Type variables ───────────────────────────────────────────────────
        // Uninstantiated type variables are treated as `any`.
        (Type::Var(_), _) | (_, Type::Var(_)) => true,

        // ── Literal symbol types ─────────────────────────────────────────────
        // `LiteralSymbol(s) <: LiteralSymbol(t)` iff `s == t` (handled by
        // reflexivity above).  `LiteralSymbol(_) <: Symbol` always.
        (Type::LiteralSymbol(_), Type::Symbol) => true,

        // ── Primitives — flat, no cross-primitive subtyping ───────────────────
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
        // No cross-kind subtyping between set/vec/array/list.

        // ── List — covariant ──────────────────────────────────────────────────
        (Type::List(a), Type::List(b)) => is_subtype(a, b),

        // ── Tuple ─────────────────────────────────────────────────────────────
        // Tuples widen to lists: `(A, B) <: [A | B]`
        (Type::Tuple(elems), Type::List(elem_ty)) => {
            // The tuple is a subtype of `[U]` when every element type is <: U.
            elems.iter().all(|e| is_subtype(e, elem_ty))
        }
        // Tuples are covariant in each component position.
        (Type::Tuple(as_), Type::Tuple(bs)) => {
            as_.len() == bs.len() && as_.iter().zip(bs.iter()).all(|(a, b)| is_subtype(a, b))
        }

        // ── Record — width + depth subtyping ──────────────────────────────────
        //
        // `S <: T` when every field required by `T` is present in `S` and the
        // field type in `S` is a subtype of the field type in `T`.
        //
        // For closed `T`: `S` must have exactly the same set of fields
        // (no extras allowed unless `S` is open).
        // For open `T`: `S` may have additional fields.
        (
            Type::Record {
                fields: s_fields,
                open: s_open,
            },
            Type::Record {
                fields: t_fields,
                open: t_open,
            },
        ) => {
            // Every field required by T must be present in S with a subtype.
            let fields_ok = t_fields
                .iter()
                .all(|(name, t_ty)| match s_fields.get(name) {
                    Some(s_ty) => is_subtype(s_ty, t_ty),
                    None => false,
                });

            if !fields_ok {
                return false;
            }

            // If T is closed, S must not be open: an open S might carry extra
            // fields at runtime that violate T's closed contract.
            // Width subtyping allows S to declare more fields than T (a record
            // with extra fields is more specific, hence a subtype).
            if !t_open && *s_open {
                return false;
            }

            true
        }

        // ── Function — contravariant input, covariant output ──────────────────
        // `(A -> B) <: (C -> D)` iff `C <: A` and `B <: D`
        (Type::Function(a, b), Type::Function(c, d)) => is_subtype(c, a) && is_subtype(b, d),

        // ── IO — covariant ────────────────────────────────────────────────────
        (Type::IO(a), Type::IO(b)) => is_subtype(a, b),

        // ── Lens / Traversal — optics ─────────────────────────────────────────
        // `Lens(A, B) <: Traversal(A, B)` (a lens is a traversal of exactly one).
        // Both Lens and Traversal are covariant in both parameters.
        (Type::Lens(a1, b1), Type::Lens(a2, b2)) => is_subtype(a1, a2) && is_subtype(b1, b2),
        (Type::Traversal(a1, b1), Type::Traversal(a2, b2)) => {
            is_subtype(a1, a2) && is_subtype(b1, b2)
        }
        // Lens is a subtype of Traversal when parameters are compatible.
        (Type::Lens(a1, b1), Type::Traversal(a2, b2)) => is_subtype(a1, a2) && is_subtype(b1, b2),

        // ── Union ─────────────────────────────────────────────────────────────
        // `A | B <: C` — a union is a subtype of C when every variant is.
        // This arm must come before the T-is-union arm so that when both S and
        // T are unions we recurse correctly (each variant of S must be a
        // subtype of T, which in turn checks membership in T's variants).
        (Type::Union(variants), t) => variants.iter().all(|v| is_subtype(v, t)),
        // `A <: A | B` — any (non-union) type is a subtype of a union that
        // contains a supertype of it.
        (s, Type::Union(variants)) => variants.iter().any(|v| is_subtype(s, v)),

        // ── Everything else ───────────────────────────────────────────────────
        _ => false,
    }
}

/// Return `true` if types `s` and `t` are consistent (`s ~ t`).
///
/// Consistency is the gradual-typing relation — it is weaker than subtyping.
/// `any` is consistent with every type in both directions.  For all other
/// types, consistency falls through to subtyping (i.e. `s ~ t` iff `s <: t`
/// when neither is `any`).
///
/// Note: consistency is symmetric but NOT transitive.  `number ~ any` and
/// `any ~ string` do not imply `number ~ string`.
pub fn is_consistent(s: &Type, t: &Type) -> bool {
    // `any` is consistent with everything in both directions.
    if matches!(s, Type::Any) || matches!(t, Type::Any) {
        return true;
    }

    // Literal symbol consistency: `LiteralSymbol(s) ~ Symbol` and vice versa.
    match (s, t) {
        (Type::LiteralSymbol(_), Type::Symbol) | (Type::Symbol, Type::LiteralSymbol(_)) => {
            return true;
        }
        _ => {}
    }

    // Structural consistency: recurse into composite types so that, e.g.,
    // `[any] ~ [number]` holds.
    match (s, t) {
        (Type::List(a), Type::List(b)) => is_consistent(a, b),
        (Type::Tuple(as_), Type::Tuple(bs)) if as_.len() == bs.len() => {
            as_.iter().zip(bs.iter()).all(|(a, b)| is_consistent(a, b))
        }
        (Type::IO(a), Type::IO(b)) => is_consistent(a, b),
        (Type::Lens(a1, b1), Type::Lens(a2, b2)) => is_consistent(a1, a2) && is_consistent(b1, b2),
        (Type::Traversal(a1, b1), Type::Traversal(a2, b2)) => {
            is_consistent(a1, a2) && is_consistent(b1, b2)
        }
        (Type::Lens(a1, b1), Type::Traversal(a2, b2)) => {
            is_consistent(a1, a2) && is_consistent(b1, b2)
        }
        (Type::Function(a, b), Type::Function(c, d)) => {
            // Contravariant input, covariant output — same as subtyping.
            is_consistent(c, a) && is_consistent(b, d)
        }
        (
            Type::Record {
                fields: s_fields,
                open: s_open,
            },
            Type::Record {
                fields: t_fields,
                open: t_open,
            },
        ) => {
            // For shared field names, field types must be consistent.
            // Extra fields in an open record are fine.
            let shared_ok = t_fields.iter().all(|(name, t_ty)| {
                match s_fields.get(name) {
                    Some(s_ty) => is_consistent(s_ty, t_ty),
                    // Missing field in closed S is inconsistent.
                    None => *s_open,
                }
            });
            if !shared_ok {
                return false;
            }
            // Extra fields in S are OK only if T is open or S has no extras.
            if !t_open && s_fields.len() > t_fields.len() && !s_open {
                return false;
            }
            true
        }
        // For union types: consistent if any variant is consistent with t.
        (Type::Union(vs), t) => vs.iter().any(|v| is_consistent(v, t)),
        (s, Type::Union(vs)) => vs.iter().any(|v| is_consistent(s, v)),
        // Fall through to subtyping for all other combinations.
        _ => is_subtype(s, t),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::typecheck::types::{Type, TypeVarId};

    fn var(name: &str) -> Type {
        Type::Var(TypeVarId(name.to_string()))
    }

    fn list(t: Type) -> Type {
        Type::List(Box::new(t))
    }

    fn func(a: Type, b: Type) -> Type {
        Type::Function(Box::new(a), Box::new(b))
    }

    fn io(t: Type) -> Type {
        Type::IO(Box::new(t))
    }

    fn lens(a: Type, b: Type) -> Type {
        Type::Lens(Box::new(a), Box::new(b))
    }

    fn traversal(a: Type, b: Type) -> Type {
        Type::Traversal(Box::new(a), Box::new(b))
    }

    fn closed(fields: &[(&str, Type)]) -> Type {
        Type::Record {
            fields: fields
                .iter()
                .map(|(k, v)| ((*k).to_string(), v.clone()))
                .collect(),
            open: false,
        }
    }

    fn open(fields: &[(&str, Type)]) -> Type {
        Type::Record {
            fields: fields
                .iter()
                .map(|(k, v)| ((*k).to_string(), v.clone()))
                .collect(),
            open: true,
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

    // ── Primitives — no cross-primitive subtyping ────────────────────────────

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

    // ── Tuples ───────────────────────────────────────────────────────────────

    #[test]
    fn tuple_widens_to_list() {
        // (A, B) <: [A | B]  i.e. (number, string) <: [number | string]
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
        // {x: number, y: string} <: {x: number}
        let wider = closed(&[("x", Type::Number), ("y", Type::String)]);
        let narrower = closed(&[("x", Type::Number)]);
        assert!(is_subtype(&wider, &narrower));
        assert!(!is_subtype(&narrower, &wider));
    }

    #[test]
    fn record_depth_subtyping() {
        // {x: never} <: {x: number}
        let s = closed(&[("x", Type::Never)]);
        let t = closed(&[("x", Type::Number)]);
        assert!(is_subtype(&s, &t));
        assert!(!is_subtype(&t, &s));
    }

    #[test]
    fn open_record_subtype_of_open_with_fewer_fields() {
        // {x: number, y: string, ..} <: {x: number, ..}
        let wider = open(&[("x", Type::Number), ("y", Type::String)]);
        let narrower = open(&[("x", Type::Number)]);
        assert!(is_subtype(&wider, &narrower));
    }

    #[test]
    fn open_record_not_subtype_of_closed() {
        // {x: number, ..} is NOT <: {x: number}  because open S might have extras
        let s = open(&[("x", Type::Number)]);
        let t = closed(&[("x", Type::Number)]);
        assert!(!is_subtype(&s, &t));
    }

    #[test]
    fn closed_subtype_of_open() {
        // {x: number} <: {x: number, ..}
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
        // (top -> number) <: (number -> number)  — input is contravariant
        let s = func(Type::Top, Type::Number);
        let t = func(Type::Number, Type::Number);
        assert!(is_subtype(&s, &t));
        // (number -> number) is NOT <: (top -> number)
        assert!(!is_subtype(&t, &s));
    }

    #[test]
    fn function_covariant_output() {
        // (number -> never) <: (number -> string)  — output is covariant
        let s = func(Type::Number, Type::Never);
        let t = func(Type::Number, Type::String);
        assert!(is_subtype(&s, &t));
        // (number -> string) is NOT <: (number -> never)
        assert!(!is_subtype(&t, &s));
    }

    #[test]
    fn function_full_rule() {
        // (C -> D) <: (A -> B) iff A <: C and D <: B
        // (top -> never) <: (number -> string)
        let s = func(Type::Top, Type::Never);
        let t = func(Type::Number, Type::String);
        assert!(is_subtype(&s, &t));
    }

    #[test]
    fn function_reflexive() {
        let f = func(Type::Number, Type::String);
        assert!(is_subtype(&f, &f));
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
        // Traversal is NOT a subtype of Lens
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
        // never <: number, so [never] <: [number] <: [top]
        assert!(is_subtype(&list(Type::Never), &list(Type::Number)));
        assert!(is_subtype(&list(Type::Number), &list(Type::Top)));
        assert!(is_subtype(&list(Type::Never), &list(Type::Top)));
    }

    #[test]
    fn transitivity_function() {
        // (top -> never) <: (number -> bool) <: (never -> top)
        let f1 = func(Type::Top, Type::Never);
        let f2 = func(Type::Number, Type::Bool);
        let f3 = func(Type::Never, Type::Top);
        assert!(is_subtype(&f1, &f2));
        assert!(is_subtype(&f2, &f3));
        assert!(is_subtype(&f1, &f3));
    }

    // ── Any (gradual) ────────────────────────────────────────────────────────

    #[test]
    fn any_is_subtype_of_everything_in_subtype() {
        // In our is_subtype, any flows freely (gradual semantics).
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
    fn consistency_symmetric_for_any() {
        assert!(is_consistent(&Type::Any, &Type::Number));
        assert!(is_consistent(&Type::Number, &Type::Any));
    }

    #[test]
    fn consistency_not_transitive() {
        // number ~ any, any ~ string, but number is NOT consistent with string
        assert!(is_consistent(&Type::Number, &Type::Any));
        assert!(is_consistent(&Type::Any, &Type::String));
        assert!(!is_consistent(&Type::Number, &Type::String));
    }

    #[test]
    fn consistency_falls_through_to_subtyping() {
        // Without any, consistency == subtyping
        assert!(is_consistent(&Type::Number, &Type::Number));
        assert!(is_consistent(&Type::Never, &Type::Number));
        assert!(!is_consistent(&Type::Number, &Type::String));
    }

    #[test]
    fn consistency_list_any() {
        // [any] ~ [number]
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
        // Any variant consistent with target → union is consistent
        assert!(is_consistent(&u, &Type::String));
    }

    // ── Literal symbol types ────────────────────────────────────────────

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
    fn literal_symbol_consistent_with_same() {
        let a = Type::LiteralSymbol("x".to_string());
        let b = Type::LiteralSymbol("x".to_string());
        assert!(is_consistent(&a, &b));
    }

    #[test]
    fn literal_symbol_not_consistent_with_different() {
        let a = Type::LiteralSymbol("x".to_string());
        let b = Type::LiteralSymbol("y".to_string());
        assert!(!is_consistent(&a, &b));
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
        // Empty union behaves like Never
        let empty = union(vec![]);
        assert!(is_subtype(&empty, &Type::Number));
        assert!(is_subtype(&empty, &Type::Top));
    }

    #[test]
    fn lens_subtype_of_matching_traversal_with_never() {
        // Lens(never, never) <: Traversal(number, string) via covariance through Never
        let l = lens(Type::Never, Type::Never);
        let tr = traversal(Type::Number, Type::String);
        assert!(is_subtype(&l, &tr));
    }
}
