//! Closedness analysis for `Let`/`LetRec` bindings (eu-oaz72.175).
//!
//! A binding is *closed* when no local reference in its code can resolve
//! to a slot beyond the binding's own frames: the lambda's argument frame
//! (`arity` slots) and any frames its body introduces (nested `let`s, case
//! branches, `DeMeta` handlers). Such a closure never reads its captured
//! environment, so the machine closes it over the (empty-of-locals) globals
//! frame instead of the enclosing frame. That stops a long-lived closure
//! (e.g. the `+` lambda threaded through a fold) from pinning the frame it
//! happened to be allocated in — and everything that frame holds, such as
//! the head of a list being consumed.
//!
//! This is the narrow, semantics-preserving special case of flat closures:
//! a closure that captures nothing needs no parent frame. The encoder sets
//! [`FORM_CLOSED`](super::FORM_CLOSED) on the binding's form header.
//!
//! **Soundness rule: if in doubt, not closed.** Local references are flat
//! de Bruijn indices across the frame chain, so a reference at index `i`
//! inside `s` locally-introduced slots reaches the captured environment
//! exactly when `i >= s`. The analysis therefore needs a *lower bound* on
//! every frame size it counts; overestimating a frame would wrongly
//! classify an escaping reference as local. Frame sizes whose size is not
//! certain (`DeMeta` handlers, unknown or irregular constructors) count as
//! zero. Siblings in a `LetRec` frame and every outer slot are, by
//! definition, beyond the binding's own frames, so a binding that mentions
//! them (including itself) is never closed.

use crate::eval::stg::{
    syntax::{LambdaForm, Reference, StgSyn},
    tags::{DataConstructor, Tag},
};

/// True when `lf` can safely be closed over the globals frame.
pub fn binding_is_closed(lf: &LambdaForm) -> bool {
    closed_in(lf.body(), lf.arity() as usize)
}

/// Lower bound on the frame a case branch for `tag` pushes.
///
/// `BlockKvList` is built with a single field although its arity is 2 (see
/// `free_slots` in the machine), so it counts as 1; tags that are not data
/// constructors count as 0.
fn branch_frame_lower_bound(tag: Tag) -> usize {
    match DataConstructor::try_from(tag) {
        Ok(DataConstructor::BlockKvList) => 1,
        Ok(dc) => dc.arity(),
        Err(_) => 0,
    }
}

/// Every local ref in `syn` stays within the `s` slots introduced so far.
fn closed_in(syn: &StgSyn, s: usize) -> bool {
    let local_ok = |r: &Reference<_>| match r {
        Reference::L(i) => *i < s,
        _ => true,
    };
    match syn {
        StgSyn::Atom { evaluand } => local_ok(evaluand),
        StgSyn::Case {
            scrutinee,
            branches,
            fallback,
        } => {
            closed_in(scrutinee, s)
                && branches
                    .iter()
                    .all(|(t, b)| closed_in(b, s + branch_frame_lower_bound(*t)))
                && fallback.as_ref().is_none_or(|f| closed_in(f, s + 1))
        }
        StgSyn::Cons { args, .. } | StgSyn::Bif { args, .. } => args.iter().all(local_ok),
        StgSyn::App { callable, args, .. } | StgSyn::DirectApp { callable, args, .. } => {
            local_ok(callable) && args.iter().all(local_ok)
        }
        StgSyn::Let { bindings, body } => {
            // Bindings close over the current env; the body sees the new frame.
            bindings
                .iter()
                .all(|lf| closed_in(lf.body(), s + lf.arity() as usize))
                && closed_in(body, s + bindings.len())
        }
        StgSyn::LetRec { bindings, body } => {
            let inner = s + bindings.len();
            bindings
                .iter()
                .all(|lf| closed_in(lf.body(), inner + lf.arity() as usize))
                && closed_in(body, inner)
        }
        StgSyn::Ann { body, .. } => closed_in(body, s),
        StgSyn::Meta { meta, body } => local_ok(meta) && local_ok(body),
        // Handler/or_else frame sizes are not counted (zero lower bound).
        StgSyn::DeMeta {
            scrutinee,
            handler,
            or_else,
        } => closed_in(scrutinee, s) && closed_in(handler, s) && closed_in(or_else, s),
        StgSyn::Seq { scrutinee, body } => closed_in(scrutinee, s) && closed_in(body, s),
        StgSyn::LookupLit {
            key, obj, default, ..
        } => local_ok(key) && local_ok(obj) && local_ok(default),
        // The encoder emits only the operand refs, never `inner`.
        StgSyn::FusedPrimop { left, right, .. } => local_ok(left) && local_ok(right),
        StgSyn::BlackHole => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::stg::syntax::dsl;
    use crate::eval::stg::tags::DataConstructor as DC;

    fn l(i: usize) -> Reference<crate::eval::stg::syntax::Native> {
        Reference::L(i)
    }

    #[test]
    fn lambda_using_only_its_args_is_closed() {
        let lf = dsl::lambda(2, dsl::app(l(0), vec![l(1)]));
        assert!(binding_is_closed(&lf));
    }

    #[test]
    fn lambda_reading_outer_local_is_not_closed() {
        // arity 1: index 0 is the arg, index 1 is the captured env.
        let lf = dsl::lambda(1, dsl::app(l(0), vec![l(1)]));
        assert!(!binding_is_closed(&lf));
    }

    #[test]
    fn thunk_reading_any_local_is_not_closed() {
        assert!(!binding_is_closed(&dsl::thunk(dsl::local(0))));
        assert!(binding_is_closed(&dsl::thunk(dsl::global(3))));
    }

    #[test]
    fn nested_lambda_referencing_outer_local_is_not_closed() {
        // λ{1}. let g = λ{1}. (✳2)  in g — index 2 inside the inner lambda
        // (1 inner arg + 1 let slot... 1 arg, 1 let frame = 2 locals, so the
        // inner lambda's own arg (0), the letrec slot (1), outer arg (2)).
        // Within the inner lambda s = 1 (inner arg) + 1 (letrec frame) +
        // 1 (outer arg) = 3, so ✳3 escapes while ✳2 is the outer arg.
        let inner = dsl::lambda(1, dsl::atom(l(3)));
        let outer = dsl::lambda(1, dsl::letrec_(vec![inner], dsl::local(0)));
        assert!(!binding_is_closed(&outer));
        let inner_ok = dsl::lambda(1, dsl::atom(l(2)));
        let outer_ok = dsl::lambda(1, dsl::letrec_(vec![inner_ok], dsl::local(0)));
        assert!(binding_is_closed(&outer_ok));
    }

    #[test]
    fn letrec_sibling_and_self_references_are_not_closed() {
        // A binding mentioning its own LetRec slot (index = arity) escapes.
        let self_ref = dsl::lambda(1, dsl::app(l(1), vec![l(0)]));
        assert!(!binding_is_closed(&self_ref));
    }

    #[test]
    fn case_branch_fields_extend_the_local_frame() {
        // ListCons branch pushes 2 fields: with arity 1, indices 0..3 local.
        let branch = dsl::atom(l(2));
        let ok = dsl::lambda(
            1,
            dsl::switch(dsl::local(0), vec![(DC::ListCons.tag(), branch)]),
        );
        assert!(binding_is_closed(&ok));
        let escaping = dsl::lambda(
            1,
            dsl::switch(dsl::local(0), vec![(DC::ListCons.tag(), dsl::atom(l(3)))]),
        );
        assert!(!binding_is_closed(&escaping));
    }

    #[test]
    fn block_kv_list_branch_counts_one_field() {
        // Built with one field despite arity 2: index 2 must NOT count local.
        let lf = dsl::lambda(
            1,
            dsl::switch(
                dsl::local(0),
                vec![(DC::BlockKvList.tag(), dsl::atom(l(2)))],
            ),
        );
        assert!(!binding_is_closed(&lf));
    }

    #[test]
    fn unknown_tag_branch_counts_zero_fields() {
        let lf = dsl::lambda(1, dsl::switch(dsl::local(0), vec![(200, dsl::atom(l(1)))]));
        assert!(!binding_is_closed(&lf));
    }

    #[test]
    fn case_fallback_counts_one_slot() {
        let lf = dsl::lambda(1, dsl::case(dsl::local(0), vec![], dsl::atom(l(1))));
        assert!(binding_is_closed(&lf));
        let lf = dsl::lambda(1, dsl::case(dsl::local(0), vec![], dsl::atom(l(2))));
        assert!(!binding_is_closed(&lf));
    }

    #[test]
    fn demeta_handler_frames_are_not_counted() {
        let lf = dsl::lambda(
            1,
            dsl::demeta(dsl::local(0), dsl::atom(l(1)), dsl::atom(l(0))),
        );
        assert!(!binding_is_closed(&lf));
    }

    #[test]
    fn nested_let_body_sees_new_frame_but_bindings_do_not() {
        // let x = ✳1 (escapes: binding sees the outer env, arity 1) in ✳0
        let escaping = dsl::lambda(1, dsl::let_(vec![dsl::thunk(dsl::local(1))], dsl::local(0)));
        assert!(!binding_is_closed(&escaping));
        // let x = ✳0 in ✳1 — body sees the let slot (0) and arg (1).
        let ok = dsl::lambda(1, dsl::let_(vec![dsl::thunk(dsl::local(0))], dsl::local(1)));
        assert!(binding_is_closed(&ok));
    }

    #[test]
    fn globals_and_values_are_always_local_ok() {
        let lf = dsl::lambda(1, dsl::app(Reference::G(4), vec![dsl::num(1), l(0)]));
        assert!(binding_is_closed(&lf));
    }
}
