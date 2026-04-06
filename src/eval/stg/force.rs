//! Utility globals for forcing / evaluating / sequencing

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{CallGlobal1, IntrinsicMachine, StgIntrinsic},
        memory::{mutator::MutatorHeapView, syntax::Ref},
    },
};

use super::{
    syntax::{
        dsl::{case, data, force, lambda, local, lref, switch, unbox_num, unbox_str},
        LambdaForm,
    },
    tags::DataConstructor,
};

/// seqStrList to evaluate and unbox lists of strings
pub struct SeqStrList;

impl StgIntrinsic for SeqStrList {
    fn name(&self) -> &str {
        "seqStrList"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            1,
            switch(
                local(0),
                vec![
                    (DataConstructor::ListNil.tag(), local(0)),
                    (
                        DataConstructor::ListCons.tag(), // [h t]
                        unbox_str(
                            local(0),
                            // [unboxed] [h t]
                            force(
                                local(0),
                                // [eval] [unbox] [h t]
                                force(
                                    SeqStrList.global(lref(3)),
                                    // [stail] [evaled] [unboxed] h t]
                                    data(DataConstructor::ListCons.tag(), vec![lref(1), lref(0)]),
                                ),
                            ),
                        ),
                    ),
                ],
            ),
        )
    }
}

impl CallGlobal1 for SeqStrList {}

/// seqNumList to evaluate and unbox lists of numbers
pub struct SeqNumList;

impl StgIntrinsic for SeqNumList {
    fn name(&self) -> &str {
        "seqNumList"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            1,
            switch(
                local(0),
                vec![
                    (DataConstructor::ListNil.tag(), local(0)),
                    (
                        DataConstructor::ListCons.tag(), // [h t]
                        unbox_num(
                            local(0),
                            // [unboxed] [h t]
                            force(
                                local(0),
                                // [eval] [unbox] [h t]
                                force(
                                    SeqNumList.global(lref(3)),
                                    // [stail] [evaled] [unboxed] [h t]
                                    data(DataConstructor::ListCons.tag(), vec![lref(1), lref(0)]),
                                ),
                            ),
                        ),
                    ),
                ],
            ),
        )
    }
}

impl CallGlobal1 for SeqNumList {}

/// SeqList — deep-force a list of primitives, evaluating each element
/// to WHNF (and boxed inner values too).
///
/// Unlike `SeqNumList` / `SeqStrList`, this accepts all primitive
/// types (numbers, strings, symbols).  Used by `VecOf` and
/// `SetFromList` which need fully-evaluated elements before their
/// `execute()` methods navigate heap closures.
///
/// Uses the same pattern as `SeqNumList`: force head, force inner
/// (via unbox), recurse on tail, rebuild list.  The only difference
/// is that it handles all three box types via separate branches
/// rather than just one.
pub struct SeqList;

impl StgIntrinsic for SeqList {
    fn name(&self) -> &str {
        "seqList"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        // Exactly mirrors SeqNumList/SeqStrList but handles all box types.
        //
        // For each cons cell [h, t]:
        //   1. Force h to WHNF (switch on list, force on head)
        //   2. If boxed (BoxedNumber/String/Symbol): unbox to get [inner],
        //      force inner, recurse on tail, rebuild ListCons(forced_inner, seq_t)
        //   3. If raw atom: just recurse on tail, rebuild ListCons(forced_h, seq_t)

        // case already destructured BoxedX([inner]) → local(0) = inner
        // Force inner, recurse on tail, rebuild
        //
        // Entry: [inner] [h t]  (inner = local(0), h = lref(1), t = lref(2))
        // force(local(0)) → [forced] [inner] [h t]
        // SeqList(lref(3)) = SeqList(t) → [seq_t] [forced] [inner] [h t]
        // rebuild: ListCons(lref(1), lref(0)) = ListCons(forced, seq_t)
        let force_inner_then_tail = force(
            local(0),
            force(
                SeqList.global(lref(3)),
                data(DataConstructor::ListCons.tag(), vec![lref(1), lref(0)]),
            ),
        );

        lambda(
            1,
            switch(
                local(0),
                vec![
                    (DataConstructor::ListNil.tag(), local(0)),
                    (
                        DataConstructor::ListCons.tag(), // [h t]
                        // Force head to WHNF then try each box type
                        // If none match, fall through to raw atom path
                        case(
                            local(0),
                            vec![
                                (
                                    DataConstructor::BoxedNumber.tag(),
                                    force_inner_then_tail.clone(),
                                ),
                                (
                                    DataConstructor::BoxedString.tag(),
                                    force_inner_then_tail.clone(),
                                ),
                                (
                                    DataConstructor::BoxedSymbol.tag(),
                                    force_inner_then_tail.clone(),
                                ),
                            ],
                            // Fallback: raw atom, already at WHNF
                            // [forced_h] [h t]
                            force(
                                SeqList.global(lref(2)),
                                // [seq_t] [forced_h] [h t]
                                data(DataConstructor::ListCons.tag(), vec![lref(1), lref(0)]),
                            ),
                        ),
                    ),
                ],
            ),
        )
    }
}

impl CallGlobal1 for SeqList {}

/// FORCE_WHNF — force a thunk to weak head normal form from within an intrinsic.
///
/// Unlike the STG-level `force` DSL combinator (which is woven into the
/// static wrapper lambdas and only available at compile time), this BIF
/// calls back into the machine at runtime via `IntrinsicMachine::evaluate_to_whnf`.
///
/// It is primarily a test vehicle that proves intrinsics can force thunks
/// on demand.  The argument is intentionally non-strict (no strict arg
/// index) so the wrapper does not pre-evaluate it; the BIF receives the
/// raw (possibly unevaluated) closure and forces it itself.
pub struct ForceWhnf;

impl StgIntrinsic for ForceWhnf {
    fn name(&self) -> &str {
        "FORCE_WHNF"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let closure = machine.nav(view).resolve(&args[0])?;
        let forced = machine.evaluate_to_whnf(closure)?;
        machine.set_closure(forced)
    }
}

impl CallGlobal1 for ForceWhnf {}

#[cfg(test)]
mod tests {
    use crate::eval::{
        intrinsics,
        memory::syntax::Native,
        stg::{
            syntax::dsl::{app, app_bif, box_num, gref, let_, letrec_, lref, local, num, unbox_num, value},
            testing,
        },
    };

    use super::ForceWhnf;

    /// Verify that `FORCE_WHNF` can force a thunk inside an intrinsic execution.
    ///
    /// The test wraps a boxed number in a let-bound thunk (a lazy allocation
    /// that is not pre-evaluated by the wrapper's strict-arg mechanism).
    /// `FORCE_WHNF` receives that thunk, forces it via `evaluate_to_whnf`,
    /// and the result is a `BoxedNumber`.  We then unbox it and confirm the
    /// numeric value.
    #[test]
    fn test_force_whnf_forces_thunk() {
        // Build the runtime with only FORCE_WHNF
        let rt = testing::runtime(vec![Box::new(ForceWhnf)]);

        // Syntax:
        //   letrec [
        //     thunk = box_num(42)           -- a closure that yields BoxedNumber(42)
        //   ] in
        //   let [
        //     forced = FORCE_WHNF(lref(0))  -- force the thunk via the BIF
        //   ] in
        //   unbox_num(local(0), local(0))   -- unbox the result to get Num(42)
        let syntax = letrec_(
            vec![value(box_num(42))],
            let_(
                vec![value(app(
                    gref(intrinsics::index("FORCE_WHNF").unwrap()),
                    vec![lref(0)],
                ))],
                unbox_num(local(0), local(0)),
            ),
        );

        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        assert_eq!(m.native_return(), Some(Native::Num(42.into())));
    }

    /// Verify that `FORCE_WHNF` forces an arithmetic thunk.
    ///
    /// Uses ADD (via gref) wrapped in a thunk to produce a computed value.
    /// `FORCE_WHNF` forces the thunk; we then confirm the result terminated.
    #[test]
    fn test_force_whnf_forces_arithmetic_thunk() {
        use crate::eval::stg::arith::Add;
        let rt = testing::runtime(vec![Box::new(ForceWhnf), Box::new(Add)]);

        // letrec [
        //   add_thunk = ADD(num(3), num(4))  -- unevaluated arithmetic
        // ] in
        //   FORCE_WHNF(lref(0))             -- force it
        let syntax = letrec_(
            vec![value(app_bif(
                intrinsics::index_u8("ADD"),
                vec![num(3), num(4)],
            ))],
            app_bif(intrinsics::index_u8("FORCE_WHNF"), vec![lref(0)]),
        );

        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        assert!(m.terminated());
    }
}
