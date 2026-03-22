//! Utility globals for forcing / evaluating / sequencing

use crate::{
    common::sourcemap::Smid,
    eval::machine::intrinsic::{CallGlobal1, StgIntrinsic},
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
