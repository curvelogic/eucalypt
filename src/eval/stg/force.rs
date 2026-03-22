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
/// and its boxed inner value to WHNF.
///
/// Unlike `SeqNumList` / `SeqStrList`, this accepts all primitive
/// types (numbers, strings, symbols).  Used by `VecOf` and
/// `SetFromList` which need fully-evaluated elements before their
/// `execute()` methods navigate heap closures.
///
/// For each element, forces to WHNF.  If the result is a boxed
/// constructor (`BoxedNumber`, `BoxedString`, `BoxedSymbol`), forces
/// the inner value too — this ensures that computed values (e.g. from
/// string interpolation) are fully resolved before `extract_primitive`
/// / `navigate_local_native` attempts to read them.
///
/// The returned list reuses the forced/unboxed values, matching the
/// pattern used by `SeqNumList` / `SeqStrList`.
pub struct SeqList;

impl StgIntrinsic for SeqList {
    fn name(&self) -> &str {
        "seqList"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        // For boxed values (BoxedNumber/String/Symbol): match
        // destructures into [inner], force inner, then recurse on tail.
        //
        // Stack after BoxedX match:   [inner] [h t]
        //   force inner →             [forced_inner] [inner] [h t]
        //   recurse SeqList(tail) →    [seq_t] [forced_inner] [inner] [h t]
        //   rebuild: ListCons(lref(1), lref(0))
        //     = ListCons(forced_inner, seq_t)
        let unbox_force_then_tail = force(
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
                        // Force head to WHNF, then branch on constructor
                        case(
                            local(0),
                            vec![
                                (
                                    DataConstructor::BoxedNumber.tag(),
                                    unbox_force_then_tail.clone(),
                                ),
                                (
                                    DataConstructor::BoxedString.tag(),
                                    unbox_force_then_tail.clone(),
                                ),
                                (
                                    DataConstructor::BoxedSymbol.tag(),
                                    unbox_force_then_tail.clone(),
                                ),
                            ],
                            // Fallback: raw atom, already fully evaluated.
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
