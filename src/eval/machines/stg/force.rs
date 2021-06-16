//! Utility globals for forcing / evaluating / sequencing

use crate::common::sourcemap::Smid;

use super::{
    intrinsic::{CallGlobal1, StgIntrinsic},
    syntax::{
        dsl::{annotated_lambda, data, force, local, lref, switch, unbox_str},
        tags, LambdaForm,
    },
};

/// seqStrList to evaluate and unbox lists of strings
pub struct SeqStrList;

impl StgIntrinsic for SeqStrList {
    fn name(&self) -> &str {
        "seqStrList"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            1,
            switch(
                local(0),
                vec![
                    (tags::LIST_NIL, local(0)),
                    (
                        tags::LIST_CONS, // [h t]
                        unbox_str(
                            local(0),
                            // [unboxed] [h t]
                            force(
                                local(0),
                                // [eval] [unbox] [h t]
                                force(
                                    SeqStrList.global(lref(3)),
                                    // [stail] [evaled] [unboxed] h t]
                                    data(tags::LIST_CONS, vec![lref(1), lref(0)]),
                                ),
                            ),
                        ),
                    ),
                ],
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for SeqStrList {}
