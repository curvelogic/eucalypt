//! Utility globals for forcing / evaluating / sequencing

use crate::common::sourcemap::Smid;

use super::{
    intrinsic::{CallGlobal1, StgIntrinsic},
    syntax::{
        dsl::{annotated_lambda, data, force, local, lref, switch, unbox_str},
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

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
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
            annotation,
        )
    }
}

impl CallGlobal1 for SeqStrList {}
