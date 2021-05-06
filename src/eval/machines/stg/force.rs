//! Utility globals for forcing / evaluating / sequencing

use crate::{common::sourcemap::SourceMap, eval::error::ExecutionError};

use super::{
    machine::{Machine, StgIntrinsic},
    runtime::{call, StgWrapper},
    syntax::{
        dsl::{annotated_lambda, data, force, local, lref, switch, unbox_str},
        tags, LambdaForm, Ref,
    },
};

/// seqStrList to evaluate and unbox lists of strings
pub struct SeqStrList;

impl StgWrapper for SeqStrList {
    fn name(&self) -> &str {
        "seqStrList"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
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
                                    call::global::seq_str_list(lref(3)),
                                    // [stail] [evaled] [unboxed] h t]
                                    data(tags::LIST_CONS, vec![lref(1), lref(0)]),
                                ),
                            ),
                        ),
                    ),
                ],
            ),
            source_map.add_synthetic("seqStrList"),
        )
    }
}

impl StgIntrinsic for SeqStrList {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("seqStrList is STG only")
    }
}
