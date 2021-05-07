//! Metadata intrinsics

use crate::{common::sourcemap::SourceMap, eval::error::ExecutionError};

use super::{
    machine::{Machine, StgIntrinsic},
    runtime::StgWrapper,
    syntax::{
        dsl::{annotated_lambda, data, demeta, let_, local, lref, value, with_meta},
        tags, LambdaForm, Ref,
    },
};

/// META(obj) - return metadata of object or empty block
pub struct Meta;

impl StgWrapper for Meta {
    fn name(&self) -> &str {
        "META"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            1,
            demeta(
                local(0),
                local(0), // [meta body] [...]
                let_(
                    vec![value(data(tags::LIST_NIL, vec![]))],
                    data(tags::BLOCK, vec![lref(0)]),
                ),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for Meta {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("META is STG only")
    }
}

/// WITHMETA(meta, obj) - add meta to obj
pub struct WithMeta;

impl StgWrapper for WithMeta {
    fn name(&self) -> &str {
        "WITHMETA"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            2,
            with_meta(lref(0), lref(1)),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl StgIntrinsic for WithMeta {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("WITHMETA is STG only")
    }
}
