//! Metadata intrinsics

use crate::common::sourcemap::SourceMap;

use super::{
    block::Merge,
    constant::KEmptyBlock,
    intrinsic::{CallGlobal1, CallGlobal2, Const, StgIntrinsic},
    syntax::{
        dsl::{annotated_lambda, demeta, let_, local, lref, value, with_meta},
        LambdaForm,
    },
};

/// META(obj) - return metadata of object or empty block
pub struct Meta;

impl StgIntrinsic for Meta {
    fn name(&self) -> &str {
        "META"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            1,
            demeta(
                local(0),
                // careful: body itself may have more metadata so merge...
                let_(
                    // [meta body] [...]
                    vec![value(Meta.global(lref(1)))],
                    // [inner-meta] [meta body] [...]
                    Merge.global(lref(0), lref(1)),
                ),
                KEmptyBlock.global(),
            ),
            source_map.add_synthetic(self.name()),
        )
    }
}

impl CallGlobal1 for Meta {}

/// WITHMETA(meta, obj) - add meta to obj
pub struct WithMeta;

impl StgIntrinsic for WithMeta {
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

impl CallGlobal2 for WithMeta {}
