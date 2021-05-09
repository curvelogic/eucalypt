//! Metadata intrinsics

use crate::common::sourcemap::SourceMap;

use super::{
    intrinsic::StgIntrinsic,
    syntax::{
        dsl::{annotated_lambda, data, demeta, let_, local, lref, value, with_meta},
        tags, LambdaForm,
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
