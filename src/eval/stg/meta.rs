//! Metadata intrinsics

use crate::{
    common::sourcemap::Smid,
    eval::machine::intrinsic::{CallGlobal1, CallGlobal2, Const, StgIntrinsic},
};

use super::{
    block::Merge,
    constant::KEmptyBlock,
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

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
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
            annotation,
        )
    }
}

impl CallGlobal1 for Meta {}

/// RAWMETA(obj) - return immediate metadata of object without recursing
/// into inner layers. Unlike META which merges all nested metadata,
/// this returns only the outermost metadata block. Useful for
/// inspecting metadata without triggering infinite recursion when
/// metadata values themselves carry metadata.
pub struct RawMeta;

impl StgIntrinsic for RawMeta {
    fn name(&self) -> &str {
        "RAWMETA"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            1,
            demeta(
                local(0),
                // Return just the immediate metadata without recursing
                // [meta body] — return meta (local(0))
                local(0),
                KEmptyBlock.global(),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for RawMeta {}

/// WITHMETA(meta, obj) - add meta to obj
pub struct WithMeta;

impl StgIntrinsic for WithMeta {
    fn name(&self) -> &str {
        "WITHMETA"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(2, with_meta(lref(0), lref(1)), annotation)
    }
}

impl CallGlobal2 for WithMeta {}
