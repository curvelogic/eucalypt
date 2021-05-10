//! Common definitions of frequent constants

use dsl::nil;

use crate::common::sourcemap::SourceMap;

use super::{
    intrinsic::{Const, StgIntrinsic},
    syntax::{
        dsl::{self, data, let_, lref, value},
        tags, LambdaForm,
    },
};

/// KNIL - unit
pub struct KNil;

impl StgIntrinsic for KNil {
    fn name(&self) -> &str {
        "KNIL"
    }

    fn wrapper(&self, _source_map: &mut SourceMap) -> LambdaForm {
        value(nil())
    }
}

impl Const for KNil {}

/// K[] - empty list
pub struct KEmptyList;

impl StgIntrinsic for KEmptyList {
    fn name(&self) -> &str {
        "K[]"
    }

    fn wrapper(&self, _source_map: &mut SourceMap) -> LambdaForm {
        value(data(tags::LIST_NIL, vec![]))
    }
}

impl Const for KEmptyList {}

/// K{} - empty block
pub struct KEmptyBlock;

impl StgIntrinsic for KEmptyBlock {
    fn name(&self) -> &str {
        "K{}"
    }

    fn wrapper(&self, _source_map: &mut SourceMap) -> LambdaForm {
        value(let_(
            vec![value(data(tags::LIST_NIL, vec![]))],
            data(tags::BLOCK, vec![lref(0)]),
        ))
    }
}

impl Const for KEmptyBlock {}
