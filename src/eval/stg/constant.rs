//! Common definitions of frequent constants

use dsl::nil;

use crate::{
    common::sourcemap::Smid,
    eval::machine::intrinsic::{Const, StgIntrinsic},
};

use super::{
    syntax::{
        dsl::{self, data, let_, lref, no_index, value},
        LambdaForm,
    },
    tags::DataConstructor,
};

/// KNIL - unit
pub struct KNil;

impl StgIntrinsic for KNil {
    fn name(&self) -> &str {
        "KNIL"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        value(data(DataConstructor::ListNil.tag(), vec![]))
    }
}

impl Const for KEmptyList {}

/// K{} - empty block
pub struct KEmptyBlock;

impl StgIntrinsic for KEmptyBlock {
    fn name(&self) -> &str {
        "K{}"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        value(let_(
            vec![value(data(DataConstructor::ListNil.tag(), vec![]))],
            data(DataConstructor::Block.tag(), vec![lref(0), no_index()]),
        ))
    }
}

impl Const for KEmptyBlock {}
