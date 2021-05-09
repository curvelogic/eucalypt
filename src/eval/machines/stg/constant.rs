//! Common definitions of frequent constants

use dsl::nil;

use crate::{common::sourcemap::SourceMap, eval::error::ExecutionError};

use super::{
    machine::{Machine, StgIntrinsic},
    runtime::StgWrapper,
    syntax::{
        dsl::{self, data, let_, lref, value},
        tags, LambdaForm, Ref,
    },
};

/// KNIL - unit
pub struct KNil;

impl StgWrapper for KNil {
    fn name(&self) -> &str {
        "KNIL"
    }

    fn wrapper(&self, _source_map: &mut SourceMap) -> LambdaForm {
        value(nil())
    }
}

impl StgIntrinsic for KNil {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("KNIL is STG only")
    }
}

/// K[] - empty list
pub struct KEmptyList;

impl StgWrapper for KEmptyList {
    fn name(&self) -> &str {
        "K[]"
    }

    fn wrapper(&self, _source_map: &mut SourceMap) -> LambdaForm {
        value(data(tags::LIST_NIL, vec![]))
    }
}

impl StgIntrinsic for KEmptyList {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("K[] is STG only")
    }
}

/// K{} - empty block
pub struct KEmptyBlock;

impl StgWrapper for KEmptyBlock {
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

impl StgIntrinsic for KEmptyBlock {
    fn execute(&self, _machine: &mut Machine, _args: &[Ref]) -> Result<(), ExecutionError> {
        panic!("{}", "K{{}} is STG only")
    }
}
