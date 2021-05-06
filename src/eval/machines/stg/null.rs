//! The NULL built-in for exporting null

use crate::common::sourcemap::SourceMap;

use super::{
    machine::StgIntrinsic,
    runtime::StgWrapper,
    syntax::{
        dsl::{unit, value},
        LambdaForm,
    },
};

/// A constant for NULL
pub struct Null;

impl StgWrapper for Null {
    fn name(&self) -> &str {
        "NULL"
    }

    fn wrapper(&self, _source_map: &mut SourceMap) -> LambdaForm {
        value(unit())
    }
}

impl StgIntrinsic for Null {
    fn execute(
        &self,
        _machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        panic!("NULL is STG only")
    }
}
