//! The NULL built-in for exporting null

use crate::common::sourcemap::SourceMap;

use super::{
    intrinsic::StgIntrinsic,
    syntax::{
        dsl::{unit, value},
        LambdaForm,
    },
};

/// A constant for NULL
pub struct Null;

impl StgIntrinsic for Null {
    fn name(&self) -> &str {
        "NULL"
    }

    fn wrapper(&self, _source_map: &mut SourceMap) -> LambdaForm {
        value(unit())
    }
}
