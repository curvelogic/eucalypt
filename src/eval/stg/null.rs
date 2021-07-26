//! The NULL built-in for exporting null

use crate::common::sourcemap::Smid;

use super::{
    intrinsic::{Const, StgIntrinsic},
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        value(unit())
    }
}

impl Const for Null {}
