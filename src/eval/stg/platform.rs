//! Platform constants exposed to eucalypt as intrinsics.

use crate::common::sourcemap::Smid;
use crate::eval::machine::intrinsic::{Const, StgIntrinsic};

use super::syntax::{
    dsl::{box_str, value},
    LambdaForm,
};

/// __OS — compile-time OS constant (linux, macos, windows, etc.)
pub struct Os;

impl StgIntrinsic for Os {
    fn name(&self) -> &str {
        "OS"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        value(box_str(std::env::consts::OS))
    }
}

impl Const for Os {}

/// __ARCH — compile-time CPU architecture constant (x86_64, aarch64, etc.)
pub struct Arch;

impl StgIntrinsic for Arch {
    fn name(&self) -> &str {
        "ARCH"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        value(box_str(std::env::consts::ARCH))
    }
}

impl Const for Arch {}
