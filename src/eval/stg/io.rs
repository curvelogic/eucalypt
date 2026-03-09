//! IO monad intrinsics: IO_RETURN, IO_BIND, IO_ACTION
//!
//! These intrinsics are STG-only — their wrappers construct IO data
//! constructors directly without a BIF call. They are the primitive
//! building blocks for the IO monad and are called from the eucalypt
//! `io` prelude.

use crate::{
    common::sourcemap::Smid,
    eval::machine::intrinsic::{CallGlobal2, CallGlobal3, StgIntrinsic},
};

use super::{
    syntax::{
        dsl::{annotated_lambda, data, lref},
        LambdaForm,
    },
    tags::DataConstructor,
};

/// IO_RETURN(world, value) → IoReturn(world, value)
///
/// Wraps a pure value in the IO monad. The world token is threaded
/// through unchanged.
pub struct IoReturn;

impl StgIntrinsic for IoReturn {
    fn name(&self) -> &str {
        "IO_RETURN"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            2, // [world value]
            data(DataConstructor::IoReturn.tag(), vec![lref(1), lref(0)]),
            annotation,
        )
    }
}

impl CallGlobal2 for IoReturn {}

/// IO_BIND(world, action, continuation) → IoBind(world, action, continuation)
///
/// Sequences two IO actions: evaluates `action` then passes its result
/// to `continuation`. The io-run driver interprets the resulting IoBind
/// constructor.
pub struct IoBind;

impl StgIntrinsic for IoBind {
    fn name(&self) -> &str {
        "IO_BIND"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            3, // [world action continuation]
            data(
                DataConstructor::IoBind.tag(),
                vec![lref(2), lref(1), lref(0)],
            ),
            annotation,
        )
    }
}

impl CallGlobal3 for IoBind {}

/// IO_ACTION(world, spec_block) → IoAction(world, spec_block)
///
/// Constructs a primitive IO action from a spec block. The spec block
/// must carry a recognised tag (e.g. `:io-shell`, `:io-exec`) for the
/// io-run driver to dispatch on.
pub struct IoAction;

impl StgIntrinsic for IoAction {
    fn name(&self) -> &str {
        "IO_ACTION"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            2, // [world spec_block]
            data(DataConstructor::IoAction.tag(), vec![lref(1), lref(0)]),
            annotation,
        )
    }
}

impl CallGlobal2 for IoAction {}
