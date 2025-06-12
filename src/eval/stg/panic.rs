//! The panic intrinsic

use crate::eval::{
    emit::Emitter,
    error::ExecutionError,
    machine::intrinsic::{CallGlobal1, IntrinsicMachine, StgIntrinsic},
    memory::{mutator::MutatorHeapView, syntax::Ref},
};

use super::support::str_arg;

pub struct Panic;

impl StgIntrinsic for Panic {
    fn name(&self) -> &str {
        "PANIC"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), crate::eval::error::ExecutionError> {
        let message = str_arg(machine, view, &args[0])?;
        Err(ExecutionError::Panic(message))
    }
}

impl CallGlobal1 for Panic {}
