//! The panic intrinsic

use crate::eval::error::ExecutionError;

use super::{
    machine::{self, StgIntrinsic},
    runtime::{str_arg, StgWrapper},
};

pub struct Panic;

impl StgWrapper for Panic {
    fn name(&self) -> &str {
        "PANIC"
    }
}

impl StgIntrinsic for Panic {
    fn execute(
        &self,
        machine: &mut machine::Machine,
        args: &[super::syntax::Ref],
    ) -> Result<(), ExecutionError> {
        let message = str_arg(machine, &args[0])?;
        Err(ExecutionError::Panic(message))
    }
}
