//! The panic intrinsic

use crate::eval::error::ExecutionError;

use super::{intrinsic::StgIntrinsic, machine, runtime::str_arg};

pub struct Panic;

impl StgIntrinsic for Panic {
    fn name(&self) -> &str {
        "PANIC"
    }

    fn execute(
        &self,
        machine: &mut machine::Machine,
        args: &[super::syntax::Ref],
    ) -> Result<(), ExecutionError> {
        let message = str_arg(machine, &args[0])?;
        Err(ExecutionError::Panic(message))
    }
}
