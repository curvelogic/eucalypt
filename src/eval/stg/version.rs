//! Version assertion intrinsic

use crate::eval::{
    emit::Emitter,
    error::ExecutionError,
    machine::intrinsic::{CallGlobal1, IntrinsicMachine, StgIntrinsic},
    memory::{mutator::MutatorHeapView, syntax::Ref},
};

use super::support::{machine_return_unit, str_arg};

/// REQUIRES(constraint) — assert that the current eucalypt version
/// satisfies the given semver constraint string.
pub struct Requires;

impl StgIntrinsic for Requires {
    fn name(&self) -> &str {
        "REQUIRES"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let constraint_str = str_arg(machine, view, &args[0])?;

        let req = semver::VersionReq::parse(&constraint_str).map_err(|e| {
            ExecutionError::Panic(format!(
                "invalid version constraint \"{constraint_str}\": {e}"
            ))
        })?;

        // Strip any ".dev" suffix from the Cargo package version
        let version_str = env!("CARGO_PKG_VERSION").replace(".dev", "");
        let version = semver::Version::parse(&version_str).map_err(|e| {
            ExecutionError::Panic(format!(
                "failed to parse eucalypt version \"{version_str}\": {e}"
            ))
        })?;

        if req.matches(&version) {
            machine_return_unit(machine, view)
        } else {
            Err(ExecutionError::Panic(format!(
                "eucalypt version {version} does not satisfy requirement {constraint_str}"
            )))
        }
    }
}

impl CallGlobal1 for Requires {}
