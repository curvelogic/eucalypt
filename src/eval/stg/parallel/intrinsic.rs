//! The `__PARMAP` intrinsic — the sole parallel primitive.
//!
//! The reduction combinators (`par-sum`/`par-max`/`par-min`/`par-concat`) are
//! prelude wrappers that pipe `par-map` into the exact sequential prelude
//! reducer, so they need no dedicated intrinsic and are byte-for-byte identical
//! to their sequential forms by construction.

use crate::eval::{
    error::ExecutionError,
    machine::intrinsic::{CallGlobal2, IntrinsicMachine, StgIntrinsic},
    memory::{mutator::MutatorHeapView, syntax::Ref},
};

use crate::eval::emit::Emitter;

/// `__PARMAP(f, xs)` — parallel map, semantically identical to `xs map(f)`,
/// order-preserving, with a transparent sequential fallback. `xs` is strict
/// (arg 1), so the wrapper forces it to WHNF before the intrinsic runs.
pub struct ParMap;

impl StgIntrinsic for ParMap {
    fn name(&self) -> &str {
        "PARMAP"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args = [f, xs]
        super::par_map(machine, view, &args[0], &args[1], "par-map")
    }
}

impl CallGlobal2 for ParMap {}
