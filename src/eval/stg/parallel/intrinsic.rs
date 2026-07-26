//! The `__PARMAP` intrinsic — the sole parallel primitive.
//!
//! The reduction combinators (`par-sum`/`par-max`/`par-min`/`par-concat`) are
//! prelude wrappers that pipe `par-map` into the exact sequential prelude
//! reducer, so they need no dedicated intrinsic and are byte-for-byte identical
//! to their sequential forms by construction.

use crate::eval::{
    error::ExecutionError,
    machine::intrinsic::{CallGlobal3, IntrinsicMachine, StgIntrinsic},
    memory::{mutator::MutatorHeapView, syntax::Ref},
    stg::support::sym_arg,
};

use crate::eval::emit::Emitter;

/// `__PARMAP(name, f, xs)` — parallel map, semantically identical to
/// `xs map(f)`, order-preserving, with a transparent sequential fallback.
///
/// `name` is the surface combinator the user wrote (`:par-map`, `:par-sum`,
/// …). Since the reductions are prelude wrappers over this one primitive, it
/// is the only way a boundary error can name the combinator actually called
/// rather than always saying `par-map`.
///
/// `name` (arg 0) and `xs` (arg 2) are strict, so the wrapper forces the symbol
/// and the list to WHNF before the intrinsic runs.
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
        // args = [name, f, xs]
        let combinator = sym_arg(machine, view, &args[0])?;
        super::par_map(machine, view, &args[1], &args[2], &combinator)
    }
}

impl CallGlobal3 for ParMap {}
