//! PRODUCER_NEXT intrinsic implementation
//!
//! Produces a lazy cons cell for each producer element, allowing the
//! STG machine's Update mechanism to memoise the result of each
//! tail thunk.

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{CallGlobal1, IntrinsicMachine, StgIntrinsic},
        memory::{mutator::MutatorHeapView, syntax::Ref},
    },
};

use super::{
    materialise::materialise_data, stream::producer_next, support::num_arg, tags::DataConstructor,
};

/// PRODUCER_NEXT(handle)
///
/// Advances the producer by one element. If the producer is
/// exhausted, returns Nil. Otherwise, builds a lazy cons cell whose
/// head is the current value and whose tail is an updatable thunk
/// that calls PRODUCER_NEXT(handle) again when forced.
///
/// The `update=true` flag on the tail thunk ensures the STG
/// machine's Update continuation memoises the result, so repeated
/// traversals of the same cons cell share work and the producer is
/// advanced at most once per list position.
pub struct ProducerNext;

impl StgIntrinsic for ProducerNext {
    fn name(&self) -> &str {
        "PRODUCER_NEXT"
    }

    /// Disable wrapper inlining — the side-effecting nature of producer
    /// advancement means the Bif node must not be duplicated by the
    /// compiler's `ProtoInline` substitution.
    fn inlinable(&self) -> bool {
        false
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let handle_num = num_arg(machine, view, &args[0])?;
        let handle = handle_num.as_u64().ok_or_else(|| {
            ExecutionError::Panic(
                Smid::default(),
                "producer handle must be a positive integer".to_string(),
            )
        })? as u32;

        // Advance the producer by one element
        let value_stg = match producer_next(handle) {
            Some(Ok(v)) => v,
            Some(Err(e)) => return Err(e),
            None => {
                // Producer exhausted — return Nil
                let nil = machine.data_value(view, DataConstructor::ListNil.tag(), &[])?;
                return machine.set_result(nil);
            }
        };

        // Build a lazy cons cell `ListCons(value, tail)` where:
        //
        // - `value` is the current element, materialised as a data value; and
        // - `tail` is an updatable thunk that re-enters PRODUCER_NEXT(handle)
        //   when forced, memoised by the machine's Update continuation so the
        //   producer advances at most once per list position.
        //
        // Both `value` and `tail` are built via engine-neutral primitives, so
        // this runs byte-identically on the HeapSyn and bytecode engines. The
        // element is a pure data literal, materialised through the canonical
        // shared `materialise_data` (clone the pool so `&machine` and the
        // interning `&mut pool` do not alias; publish interned symbols after).
        let mut pool = machine.symbol_pool_mut().clone();
        let value = materialise_data(&*machine, view, &mut pool, &value_stg, &[])?;
        *machine.symbol_pool_mut() = pool;
        let tail = machine.bif_tail_thunk(view, self.index() as u8, handle as u64)?;
        let cons = machine.data_value(view, DataConstructor::ListCons.tag(), &[value, tail])?;

        machine.set_result(cons)
    }
}

impl CallGlobal1 for ProducerNext {}
