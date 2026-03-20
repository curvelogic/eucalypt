//! __STREAM_NEXT intrinsic implementation
//!
//! Produces a lazy cons cell for each stream element, allowing the
//! STG machine's Update mechanism to memoise the result of each
//! tail thunk.

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::{
            env::SynClosure,
            intrinsic::{CallGlobal1, IntrinsicMachine, StgIntrinsic},
        },
        memory::{
            array::Array,
            loader::load,
            mutator::MutatorHeapView,
            syntax::{LambdaForm, Ref, StgBuilder},
        },
    },
};

use super::{stream::stream_next, support::num_arg, tags::DataConstructor};

/// STREAM_NEXT(handle)
///
/// Advances the stream producer by one element. If the stream is
/// exhausted, returns Nil. Otherwise, builds a lazy cons cell whose
/// head is the current value and whose tail is an updatable thunk
/// that calls STREAM_NEXT(handle) again when forced.
///
/// The `update=true` flag on the tail thunk ensures the STG
/// machine's Update continuation memoises the result, so repeated
/// traversals of the same cons cell share work and the producer is
/// advanced at most once per list position.
pub struct StreamNext;

impl StgIntrinsic for StreamNext {
    fn name(&self) -> &str {
        "STREAM_NEXT"
    }

    /// Disable wrapper inlining — the side-effecting nature of stream
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
                "stream handle must be a positive integer".to_string(),
            )
        })? as u32;

        // Advance the stream by one element
        let value_stg = match stream_next(handle) {
            Some(v) => v,
            None => {
                // Stream exhausted — return Nil
                let nil = view.nil()?;
                return machine.set_closure(SynClosure::new(nil.as_ptr(), machine.root_env()));
            }
        };

        // Build a lazy cons cell:
        //
        //   Let([value, tail_thunk],
        //       Cons(ListCons, [L(0), L(1)]))
        //
        // - Binding 0 (value): the current element, as a non-updatable value
        // - Binding 1 (tail_thunk): an updatable thunk (arity=0, update=true)
        //   whose body calls STREAM_NEXT(handle) again — memoised by the STG
        //   Update continuation so the producer advances at most once per position
        let value_ptr = load(&view, machine.symbol_pool_mut(), value_stg)?;

        let handle_ref = Ref::num(handle as u64);
        let bif_index = self.index() as u8;
        let tail_body = view.app_bif(bif_index, Array::from_slice(&view, &[handle_ref]))?;

        let bindings = Array::from_slice(
            &view,
            &[
                LambdaForm::value(value_ptr),
                LambdaForm::thunk(tail_body.as_ptr()),
            ],
        );

        let cons_body = view.data(
            DataConstructor::ListCons.tag(),
            Array::from_slice(&view, &[Ref::L(0), Ref::L(1)]),
        )?;

        let result = view.let_(bindings, cons_body)?;

        machine.set_closure(SynClosure::new(result.as_ptr(), machine.root_env()))
    }
}

impl CallGlobal1 for StreamNext {}
