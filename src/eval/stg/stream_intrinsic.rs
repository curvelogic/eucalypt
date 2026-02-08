//! __STREAM_NEXT intrinsic implementation
//!
//! Eagerly drains a stream producer and builds a complete list,
//! avoiding thunk-update issues with lazy cons cells in the STG
//! machine.

use crate::eval::{
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
};

use super::{stream::stream_drain, support::num_arg, tags::DataConstructor};

/// STREAM_NEXT(handle)
///
/// Eagerly drains all remaining values from the stream producer
/// identified by the numeric handle, then builds and returns the
/// complete list. This avoids the STG thunk-update issue where
/// lazy tail thunks in `Let` bindings are not memoised across
/// multiple traversals of the same cons cell.
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
            ExecutionError::Panic("stream handle must be a positive integer".to_string())
        })? as u32;

        // Eagerly drain all values from the stream producer
        let values = stream_drain(handle);

        if values.is_empty() {
            let nil = view.nil()?;
            return machine.set_closure(SynClosure::new(nil.as_ptr(), machine.root_env()));
        }

        // Build a complete list in reverse: start with Nil, then
        // prepend each element as a Cons cell.
        //
        // Layout: LetRec([nil, v_n, cons_n, ..., v_0, cons_0], Atom(L(last)))
        let mut bindings: Vec<LambdaForm> = Vec::with_capacity(1 + values.len() * 2);

        // Binding 0: Nil
        let nil_ptr = view.nil()?.as_ptr();
        bindings.push(LambdaForm::value(nil_ptr));

        // Build cons cells from last to first
        for value_stg in values.into_iter().rev() {
            let value_ptr = load(&view, machine.symbol_pool_mut(), value_stg)?;
            bindings.push(LambdaForm::value(value_ptr));
            let val_idx = bindings.len() - 1;
            let prev_idx = val_idx - 1; // previous cons cell (or nil)

            let cons_node = view
                .data(
                    DataConstructor::ListCons.tag(),
                    Array::from_slice(&view, &[Ref::L(val_idx), Ref::L(prev_idx)]),
                )?
                .as_ptr();
            bindings.push(LambdaForm::value(cons_node));
        }

        // The last binding is the outermost cons cell
        let list_idx = bindings.len() - 1;
        let body = view.atom(Ref::L(list_idx))?;
        let result = view.letrec(Array::from_slice(&view, &bindings), body)?;

        machine.set_closure(SynClosure::new(result.as_ptr(), machine.root_env()))
    }
}

impl CallGlobal1 for StreamNext {}
