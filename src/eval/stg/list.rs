//! List intrinsics

use std::convert::TryInto;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{CallGlobal1, CallGlobal2, Const, IntrinsicMachine, StgIntrinsic},
        memory::{mutator::MutatorHeapView, syntax::Ref},
    },
};

use super::{
    force::SeqNumList,
    panic::Panic,
    support::{collect_num_list, machine_return_bool, machine_return_num_list},
    syntax::{
        dsl::{annotated_lambda, app_bif, case, data, force, local, lref, str, value},
        LambdaForm,
    },
    tags::DataConstructor,
};

/// A constant for CONS
pub struct Cons;

impl StgIntrinsic for Cons {
    fn name(&self) -> &str {
        "CONS"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            2, // [h t]
            data(DataConstructor::ListCons.tag(), vec![lref(0), lref(1)]),
            annotation,
        )
    }
}

impl CallGlobal2 for Cons {}

/// A constant for NIL
pub struct Nil;

impl StgIntrinsic for Nil {
    fn name(&self) -> &str {
        "NIL"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        value(data(DataConstructor::ListNil.tag(), vec![]))
    }
}

impl Const for Nil {}

/// (Unsafe) list TAIL
pub struct Tail;

impl StgIntrinsic for Tail {
    fn name(&self) -> &str {
        "TAIL"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            1,
            case(
                local(0),
                vec![(DataConstructor::ListCons.tag(), local(1))],
                Panic.global(str("TAIL on empty list")),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for Tail {}

/// (Unsafe) list HEAD
pub struct Head;

impl StgIntrinsic for Head {
    fn name(&self) -> &str {
        "HEAD"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            1,
            case(
                local(0),
                vec![(DataConstructor::ListCons.tag(), local(0))],
                Panic.global(str("HEAD on empty list")),
            ),
            annotation,
        )
    }
}

impl CallGlobal1 for Head {}

/// ISLIST(value)
///
/// Return true if the value is a list (cons or nil), false otherwise
pub struct IsList;

impl StgIntrinsic for IsList {
    fn name(&self) -> &str {
        "ISLIST"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        use crate::eval::memory::syntax;
        let closure = machine.nav(view).resolve(&args[0])?;
        let code = view.scoped(closure.code());
        let is_list = matches!(
            &*code,
            syntax::HeapSyn::Cons { tag, .. }
                if *tag == DataConstructor::ListCons.tag()
                    || *tag == DataConstructor::ListNil.tag()
        );
        machine_return_bool(machine, view, is_list)
    }
}

impl CallGlobal1 for IsList {}

/// SORT_NUM_LIST — sort a list of numbers in Rust
///
/// The wrapper first applies SeqNumList to force and unbox all elements,
/// then calls the execute method which sorts in Rust and returns a
/// sorted list of boxed numbers.
pub struct SortNumList;

impl StgIntrinsic for SortNumList {
    fn name(&self) -> &str {
        "SORT_NUM_LIST"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        let bif_index: u8 = self.index().try_into().unwrap();
        annotated_lambda(
            1, // [xs]
            force(
                SeqNumList.global(lref(0)),
                // [concrete_list] [xs]
                app_bif(bif_index, vec![lref(0)]),
            ),
            annotation,
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let mut numbers = collect_num_list(machine, view, args[0].clone())?;
        numbers.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        machine_return_num_list(machine, view, numbers)
    }
}

impl CallGlobal1 for SortNumList {}
