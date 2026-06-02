//! List intrinsics

use std::convert::TryInto;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::{
            env::SynClosure,
            intrinsic::{
                CallGlobal0, CallGlobal1, CallGlobal2, Const, IntrinsicMachine, StgIntrinsic,
            },
        },
        memory::{mutator::MutatorHeapView, syntax::Ref},
    },
};

use super::{
    force::SeqNumList,
    support::{
        collect_num_list, data_list_arg, machine_return_bool, machine_return_num_list, num_arg,
    },
    syntax::{
        dsl::{app_bif, case, data, f, force, lambda, local, lref, t, unbox_num, value},
        LambdaForm,
    },
    tags::DataConstructor,
};

use crate::eval::memory::syntax::HeapSyn;

/// A constant for CONS
pub struct Cons;

impl StgIntrinsic for Cons {
    fn name(&self) -> &str {
        "CONS"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            2, // [h t]
            data(DataConstructor::ListCons.tag(), vec![lref(0), lref(1)]),
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            1,
            case(
                local(0),
                vec![
                    (DataConstructor::ListCons.tag(), local(1)),
                    (DataConstructor::ListNil.tag(), TailEmptyErr.global()),
                ],
                // Non-list value: call BIF to render actual value in error
                app_bif(self.index() as u8, vec![lref(0)]),
            ),
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let repr = super::debug::render_debug_repr(machine, view, &args[0]);
        Err(ExecutionError::TailOfNonList(machine.annotation(), repr))
    }
}

impl CallGlobal1 for Tail {}

/// (Unsafe) list HEAD
pub struct Head;

impl StgIntrinsic for Head {
    fn name(&self) -> &str {
        "HEAD"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            1,
            case(
                local(0),
                vec![
                    (DataConstructor::ListCons.tag(), local(0)),
                    (DataConstructor::ListNil.tag(), HeadEmptyErr.global()),
                ],
                // Non-list value: call BIF to render actual value in error
                app_bif(self.index() as u8, vec![lref(0)]),
            ),
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let repr = super::debug::render_debug_repr(machine, view, &args[0]);
        Err(ExecutionError::HeadOfNonList(machine.annotation(), repr))
    }
}

impl CallGlobal1 for Head {}

/// HEAD_EMPTY_ERR — called by the HEAD wrapper when applied to an empty list.
///
/// Emits a `HeadOfEmptyList` error with the current annotation Smid so that
/// the diagnostic points at the call site rather than showing a generic
/// "panic:" message.
pub struct HeadEmptyErr;

impl StgIntrinsic for HeadEmptyErr {
    fn name(&self) -> &str {
        "HEAD_EMPTY_ERR"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        _view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        Err(ExecutionError::HeadOfEmptyList(machine.annotation()))
    }
}

/// TAIL_EMPTY_ERR — called by the TAIL wrapper when applied to an empty list.
///
/// Emits a `TailOfEmptyList` error with the current annotation Smid.
pub struct TailEmptyErr;

impl StgIntrinsic for TailEmptyErr {
    fn name(&self) -> &str {
        "TAIL_EMPTY_ERR"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        _view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        Err(ExecutionError::TailOfEmptyList(machine.annotation()))
    }
}

impl CallGlobal0 for HeadEmptyErr {}
impl CallGlobal0 for TailEmptyErr {}

/// NILP(xs) — test whether a list is empty.
///
/// Generates a direct tag check rather than the lambda `λ(x). EQ(x, [])`,
/// saving a function-call, env-frame creation, and EQ dispatch per call.
/// The wrapper inlines (via `ProtoInline`) to:
///   `case xs of Nil → True | _ → False`
pub struct NilP;

impl StgIntrinsic for NilP {
    fn name(&self) -> &str {
        "NILP"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            1,
            case(local(0), vec![(DataConstructor::ListNil.tag(), t())], f()),
        )
    }
}

impl CallGlobal1 for NilP {}

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

/// ISNUMBER(value)
///
/// Return true if the value is a number, false otherwise
pub struct IsNumber;

impl StgIntrinsic for IsNumber {
    fn name(&self) -> &str {
        "ISNUMBER"
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
        let result = matches!(
            &*code,
            syntax::HeapSyn::Cons { tag, .. } if *tag == DataConstructor::BoxedNumber.tag()
        );
        machine_return_bool(machine, view, result)
    }
}

impl CallGlobal1 for IsNumber {}

/// ISSTRING(value)
///
/// Return true if the value is a string, false otherwise
pub struct IsString;

impl StgIntrinsic for IsString {
    fn name(&self) -> &str {
        "ISSTRING"
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
        let result = matches!(
            &*code,
            syntax::HeapSyn::Cons { tag, .. } if *tag == DataConstructor::BoxedString.tag()
        );
        machine_return_bool(machine, view, result)
    }
}

impl CallGlobal1 for IsString {}

/// ISSYMBOL(value)
///
/// Return true if the value is a symbol, false otherwise
pub struct IsSymbol;

impl StgIntrinsic for IsSymbol {
    fn name(&self) -> &str {
        "ISSYMBOL"
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
        let result = matches!(
            &*code,
            syntax::HeapSyn::Cons { tag, .. } if *tag == DataConstructor::BoxedSymbol.tag()
        );
        machine_return_bool(machine, view, result)
    }
}

impl CallGlobal1 for IsSymbol {}

/// ISBOOL(value)
///
/// Return true if the value is a boolean, false otherwise
pub struct IsBool;

impl StgIntrinsic for IsBool {
    fn name(&self) -> &str {
        "ISBOOL"
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
        let is_bool = matches!(
            &*code,
            syntax::HeapSyn::Cons { tag, .. }
                if *tag == DataConstructor::BoolTrue.tag()
                    || *tag == DataConstructor::BoolFalse.tag()
        );
        machine_return_bool(machine, view, is_bool)
    }
}

impl CallGlobal1 for IsBool {}

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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        let bif_index: u8 = self.index().try_into().unwrap();
        lambda(
            1, // [xs]
            force(
                SeqNumList.global(lref(0)),
                // [concrete_list] [xs]
                app_bif(bif_index, vec![lref(0)]),
            ),
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

/// LIST.NTH(list, n) — return the nth element (0-indexed) of a list.
///
/// Both arguments are forced (strict: [0, 1]). The list must be a
/// fully-evaluated cons structure. Panics if the list has fewer than
/// n+1 elements.
pub struct ListNth;

impl StgIntrinsic for ListNth {
    fn name(&self) -> &str {
        "LIST.NTH"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let n = {
            let num = num_arg(machine, view, &args[1])?;
            num.as_u64().unwrap_or(0) as usize
        };
        let mut iter = data_list_arg(machine, view, args[0].clone())?;
        let mut current: Option<SynClosure> = None;
        for _ in 0..=n {
            current = iter.next().transpose()?;
        }
        match current {
            Some(closure) => machine.set_closure(closure),
            None => Err(ExecutionError::ListIndexOutOfBounds(
                machine.annotation(),
                n,
            )),
        }
    }
}

impl CallGlobal2 for ListNth {}

/// LIST.DROP(n, list) — drop the first n elements and return the remainder.
///
/// Both arguments are forced (strict: [0, 1]). The list must be a
/// fully-evaluated cons structure. Returns an empty list if n exceeds
/// the list length.
pub struct ListDrop;

impl StgIntrinsic for ListDrop {
    fn name(&self) -> &str {
        "LIST.DROP"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let n = {
            let num = num_arg(machine, view, &args[0])?;
            num.as_u64().unwrap_or(0) as usize
        };

        // Navigate through the cons structure, skipping n elements.
        // We traverse the tail links directly so we can return the
        // remaining cons cell (rather than reconstructing the list).
        let mut closure = machine.nav(view).resolve(&args[1])?;
        for _ in 0..n {
            let code = view.scoped(closure.code());
            match &*code {
                HeapSyn::Cons {
                    tag,
                    args: cons_args,
                } => {
                    match (*tag).try_into() {
                        Ok(DataConstructor::ListCons) => {
                            let tail_ref = cons_args.get(1).ok_or_else(|| {
                                ExecutionError::Panic(
                                    Smid::default(),
                                    "malformed cons cell".to_string(),
                                )
                            })?;
                            closure = closure.navigate_local(&view, tail_ref);
                        }
                        Ok(DataConstructor::ListNil) => {
                            // Ran out of elements — return the nil (empty list)
                            return machine.set_closure(closure);
                        }
                        _ => {
                            return Err(ExecutionError::Panic(
                                Smid::default(),
                                "LIST.DROP: expected list".to_string(),
                            ));
                        }
                    }
                }
                _ => {
                    return Err(ExecutionError::Panic(
                        Smid::default(),
                        "LIST.DROP: expected list".to_string(),
                    ));
                }
            }
        }
        machine.set_closure(closure)
    }
}

impl CallGlobal2 for ListDrop {}

/// `TAKE_NUM_LIST(n, nums)` — return the first `n` elements of a number list
/// as a new number list.
///
/// Replaces `take(n, nums)` for the common case where the input is a num list.
/// The general `take` is a recursive interpreter-level function that creates a
/// thunk chain of depth n; this BIF forces the list via `SeqNumList` and
/// extracts n elements with a single Rust loop.
///
/// Returns an empty num list when n = 0 or the input has fewer than n elements
/// (truncates silently rather than erroring).
///
/// Example: `[3, 1, 4, 1, 5]` with n=3 → `[3.0, 1.0, 4.0]`
pub struct TakeNumList;

impl StgIntrinsic for TakeNumList {
    fn name(&self) -> &str {
        "TAKE_NUM_LIST"
    }

    /// Wrapper: force the num list (arg 1, `nums`) via `SeqNumList`, then
    /// unbox arg 0 (`n`, a `BoxedNumber`), then call the BIF.
    ///
    /// STG argument layout inside the lambda:
    ///   local 0 = n    (BoxedNumber)
    ///   local 1 = nums (list)
    ///
    /// After `force(SeqNumList(local 1), ...)`:
    ///   local 0 = forced_list
    ///   local 1 = n (BoxedNumber, was local 0)
    ///
    /// After `unbox_num(local(1), ...)`:
    ///   local 0 = n_inner (native Num)
    ///   local 1 = forced_list
    ///
    /// BIF call: `(local 0 = n_native, local 1 = forced_list)`
    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        let bif_index: u8 = self.index().try_into().unwrap();
        lambda(
            2, // [n, nums] — n is local 0, nums is local 1
            force(
                SeqNumList.global(lref(1)),
                // After force: local 0 = forced_list, local 1 = n (BoxedNumber)
                unbox_num(
                    local(1),
                    // After unbox_num: local 0 = n_inner (native Num), local 1 = forced_list
                    app_bif(bif_index, vec![lref(0), lref(1)]),
                ),
            ),
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let n = {
            let num = num_arg(machine, view, &args[0])?;
            num.as_u64().unwrap_or(0) as usize
        };
        let nums = collect_num_list(machine, view, args[1].clone())?;
        let result: Vec<f64> = nums.into_iter().take(n).collect();
        machine_return_num_list(machine, view, result)
    }
}

impl CallGlobal2 for TakeNumList {}
