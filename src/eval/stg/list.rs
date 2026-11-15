//! List intrinsics

use std::convert::TryInto;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{
            CallGlobal0, CallGlobal1, CallGlobal2, Const, IntrinsicMachine, StgIntrinsic,
        },
        memory::{
            mutator::MutatorHeapView,
            syntax::{Native, Ref},
        },
    },
};

use super::{
    force::SeqNumList,
    support::{collect_num_list, machine_return_bool, machine_return_num_list, num_arg},
    syntax::{
        dsl::{app_bif, case, data, f, force, lambda, local, lref, t, value},
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
        let closure = machine.resolve_closure(view, &args[0])?;
        let is_list = matches!(
            machine.data_tag(view, &closure),
            Some(tag)
                if tag == DataConstructor::ListCons.tag()
                    || tag == DataConstructor::ListNil.tag()
        );
        machine_return_bool(machine, view, is_list)
    }
}

impl CallGlobal1 for IsList {}

/// ISNUMBER(value)
///
/// Return true if the value is a number, false otherwise. A number is
/// either boxed (a literal) or a bare native (the result of arithmetic),
/// so both representations count.
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
        let closure = machine.resolve_closure(view, &args[0])?;
        let result = match machine.data_tag(view, &closure) {
            Some(tag) => tag == DataConstructor::BoxedNumber.tag(),
            None => matches!(machine.value_native(view, &closure), Some(Native::Num(_))),
        };
        machine_return_bool(machine, view, result)
    }
}

impl CallGlobal1 for IsNumber {}

/// ISSTRING(value)
///
/// Return true if the value is a string, false otherwise. Strings are
/// normally boxed by their intrinsic's wrapper; the bare `Native::Str`
/// form is accepted too, for any intrinsic that returns through
/// `machine_return_str` without a boxing wrapper.
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
        let closure = machine.resolve_closure(view, &args[0])?;
        let result = match machine.data_tag(view, &closure) {
            Some(tag) => tag == DataConstructor::BoxedString.tag(),
            None => matches!(machine.value_native(view, &closure), Some(Native::Str(_))),
        };
        machine_return_bool(machine, view, result)
    }
}

impl CallGlobal1 for IsString {}

/// ISSYMBOL(value)
///
/// Return true if the value is a symbol, false otherwise. Boxed and
/// bare native symbols both count.
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
        let closure = machine.resolve_closure(view, &args[0])?;
        let result = match machine.data_tag(view, &closure) {
            Some(tag) => tag == DataConstructor::BoxedSymbol.tag(),
            None => matches!(machine.value_native(view, &closure), Some(Native::Sym(_))),
        };
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
        let closure = machine.resolve_closure(view, &args[0])?;
        let is_bool = matches!(
            machine.data_tag(view, &closure),
            Some(tag)
                if tag == DataConstructor::BoolTrue.tag()
                    || tag == DataConstructor::BoolFalse.tag()
        );
        machine_return_bool(machine, view, is_bool)
    }
}

impl CallGlobal1 for IsBool {}

/// ISTYPEDATA(value)
///
/// Return true if the value is a type-data literal (s"..." syntax), false otherwise
pub struct IsTypeData;

impl StgIntrinsic for IsTypeData {
    fn name(&self) -> &str {
        "ISTYPEDATA"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let closure = machine.resolve_closure(view, &args[0])?;
        let result = matches!(
            machine.data_tag(view, &closure),
            Some(tag) if tag == DataConstructor::BoxedTypeData.tag()
        );
        machine_return_bool(machine, view, result)
    }
}

impl CallGlobal1 for IsTypeData {}

/// ISZDT(value)
///
/// Return true if the value is a zoned datetime, false otherwise. Boxed
/// and bare native datetimes both count.
pub struct IsZdt;

impl StgIntrinsic for IsZdt {
    fn name(&self) -> &str {
        "ISZDT"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let closure = machine.resolve_closure(view, &args[0])?;
        let result = match machine.data_tag(view, &closure) {
            Some(tag) => tag == DataConstructor::BoxedZdt.tag(),
            None => matches!(machine.value_native(view, &closure), Some(Native::Zdt(_))),
        };
        machine_return_bool(machine, view, result)
    }
}

impl CallGlobal1 for IsZdt {}

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
/// Walks exactly n+1 cons cells, forcing only as far as needed, and returns
/// the n-th element unforced. The list head is already forced (strict:
/// [0, 1]); successive tails are forced as the walk advances. Raises
/// `ListIndexOutOfBounds` if the list has fewer than n+1 elements.
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
        let smid = machine.annotation();

        // Bounded traversal via the engine-neutral data_tag/data_field ABI
        // (works on both engines; the HeapSyn-only navigator is not used).
        // Forcing only the n+1 cells we visit restores the lazy invariant: a
        // list well-formed up to index n succeeds even when its tail beyond n
        // would error if forced (eu-xk49). No handle is held across a force —
        // each `force` consumes the handle it is given — so it is GC-safe.
        let mut current = machine.resolve_closure(view, &args[0])?;
        for _ in 0..n {
            let cell = machine.force(current)?;
            match machine.data_tag(view, &cell) {
                Some(tag) if tag == DataConstructor::ListCons.tag() => {
                    current = machine.data_field(view, &cell, 1).ok_or_else(|| {
                        ExecutionError::Panic(smid, "malformed cons cell".to_string())
                    })?;
                }
                Some(tag) if tag == DataConstructor::ListNil.tag() => {
                    return Err(ExecutionError::ListIndexOutOfBounds(smid, n));
                }
                _ => {
                    return Err(ExecutionError::NotValue(
                        smid,
                        "expected list data".to_string(),
                    ));
                }
            }
        }

        let cell = machine.force(current)?;
        match machine.data_tag(view, &cell) {
            Some(tag) if tag == DataConstructor::ListCons.tag() => {
                let head = machine.data_field(view, &cell, 0).ok_or_else(|| {
                    ExecutionError::Panic(smid, "malformed cons cell".to_string())
                })?;
                machine.set_result(head)
            }
            Some(tag) if tag == DataConstructor::ListNil.tag() => {
                Err(ExecutionError::ListIndexOutOfBounds(smid, n))
            }
            _ => Err(ExecutionError::NotValue(
                smid,
                "expected list data".to_string(),
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
        let smid = machine.annotation();

        // Walk the cons spine, skipping n elements, then return the remaining
        // cell. Uses the engine-neutral data_tag/data_field ABI (the strict
        // wrapper has forced the spine), so it runs on both engines rather
        // than the HeapSyn-only navigator, which panics on the bytecode engine
        // (eu-mr5e). No handle is held across an allocation, so it is GC-safe.
        let mut current = machine.resolve_closure(view, &args[1])?;
        for _ in 0..n {
            match machine.data_tag(view, &current) {
                Some(tag) if tag == DataConstructor::ListCons.tag() => {
                    current = machine.data_field(view, &current, 1).ok_or_else(|| {
                        ExecutionError::Panic(smid, "malformed cons cell".to_string())
                    })?;
                }
                Some(tag) if tag == DataConstructor::ListNil.tag() => {
                    // Ran out of elements — return the nil (empty list).
                    return machine.set_result(current);
                }
                _ => {
                    return Err(ExecutionError::NotValue(
                        smid,
                        "drop: expected a list".to_string(),
                    ));
                }
            }
        }
        machine.set_result(current)
    }
}

impl CallGlobal2 for ListDrop {}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::eval::stg::{
        boolean::{False, True},
        runtime::Runtime,
        syntax::{dsl::*, StgSyn},
        testing,
    };
    use chrono::{FixedOffset, TimeZone};
    use std::rc::Rc;

    fn runtime() -> Box<dyn Runtime> {
        testing::runtime(vec![
            Box::new(IsNumber),
            Box::new(IsString),
            Box::new(IsSymbol),
            Box::new(IsZdt),
            Box::new(True),
            Box::new(False),
        ])
    }

    fn eval_bool(syntax: Rc<StgSyn>) -> Option<bool> {
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(100)).unwrap();
        m.bool_return()
    }

    /// Apply a predicate to a boxed value bound in a `let`.
    fn boxed(
        pred: impl Fn(crate::eval::stg::syntax::Ref) -> Rc<StgSyn>,
        value_form: Rc<StgSyn>,
    ) -> Rc<StgSyn> {
        let_(vec![value(value_form)], pred(lref(0)))
    }

    fn a_datetime() -> chrono::DateTime<FixedOffset> {
        FixedOffset::east_opt(0)
            .unwrap()
            .with_ymd_and_hms(2024, 3, 15, 0, 0, 0)
            .unwrap()
    }

    // eu-oaz72.93: each scalar type predicate recognises both the boxed
    // form and the bare native form of its type.

    #[test]
    fn is_number_recognises_boxed_and_bare_numbers() {
        assert_eq!(
            eval_bool(boxed(|r| IsNumber.global(r), box_num(1))),
            Some(true)
        );
        assert_eq!(eval_bool(IsNumber.global(num(1))), Some(true));
        assert_eq!(eval_bool(IsNumber.global(str("1"))), Some(false));
        assert_eq!(
            eval_bool(boxed(|r| IsNumber.global(r), box_str("1"))),
            Some(false)
        );
    }

    #[test]
    fn is_string_recognises_boxed_and_bare_strings() {
        assert_eq!(
            eval_bool(boxed(|r| IsString.global(r), box_str("a"))),
            Some(true)
        );
        assert_eq!(eval_bool(IsString.global(str("a"))), Some(true));
        assert_eq!(eval_bool(IsString.global(sym("a"))), Some(false));
        assert_eq!(eval_bool(IsString.global(num(1))), Some(false));
        assert_eq!(
            eval_bool(boxed(|r| IsString.global(r), box_sym("a"))),
            Some(false)
        );
    }

    #[test]
    fn is_symbol_recognises_boxed_and_bare_symbols() {
        assert_eq!(
            eval_bool(boxed(|r| IsSymbol.global(r), box_sym("a"))),
            Some(true)
        );
        assert_eq!(eval_bool(IsSymbol.global(sym("a"))), Some(true));
        assert_eq!(eval_bool(IsSymbol.global(str("a"))), Some(false));
        assert_eq!(
            eval_bool(boxed(|r| IsSymbol.global(r), box_str("a"))),
            Some(false)
        );
    }

    #[test]
    fn is_zdt_recognises_boxed_and_bare_datetimes() {
        assert_eq!(
            eval_bool(boxed(|r| IsZdt.global(r), box_zdt(a_datetime()))),
            Some(true)
        );
        assert_eq!(eval_bool(IsZdt.global(zdt(a_datetime()))), Some(true));
        assert_eq!(eval_bool(IsZdt.global(str("a"))), Some(false));
        assert_eq!(eval_bool(IsZdt.global(num(1))), Some(false));
        assert_eq!(
            eval_bool(boxed(|r| IsZdt.global(r), box_str("a"))),
            Some(false)
        );
    }
}
