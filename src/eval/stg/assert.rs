//! ASSERT_FAIL intrinsic — reports expected vs actual on assertion failure
//!
//! Called only when an assertion has already been determined to have
//! failed. Attempts to display both values as human-readable strings;
//! falls back to a type label when the value is a structured type
//! (list, block) that cannot be reduced to a scalar.

use std::convert::TryInto;

use crate::eval::{
    emit::Emitter,
    error::ExecutionError,
    machine::intrinsic::{AbiClosure, CallGlobal2, IntrinsicMachine, StgIntrinsic},
    memory::{
        mutator::MutatorHeapView,
        syntax::{Native, Ref},
    },
};

use super::tags::DataConstructor;

/// Format a value ref as a human-readable string for assertion messages.
///
/// Resolves the closure at `r` and formats it:
/// - Boxed scalars (number, string, symbol, zdt) — their literal form
/// - Booleans and null — their literal form
/// - Lists and blocks — a type label such as `[list]` or `{block}`
/// - Unevaluated / unknown — `<unevaluated>`
///
/// Classification goes through the engine-neutral ABI (`resolve_closure`/
/// `data_tag`/`value_native`), so it runs on both the HeapSyn and bytecode
/// engines. The HeapSyn-only `nav` navigator panics on the bytecode engine
/// (eu-mr5e). The value is already at WHNF — the `//=>` operator forced both
/// sides via the equality check — so no forcing is needed here.
fn format_ref(machine: &dyn IntrinsicMachine, view: MutatorHeapView<'_>, r: &Ref) -> String {
    let closure = match machine.resolve_closure(view, r) {
        Ok(c) => c,
        Err(_) => return "<unknown>".to_string(),
    };

    match machine.data_tag(view, &closure) {
        Some(tag) => {
            let dc: Result<DataConstructor, _> = tag.try_into();
            match dc {
                Ok(DataConstructor::BoolTrue) => "true".to_string(),
                Ok(DataConstructor::BoolFalse) => "false".to_string(),
                Ok(DataConstructor::Unit) => "null".to_string(),
                Ok(DataConstructor::ListNil) => "[]".to_string(),
                Ok(DataConstructor::ListCons) => "[list]".to_string(),
                Ok(DataConstructor::Block)
                | Ok(DataConstructor::BlockPair)
                | Ok(DataConstructor::BlockKvList) => "{block}".to_string(),
                Ok(DataConstructor::BoxedNumber) => {
                    boxed_scalar(machine, view, &closure, "<number>")
                }
                Ok(DataConstructor::BoxedString) => {
                    boxed_scalar(machine, view, &closure, "<string>")
                }
                Ok(DataConstructor::BoxedSymbol) => {
                    boxed_scalar(machine, view, &closure, "<symbol>")
                }
                Ok(DataConstructor::BoxedZdt) => {
                    boxed_scalar(machine, view, &closure, "<datetime>")
                }
                Ok(DataConstructor::BoxedTypeData) => {
                    boxed_scalar(machine, view, &closure, "<type-data>")
                }
                Ok(DataConstructor::IoReturn) => "<io-return>".to_string(),
                Ok(DataConstructor::IoBind) => "<io-bind>".to_string(),
                Ok(DataConstructor::IoAction) => "<io-action>".to_string(),
                Ok(DataConstructor::IoFail) => "<io-fail>".to_string(),
                Ok(DataConstructor::Clause) => "<clause>".to_string(),
                Err(_) => "<value>".to_string(),
            }
        }
        None => {
            // Not a data constructor: a bare native atom renders as its
            // literal; anything else is still unevaluated.
            match machine.value_native(view, &closure) {
                Some(n) => format_native(&n, view, machine),
                None => "<unevaluated>".to_string(),
            }
        }
    }
}

/// Render a boxed scalar's inner native (field 0) as a string, falling back to
/// `placeholder` when the inner value is not (yet) available as a native.
fn boxed_scalar(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    closure: &AbiClosure,
    placeholder: &str,
) -> String {
    match machine.value_native(view, closure) {
        Some(n) => format_native(&n, view, machine),
        None => placeholder.to_string(),
    }
}

/// Render a `Native` value as a human-readable string.
fn format_native(n: &Native, view: MutatorHeapView<'_>, machine: &dyn IntrinsicMachine) -> String {
    match n {
        Native::Num(num) => num.to_string(),
        Native::Str(s) => {
            let scoped = view.scoped(*s);
            format!("\"{}\"", (*scoped).as_str())
        }
        Native::Sym(id) => format!(":{}", machine.symbol_pool().resolve(*id)),
        Native::Zdt(dt) => dt.to_rfc3339(),
        Native::Index(_) => "<block-index>".to_string(),
        Native::Set(_) => "<set>".to_string(),
        Native::NdArray(ptr) => {
            let arr = view.scoped(*ptr);
            let shape = arr
                .shape()
                .iter()
                .map(|d| d.to_string())
                .collect::<Vec<_>>()
                .join(",");
            format!("<array [{shape}]>")
        }
        Native::Vec(ptr) => {
            let v = view.scoped(*ptr);
            format!("<vec [{}]>", v.len())
        }
        Native::Prng(state) => format!("<prng {state}>"),
        Native::Producer(handle) => format!("<producer {handle}>"),
    }
}

/// ASSERT_FAIL(actual, expected)
///
/// Always panics. Called by the `//=>`, `//!`, and `//!!` operators
/// when an assertion has been determined to have failed. Formats the
/// panic message with expected and actual values where possible.
pub struct AssertFail;

impl StgIntrinsic for AssertFail {
    fn name(&self) -> &str {
        "ASSERT_FAIL"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let actual_str = format_ref(machine, view, &args[0]);
        let expected_str = format_ref(machine, view, &args[1]);
        Err(ExecutionError::AssertionFailed(
            machine.annotation(),
            actual_str,
            expected_str,
        ))
    }
}

impl CallGlobal2 for AssertFail {}
