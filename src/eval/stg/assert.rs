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
    machine::intrinsic::{CallGlobal2, IntrinsicMachine, StgIntrinsic},
    memory::{
        mutator::MutatorHeapView,
        syntax::{HeapSyn, Native, Ref},
    },
};

use super::tags::DataConstructor;

/// Format a value ref as a human-readable string for assertion messages.
///
/// Resolves the closure at `r` to WHNF and formats it:
/// - Boxed scalars (number, string, symbol, zdt) — their literal form
/// - Booleans and null — their literal form
/// - Lists and blocks — a type label such as `[list]` or `{block}`
/// - Unevaluated / unknown — `<unevaluated>`
fn format_ref(machine: &dyn IntrinsicMachine, view: MutatorHeapView<'_>, r: &Ref) -> String {
    // Resolve to a closure, following Atom indirections.
    // The closure should already be at WHNF since the //=> operator
    // forced both sides via the equality check.
    let closure = match machine.nav(view).resolve(r) {
        Ok(c) => c,
        Err(_) => return "<unknown>".to_string(),
    };

    let code = view.scoped(closure.code());
    let env = view.scoped(closure.env());

    match &*code {
        HeapSyn::Cons { tag, args } => {
            let dc: Result<DataConstructor, _> = (*tag).try_into();
            match dc {
                Ok(DataConstructor::BoolTrue) => "true".to_string(),
                Ok(DataConstructor::BoolFalse) => "false".to_string(),
                Ok(DataConstructor::Unit) => "null".to_string(),
                Ok(DataConstructor::ListNil) => "[]".to_string(),
                Ok(DataConstructor::ListCons) => "[list]".to_string(),
                Ok(DataConstructor::Block) => "{block}".to_string(),
                Ok(DataConstructor::BlockPair) | Ok(DataConstructor::BlockKvList) => {
                    "{block}".to_string()
                }
                Ok(DataConstructor::BoxedNumber) => {
                    // The inner ref is args[0]: try to get it as a native num
                    args.get(0)
                        .and_then(|inner| format_inner_ref(machine, view, &env, &inner))
                        .unwrap_or_else(|| "<number>".to_string())
                }
                Ok(DataConstructor::BoxedString) => args
                    .get(0)
                    .and_then(|inner| format_inner_ref(machine, view, &env, &inner))
                    .unwrap_or_else(|| "<string>".to_string()),
                Ok(DataConstructor::BoxedSymbol) => args
                    .get(0)
                    .and_then(|inner| format_inner_ref(machine, view, &env, &inner))
                    .unwrap_or_else(|| "<symbol>".to_string()),
                Ok(DataConstructor::BoxedZdt) => args
                    .get(0)
                    .and_then(|inner| format_inner_ref(machine, view, &env, &inner))
                    .unwrap_or_else(|| "<datetime>".to_string()),
                Ok(DataConstructor::IoReturn) => "<io-return>".to_string(),
                Ok(DataConstructor::IoBind) => "<io-bind>".to_string(),
                Ok(DataConstructor::IoAction) => "<io-action>".to_string(),
                Ok(DataConstructor::IoFail) => "<io-fail>".to_string(),
                Err(_) => "<value>".to_string(),
            }
        }
        HeapSyn::Atom {
            evaluand: Ref::V(n),
        } => format_native(n, view, machine),
        _ => "<unevaluated>".to_string(),
    }
}

/// Format an inner ref (arg of a boxed constructor) as a string.
///
/// Handles `Ref::V` directly and resolves `Ref::L` through the given
/// env frame.
fn format_inner_ref(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    env: &crate::eval::machine::env::EnvFrame,
    r: &Ref,
) -> Option<String> {
    let native = match r {
        Ref::V(n) => n.clone(),
        Ref::L(idx) => {
            let closure = env.get(&view, *idx)?;
            let code = view.scoped(closure.code());
            match &*code {
                HeapSyn::Atom {
                    evaluand: Ref::V(n),
                } => n.clone(),
                _ => return None,
            }
        }
        _ => return None,
    };
    Some(format_native(&native, view, machine))
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
