//! Debug representation and tracing intrinsics.
//!
//! Provides:
//! - `__DBG_REPR(value)` — render any eucalypt value to a compact, human-readable string
//! - `__DBG(label, value)` — print value to stderr and return it transparently

use std::convert::TryInto;

use crate::eval::{
    emit::Emitter,
    error::ExecutionError,
    machine::intrinsic::{CallGlobal1, CallGlobal2, IntrinsicMachine, StgIntrinsic},
    memory::{
        mutator::MutatorHeapView,
        syntax::{HeapSyn, Native, Ref},
    },
    stg::support::{machine_return_str, str_arg},
};

use super::tags::DataConstructor;

/// Render a eucalypt value to a compact, human-readable debug string.
///
/// Scalars are rendered in their literal form:
/// - Numbers: `42`, `3.14`
/// - Strings: `"hello"` (with surrounding quotes)
/// - Symbols: `:foo`
/// - Booleans: `true`, `false`
/// - Null: `null`
///
/// Structured types produce a type label: `[list]`, `{block}`.
/// Unevaluated thunks produce `<unevaluated>`.
pub fn render_debug_repr(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    r: &Ref,
) -> String {
    let closure = match machine.nav(view).resolve(r) {
        Ok(c) => c,
        Err(_) => return "<error>".to_string(),
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
                Ok(DataConstructor::Block)
                | Ok(DataConstructor::BlockPair)
                | Ok(DataConstructor::BlockKvList) => "{block}".to_string(),
                Ok(DataConstructor::BoxedNumber) => args
                    .get(0)
                    .and_then(|inner| render_inner(machine, view, &env, &inner))
                    .unwrap_or_else(|| "<number>".to_string()),
                Ok(DataConstructor::BoxedString) => args
                    .get(0)
                    .and_then(|inner| render_inner_quoted(machine, view, &env, &inner))
                    .unwrap_or_else(|| "<string>".to_string()),
                Ok(DataConstructor::BoxedSymbol) => args
                    .get(0)
                    .and_then(|inner| render_inner_symbol(machine, view, &env, &inner))
                    .unwrap_or_else(|| "<symbol>".to_string()),
                Ok(DataConstructor::BoxedZdt) => args
                    .get(0)
                    .and_then(|inner| render_inner(machine, view, &env, &inner))
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
        } => render_native(n, view, machine),
        _ => "<unevaluated>".to_string(),
    }
}

/// Render a native value to a debug string.
fn render_native(n: &Native, view: MutatorHeapView<'_>, machine: &dyn IntrinsicMachine) -> String {
    match n {
        Native::Num(num) => num.to_string(),
        Native::Str(s) => {
            let scoped = view.scoped(*s);
            format!("{:?}", (*scoped).as_str())
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
    }
}

/// Render an inner argument ref (from a boxed constructor) as a debug string.
fn render_inner(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    env: &crate::eval::machine::env::EnvFrame,
    r: &Ref,
) -> Option<String> {
    let native = extract_native(view, env, r)?;
    Some(render_native(&native, view, machine))
}

/// Render an inner string arg with surrounding double quotes.
fn render_inner_quoted(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    env: &crate::eval::machine::env::EnvFrame,
    r: &Ref,
) -> Option<String> {
    let native = extract_native(view, env, r)?;
    match &native {
        Native::Str(s) => {
            let scoped = view.scoped(*s);
            Some(format!("{:?}", (*scoped).as_str()))
        }
        _ => Some(render_native(&native, view, machine)),
    }
}

/// Render an inner symbol arg with `:` prefix.
fn render_inner_symbol(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    env: &crate::eval::machine::env::EnvFrame,
    r: &Ref,
) -> Option<String> {
    let native = extract_native(view, env, r)?;
    match &native {
        Native::Sym(id) => Some(format!(":{}", machine.symbol_pool().resolve(*id))),
        _ => Some(render_native(&native, view, machine)),
    }
}

/// Extract a native value from a ref, following local environment lookups.
fn extract_native(
    view: MutatorHeapView<'_>,
    env: &crate::eval::machine::env::EnvFrame,
    r: &Ref,
) -> Option<Native> {
    match r {
        Ref::V(n) => Some(n.clone()),
        Ref::L(idx) => {
            let closure = env.get(&view, *idx)?;
            let code = view.scoped(closure.code());
            match &*code {
                HeapSyn::Atom {
                    evaluand: Ref::V(n),
                } => Some(n.clone()),
                _ => None,
            }
        }
        _ => None,
    }
}

/// `__DBG_REPR(value)` — render any value to a compact debug string.
///
/// Used as the shared foundation by both debug tracing functions and
/// expectation failure messages. Returns a eucalypt string.
pub struct DbgRepr;

impl StgIntrinsic for DbgRepr {
    fn name(&self) -> &str {
        "DBG_REPR"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let repr = render_debug_repr(machine, view, &args[0]);
        machine_return_str(machine, view, repr)
    }
}

impl CallGlobal1 for DbgRepr {}

/// `__DBG(label, value)` — print debug output to stderr and return `value` transparently.
///
/// - `label`: a string label prepended to the output (may be empty).
/// - `value`: the value to inspect; returned unchanged.
///
/// Output format:
/// - With empty label: `▶ <repr>`
/// - With label: `▶ label: <repr>`
///
/// This is the underlying BIF for both the `dbg` prelude function and the
/// `▶` prefix operator.
pub struct Dbg;

impl StgIntrinsic for Dbg {
    fn name(&self) -> &str {
        "DBG"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args[0] = label (strict string)
        // args[1] = value to debug (strict — we want the evaluated result)
        let label = str_arg(machine, view, &args[0])?;
        let repr = render_debug_repr(machine, view, &args[1]);

        if label.is_empty() {
            eprintln!("▶ {repr}");
        } else {
            eprintln!("▶ {label}: {repr}");
        }

        // Return args[1] transparently
        let closure = machine.nav(view).resolve(&args[1])?;
        machine.set_closure(closure)
    }
}

impl CallGlobal2 for Dbg {}
