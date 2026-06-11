//! Debug representation and tracing intrinsics.
//!
//! Provides:
//! - `__DBG_REPR(value)` — render any eucalypt value to a compact, human-readable string
//! - `__DBG(label, value)` — print value to stderr and return it transparently
//! - `__TRACE_ENTRY(name, args_list, strict)` — print entry trace to stderr
//! - `__TRACE_EXIT(name, value, strict)` — print exit trace to stderr, return value

use std::convert::TryInto;

use crate::eval::{
    emit::Emitter,
    error::ExecutionError,
    machine::{
        env::SynClosure,
        intrinsic::{CallGlobal1, CallGlobal3, CallGlobal4, IntrinsicMachine, StgIntrinsic},
    },
    memory::{
        mutator::MutatorHeapView,
        syntax::{HeapSyn, Native, Ref},
    },
    stg::support::{machine_return_str, str_arg},
};

use crate::common::sourcemap::Smid;

use super::tags::DataConstructor;

/// Render a eucalypt value from a resolved closure to a compact debug string.
///
/// Inner helper shared by `render_debug_repr` (takes `&Ref`) and the tracing
/// intrinsics (which work with resolved `SynClosure` objects).
fn render_debug_repr_closure(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    closure: SynClosure,
) -> String {
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
                Ok(DataConstructor::Clause) => "<clause>".to_string(),
                Err(_) => "<value>".to_string(),
            }
        }
        HeapSyn::Atom {
            evaluand: Ref::V(n),
        } => render_native(n, view, machine),
        _ => "<unevaluated>".to_string(),
    }
}

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
    match machine.nav(view).resolve(r) {
        Ok(closure) => render_debug_repr_closure(machine, view, closure),
        Err(_) => "<error>".to_string(),
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
        Native::Vec(ptr) => {
            let v = view.scoped(*ptr);
            format!("<vec({})>", v.len())
        }
        Native::Prng(state) => format!("<prng({state})>"),
        Native::Producer(handle) => format!("<producer({handle})>"),
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

/// `__DBG(label, rendered, value)` — print debug output to stderr and return
/// `value` transparently.
///
/// - `label`: a string label (may be empty).
/// - `rendered`: a pre-rendered string representation of the value
///   (produced by the caller via `render-as`).
/// - `value`: the original value to return unchanged.
///
/// Output format:
/// - With empty label: `▶ <rendered>`
/// - With label: `▶ label: <rendered>`
///
/// **Important**: wrap the call in a let-binding (block field) to ensure
/// the Update continuation memoises the result — otherwise the BIF fires
/// on every force.
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
        // args[1] = rendered representation (strict string)
        // args[2] = original value (strict — forced to WHNF)
        let label = str_arg(machine, view, &args[0])?;
        let rendered = str_arg(machine, view, &args[1])?;

        let trimmed = rendered.trim();
        if label.is_empty() {
            eprintln!("▶ {trimmed}");
        } else {
            eprintln!("▶ {label}: {trimmed}");
        }

        // Return args[2] (original value) transparently
        let closure = machine.nav(view).resolve(&args[2])?;
        machine.set_closure(closure)
    }
}

impl CallGlobal3 for Dbg {}

/// Render a value without forcing evaluation (non-strict peek).
///
/// - Native scalars (numbers, strings, symbols, booleans, null) are rendered
///   using the same format as `render_debug_repr`.
/// - Data constructors that represent structured types show their outer
///   shape without recursing into contents: `[...]` for non-empty lists,
///   `{...}` for blocks.
/// - Anything unevaluated (thunks, applications, case expressions, etc.)
///   renders as `<thunk>`.
pub fn peek_debug_repr(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    r: &Ref,
) -> String {
    match machine.nav(view).resolve(r) {
        Ok(closure) => peek_debug_repr_closure(machine, view, closure),
        Err(_) => "<error>".to_string(),
    }
}

/// Inner helper for `peek_debug_repr` that works on an already-resolved closure.
fn peek_debug_repr_closure(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    closure: SynClosure,
) -> String {
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
                Ok(DataConstructor::ListCons) => "[...]".to_string(),
                Ok(DataConstructor::Block)
                | Ok(DataConstructor::BlockPair)
                | Ok(DataConstructor::BlockKvList) => "{...}".to_string(),
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
                _ => "<value>".to_string(),
            }
        }
        HeapSyn::Atom {
            evaluand: Ref::V(n),
        } => render_native(n, view, machine),
        _ => "<thunk>".to_string(),
    }
}

/// Resolve a strict bool argument to a Rust `bool`.
///
/// Since the arg must be declared strict in the intrinsic catalogue, it will
/// have been evaluated to WHNF (a `BoolTrue` or `BoolFalse` constructor)
/// before `execute` runs.
fn resolve_bool(machine: &dyn IntrinsicMachine, view: MutatorHeapView<'_>, r: &Ref) -> bool {
    let closure = match machine.nav(view).resolve(r) {
        Ok(c) => c,
        Err(_) => return false,
    };
    let code = view.scoped(closure.code());
    match &*code {
        HeapSyn::Cons { tag, .. } => {
            let dc: Result<DataConstructor, _> = (*tag).try_into();
            matches!(dc, Ok(DataConstructor::BoolTrue))
        }
        _ => false,
    }
}

/// Extract a string value from a closure representing a string literal.
///
/// Handles both unboxed atoms (`Atom { Ref::V(Native::Str(…)) }`) and
/// boxed string constructors (`Cons(BoxedString, [inner_ref])`).
/// Returns `None` if the closure does not hold a string.
fn extract_str_from_closure(view: MutatorHeapView<'_>, closure: &SynClosure) -> Option<String> {
    let code = view.scoped(closure.code());
    let env = view.scoped(closure.env());

    match &*code {
        HeapSyn::Atom {
            evaluand: Ref::V(Native::Str(s)),
        } => Some((*view.scoped(*s)).as_str().to_string()),
        HeapSyn::Cons { tag, args } => {
            let dc: Result<DataConstructor, _> = DataConstructor::try_from(*tag);
            match dc {
                Ok(DataConstructor::BoxedString) => {
                    let inner = args.get(0)?;
                    let native = extract_native(view, &env, &inner)?;
                    if let Native::Str(s) = native {
                        Some((*view.scoped(s)).as_str().to_string())
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// Traverse a cons-list to the element at `index`, returning its closure.
///
/// The list is always re-resolved from `list_ref` (a stable `Ref` into the
/// machine's env frame), so this is safe to call after a `evaluate_to_whnf`
/// call that may have triggered GC.
///
/// Returns an error if the list is shorter than `index + 1` elements or if
/// the structure is malformed.
fn get_list_element_at(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    list_ref: &Ref,
    index: usize,
) -> Result<SynClosure, ExecutionError> {
    let mut current = machine.nav(view).resolve(list_ref)?;

    for _ in 0..index {
        let code = view.scoped(current.code());
        match &*code {
            HeapSyn::Cons { tag, args } => {
                let dc: Result<DataConstructor, _> = (*tag).try_into();
                match dc {
                    Ok(DataConstructor::ListCons) => {
                        let t_ref = args.get(1).ok_or_else(|| {
                            ExecutionError::Panic(
                                Smid::default(),
                                "malformed TRACE args_list cons cell (missing tail)".to_string(),
                            )
                        })?;
                        current = machine
                            .nav(view)
                            .resolve_in_closure(&current, t_ref.clone())
                            .ok_or_else(|| {
                                ExecutionError::Panic(
                                    Smid::default(),
                                    "TRACE args_list: invalid tail ref in cons cell".to_string(),
                                )
                            })?;
                    }
                    Ok(DataConstructor::ListNil) => {
                        return Err(ExecutionError::Panic(
                            Smid::default(),
                            format!("TRACE args_list too short: expected element at index {index}"),
                        ))
                    }
                    _ => {
                        return Err(ExecutionError::Panic(
                            Smid::default(),
                            "TRACE args_list: unexpected data constructor".to_string(),
                        ))
                    }
                }
            }
            _ => {
                return Err(ExecutionError::Panic(
                    Smid::default(),
                    "TRACE args_list: expected cons cell, found unevaluated closure".to_string(),
                ))
            }
        }
    }

    // Extract the head at the current position.
    let code = view.scoped(current.code());
    match &*code {
        HeapSyn::Cons { tag, args } => {
            let dc: Result<DataConstructor, _> = (*tag).try_into();
            match dc {
                Ok(DataConstructor::ListCons) => {
                    let h_ref = args.get(0).ok_or_else(|| {
                        ExecutionError::Panic(
                            Smid::default(),
                            "malformed TRACE args_list cons cell (missing head)".to_string(),
                        )
                    })?;
                    machine
                        .nav(view)
                        .resolve_in_closure(&current, h_ref.clone())
                        .ok_or_else(|| {
                            ExecutionError::Panic(
                                Smid::default(),
                                "TRACE args_list: invalid head ref in cons cell".to_string(),
                            )
                        })
                }
                Ok(DataConstructor::ListNil) => Err(ExecutionError::Panic(
                    Smid::default(),
                    format!("TRACE args_list too short: expected element at index {index}"),
                )),
                _ => Err(ExecutionError::Panic(
                    Smid::default(),
                    "TRACE args_list: unexpected data constructor".to_string(),
                )),
            }
        }
        _ => Err(ExecutionError::Panic(
            Smid::default(),
            "TRACE args_list: expected cons cell, found unevaluated closure".to_string(),
        )),
    }
}

/// Count the number of elements in a cons-list without forcing any values.
fn count_list_elements(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    list_ref: &Ref,
) -> Result<usize, ExecutionError> {
    let mut count = 0usize;
    let mut current = machine.nav(view).resolve(list_ref)?;
    loop {
        let code = view.scoped(current.code());
        match &*code {
            HeapSyn::Cons { tag, args } => {
                let dc: Result<DataConstructor, _> = (*tag).try_into();
                match dc {
                    Ok(DataConstructor::ListNil) => break,
                    Ok(DataConstructor::ListCons) => {
                        count += 1;
                        let t_ref = args.get(1).ok_or_else(|| {
                            ExecutionError::Panic(
                                Smid::default(),
                                "malformed cons cell in TRACE args_list".to_string(),
                            )
                        })?;
                        current = machine
                            .nav(view)
                            .resolve_in_closure(&current, t_ref.clone())
                            .ok_or_else(|| {
                                ExecutionError::Panic(
                                    Smid::default(),
                                    "TRACE args_list: invalid tail ref in cons cell".to_string(),
                                )
                            })?;
                    }
                    _ => {
                        return Err(ExecutionError::Panic(
                            Smid::default(),
                            "unexpected data constructor in TRACE args_list".to_string(),
                        ))
                    }
                }
            }
            _ => {
                return Err(ExecutionError::Panic(
                    Smid::default(),
                    "expected cons cell in TRACE args_list, found unevaluated closure".to_string(),
                ))
            }
        }
    }
    Ok(count)
}

/// `TRACE_ENTRY(name, args_list, strict, body)` — print a function entry trace to
/// stderr, then return `body` transparently.
///
/// - `name`: string — the declaration name.
/// - `args_list`: cons-list alternating `[arg_name, value, arg_name, value, …]`.
/// - `strict`: bool — if `true`, force each value to WHNF before rendering;
///   if `false`, peek (render already-evaluated values, show `<thunk>` for the rest).
/// - `body`: the declaration body expression — returned unchanged.
///
/// Output format: `→ name(arg1: repr1, arg2: repr2)`
pub struct TraceEntry;

impl StgIntrinsic for TraceEntry {
    fn name(&self) -> &str {
        "TRACE_ENTRY"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args[0] = name (strict string)
        // args[1] = args_list (strict cons-list structure, values may be thunks)
        // args[2] = strict_bool (strict bool)
        // args[3] = body (lazy — returned transparently after printing)
        let name = str_arg(machine, view, &args[0])?;
        let strict = resolve_bool(machine, view, &args[2]);

        // Count pairs: the list has 2*n elements for n arguments.
        let n_elements = count_list_elements(machine, view, &args[1])?;
        let n_pairs = n_elements / 2;

        // Pass 1: collect names (no GC — no evaluate_to_whnf calls here).
        let mut arg_names: Vec<String> = Vec::with_capacity(n_pairs);
        for i in 0..n_pairs {
            let name_closure = get_list_element_at(machine, view, &args[1], 2 * i)?;
            let s = extract_str_from_closure(view, &name_closure)
                .unwrap_or_else(|| "<arg>".to_string());
            arg_names.push(s);
        }

        // Pass 2: render values (may trigger GC via evaluate_to_whnf in strict mode).
        // Each iteration re-traverses from args[1] to remain GC-safe.
        let mut arg_reprs: Vec<String> = Vec::with_capacity(n_pairs);
        for i in 0..n_pairs {
            let value_closure = get_list_element_at(machine, view, &args[1], 2 * i + 1)?;
            let repr = if strict {
                let forced = machine.evaluate_to_whnf(value_closure)?;
                render_debug_repr_closure(machine, view, forced)
            } else {
                peek_debug_repr_closure(machine, view, value_closure)
            };
            arg_reprs.push(repr);
        }

        // Format and print.
        let arg_strs: Vec<String> = arg_names
            .iter()
            .zip(arg_reprs.iter())
            .map(|(n, r)| format!("{n}: {r}"))
            .collect();
        eprintln!("\u{2192} {name}({})", arg_strs.join(", "));

        // Return the body unchanged.
        let body = machine.nav(view).resolve(&args[3])?;
        machine.set_closure(body)
    }
}

impl CallGlobal4 for TraceEntry {}

/// `__TRACE_EXIT(name, value, strict)` — print a function exit trace to stderr and
/// return `value` transparently.
///
/// - `name`: string — the declaration name.
/// - `value`: the body result (lazy — not forced by the wrapper).
/// - `strict`: bool — if `true`, force `value` to WHNF before rendering.
///
/// Output format: `← name: repr`
///
/// Returns `value` (or the forced closure in strict mode) unchanged.
pub struct TraceExit;

impl StgIntrinsic for TraceExit {
    fn name(&self) -> &str {
        "TRACE_EXIT"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args[0] = name (strict string)
        // args[1] = value (lazy — may be a thunk)
        // args[2] = strict_bool (strict bool)
        let name = str_arg(machine, view, &args[0])?;
        let strict = resolve_bool(machine, view, &args[2]);

        let result_closure = if strict {
            let value_closure = machine.nav(view).resolve(&args[1])?;
            let forced = machine.evaluate_to_whnf(value_closure)?;
            let repr = render_debug_repr_closure(machine, view, forced.clone());
            eprintln!("\u{2190} {name}: {repr}");
            forced
        } else {
            let repr = peek_debug_repr(machine, view, &args[1]);
            eprintln!("\u{2190} {name}: {repr}");
            machine.nav(view).resolve(&args[1])?
        };

        machine.set_closure(result_closure)
    }
}

impl CallGlobal3 for TraceExit {}
