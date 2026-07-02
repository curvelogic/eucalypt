//! Debug representation and tracing intrinsics.
//!
//! Provides:
//! - `__DBG_REPR(value)` — render any eucalypt value to a compact, human-readable string
//! - `__DBG(label, value)` — print value to stderr and return it transparently
//! - `__TRACE_ENTRY(name, args_list, strict)` — print entry trace to stderr
//! - `__TRACE_EXIT(name, value, strict)` — print exit trace to stderr, return value
//!
//! All value inspection here goes through the engine-neutral intrinsic ABI
//! (`resolve_closure`/`data_tag`/`data_field`/`value_native`/`force`), so these
//! intrinsics run identically on the HeapSyn and bytecode engines.

use std::convert::TryInto;

use crate::eval::{
    emit::Emitter,
    error::ExecutionError,
    machine::intrinsic::{
        AbiClosure, CallGlobal1, CallGlobal3, CallGlobal4, IntrinsicMachine, StgIntrinsic,
    },
    memory::{
        mutator::MutatorHeapView,
        syntax::{Native, Ref},
    },
    stg::support::{machine_return_str, str_arg},
};

use crate::common::sourcemap::Smid;

use super::tags::DataConstructor;

/// The `DataConstructor` of a resolved value handle, if it is a data
/// constructor in WHNF.
fn data_constructor(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    clo: &AbiClosure,
) -> Option<DataConstructor> {
    machine
        .data_tag(view, clo)
        .and_then(|tag| tag.try_into().ok())
}

/// The native payload of a boxed scalar's field 0.
///
/// With `force`, an unevaluated inner thunk (e.g. the string-concat thunk
/// inside a `BoxedString` produced by interpolation) is evaluated to WHNF
/// first; without it, such a thunk yields `None` (the non-forcing peek).
fn boxed_native(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    clo: &AbiClosure,
    force: bool,
) -> Option<Native> {
    let field = machine.data_field(view, clo, 0)?;
    let field = if force {
        machine.force(field).ok()?
    } else {
        field
    };
    machine.value_native(view, &field)
}

/// Render a boxed scalar's inner value, falling back to `placeholder` when the
/// inner native is not (yet) available.
fn boxed_scalar(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    clo: &AbiClosure,
    force: bool,
    placeholder: &str,
) -> String {
    match boxed_native(machine, view, clo, force) {
        Some(native) => render_native(&native, view, machine),
        None => placeholder.to_string(),
    }
}

/// Render a native value to a debug string.
///
/// Scalars are rendered in their literal form: numbers `42`/`3.14`, strings
/// `"hello"` (quoted), symbols `:foo`, datetimes as RFC-3339.
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

/// Render a resolved value handle to a compact debug string.
///
/// Scalars are rendered in literal form; structured types produce a type label
/// (`[list]`, `{block}`). Unevaluated values render as `<unevaluated>`. With
/// `force`, inner scalar thunks (e.g. an interpolated `BoxedString`) are
/// evaluated so the rendered output is accurate rather than `<string>`.
fn render_value(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    clo: &AbiClosure,
    force: bool,
) -> String {
    let Some(dc) = data_constructor(machine, view, clo) else {
        // Not a data constructor: a bare native atom renders as its literal,
        // anything else is still a thunk.
        return match machine.value_native(view, clo) {
            Some(native) => render_native(&native, view, machine),
            None => "<unevaluated>".to_string(),
        };
    };

    match dc {
        DataConstructor::BoolTrue => "true".to_string(),
        DataConstructor::BoolFalse => "false".to_string(),
        DataConstructor::Unit => "null".to_string(),
        DataConstructor::ListNil => "[]".to_string(),
        DataConstructor::ListCons => "[list]".to_string(),
        DataConstructor::Block | DataConstructor::BlockPair | DataConstructor::BlockKvList => {
            "{block}".to_string()
        }
        DataConstructor::BoxedNumber => boxed_scalar(machine, view, clo, force, "<number>"),
        DataConstructor::BoxedString => boxed_scalar(machine, view, clo, force, "<string>"),
        DataConstructor::BoxedSymbol => boxed_scalar(machine, view, clo, force, "<symbol>"),
        DataConstructor::BoxedZdt => boxed_scalar(machine, view, clo, force, "<datetime>"),
        DataConstructor::BoxedTypeData => boxed_scalar(machine, view, clo, force, "<type-data>"),
        DataConstructor::IoReturn => "<io-return>".to_string(),
        DataConstructor::IoBind => "<io-bind>".to_string(),
        DataConstructor::IoAction => "<io-action>".to_string(),
        DataConstructor::IoFail => "<io-fail>".to_string(),
        DataConstructor::Clause => "<clause>".to_string(),
    }
}

/// Render a eucalypt value to a compact, human-readable debug string.
///
/// Non-forcing: lazy inner scalars render as `<number>`/`<string>` etc.  Use
/// [`render_debug_repr_forced`] when inner thunks should be evaluated.
pub fn render_debug_repr(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    r: &Ref,
) -> String {
    match machine.resolve_closure(view, r) {
        Ok(clo) => render_value(machine, view, &clo, false),
        Err(_) => "<error>".to_string(),
    }
}

/// Render a eucalypt value to a compact debug string, forcing inner thunks.
///
/// The forcing variant of [`render_debug_repr`], suitable for intrinsic
/// `execute` methods.  It evaluates lazy inner refs (e.g. string-interpolation
/// results inside a `BoxedString`) via [`IntrinsicMachine::force`], so scalar
/// values render accurately rather than as `<string>`/`<number>`.
pub fn render_debug_repr_forced(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    r: &Ref,
) -> String {
    match machine.resolve_closure(view, r) {
        Ok(clo) => render_value(machine, view, &clo, true),
        Err(_) => "<error>".to_string(),
    }
}

/// Render a value without forcing evaluation (non-strict peek).
///
/// - Native scalars are rendered as by [`render_debug_repr`].
/// - Structured types show only their outer shape: `[...]` for non-empty lists,
///   `{...}` for blocks.
/// - Anything unevaluated renders as `<thunk>`.
fn peek_value(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    clo: &AbiClosure,
) -> String {
    let Some(dc) = data_constructor(machine, view, clo) else {
        return match machine.value_native(view, clo) {
            Some(native) => render_native(&native, view, machine),
            None => "<thunk>".to_string(),
        };
    };

    match dc {
        DataConstructor::BoolTrue => "true".to_string(),
        DataConstructor::BoolFalse => "false".to_string(),
        DataConstructor::Unit => "null".to_string(),
        DataConstructor::ListNil => "[]".to_string(),
        DataConstructor::ListCons => "[...]".to_string(),
        DataConstructor::Block | DataConstructor::BlockPair | DataConstructor::BlockKvList => {
            "{...}".to_string()
        }
        DataConstructor::BoxedNumber => boxed_scalar(machine, view, clo, false, "<number>"),
        DataConstructor::BoxedString => boxed_scalar(machine, view, clo, false, "<string>"),
        DataConstructor::BoxedSymbol => boxed_scalar(machine, view, clo, false, "<symbol>"),
        DataConstructor::BoxedZdt => boxed_scalar(machine, view, clo, false, "<datetime>"),
        _ => "<value>".to_string(),
    }
}

/// Render a value without forcing evaluation (non-strict peek).
pub fn peek_debug_repr(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    r: &Ref,
) -> String {
    match machine.resolve_closure(view, r) {
        Ok(clo) => peek_value(machine, view, &clo),
        Err(_) => "<error>".to_string(),
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
        let closure = machine.resolve_closure(view, &args[2])?;
        machine.set_result(closure)
    }
}

impl CallGlobal3 for Dbg {}

/// Resolve a strict bool argument to a Rust `bool`.
///
/// Since the arg must be declared strict in the intrinsic catalogue, it will
/// have been evaluated to WHNF (a `BoolTrue` or `BoolFalse` constructor)
/// before `execute` runs.
fn resolve_bool(machine: &mut dyn IntrinsicMachine, view: MutatorHeapView<'_>, r: &Ref) -> bool {
    match machine.resolve_closure(view, r) {
        Ok(clo) => matches!(
            data_constructor(machine, view, &clo),
            Some(DataConstructor::BoolTrue)
        ),
        Err(_) => false,
    }
}

/// Extract a string value from a resolved closure representing a string.
///
/// Handles both unboxed native strings and boxed `BoxedString` constructors
/// (both surface a `Native::Str` through `value_native`).  Returns `None` if
/// the closure does not hold a string.
fn extract_str(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    clo: &AbiClosure,
) -> Option<String> {
    match machine.value_native(view, clo)? {
        Native::Str(s) => Some((*view.scoped(s)).as_str().to_string()),
        _ => None,
    }
}

/// Traverse a cons-list to the element at `index`, returning its closure.
///
/// The list is always re-resolved from `list_ref` (a stable `Ref` into the
/// machine's env frame), so this is safe to call after a `force` call that may
/// have triggered GC — the caller re-invokes it each iteration rather than
/// holding resolved element closures across a force.
///
/// Returns an error if the list is shorter than `index + 1` elements or if
/// the structure is malformed.
fn get_list_element_at(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    list_ref: &Ref,
    index: usize,
) -> Result<AbiClosure, ExecutionError> {
    let malformed =
        |what: &str| ExecutionError::Panic(Smid::default(), format!("TRACE args_list: {what}"));
    let too_short = || {
        ExecutionError::Panic(
            Smid::default(),
            format!("TRACE args_list too short: expected element at index {index}"),
        )
    };

    let mut current = machine.resolve_closure(view, list_ref)?;
    for _ in 0..index {
        match data_constructor(machine, view, &current) {
            Some(DataConstructor::ListCons) => {
                current = machine
                    .data_field(view, &current, 1)
                    .ok_or_else(|| malformed("invalid tail ref in cons cell"))?;
            }
            Some(DataConstructor::ListNil) => return Err(too_short()),
            _ => return Err(malformed("expected cons cell")),
        }
    }

    match data_constructor(machine, view, &current) {
        Some(DataConstructor::ListCons) => machine
            .data_field(view, &current, 0)
            .ok_or_else(|| malformed("invalid head ref in cons cell")),
        Some(DataConstructor::ListNil) => Err(too_short()),
        _ => Err(malformed("expected cons cell")),
    }
}

/// Count the number of elements in a cons-list without forcing any values.
fn count_list_elements(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    list_ref: &Ref,
) -> Result<usize, ExecutionError> {
    let malformed = || {
        ExecutionError::Panic(
            Smid::default(),
            "malformed cons cell in TRACE args_list".to_string(),
        )
    };

    let mut count = 0usize;
    let mut current = machine.resolve_closure(view, list_ref)?;
    loop {
        match data_constructor(machine, view, &current) {
            Some(DataConstructor::ListNil) => break,
            Some(DataConstructor::ListCons) => {
                count += 1;
                current = machine
                    .data_field(view, &current, 1)
                    .ok_or_else(malformed)?;
            }
            _ => {
                return Err(ExecutionError::Panic(
                    Smid::default(),
                    "unexpected data constructor in TRACE args_list".to_string(),
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

        // Pass 1: collect names (no forcing — no GC here).
        let mut arg_names: Vec<String> = Vec::with_capacity(n_pairs);
        for i in 0..n_pairs {
            let name_closure = get_list_element_at(machine, view, &args[1], 2 * i)?;
            let s =
                extract_str(machine, view, &name_closure).unwrap_or_else(|| "<arg>".to_string());
            arg_names.push(s);
        }

        // Pass 2: render values (may trigger GC via `force` in strict mode).
        // Each iteration re-traverses from args[1] to remain GC-safe.
        let mut arg_reprs: Vec<String> = Vec::with_capacity(n_pairs);
        for i in 0..n_pairs {
            let value_closure = get_list_element_at(machine, view, &args[1], 2 * i + 1)?;
            let repr = if strict {
                let forced = machine.force(value_closure)?;
                render_value(machine, view, &forced, false)
            } else {
                peek_value(machine, view, &value_closure)
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
        let body = machine.resolve_closure(view, &args[3])?;
        machine.set_result(body)
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
            let value_closure = machine.resolve_closure(view, &args[1])?;
            let forced = machine.force(value_closure)?;
            let repr = render_value(machine, view, &forced, false);
            eprintln!("\u{2190} {name}: {repr}");
            forced
        } else {
            let repr = peek_debug_repr(machine, view, &args[1]);
            eprintln!("\u{2190} {name}: {repr}");
            machine.resolve_closure(view, &args[1])?
        };

        machine.set_result(result_closure)
    }
}

impl CallGlobal3 for TraceExit {}
