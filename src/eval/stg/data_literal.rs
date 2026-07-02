//! Materialise a data-literal Core expression directly as a runtime value,
//! via the neutral data-construction ABI (`native_value`/`data_value`/
//! `return_closure_list`/`meta_value` — BV1 §5.5), with zero runtime code
//! synthesis.
//!
//! `PARSE_STRING` (parsing untrusted input via the import readers in
//! `data_only` mode) and `TYPE_TO_DATA` (`typedata::type_to_rcexpr`) both
//! produce a self-contained `RcExpr` built only from data-literal shapes —
//! literals, lists, blocks, `DefaultBlockLet` wrappers and metadata — with
//! no free variables and no intrinsic/function application. That means the
//! value can be built directly rather than compiled to STG and `load()`ed
//! as runtime code, which would anchor it to `HeapSyn` and, on the bytecode
//! engine, require synthesising fresh code per call (the arena-growth
//! problem the plan explicitly rules out).
//!
//! Supported shapes: `Literal` (str/sym/num/bool/null/type-data), `List`,
//! `Block`, `Let` with `LetType::DefaultBlockLet` (as produced by the import
//! readers for ordinary blocks — every binding is itself var-free, so a
//! simple, non-recursive de Bruijn evaluation suffices), `Meta` (e.g. EDN
//! tagged elements / YAML custom tags), and the `Var::Bound` references a
//! `DefaultBlockLet`'s generated body makes into its own bindings.

use crate::{
    core::{
        binding::Var,
        expr::{Expr, LetType, Primitive, RcExpr},
    },
    eval::{
        error::ExecutionError,
        machine::intrinsic::{AbiClosure, IntrinsicMachine},
        memory::{
            mutator::MutatorHeapView,
            syntax::{Native, Ref, StgBuilder},
        },
        stg::{support::list_value, tags::DataConstructor},
    },
};

/// Materialise a data-literal `RcExpr` as a value handle, engine-neutrally.
pub fn build_value(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    expr: &RcExpr,
) -> Result<AbiClosure, ExecutionError> {
    build(machine, view, expr, &mut Vec::new())
}

/// `env` is a stack of `Let` scope frames (innermost last). A `Var::Bound`
/// reference's `scope` counts outward from the innermost frame and
/// `binder` indexes within that frame — the same de Bruijn scheme the STG
/// compiler itself uses, so no name-based substitution (and its shadowing
/// pitfalls) is needed.
fn build(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    expr: &RcExpr,
    env: &mut Vec<Vec<AbiClosure>>,
) -> Result<AbiClosure, ExecutionError> {
    match &*expr.inner {
        Expr::Literal(_, prim) => build_literal(machine, view, prim),
        Expr::List(_, items) => {
            let mut vals = Vec::with_capacity(items.len());
            for item in items {
                vals.push(build(machine, view, item, env)?);
            }
            list_value(machine, view, vals)
        }
        Expr::Block(_, block_map) => build_block(machine, view, block_map.iter(), env),
        Expr::Let(_, scope, LetType::DefaultBlockLet) => {
            let mut frame = Vec::with_capacity(scope.pattern.len());
            for binding in &scope.pattern {
                frame.push(build(machine, view, &binding.expr, env)?);
            }
            env.push(frame);
            let result = build(machine, view, &scope.body, env);
            env.pop();
            result
        }
        Expr::Meta(_, body, meta) => {
            let b = build(machine, view, body, env)?;
            let m = build(machine, view, meta, env)?;
            machine.meta_value(view, m, b)
        }
        Expr::Var(smid, Var::Bound(bv)) => {
            let frame_idx = env.len().checked_sub(1 + bv.scope as usize);
            let value = frame_idx.and_then(|i| env[i].get(bv.binder as usize));
            value.cloned().ok_or_else(|| {
                ExecutionError::Panic(*smid, "data literal: unbound variable".to_string())
            })
        }
        other => Err(ExecutionError::Panic(
            machine.annotation(),
            format!("data literal: unsupported core expression shape ({other:?})"),
        )),
    }
}

fn build_literal(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    prim: &Primitive,
) -> Result<AbiClosure, ExecutionError> {
    match prim {
        Primitive::Num(n) => {
            let native = machine.native_value(view, Native::Num(n.clone()))?;
            machine.data_value(view, DataConstructor::BoxedNumber.tag(), &[native])
        }
        Primitive::Str(s) => {
            let Ref::V(native) = view.str_ref(s)? else {
                unreachable!("str_ref yields a value ref")
            };
            let boxed = machine.native_value(view, native)?;
            machine.data_value(view, DataConstructor::BoxedString.tag(), &[boxed])
        }
        Primitive::Sym(s) => {
            let Ref::V(native) = view.sym_ref(machine.symbol_pool_mut(), s)? else {
                unreachable!("sym_ref yields a value ref")
            };
            let boxed = machine.native_value(view, native)?;
            machine.data_value(view, DataConstructor::BoxedSymbol.tag(), &[boxed])
        }
        Primitive::TypeData(s) => {
            let Ref::V(native) = view.str_ref(s)? else {
                unreachable!("str_ref yields a value ref")
            };
            let boxed = machine.native_value(view, native)?;
            machine.data_value(view, DataConstructor::BoxedTypeData.tag(), &[boxed])
        }
        Primitive::Bool(b) => {
            let tag = if *b {
                DataConstructor::BoolTrue
            } else {
                DataConstructor::BoolFalse
            };
            machine.data_value(view, tag.tag(), &[])
        }
        Primitive::Null => machine.data_value(view, DataConstructor::Unit.tag(), &[]),
    }
}

/// Build a `Block` value from `(key, value-expr)` pairs, in insertion order
/// — a `BlockPair` cons-list wrapped with the no-index sentinel, matching
/// `Compiler::compile_block` exactly (`stg/compiler.rs`).
fn build_block<'a>(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    entries: impl Iterator<Item = (&'a String, &'a RcExpr)>,
    env: &mut Vec<Vec<AbiClosure>>,
) -> Result<AbiClosure, ExecutionError> {
    let mut pairs = Vec::new();
    for (k, v) in entries {
        let value = build(machine, view, v, env)?;
        let Ref::V(sym_native) = view.sym_ref(machine.symbol_pool_mut(), k)? else {
            unreachable!("sym_ref yields a value ref")
        };
        let key = machine.native_value(view, sym_native)?;
        pairs.push(machine.data_value(view, DataConstructor::BlockPair.tag(), &[key, value])?);
    }
    let kv_list = list_value(machine, view, pairs)?;
    let no_index = machine.native_value(view, Native::Num(serde_json::Number::from(0)))?;
    machine.data_value(view, DataConstructor::Block.tag(), &[kv_list, no_index])
}
