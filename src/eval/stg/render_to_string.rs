//! RENDER_TO_STRING intrinsic — serialises a eucalypt value to a string
//!
//! Takes two strict arguments: (value, format_sym), where format_sym is a
//! symbol such as `:yaml`, `:json`, `:toml`, `:text`, `:edn`, or `:html`.
//!
//! The value must already be at WHNF (it is declared strict so the wrapper
//! ensures this).  The intrinsic traverses the heap tree directly in Rust,
//! emitting events to a `Vec<u8>` buffer via the existing `Emitter` trait,
//! and returns the resulting UTF-8 string as a `Native::Str`.
//!
//! This avoids any recursive machine re-entry: because the value is forced
//! before the BIF executes we can walk the heap without needing to run the
//! STG machine again.

use std::convert::TryInto;

use crate::{
    eval::{
        emit::{Emitter, RenderMetadata},
        error::ExecutionError,
        machine::{
            env::{EnvFrame, SynClosure},
            intrinsic::{CallGlobal2, IntrinsicMachine, StgIntrinsic},
        },
        memory::{
            alloc::ScopedAllocator,
            array::Array,
            mutator::MutatorHeapView,
            ndarray::HeapNdArray,
            set::Primitive as SetPrimitive,
            symbol::SymbolPool,
            syntax::{HeapSyn, Native, Ref, RefPtr},
        },
        primitive::Primitive,
    },
    export,
};

use super::{
    support::{machine_return_str, sym_arg},
    tags::DataConstructor,
};

/// Resolve a constructor arg `Ref` to a `SynClosure` relative to `closure`'s
/// environment.
///
/// Constructor args in the STG heap can be:
/// - `Ref::L(i)` — a local environment slot (the common case).
/// - `Ref::V(n)` — an inline native value; wrap it in a heap `Atom`.
/// - `Ref::G(i)` — a global; fall back to an `Atom` wrapping the ref.
///
/// The `navigate_local` method on `SynClosure` only handles `Ref::L` and
/// panics for other cases.  This helper handles all three variants so that
/// render traversal does not crash on blocks with small literal values.
fn resolve_cons_arg(
    closure: &SynClosure,
    view: &MutatorHeapView<'_>,
    r: Ref,
    root_env: RefPtr<EnvFrame>,
) -> Result<SynClosure, ExecutionError> {
    match r {
        Ref::L(i) => {
            let env = view.scoped(closure.env());
            (*env)
                .get(view, i)
                .ok_or(ExecutionError::BadEnvironmentIndex(i))
        }
        Ref::V(_) | Ref::G(_) => {
            // Wrap the inline/global ref in a heap Atom so the caller can
            // render it via the normal closure traversal path.
            let atom = view.alloc(HeapSyn::Atom { evaluand: r })?.as_ptr();
            Ok(SynClosure::new(atom, root_env))
        }
    }
}

/// Convert a set primitive to a render primitive
fn set_primitive_to_render_primitive(prim: &SetPrimitive, pool: &SymbolPool) -> Primitive {
    match prim {
        SetPrimitive::Num(n) => {
            let num = serde_json::Number::from_f64(n.into_inner())
                .unwrap_or_else(|| serde_json::Number::from(0));
            Primitive::Num(num)
        }
        SetPrimitive::Str(s) => Primitive::Str(s.clone()),
        SetPrimitive::Sym(id) => Primitive::Sym(pool.resolve(*id).to_string()),
    }
}

/// Render a native value to the emitter
fn render_native(
    native: &Native,
    pool: &SymbolPool,
    view: MutatorHeapView<'_>,
    emitter: &mut dyn Emitter,
) {
    match native {
        Native::Num(n) => {
            emitter.scalar(&RenderMetadata::empty(), &Primitive::Num(n.clone()));
        }
        Native::Str(s) => {
            let text = (*view.scoped(*s)).as_str().to_string();
            emitter.scalar(&RenderMetadata::empty(), &Primitive::Str(text));
        }
        Native::Sym(id) => {
            let sym = pool.resolve(*id).to_string();
            emitter.scalar(&RenderMetadata::empty(), &Primitive::Sym(sym));
        }
        Native::Zdt(dt) => {
            emitter.scalar(&RenderMetadata::empty(), &Primitive::ZonedDateTime(*dt));
        }
        Native::Set(ptr) => {
            let set = view.scoped(*ptr);
            emitter.sequence_start(&RenderMetadata::empty());
            for elem in set.sorted_elements() {
                let prim = set_primitive_to_render_primitive(elem, pool);
                emitter.scalar(&RenderMetadata::empty(), &prim);
            }
            emitter.sequence_end();
        }
        Native::NdArray(ptr) => {
            let arr = view.scoped(*ptr);
            render_ndarray_data(emitter, &arr, &RenderMetadata::empty());
        }
        Native::Index(_) => {
            // Block index — not directly renderable; emit null
            emitter.scalar(&RenderMetadata::empty(), &Primitive::Null);
        }
    }
}

/// Render an n-dimensional array as nested sequences (mirrors emit.rs logic)
fn render_ndarray_data(emitter: &mut dyn Emitter, arr: &HeapNdArray, metadata: &RenderMetadata) {
    let rank = arr.rank();
    if rank == 0 {
        let val = arr.get(&[]).unwrap_or(0.0);
        let num = serde_json::Number::from_f64(val).unwrap_or_else(|| serde_json::Number::from(0));
        emitter.scalar(metadata, &Primitive::Num(num));
    } else if rank == 1 {
        emitter.sequence_start(metadata);
        let len = arr.shape()[0];
        for i in 0..len {
            let val = arr.get(&[i]).unwrap_or(0.0);
            let num =
                serde_json::Number::from_f64(val).unwrap_or_else(|| serde_json::Number::from(0));
            emitter.scalar(&RenderMetadata::empty(), &Primitive::Num(num));
        }
        emitter.sequence_end();
    } else {
        emitter.sequence_start(metadata);
        let rows = arr.shape()[0];
        for i in 0..rows {
            if let Some(sub) = arr.slice_along(0, i) {
                render_ndarray_data(emitter, &sub, &RenderMetadata::empty());
            }
        }
        emitter.sequence_end();
    }
}

/// Recursively render a closure's value to the emitter.
///
/// The closure should already be at WHNF.  This function mirrors the
/// logic of the STG `Render` wrapper but executes it entirely in Rust
/// by walking the heap directly.
///
/// `pool` provides symbol resolution; `root_env` is used when wrapping
/// inline `V` or `G` refs in fresh `Atom` closures.
///
/// # Recursion depth
///
/// This function recurses proportionally to the nesting depth of the
/// eucalypt value.  Very deeply nested structures may overflow the Rust
/// stack.  In practice eucalypt programs rarely produce nesting depths
/// that would be problematic.
pub(crate) fn render_closure_to_emitter(
    closure: SynClosure,
    pool: &SymbolPool,
    root_env: RefPtr<EnvFrame>,
    view: MutatorHeapView<'_>,
    emitter: &mut dyn Emitter,
) -> Result<(), ExecutionError> {
    let code = view.scoped(closure.code());
    let env = view.scoped(closure.env());

    match &*code {
        HeapSyn::Atom { evaluand } => match evaluand {
            Ref::V(native) => {
                render_native(native, pool, view, emitter);
                Ok(())
            }
            Ref::L(i) => {
                let inner = (*env)
                    .get(&view, *i)
                    .ok_or(ExecutionError::BadEnvironmentIndex(*i))?;
                render_closure_to_emitter(inner, pool, root_env, view, emitter)
            }
            Ref::G(_) => {
                // Global refs are wrappers (lambda forms) — not directly
                // renderable as data values.
                emitter.scalar(&RenderMetadata::empty(), &Primitive::Null);
                Ok(())
            }
        },
        HeapSyn::Cons { tag, args } => {
            let dc: Result<DataConstructor, _> = (*tag).try_into();
            match dc {
                Ok(DataConstructor::Unit) => {
                    emitter.scalar(&RenderMetadata::empty(), &Primitive::Null);
                    Ok(())
                }
                Ok(DataConstructor::BoolTrue) => {
                    emitter.scalar(&RenderMetadata::empty(), &Primitive::Bool(true));
                    Ok(())
                }
                Ok(DataConstructor::BoolFalse) => {
                    emitter.scalar(&RenderMetadata::empty(), &Primitive::Bool(false));
                    Ok(())
                }
                Ok(DataConstructor::BoxedNumber)
                | Ok(DataConstructor::BoxedString)
                | Ok(DataConstructor::BoxedSymbol)
                | Ok(DataConstructor::BoxedZdt) => {
                    // Boxed scalar: the inner value is in args[0]
                    let inner_ref = args
                        .get(0)
                        .ok_or_else(|| ExecutionError::Panic("empty boxed value".to_string()))?;
                    let inner = resolve_cons_arg(&closure, &view, inner_ref, root_env)?;
                    render_closure_to_emitter(inner, pool, root_env, view, emitter)
                }
                Ok(DataConstructor::ListCons) => {
                    // List: emit a sequence by traversing the cons chain
                    emitter.sequence_start(&RenderMetadata::empty());
                    render_list_from_cons(closure.clone(), args, pool, root_env, view, emitter)?;
                    emitter.sequence_end();
                    Ok(())
                }
                Ok(DataConstructor::ListNil) => {
                    emitter.sequence_start(&RenderMetadata::empty());
                    emitter.sequence_end();
                    Ok(())
                }
                Ok(DataConstructor::Block) => {
                    // Block: args[0] is the items list
                    let items_ref = args
                        .get(0)
                        .ok_or_else(|| ExecutionError::Panic("block missing items".to_string()))?;
                    let items_closure = resolve_cons_arg(&closure, &view, items_ref, root_env)?;
                    emitter.block_start(&RenderMetadata::empty());
                    render_block_items(items_closure, pool, root_env, view, emitter)?;
                    emitter.block_end();
                    Ok(())
                }
                Ok(DataConstructor::BlockPair) => {
                    // Standalone BlockPair: render as a single-entry block
                    emitter.block_start(&RenderMetadata::empty());
                    render_block_pair_args(&closure, args, pool, root_env, view, emitter)?;
                    emitter.block_end();
                    Ok(())
                }
                Ok(DataConstructor::BlockKvList) => {
                    // BlockKvList: treat the enclosed cons ref as a list
                    let inner_ref = args.get(0).ok_or_else(|| {
                        ExecutionError::Panic("block-kv-list missing cons".to_string())
                    })?;
                    let inner = resolve_cons_arg(&closure, &view, inner_ref, root_env)?;
                    render_list_items_raw(inner, pool, root_env, view, emitter)
                }
                _ => {
                    // Unknown / IO constructors — emit null
                    emitter.scalar(&RenderMetadata::empty(), &Primitive::Null);
                    Ok(())
                }
            }
        }
        _ => {
            // Other HeapSyn nodes (Let, App, etc.) — value was not fully
            // evaluated.  Emit null and continue.
            emitter.scalar(&RenderMetadata::empty(), &Primitive::Null);
            Ok(())
        }
    }
}

/// Traverse a `ListCons` chain and render each item (without emitting
/// the surrounding sequence delimiters).
///
/// `current` is the `ListCons` closure, `initial_args` are its args.
fn render_list_from_cons(
    mut current: SynClosure,
    initial_args: &Array<Ref>,
    pool: &SymbolPool,
    root_env: RefPtr<EnvFrame>,
    view: MutatorHeapView<'_>,
    emitter: &mut dyn Emitter,
) -> Result<(), ExecutionError> {
    // Process the first cons cell whose args we already have
    let head_ref = initial_args
        .get(0)
        .ok_or_else(|| ExecutionError::Panic("malformed list cons (no head)".to_string()))?;
    let tail_ref = initial_args
        .get(1)
        .ok_or_else(|| ExecutionError::Panic("malformed list cons (no tail)".to_string()))?;

    let head = resolve_cons_arg(&current, &view, head_ref, root_env)?;
    render_closure_to_emitter(head, pool, root_env, view, emitter)?;

    // Walk the remaining tail without recursion
    let mut tail = resolve_cons_arg(&current, &view, tail_ref, root_env)?;

    loop {
        let tail_code = view.scoped(tail.code());
        match &*tail_code {
            HeapSyn::Cons { tag, args } => match (*tag).try_into() {
                Ok(DataConstructor::ListNil) => break,
                Ok(DataConstructor::ListCons) => {
                    let h_ref = args
                        .get(0)
                        .ok_or_else(|| ExecutionError::Panic("malformed list cons".to_string()))?;
                    let t_ref = args
                        .get(1)
                        .ok_or_else(|| ExecutionError::Panic("malformed list cons".to_string()))?;
                    let head = resolve_cons_arg(&tail, &view, h_ref, root_env)?;
                    render_closure_to_emitter(head, pool, root_env, view, emitter)?;
                    let next_tail = resolve_cons_arg(&tail, &view, t_ref, root_env)?;
                    current = tail;
                    tail = next_tail;
                    let _ = current; // keep borrow checker happy
                }
                _ => break,
            },
            _ => break,
        }
    }

    Ok(())
}

/// Render list items without sequence delimiters (used for BlockKvList).
fn render_list_items_raw(
    mut current: SynClosure,
    pool: &SymbolPool,
    root_env: RefPtr<EnvFrame>,
    view: MutatorHeapView<'_>,
    emitter: &mut dyn Emitter,
) -> Result<(), ExecutionError> {
    loop {
        let code = view.scoped(current.code());
        match &*code {
            HeapSyn::Cons { tag, args } => match (*tag).try_into() {
                Ok(DataConstructor::ListNil) => break,
                Ok(DataConstructor::ListCons) => {
                    let h_ref = args
                        .get(0)
                        .ok_or_else(|| ExecutionError::Panic("malformed list cons".to_string()))?;
                    let t_ref = args
                        .get(1)
                        .ok_or_else(|| ExecutionError::Panic("malformed list cons".to_string()))?;
                    let head = resolve_cons_arg(&current, &view, h_ref, root_env)?;
                    render_closure_to_emitter(head, pool, root_env, view, emitter)?;
                    current = resolve_cons_arg(&current, &view, t_ref, root_env)?;
                }
                _ => break,
            },
            _ => break,
        }
    }
    Ok(())
}

/// Traverse a list of block items (BlockPair or BlockKvList) and render each.
fn render_block_items(
    mut current: SynClosure,
    pool: &SymbolPool,
    root_env: RefPtr<EnvFrame>,
    view: MutatorHeapView<'_>,
    emitter: &mut dyn Emitter,
) -> Result<(), ExecutionError> {
    loop {
        let code = view.scoped(current.code());
        match &*code {
            HeapSyn::Cons { tag, args } => match (*tag).try_into() {
                Ok(DataConstructor::ListNil) => break,
                Ok(DataConstructor::ListCons) => {
                    let h_ref = args.get(0).ok_or_else(|| {
                        ExecutionError::Panic("malformed block items list (no head)".to_string())
                    })?;
                    let t_ref = args.get(1).ok_or_else(|| {
                        ExecutionError::Panic("malformed block items list (no tail)".to_string())
                    })?;

                    let head = resolve_cons_arg(&current, &view, h_ref, root_env)?;
                    let head_code = view.scoped(head.code());

                    if let HeapSyn::Cons {
                        tag: item_tag,
                        args: item_args,
                    } = &*head_code
                    {
                        if *item_tag == DataConstructor::BlockPair.tag() {
                            render_block_pair_args(
                                &head, item_args, pool, root_env, view, emitter,
                            )?;
                        } else if *item_tag == DataConstructor::BlockKvList.tag() {
                            // BlockKvList nested in a block: render its items inline
                            let inner_ref = item_args.get(0).ok_or_else(|| {
                                ExecutionError::Panic("empty block-kv-list".to_string())
                            })?;
                            let inner = resolve_cons_arg(&head, &view, inner_ref, root_env)?;
                            render_list_items_raw(inner, pool, root_env, view, emitter)?;
                        }
                        // Other tags: skip
                    }

                    current = resolve_cons_arg(&current, &view, t_ref, root_env)?;
                }
                _ => break,
            },
            _ => break,
        }
    }
    Ok(())
}

/// Render a single BlockPair's key and value to the emitter.
///
/// `closure` is the `BlockPair` constructor closure.
/// `args` are its constructor args: [key_ref, val_ref].
fn render_block_pair_args(
    closure: &SynClosure,
    args: &Array<Ref>,
    pool: &SymbolPool,
    root_env: RefPtr<EnvFrame>,
    view: MutatorHeapView<'_>,
    emitter: &mut dyn Emitter,
) -> Result<(), ExecutionError> {
    let key_ref = args
        .get(0)
        .ok_or_else(|| ExecutionError::Panic("block pair missing key".to_string()))?;
    let val_ref = args
        .get(1)
        .ok_or_else(|| ExecutionError::Panic("block pair missing value".to_string()))?;

    // The key is an unboxed symbol (or string) ref
    let key_native = closure.navigate_local_native(&view, key_ref);
    let key_str = match key_native {
        Native::Sym(id) => pool.resolve(id).to_string(),
        Native::Str(s) => (*view.scoped(s)).as_str().to_string(),
        _ => "<key>".to_string(),
    };

    // Emit key as a symbol scalar (the emitters interpret this as a mapping key)
    emitter.scalar(&RenderMetadata::empty(), &Primitive::Sym(key_str));

    // Emit value
    let val = resolve_cons_arg(closure, &view, val_ref, root_env)?;
    render_closure_to_emitter(val, pool, root_env, view, emitter)
}

/// RENDER_TO_STRING(value, format_sym) → Str
///
/// Serialises `value` to a string using the specified format.
/// Recognised formats: `yaml`, `json`, `toml`, `text`, `edn`, `html`.
///
/// Both arguments are strict: the value must already be at WHNF and the
/// format symbol must be a resolved native symbol.
pub struct RenderToString;

impl StgIntrinsic for RenderToString {
    fn name(&self) -> &str {
        "RENDER_TO_STRING"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args[0] = value (strict — already at WHNF)
        // args[1] = format_sym (strict — already resolved to a native sym)
        let format_name = sym_arg(machine, view, &args[1])?;

        // Resolve args[0] to a closure for traversal
        let value_closure = machine.nav(view).resolve(&args[0])?;

        // Extract pool and root_env before entering the render traversal
        let pool = machine.symbol_pool().clone();
        let root_env = machine.root_env();

        // Capture output into a Vec<u8> buffer
        let mut buffer: Vec<u8> = Vec::new();
        let mut string_emitter =
            export::create_emitter(&format_name, &mut buffer).ok_or_else(|| {
                ExecutionError::Panic(format!("unknown render format: {format_name}"))
            })?;

        // Emit stream/document wrapper
        string_emitter.stream_start();
        string_emitter.doc_start();

        render_closure_to_emitter(
            value_closure,
            &pool,
            root_env,
            view,
            string_emitter.as_mut(),
        )?;

        string_emitter.doc_end();
        string_emitter.stream_end();

        // Drop the emitter so the mutable borrow on `buffer` ends
        drop(string_emitter);

        let result = String::from_utf8(buffer)
            .map_err(|e| ExecutionError::Panic(format!("render output is not valid UTF-8: {e}")))?;

        machine_return_str(machine, view, result)
    }
}

impl CallGlobal2 for RenderToString {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::stg::{
        block, boolean, emit as stg_emit, eq,
        panic::Panic,
        render::{Render, RenderBlockItems, RenderItems, RenderKv, Saturated, Suppresses, Tag},
        runtime::{self, Runtime},
        syntax::dsl::*,
        testing,
    };

    fn render_runtime() -> Box<dyn Runtime> {
        testing::runtime(vec![
            Box::new(stg_emit::Emit0),
            Box::new(stg_emit::EmitT),
            Box::new(stg_emit::EmitF),
            Box::new(stg_emit::EmitNative),
            Box::new(stg_emit::EmitTagNative),
            Box::new(stg_emit::EmitSeqStart),
            Box::new(stg_emit::EmitTagSeqStart),
            Box::new(stg_emit::EmitSeqEnd),
            Box::new(stg_emit::EmitBlockStart),
            Box::new(stg_emit::EmitTagBlockStart),
            Box::new(stg_emit::EmitBlockEnd),
            Box::new(stg_emit::EmitDocStart),
            Box::new(stg_emit::EmitDocEnd),
            Box::new(Render),
            Box::new(RenderItems),
            Box::new(RenderBlockItems),
            Box::new(RenderKv),
            Box::new(RenderToString),
            Box::new(Saturated),
            Box::new(Suppresses),
            Box::new(Tag),
            Box::new(block::Block),
            Box::new(block::Kv),
            Box::new(block::LookupOr(runtime::NativeVariant::Unboxed)),
            Box::new(block::MatchesKey),
            Box::new(block::ExtractValue),
            Box::new(eq::Eq),
            Box::new(Panic),
            Box::new(boolean::And),
            Box::new(boolean::Not),
            Box::new(boolean::True),
            Box::new(boolean::False),
        ])
    }

    #[test]
    fn test_render_to_string_yaml_number() {
        // render-as(42, :yaml) should produce a YAML string containing "42".
        // The wrapper boxes the result as BoxedString, so we verify the machine
        // terminates successfully and the result is a BoxedString constructor.
        let syntax = letrec_(
            vec![value(box_num(42)), value(box_sym("yaml"))],
            RenderToString.global(lref(0), lref(1)),
        );

        let rt = render_runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        // The wrapper boxes the returned string in BoxedString — verify via bool_return
        // absence (not a bool) and that the machine terminated without error.
        assert!(m.terminated(), "machine did not terminate");
        assert!(m.bool_return().is_none(), "unexpected bool return");
        assert!(!m.unit_return(), "unexpected unit return");
    }

    #[test]
    fn test_render_to_string_json_string() {
        // render-as("hello", :json) should produce a JSON string.
        // The wrapper boxes the result as BoxedString; we verify no panic occurs.
        let syntax = letrec_(
            vec![value(box_str("hello")), value(box_sym("json"))],
            RenderToString.global(lref(0), lref(1)),
        );

        let rt = render_runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        assert!(m.terminated(), "machine did not terminate");
        assert!(m.bool_return().is_none(), "unexpected bool return");
        assert!(!m.unit_return(), "unexpected unit return");
    }
}
