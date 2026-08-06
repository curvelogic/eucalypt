//! STG allocation benchmarks
//!
//! Each benchmark group gets its own fresh `Heap` so that GC pressure from one
//! group cannot contaminate the measurements of another.  Previously all
//! benchmarks shared a single heap: as `alloc_let` filled it, `alloc_letrec`
//! ran against a heap with higher occupancy, causing phantom regressions of
//! ±30–80% in criterion comparisons.
//!
//! Ported from the HeapSyn engine (`machine::env_builder::EnvBuilder`,
//! `SynClosure`, `EnvFrame`) to the bytecode engine (`BcEnvBuilder`,
//! `BcValue`, `BcEnvFrame`) by the Phase 4 collapse (eu-oufc), which deleted
//! HeapSyn. Bytecode natives carry no code to live in (unlike a HeapSyn
//! `Atom{V(native)}` node), so `native_one` needs no heap allocation at all —
//! an architectural difference from the original `box_one`, not a
//! benchmark-fidelity gap. Bytecode's frame-allocation primitive
//! (`from_saturation`) does not distinguish let from letrec the way
//! `HeapSyn::Let`/`HeapSyn::LetRec` did; `alloc_let`/`alloc_letrec` are kept
//! as separate benchmarks (preserving their criterion history) even though
//! they now measure the same underlying allocation.

use std::iter;

use eucalypt::{
    common::sourcemap::Smid,
    eval::{
        bytecode::{encode, partially_apply, BcClosure, BcEnvBuilder, BcEnvFrame, BcValue},
        memory::{
            alloc::ScopedAllocator, array::Array, heap::Heap, mutator::MutatorHeapView,
            symbol::SymbolPool, syntax::Native, syntax::RefPtr,
        },
        stg::syntax::dsl,
    },
};

use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;

/// A bare native WHNF value — no heap allocation, unlike the HeapSyn
/// `box_one` this replaces (see the module doc).
fn native_one() -> BcValue {
    BcValue::Native(Native::Num(1.into()))
}

/// An arbitrary, never-entered code offset usable as a closure's `code`
/// field in these allocation-only benchmarks (they never dispatch on it).
const DUMMY_CODE: u32 = 0;

fn fake_bindings(width: usize) -> Vec<BcValue> {
    iter::repeat_with(native_one).take(width).collect()
}

fn fake_env_stack(
    view: MutatorHeapView,
    empty: RefPtr<BcEnvFrame>,
    width: usize,
    height: usize,
) -> RefPtr<BcEnvFrame> {
    let mut base = empty;

    for _ in 0..height {
        let bindings = fake_bindings(width);
        base = view
            .from_values(bindings.into_iter(), width, base, Smid::default())
            .unwrap();
    }

    base
}

/// Allocate a let-shaped frame of native bindings
fn alloc_let(
    view: MutatorHeapView,
    empty: RefPtr<BcEnvFrame>,
    bindings: &[BcValue],
) -> RefPtr<BcEnvFrame> {
    view.from_saturation(Array::from_slice(&view, bindings), empty, Smid::default())
        .unwrap()
}

/// Allocate a letrec-shaped frame of native bindings
fn alloc_letrec(
    view: MutatorHeapView,
    empty: RefPtr<BcEnvFrame>,
    bindings: &[BcValue],
) -> RefPtr<BcEnvFrame> {
    view.from_saturation(Array::from_slice(&view, bindings), empty, Smid::default())
        .unwrap()
}

/// Access deep closure
fn access(view: MutatorHeapView, env: RefPtr<BcEnvFrame>, depth: usize) -> Option<BcValue> {
    let e = view.scoped(env);
    (*e).get(&view, depth)
}

/// Update deep closure with a new value
fn update(view: MutatorHeapView, env: RefPtr<BcEnvFrame>, depth: usize) {
    let e = view.scoped(env);
    (*e).update(&view, depth, native_one()).unwrap();
}

/// Create an identity lambda and saturate it
fn create_and_saturate_lambda(view: MutatorHeapView, empty: RefPtr<BcEnvFrame>) {
    let lambda = BcClosure::new_annotated_lambda(DUMMY_CODE, 1, empty, Smid::default());
    let args = [native_one()];
    view.saturate(&lambda, &args).unwrap();
}

/// Create a two-argument lambda, partially apply it, then saturate
fn create_partially_apply_and_saturate_lambda(
    view: MutatorHeapView,
    empty: RefPtr<BcEnvFrame>,
    prog: &eucalypt::eval::bytecode::BytecodeProgram,
) {
    let lambda = BcClosure::new_annotated_lambda(DUMMY_CODE, 2, empty, Smid::default());
    let first_arg = Array::from_slice(&view, &[native_one()]);
    let second_arg = [native_one()];

    let lambda = partially_apply(view, prog, &lambda, first_arg).unwrap();
    view.saturate(&lambda, &second_arg).unwrap();
}

pub fn criterion_benchmark(c: &mut Criterion) {
    // A minimal encoded program, purely to get a `BytecodeProgram` with PAP
    // templates populated for `create_partially_apply_and_saturate_lambda`.
    let (prog, _root, _gforms) = encode(&dsl::atom(dsl::num(0)), &[]);
    let mut pool = SymbolPool::new();

    // alloc_let — isolated heap so its GC pressure does not affect alloc_letrec.
    {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let _constants = prog.prepare_constants(view, &mut pool);
        let empty = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
        let bindings = fake_bindings(10);
        c.bench_function("alloc_let", |b| {
            b.iter(|| black_box(alloc_let(view, empty, &bindings)))
        });
    }

    // alloc_letrec — fresh heap, GC state independent of alloc_let run above.
    {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let empty = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
        let bindings = fake_bindings(10);
        c.bench_function("alloc_letrec", |b| {
            b.iter(|| black_box(alloc_letrec(view, empty, &bindings)))
        });
    }

    // deep env access / update — share one heap; env_stack is read-only for
    // access, and update only writes within the existing allocation.
    {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let empty = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
        let env_stack = fake_env_stack(view, empty, 20, 4);
        c.bench_function("deep_env_access", |b| {
            b.iter(|| access(view, env_stack, black_box(73)))
        });
        c.bench_function("deep_env_update", |b| {
            b.iter(|| update(view, env_stack, black_box(73)))
        });
    }

    // lambda construction — isolated heap.
    {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let empty = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
        c.bench_function("create_and_saturate_lambda", |b| {
            b.iter(|| create_and_saturate_lambda(view, empty))
        });
        c.bench_function("create_partially_apply_and_saturate_lambda", |b| {
            b.iter(|| create_partially_apply_and_saturate_lambda(view, empty, &prog))
        });
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
