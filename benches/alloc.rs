//! STG allocation benchmarks
//!
//! Each benchmark group gets its own fresh `Heap` so that GC pressure from one
//! group cannot contaminate the measurements of another.  Previously all
//! benchmarks shared a single heap: as `alloc_let` filled it, `alloc_letrec`
//! ran against a heap with higher occupancy, causing phantom regressions of
//! ±30–80% in criterion comparisons.

use std::iter;

use eucalypt::{
    common::sourcemap::Smid,
    eval::{
        machine::{
            env::{EnvFrame, SynClosure},
            env_builder::EnvBuilder,
        },
        memory::{
            alloc::ScopedAllocator,
            array::Array,
            heap::Heap,
            mutator::MutatorHeapView,
            syntax::{LambdaForm, Native, Ref, RefPtr, StgBuilder},
        },
        stg::tags::DataConstructor,
    },
};

use criterion::{criterion_group, criterion_main, Criterion};
use std::hint::black_box;

fn box_one(view: MutatorHeapView, empty: RefPtr<EnvFrame>) -> SynClosure {
    SynClosure::new(
        view.data(
            DataConstructor::BoxedString.tag(),
            Array::from_slice(&view, &[Ref::V(Native::Num(1.into()))]),
        )
        .unwrap()
        .as_ptr(),
        empty,
    )
}

fn fake_bindings(view: MutatorHeapView, width: usize) -> Vec<LambdaForm> {
    iter::repeat_with(|| {
        LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default())
    })
    .take(width)
    .collect()
}

fn fake_env_stack(
    view: MutatorHeapView,
    empty: RefPtr<EnvFrame>,
    width: usize,
    height: usize,
) -> RefPtr<EnvFrame> {
    let mut base = empty;

    for _ in 0..height {
        let bindings = fake_bindings(view, width);
        base = view.from_letrec(&bindings, base, Smid::default()).unwrap();
    }

    base
}

/// Allocate a let frame of identity function bindings
fn alloc_let(
    view: MutatorHeapView,
    empty: RefPtr<EnvFrame>,
    bindings: &[LambdaForm],
) -> RefPtr<EnvFrame> {
    view.from_let(bindings, empty, Smid::default()).unwrap()
}

/// Allocate a letrec frame of identity function bindings
fn alloc_letrec(
    view: MutatorHeapView,
    empty: RefPtr<EnvFrame>,
    bindings: &[LambdaForm],
) -> RefPtr<EnvFrame> {
    view.from_letrec(bindings, empty, Smid::default()).unwrap()
}

/// Access deep closure
fn access(view: MutatorHeapView, env: RefPtr<EnvFrame>, depth: usize) -> Option<SynClosure> {
    let e = view.scoped(env);
    (*e).get(&view, depth)
}

/// Update deep closure with a new value
fn update(view: MutatorHeapView, empty: RefPtr<EnvFrame>, env: RefPtr<EnvFrame>, depth: usize) {
    let e = view.scoped(env);
    let value = box_one(view, empty);
    (*e).update(&view, depth, value).unwrap();
}

/// Create an identity lambda and saturate it
fn create_and_saturate_lambda(view: MutatorHeapView, empty: RefPtr<EnvFrame>) {
    let lambda = SynClosure::close(
        &LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default()),
        empty,
    );
    let args = vec![box_one(view, empty)];
    view.saturate(&lambda, args.as_slice()).unwrap();
}

/// Create a two-argument lambda, partially apply it, then saturate
fn create_partially_apply_and_saturate_lambda(view: MutatorHeapView, empty: RefPtr<EnvFrame>) {
    let lambda = SynClosure::close(
        &LambdaForm::new(2, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default()),
        empty,
    );
    let first_arg = vec![box_one(view, empty)];
    let second_arg = vec![box_one(view, empty)];

    let lambda = view.partially_apply(&lambda, first_arg.as_slice()).unwrap();
    view.saturate(&lambda, second_arg.as_slice()).unwrap();
}

pub fn criterion_benchmark(c: &mut Criterion) {
    // alloc_let — isolated heap so its GC pressure does not affect alloc_letrec.
    {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let empty = view.alloc(EnvFrame::default()).unwrap().as_ptr();
        let bindings = fake_bindings(view, 10);
        c.bench_function("alloc_let", |b| {
            b.iter(|| black_box(alloc_let(view, empty, &bindings)))
        });
    }

    // alloc_letrec — fresh heap, GC state independent of alloc_let run above.
    {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let empty = view.alloc(EnvFrame::default()).unwrap().as_ptr();
        let bindings = fake_bindings(view, 10);
        c.bench_function("alloc_letrec", |b| {
            b.iter(|| black_box(alloc_letrec(view, empty, &bindings)))
        });
    }

    // deep env access / update — share one heap; env_stack is read-only for
    // access, and update only writes within the existing allocation.
    {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let empty = view.alloc(EnvFrame::default()).unwrap().as_ptr();
        let env_stack = fake_env_stack(view, empty, 20, 4);
        c.bench_function("deep_env_access", |b| {
            b.iter(|| access(view, env_stack, black_box(73)))
        });
        c.bench_function("deep_env_update", |b| {
            b.iter(|| update(view, empty, env_stack, black_box(73)))
        });
    }

    // lambda construction — isolated heap.
    {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let empty = view.alloc(EnvFrame::default()).unwrap().as_ptr();
        c.bench_function("create_and_saturate_lambda", |b| {
            b.iter(|| create_and_saturate_lambda(view, empty))
        });
        c.bench_function("create_partially_apply_and_saturate_lambda", |b| {
            b.iter(|| create_partially_apply_and_saturate_lambda(view, empty))
        });
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
