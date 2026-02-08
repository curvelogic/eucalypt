//! GC collection benchmarks
//!
//! Benchmarks for garbage collection performance: allocation followed by
//! collection, collection with varying survivor ratios, and allocation
//! into recycled blocks.

use std::iter::repeat_with;

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use eucalypt::{
    common::sourcemap::Smid,
    eval::{
        machine::metrics::{Clock, ThreadOccupation},
        memory::{
            collect::collect,
            heap::Heap,
            mutator::MutatorHeapView,
            syntax::{HeapSyn, LambdaForm, Ref, StgBuilder},
        },
    },
};
use std::hint::black_box;
use std::ptr::NonNull;

/// Allocate n let-expressions into the heap, each wrapping a single
/// identity lambda binding. Returns pointers to the HeapSyn nodes.
fn allocate_let_exprs(heap: &Heap, n: usize) -> Vec<NonNull<HeapSyn>> {
    let view = MutatorHeapView::new(heap);
    let mut pool = eucalypt::eval::memory::symbol::SymbolPool::new();

    repeat_with(|| {
        let binding = LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default());
        let bindings = view.array(&[binding]);
        let body = view
            .app(
                Ref::L(0),
                view.singleton(view.sym_ref(&mut pool, "x").unwrap()),
            )
            .unwrap();
        view.let_(bindings, body).unwrap().as_ptr()
    })
    .take(n)
    .collect()
}

/// Benchmark: allocate objects then collect with no survivors
fn bench_alloc_then_collect(c: &mut Criterion) {
    let mut group = c.benchmark_group("gc_alloc_then_collect");

    for count in [256, 1024, 4096] {
        group.bench_with_input(BenchmarkId::from_parameter(count), &count, |b, &count| {
            b.iter(|| {
                let mut heap = Heap::new();
                let mut clock = Clock::default();
                clock.switch(ThreadOccupation::Mutator);

                // Allocate objects (all garbage — no roots retained)
                let _ptrs = allocate_let_exprs(&heap, count);

                // Collect with empty root set
                let mut empty_roots: Vec<NonNull<HeapSyn>> = vec![];
                collect(black_box(&mut empty_roots), &mut heap, &mut clock, false);
            });
        });
    }
    group.finish();
}

/// Benchmark: collect with varying percentage of survivors
fn bench_collect_with_survivors(c: &mut Criterion) {
    let mut group = c.benchmark_group("gc_collect_with_survivors");

    let total = 1024;
    for survivor_pct in [0, 25, 50, 75, 100] {
        group.bench_with_input(
            BenchmarkId::new("pct", survivor_pct),
            &survivor_pct,
            |b, &pct| {
                b.iter(|| {
                    let mut heap = Heap::new();
                    let mut clock = Clock::default();
                    clock.switch(ThreadOccupation::Mutator);

                    let ptrs = allocate_let_exprs(&heap, total);

                    // Retain a fraction as roots
                    let survivor_count = total * pct / 100;
                    let mut roots: Vec<NonNull<HeapSyn>> =
                        ptrs.into_iter().take(survivor_count).collect();

                    collect(black_box(&mut roots), &mut heap, &mut clock, false);
                });
            },
        );
    }
    group.finish();
}

/// Benchmark: allocate, collect, then allocate into recycled blocks
fn bench_alloc_into_recycled(c: &mut Criterion) {
    let mut group = c.benchmark_group("gc_alloc_into_recycled");

    for count in [256, 1024, 4096] {
        group.bench_with_input(BenchmarkId::from_parameter(count), &count, |b, &count| {
            b.iter(|| {
                let mut heap = Heap::new();
                let mut clock = Clock::default();
                clock.switch(ThreadOccupation::Mutator);

                // Phase 1: allocate and collect to produce recycled blocks
                let _ptrs = allocate_let_exprs(&heap, count);
                let mut empty_roots: Vec<NonNull<HeapSyn>> = vec![];
                collect(&mut empty_roots, &mut heap, &mut clock, false);

                // Phase 2: allocate into recycled space
                clock.switch(ThreadOccupation::Mutator);
                let _new_ptrs = allocate_let_exprs(&heap, black_box(count));
            });
        });
    }
    group.finish();
}

pub fn criterion_benchmark(c: &mut Criterion) {
    bench_alloc_then_collect(c);
    bench_collect_with_survivors(c);
    bench_alloc_into_recycled(c);
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
