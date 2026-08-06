//! GC collection benchmarks
//!
//! Benchmarks for garbage collection performance: allocation followed by
//! collection, collection with varying survivor ratios, and allocation
//! into recycled blocks.
//!
//! Ported from HeapSyn let-expression fixtures (`HeapSyn::Let { bindings,
//! body }` over an identity-lambda binding and an `App` body) to a minimal,
//! engine-neutral `GcScannable` fixture by the Phase 4 collapse (eu-oufc),
//! which deleted HeapSyn. `Node::Compound` mirrors the same shape — one
//! bindings array plus a separate body pointer — so it exercises the same
//! collector code paths (array-backing evacuation, per-object marking) the
//! original fixture did.

use std::iter::repeat_with;
use std::ptr::NonNull;

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use eucalypt::eval::machine::metrics::{Clock, ThreadOccupation};
use eucalypt::eval::memory::alloc::{ScopedAllocator, StgObject};
use eucalypt::eval::memory::array::Array;
use eucalypt::eval::memory::collect::{
    collect, CollectorHeapView, CollectorScope, GcScannable, OpaqueHeapBytes, ScanPtr,
};
use eucalypt::eval::memory::heap::Heap;
use eucalypt::eval::memory::mutator::MutatorHeapView;
use std::hint::black_box;

/// Minimal `GcScannable` fixture: a `Compound` node (one bindings array plus
/// a separate body pointer, mirroring `HeapSyn::Let { bindings, body }`)
/// over `Leaf` bindings (no further pointers, mirroring an identity-lambda
/// `Atom` binding).
enum Node {
    Leaf,
    Compound(Array<NonNull<Node>>, NonNull<Node>),
}

impl StgObject for Node {}

impl GcScannable for Node {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        if let Node::Compound(bindings, body) = self {
            if marker.mark_array(bindings) {
                if let Some(backing_ptr) = bindings.allocated_data() {
                    out.push(ScanPtr::from_non_null(
                        scope,
                        backing_ptr.cast::<OpaqueHeapBytes>(),
                    ));
                }
                for b in bindings.iter() {
                    if marker.mark(*b) {
                        out.push(ScanPtr::from_non_null(scope, *b));
                    }
                }
            }
            if marker.mark(*body) {
                out.push(ScanPtr::from_non_null(scope, *body));
            }
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        if let Node::Compound(bindings, body) = self {
            if let Some(old_ptr) = bindings.allocated_data() {
                if let Some(new_ptr) = heap.forwarded_to(old_ptr) {
                    // SAFETY: new_ptr is a valid evacuated copy of the same
                    // backing allocation.
                    unsafe { bindings.set_backing_ptr(new_ptr.cast()) };
                }
            }
            for b in bindings.iter_mut() {
                if let Some(new) = heap.forwarded_to(*b) {
                    *b = new;
                }
            }
            if let Some(new) = heap.forwarded_to(*body) {
                *body = new;
            }
        }
    }
}

/// Allocate n let-expression-shaped nodes into the heap, each a `Compound`
/// wrapping a single `Leaf` binding and a `Leaf` body. Returns pointers to
/// the nodes.
fn allocate_let_exprs(heap: &Heap, n: usize) -> Vec<NonNull<Node>> {
    let view = MutatorHeapView::new(heap);

    repeat_with(|| {
        let binding = view.alloc(Node::Leaf).unwrap().as_ptr();
        let bindings = view.array(&[binding]);
        let body = view.alloc(Node::Leaf).unwrap().as_ptr();
        view.alloc(Node::Compound(bindings, body)).unwrap().as_ptr()
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
                let mut empty_roots: Vec<NonNull<Node>> = vec![];
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
                    let mut roots: Vec<NonNull<Node>> =
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
                let mut empty_roots: Vec<NonNull<Node>> = vec![];
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
