//! STG allocation benchmarks

use eucalypt::common::sourcemap::Smid;
use eucalypt::eval::machines::stg::env::*;
use eucalypt::eval::machines::stg::syntax::dsl;
use eucalypt::eval::machines::stg::syntax::LambdaForm;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use std::{cell::RefCell, iter, rc::Rc};

fn fake_bindings(width: usize) -> Vec<LambdaForm> {
    iter::repeat_with(|| dsl::lambda(1, dsl::local(0)))
        .take(width)
        .collect()
}

fn fake_env_stack(width: usize, height: usize) -> Rc<EnvFrame> {
    let mut base = Rc::new(EnvFrame::empty());

    for _ in 0..height {
        let bindings = fake_bindings(width);
        base = EnvFrame::from_letrec(&bindings, &base, Smid::default());
    }

    base
}

/// Allocate a letrec of identify function bindings
fn alloc_let(bindings: &[LambdaForm]) -> Rc<EnvFrame> {
    EnvFrame::from_let(bindings, &Rc::new(EnvFrame::empty()), Smid::default())
}

/// Allocate a letrec of identify function bindings
fn alloc_letrec(bindings: &[LambdaForm]) -> Rc<EnvFrame> {
    EnvFrame::from_letrec(bindings, &Rc::new(EnvFrame::empty()), Smid::default())
}

/// Access deep closure
fn access(env: &Rc<EnvFrame>, depth: usize) -> Option<&RefCell<Closure>> {
    env.get(depth)
}

fn criterion_benchmark(c: &mut Criterion) {
    let bindings = fake_bindings(10);
    let env_stack = fake_env_stack(20, 4);
    c.bench_function("alloc_let", |b| b.iter(|| alloc_letrec(&bindings)));
    c.bench_function("alloc_letrec", |b| b.iter(|| alloc_letrec(&bindings)));
    c.bench_function("deep_env_access", |b| {
        b.iter(|| access(&env_stack, black_box(73)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
