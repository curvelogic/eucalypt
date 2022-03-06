pub mod alloc;
pub mod parse;

use criterion::{criterion_group, criterion_main};

criterion_group!(
    benches,
    alloc::criterion_benchmark,
    parse::criterion_benchmark
);
criterion_main!(benches);
