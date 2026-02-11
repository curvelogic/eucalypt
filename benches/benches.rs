pub mod alloc;
pub mod gc;
pub mod parse;
pub mod text;

use criterion::{criterion_group, criterion_main};

criterion_group!(
    benches,
    alloc::criterion_benchmark,
    gc::criterion_benchmark,
    parse::criterion_benchmark,
    text::criterion_benchmark
);
criterion_main!(benches);
