//! Simple parse benchmarks

use criterion::{criterion_group, criterion_main, Criterion};
use eucalypt::syntax::rowan::{ast::Unit, Parse};

const UNIT: &str = r#"
"some documentation metadata"

foo: :bar
identity(x): x
block: {
  x: { y: 99, z: 100 }
  α: [1, 2, 3, 4]
}

{ :meta :data :only :block }
"#;

fn rowan_parser(text: &str) -> Parse<Unit> {
    eucalypt::syntax::rowan::parse_unit(text)
}

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("rowan parser", |b| b.iter(|| rowan_parser(UNIT)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
