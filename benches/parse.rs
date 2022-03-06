//! Simple parse benchmarks

use codespan_reporting::files::SimpleFiles;
use criterion::{criterion_group, criterion_main, Criterion};
use eucalypt::syntax::{
    ast::Block,
    error::ParserError,
    rowan::{ast::Unit, validate, Parse},
};

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

fn old_parser(text: &str) -> Result<Block, ParserError> {
    let mut files = SimpleFiles::new();
    let id = files.add("test".to_string(), text.to_string());
    eucalypt::syntax::parser::parse_unit(&files, id)
}

fn new_lossless_parser(text: &str) -> Parse<Unit> {
    eucalypt::syntax::rowan::parse_unit(text)
}

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("new parser", |b| b.iter(|| new_lossless_parser(UNIT)));
    c.bench_function("old parser", |b| b.iter(|| old_parser(UNIT)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
