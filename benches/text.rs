//! End-to-end text pipeline benchmarks
//!
//! Benchmarks that exercise the full evaluation pipeline (parse, desugar,
//! cook, inline, compile, execute) on text-heavy workloads. These measure
//! realistic performance for string operations, regex matching, interpolation,
//! and text transformation chains.

use std::path::{Path, PathBuf};

use criterion::{criterion_group, criterion_main, Criterion};
use eucalypt::driver::{
    eval, options::EucalyptOptions, prepare, source::SourceLoader, statistics::Timings,
};
use eucalypt::syntax::input::{Input, Locator};

/// Run a benchmark .eu file through the full pipeline, capturing output.
fn run_bench_file(path: &Path) {
    let lib_path = vec![path.parent().unwrap().to_path_buf(), PathBuf::from("lib")];

    let input = Input::from(Locator::Fs(path.to_path_buf()));
    let opts = EucalyptOptions::default()
        .with_explicit_inputs(vec![input])
        .with_lib_path(lib_path)
        .with_export_type("json".to_string())
        .build();

    let mut loader = SourceLoader::new(opts.lib_path().to_vec());
    let mut timings = Timings::default();
    prepare::prepare(&opts, &mut loader, &mut timings).expect("prepare failed");

    let mut out = Vec::new();
    let mut err = Vec::new();
    let mut executor = eval::Executor::from(loader);
    executor.capture_output(Box::new(&mut out), Box::new(&mut err));

    let mut stats = eucalypt::driver::statistics::Statistics::default();
    executor
        .execute(&opts, &mut stats, "json".to_string())
        .expect("execution failed");
}

fn bench_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("harness/test/bench")
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("text_pipelines");

    let bench_files = [
        ("string_split_join", "011_string_split_join.eu"),
        ("regex_match", "012_regex_match.eu"),
        ("interpolation", "013_interpolation.eu"),
        ("text_transform", "014_text_transform.eu"),
    ];

    for (name, filename) in &bench_files {
        let path = bench_dir().join(filename);
        if path.exists() {
            group.bench_function(*name, |b| {
                b.iter(|| run_bench_file(&path));
            });
        }
    }

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
