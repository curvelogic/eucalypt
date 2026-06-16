//! Fuzz target for the eucalypt evaluator (VM + GC).
//!
//! Takes arbitrary bytes, interprets them as UTF-8 eucalypt source, and runs
//! the full parse → desugar → compile → evaluate pipeline.  Any panic is a
//! fuzz finding.
//!
//! Uses a small heap limit (64 MiB) and no-prelude mode to keep execution
//! fast and avoid OOM.  Evaluation errors (type mismatch, unresolved names,
//! etc.) are expected and ignored — only panics matter.
#![no_main]

use libfuzzer_sys::fuzz_target;

use eucalypt::driver::{
    eval, options::EucalyptOptions, prepare, source::SourceLoader, statistics::Statistics,
    statistics::Timings,
};
use eucalypt::eval::stg::StgSettings;
use eucalypt::syntax::input::Input;
use std::str::FromStr;

fuzz_target!(|data: &[u8]| {
    let Ok(source) = std::str::from_utf8(data) else {
        return;
    };

    // Skip empty inputs and very large inputs (avoid slow runs).
    if source.is_empty() || source.len() > 4096 {
        return;
    }

    // Write the source to a temp file so the driver can load it.
    let dir = match tempfile::tempdir() {
        Ok(d) => d,
        Err(_) => return,
    };
    let path = dir.path().join("fuzz.eu");
    if std::fs::write(&path, source).is_err() {
        return;
    }

    let path_str = path.to_string_lossy().into_owned();
    let input = match Input::from_str(&path_str) {
        Ok(i) => i,
        Err(_) => return,
    };

    let opt = EucalyptOptions {
        stg_settings: StgSettings {
            heap_limit_mib: Some(64),
            ..StgSettings::default()
        },
        ..EucalyptOptions::default()
            .with_explicit_inputs(vec![input])
            .build()
    };

    let mut loader = SourceLoader::new(vec![]);
    let mut timings = Timings::default();

    // Prepare (parse + desugar + compile).  Errors here are fine.
    if prepare::prepare(&opt, &mut loader, &mut timings).is_err() {
        return;
    }

    let mut stats = Statistics::default();
    let mut out: Vec<u8> = Vec::new();
    let mut err_buf: Vec<u8> = Vec::new();
    let mut executor = eval::Executor::from(loader);
    executor.capture_output(Box::new(&mut out), Box::new(&mut err_buf));

    // Evaluate.  Any panic propagates as a fuzz finding.
    // Evaluation errors are expected and ignored.
    let _ = executor.execute(&opt, &mut stats, "yaml".to_string());
});
