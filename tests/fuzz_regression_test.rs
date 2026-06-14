#![cfg(not(target_arch = "wasm32"))]
//! Regression tests for fuzz targets.
//!
//! Each test exercises the fuzz target function on inputs derived from the
//! existing test suite — the same files that would populate the seed corpus
//! for a real fuzz run.  This ensures that:
//!
//! - The fuzz entry points accept all existing test inputs without panicking.
//! - Any refactoring that introduces a panic on valid input is caught in CI.
//!
//! Seeds are NOT checked into the repo.  For actual fuzzing, run
//! `cargo xtask fuzz-seed` to populate `fuzz/corpus/` from existing tests,
//! then `cargo fuzz run <target>`.

use codespan_reporting::files::SimpleFiles;
use eucalypt::{common::sourcemap::SourceMap, import, syntax::rowan};
use std::path::Path;

/// Extract type annotation strings from `.eu` files and run each through
/// `parse_type`.
#[test]
fn fuzz_type_dsl_regression() {
    let mut count = 0usize;

    // Scan all .eu files in tests/harness/ and lib/ for type: annotations.
    let dirs = &["tests/harness", "lib"];
    let type_re = regex::Regex::new(r#"type:\s*([cs]?"[^"]*")"#).unwrap();

    for dir in dirs {
        let dir = Path::new(dir);
        if !dir.exists() {
            continue;
        }
        for entry in walkdir::WalkDir::new(dir)
            .into_iter()
            .filter_map(|e| e.ok())
        {
            let path = entry.path();
            if path.extension().is_none_or(|e| e != "eu") {
                continue;
            }
            if let Ok(content) = std::fs::read_to_string(path) {
                for cap in type_re.captures_iter(&content) {
                    if let Some(m) = cap.get(1) {
                        let raw = m.as_str();
                        // Strip the string delimiters and prefix
                        let type_str = raw
                            .trim_start_matches('c')
                            .trim_start_matches('s')
                            .trim_start_matches('"')
                            .trim_end_matches('"');
                        // Must not panic.
                        let _ = eucalypt::core::typecheck::parse::parse_type(type_str);
                        count += 1;
                    }
                }
            }
        }
    }

    eprintln!("fuzz_type_dsl: tested {count} type annotations");
    assert!(
        count > 50,
        "expected at least 50 type annotations, found {count}"
    );
}

/// Run all `.eu` files from the test harness through the parser.
#[test]
fn fuzz_parser_regression() {
    let mut count = 0usize;

    let dirs = &["tests/harness", "lib"];
    for dir in dirs {
        let dir = Path::new(dir);
        if !dir.exists() {
            continue;
        }
        for entry in walkdir::WalkDir::new(dir)
            .into_iter()
            .filter_map(|e| e.ok())
        {
            let path = entry.path();
            if path.extension().is_none_or(|e| e != "eu") {
                continue;
            }
            if let Ok(content) = std::fs::read_to_string(path) {
                // Must not panic — every input must produce a parse result.
                let _ = rowan::parse_unit(&content);
                count += 1;
            }
        }
    }

    eprintln!("fuzz_parser: tested {count} .eu files");
    assert!(
        count > 100,
        "expected at least 100 .eu files, found {count}"
    );
}

/// Run all data files (YAML, JSON, TOML, CSV, XML) through the import loaders.
#[test]
fn fuzz_loader_regression() {
    let mut count = 0usize;

    let harness = Path::new("tests/harness");
    if !harness.exists() {
        return;
    }

    let format_exts: &[(&str, &str)] = &[
        ("yaml", "yaml"),
        ("yaml", "yml"),
        ("json", "json"),
        ("toml", "toml"),
        ("csv", "csv"),
        ("xml", "xml"),
    ];

    for entry in walkdir::WalkDir::new(harness)
        .into_iter()
        .filter_entry(|e| {
            // Skip .result directories (test output, not real data files)
            e.file_name() != ".result"
        })
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");

        if let Some(&(format, _)) = format_exts.iter().find(|&&(_, e)| e == ext) {
            if let Ok(content) = std::fs::read_to_string(path) {
                let mut files: SimpleFiles<String, String> = SimpleFiles::new();
                let mut source_map = SourceMap::new();
                let file_id = files.add(path.display().to_string(), content);
                // Must not panic.
                let _ =
                    import::read_to_core_data_only(format, &mut files, &mut source_map, file_id);
                count += 1;
            }
        }
    }

    eprintln!("fuzz_loader: tested {count} data files");
    assert!(count > 5, "expected at least 5 data files, found {count}");
}
