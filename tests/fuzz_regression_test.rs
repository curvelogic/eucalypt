//! Regression tests for fuzz corpus seeds.
//!
//! Each test runs all seed files for a fuzz target through the corresponding
//! entry-point function, verifying that no seed panics.  This ensures that:
//!
//! - The seed corpus continues to be accepted (no regressions from refactoring).
//! - Any crash input added to the corpus during a fuzzing session is covered by CI.
//!
//! The tests do **not** run the fuzzer; they only exercise the target function
//! on known inputs.  Fuzzing itself is run separately via `cargo fuzz run`.

use codespan_reporting::files::SimpleFiles;
use eucalypt::{common::sourcemap::SourceMap, import, syntax::rowan};
use std::path::Path;

/// Run all seeds for `fuzz_type_dsl` through `parse_type`.
///
/// Seeds live in `fuzz/corpus/fuzz_type_dsl/`.
#[test]
fn fuzz_type_dsl_seeds() {
    let corpus_dir = Path::new("fuzz/corpus/fuzz_type_dsl");
    if !corpus_dir.exists() {
        // No corpus dir — skip gracefully (e.g. in a partial checkout).
        return;
    }

    let mut count = 0usize;
    for entry in std::fs::read_dir(corpus_dir).expect("read corpus dir") {
        let entry = entry.expect("dir entry");
        let path = entry.path();
        if path.is_file() {
            let data = std::fs::read(&path).expect("read seed file");
            if let Ok(s) = std::str::from_utf8(&data) {
                // Must not panic — only Ok or Err are acceptable.
                let _ = eucalypt::core::typecheck::parse::parse_type(s);
                count += 1;
            }
        }
    }

    eprintln!("fuzz_type_dsl: ran {count} seeds");
    assert!(
        count > 0,
        "corpus is empty — check fuzz/corpus/fuzz_type_dsl/"
    );
}

/// Run all seeds for `fuzz_parser` through `parse_unit`.
///
/// Seeds live in `fuzz/corpus/fuzz_parser/`.
#[test]
fn fuzz_parser_seeds() {
    let corpus_dir = Path::new("fuzz/corpus/fuzz_parser");
    if !corpus_dir.exists() {
        return;
    }

    let mut count = 0usize;
    for entry in std::fs::read_dir(corpus_dir).expect("read corpus dir") {
        let entry = entry.expect("dir entry");
        let path = entry.path();
        if path.is_file() {
            let data = std::fs::read(&path).expect("read seed file");
            if let Ok(s) = std::str::from_utf8(&data) {
                // Must not panic — every input must produce a parse result.
                let _ = rowan::parse_unit(s);
                count += 1;
            }
        }
    }

    eprintln!("fuzz_parser: ran {count} seeds");
    assert!(
        count > 0,
        "corpus is empty — check fuzz/corpus/fuzz_parser/"
    );
}

/// Run all seeds for `fuzz_loader` through the relevant import loader.
///
/// Seeds live in `fuzz/corpus/fuzz_loader/`.  Each seed file has a
/// format-tag prefix byte:
///
/// | byte | format |
/// |------|--------|
/// | 0    | yaml   |
/// | 1    | json   |
/// | 2    | toml   |
/// | 3    | csv    |
/// | 4    | xml    |
/// | 5    | jsonl  |
/// | 6    | text   |
/// | _    | yaml (default) |
#[test]
fn fuzz_loader_seeds() {
    let corpus_dir = Path::new("fuzz/corpus/fuzz_loader");
    if !corpus_dir.exists() {
        return;
    }

    let mut count = 0usize;
    for entry in std::fs::read_dir(corpus_dir).expect("read corpus dir") {
        let entry = entry.expect("dir entry");
        let path = entry.path();
        if !path.is_file() {
            continue;
        }

        let data = std::fs::read(&path).expect("read seed file");
        if data.is_empty() {
            continue;
        }

        let format = match data[0] {
            0 => "yaml",
            1 => "json",
            2 => "toml",
            3 => "csv",
            4 => "xml",
            5 => "jsonl",
            6 => "text",
            _ => "yaml",
        };
        let rest = &data[1..];

        if let Ok(text) = std::str::from_utf8(rest) {
            let mut files: SimpleFiles<String, String> = SimpleFiles::new();
            let mut source_map = SourceMap::new();
            let file_id = files.add("fuzz_seed".to_string(), text.to_string());

            // Must not panic.
            let _ = import::read_to_core_data_only(format, &mut files, &mut source_map, file_id);
            count += 1;
        }
    }

    eprintln!("fuzz_loader: ran {count} seeds");
    assert!(
        count > 0,
        "corpus is empty — check fuzz/corpus/fuzz_loader/"
    );
}
