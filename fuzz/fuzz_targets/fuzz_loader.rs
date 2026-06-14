//! Fuzz target for eucalypt import loaders.
//!
//! The first byte of the fuzz input selects the format:
//!
//! | byte | format |
//! |------|--------|
//! | 0    | yaml   |
//! | 1    | json   |
//! | 2    | toml   |
//! | 3    | csv    |
//! | 4    | xml    |
//! | 5    | jsonl  |
//! | 6    | text   |
//! | _    | yaml (default) |
//!
//! The remainder of the input is fed to the chosen loader as UTF-8 text.
//! The wrapper code around third-party crates (`serde_yaml`, `toml`, `csv`,
//! `roxmltree`) can panic on unexpected input shapes.  The expected contract
//! is: return a core expression or an error, never panic.
#![no_main]

use codespan_reporting::files::SimpleFiles;
use eucalypt::common::sourcemap::SourceMap;
use eucalypt::import;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if data.is_empty() {
        return;
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

    let Ok(text) = std::str::from_utf8(rest) else {
        return;
    };

    let mut files: SimpleFiles<String, String> = SimpleFiles::new();
    let mut source_map = SourceMap::new();
    let file_id = files.add("fuzz_input".to_string(), text.to_string());

    // Must not panic — only Ok or Err are acceptable.
    // Use data-only mode so `!eu` tags in YAML do not evaluate eucalypt code.
    let _ = import::read_to_core_data_only(format, &mut files, &mut source_map, file_id);
});
