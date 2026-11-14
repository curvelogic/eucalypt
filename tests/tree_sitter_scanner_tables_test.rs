//! The tree-sitter external scanner's character classes must be the rowan
//! lexer's (eu-ioz21.9).
//!
//! `editors/tree-sitter-eucalypt/src/scanner.c` decides where a declaration
//! starts by looking ahead for `name:`. It used to classify identifier
//! characters with `iswalpha()`, which follows the C locale: a native build
//! (Emacs, Neovim, the tree-sitter CLI) saw no non-ASCII letter as an
//! identifier, while the WASM build (emscripten's locale-independent libc)
//! did, so a block of Greek-named declarations parsed differently in each.
//!
//! The scanner now carries range tables generated from the rowan lexer's own
//! predicates (`is_normal_start`, `is_normal_continuation` and
//! `char::is_whitespace`), between `BEGIN GENERATED` / `END GENERATED`
//! markers. This test regenerates them and fails if `scanner.c` differs —
//! after a lexer change, or a Rust toolchain with a newer Unicode version.
//! Rewrite them with
//!
//! ```text
//! EU_UPDATE_SCANNER_TABLES=1 cargo test --test tree_sitter_scanner_tables_test
//! ```
//!
//! then rebuild the WASM (`npm run build-wasm` in the grammar directory).

use eucalypt::syntax::rowan::lex::{is_normal_continuation, is_normal_start};
use std::fmt::Write as _;
use std::fs;
use std::path::{Path, PathBuf};

const BEGIN: &str = "// BEGIN GENERATED CHARACTER TABLES";
const END: &str = "// END GENERATED CHARACTER TABLES";

fn scanner_path() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("editors/tree-sitter-eucalypt/src/scanner.c")
}

/// The maximal ranges of code points satisfying `pred`.
fn ranges(pred: fn(char) -> bool) -> Vec<(u32, u32)> {
    let mut out: Vec<(u32, u32)> = Vec::new();
    for c in (0..=char::MAX as u32).filter_map(char::from_u32) {
        if !pred(c) {
            continue;
        }
        let cp = c as u32;
        match out.last_mut() {
            Some((_, hi)) if *hi + 1 == cp => *hi = cp,
            _ => out.push((cp, cp)),
        }
    }
    out
}

fn table(out: &mut String, name: &str, source: &str, pred: fn(char) -> bool) {
    let rs = ranges(pred);
    writeln!(out, "// {source}").unwrap();
    writeln!(out, "static const CharRange {name}[{}] = {{", rs.len()).unwrap();
    for chunk in rs.chunks(4) {
        let line: Vec<String> = chunk
            .iter()
            .map(|(lo, hi)| format!("{{0x{lo:04X}, 0x{hi:04X}}}"))
            .collect();
        writeln!(out, "    {},", line.join(", ")).unwrap();
    }
    writeln!(out, "}};").unwrap();
}

fn generated() -> String {
    let mut out = String::new();
    writeln!(out, "{BEGIN}").unwrap();
    writeln!(
        out,
        "// Generated from the rowan lexer by tests/tree_sitter_scanner_tables_test.rs.\n\
         // Do not edit: regenerate with\n\
         //   EU_UPDATE_SCANNER_TABLES=1 cargo test --test tree_sitter_scanner_tables_test"
    )
    .unwrap();
    writeln!(out).unwrap();
    table(
        &mut out,
        "IDENTIFIER_START",
        "is_normal_start in src/syntax/rowan/lex.rs",
        is_normal_start,
    );
    writeln!(out).unwrap();
    table(
        &mut out,
        "IDENTIFIER_CONT",
        "is_normal_continuation in src/syntax/rowan/lex.rs",
        is_normal_continuation,
    );
    writeln!(out).unwrap();
    table(
        &mut out,
        "WHITESPACE",
        "char::is_whitespace, as the rowan lexer uses",
        char::is_whitespace,
    );
    write!(out, "{END}").unwrap();
    out
}

#[test]
fn scanner_character_tables_match_rowan_lexer() {
    let path = scanner_path();
    let source = fs::read_to_string(&path).expect("cannot read scanner.c");
    let start = source
        .find(BEGIN)
        .expect("scanner.c has no BEGIN GENERATED marker");
    let end = source
        .find(END)
        .expect("scanner.c has no END GENERATED marker")
        + END.len();
    let current = &source[start..end];
    let expected = generated();

    if current == expected {
        return;
    }
    if std::env::var_os("EU_UPDATE_SCANNER_TABLES").is_some() {
        let updated = format!("{}{}{}", &source[..start], expected, &source[end..]);
        fs::write(&path, updated).expect("cannot write scanner.c");
        return;
    }
    panic!(
        "the character tables in editors/tree-sitter-eucalypt/src/scanner.c are not the \
         rowan lexer's; regenerate with `EU_UPDATE_SCANNER_TABLES=1 cargo test --test \
         tree_sitter_scanner_tables_test`, then run `npm ci && npm run build-wasm` in \
         editors/tree-sitter-eucalypt and commit scanner.c, tree-sitter-eucalypt.wasm and \
         wasm-manifest.json"
    );
}
