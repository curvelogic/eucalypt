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
//! markers, with the Unicode version of the toolchain that generated them.
//! This test regenerates them and fails if `scanner.c` differs. Under a
//! toolchain with another Unicode version (`char::UNICODE_VERSION`) only the
//! code points below U+0100 — where the lexer's hand-written cases live — are
//! compared, so a Rust release that moves Unicode does not fail unrelated
//! work; the test prints a note instead. Rewrite them, under the Rust stable
//! that CI uses, with
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
    let (major, minor, update) = char::UNICODE_VERSION;
    writeln!(out, "// Unicode {major}.{minor}.{update}").unwrap();
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

/// The `// Unicode x.y.z` line of a generated block.
fn unicode_line(block: &str) -> Option<&str> {
    block.lines().find(|l| l.starts_with("// Unicode "))
}

/// Each table's ranges, clipped to code points below `limit`.
fn parse_tables(block: &str, limit: u32) -> Vec<(String, Vec<(u32, u32)>)> {
    let hex = |s: &str| u32::from_str_radix(s.trim().trim_start_matches("0x"), 16).unwrap();
    let mut tables: Vec<(String, Vec<(u32, u32)>)> = Vec::new();
    for line in block.lines() {
        if let Some(rest) = line.strip_prefix("static const CharRange ") {
            let name = rest.split('[').next().unwrap().to_string();
            tables.push((name, Vec::new()));
        } else if let Some((_, ranges)) = tables.last_mut() {
            for pair in line.split('{').skip(1) {
                let pair = pair.split('}').next().unwrap();
                let (lo, hi) = pair.split_once(',').unwrap();
                let (lo, hi) = (hex(lo), hex(hi));
                if lo < limit {
                    ranges.push((lo, hi.min(limit - 1)));
                }
            }
        }
    }
    tables
}

const REGENERATE: &str = "regenerate with `EU_UPDATE_SCANNER_TABLES=1 cargo test --test \
    tree_sitter_scanner_tables_test` under the Rust stable CI uses, then run `npm ci && npm \
    run build-wasm` in editors/tree-sitter-eucalypt and commit scanner.c, \
    tree-sitter-eucalypt.wasm and wasm-manifest.json";

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
    let recorded = unicode_line(current).expect("scanner.c's tables record no Unicode version");
    let toolchain = unicode_line(&expected).unwrap();
    if recorded != toolchain {
        // Another Unicode version: compare only the hand-written region.
        assert_eq!(
            parse_tables(current, 0x100),
            parse_tables(&expected, 0x100),
            "the character tables in editors/tree-sitter-eucalypt/src/scanner.c are not \
             the rowan lexer's below U+0100; {REGENERATE}"
        );
        eprintln!(
            "note: scanner.c's character tables are for {}, this toolchain has {}; \
             compared below U+0100 only. To follow it, {REGENERATE}",
            &recorded[3..],
            &toolchain[3..]
        );
        return;
    }
    panic!(
        "the character tables in editors/tree-sitter-eucalypt/src/scanner.c are not the \
         rowan lexer's; {REGENERATE}"
    );
}
