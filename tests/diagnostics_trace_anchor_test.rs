//! Regression gate for **eu-og3u6**: in a unit with many bindings, the
//! primary label and the curated trace anchor must name the declaration that
//! made the failing call.
//!
//! The defect was a stale annotation register in the bytecode engine. Popping
//! a `SeqBind` continuation restored its captured annotation only when that
//! annotation was valid (eu-0lvf), so an annotation established while
//! rendering one top-level binding survived the render loop's per-item
//! `force` — which legitimately carries `Smid::default()` — and was stamped
//! onto every continuation created for every *later* binding. The first
//! annotated declaration in the unit therefore became a permanent blame
//! anchor: a failure inside `nth` called from `result` was reported against
//! `pad0: 0 + 0`.
//!
//! **Why the fixture is large.** With a two-declaration file the bug is
//! invisible: the leaked annotation happens to be the calling declaration's
//! own, so every small fixture in the corpus passed throughout. The bug only
//! appears once some other binding renders first, and the *distance* between
//! the innocent declaration and the guilty one is what makes a regression
//! unmistakable — hence 900 padding declarations here, matching the bead's
//! repro. `tests/harness/errors/195_og3u6_trace_anchor.eu` covers the same
//! property with a readable ten-binding fixture.
//!
//! Deliberately **not** gated on `prelude_blob_ok`: the defect reproduced
//! identically with and without `lib/prelude.blob`, so this must run in every
//! CI configuration. Both engines are exercised explicitly.

use std::process::Command;

/// Padding declarations between `xs` and `result`.
const PADS: usize = 900;

/// Build the fixture. Line 1 is `xs`, lines 2..=PADS+1 are the padding
/// bindings, and the failing call is the last line.
fn fixture() -> (String, usize) {
    let mut src = String::from("xs: [1, 2, 3]\n");
    for i in 0..PADS {
        src.push_str(&format!("pad{i}: {i} + {i}\n"));
    }
    src.push_str("result: xs nth(10)\n");
    // 1-based line number of the `result` declaration.
    (src, PADS + 2)
}

/// Strip ANSI SGR sequences so assertions can look at the rendered text.
fn strip_ansi(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\u{1b}' {
            // Consume up to and including the final byte of the escape.
            for e in chars.by_ref() {
                if e.is_ascii_alphabetic() {
                    break;
                }
            }
        } else {
            out.push(c);
        }
    }
    out
}

fn run(src: &str, heapsyn: bool) -> String {
    let dir = std::env::temp_dir().join(format!(
        "eu-og3u6-{}-{}",
        std::process::id(),
        if heapsyn { "hs" } else { "bc" }
    ));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("many_bindings.eu");
    std::fs::write(&path, src).unwrap();

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_eu"));
    cmd.args(["--heap-limit-mib", "4096"]).arg(&path);
    if heapsyn {
        cmd.env("EU_HEAPSYN", "1");
    }
    let out = cmd.output().expect("run eu");
    assert!(
        !out.status.success(),
        "fixture must fail; it evaluated successfully"
    );
    strip_ansi(&String::from_utf8_lossy(&out.stderr))
}

/// The primary label's `file:line:col`, taken from the `┌─` line specifically
/// — *not* from the `while evaluating (outermost first):` note, which a
/// partial fix could satisfy on its own. The leading directory is dropped:
/// each engine runs in its own temporary directory.
fn primary_label_location(stderr: &str) -> String {
    let line = stderr
        .lines()
        .find(|l| l.contains('┌'))
        .unwrap_or_else(|| panic!("no primary label line in diagnostic:\n{stderr}"));
    // Take everything after the `┌─ ` marker rather than the last
    // whitespace-delimited token: a temporary directory containing a space
    // would otherwise truncate the path and fail this test with a message
    // that reads like a diagnostics regression rather than a test bug.
    let located = line
        .split_once("┌─ ")
        .map(|(_, rest)| rest.trim())
        .expect("primary label carries a location");
    // Native separators — `Locator::Fs` renders via `path.to_string_lossy()`.
    located
        .rsplit(std::path::MAIN_SEPARATOR)
        .next()
        .unwrap_or(located)
        .to_string()
}

fn assert_anchors_on_the_caller(heapsyn: bool) {
    let (src, result_line) = fixture();
    let stderr = run(&src, heapsyn);
    let engine = if heapsyn { "HeapSyn" } else { "bytecode" };

    // The `nth` token starts at column 12 of `result: xs nth(10)`.
    let expected = format!("many_bindings.eu:{result_line}:12");
    let primary = primary_label_location(&stderr);
    assert!(
        primary == expected,
        "{engine}: primary label must point at the `result` declaration \
         ({expected}), got {primary}\n---\n{stderr}"
    );

    // No padding declaration may be blamed anywhere in the diagnostic —
    // neither as a label nor as a trace frame.
    assert!(
        !stderr.contains("pad"),
        "{engine}: diagnostic blames a padding declaration\n---\n{stderr}"
    );

    // The curated trace must anchor on the same declaration.
    assert!(
        stderr.contains(&format!("- result at {expected}")),
        "{engine}: curated trace must anchor on `result at {expected}`\n---\n{stderr}"
    );
}

#[test]
fn many_binding_unit_anchors_blame_on_the_calling_declaration_bytecode() {
    assert_anchors_on_the_caller(false);
}

#[test]
fn many_binding_unit_anchors_blame_on_the_calling_declaration_heapsyn() {
    assert_anchors_on_the_caller(true);
}

/// The two engines must agree, frame for frame, on a many-binding unit. The
/// defect was bytecode-only — HeapSyn restores its annotation register
/// unconditionally and always named `result` — so an engine-parity assertion
/// is the sharpest statement of what was wrong.
#[test]
fn both_engines_agree_on_the_anchor_in_a_many_binding_unit() {
    let (src, _) = fixture();
    let bytecode = run(&src, false);
    let heapsyn = run(&src, true);
    assert_eq!(
        primary_label_location(&bytecode),
        primary_label_location(&heapsyn),
        "engines disagree on the primary label\n--- bytecode\n{bytecode}\n--- heapsyn\n{heapsyn}"
    );
}
