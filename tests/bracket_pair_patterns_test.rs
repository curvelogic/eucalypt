//! A bracket pair destructures a single block item through a list
//! pattern: `⟦ [{m}] ⟧: m` (eu-hvuqc).
//!
//! This is the form the `BracketPairBlockParameter` error and
//! `docs/appendices/syntax-gotchas.md` recommend in place of `⟦{m}⟧: m`.
//! The tree-sitter grammar cannot yet parse or highlight a bare-name block
//! pattern in a bracket head (eu-jd7l4), so a `.eu` harness fixture would
//! fail the editor-grammar corpus checks. This drives the binary instead.

use std::process::Command;

/// Shorthand, and a rename followed by a shorthand, in a block pattern
/// inside a bracket pair's list parameter.
const PROGRAM: &str = "\
⟦ [{m}] ⟧: m
⟪ [{k: kk, m}] ⟫: [kk, m]
a: ⟦ {m: 7} ⟧
b: ⟪ {k: 1, m: 2} ⟫
";

#[test]
fn block_pattern_inside_bracket_list_pattern_binds() {
    let dir = tempfile::tempdir().expect("temp dir");
    let path = dir.path().join("prog.eu");
    std::fs::write(&path, PROGRAM).expect("write fixture");

    let output = Command::new(env!("CARGO_BIN_EXE_eu"))
        .arg("-j")
        .arg(&path)
        .output()
        .expect("run eu");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "eu failed: {stderr}");

    let value: serde_json::Value = serde_json::from_slice(&output.stdout).expect("eu printed JSON");
    assert_eq!(value["a"], serde_json::json!(7), "⟦ [{{m}}] ⟧ binds m");
    assert_eq!(
        value["b"],
        serde_json::json!([1, 2]),
        "⟪ [{{k: kk, m}}] ⟫ binds kk and m"
    );
}
