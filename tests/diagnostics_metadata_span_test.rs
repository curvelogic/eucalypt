//! Regression test for eu-1tkk.7.7 (metadata-in-span).
//!
//! A declaration's label span must start at the declaration *head*, not at
//! its leading backtick metadata. When a declaration carries doc metadata
//! and participates in an error's stack trace (e.g. as the frame recording
//! where a called function was defined), the Smid minted for that
//! declaration must not resolve to a position on the metadata line.
//!
//! `tests/diagnostics/corpus/metadata_span.eu` line 1 is a backtick doc
//! comment attached to `scale` (declared on line 2). Before the fix, the
//! Smid minted for `scale`'s lambda spanned from the metadata's backtick
//! through the end of the declaration body, so any diagnostic label built
//! from that Smid reported line 1 (the doc comment) instead of line 2 (the
//! declaration head). This test asserts no diagnostic label — primary or
//! secondary — ever lands on the metadata line.
use std::process::Command;

/// Extract the JSON diagnostic object from combined stdout/stderr, scanning
/// from the bottom (mirrors `tests/diagnostics_invariants.rs`'s
/// `last_json_object`, since human-readable warnings may precede the JSON
/// diagnostic line).
fn last_json_object(streams: &[&str]) -> Option<serde_json::Value> {
    for stream in streams {
        for line in stream.lines().rev() {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            if let Ok(v) = serde_json::from_str::<serde_json::Value>(trimmed) {
                if v.is_object() {
                    return Some(v);
                }
            }
        }
    }
    None
}

#[test]
fn declaration_label_span_excludes_leading_metadata() {
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/diagnostics/corpus/metadata_span.eu"
    );
    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .args(["--error-format", "json", "--heap-limit-mib", "2048"])
        .arg(path)
        .output()
        .expect("run eu");
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();
    let v = last_json_object(&[&stderr, &stdout])
        .unwrap_or_else(|| panic!("no JSON diagnostic parsed; stdout={stdout} stderr={stderr}"));

    // Line 1 of the fixture is the backtick doc comment attached to `scale`
    // (declared on line 2). No label — primary or secondary — should ever
    // report line 1: that would mean a Smid's span still starts at the
    // metadata rather than the declaration head.
    let labels = v["labels"]
        .as_array()
        .cloned()
        .unwrap_or_else(|| panic!("no labels array in diagnostic: {v}"));
    assert!(
        !labels.is_empty(),
        "expected at least one label in diagnostic: {v}"
    );

    let on_metadata_line: Vec<&serde_json::Value> = labels
        .iter()
        .filter(|l| l["pos"]["file"].as_str() == Some(path) && l["pos"]["line"] == 1)
        .collect();

    assert!(
        on_metadata_line.is_empty(),
        "a label's span starts at the declaration's leading backtick metadata \
         (line 1) rather than the declaration head (line 2): {on_metadata_line:?}\n\
         full diagnostic: {v}"
    );
}
