//! Blob-mode list retention (eu-oaz72.175).
//!
//! `cargo xtask prelude-compile` must run the demand analysis over each
//! prelude body, as source mode does. Without it the blob's `range` passes
//! its recursive counter `up(n + 1)` unforced, an O(n) alias chain pins every
//! cons cell, and a streaming consumer's peak heap grows with the list.
//!
//! The consumer is `foldl-strict`, so the bound isolates the producer's chain
//! from the lambda-capture pins that other consumers can add.
//!
//! Gated on `cfg(prelude_blob_ok)`: with no fresh blob the source prelude
//! runs, which has no such defect and nothing blob-specific to assert.
#![cfg(prelude_blob_ok)]

use std::process::Command;

/// Peak heap blocks (`-S`) for `range(0, n) foldl-strict(+, 0)`.
fn peak_blocks(n: u64) -> u64 {
    let dir = std::env::temp_dir().join(format!("eu-blob-retention-{}-{n}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("retention.eu");
    std::fs::write(&path, format!("main: range(0, {n}) foldl-strict(+, 0)\n")).unwrap();

    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .args(["-S", "--heap-limit-mib", "2048"])
        .arg(&path)
        .output()
        .expect("run eu");
    assert!(out.status.success(), "eu failed: {:?}", out);
    let stats = String::from_utf8_lossy(&out.stderr);
    stats
        .lines()
        .find_map(|l| l.trim().strip_prefix("Peak Blocks"))
        .and_then(|rest| {
            rest.trim_start_matches([' ', ':'])
                .replace(',', "")
                .trim()
                .parse()
                .ok()
        })
        .unwrap_or_else(|| panic!("no Peak Blocks line in:\n{stats}"))
}

#[test]
fn blob_range_does_not_retain_the_list() {
    let small = peak_blocks(100_000);
    let large = peak_blocks(400_000);
    // Constant (~1,031) once the counter is forced; ~5,900 at 200k without.
    assert!(large <= 1_100, "peak blocks at 400k cells: {large}");
    assert!(
        large <= small + 16,
        "peak blocks grew with n: {small} at 100k, {large} at 400k"
    );
}
