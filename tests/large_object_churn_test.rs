//! eu-oaz72.55: large objects (allocations of a GC block or more) are freed
//! once dead.
//!
//! Runs `tests/harness/238_oaz72_55_large_object_churn.eu` — 3,000 rounds of
//! `arr.set` on a 100x100 array, each round allocating a new large object
//! and dropping the previous one — under a small heap limit, and reads the
//! large-object count still held at exit from `--statistics-file`.
//!
//! Before the fix every large object ever allocated was still held (3,002
//! when measured); with it only the live ones and those allocated since the
//! last collection are (2 when measured). The bound sits well between.

use std::path::Path;

const FIXTURE: &str = "tests/harness/238_oaz72_55_large_object_churn.eu";
const MAX_LOBS_HELD: u64 = 1_000;

#[test]
fn large_object_churn_frees_dead_blocks() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let stats_path = dir.path().join("stats.json");

    // `output()` drains stdout and stderr, so verbose GC diagnostics under
    // EU_GC_VERIFY / EU_GC_POISON cannot fill a pipe and stall the child.
    let output = std::process::Command::new(Path::new(env!("CARGO_BIN_EXE_eu")))
        .args(["run", "--statistics-file"])
        .arg(&stats_path)
        .args(["--heap-limit-mib", "64", FIXTURE])
        .output()
        .expect("failed to run eu");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        output.status.success() && stdout.contains("RESULT: PASS"),
        "eu exited with {:?}\nstdout tail: {}\nstderr: {}",
        output.status.code(),
        &stdout[stdout.len().saturating_sub(400)..],
        String::from_utf8_lossy(&output.stderr)
    );

    let json: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&stats_path).expect("read statistics file"))
            .expect("statistics file is not valid JSON");
    let lobs = json["lobs_allocated"]
        .as_u64()
        .expect("statistics JSON missing lobs_allocated");
    assert!(
        lobs <= MAX_LOBS_HELD,
        "eu-oaz72.55: {lobs} large object blocks still held after 3,000 rounds \
         of large-object churn (bound {MAX_LOBS_HELD}): dead large objects are \
         not being freed — see HeapState::sweep_large_objects"
    );
}
