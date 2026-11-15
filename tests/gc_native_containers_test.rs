#![cfg(not(target_arch = "wasm32"))]
//! Native containers (set, vec, ndarray) under collection (bead eu-oaz72.54).
//!
//! The harness cases for these containers run with no heap limit and so do
//! not collect. This spawns the compiled `eu` binary on a fixture that
//! holds each container live across many collections under a small
//! `--heap-limit-mib`, and reads them back. It gates the container arms of
//! `scan_native` / `scan_const` (src/eval/bytecode): if the collector fails
//! to mark or update a container, the fixture reads back wrong or crashes.

use std::path::Path;
use std::process::Command;
use std::time::Duration;
use wait_timeout::ChildExt;

#[test]
fn native_containers_survive_collection_under_small_heap_limit() {
    let dir = tempfile::tempdir().unwrap();
    let stats = dir.path().join("stats.json");
    let mut child = Command::new(Path::new(env!("CARGO_BIN_EXE_eu")))
        .args(["--heap-limit-mib", "32", "--statistics-file"])
        .arg(&stats)
        .arg("tests/fixtures/native_containers_gc.eu")
        .env("EU_PP_THRESHOLD", "999999999")
        // Redirect to files so a full pipe can never block the child.
        .stdout(std::fs::File::create(dir.path().join("out")).unwrap())
        .stderr(std::fs::File::create(dir.path().join("err")).unwrap())
        .spawn()
        .expect("failed to spawn eu");
    let status = match child.wait_timeout(Duration::from_secs(120)).unwrap() {
        Some(s) => s,
        None => {
            child.kill().ok();
            child.wait().ok();
            panic!("eu did not finish within 120s");
        }
    };
    let out = std::fs::read_to_string(dir.path().join("out")).unwrap();
    let err = std::fs::read_to_string(dir.path().join("err")).unwrap();
    assert!(status.success(), "eu failed: {err}");
    assert!(
        out.contains("r: 1"),
        "containers read back wrong after collection: {out}"
    );

    let json: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&stats).unwrap()).unwrap();
    fn find(v: &serde_json::Value) -> Option<u64> {
        match v {
            serde_json::Value::Object(m) => m
                .get("collections_count")
                .and_then(|x| x.as_u64())
                .or_else(|| m.values().find_map(find)),
            _ => None,
        }
    }
    let collections = find(&json).expect("no collections_count in statistics");
    assert!(collections >= 5, "only {collections} collections ran");
}
