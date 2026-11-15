#![cfg(not(target_arch = "wasm32"))]
//! Native containers (set, vec, ndarray) under collection (bead eu-oaz72.54).
//!
//! The harness run of `tests/harness/234_oaz72_5_native_storage_gc.eu` has
//! no heap limit and so does not collect. This spawns the compiled `eu`
//! binary on that file under `--heap-limit-mib 32` and `EU_GC_STRESS=1`
//! (force-evacuate), where its `gc-checks` hold a set, a vec and an ndarray
//! live across many collections and read them back. It gates the container
//! arms of `scan_native` and `update_native` in
//! `src/eval/bytecode/closure.rs`. The `scan_const` arms are not reachable
//! from a program and stay gated by the `machine.rs` unit tests.

use std::path::Path;
use std::process::Command;
use std::time::Duration;
use wait_timeout::ChildExt;

const HARNESS: &str = "tests/harness/234_oaz72_5_native_storage_gc.eu";

/// Run `eu` with `args` under the small heap limit and `EU_GC_STRESS=1`,
/// output to files so a full pipe cannot block the child. Returns
/// `(stdout, stderr)`; panics unless it succeeds within the deadline.
fn run_eu(args: &[&str], dir: &Path, tag: &str) -> (String, String) {
    let out_path = dir.join(format!("{tag}.out"));
    let err_path = dir.join(format!("{tag}.err"));
    let mut child = Command::new(Path::new(env!("CARGO_BIN_EXE_eu")))
        .args(args)
        .env("EU_PP_THRESHOLD", "999999999")
        .env("EU_GC_STRESS", "1")
        .stdout(std::fs::File::create(&out_path).unwrap())
        .stderr(std::fs::File::create(&err_path).unwrap())
        .spawn()
        .expect("failed to spawn eu");
    let status = match child.wait_timeout(Duration::from_secs(300)).unwrap() {
        Some(s) => s,
        None => {
            child.kill().ok();
            child.wait().ok();
            panic!("eu {args:?} did not finish within 300s");
        }
    };
    let out = std::fs::read_to_string(out_path).unwrap();
    let err = std::fs::read_to_string(err_path).unwrap();
    assert!(status.success(), "eu {args:?} failed: {out}\n{err}");
    (out, err)
}

fn collections(v: &serde_json::Value) -> Option<u64> {
    match v {
        serde_json::Value::Object(m) => m
            .get("collections_count")
            .and_then(|x| x.as_u64())
            .or_else(|| m.values().find_map(collections)),
        _ => None,
    }
}

#[test]
fn native_containers_survive_collection_under_small_heap_limit() {
    let dir = tempfile::tempdir().unwrap();

    // The harness verdict for the whole file.
    let (out, err) = run_eu(
        &["test", "--heap-limit-mib", "32", HARNESS],
        dir.path(),
        "verdict",
    );
    assert!(
        out.contains("PASS") || err.contains("PASS"),
        "no PASS verdict: {out}{err}"
    );

    // The gc-checks alone, with statistics: they must pass and must have
    // run through real collections.
    let stats = dir.path().join("stats.json");
    let (out, _) = run_eu(
        &[
            "--heap-limit-mib",
            "32",
            "--statistics-file",
            stats.to_str().unwrap(),
            HARNESS,
            "-e",
            "gc-checks.trues all-true?",
        ],
        dir.path(),
        "gc",
    );
    assert!(
        out.trim_start_matches("---").trim() == "true",
        "gc-checks failed: {out}"
    );
    let json: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&stats).unwrap()).unwrap();
    let n = collections(&json).expect("no collections_count in statistics");
    assert!(n >= 5, "only {n} collections ran");
}
