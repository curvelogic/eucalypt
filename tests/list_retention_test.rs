#![cfg(not(target_arch = "wasm32"))]
//! Space gate for consuming a long list through a wrapper frame
//! (bead eu-oaz72.175).
//!
//! `range(0, n) count`, and a wrapper function that passes a `+` lambda to a
//! strict fold, kept the *whole consumed list* alive: a closure allocated in
//! the wrapper's frame (the inlined `+`) was closed over that frame, whose
//! parameter slot held the list head, and the closure was threaded through
//! every iteration. Peak heap blocks grew with the list (about 11,000 at
//! 200,000 cells) and every collection re-marked the retained prefix.
//!
//! The fix closes a `Let`/`LetRec` binding over the globals frame when no
//! local reference in its code escapes its own frames (a "closed" binding),
//! so the closure no longer pins the frame it was allocated in. Peak blocks
//! are deterministic, so this gate does not depend on wall time. Measured:
//! 1,026 with the fix, 10,9xx without; the bound sits well between.
//!
//! Both dispatch paths are exercised (`EU_PREDECODE=0` selects byte dispatch).

use std::path::Path;
use std::process::Command;
use std::time::Duration;

fn eu_binary() -> &'static Path {
    Path::new(env!("CARGO_BIN_EXE_eu"))
}

/// Peak heap blocks and stdout for `fixture` under the given dispatch path.
fn run(fixture: &str, predecode: &str) -> (u64, String) {
    use wait_timeout::ChildExt;
    let dir = tempfile::tempdir().unwrap();
    let stats = dir.path().join("stats.json");
    let mut child = Command::new(eu_binary())
        .arg("--statistics-file")
        .arg(&stats)
        .arg(fixture)
        .env("EU_PREDECODE", predecode)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("failed to spawn eu");
    let status = child
        .wait_timeout(Duration::from_secs(300))
        .expect("wait failed");
    let Some(status) = status else {
        child.kill().ok();
        panic!("eu timed out on {fixture}");
    };
    let out = child.wait_with_output().unwrap();
    assert!(status.success(), "eu failed on {fixture}");
    let json: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&stats).unwrap()).unwrap();
    fn walk(v: &serde_json::Value) -> Option<u64> {
        match v {
            serde_json::Value::Object(m) => m
                .get("peak_heap_blocks")
                .and_then(|x| x.as_u64())
                .or_else(|| m.values().find_map(walk)),
            serde_json::Value::Array(a) => a.iter().find_map(walk),
            _ => None,
        }
    }
    (
        walk(&json).expect("no peak_heap_blocks in statistics"),
        String::from_utf8_lossy(&out.stdout).into_owned(),
    )
}

fn assert_constant_space(fixture: &str, expect: &str) {
    for predecode in ["1", "0"] {
        let (peak, stdout) = run(fixture, predecode);
        assert!(
            stdout.contains(expect),
            "{fixture} (EU_PREDECODE={predecode}): unexpected output {stdout}"
        );
        assert!(
            peak <= 2048,
            "{fixture} (EU_PREDECODE={predecode}) peaked at {peak} heap blocks: \
             the consumed list prefix is being retained through a frame"
        );
    }
}

#[test]
fn count_over_range_runs_in_constant_space() {
    assert_constant_space("tests/fixtures/list_retention_count.eu", "x: 200000");
}

#[test]
fn wrapper_frame_does_not_pin_consumed_list() {
    assert_constant_space("tests/fixtures/list_retention_wrapper.eu", "x: 19999900000");
}
