#![cfg(not(target_arch = "wasm32"))]
//! GC trigger policy gates (beads eu-oaz72.4, eu-oaz72.73).
//!
//! Before eu-oaz72.4 the collector ran only once the heap reached its
//! limit: with the 32 GiB default it never ran at all (programs grew until
//! the kernel killed them), and under a small `--heap-limit-mib` it ran on
//! nearly every poll once at the limit, growing past it rather than
//! failing. Collections are now triggered by allocation volume since the
//! last collection, and live data the limit cannot hold is an error.
//!
//! Medium objects (more than a line, less than a block) once always took a
//! fresh block, so the heap grew with their allocation volume however
//! little was live, and past `--heap-limit-mib`, which only checked live
//! data (eu-oaz72.73). They now reuse swept blocks, and the blocks held
//! answer to the limit too.
//!
//! These spawn the compiled `eu` binary and use `tempfile`, so they are
//! excluded from the wasm32 target.

use std::path::Path;
use std::process::Command;
use std::time::Duration;

fn eu_binary() -> &'static Path {
    Path::new(env!("CARGO_BIN_EXE_eu"))
}

/// Run `cmd`, killing it and returning `None` if it does not finish within
/// `deadline`.
fn run_with_deadline(mut cmd: Command, deadline: Duration) -> Option<std::process::Output> {
    use std::io::Read;
    use wait_timeout::ChildExt;

    let mut child = cmd
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("failed to spawn eu");
    match child.wait_timeout(deadline).expect("wait_timeout failed") {
        Some(status) => {
            let mut stdout = Vec::new();
            let mut stderr = Vec::new();
            if let Some(mut s) = child.stdout.take() {
                s.read_to_end(&mut stdout).ok();
            }
            if let Some(mut s) = child.stderr.take() {
                s.read_to_end(&mut stderr).ok();
            }
            Some(std::process::Output {
                status,
                stdout,
                stderr,
            })
        }
        None => {
            child.kill().ok();
            child.wait().ok();
            None
        }
    }
}

/// Read a numeric statistic, wherever it is nested, from the JSON
/// `--statistics-file` wrote.
fn statistic(stats: &Path, key: &str) -> u64 {
    fn walk(v: &serde_json::Value, key: &str) -> Option<u64> {
        match v {
            serde_json::Value::Object(m) => m
                .get(key)
                .and_then(|x| x.as_u64())
                .or_else(|| m.values().find_map(|x| walk(x, key))),
            serde_json::Value::Array(a) => a.iter().find_map(|x| walk(x, key)),
            _ => None,
        }
    }
    let json: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(stats).unwrap()).unwrap();
    walk(&json, key).unwrap_or_else(|| panic!("no {key} in statistics"))
}

/// Run the medium-object churn fixture under `heap_limit_mib` and return
/// its peak heap blocks.
fn medium_churn_peak_blocks(heap_limit_mib: &str) -> u64 {
    let dir = tempfile::tempdir().unwrap();
    let stats = dir.path().join("stats.json");
    let mut cmd = Command::new(eu_binary());
    cmd.arg("--heap-limit-mib")
        .arg(heap_limit_mib)
        .arg("--statistics-file")
        .arg(&stats)
        .arg("tests/fixtures/medium_object_churn.eu")
        .env("EU_PP_THRESHOLD", "999999999");
    let out = run_with_deadline(cmd, Duration::from_secs(600)).expect("eu timed out");
    assert!(
        out.status.success(),
        "eu failed under --heap-limit-mib {heap_limit_mib}: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(String::from_utf8_lossy(&out.stdout).contains("r: 1000"));
    statistic(&stats, "peak_heap_blocks")
}

/// Medium-object churn with one set live at a time keeps the heap near
/// the minimum trigger (1,030 blocks measured) instead of growing with
/// the medium allocation volume (26,063 measured before eu-oaz72.73).
#[test]
fn medium_object_churn_reuses_swept_blocks() {
    let peak = medium_churn_peak_blocks("0");
    assert!(
        peak <= 2048,
        "medium-object churn grew the heap to {peak} blocks: medium \
         allocations are not reusing swept blocks (HeapState::replace_overflow)"
    );
}

/// Under a heap limit the blocks held, not only the live data, stay within
/// it: 32 MiB is 1,024 blocks. Measured: 901 blocks; 1,026 with medium
/// blocks reused but the footprint not enforced; 25,976 before
/// eu-oaz72.73. The bound, 15/16 of the limit, sits between.
#[test]
fn medium_object_churn_respects_heap_limit_footprint() {
    let peak = medium_churn_peak_blocks("32");
    assert!(
        peak <= 960,
        "medium-object churn held {peak} blocks under a 32 MiB (1,024-block) \
         heap limit: the footprint is not held to the limit"
    );
}

/// With no heap limit, a program allocating far more than the minimum
/// trigger collects, and its heap stays near that trigger rather than
/// growing with total allocation.
#[test]
fn unbounded_heap_collects_on_allocation_volume() {
    let dir = tempfile::tempdir().unwrap();
    let stats = dir.path().join("stats.json");
    let mut cmd = Command::new(eu_binary());
    cmd.arg("--heap-limit-mib")
        .arg("0")
        .arg("--statistics-file")
        .arg(&stats)
        .arg("tests/fixtures/gc_volume_trigger_loop.eu")
        .env("EU_PP_THRESHOLD", "999999999");
    let out = run_with_deadline(cmd, Duration::from_secs(300)).expect("eu timed out");
    assert!(
        out.status.success(),
        "eu failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(String::from_utf8_lossy(&out.stdout).contains("r: 300000"));

    let collections = statistic(&stats, "collections_count");
    let peak = statistic(&stats, "peak_heap_blocks");
    assert!(
        collections > 0,
        "no collection ran without a heap limit (peak {peak} blocks)"
    );
    // The minimum trigger is 1024 blocks (32 MiB); the loop's live data is
    // tiny, so the heap must stay within a small multiple of it.
    assert!(
        peak <= 4096,
        "heap grew to {peak} blocks over {collections} collections; \
         collection is not keeping pace with allocation"
    );
}

/// Live data that exceeds a small heap limit is reported as an eucalypt
/// error — not grown past, thrashed on, or left to the kernel.
#[test]
fn heap_limit_exhaustion_is_an_error() {
    let mut cmd = Command::new(eu_binary());
    cmd.arg("--heap-limit-mib")
        .arg("32")
        .arg("tests/fixtures/heap_limit_exhaustion.eu")
        .env("EU_PP_THRESHOLD", "999999999");
    let out = run_with_deadline(cmd, Duration::from_secs(300))
        .expect("eu did not stop within 300s under a 32 MiB heap limit");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        !out.status.success(),
        "eu succeeded under a 32 MiB limit; expected a heap-limit error"
    );
    assert!(
        stderr.contains("heap limit exceeded") && stderr.contains("32 MiB"),
        "expected a heap-limit-exceeded diagnostic, got: {stderr}"
    );
}
