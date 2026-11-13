#![cfg(not(target_arch = "wasm32"))]
//! GC trigger policy gates (bead eu-oaz72.4).
//!
//! Before eu-oaz72.4 the collector ran only once the heap reached its
//! limit: with the 32 GiB default it never ran at all (programs grew until
//! the kernel killed them), and under a small `--heap-limit-mib` it ran on
//! nearly every poll once at the limit, growing past it rather than
//! failing. Collections are now triggered by allocation volume since the
//! last collection, and live data the limit cannot hold is an error.
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

/// Run the volume-trigger loop fixture under `--heap-limit-mib limit_mib`
/// and return its `(collections, peak heap blocks)` statistics.
fn loop_fixture_stats(limit_mib: &str) -> (u64, u64) {
    let dir = tempfile::tempdir().unwrap();
    let stats = dir.path().join("stats.json");
    let mut cmd = Command::new(eu_binary());
    cmd.arg("--heap-limit-mib")
        .arg(limit_mib)
        .arg("--statistics-file")
        .arg(&stats)
        .arg("tests/fixtures/gc_volume_trigger_loop.eu")
        .env("EU_PP_THRESHOLD", "999999999");
    let out = run_with_deadline(cmd, Duration::from_secs(300)).expect("eu timed out");
    assert!(
        out.status.success(),
        "eu failed under --heap-limit-mib {limit_mib}: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(String::from_utf8_lossy(&out.stdout).contains("r: 300000"));

    let json: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&stats).unwrap()).unwrap();
    let find = |key: &str| -> u64 {
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
        walk(&json, key).unwrap_or_else(|| panic!("no {key} in statistics"))
    };
    (find("collections_count"), find("peak_heap_blocks"))
}

/// With no heap limit, a program allocating far more than the minimum
/// trigger collects, and its heap stays near that trigger rather than
/// growing with total allocation.
#[test]
fn unbounded_heap_collects_on_allocation_volume() {
    let (collections, peak) = loop_fixture_stats("0");
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

/// A heap limit well above the live data does not make the collector run
/// more often: a capped run collects about as often as an unbounded one,
/// rather than on nearly every poll (the thrashing eu-oaz72.4 reported:
/// 39,326 collections under `--heap-limit-mib 1024` against 0 unbounded).
#[test]
fn heap_limit_above_live_data_does_not_thrash() {
    let (unbounded, _) = loop_fixture_stats("0");
    let (capped, peak) = loop_fixture_stats("1024");
    assert!(
        capped <= unbounded * 2,
        "{capped} collections under --heap-limit-mib 1024 against {unbounded} \
         unbounded (peak {peak} blocks): the limit makes the collector thrash"
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
