use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Extract the last parseable JSON *object* from a stream, scanning bottom-up.
///
/// Why: `eu --error-format json` emits exactly one JSON diagnostic line, but
/// some inputs also trigger non-fatal type-checker WARNINGS, which are
/// rendered human-readable to stderr and printed BEFORE the JSON diagnostic
/// line (bead eu-6q1f). Naively parsing the whole trimmed stream therefore
/// fails for those cases. This mirrors `last_json_object` in
/// `tests/diagnostics_invariants.rs`, which gates the same emitter.
fn last_json_object(stream: &str) -> Option<serde_json::Value> {
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
    None
}

/// Per-call counter so concurrent `run_json` calls (cargo runs tests in this
/// file as threads within one process, so `std::process::id()` alone is
/// shared) each get their own temp directory.
static CASE_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn run_json(src: &str) -> serde_json::Value {
    let n = CASE_COUNTER.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("eu-diag-{}-{n}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("case.eu");
    std::fs::write(&path, src).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .args(["--error-format", "json", "--heap-limit-mib", "2048"])
        .arg(&path)
        .output()
        .expect("run eu");
    // JSON diagnostic may land on stdout or stderr depending on wiring; check
    // both, taking the LAST parseable JSON object from each (see
    // `last_json_object` doc above).
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    last_json_object(&stderr)
        .or_else(|| last_json_object(&stdout))
        .unwrap_or_else(|| panic!("no JSON diagnostic parsed\nstdout={stdout}\nstderr={stderr}"))
}

#[test]
fn json_error_carries_user_location_for_direct_type_error() {
    // Direct user-site type error: JSON must locate it in the user file, not lose the location.
    let v = run_json("a: \"3\"\nresult: a + 1\n");
    let primary = &v["primary"];
    assert_eq!(
        primary["in_user_file"],
        serde_json::json!(true),
        "primary must be in the user file, got {v}"
    );
    assert_eq!(primary["line"], serde_json::json!(2));
}

#[test]
fn type_error_carries_a_stable_code() {
    let v = run_json("a: \"3\"\nresult: a + 1\n");
    assert_eq!(
        v["code"],
        serde_json::json!("EU-EVAL-TYPE"),
        "type mismatch must carry the stable EU-EVAL-TYPE code, got {v}"
    );
}

#[test]
fn error_command_prints_catalogue_entry() {
    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .args(["error", "EU-EVAL-TYPE"])
        .output()
        .expect("run eu error");
    assert!(
        out.status.success(),
        "eu error EU-EVAL-TYPE should exit 0, stderr={}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout).to_lowercase();
    assert!(
        stdout.contains("type"),
        "catalogue entry should mention 'type', got: {stdout}"
    );
}
