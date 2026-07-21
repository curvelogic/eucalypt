//! Objective invariant gate for eucalypt diagnostics (spec 2026-07-21 §5.2).
//! Runs every corpus fixture through `eu --error-format json` and asserts the five
//! invariants. Fixtures marked `xfail = true` in their sidecar are expected to VIOLATE
//! (documenting a known bug); when a bead lands and flips one to passing, remove the
//! marker so the gate guards it forever.
use std::process::Command;

const TRACE_BUDGET: usize = 12;

struct Meta {
    region: (u32, u32),
    xfail: bool,
}

fn parse_meta(toml_src: &str) -> Meta {
    // Minimal hand-parse to avoid a toml dev-dep; the sidecar format is fixed and simple.
    let mut start = 0u32;
    let mut end = 0u32;
    let mut xfail = false;
    for line in toml_src.lines() {
        let l = line.trim();
        if let Some(v) = l.strip_prefix("region_start_line") {
            start = v.trim_start_matches([' ', '=']).trim().parse().unwrap();
        } else if let Some(v) = l.strip_prefix("region_end_line") {
            end = v.trim_start_matches([' ', '=']).trim().parse().unwrap();
        } else if let Some(v) = l.strip_prefix("xfail ") {
            xfail = v.contains("true");
        } else if l == "xfail = true" {
            xfail = true;
        }
    }
    Meta {
        region: (start, end),
        xfail,
    }
}

/// Extract the JSON diagnostic from a process's combined output.
///
/// Why this is needed: `eu --error-format json` emits exactly one JSON diagnostic
/// line, but some fixtures (e.g. `swap_args`) *also* trigger non-fatal type-checker
/// WARNINGS, which are rendered HUMAN-readable to stderr and printed BEFORE the JSON
/// diagnostic line. That means `stderr.trim()` as a whole is not valid JSON for those
/// fixtures — naively parsing the entire stream fails. The one line that IS the JSON
/// diagnostic is always the last thing eu prints (a single `eprintln!` of one object),
/// on stdout or stderr depending on wiring. So: scan candidate streams from the bottom
/// and return the first line that parses as a JSON *object* — that is the diagnostic,
/// regardless of how much human-rendered warning noise precedes it.
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

fn run(path: &std::path::Path) -> (serde_json::Value, String, Option<i32>) {
    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .args(["--error-format", "json", "--heap-limit-mib", "2048"])
        .arg(path)
        .output()
        .expect("run eu");
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();
    // JSON diagnostic may land on stdout or stderr depending on wiring; check both,
    // taking the LAST parseable JSON object from each (see last_json_object doc).
    let v = last_json_object(&[&stderr, &stdout]).unwrap_or(serde_json::Value::Null);
    (v, format!("{stdout}{stderr}"), out.status.code())
}

fn violations(v: &serde_json::Value, all_output: &str, code: Option<i32>, m: &Meta) -> Vec<String> {
    let mut errs = vec![];
    // (ii) no panic — checked first; a panic makes the rest meaningless.
    if all_output.contains("panicked") || code.is_none() {
        errs.push(format!("(ii) panic/abort: code={code:?}"));
        return errs;
    }
    if v.is_null() {
        errs.push("no JSON diagnostic parsed".into());
        return errs;
    }
    let primary = &v["primary"];
    if primary["in_user_file"] != serde_json::json!(true) {
        errs.push(format!("(i) primary not in user file: {primary}"));
    }
    if let Some(line) = primary["line"].as_u64() {
        let l = line as u32;
        if l < m.region.0 || l > m.region.1 {
            errs.push(format!(
                "(iii) primary line {l} outside region {:?}",
                m.region
            ));
        }
    } else {
        errs.push("(iii) primary has no line".into());
    }
    let trace = v["trace"].as_array().cloned().unwrap_or_default();
    let has_user = trace.iter().any(|f| f["kind"] == serde_json::json!("user"));
    let primary_is_user = primary["in_user_file"] == serde_json::json!(true);
    if !trace.is_empty() && !has_user && !primary_is_user {
        errs.push("(iv) trace present but no user frame".into());
    }
    if trace.len() > TRACE_BUDGET {
        errs.push(format!(
            "(v) trace length {} > budget {TRACE_BUDGET}",
            trace.len()
        ));
    }
    errs
}

#[test]
fn corpus_satisfies_invariants() {
    let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/diagnostics/corpus");
    let mut hard_failures = vec![];
    let mut unexpected_pass = vec![];
    for entry in std::fs::read_dir(dir).expect("corpus dir") {
        let p = entry.unwrap().path();
        if p.extension().and_then(|e| e.to_str()) != Some("eu") {
            continue;
        }
        let meta_path = p.with_extension("meta.toml");
        let m = parse_meta(&std::fs::read_to_string(&meta_path).expect("meta"));
        let (v, out, code) = run(&p);
        let errs = violations(&v, &out, code, &m);
        let name = p.file_name().unwrap().to_string_lossy().to_string();
        match (m.xfail, errs.is_empty()) {
            (false, false) => hard_failures.push(format!("{name}: {errs:?}")),
            (true, true) => unexpected_pass.push(name), // fixed! remove the xfail marker.
            _ => {}
        }
    }
    assert!(
        hard_failures.is_empty(),
        "invariant violations:\n{}",
        hard_failures.join("\n")
    );
    assert!(
        unexpected_pass.is_empty(),
        "these fixtures now PASS — remove their `xfail` marker to lock the gain:\n{}",
        unexpected_pass.join("\n")
    );
}
