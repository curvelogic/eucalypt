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

/// Run a fixture with `run --debug-trace`, returning its JSON diagnostic.
fn run_debug_trace(path: &std::path::Path) -> serde_json::Value {
    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .args([
            "run",
            "--debug-trace",
            "--error-format",
            "json",
            "--heap-limit-mib",
            "2048",
        ])
        .arg(path)
        .output()
        .expect("run eu --debug-trace");
    let stdout = String::from_utf8_lossy(&out.stdout).to_string();
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();
    last_json_object(&[&stderr, &stdout]).unwrap_or(serde_json::Value::Null)
}

/// Frame `(kind, name)` pairs from a JSON diagnostic's trace.
fn frames(v: &serde_json::Value) -> Vec<(String, String)> {
    v["trace"]
        .as_array()
        .cloned()
        .unwrap_or_default()
        .iter()
        .map(|f| {
            (
                f["kind"].as_str().unwrap_or_default().to_string(),
                f["name"].as_str().unwrap_or_default().to_string(),
            )
        })
        .collect()
}

/// The default trace is curated and `--debug-trace` restores the raw,
/// uncurated continuation dump (design spec §4.3, eu-1tkk.7.12).
///
/// Every assertion here holds on both engines. The two engines annotate
/// differently enough that *which* fixtures carry a transparent frame in
/// the raw dump varies (see the `hof_bad_arg` note on the unexpected-pass
/// assertion below), so the fixture-specific assertions use `swap_args`
/// and `nth_out_of_range`, measured to behave identically under the
/// default engine and under `EU_HEAPSYN=1`.
#[test]
fn debug_trace_restores_the_uncurated_trace() {
    let dir = std::path::Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/diagnostics/corpus"
    ));

    // Universal: curation never leaves a transparent frame behind, on any
    // fixture. Deleting the transparent-drop step fails this outright.
    for entry in std::fs::read_dir(dir).expect("corpus dir") {
        let p = entry.unwrap().path();
        if p.extension().and_then(|e| e.to_str()) != Some("eu") {
            continue;
        }
        let (v, _, _) = run(&p);
        let curated = frames(&v);
        assert!(
            !curated.iter().any(|(kind, _)| kind == "transparent"),
            "{}: curated trace must contain no transparent frames, got {curated:?}",
            p.file_name().unwrap().to_string_lossy()
        );
    }

    // `nth`'s internal recursion is a transparent frame in the raw dump and
    // is gone from the curated trace: proves `--debug-trace` bypasses
    // curation rather than being an inert flag.
    let path = dir.join("swap_args.eu");
    let raw = frames(&run_debug_trace(&path));
    assert!(
        raw.iter().any(|(kind, _)| kind == "transparent"),
        "swap_args.eu: expected the raw --debug-trace dump to retain a transparent \
         frame, got {raw:?}"
    );
    let (curated_json, _, _) = run(&path);
    assert!(
        frames(&curated_json).is_empty(),
        "swap_args.eu: expected curation to drop every transparent frame, got {:?}",
        frames(&curated_json)
    );

    // `nth` raises at its own edge, so its boundary frame is in the env
    // trace, not the stack trace: the raw dump does not have it and the
    // curated trace recovers it as named context alongside the user anchor.
    let path = dir.join("nth_out_of_range.eu");
    let raw = frames(&run_debug_trace(&path));
    let (curated_json, _, _) = run(&path);
    let curated = frames(&curated_json);
    assert!(
        !raw.iter().any(|(kind, _)| kind == "boundary"),
        "nth_out_of_range.eu: raw dump unexpectedly carries a boundary frame: {raw:?}"
    );
    assert!(
        curated.contains(&("boundary".to_string(), "nth".to_string())),
        "nth_out_of_range.eu: curated trace must name the boundary combinator, got {curated:?}"
    );
    assert!(
        curated.iter().any(|(kind, _)| kind == "user"),
        "nth_out_of_range.eu: curated trace must keep the user anchor, got {curated:?}"
    );
}

/// Secondary "called from here" labels must not excerpt library internals
/// (design spec §4.3, eu-1tkk.7.12).
///
/// A secondary label renders a source excerpt with a marker under it, which
/// only helps when the reader can act on the line it points at. Pointing it
/// into the prelude shows code the user did not write and cannot change.
/// The named boundary combinator is not lost — it moves to the curated
/// `stack trace:` note, which is the one place `[prelude]` may still appear.
///
/// The gate is a count: exactly one `[prelude]` mention in the human
/// rendering of `nth_out_of_range`, namely the trace note's `in 'nth' at
/// [prelude]:NNNN`. Restoring the uncurated secondary labels re-excerpts
/// `nth`'s own body and pushes the count above one.
#[test]
fn secondary_labels_do_not_excerpt_library_internals() {
    let dir = std::path::Path::new(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/diagnostics/corpus"
    ));

    for (fixture, expected) in [("nth_out_of_range.eu", 1usize), ("metadata_span.eu", 0)] {
        let out = Command::new(env!("CARGO_BIN_EXE_eu"))
            .args(["--heap-limit-mib", "2048"])
            .arg(dir.join(fixture))
            .output()
            .expect("run eu");
        let stderr = String::from_utf8_lossy(&out.stderr).to_string();
        let mentions = stderr.matches("[prelude]").count();
        assert_eq!(
            mentions, expected,
            "{fixture}: expected exactly {expected} '[prelude]' mention(s) (the curated \
             trace note only), found {mentions} in:\n{stderr}"
        );
    }
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
    // (iv) trace must be user-anchored: this is checked as a STRICT, independent
    // invariant, not merely as a fallback when the primary happens to fail (i).
    // A diagnostic is valid iff its trace contains at least one user frame, OR
    // the trace is empty AND the primary itself is a user location. This must
    // hold on its own terms — invariant (i) already forces primary.in_user_file
    // for every live guard, so if (iv) were allowed to fall back to
    // `primary_is_user` it could never independently fail, making it vacuous.
    // The strict form also catches the case a loose reading missed: an empty
    // trace paired with a non-user primary is a violation, not a pass.
    let trace = v["trace"].as_array().cloned().unwrap_or_default();
    let has_user = trace.iter().any(|f| f["kind"] == serde_json::json!("user"));
    let primary_is_user = primary["in_user_file"] == serde_json::json!(true);
    let valid_iv = has_user || (trace.is_empty() && primary_is_user);
    if !valid_iv {
        errs.push(
            "(iv) trace not user-anchored: non-empty with no user frame (or empty with non-user primary)"
                .into(),
        );
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
    // The `xfail` markers describe the DEFAULT (bytecode) engine, so the
    // "you fixed it, lock it in" arm is only asserted there (eu-1tkk.7.12).
    //
    // Measured divergence, not a guess: on `hof_bad_arg.eu` the legacy
    // HeapSyn engine's error Smid is the user's own `result` binding
    // (`error_has_user_file: true`), while the bytecode engine's is a
    // `[prelude]` `map` Smid with no user Smid anywhere in either trace.
    // HeapSyn therefore satisfies all five invariants on a fixture the
    // default engine still violates. That is a backend annotation-propagation
    // gap, not a diagnostics-presentation one, and clearing an xfail on the
    // strength of the engine being deleted (eu-1hcw) would be locking in a
    // gain the shipping engine does not have. Tracked by eu-gvci.
    //
    // The `hard_failures` arm above is NOT relaxed: a live guard that
    // regresses fails on either engine.
    let heapsyn = std::env::var("EU_HEAPSYN").as_deref() == Ok("1");
    assert!(
        heapsyn || unexpected_pass.is_empty(),
        "these fixtures now PASS — remove their `xfail` marker to lock the gain:\n{}",
        unexpected_pass.join("\n")
    );
}
