//! Objective invariant gate for eucalypt diagnostics (spec 2026-07-21 §5.2).
//! Runs every corpus fixture through `eu --error-format json` and asserts the five
//! invariants.
//!
//! # Expected failures are per engine
//!
//! A fixture's sidecar may list the engines on which it is expected to
//! VIOLATE the invariants — `xfail_engines = ["bytecode"]` — documenting a
//! known bug. On an engine not in that list the fixture is a live guard. When
//! a bead lands and flips one to passing, the gate fails and tells you to
//! drop the engine from the list, so the gain is locked in forever.
//!
//! It used to be a single engine-blind `xfail = true`, relaxed under
//! `EU_HEAPSYN=1` by an `assert!(heapsyn || …)`. That is unsound in both
//! directions: a fixture violating on one engine only is either marked and
//! silently un-ratcheted on the other, or unmarked and hard-failing on the
//! one where the bug lives. The blanket relaxation additionally meant the
//! ratchet asserted nothing at all under HeapSyn (eu-1tkk.7.17).
//!
//! Two further hazards this file defends against, both instances of a gate
//! that cannot fail (eu-oxtcq):
//!
//! * **A silently inert marker.** [`parse_meta`] rejects an unrecognised
//!   sidecar key rather than skipping it, so a typo (`xfail_engine`,
//!   `xfails`, or the retired `xfail = true`) is a loud parse error, not a
//!   marker that quietly does nothing.
//! * **A vacuous ratchet arm.** No fixture is `xfail` today, so the
//!   "you fixed it, lock it in" branch is never taken by the corpus sweep.
//!   The verdict is therefore a pure function, [`verdict`], with a unit test
//!   for each of its four cases — the arm is provably reachable whatever the
//!   corpus happens to contain.
use std::process::Command;

/// Invariant (v)'s budget, taken from the crate rather than restated here.
///
/// `src/eval/error.rs` documents this constant as "shared by the curated
/// human/JSON trace and the objective invariant gate … centralised here so
/// the two cannot drift apart". A local `const TRACE_BUDGET: usize = 12`
/// in this file made that documentation false: the values agreed, but
/// nothing forced them to, and raising the crate's budget would have left
/// the gate silently enforcing the old one (eu-0cc1). Importing is what
/// makes the stated single source of truth actually single.
use eucalypt::eval::error::TRACE_BUDGET;

/// The evaluation engine the fixture sweep is running under.
///
/// Selected the same way the binary selects it, so the gate and the `eu`
/// child process it spawns always agree: the child inherits `EU_HEAPSYN`
/// from this process.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Engine {
    Bytecode,
    HeapSyn,
}

impl Engine {
    fn current() -> Engine {
        if std::env::var("EU_HEAPSYN").as_deref() == Ok("1") {
            Engine::HeapSyn
        } else {
            Engine::Bytecode
        }
    }

    /// The name used in a sidecar's `xfail_engines` list.
    fn key(self) -> &'static str {
        match self {
            Engine::Bytecode => "bytecode",
            Engine::HeapSyn => "heapsyn",
        }
    }

    fn all() -> [Engine; 2] {
        [Engine::Bytecode, Engine::HeapSyn]
    }
}

struct Meta {
    region: (u32, u32),
    /// Engines on which this fixture is expected to violate the invariants.
    xfail_engines: Vec<Engine>,
}

impl Meta {
    fn is_xfail_on(&self, engine: Engine) -> bool {
        self.xfail_engines.contains(&engine)
    }
}

/// Keys a sidecar may carry. Anything else is a typo and must be loud.
///
/// `mutation`, `description` and `expected_class` are documentation the gate
/// does not read; they are listed so that recognising a key and *acting* on
/// it stay separate concerns.
const KNOWN_KEYS: [&str; 6] = [
    "mutation",
    "description",
    "expected_class",
    "region_start_line",
    "region_end_line",
    "xfail_engines",
];

/// Minimal hand-parse of a fixture sidecar, avoiding a `toml` dev-dependency.
///
/// Strict by design. The previous parser skipped any line it did not
/// recognise, which made a mistyped marker indistinguishable from no marker —
/// the fixture silently became a live guard (or silently stopped being one)
/// with nothing to say so.
fn parse_meta(name: &str, toml_src: &str) -> Meta {
    let mut start: Option<u32> = None;
    let mut end: Option<u32> = None;
    let mut xfail_engines = Vec::new();

    for (n, line) in toml_src.lines().enumerate() {
        let l = line.trim();
        if l.is_empty() || l.starts_with('#') {
            continue;
        }
        let (key, value) = l
            .split_once('=')
            .unwrap_or_else(|| panic!("{name}:{}: not a `key = value` line: {l:?}", n + 1));
        let key = key.trim();
        let value = value.trim();
        assert!(
            KNOWN_KEYS.contains(&key),
            "{name}:{}: unrecognised sidecar key {key:?} (known: {KNOWN_KEYS:?}). \
             A key the gate does not recognise is a marker that silently does \
             nothing — note that the engine-blind `xfail` was replaced by \
             `xfail_engines` (eu-1tkk.7.17).",
            n + 1
        );
        match key {
            "region_start_line" => {
                start = Some(
                    value
                        .parse()
                        .unwrap_or_else(|_| panic!("{name}:{}: bad line number", n + 1)),
                )
            }
            "region_end_line" => {
                end = Some(
                    value
                        .parse()
                        .unwrap_or_else(|_| panic!("{name}:{}: bad line number", n + 1)),
                )
            }
            "xfail_engines" => xfail_engines = parse_engine_list(name, n + 1, value),
            _ => {}
        }
    }

    let region = (
        start.unwrap_or_else(|| panic!("{name}: missing region_start_line")),
        end.unwrap_or_else(|| panic!("{name}: missing region_end_line")),
    );
    assert!(
        region.0 <= region.1 && region.0 >= 1,
        "{name}: nonsensical region {region:?}"
    );
    Meta {
        region,
        xfail_engines,
    }
}

/// Parse `["bytecode", "heapsyn"]`, rejecting an unknown engine name.
fn parse_engine_list(name: &str, line_no: usize, value: &str) -> Vec<Engine> {
    let inner = value
        .strip_prefix('[')
        .and_then(|v| v.strip_suffix(']'))
        .unwrap_or_else(|| panic!("{name}:{line_no}: xfail_engines must be a list: {value:?}"));
    inner
        .split(',')
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .map(|s| {
            let s = s.trim_matches('"');
            Engine::all()
                .into_iter()
                .find(|e| e.key() == s)
                .unwrap_or_else(|| panic!("{name}:{line_no}: unknown engine {s:?}"))
        })
        .collect()
}

/// What the gate should do about one fixture on one engine.
///
/// Extracted as a pure function so that every arm — including the
/// "you fixed it, lock it in" arm the corpus does not currently exercise —
/// is reachable from a unit test rather than only from whatever fixtures
/// happen to be checked in (eu-oxtcq mechanism 9).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Verdict {
    /// Behaved as the sidecar says it should.
    AsExpected,
    /// A live guard that regressed.
    HardFailure,
    /// Marked expected-to-fail on this engine, but it passes: drop the marker.
    UnexpectedPass,
}

fn verdict(is_xfail_on_this_engine: bool, violates: bool) -> Verdict {
    match (is_xfail_on_this_engine, violates) {
        (false, true) => Verdict::HardFailure,
        (true, false) => Verdict::UnexpectedPass,
        (false, false) | (true, true) => Verdict::AsExpected,
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
    run_debug_trace_with(path, &[])
}

/// Run a fixture with `run --debug-trace` plus `extra` flags, returning its
/// JSON diagnostic.
fn run_debug_trace_with(path: &std::path::Path, extra: &[&str]) -> serde_json::Value {
    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .args([
            "run",
            "--debug-trace",
            "--error-format",
            "json",
            "--heap-limit-mib",
            "2048",
        ])
        .args(extra)
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

    // A transparent library frame is present in the raw `--debug-trace` dump
    // and is gone from the curated trace: proves `--debug-trace` bypasses
    // curation rather than being an inert flag. Since the DirectApp call-site
    // fix (eu-1tkk.7) swap_args anchors on the user call site with the named
    // boundary combinator kept as context — the same shape as
    // nth_out_of_range below — where it previously curated to nothing because
    // no user frame reached the trace at all. The test's intent is unchanged:
    // curation drops every *transparent* frame while keeping the user anchor
    // and the boundary combinator.
    let path = dir.join("swap_args.eu");
    let raw = frames(&run_debug_trace(&path));
    assert!(
        raw.iter().any(|(kind, _)| kind == "transparent"),
        "swap_args.eu: expected the raw --debug-trace dump to retain a transparent \
         frame, got {raw:?}"
    );
    let (curated_json, _, _) = run(&path);
    let curated = frames(&curated_json);
    assert!(
        !curated.iter().any(|(kind, _)| kind == "transparent"),
        "swap_args.eu: curation must drop every transparent frame the raw dump \
         carried, got {curated:?}"
    );
    assert!(
        curated.contains(&("boundary".to_string(), "nth".to_string())),
        "swap_args.eu: curated trace must name the boundary combinator, got {curated:?}"
    );
    assert!(
        curated.iter().any(|(kind, _)| kind == "user"),
        "swap_args.eu: curated trace must keep the user anchor, got {curated:?}"
    );

    // `nth` raises at its own edge, so with the prelude compiled from source
    // its boundary frame is in the env trace, not the stack trace: the raw
    // dump does not have it and `curate_trace_with_env` recovers it as named
    // context alongside the user anchor.
    //
    // That "not in the raw stack dump" shape is specific to the
    // source-compiled prelude, where the inliner folds `nth`'s recursion into
    // its caller and the surviving continuations are annotated with `nth`'s
    // inner `aux` helper. Under the shipped prelude blob, `nth` is a real
    // global call and its own frames legitimately reach the raw stack dump
    // (blob mode carries per-binding identity, so those frames are labelled
    // `nth`) — so the env-recovery precondition is asserted against
    // `--source-prelude`, which pins the configuration that exercises that
    // code path rather than leaving it to whether a blob happens to be
    // present (eu-7x0r). The *outcome* below — the curated trace names the
    // boundary and keeps the user anchor — is required of whichever prelude
    // is actually in use, and is asserted unconditionally.
    let path = dir.join("nth_out_of_range.eu");
    let raw_from_source = frames(&run_debug_trace_with(&path, &["--source-prelude"]));
    assert!(
        !raw_from_source.iter().any(|(kind, _)| kind == "boundary"),
        "nth_out_of_range.eu (--source-prelude): raw dump unexpectedly carries a \
         boundary frame: {raw_from_source:?}"
    );
    let (curated_json, _, _) = run(&path);
    let curated = frames(&curated_json);
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

/// Sweep the corpus on whichever engine this process was launched under.
///
/// Both arms bind on both engines. `hard_failures` always did; `unexpected_pass`
/// now does too, because the marker it ratchets is itself per-engine — a
/// fixture listed for `bytecode` only is a live guard under HeapSyn, and one
/// listed for both is ratcheted on whichever engine fixes it first
/// (eu-1tkk.7.17).
///
/// Historical note, for why the old relaxation existed: `hof_bad_arg.eu` used
/// to violate invariants (i)/(iii)/(iv) on the default (bytecode) engine only
/// — its error Smid was a `[prelude]` `map` Smid with no user Smid anywhere in
/// either trace, while HeapSyn's error Smid was already the user's own
/// `result` binding. Root cause: `step`/`step_predecoded` routed an arity>0
/// (partial-application) WHNF value straight to `return_fun` without first
/// refreshing `state.annotation` from the value's own closure annotation —
/// unlike `vm.rs` `handle_instruction`, which does that refresh
/// unconditionally, before its `remaining_arity > 0` check. A stale
/// prelude-internal annotation therefore leaked into any error raised against
/// that value by its caller. Fixed by eu-gvci. That is exactly the shape
/// `xfail_engines = ["bytecode"]` now expresses precisely, instead of
/// switching the whole ratchet off under HeapSyn.
#[test]
fn corpus_satisfies_invariants() {
    let engine = Engine::current();
    let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/diagnostics/corpus");
    let mut hard_failures = vec![];
    let mut unexpected_pass = vec![];
    let mut swept = 0usize;
    for entry in std::fs::read_dir(dir).expect("corpus dir") {
        let p = entry.unwrap().path();
        if p.extension().and_then(|e| e.to_str()) != Some("eu") {
            continue;
        }
        swept += 1;
        let name = p.file_name().unwrap().to_string_lossy().to_string();
        let meta_path = p.with_extension("meta.toml");
        let m = parse_meta(
            &name,
            &std::fs::read_to_string(&meta_path)
                .unwrap_or_else(|_| panic!("{name}: missing {}", meta_path.display())),
        );
        let (v, out, code) = run(&p);
        let errs = violations(&v, &out, code, &m);
        match verdict(m.is_xfail_on(engine), !errs.is_empty()) {
            Verdict::AsExpected => {}
            Verdict::HardFailure => hard_failures.push(format!("{name}: {errs:?}")),
            Verdict::UnexpectedPass => unexpected_pass.push(name),
        }
    }
    // A corpus directory that has gone missing or been renamed would otherwise
    // sweep zero fixtures and report a clean pass (eu-oxtcq mechanism 1).
    assert!(swept > 0, "swept no corpus fixtures from {dir}");
    assert!(
        hard_failures.is_empty(),
        "invariant violations on the {} engine:\n{}",
        engine.key(),
        hard_failures.join("\n")
    );
    assert!(
        unexpected_pass.is_empty(),
        "these fixtures now PASS on the {} engine — remove {:?} from their \
         `xfail_engines` to lock the gain:\n{}",
        engine.key(),
        engine.key(),
        unexpected_pass.join("\n")
    );
}

#[cfg(test)]
mod meta_tests {
    use super::*;

    /// Every arm of the ratchet, including the one the checked-in corpus does
    /// not currently reach. Without this, "no fixture is xfail" would make the
    /// `UnexpectedPass` branch unreachable and the ratchet decorative.
    #[test]
    fn verdict_covers_all_four_cases() {
        assert_eq!(verdict(false, false), Verdict::AsExpected);
        assert_eq!(verdict(false, true), Verdict::HardFailure);
        assert_eq!(verdict(true, true), Verdict::AsExpected);
        assert_eq!(verdict(true, false), Verdict::UnexpectedPass);
    }

    /// An `xfail_engines` list binds only the engines it names.
    #[test]
    fn xfail_is_scoped_to_the_engines_it_names() {
        let m = parse_meta(
            "t",
            "region_start_line = 1\nregion_end_line = 2\nxfail_engines = [\"bytecode\"]\n",
        );
        assert!(m.is_xfail_on(Engine::Bytecode));
        assert!(
            !m.is_xfail_on(Engine::HeapSyn),
            "a bytecode-only xfail must leave the fixture a live guard under HeapSyn"
        );
        assert_eq!(
            verdict(m.is_xfail_on(Engine::HeapSyn), true),
            Verdict::HardFailure
        );
    }

    #[test]
    fn absent_marker_means_live_guard_on_every_engine() {
        let m = parse_meta("t", "region_start_line = 2\nregion_end_line = 2\n");
        for e in Engine::all() {
            assert!(!m.is_xfail_on(e));
        }
    }

    #[test]
    fn comments_and_documentation_keys_are_accepted() {
        let m = parse_meta(
            "t",
            "# a comment\nmutation = \"x\"\ndescription = \"y\"\nexpected_class = \"number\"\n\
             region_start_line = 2\nregion_end_line = 3\n",
        );
        assert_eq!(m.region, (2, 3));
    }

    /// The retired engine-blind key must be a loud error, not a silent no-op.
    #[test]
    #[should_panic(expected = "unrecognised sidecar key")]
    fn the_retired_xfail_key_is_rejected() {
        parse_meta(
            "t",
            "region_start_line = 1\nregion_end_line = 1\nxfail = true\n",
        );
    }

    #[test]
    #[should_panic(expected = "unrecognised sidecar key")]
    fn a_mistyped_marker_is_rejected() {
        parse_meta(
            "t",
            "region_start_line = 1\nregion_end_line = 1\nxfail_engine = [\"bytecode\"]\n",
        );
    }

    #[test]
    #[should_panic(expected = "unknown engine")]
    fn an_unknown_engine_name_is_rejected() {
        parse_meta(
            "t",
            "region_start_line = 1\nregion_end_line = 1\nxfail_engines = [\"predecode\"]\n",
        );
    }

    #[test]
    #[should_panic(expected = "missing region_start_line")]
    fn a_sidecar_without_a_region_is_rejected() {
        parse_meta("t", "description = \"no region\"\n");
    }

    #[test]
    fn engine_selection_matches_the_binarys() {
        // Documents the contract the child process relies on: the gate reads
        // the same variable `eu` does, and the child inherits it.
        assert_eq!(
            Engine::current(),
            if std::env::var("EU_HEAPSYN").as_deref() == Ok("1") {
                Engine::HeapSyn
            } else {
                Engine::Bytecode
            }
        );
    }
}
