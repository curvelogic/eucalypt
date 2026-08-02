//! Meta-gate: a conditionally compiled test file must not be able to
//! contribute zero tests in silence (eu-oxtcq mechanism 1).
//!
//! # The failure mode
//!
//! `build.rs` emits `prelude_blob_ok` only when `lib/prelude.blob` is present
//! and hashes against the current `lib/prelude.eu`. A test file gated on that
//! cfg therefore compiles to *nothing at all* without a blob — and a test
//! binary containing no tests reports
//!
//! ```text
//! test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
//! ```
//!
//! and exits 0. Absent coverage is indistinguishable from passing coverage.
//! Measured on master: with no blob present, `tests/diagnostics_blame_plumbing_test.rs`
//! contributes 0 tests and `cargo test` is green.
//!
//! That is survivable while a CI job reliably supplies the cfg. It stops being
//! survivable the moment someone reorders, renames or drops the
//! `cargo xtask prelude-compile` step in that job: the gated assertions vanish
//! and nothing anywhere goes red.
//!
//! # What this file enforces
//!
//! 1. **Registration.** Every conditionally compiled test file under `tests/`
//!    must appear in [`REGISTRY`] below. A new gate cannot be added without a
//!    reviewer seeing an entry appear, which is the part a one-off audit
//!    cannot give you — the audit catches today's gates, the registry catches
//!    tomorrow's.
//! 2. **No rot.** A registered file that no longer exists, or no longer
//!    carries the cfg it is registered for, fails. The registry cannot drift
//!    into describing a world that no longer exists.
//! 3. **A supplier that actually supplies.** For every gate whose cfg comes
//!    from the prelude blob, the CI job named in its entry must exist in
//!    `.github/workflows/build-rust.yaml`, must run `cargo xtask
//!    prelude-compile`, *and must actually run the target*. This is the check
//!    that turns a dropped build step from silence into a red build, and it is
//!    static: it needs no blob, no environment variable and no CI wiring, so it
//!    runs in every configuration including the deliberately blob-less
//!    "Test Suite (source-prelude fallback, no blob)" job.
//!
//!    The "must actually run the target" half was added by eu-1tkk.7.43, and
//!    it was not pedantry. Without it, `tests/harness_test.rs` and
//!    `tests/wire_format_enforcement_test.rs` both named a supplier job that
//!    built a blob and never ran them — so their blob-gated tests executed in
//!    no CI job at all, while this file certified the arrangement as sound.
//! 4. **An explicit demand, on request.** Setting `EU_REQUIRE_PRELUDE_BLOB=1`
//!    asserts the blob cfg is actually on, for a job that wants to state the
//!    requirement at runtime rather than rely on (3). The blob-mode job sets
//!    it, and sweeps this target, so the demand is live.
//! 5. **A discovery rule that stays in step.** `scripts/blob-mode-tests.sh`
//!    decides which `cargo test` targets to run under a blob by scanning the
//!    tree for two markers. [`SWEEP_MARKERS`] models that rule here, and a
//!    test asserts the script still uses those exact markers — so the sweep
//!    and this registry cannot drift into disagreeing about what is
//!    mode-sensitive.
//!
//! This file is deliberately never itself conditionally compiled — it is the
//! thing that detects conditional compilation, so gating it would be
//! self-defeating. It is excluded from its own scan for that reason. If it
//! ever did compile to zero tests, `scripts/diagnostics-gate.sh`'s
//! zero-test check would catch it.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// Where a gate's cfg comes from, and therefore what must be true for the
/// gated tests to exist.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Supplier {
    /// `build.rs` sets the cfg only when a verified `lib/prelude.blob` is
    /// present. The named job (a key under `jobs:` in
    /// `.github/workflows/build-rust.yaml`) must generate one.
    PreludeBlobJob(&'static str),
    /// Active on every native target; only a `wasm32` build compiles these
    /// out, and no `wasm32` job runs a test *target*, so nothing is lost.
    NativeTargetsOnly,
}

struct Gate {
    /// Path relative to the repository root.
    path: &'static str,
    /// The cfg predicate, exactly as written in the source.
    cfg: &'static str,
    supplier: Supplier,
    /// Why the gate exists — what the tests assert that is meaningless
    /// without it.
    rationale: &'static str,
}

/// Every conditionally compiled test file, and what supplies its cfg.
///
/// Adding a gate without adding an entry here fails
/// `every_conditionally_compiled_test_file_is_registered`.
const REGISTRY: &[Gate] = &[
    Gate {
        path: "tests/diagnostics_blame_plumbing_test.rs",
        cfg: "prelude_blob_ok",
        supplier: Supplier::PreludeBlobJob("diagnostics-blob-mode"),
        rationale: "asserts blob-mode Smid reconstruction and primary-label \
                    blame; under the source-prelude fallback there is no blob \
                    to reconstruct from, so the assertions are meaningless \
                    rather than merely untested",
    },
    Gate {
        path: "tests/tick_parity_test.rs",
        cfg: "not(target_arch = \"wasm32\")",
        supplier: Supplier::NativeTargetsOnly,
        rationale: "spawns the eu binary and counts ticks; there is no binary \
                    to spawn under wasm32",
    },
    Gate {
        path: "tests/fold_over_map_growth_test.rs",
        cfg: "not(target_arch = \"wasm32\")",
        supplier: Supplier::NativeTargetsOnly,
        rationale: "spawns the eu binary and compares tick counts across a 4x \
                    size increase to gate the complexity class of `xs map(f) \
                    fold` (eu-wpswc); there is no binary to spawn under \
                    wasm32, and it also needs tempfile, a non-wasm32 \
                    dev-dependency",
    },
    Gate {
        path: "tests/property_test.rs",
        cfg: "not(target_arch = \"wasm32\")",
        supplier: Supplier::NativeTargetsOnly,
        rationale: "proptest-driven evaluation over the native runtime",
    },
    Gate {
        path: "tests/fuzz_regression_test.rs",
        cfg: "not(target_arch = \"wasm32\")",
        supplier: Supplier::NativeTargetsOnly,
        rationale: "replays fuzzer corpus files through the native pipeline",
    },
    Gate {
        path: "tests/blob_deprecation_test.rs",
        cfg: "prelude_blob_ok",
        supplier: Supplier::PreludeBlobJob("diagnostics-blob-mode"),
        rationale: "asserts that the blob carries the prelude's own \
                    deprecation table and that the evaluate path reports it; \
                    without a blob the eval path falls back to \
                    run_type_checker and there is no blob behaviour to \
                    assert (eu-1tkk.2)",
    },
    Gate {
        path: "tests/wire_format_enforcement_test.rs",
        cfg: "prelude_blob_ok",
        supplier: Supplier::PreludeBlobJob("diagnostics-blob-mode"),
        rationale: "one gated test re-derives the blob source hash from the \
                    files on disk and compares it with the embedded blob's; \
                    with no blob embedded there is nothing to compare \
                    against. The rest of the file is ungated (eu-3skeg). Ran \
                    in no CI job at all until eu-1tkk.7.43 brought it into the \
                    sweep",
    },
    Gate {
        path: "tests/harness_test.rs",
        cfg: "prelude_blob_ok",
        supplier: Supplier::PreludeBlobJob("diagnostics-blob-mode"),
        rationale: "two gated tests compare blob-core and source-prelude \
                    diagnostics byte for byte on the eval path; without a blob \
                    both invocations take the source path and the comparison \
                    is a tautology (eu-r4647). The rest of the file is \
                    ungated, and includes the 174 tests/harness/errors \
                    fixtures with .expect sidecars — the diagnostics quality \
                    surface, which ran only against the source prelude until \
                    eu-1tkk.7.43 brought this target into the sweep",
    },
    Gate {
        path: "tests/diagnostics_snapshots.rs",
        cfg: "prelude_blob_ok",
        supplier: Supplier::PreludeBlobJob("diagnostics-blob-mode"),
        rationale: "blob_prelude_snapshots_match renders every fixture through \
                    the pre-compiled prelude — the rendering a released binary \
                    produces — and divergence_inventory_is_current compares it \
                    with the source-prelude rendering; neither exists without a \
                    blob to render through. The job additionally asserts both \
                    test names appear in `--list`, because this file has \
                    ungated tests too and a target-level zero-test guard would \
                    stay green with only those running (eu-1tkk.7.4)",
    },
];

/// The path of the sweep that runs mode-sensitive targets under a blob.
const SWEEP_SCRIPT: &str = "scripts/blob-mode-tests.sh";

/// The markers `scripts/blob-mode-tests.sh` scans for when deciding whether a
/// `cargo test` target is prelude-mode-sensitive, modelled here so this file
/// can answer "would the sweep pick this target up?" (eu-1tkk.7.43).
///
/// `CARGO_BIN_EXE_eu` means the target spawns the `eu` binary — the only
/// caller of `driver::eval::maybe_load_prelude_blob` besides the in-process
/// tester — so the prelude mode is a genuine input to what it asserts.
/// `cfg(prelude_blob_ok)` means part of the target does not merely go untested
/// without a blob, it ceases to exist.
const SWEEP_MARKERS: [&str; 2] = ["CARGO_BIN_EXE_eu", "cfg(prelude_blob_ok)"];

/// Substrings that mark a file as conditionally compiled for the purposes of
/// this check.
///
/// The crate-level form is what makes a whole target vanish; the two
/// build-script cfgs are tracked wherever they appear, including on an
/// individual `#[test]`, because a gated test inside an ungated file
/// disappears just as quietly.
fn gate_markers() -> [String; 3] {
    [
        // Built by concatenation so that this file does not match its own scan
        // through the doc comments above.
        concat!("#!", "[cfg(").to_string(),
        "cfg(prelude_blob_ok)".to_string(),
        "cfg(prelude_blob_stale)".to_string(),
    ]
}

/// Every `.rs` file under `tests/`, recursively, relative to the repo root.
fn test_sources() -> Vec<String> {
    fn walk(dir: &Path, root: &Path, out: &mut Vec<String>) {
        let entries = match std::fs::read_dir(dir) {
            Ok(e) => e,
            Err(_) => return,
        };
        for entry in entries.flatten() {
            let p = entry.path();
            if p.is_dir() {
                walk(&p, root, out);
            } else if p.extension().and_then(|e| e.to_str()) == Some("rs") {
                out.push(
                    p.strip_prefix(root)
                        .expect("under root")
                        .to_string_lossy()
                        .replace('\\', "/"),
                );
            }
        }
    }
    let root = repo_root();
    let mut out = vec![];
    walk(&root.join("tests"), &root, &mut out);
    out.sort();
    out
}

/// This file, which is excluded from the scan (see the module docs).
const SELF_PATH: &str = "tests/gate_liveness.rs";

fn is_gated(source: &str) -> bool {
    gate_markers().iter().any(|m| source.contains(m.as_str()))
}

/// A gate must be registered before it can hide anything.
#[test]
fn every_conditionally_compiled_test_file_is_registered() {
    let root = repo_root();
    let registered: BTreeSet<&str> = REGISTRY.iter().map(|g| g.path).collect();
    let sources = test_sources();
    assert!(
        sources.len() > 5,
        "scanned only {} files under tests/ — the walk is broken, and a broken \
         walk finds no unregistered gates",
        sources.len()
    );

    let mut unregistered = vec![];
    for rel in &sources {
        if rel == SELF_PATH {
            continue;
        }
        let src = std::fs::read_to_string(root.join(rel)).expect("read test source");
        if is_gated(&src) && !registered.contains(rel.as_str()) {
            unregistered.push(rel.clone());
        }
    }
    assert!(
        unregistered.is_empty(),
        "these test files are conditionally compiled but are not in REGISTRY in \
         {SELF_PATH}:\n  {}\n\nA cfg-gated test file contributes ZERO tests when \
         its cfg is off, and a test binary with zero tests reports \"ok. 0 passed\" \
         and exits 0 — so the coverage silently disappears. Add an entry saying \
         which cfg it needs and which CI job supplies it (eu-oxtcq).",
        unregistered.join("\n  ")
    );
}

/// The registry must describe the tree as it is, not as it was.
#[test]
fn no_registry_entry_is_stale() {
    let root = repo_root();
    for gate in REGISTRY {
        let path = root.join(gate.path);
        assert!(
            path.exists(),
            "REGISTRY names {} but no such file exists — delete the entry",
            gate.path
        );
        let src = std::fs::read_to_string(&path).expect("read registered source");
        assert!(
            src.contains(&format!("cfg({})", gate.cfg)),
            "REGISTRY says {} is gated on `cfg({})`, but its source does not \
             contain that predicate. Either the gate was removed (delete the \
             entry) or the predicate changed (update it) — a registry that \
             describes a gate nobody has any more is worse than no registry.",
            gate.path,
            gate.cfg
        );
        assert!(
            !gate.rationale.is_empty(),
            "{}: a gate needs a stated reason",
            gate.path
        );
    }
}

/// The body of one job in `.github/workflows/build-rust.yaml`, **with comment
/// lines removed**.
///
/// Jobs are keys at two-space indentation; the body runs to the next such key.
///
/// Dropping comments is load-bearing, not tidiness. These jobs are heavily
/// commented, and the comments name the very commands the checks below look
/// for — so a `run:` line could be deleted while a paragraph explaining it
/// remained, and every check here would go on passing. Found by fault
/// injection while writing eu-1tkk.7.43: replacing `run:
/// scripts/blob-mode-tests.sh` with a no-op left all of these green, because
/// the comment above it still mentioned the script.
fn workflow_job_body(workflow: &str, job: &str) -> Option<String> {
    let header = format!("  {job}:");
    let mut lines = workflow.lines().skip_while(|l| l.trim_end() != header);
    lines.next()?;
    let body: Vec<&str> = lines
        .take_while(|l| {
            l.trim().is_empty()
                || (l.starts_with("   ") || l.starts_with("  -") || l.starts_with('\t'))
        })
        .filter(|l| !l.trim_start().starts_with('#'))
        .collect();
    Some(body.join("\n"))
}

/// The cargo test target name for a `tests/<name>.rs` path.
fn target_name(path: &str) -> &str {
    path.trim_start_matches("tests/").trim_end_matches(".rs")
}

/// The CI job a blob-gated test file depends on must build a blob *and run the
/// target*.
///
/// This is the check that would have made a dropped `cargo xtask
/// prelude-compile` step red. It is static — no blob, no env var, no CI wiring
/// required — so it runs everywhere, including the blob-less
/// "Test Suite (source-prelude fallback, no blob)" job where the gated tests
/// themselves do not exist.
///
/// The second half is the eu-1tkk.7.43 addition. Building a blob and then not
/// running the target leaves the gated tests exactly as absent as having no
/// blob at all, but with a registry entry asserting otherwise — which is worse
/// than no entry, because it reads as coverage. Two files were in precisely
/// that state when this was written.
///
/// A job may satisfy it either by naming the target (`--test <name>`, or
/// `diagnostics-gate.sh <name>`) or by running the sweep, which discovers the
/// target from the tree.
#[test]
fn every_blob_gated_file_names_a_ci_job_that_runs_it_under_a_blob() {
    let root = repo_root();
    let workflow_path = root.join(".github/workflows/build-rust.yaml");
    let workflow = std::fs::read_to_string(&workflow_path).expect("read build-rust.yaml");

    for gate in REGISTRY {
        let Supplier::PreludeBlobJob(job) = gate.supplier else {
            continue;
        };
        let body = workflow_job_body(&workflow, job).unwrap_or_else(|| {
            panic!(
                "{} is gated on `cfg({})`, whose only supplier is the CI job `{job}` — \
                 but there is no such job in {}. Without that job the gated tests run \
                 nowhere at all, silently.",
                gate.path,
                gate.cfg,
                workflow_path.display()
            )
        });
        assert!(
            body.contains("prelude-compile"),
            "CI job `{job}` is the supplier for {} (gated on `cfg({})`), but it does \
             not run `cargo xtask prelude-compile`. Without that step build.rs sets \
             `prelude_blob_stale`, the gated file compiles to ZERO tests, and the job \
             reports green having asserted nothing (eu-oxtcq mechanism 1).",
            gate.path,
            gate.cfg
        );

        let target = target_name(gate.path);
        let named = body.contains(&format!("--test {target}"))
            || body.contains(&format!("diagnostics-gate.sh {target}"));
        let source = std::fs::read_to_string(root.join(gate.path)).expect("read gate source");
        let swept = body.contains(SWEEP_SCRIPT) && SWEEP_MARKERS.iter().any(|m| source.contains(m));
        assert!(
            named || swept,
            "CI job `{job}` is the supplier for {} but never runs the `{target}` target: \
             it neither names it (`--test {target}`) nor runs `{SWEEP_SCRIPT}` in a way \
             that would discover it. Building a blob and not running the target leaves \
             the gated tests as absent as having no blob at all, while this registry \
             claims they are covered (eu-1tkk.7.43).",
            gate.path
        );
    }
}

/// Some blob-generating job must run the sweep, or the derived target list is
/// a list nobody reads.
///
/// The sweep is what makes prelude mode an explicit dimension rather than an
/// emergent property of which job happens to run which target: it discovers
/// every mode-sensitive target from the tree, so a new one is covered on its
/// first CI run with no workflow edit. Deleting the step would restore the old
/// allowlist behaviour silently.
#[test]
fn a_blob_generating_job_runs_the_mode_sensitive_sweep() {
    let workflow_path = repo_root().join(".github/workflows/build-rust.yaml");
    let workflow = std::fs::read_to_string(&workflow_path).expect("read build-rust.yaml");

    // Job keys are the two-space-indented keys under `jobs:`.
    let jobs: Vec<&str> = workflow
        .lines()
        .filter(|l| l.starts_with("  ") && !l.starts_with("   ") && l.trim_end().ends_with(':'))
        .map(|l| l.trim().trim_end_matches(':'))
        .collect();

    let mut suppliers = vec![];
    for job in &jobs {
        if let Some(body) = workflow_job_body(&workflow, job) {
            if body.contains(SWEEP_SCRIPT) && body.contains("prelude-compile") {
                suppliers.push(*job);
            }
        }
    }
    assert!(
        !suppliers.is_empty(),
        "no job in {} both runs `cargo xtask prelude-compile` and runs `{SWEEP_SCRIPT}`. \
         Without one, no `cargo test` target runs under a prelude blob at all, and every \
         `cfg(prelude_blob_ok)` test in the tree is compiled out everywhere (eu-1tkk.7.43).",
        workflow_path.display()
    );
}

/// The sweep's discovery rule and this file's model of it must not drift.
///
/// [`SWEEP_MARKERS`] is how the checks above answer "would the sweep pick this
/// target up?". If the script changed its markers and this file did not, those
/// answers would be confident and wrong.
#[test]
fn the_sweep_script_scans_for_the_markers_this_file_models() {
    let path = repo_root().join(SWEEP_SCRIPT);
    let script = std::fs::read_to_string(&path).unwrap_or_else(|e| {
        panic!(
            "cannot read {} — the sweep is the only thing running cargo test targets \
             under a blob: {e}",
            path.display()
        )
    });
    for marker in SWEEP_MARKERS {
        assert!(
            script.contains(marker),
            "{} no longer scans for `{marker}`, but SWEEP_MARKERS in {SELF_PATH} still \
             says it does. Either the discovery rule changed (update SWEEP_MARKERS) or \
             the scan lost a marker (restore it) — a model that disagrees with the \
             script makes the coverage checks above answer confidently and wrongly.",
            path.display()
        );
    }
}

/// Let a caller demand the blob cfg rather than infer it.
///
/// Off by default, because the "Test Suite (source-prelude fallback, no blob)"
/// job is deliberately blob-less to keep that fallback exercised. A job that
/// has just run `prelude-compile` can set `EU_REQUIRE_PRELUDE_BLOB=1` to assert
/// the blob really took effect; "Blob-mode cargo tests" does, at job level, and
/// sweeps this target, so the demand is live rather than merely available.
///
/// Worth knowing when reproducing locally: restoring a blob with `mv` or
/// `cp -p` preserves its mtime, so cargo does not rerun `build.rs` and the
/// stale cfg persists. `touch build.rs` after restoring, and read the verdict
/// from the build-script output (`target/debug/build/eucalypt-*/output`),
/// never from the blob file merely existing.
#[test]
fn prelude_blob_is_present_when_demanded() {
    if std::env::var("EU_REQUIRE_PRELUDE_BLOB").as_deref() != Ok("1") {
        return;
    }
    // Bound through a `let` rather than asserted inline: `cfg!` folds to a
    // literal, and clippy's `assertions_on_constants` rejects `assert!(false)`
    // written directly — which is precisely the case this must still handle.
    let blob_ok = cfg!(prelude_blob_ok);
    assert!(
        blob_ok,
        "EU_REQUIRE_PRELUDE_BLOB=1 but this build has `prelude_blob_stale`: \
         lib/prelude.blob is missing, unparseable, or does not hash against the \
         current lib/prelude.eu. Every `cfg(prelude_blob_ok)` test file is \
         compiled out and contributing nothing. Run `cargo xtask prelude-compile`."
    );
}

/// Announce, on every run, which registered gates are inactive here.
///
/// Not an assertion — being blob-less is a legitimate configuration. It exists
/// so that a developer reading test output can see that some coverage is
/// absent, instead of the absence looking exactly like success.
#[test]
fn inactive_gates_are_announced() {
    let blob_ok = cfg!(prelude_blob_ok);
    for gate in REGISTRY {
        if matches!(gate.supplier, Supplier::PreludeBlobJob(_)) && !blob_ok {
            eprintln!(
                "NOTE: {} is compiled out here (no verified prelude blob); its \
                 assertions — {} — are not running. Supplier: {:?}.",
                gate.path, gate.rationale, gate.supplier
            );
        }
    }
}

#[cfg(test)]
mod scan_tests {
    use super::*;

    #[test]
    fn detects_a_crate_level_gate() {
        let src = format!("{}prelude_blob_ok)]\nfn x() {{}}\n", concat!("#!", "[cfg("));
        assert!(is_gated(&src));
    }

    #[test]
    fn detects_a_gate_on_a_single_test() {
        assert!(is_gated("#[cfg(prelude_blob_ok)]\n#[test]\nfn x() {}\n"));
        assert!(is_gated("#[cfg(prelude_blob_stale)]\nfn y() {}\n"));
    }

    #[test]
    fn an_ordinary_test_file_is_not_gated() {
        assert!(!is_gated("#[test]\nfn x() { assert!(true); }\n"));
        // `#[cfg(test)]` on an inner module is not a target-level gate.
        assert!(!is_gated("#[cfg(test)]\nmod t { #[test] fn x() {} }\n"));
    }

    #[test]
    fn job_body_extraction_finds_the_right_job() {
        let wf = "jobs:\n  alpha:\n    steps:\n      - run: cargo xtask prelude-compile\n  beta:\n    steps:\n      - run: cargo test\n";
        assert!(workflow_job_body(wf, "alpha")
            .unwrap()
            .contains("prelude-compile"));
        assert!(!workflow_job_body(wf, "beta")
            .unwrap()
            .contains("prelude-compile"));
        assert!(workflow_job_body(wf, "gamma").is_none());
    }

    /// A comment naming a command must not stand in for running it.
    #[test]
    fn job_body_extraction_drops_comments() {
        let wf = "jobs:\n  alpha:\n    steps:\n      # we run scripts/blob-mode-tests.sh here\n      - run: echo skipped\n";
        let body = workflow_job_body(wf, "alpha").unwrap();
        assert!(!body.contains("blob-mode-tests.sh"));
        assert!(body.contains("echo skipped"));
    }
}
