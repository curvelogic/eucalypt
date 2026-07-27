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
//!    `.github/workflows/build-rust.yaml` *and* must run
//!    `cargo xtask prelude-compile`. This is the check that turns a dropped
//!    build step from silence into a red build, and it is static: it needs no
//!    blob, no environment variable and no CI wiring, so it runs in every
//!    configuration including the deliberately blob-less "Test Suite" job.
//! 4. **An explicit demand, on request.** Setting `EU_REQUIRE_PRELUDE_BLOB=1`
//!    asserts the blob cfg is actually on, for a job that wants to state the
//!    requirement at runtime rather than rely on (3).
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
                    against. The rest of the file is ungated (eu-3skeg)",
    },
    Gate {
        path: "tests/harness_test.rs",
        cfg: "prelude_blob_ok",
        supplier: Supplier::PreludeBlobJob("diagnostics-blob-mode"),
        rationale: "one gated test compares blob-core and source-prelude \
                    diagnostics byte for byte on a large file; without a blob \
                    both invocations take the source path and the comparison \
                    is a tautology. The rest of the file is ungated \
                    (eu-r4647)",
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

/// The body of one job in `.github/workflows/build-rust.yaml`.
///
/// Jobs are keys at two-space indentation; the body runs to the next such key.
fn workflow_job_body(workflow: &str, job: &str) -> Option<String> {
    let header = format!("  {job}:");
    let mut lines = workflow.lines().skip_while(|l| l.trim_end() != header);
    lines.next()?;
    let body: Vec<&str> = lines
        .take_while(|l| {
            l.trim().is_empty()
                || (l.starts_with("   ") || l.starts_with("  -") || l.starts_with('\t'))
        })
        .collect();
    Some(body.join("\n"))
}

/// The CI job a blob-gated test file depends on must actually build a blob.
///
/// This is the check that would have made a dropped `cargo xtask
/// prelude-compile` step red. It is static — no blob, no env var, no CI wiring
/// required — so it runs everywhere, including the blob-less "Test Suite" job
/// where the gated tests themselves do not exist.
#[test]
fn every_blob_gated_file_names_a_ci_job_that_generates_a_blob() {
    let workflow_path = repo_root().join(".github/workflows/build-rust.yaml");
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
    }
}

/// Let a caller demand the blob cfg rather than infer it.
///
/// Off by default, because the "Test Suite" job is deliberately blob-less to
/// keep the source-prelude fallback exercised. A job that has just run
/// `prelude-compile` can set `EU_REQUIRE_PRELUDE_BLOB=1` to assert the blob
/// really took effect.
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
}
