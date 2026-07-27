//! Regression test for eu-1tkk.2: the prelude's own deprecations must reach
//! the type checker on the **evaluate** path in **blob** mode.
//!
//! `deprecated` metadata is consumed by the desugarer
//! (`Desugarer::record_deprecation`) and does not survive as runtime
//! `Expr::Meta`, so the blob's baked `desugared_unit_cores` cannot supply it.
//! `run_type_checker_from_blob_core` used to build its injected prelude
//! `TranslationUnit` with `deprecations: Default::default()`, which made every
//! `deprecated` declaration in `lib/prelude.eu` silently inert in the shipped
//! default configuration while still working under `eu check` (which loads the
//! prelude from source). `PreludeBlob::deprecations` now carries the table, the
//! same desugar-phase side channel `PreludeBlob::blame` already used.
//!
//! Why a separate file rather than a typecheck fixture: `run_typecheck_test`
//! shells out to `eu check`, the one path where this always worked, so a
//! fixture alone cannot discriminate. Fixture 115 does now also drive the
//! evaluate path (eu-ntwg.1's eval-path coverage), but only this file states
//! the blob-mode contract directly. That `run_type_checker_from_blob_core` is
//! reachable only from the eval path, and is therefore invisible to every
//! `eu check`-based test, is the general hole tracked as **eu-vbctt**; this
//! test closes one instance of it and does not subsume the bead.
//!
//! Gated on `cfg(prelude_blob_ok)` like `diagnostics_blame_plumbing_test.rs`:
//! in a build with no `lib/prelude.blob` the eval path falls back to
//! `run_type_checker` and there is no blob behaviour to assert. The
//! "Bytecode + blob harness" and "GC-verified harness" CI jobs generate the
//! blob first and so do run this file.
#![cfg(prelude_blob_ok)]

use std::process::Command;

/// Run `eu --strict <file>` (the evaluate path, no subcommand) on `src` and
/// return its exit code and stderr.
fn run_eval_strict(name: &str, src: &str) -> (i32, String) {
    let dir = std::env::temp_dir().join(format!("eu-blob-deprecation-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join(format!("{name}.eu"));
    std::fs::write(&path, src).unwrap();

    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .args(["--strict", "--heap-limit-mib", "2048"])
        .arg(&path)
        .output()
        .expect("run eu");

    (
        out.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

/// The blob must carry what `lib/prelude.eu` declared.
#[test]
fn embedded_blob_carries_the_preludes_deprecation_table() {
    use eucalypt::eval::stg::blob::PreludeBlob;

    let blob = PreludeBlob::from_bytes(eucalypt::driver::resources::PRELUDE_BLOB_BYTES)
        .expect("embedded blob should deserialise");

    let spec = blob.deprecations.get("random.exec").unwrap_or_else(|| {
        panic!(
            "expected 'random.exec' in the blob's deprecation table; found: {:?}",
            blob.deprecations.keys().collect::<Vec<_>>()
        )
    });
    assert_eq!(spec.replacement.as_deref(), Some("run"));
}

/// End-to-end: referencing the prelude's deprecated `random.exec` on the
/// evaluate path must warn and, under `--strict`, fail.
#[test]
fn eval_path_warns_on_deprecated_prelude_member() {
    let (code, stderr) = run_eval_strict(
        "prelude-member",
        "result: random.eval(random.float, random.exec(random.float, random.stream(1)))\n",
    );

    assert!(
        stderr.contains("'random.exec' is deprecated"),
        "eval path in blob mode did not report the prelude's own deprecation.\nstderr:\n{stderr}"
    );
    assert_eq!(
        code, 1,
        "--strict should fail on a deprecation warning.\nstderr:\n{stderr}"
    );
}

/// The complement, so the test above cannot pass by warning about everything:
/// a program that references nothing deprecated stays silent and exits 0 even
/// with the prelude's table loaded.
#[test]
fn eval_path_stays_silent_without_a_deprecated_reference() {
    let (code, stderr) = run_eval_strict(
        "no-deprecated-reference",
        "result: random.eval(random.float, random.stream(1))\n",
    );

    assert!(
        !stderr.contains("is deprecated"),
        "unexpected deprecation warning.\nstderr:\n{stderr}"
    );
    assert_eq!(code, 0, "stderr:\n{stderr}");
}

/// A user binding whose bare name is the leaf of a deprecated prelude member
/// must stay silent on this path too — the blob's table is keyed by the path a
/// caller writes, so loading it must not deprecate the name `exec` globally.
#[test]
fn eval_path_does_not_deprecate_the_bare_leaf_name() {
    let (code, stderr) = run_eval_strict("bare-leaf", "exec(cmd): cmd\nresult: exec(\"hello\")\n");

    assert!(
        !stderr.contains("is deprecated"),
        "a user's own 'exec' must not inherit random.exec's deprecation.\nstderr:\n{stderr}"
    );
    assert_eq!(code, 0, "stderr:\n{stderr}");
}
