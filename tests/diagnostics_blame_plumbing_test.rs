//! Regression test for eu-1tkk.7.11 (Phase 2 blob blame-table plumbing).
//!
//! Prior to this change, every prelude frame in a blob-mode (shipped-binary)
//! error trace carried `Smid::default()` — no identity at all — because
//! `xtask/src/main.rs` compiles the prelude with `generate_annotations:
//! false`, and blob-mode STG-arena reconstruction (`StgArena::
//! reconstruct_form`) unconditionally zeroed any Smid it did carry (a raw
//! xtask-sourced Smid would index into a `SourceMap` the loading process
//! never populated). This meant Phase 2's blame classifier (`PreludeBlob::
//! blame`, declared via `` ` :transparent ``/`` ` :boundary `` in
//! `lib/prelude.eu`) had nothing to classify on the path that actually
//! ships: the *material* it needs was unreachable, even once declared.
//!
//! `Smid::global_slot`/`Smid::as_global_slot` plus `StgArena::
//! reconstruct_form_annotated` restore a disjoint, collision-free identity
//! (which prelude global slot, not a source position) at the two blob-mode
//! reconstruction chokepoints (`StandardRuntime::globals()` for the HeapSyn
//! engine, xtask's bytecode pre-encode loop for the bytecode engine). This
//! test asserts that identity actually reaches a live error trace under the
//! default (bytecode, blob) engine — not just that the blob's static tables
//! are populated (see `src/eval/stg/blob.rs`'s
//! `embedded_blob_has_declared_blame_for_nth_and_map` for that).
//!
//! The whole file is gated on `#[cfg(prelude_blob_ok)]`: every assertion
//! here is specific to a build that actually has `lib/prelude.blob`
//! embedded (i.e. `cargo xtask prelude-compile` ran before `cargo
//! build`/`cargo test`). CI's plain "Test Suite" job deliberately runs
//! `cargo test` without that step, to keep the source-prelude fallback path
//! exercised — under that build `eu` falls back to compiling the prelude
//! from source at runtime, every prelude Smid is a real (non-tagged) source
//! position, and these assertions would not apply. Gating the whole file
//! (rather than just the `#[test]` fns) means the helper functions below
//! aren't flagged as dead code by `cargo clippy -D warnings` in a
//! blob-less build (e.g. the "Lint" CI job, which also never generates a
//! blob). The dedicated "Bytecode + blob harness" / "GC-verified harness"
//! CI jobs do generate the blob first and so do exercise this file.
#![cfg(prelude_blob_ok)]

use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

static CASE_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Run `eu` under `EU_ERROR_TRACE_DUMP=1` on `src` and return combined
/// stdout+stderr.
fn run_trace_dump(src: &str) -> String {
    let n = CASE_COUNTER.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("eu-blame-plumbing-{}-{n}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("case.eu");
    std::fs::write(&path, src).unwrap();

    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .env("EU_ERROR_TRACE_DUMP", "1")
        .args(["--heap-limit-mib", "2048"])
        .arg(&path)
        .output()
        .expect("run eu");

    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    format!("{stdout}{stderr}")
}

/// Parse every `smid=NNNN` occurrence out of an `EU_ERROR_TRACE_DUMP=1`
/// dump (see `ExecutionError::format_smid_detail`'s `"smid={index} ..."`
/// format) and return the decoded `u32` values.
fn extract_smid_indices(dump: &str) -> Vec<u32> {
    dump.split("smid=")
        .skip(1)
        .filter_map(|rest| rest.split(|c: char| !c.is_ascii_digit()).next())
        .filter_map(|digits| digits.parse::<u32>().ok())
        .collect()
}

/// `ExecutionError::format_smid_detail` prints `Smid::get()` (a 0-based
/// index: `raw NonZeroU32 - 1`), not the raw stored value `Smid::global_slot`
/// encodes into. Reconstruct the actual `Smid` from a printed index.
fn smid_from_printed_index(index: u32) -> eucalypt::common::sourcemap::Smid {
    eucalypt::common::sourcemap::Smid::from(index + 1)
}

/// The epic's own flagship specimen (design spec §4.3's before/after
/// example): `xs nth(10)` on a 3-element list. Under the default
/// bytecode+blob engine, the resulting trace must carry at least one Smid
/// that decodes as a global-slot identity — proving blame-table material is
/// actually reachable from a live error, not just present in the blob's
/// static tables.
#[test]
fn blob_mode_trace_carries_a_global_slot_smid_for_nth_out_of_range() {
    let dump = run_trace_dump("xs: [1, 2, 3]\nresult: xs nth(10)\n");

    let global_slot_smids: Vec<u32> = extract_smid_indices(&dump)
        .into_iter()
        .filter_map(|idx| smid_from_printed_index(idx).as_global_slot())
        .collect();

    assert!(
        !global_slot_smids.is_empty(),
        "expected at least one global-slot Smid in the blob-mode trace dump, found none.\n\
         dump:\n{dump}"
    );
}

/// Stronger assertion for the same specimen: the global-slot identity must
/// resolve, via the embedded blob's own `name_to_slot`/`blame` tables, to
/// the declared `Boundary` combinator the design spec's before/after
/// example names explicitly (`nth`) — not merely to *some* prelude global.
#[test]
fn blob_mode_trace_global_slot_resolves_to_declared_boundary_combinator() {
    use eucalypt::common::diagnostic_json::FrameKind;
    use eucalypt::eval::stg::blob::PreludeBlob;

    let blob = PreludeBlob::from_bytes(eucalypt::driver::resources::PRELUDE_BLOB_BYTES)
        .expect("embedded blob should deserialise");

    let dump = run_trace_dump("xs: [1, 2, 3]\nresult: xs nth(10)\n");

    let resolved_names: Vec<(String, FrameKind)> = extract_smid_indices(&dump)
        .into_iter()
        .filter_map(|idx| smid_from_printed_index(idx).as_global_slot())
        .filter_map(|slot| blob.slot_name(slot).map(|n| n.to_string()))
        .filter_map(|name| blob.blame_for(&name).map(|kind| (name, kind)))
        .collect();

    assert!(
        resolved_names
            .iter()
            .any(|(name, kind)| name == "nth" && *kind == FrameKind::Boundary),
        "expected the trace to resolve a global-slot Smid to 'nth' (declared :boundary); \
         resolved names: {resolved_names:?}\ndump:\n{dump}"
    );
}

// ── eu-1tkk.7.21: the stamp must not destroy the user's call site ────────────

/// Run `eu run` on `src` with `extra` extra CLI args and return its stderr
/// with ANSI escapes stripped.
///
/// Unlike [`run_trace_dump`] this asks for the rendered diagnostic itself,
/// because what regressed was the *presented* primary label, not the
/// underlying trace.
fn run_diagnostic(src: &str, extra: &[&str]) -> String {
    let n = CASE_COUNTER.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("eu-call-site-{}-{n}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("case.eu");
    std::fs::write(&path, src).unwrap();

    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .arg("run")
        .args(["--heap-limit-mib", "2048"])
        .args(extra)
        .arg(&path)
        .output()
        .expect("run eu");

    strip_ansi(&String::from_utf8_lossy(&out.stderr))
}

/// Drop CSI SGR sequences (`ESC [ ... m`) so assertions can match the text.
fn strip_ansi(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\u{1b}' {
            for c in chars.by_ref() {
                if c.is_ascii_alphabetic() {
                    break;
                }
            }
        } else {
            out.push(c);
        }
    }
    out
}

/// The `line:col` on the primary-label line, i.e. the line carrying `┌─`.
///
/// Deliberately *not* the `stack trace:` note, which renders locations as
/// `- name at file:line:col` with no `┌─`: an assertion anchored on the
/// note would pass while the user still saw no label at all.
fn primary_label_position(stderr: &str) -> Option<String> {
    let line = stderr.lines().find(|l| l.contains("┌─"))?;
    let (_, after) = line.rsplit_once("case.eu:")?;
    Some(after.trim().to_string())
}

/// eu-1tkk.7.21. A prelude combinator that fails immediately — no
/// continuation stack, no annotated environment — leaves the caller's
/// call-site Smid as the only user-file location in existence.
///
/// Stamping a `Smid::global_slot` identity onto the blob-reconstructed
/// global overwrote it on entry (both machines assign `state.annotation =
/// closure.annotation()` for any *valid* annotation, and a global-slot Smid
/// is valid but resolves to no source position), so `to_diagnostic` had
/// nothing to blame and emitted no primary label.
///
/// The call is made from inside a user-defined helper, so this also pins
/// that the label tracks the real call site rather than the top-level
/// target.
#[test]
fn blob_mode_immediate_prelude_failure_blames_the_user_call_site() {
    let stderr = run_diagnostic(
        "` { target: :main requires-io: true }\n\
         main: run-it(\"ignored\")\n\
         \n\
         run-it(s): io.shell({ stdin: s })\n",
        &["--allow-io"],
    );

    assert!(
        stderr.contains("'cmd' field is required"),
        "expected the io.shell error; got:\n{stderr}"
    );
    assert_eq!(
        primary_label_position(&stderr).as_deref(),
        Some("4:12"),
        "primary label must point at the user's own call site (line 4, the \
         helper body), not the prelude and not nothing.\nstderr:\n{stderr}"
    );
}

/// eu-1tkk.7.21, second specimen: the IO-permission refusal, raised before
/// the action is evaluated at all. Mirrors
/// `tests/harness/errors/094_io_no_flag.eu`, whose `.expect` sidecar cannot
/// carry this assertion because it must also hold under the source-prelude
/// fallback, where the annotation is a real prelude position that
/// `to_diagnostic` correctly suppresses.
#[test]
fn blob_mode_io_permission_refusal_blames_the_user_call_site() {
    let stderr = run_diagnostic(
        "` { target: :result }\nresult: io.shell(\"echo hello\")\n",
        &["-t", "result"],
    );

    assert!(
        stderr.contains("IO operations are not permitted"),
        "expected the IO permission error; got:\n{stderr}"
    );
    assert_eq!(
        primary_label_position(&stderr).as_deref(),
        Some("2:9"),
        "primary label must point at the user's `io.shell` call site.\nstderr:\n{stderr}"
    );
}
