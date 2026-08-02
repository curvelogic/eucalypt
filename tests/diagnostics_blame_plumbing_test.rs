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

/// Run `eu` on `src` with `extra` command-line flags and return combined
/// stdout+stderr.
fn run_eu(src: &str, extra: &[&str]) -> String {
    let n = CASE_COUNTER.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("eu-blame-plumbing-{}-{n}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("case.eu");
    std::fs::write(&path, src).unwrap();

    let out = Command::new(env!("CARGO_BIN_EXE_eu"))
        .args(["--heap-limit-mib", "2048"])
        .args(extra)
        .arg(&path)
        .output()
        .expect("run eu");

    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    format!("{stdout}{stderr}")
}

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

// ── eu-7x0r: blob-mode prelude frames must render, and Smids baked by ────────
// ── xtask must not alias user source positions ───────────────────────────────

/// End-to-end companion to the two tests above: the global-slot identity they
/// prove *reaches* the trace must also be *rendered*.
///
/// It was not. `Smid::global_slot` deliberately has no `SourceMap` entry, and
/// both trace renderers resolved names by indexing the `SourceMap`
/// (`SourceMap::resolve_trace_entry` for the human trace,
/// `Executor::json_trace` for the JSON one), so every prelude frame in a
/// blob-mode trace was silently dropped *after* classification — the shipped
/// binary named no library combinator at all. `SourceMap::global_slot_info`
/// (fed by `PreludeBlob::binding_spans` plus the lazily-registered prelude
/// source) closes that, so the blob path renders the same `in 'nth'
/// (prelude)` context line the source-compiled path does — no coordinate,
/// since a bundled-library line:col is not something a user can act on
/// (eu-1tkk.7.36), but `PreludeBlob::binding_spans` still backs the real
/// coordinate carried in the structured JSON trace.
#[test]
fn blob_mode_curated_trace_names_the_boundary_combinator_with_a_prelude_location() {
    let out = run_eu("xs: [1, 2, 3]\nresult: xs nth(10)\n", &[]);

    assert!(
        out.contains("- in 'nth' (prelude)"),
        "blob-mode curated trace must name the boundary combinator with a prelude \
         hint, e.g. \"- in 'nth' (prelude)\"\noutput:\n{out}"
    );
}

/// Every Smid reachable from a reconstructed prelude global must be either
/// invalid (`Smid::default()`) or a `Smid::global_slot` identity — never a raw
/// index into a `SourceMap`.
///
/// `xtask` compiles the prelude against its own `SourceMap`, so any Smid baked
/// into the blob indexes a map the loading process never populates. `Ann` nodes
/// were elided and `Lambda` annotations replaced, but `DirectApp` and
/// `LookupLit` carry a Smid in a *data* field that reconstruction copied
/// verbatim. Those indices are not merely useless: in any program whose own
/// `SourceMap` grows past them they resolve against unrelated *user*
/// declarations, so prelude-internal frames render as — and can become the
/// primary label of — lines the user never called (see
/// `blob_mode_trace_cannot_blame_an_unrelated_user_declaration`).
///
/// Asserting the structural invariant rather than the two known fields means a
/// future arena variant carrying a Smid cannot reintroduce the class silently.
#[test]
fn reconstructed_prelude_globals_carry_no_baked_source_smids() {
    use eucalypt::common::sourcemap::Smid;
    use eucalypt::eval::stg::arena::StgArena;
    use eucalypt::eval::stg::blob::PreludeBlob;
    use eucalypt::eval::stg::syntax::{LambdaForm, StgSyn};

    /// `(slot, field, raw smid)` for every offending Smid found.
    type Offenders = Vec<(u32, &'static str, u32)>;

    fn note(smid: Smid, what: &'static str, slot: u32, offenders: &mut Offenders) {
        if smid.is_valid() && smid.as_global_slot().is_none() {
            offenders.push((slot, what, u32::from(smid)));
        }
    }

    fn check_syn(syn: &StgSyn, slot: u32, offenders: &mut Offenders) {
        match syn {
            StgSyn::Ann { smid, body } => {
                note(*smid, "Ann", slot, offenders);
                check_syn(body, slot, offenders);
            }
            StgSyn::DirectApp { smid, .. } => note(*smid, "DirectApp", slot, offenders),
            StgSyn::LookupLit { smid, .. } => note(*smid, "LookupLit", slot, offenders),
            StgSyn::Case {
                scrutinee,
                branches,
                fallback,
            } => {
                check_syn(scrutinee, slot, offenders);
                for (_, branch) in branches {
                    check_syn(branch, slot, offenders);
                }
                if let Some(fallback) = fallback {
                    check_syn(fallback, slot, offenders);
                }
            }
            StgSyn::Let { bindings, body } | StgSyn::LetRec { bindings, body } => {
                for binding in bindings {
                    check_form(binding, slot, offenders);
                }
                check_syn(body, slot, offenders);
            }
            StgSyn::DeMeta {
                scrutinee,
                handler,
                or_else,
            } => {
                check_syn(scrutinee, slot, offenders);
                check_syn(handler, slot, offenders);
                check_syn(or_else, slot, offenders);
            }
            StgSyn::Seq { scrutinee, body } => {
                check_syn(scrutinee, slot, offenders);
                check_syn(body, slot, offenders);
            }
            _ => {}
        }
    }

    fn check_form(form: &LambdaForm, slot: u32, offenders: &mut Offenders) {
        match form {
            LambdaForm::Lambda {
                body, annotation, ..
            } => {
                note(*annotation, "Lambda.annotation", slot, offenders);
                check_syn(body, slot, offenders);
            }
            LambdaForm::Thunk { body } | LambdaForm::Value { body } => {
                check_syn(body, slot, offenders)
            }
        }
    }

    let blob = PreludeBlob::from_bytes(eucalypt::driver::resources::PRELUDE_BLOB_BYTES)
        .expect("embedded blob should deserialise");
    let arena = StgArena {
        nodes: blob.nodes.clone(),
        forms: blob.forms_pool.clone(),
    };

    let mut offenders: Offenders = vec![];
    for (slot, &entry) in blob.binding_entries.iter().enumerate() {
        let form = arena
            .reconstruct_form_annotated(entry, Smid::global_slot(slot as u32))
            .expect("reconstruct prelude form");
        check_form(&form, slot as u32, &mut offenders);
    }

    assert!(
        offenders.is_empty(),
        "reconstructed prelude globals must carry no raw SourceMap Smids; found \
         {} offender(s), first few (slot, field, raw smid): {:?}",
        offenders.len(),
        &offenders[..offenders.len().min(20)]
    );
}

/// The user-visible consequence of the invariant above, asserted end to end.
///
/// A baked Smid only misbehaves visibly once the program's own `SourceMap`
/// grows past it, so this fixture pads out enough declarations to overtake the
/// range `xtask` bakes (the offenders observed on the pre-fix blob were indices
/// 1859 and 1921). Before the fix, `xs nth(10)` here produced a trace of
/// *three* user-file frames — arbitrary `padNNN` declarations, aliased by
/// prelude-internal frames — and named no prelude combinator at all. After it,
/// there is exactly one user-file frame (the anchor) plus the named boundary
/// combinator.
///
/// The count is the assertion, deliberately, rather than "no `padNNN` appears":
/// *which* declaration the single user anchor names is a separate, pre-existing
/// and prelude-independent question (a padded unit anchors on `pad0` rather
/// than `result` under the source-compiled prelude too — tracked separately),
/// so pinning the name here would couple this gate to an unrelated bug.
#[test]
fn blob_mode_trace_frames_cannot_alias_unrelated_user_declarations() {
    let mut src = String::from("xs: [1, 2, 3]\n");
    for i in 0..900 {
        src.push_str(&format!("pad{i}: {i} + {i}\n"));
    }
    src.push_str("result: xs nth(10)\n");

    let out = run_eu(&src, &[]);

    assert!(
        out.contains("- in 'nth' (prelude)"),
        "the trace must name the boundary combinator the error was raised inside\n\
         output:\n{out}"
    );
    let user_frames = out.matches("at case.eu:").count();
    assert_eq!(
        user_frames, 1,
        "exactly one user-file trace frame is expected (the anchor); more means \
         prelude-internal frames are aliasing unrelated user declarations\noutput:\n{out}"
    );
}
