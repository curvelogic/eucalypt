#![cfg(not(target_arch = "wasm32"))]
//! Source-prelude tick-parity tripwire (eu-2sa6.5 / eu-npp9).
//!
//! Spawns the compiled `eu` binary as a subprocess and depends on
//! `tempfile` (a non-wasm32 dev-dependency, `Cargo.toml`), so this test is
//! excluded from the wasm32 target — matching `property_test.rs` /
//! `fuzz_regression_test.rs`.
//!
//! ## Background (historical handicap, now closed)
//!
//! Between 2026-07-13 and 2026-07-15, `fib(30)` executed in 88,853,885 VM
//! ticks when `lib/prelude.blob` was embedded in the binary, but 98,277,770
//! ticks (+10.6%) when the blob was absent and the driver fell back to
//! compiling the prelude from source (`--source-prelude` /
//! `EU_SOURCE_PRELUDE=1`, or simply no blob present at build time).
//! Root-caused 2026-07-13 (bead eu-2sa6.5) as a demand-analysis strictness
//! divergence rather than a fusion gap — `emit_fixtures_and_globals` in
//! `src/eval/bytecode/encode.rs` fuses the same intrinsic global forms on
//! both paths, byte-identically:
//!
//! - **Blob path**: the prelude source is never merged into the user's
//!   core expression (`prepare.rs` filters it out of `inputs` once
//!   `has_prelude_blob()` is true). References like `<=`/`+`/`-` in user
//!   code stay `Var::Free` until `Loader::inject_prelude_inlinable_bindings`
//!   (`src/driver/source.rs`) injects the blob's pre-resolved
//!   `inlinable_bindings` set as a flat `Let`, matched purely by
//!   free-variable name, immediately before the general inline pass. That
//!   exposed the strict `Case`-on-`L(0)` shape of the arithmetic/comparison
//!   intrinsics to demand analysis, which inferred `fib`'s recursive
//!   argument as `Strict` — no per-call thunk allocation.
//! - **Source-prelude path**: the prelude *is* one of the merge inputs, so
//!   `merge_units` folds it directly into the user's core expression. By
//!   the time `cook()` ran, every prelude reference had already been
//!   resolved to `Var::Bound` — there was no `Var::Free` name left for a
//!   `inject_prelude_inlinable_bindings`-style injection to catch, and
//!   every prelude binding (including the arithmetic operators) carries a
//!   `Meta(Lam, {doc, type})` wrapper that `analyse_demand`'s named-signature
//!   recording did not see through (it checked `Expr::Lam` directly against
//!   a Meta-wrapped RHS). `fib`'s argument demand was inferred `Lazy`, so
//!   each recursive call allocated and later forced a thunk for `n - 1` /
//!   `n - 2`.
//!
//! **Closed 2026-07-15** by PR #1016 (bead eu-rb5n), which fixed
//! `analyse_demand`'s Meta-blindness as one of three independent fixes
//! needed to close the *separate* HOF-combinator source-path fusion gap.
//! That fix corrects demand-signature recording for any Meta-wrapped
//! binding, not just the HOF combinators eu-rb5n was targeting — since
//! `+`/`<=` (and every other prelude binding fib's naive recursion depends
//! on) carry doc/type Meta annotations too, the fix incidentally restored
//! correct strictness-demand visibility for them on the source path as a
//! side effect. Confirmed by direct measurement on master post-#1016
//! (bead eu-npp9, closed 2026-07-15): `fib(30)`, clean release build, blob
//! vs `EU_SOURCE_PRELUDE=1`, both engines —
//! bytecode 88,853,753 vs 88,853,755 ticks (ratio 1.0000000225), HeapSyn
//! 115,779,125 vs 115,779,127 ticks (ratio 1.0000000173). The residual
//! +2-tick gap is Smid/annotation bookkeeping, not an algorithmic
//! divergence — this is full parity, not merely "improved." See
//! `docs/development/prelude-blob.md` for the user-facing summary.
//!
//! ## What this test asserts
//!
//! Not an exact tick count (too brittle across codegen changes) — instead
//! that the source-prelude handicap on a fused, strict-recursion program
//! stays within a small bound. Since the handicap is now closed (ratio
//! ~1.0000000225, i.e. real parity), the caps below are pure regression
//! tripwires, not a documented-handicap allowance: they exist to catch a
//! *reintroduction* of the demand-analysis divergence (e.g. a future
//! `analyse_demand` or inline-pass change that reintroduces Meta-blindness
//! for some binding class), not to tolerate a known gap. Halved 2026-07-16
//! (owner ruling, eu-vcr8) from the pre-closure caps of 1.12 / 1.13 — with
//! the measured ratio at ~1.00000002, even the halved caps (1.06 / 1.065)
//! leave roughly six orders of magnitude of headroom over the actual noise
//! floor, so tightening costs nothing in false-positive risk while cutting
//! the regression window in half.
//!
//! If the binary under test was built *without* a prelude blob (e.g. a
//! bare `cargo test` run without first running `cargo xtask
//! prelude-compile`), both configurations exercise the identical
//! source-fallback code path and the comparison is vacuously true — this
//! test only has teeth when a blob is embedded in the `eu` binary it
//! spawns. CI exercises the real comparison via the dedicated
//! `tick-parity` job in `.github/workflows/build-rust.yaml`, which
//! generates the blob before compiling the test binary.

use std::path::Path;

/// Regression tripwire cap for the byte-dispatch engine. The handicap this
/// guards is closed (measured ratio ~1.0000000225 on `fib(30)`, bead
/// eu-npp9) — 1.06 is not a documented allowance, it is headroom over
/// measurement noise for a ratio that should sit at ~1.00. Halved
/// 2026-07-16 from 1.12 (owner ruling, eu-vcr8) now that parity is
/// confirmed; still enormous relative to the actual noise floor, so a
/// regression that reopens the old demand-analysis gap (or any similar
/// divergence) will trip this long before the true ratio approaches 1.06.
const MAX_SOURCE_PRELUDE_RATIO: f64 = 1.06;

/// Regression tripwire cap under the pre-decoded engine (`EU_PREDECODE=1`,
/// bead eu-2sa6.13). Kept marginally above [`MAX_SOURCE_PRELUDE_RATIO`] for
/// consistency with the byte-dispatch cap's historical relationship (BV2's
/// `Op::Ann` elimination shrinks `machine_ticks` uniformly on both configs,
/// which mechanically inflates a ratio computed from smaller numbers by the
/// same absolute gap) — but with the underlying handicap now closed, that
/// baseline-shrink effect acts on an already-negligible ~2-tick gap and is
/// itself noise-scale. Halved 2026-07-16 from 1.13 (owner ruling, eu-vcr8).
const MAX_SOURCE_PRELUDE_RATIO_PREDECODE: f64 = 1.065;

fn eu_binary() -> &'static Path {
    Path::new(env!("CARGO_BIN_EXE_eu"))
}

/// Run the tick-parity fixture and return `machine_ticks` from the
/// `--statistics-file` JSON output.
fn run_ticks(source_prelude: bool, stats_path: &Path) -> u64 {
    let mut cmd = std::process::Command::new(eu_binary());
    cmd.args(["run", "-t", "tick-parity-fib", "--statistics-file"])
        .arg(stats_path)
        .args(["--heap-limit-mib", "2048"])
        .arg("tests/fixtures/tick_parity_fib.eu");
    if source_prelude {
        cmd.arg("--source-prelude");
    }

    let output = cmd.output().expect("failed to run eu");
    assert!(
        output.status.success(),
        "eu exited with {:?}\nstdout: {}\nstderr: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let json_text = std::fs::read_to_string(stats_path)
        .unwrap_or_else(|e| panic!("reading statistics file {}: {e}", stats_path.display()));
    let json: serde_json::Value =
        serde_json::from_str(&json_text).expect("statistics file is not valid JSON");
    json["machine_ticks"]
        .as_u64()
        .expect("statistics JSON missing machine_ticks")
}

#[test]
fn source_prelude_tick_parity_tripwire() {
    let dir = tempfile::tempdir().expect("create temp dir");
    let blob_stats = dir.path().join("blob-stats.json");
    let source_stats = dir.path().join("source-stats.json");

    let blob_ticks = run_ticks(false, &blob_stats);
    let source_ticks = run_ticks(true, &source_stats);

    let ratio = source_ticks as f64 / blob_ticks as f64;

    // The pre-decoded engine (`EU_PREDECODE=1`) shrinks `machine_ticks`
    // uniformly via Ann-elimination, inflating this ratio without changing the
    // absolute handicap (see `MAX_SOURCE_PRELUDE_RATIO_PREDECODE`). The spawned
    // `eu` inherits `EU_PREDECODE` from this process, so both `run_ticks` calls
    // above measured whichever engine this test itself runs under — select the
    // matching cap.
    let predecode = std::env::var("EU_PREDECODE").as_deref() == Ok("1");
    let cap = if predecode {
        MAX_SOURCE_PRELUDE_RATIO_PREDECODE
    } else {
        MAX_SOURCE_PRELUDE_RATIO
    };

    assert!(
        source_ticks as f64 <= blob_ticks as f64 * cap,
        "source-prelude tick handicap has grown beyond the tripwire bound \
         (eu-2sa6.5 / eu-npp9 — the handicap itself is closed, this is a pure \
         regression guard): default={blob_ticks} ticks, --source-prelude={source_ticks} ticks, \
         ratio={ratio:.4} > {cap} (predecode={predecode}). If this binary embeds no prelude \
         blob, default and --source-prelude use the identical code path and this assertion \
         should be trivially satisfied — a failure here with no blob present points at a \
         different regression, not the (closed) historical handicap."
    );
}
