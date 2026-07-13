#![cfg(not(target_arch = "wasm32"))]
//! Source-prelude tick-parity tripwire (eu-2sa6.5).
//!
//! Spawns the compiled `eu` binary as a subprocess and depends on
//! `tempfile` (a non-wasm32 dev-dependency, `Cargo.toml`), so this test is
//! excluded from the wasm32 target — matching `property_test.rs` /
//! `fuzz_regression_test.rs`.
//!
//! ## Background
//!
//! `fib(30)` executes in 88,853,885 VM ticks when `lib/prelude.blob` is
//! embedded in the binary, but 98,277,770 ticks (+10.6%) when the blob is
//! absent and the driver falls back to compiling the prelude from source
//! (`--source-prelude` / `EU_SOURCE_PRELUDE=1`, or simply no blob present
//! at build time). Root-caused 2026-07-13 (bead eu-2sa6.5): this is *not*
//! a fusion gap — `emit_fixtures_and_globals` in
//! `src/eval/bytecode/encode.rs` fuses the same intrinsic global forms on
//! both paths, byte-identically. The real cause is demand-analysis
//! strictness divergence:
//!
//! - **Blob path**: the prelude source is never merged into the user's
//!   core expression (`prepare.rs` filters it out of `inputs` once
//!   `has_prelude_blob()` is true). References like `<=`/`+`/`-` in user
//!   code stay `Var::Free` until `Loader::inject_prelude_inline_cores`
//!   (`src/driver/source.rs`) injects the blob's pre-resolved
//!   `inline_cores` set as a flat `Let`, matched purely by free-variable
//!   name, immediately before the general inline pass. That exposes the
//!   strict `Case`-on-`L(0)` shape of the arithmetic/comparison
//!   intrinsics to demand analysis, which infers `fib`'s recursive
//!   argument as `Strict` — no per-call thunk allocation.
//! - **Source-prelude path**: the prelude *is* one of the merge inputs, so
//!   `merge_units` folds it directly into the user's core expression.  By
//!   the time `cook()` runs, every prelude reference has already been
//!   resolved to `Var::Bound` (verified directly: `eu dump cooked
//!   --debug-format` on the source path contains zero `Free(...)` nodes
//!   and thousands of `Bound(...)` ones) — there is no `Var::Free` name
//!   left for a `inject_prelude_inline_cores`-style injection to catch.
//!   `fib`'s argument demand is inferred `Lazy`, so each recursive call
//!   allocates and later forces a thunk for `n - 1` / `n - 2`.
//!
//! A sweep of the general inline pass's iteration count (2 through 12)
//! confirmed this isn't an under-iterated fixed point either: ticks were
//! flat at 98,277,770 regardless, because the general inliner simply
//! doesn't perform the same fold as the blob path's dedicated
//! peel/fixed-point/pre-substitute pipeline.
//!
//! Unifying the two paths (making the source-prelude fallback build the
//! same in-memory global-slot/`Ref::G` structure the blob embeds, instead
//! of merging the prelude source directly) is a substantial redesign,
//! deliberately deferred to a follow-on bead so it doesn't run ahead of
//! the lever-(a) predecoded-IR work. See `docs/development/prelude-blob.md`
//! for the user-facing summary of the handicap.
//!
//! ## What this test asserts
//!
//! Not an exact tick count (too brittle across codegen changes) — instead
//! that the source-prelude handicap on a fused, strict-recursion program
//! stays within its documented bound (~10.6%, capped here at 12% to leave
//! headroom for measurement noise while still catching regressions).
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

/// The documented handicap is ~10.6% (98,277,770 / 88,853,885 on
/// `fib(30)`); cap at 12% so the tripwire has a little headroom for
/// measurement noise but still fails if the gap widens materially.
const MAX_SOURCE_PRELUDE_RATIO: f64 = 1.12;

/// Under the pre-decoded engine (`EU_PREDECODE=1`, bead eu-2sa6.13) the same
/// handicap reads as a slightly larger ratio, capped here at 13%.
///
/// This is **not** a handicap regression — it is a pure baseline-shrink
/// artifact of BV2 Ann-elimination. Eliminating `Op::Ann` from dispatch removes
/// the *same absolute number* of `machine_ticks` (dispatch steps) from both
/// configs — on `fib`, blob 722,435→623,927 and source 799,059→700,551, both
/// −98,508 — so the **absolute** handicap the tripwire guards is unchanged at
/// 76,624 ticks. Subtracting the same constant from a larger numerator and a
/// smaller denominator raises their quotient: the byte-engine ratio 1.106
/// becomes 1.1228 under pre-decode. The 1.13 cap keeps this a real gate — a
/// genuine handicap regression (a *wider* absolute gap) still pushes the ratio
/// well past 1.13 — while tolerating the accounting shift. The byte-engine cap
/// (1.12) is deliberately left untouched. Owner-signed (2026-07-13).
const MAX_SOURCE_PRELUDE_RATIO_PREDECODE: f64 = 1.13;

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
        "source-prelude tick handicap has grown beyond the documented bound \
         (eu-2sa6.5): default={blob_ticks} ticks, --source-prelude={source_ticks} ticks, \
         ratio={ratio:.4} > {cap} (predecode={predecode}). If this binary embeds no prelude \
         blob, default and --source-prelude use the identical code path and this assertion \
         should be trivially satisfied — a failure here with no blob present points at a \
         different regression, not the known handicap."
    );
}
