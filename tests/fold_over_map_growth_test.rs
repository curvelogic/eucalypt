#![cfg(not(target_arch = "wasm32"))]
//! Complexity-class gate for folding a lazily-mapped list (bead eu-wpswc).
//!
//! Spawns the compiled `eu` binary as a subprocess and depends on `tempfile`
//! (a non-wasm32 dev-dependency), so this test is excluded from the wasm32
//! target — matching `tick_parity_test.rs`.
//!
//! ## What regressed
//!
//! `xs map(f) sum` — the single most common shape in the language — was
//! **quadratic** in list length, while `xs sum` and `xs map(f) count` were
//! both linear. Only the combination was affected, because only a fold forces
//! the mapped *elements*.
//!
//! `map` is written with a curried self-call:
//!
//! ```text
//! map(f, l): if(l nil?, l, cons(l head f, l tail map(f)))
//! ```
//!
//! so its recursive call compiles to a partial application plus an apply, not
//! a saturated self-call, and the `eager_args` countermeasure added for
//! self-recursive call sites (bead eu-e3c3i, commit 6a902030) could not fire.
//! Each level therefore wrapped the function parameter in a fresh
//! `Atom{Ref::L}` alias closure over the caller's frame — twice, once at the
//! partial application and once in the PAP trampoline. Those alias closures
//! are built non-updateable, so the machine pushes no `Update` continuation
//! for them and nothing ever collapses the chain: resolving `f` at recursion
//! depth k walked 2k links, and forcing every element cost the sum of that.
//!
//! Measured on master a1af1f0b, deterministic VM ticks, all three dispatch
//! paths, fitting every point exactly:
//!
//! | dispatch | before | after |
//! |---|---|---|
//! | bytecode pre-decoded | `0.875·N² + 248.25·N − 6` | `230.75·N − 21` |
//! | bytecode byte | `0.875·N² + 259.25·N − 10` | `241.75·N − 25` |
//! | HeapSyn | `0.875·N² + 271.25·N − 5` | `248.75·N − 17` |
//!
//! The fix is in the three argument-array builders
//! (`machine::env_builder::create_arg_array`,
//! `bytecode::machine::make_arg_array` and `make_arg_array_pd`): an argument
//! naming a *settled* slot — a lambda/PAP, or an `Atom` node, neither of which
//! can ever change — is passed through by value instead of being re-aliased.
//!
//! ## Why ticks and not wall time
//!
//! Machine ticks are deterministic and identical on any machine, so this gate
//! cannot flake on a loaded CI runner the way a wall-clock deadline would. It
//! is also why the gate can afford to sit far from both the linear and the
//! quadratic value: there is no noise to leave headroom for.

use std::path::Path;

/// Size ratio between the two arms of each pair (`large-n / small-n` in
/// `tests/fixtures/fold_over_map_growth.eu`).
const SIZE_RATIO: f64 = 4.0;

/// Maximum tolerated tick-growth ratio across a 4x size increase.
///
/// Linear growth gives ~4.0 (measured: 4.0000 on every dispatch path, since
/// the fitted forms above have a negative constant term). The defect gave
/// 13.35 (bc pre-decoded: 1,123,244 → 14,992,994) for the lambda arm and
/// 13.22 for the thunk arm. 6.0 sits an order of magnitude of margin away
/// from the measured linear value in relative terms while still being less
/// than half the smallest measured quadratic value, so it distinguishes the
/// complexity classes without pinning a coefficient that legitimate codegen
/// work is allowed to move.
const MAX_GROWTH_RATIO: f64 = 6.0;

fn eu_binary() -> &'static Path {
    Path::new(env!("CARGO_BIN_EXE_eu"))
}

/// Run one target of the growth fixture and return `machine_ticks` from the
/// `--statistics-file` JSON output.
fn run_ticks(target: &str, stats_path: &Path) -> u64 {
    let output = std::process::Command::new(eu_binary())
        .args(["run", "-t", target, "--statistics-file"])
        .arg(stats_path)
        .args(["--heap-limit-mib", "4096"])
        .arg("tests/fixtures/fold_over_map_growth.eu")
        .output()
        .expect("failed to run eu");
    assert!(
        output.status.success(),
        "eu exited with {:?} on target {target}\nstdout: {}\nstderr: {}",
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

/// Assert that a `small`/`large` target pair grows no faster than linearly.
///
/// The spawned `eu` inherits `EU_HEAPSYN`/`EU_PREDECODE` from this process, so
/// whichever dispatch path this test itself runs under is the one measured —
/// the same arrangement `tick_parity_test.rs` relies on. All three CI engine
/// configurations therefore gate this independently.
fn assert_linear_growth(kind: &str, small_target: &str, large_target: &str) {
    let dir = tempfile::tempdir().expect("create temp dir");
    let small_ticks = run_ticks(small_target, &dir.path().join("small.json"));
    let large_ticks = run_ticks(large_target, &dir.path().join("large.json"));

    assert!(small_ticks > 0, "{kind}: small arm recorded no ticks");
    let ratio = large_ticks as f64 / small_ticks as f64;

    assert!(
        ratio <= MAX_GROWTH_RATIO,
        "eu-wpswc: folding a lazily-mapped list is growing faster than linearly ({kind}). \
         Ticks went {small_ticks} -> {large_ticks} for a {SIZE_RATIO}x size increase, \
         a ratio of {ratio:.2} against a cap of {MAX_GROWTH_RATIO}. Linear is ~4.0; the \
         original defect measured ~13.3. This means an argument naming a settled \
         environment slot is being re-aliased per recursion level again — see the \
         settled-slot pass-through in `machine::env_builder` and its two bytecode twins \
         `make_arg_array` / `make_arg_array_pd`, which must not drift apart."
    );
}

/// The headline shape: the mapped function is a literal lambda, so it reaches
/// the callee as an `arity > 0` closure.
#[test]
fn fold_over_map_is_linear_for_a_lambda_argument() {
    assert_linear_growth("lambda argument", "growth-small", "growth-large");
}

/// The mapped function is a *thunk* that evaluates to a function. It reaches
/// the callee as an alias closure rather than a lambda, so this arm is only
/// linear if an alias is passed on instead of being re-aliased. Gates the
/// second half of the predicate independently of the first.
#[test]
fn fold_over_map_is_linear_for_a_thunk_valued_function_argument() {
    assert_linear_growth(
        "thunk-valued function argument",
        "growth-thunk-small",
        "growth-thunk-large",
    );
}
