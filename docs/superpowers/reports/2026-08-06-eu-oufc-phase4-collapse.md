# eu-oufc: BV1 Phase 4 collapse — delete HeapSyn, retire the dual-engine ABI

- **Date:** 2026-08-06
- **Bead:** eu-oufc (parent epic eu-1tkk)
- **Authorisation:** owner, 2026-08-04, on the bead. Evidence: eu-7oshh A/B study
  (PR #1116, merged 350e4869, `docs/superpowers/reports/2026-08-03-engine-ab-study.md`)
  — day03/day08/day09/fib and the canonical suite (015-022) all at parity or
  bytecode-faster in blob mode, independently verified by the coordinator on a
  clean build.

## Summary

Deleted the legacy HeapSyn tree-walk STG machine and the `EU_HEAPSYN`/
`EU_BYTECODE` engine-select flags. The bytecode engine (`src/eval/bytecode/`)
is now the sole execution engine. Net effect: 43 files changed, ~1,200
insertions, ~9,800 deletions (~8,600 fewer lines).

## Scope actually deleted

- `src/eval/machine/vm.rs` — `Machine`/`MachineState`/`MachineBifContext`/
  `HeapNavigator`/`ArgPattern`/`classify_args`/`evaluate_to_whnf_impl` (3,460
  lines → 30). Kept `interrupted()`/`set_interrupted()` (the SIGINT flag the
  bytecode engine's run loop also polls) in place, since `bin/eu.rs` calls
  `vm::set_interrupted()` from the signal handler.
- `src/eval/memory/loader.rs` — deleted whole (StgSyn→HeapSyn loading, used
  only by the deleted `Initialiser`).
- `src/eval/machine/env_builder.rs`, `src/eval/machine/cont.rs` — deleted
  whole. Confirmed HeapSyn-only: the bytecode engine has its own independent
  `BcEnvBuilder` (`bytecode/env_builder.rs`) and `BcContinuation`
  (`bytecode/cont.rs`), not type aliases over these.
- `src/eval/machine/env.rs` — deleted `Closing<S>` (the whole struct + impls;
  its only instantiation, `SynClosure = Closing<RefPtr<HeapSyn>>`, is gone)
  and `SynClosure`/`EnvFrame`/`ScopeAndClosure`. Kept `EnvironmentFrame<C>`
  (generic, serves `BcEnvFrame = EnvironmentFrame<BcValue>` unchanged).
- `src/eval/memory/syntax.rs` — deleted the `HeapSyn` enum, its
  `GcScannable`/`StgObject` impls, the `Repr` trait + `repr` module (zero
  external callers), and `LambdaForm = InfoTagged<RefPtr<HeapSyn>>` + its
  `GcScannable` impl. Trimmed `StgBuilder` from ~20 HeapSyn-code-building
  methods (`atom`/`app`/`data`/`let_`/`case`/... ) down to the three that
  build native values, not code: `str`/`str_ref`/`sym_ref`. `Native`,
  `Reference<T>`/`Ref`, `RefPtr`, `BlockIndex` are unchanged (shared value
  representation, used directly by the bytecode engine).
- `src/eval/memory/mutator.rs` — trimmed `impl StgBuilder for
  MutatorHeapView` to match; deleted `build_branch_table` (only used by the
  deleted `case`/`switch`).
- `src/eval/machine/intrinsic.rs` — removed the `AbiClosure::Heap(SynClosure)`
  variant and `.expect_heap()`/`.as_heap()`; `AbiClosure` is now a
  single-variant wrapper over `BcValue`. Removed `nav`/`root_env`/`env`/
  `set_closure`/`evaluate_to_whnf`/`block_index_enabled` from
  `IntrinsicMachine` (all HeapSyn-only escape hatches, zero remaining
  callers once `block.rs`'s dead code below was fixed). Converted ~20
  "code-type-neutral" methods (`resolve_native`, `return_native`,
  `data_value`, `apply1_thunk`, ...) from HeapSyn-default bodies to abstract
  trait methods — the bytecode engine's `BcBifContext` already overrode
  every one of them, confirmed by inventory before deleting the defaults.
- `src/eval/bytecode/machine.rs` — deleted `BcBifContext`'s panic-stub block
  (`set_closure`/`nav`/`root_env`/`env`, which existed only to satisfy the
  old trait signature) and its `block_index_enabled() -> false` override
  (moot now the trait doesn't declare it). Fixed ~10 now-irrefutable
  `AbiClosure::Byte(..)` match patterns (`let ... else { panic!(...) }` →
  plain destructure) left over from the removed `Heap` variant. Added
  test-only accessors (`terminated`, `native_return`, `string_return`,
  `bool_return`, `unit_return`, `captures`) mirroring the deleted
  `vm.rs`/`Machine`'s equivalents, needed by the ported unit tests below.
- `src/eval/stg/block.rs` — `LookupOr`/`SafeLookup`'s mutable block-index
  optimisation (`walk_list_to_position`, `count_list`, `build_index`,
  `store_index_in_block`, `BlockListIterator`, ~350 lines) was HeapSyn-only
  (bytecode blocks are template closures with no in-place mutation, so
  `block_index_enabled()` was always `false` there); both `execute()` bodies
  now unconditionally signal "use the find loop".
- `src/eval/stg/support.rs`, `src/eval/stg/render_to_string.rs` — deleted
  two dead helpers (`machine_return_closure_list`, `extract_scalar_string`/
  `scalar_from_native`) that took `SynClosure` directly and had zero
  callers once `driver::io_run.rs` (below) was gone.
- `src/driver/io_run.rs` — deleted whole (the HeapSyn IO-run driver;
  `driver::bytecode_io_run` is its bytecode mirror and already the only one
  wired into `driver/eval.rs`).
- `src/wasm_pipeline.rs` — this evaluation entry point (WASM API + native
  `#[cfg(test)]` unit tests) built directly on `standard_machine`, with no
  bytecode path at all. Ported to `BytecodeMachine`/`encode()`, mirroring
  `driver/eval.rs`'s non-headless, non-IO rendering path (this pipeline
  never supported IO monads).
- `src/eval/stg/testing.rs` — the `#[cfg(test)]` helper `testing::machine()`
  used by ~46 unit tests across `arith.rs`/`block.rs`/`eq.rs`/`force.rs`/
  `render.rs`/`string.rs`. Rebuilt on `BytecodeMachine`/`encode()`
  (`CapturingEmitter`, since some callers read `.captures()`); the 46 call
  sites needed no changes beyond the new accessors on `BytecodeMachine`.
- `benches/alloc.rs`, `benches/gc.rs` — ported from `EnvBuilder`/
  `SynClosure`/`HeapSyn` to `BcEnvBuilder`/`BcValue`/a new minimal
  `GcScannable` fixture (`gc.rs`) respectively. `alloc.rs`'s
  `partially_apply` needed making `pub` (it was a private fn in
  `bytecode/machine.rs`) to keep the PAP-construction benchmark; noted in
  both files why bytecode natives need no allocation (`box_one` → `native_one`,
  no heap object) and why `alloc_let`/`alloc_letrec` now measure the same
  primitive (bytecode's frame allocation doesn't distinguish let/letrec the
  way `HeapSyn::Let`/`HeapSyn::LetRec` did).
- Engine-select flags: `bytecode::bytecode_enabled()`/`heapsyn_enabled()`
  deleted; `driver/eval.rs`'s dispatch collapsed to the single bytecode
  branch (`collect_machine_stats` deleted, `collect_bytecode_stats` is now
  the only stats path).

## `GcScannable` disposition

Not retired — it's shared infrastructure the bytecode engine's own closures,
environment frames and continuations still scan through (`impl GcScannable
for BcMachineState`, `BcClosure`, `EnvironmentFrame<C>`, `BcContinuation`,
`Array<T>`, `HeapString`, etc., all untouched). Only the `HeapSyn`- and
`SynClosure`-specific impls were deleted, per the bead's own instruction to
verify before assuming.

## Collector test fixture (separate PR)

`src/eval/memory/collect.rs`'s 12 unit tests used HeapSyn (`LambdaForm`,
`view.atom`/`app`/`let_`/`app_bif`) as convenient fixture data for exercising
mark/scan/evacuate/forward generically. Replaced with a minimal,
production-code-independent `GcScannable` fixture (`TestObj`:
`Leaf`/`Ptr`/`Many`/`Compound`) mirroring the pointer shapes the collector
must handle (none, one, an array, an array+separate-pointer). Landed as its
own PR (#1120, `fix/furnace-eu-oufc-gc-test-fixture`) to shrink this PR's
diff; that PR is a prerequisite, not a dependency — this PR's `collect.rs`
already reflects the same content.

Wicket's review of #1120 found a real gap in the new fixture (dropping the
body mark in `Compound::scan` left all 12 tests green) — fixed by
strengthening `test_evacuate_with_internal_refs` to actually read back
through the body pointer post-collection, and adding a new deterministic
test, `test_compound_scan_reports_body_for_marking_and_forwarding`, that
calls `scan`/`scan_and_update` directly rather than through `collect()`'s
block-eviction heuristics (which, empirically, don't reliably target an
isolated body's block). Both fixes fault-injection verified.

## The ~6 differential-comparison test files

Per the coordinator's rule — delete if the test's whole purpose was bc-vs-hs
equivalence; re-axis onto `EU_PREDECODE=0` (a still-real second execution
path, eu-1hcw) if an underlying property still needs guarding; never leave a
test comparing the default engine to itself:

| File | Disposition |
|---|---|
| `tests/tick_parity_test.rs` | Not actually a bc-vs-hs comparison (single-process, reads whichever engine it runs under). Removed the dead `EU_HEAPSYN` branch from its cap-selection logic; kept the real `EU_PREDECODE` branch. |
| `tests/diagnostics_trace_anchor_test.rs` | Had 3 tests: one bytecode-only (kept), one a HeapSyn-repeat of it (deleted, now a duplicate), one an explicit cross-engine parity assertion (deleted, whole purpose was equivalence). |
| `tests/bytecode_io_differential_test.rs` | Whole file's purpose was bc-vs-hs equivalence. Rewrote all 7 tests as direct assertions on the bytecode driver's output (stronger than "whatever the two engines agree on" — it pins the actual expected content) instead of deleting the file, preserving real regression coverage (notably eu-xqab's tag-vs-field IO dispatch bug). Incidentally found and fixed a latent bug: `io_bind_chain_agrees`'s fixture used a `where` clause, which is not valid eucalypt syntax — both engines silently agreed on the same parse error, so the test was vacuous even before HeapSyn's deletion. Rewrote using the block-scoped function pattern from `tests/harness/105_io_chain.eu`. |
| `tests/fold_over_map_growth_test.rs` | Same shape as tick_parity_test.rs — not a live comparison, just inherits whichever engine/dispatch it runs under. Updated stale comments (the fix it guards references a HeapSyn-side twin, `machine::env_builder::create_arg_array`, that no longer exists). |
| `tests/diagnostics/snapshot_engine.rs` | `engine_scope_note()` is a static string appended to a generated report (`DIVERGENCE.md`), not a live comparison. Updated the note (two non-default paths → one, `EU_PREDECODE=0`) and hand-regenerated `DIVERGENCE.md`'s matching section, verified against a real blob build (`divergence_inventory_is_current` passes). |
| `tests/harness_test.rs` | Two live comparisons: `test_pp_fork_path_equivalence_both_engines` (renamed `..._both_dispatch_configs`) and a loop inside the PP GC-stress test (`test_pp_gc_collects_during_par_map`, the loop body). Both re-axised from `EU_HEAPSYN=1` onto `EU_PREDECODE=0` — fork-path equivalence and GC-safety-during-collection are still real properties worth checking across the two bytecode dispatch configs. |

Also fixed a misleading user-facing error message
(`CompileError::BytecodeCodeTooLarge`) that suggested `EU_HEAPSYN=1` as a
workaround for a program too large for the bytecode engine's 32-bit
code-offset space — there is no workaround now; removed the suggestion.

## `cargo xtask engine-ab` disposition

Its entire premise was a live, interleaved bc/hs run. Rather than leave it
silently measuring bytecode twice under an "hs" label (which would corrupt
`results.jsonl` with mislabelled rows), `run()` now refuses with a message
pointing at the follow-up bead. `--check` (reads the ledger, flags
regressions per lineage) is untouched and still works; deleted ~250 lines of
now-dead live-run-only code (`Bench`/`SUITE`/`time_run`/`stats_run`/
`median`/`spread`/`row_json`/`append_rows`/`git_short_commit`/`host_string`/
`today`). `results.jsonl` and its history are retained, not deleted.

Follow-up bead filed: **eu-hn3j0** — redesign the live-run half onto the
predecoded-vs-byte-dispatch axis, once that axis's own fate (eu-1hcw) is
decided.

## Documentation

- `CLAUDE.md`: removed `EU_HEAPSYN`/`EU_BYTECODE` from the debug env var
  table; rewrote "Engine Performance Claims" to note the bc/hs protocol is
  now historical.
- `docs/architecture.md`: rewrote "Two execution engines share one STG" →
  "One execution engine: bytecode"; rewrote "Virtual Machine"/
  "Continuations" sections to describe `BcMachineState`/`BcContinuation`
  instead of the deleted `MachineState`/`Continuation`; fixed the code
  organisation and prelude-blob tables.
- `docs/development/gc-verification-spec.md`: updated the pointer-validity
  example from `RefPtr<HeapSyn>` node fields to the current pointer-holding
  types.
- `docs/superpowers/engine-ab/PROTOCOL.md`: marked historical (bc/hs
  protocol), pointing at eu-hn3j0 for a redesign.
- `src/eval/stg/blob.rs`: corrected the `nodes`/`forms_pool`/
  `binding_entries` blob fields' documented consumer — these are **not**
  dead weight from the deleted HeapSyn loader; they still feed the runtime
  global list (`rt.set_prelude_bindings`) the bytecode encoder consumes.
  This was verified by tracing an actual read site
  (`src/driver/eval.rs:399-408`) before writing the correction — the
  original "HeapSyn engine loader" label undersold what these fields do.
- `scripts/capture-oracle-corpus.sh`: marked obsolete (its bc/hs
  cross-validation mechanism can no longer validate anything); left as a
  historical record of the eu-2sa6.14 prototype rather than deleted.
- `ROADMAP.md`: updated the live "Phase-4 collapse deferred" status markers
  (§4.2, §4.4, index table) to shipped in 0.15; added an `eu-oufc` index
  row. Left the per-release "shipped scope" table rows (0.12/0.12.1/0.13)
  untouched — they're historical records of what each release actually
  shipped, correctly stating "deferred" as of that release.
- `CHANGELOG.md`: entry under `[0.15.0] - Unreleased`.
- Left untouched (deliberately): `docs/superpowers/{plans,specs,reports}/`
  — dated, point-in-time historical records; rewriting them to describe a
  later deletion would falsify history. `docs/llms-full.txt` — generated
  from other docs (`scripts/generate-llms-full.sh`); regenerating it here
  would have pulled in ~1,200 lines of unrelated pre-existing drift into
  this PR's diff, so left for a separate regeneration pass.

## Verification

- `cargo fmt --all`, `cargo clippy --workspace --all-targets -- -D
  warnings`: clean.
- `cargo check --workspace --all-targets`: clean (lib, tests, benches,
  xtask).
- `cargo test` (full suite): all green, including the 46 ported
  `testing::machine()` unit tests, the rewritten differential tests, and
  the WASM pipeline's native unit tests.
- `EU_GC_VERIFY=2` / `EU_GC_POISON=1`: full `harness_test` suite (555
  tests) run clean under both flags together — 555 passed, 0 failed. (An
  earlier run under heavy concurrent background load produced one
  spurious timeout in `test_228_bc34x_meta_body_memoisation`, a
  wall-clock-deadline-gated test; re-run in isolation and against a
  quiescent machine, both this branch and a fresh `master` baseline
  execute the underlying binary at parity — direct `-S` statistics runs
  showed near-identical tick counts (2,969,088 vs 2,942,423) and
  bytecode-eval time (0.578s vs 0.573s) — confirming the timeout was
  cargo-test/process-scheduling noise, not a Phase 4 regression.)
- Source-prelude tick-parity tripwire (`tests/tick_parity_test.rs`): passes
  against a real embedded blob.
- AoC spot-check: built this branch's release binary and a fresh
  `origin/master` baseline (commit c3d85655) side by side and diffed
  output on the same days the eu-7oshh A/B study gated on — day03
  part-1 (17301) and part-2 (172162399742349), day08 part-1 (54600) and
  part-2, day09 part-1 (4744899849) and part-2 — all six byte-identical
  matches.
