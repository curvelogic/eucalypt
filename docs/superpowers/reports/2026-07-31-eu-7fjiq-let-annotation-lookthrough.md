# eu-7fjiq: target_annotation Let/LetRec look-through — findings and corpus movement

2026-07-31, Clarion, DIRECTED mode. Branch `fix/clarion-eu-7fjiq-let-annotation`,
PR to master, reviewed by wicket (not the owner personally — this is a directed
brief, not a proactive Clarion PR).

## The fix

`Machine::target_annotation` (`src/eval/machine/vm.rs`, HeapSyn engine) and
`bytecode::machine::target_annotation` (`src/eval/bytecode/machine.rs`, both
byte-dispatch and pre-decode paths) resolved a thunk's fallback source
annotation by peeking only the *single leading node* of its compiled body. The
inliner's argument-sharing `Let` (eu-gua64, PR #1086) can compile `Ann(App)`
as `Let(..., Ann(App))`, which hides the `Ann` from a single-node peek and
silently drops the annotation.

Both engines now walk through a leading `Let`/`LetRec` chain to find the
`Ann`, recursing rather than peeking one level, because repeated inlining can
stack more than one sharing wrapper. All three dispatch paths (HeapSyn,
byte-dispatch, pre-decoded bytecode) needed the change — confirmed by
reproducing the bug under all three before fixing (`eu --heap-limit-mib 4096
--source-prelude` on the default build, plus `EU_HEAPSYN=1` and
`EU_PREDECODE=0`).

Verification: 7 new unit tests (3 in `vm.rs`, 4 in `bytecode/machine.rs`,
covering both dispatch paths) construct a `Let`/nested `Let(LetRec(...))`
wrapping an `Ann`, call `target_annotation` directly, and assert the smid is
found. A fourth/eighth negative test asserts a `Let` with no `Ann` anywhere
in its body still returns `Smid::default()` (no invented location). Fault
injection: reverting each `target_annotation` to its old single-node-peek
form makes the corresponding positive tests fail with `Smid(None)` instead of
the expected smid; restoring the fix makes them pass again. Confirmed for
both files.

## error_176.eu's verification criterion is NOT met — and why

The bead's own verification target — `tests/harness/errors/error_176.eu`
under `--source-prelude` regaining the "called from here" secondary at 16:22
and the "- result at ..." stack-trace line, while keeping the primary at
19:32 — is **not achieved by this fix**, confirmed empirically (before/after
binaries, byte-identical output on that fixture with or without the fix).

Root-caused via `EU_ERROR_TRACE_DUMP=1` and direct debug instrumentation
(temporary, removed before commit):

- `target_annotation`'s fix *does* work correctly for `error_176`: with it,
  `__shared_e`'s own thunk (the eu-gua64 sharing binding for
  `make-greeting("world")`) correctly resolves smid 7395 instead of
  `Smid::default()`. But `__shared_e`'s `Update` continuation is popped
  (evaluation completes successfully) *before* the `//=` assertion fails, so
  it is never on the stack — and never in `env_trace` — by the time the error
  is constructed. Fixing its annotation has no path to the rendered output.
- The actual missing piece is `result`'s *own* `Update` continuation
  annotation. `result`'s compiled thunk body is `Let(__shared_e = ...,
  EXPECT(...))` with **no `Ann` node anywhere along its leading spine at
  all** — `ProtoLet::take_syntax` (`src/eval/stg/compiler.rs`) never reads
  the Core `Expr::Let`'s own smid field, so no amount of VM-side look-through
  can find an annotation that was never emitted. This is a **compiler-side**
  gap, not a VM-side one.
- I prototyped wrapping `ProtoLet`'s output in `dsl::ann(smid, ...)` when the
  Core `Let`'s own smid is valid. It broke primary-location attribution
  broadly (the diagnostic for `error_176` collapsed to a whole-file span,
  1:1, because unrelated outer `Let`s — e.g. the top-level block-to-let
  desugaring — also carry a valid smid and now shadow more specific inner
  locations). Reverted; this needs a properly scoped design (e.g. limited to
  the specific sharing-`Let` shape eu-gua64 introduces), not a blanket
  `ProtoLet` change, and is out of scope for this fix.
- Separately, `error_176`'s error is raised from inside the `__EXPECT`
  intrinsic's own sub-evaluation (`render_debug_repr_forced` →
  `evaluate_to_whnf`), which **restores the caller's stack before
  propagating an error** — confirmed by the pre-existing test
  `test_evaluate_to_whnf_impl_restores_caller_state_before_propagating_error`
  (`vm.rs`). A constructed probe using a plain (non-`//=`) type error, where
  the error propagates through the ordinary `?` chain with no nested
  `evaluate_to_whnf`, shows `result`'s Update continuation annotation
  correctly seeded by the target-eval driver in both fixed and unfixed
  binaries — i.e. that path doesn't need this fix either, for a different
  reason.

I could not construct an end-to-end `.eu` harness case where this specific
fix is the deciding factor in the *rendered* diagnostic — every scenario
tried is covered by some other annotation mechanism, or (for `error_176`
itself) blocked by the separate compiler-side gap above. The regression
tests for this PR are therefore Rust unit tests against `target_annotation`
directly (see fault-injection above), not a `tests/harness/errors/*.eu` case.

## Corpus movement (0 of 213 differ)

Full `cargo xtask diag-snapshot --bless` run (fresh blob, both prelude
modes): **0 snapshot files changed**. `git diff --stat
tests/diagnostics/snapshots` is empty. `DIVERGENCE.md`'s "N of 213" count is
unchanged. Nothing to re-bless, nothing to explain line-by-line — the fix is
real (see unit tests) but has no *observable* effect anywhere in the current
213-fixture corpus under blob or `--source-prelude` mode, for the same
reason `error_176` doesn't move: every affected `Update` continuation in the
corpus is either popped before any error surfaces, or covered by another
already-correct annotation source.

## Bonus finding: EU_HEAPSYN divergence 7 → 6 (eu-l51r7)

While re-running the corpus under `EU_HEAPSYN=1` and `EU_PREDECODE=0` per the
gates, found that this fix **does** move one fixture in the eu-l51r7 engine
divergence inventory:

- `errors/085_destructure_short_list`, `--source-prelude`, `EU_HEAPSYN=1`:
  before this fix, `stack trace:` has 1 frame (`f at 085_...eu:2:1`); with
  it, 2 frames (`f at 2:21` then `f at 2:1`), matching the blob/default-engine
  golden exactly. Confirmed by building master (9855417a) unmodified in a
  scratch worktree and diffing against this branch's binary — same command,
  same fixture, only the frame count differs.
- `EU_HEAPSYN=1` divergence: **7 → 6** fixtures (085 no longer diverges).
- `EU_PREDECODE=0` divergence: unchanged at **6** fixtures (085 was
  HeapSyn-specific per eu-l51r7's own analysis, and remains so — byte-dispatch
  never had it).
- The two non-default sets are now **identical** (6 fixtures each, same
  list): `errors/049_dot_on_number`, `errors/050_dot_on_string`,
  `errors/102_dot_on_list_source_loc`, `errors/149_not_value_source_loc`,
  `errors/191_m93j_lookup_on_function`, `provocations/lookup_on_function`.
  Before this fix there were two disjoint mechanisms (per eu-l51r7's own
  note); now there is one.
- Every remaining divergence is still exactly the documented shape: one
  `stack trace:` note frame missing, duplicating the primary label's own
  file:line:col. Confirmed by inspecting the `cargo test` diff output for
  each of the 6 under both `EU_HEAPSYN=1` and `EU_PREDECODE=0`.

`tests/diagnostics/DIVERGENCE.md` is updated (via `snapshot_engine.rs`'s
`render_divergence_doc`, so the auto-generated file and the
`divergence_inventory_is_current` assertion stay in sync) to state plainly
that its table covers the default engine only, and to record 6/6 (not the
previously-recorded 7/6) with the eu-l51r7 reference. Recommend the
coordinator or owner update eu-l51r7 itself with the corrected count; I have
not touched the bead (Clarion does not close beads, and updating its own
count felt like the coordinator's/owner's call given it's a P2 owner
decision record).

## Gates run

- `cargo fmt --all`, `cargo clippy --workspace --all-targets -- -D warnings`:
  clean.
- `cargo test --release` (fresh blob via `cargo xtask prelude-compile`,
  `rm -f lib/prelude.blob` first per the stale-blob hazard): 23/23 suites
  green, 0 failed.
- `EU_HEAPSYN=1 cargo test --release`: 6 diagnostics-snapshot mismatches
  (the known, now-updated eu-l51r7 set) — everything else green.
- `EU_PREDECODE=0 cargo test --release`: 6 diagnostics-snapshot mismatches
  (same set) — everything else green.
