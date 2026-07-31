# eu-bc34x: metadata-annotated bindings lose thunk memoisation — root cause and fix

Furnace, 2026-07-31.

## Root cause

A binding carrying metadata (a doc string, `:suppress`, etc.) compiles to
`Meta{meta, body}`. `is_whnf()` treats `Meta` as unconditionally WHNF, so the
binding is a non-updateable Value form — cheap to re-enter, by design.
`return_meta`'s "other" branch (any consumer that isn't itself destructuring
the metadata) used to resolve `body` with a bare, passive environment-slot
read (`HeapNavigator::resolve` / `resolve_ref`) instead of entering it
through the standard black-hole-then-`Update` ceremony every other reference
to an updateable thunk goes through. If `body` names a genuinely unevaluated
thunk, that thunk's own persistent slot never got an `Update` continuation
registered, so it never memoised: every reader independently re-ran the full
computation.

Two other candidates named in the bead were ruled out:
- **is_settled/create_arg_array**: only controls whether a fresh alias
  wrapper is built when a slot is passed as a call argument; irrelevant here
  since an alias onto the Meta wrapper is harmless either way.
- **is_whnf() alone**: even if the Meta wrapper were compiled as an
  updateable Thunk, `Continuation::Update`'s existing (unmodified) branch
  writes the *original, unresolved* Meta form back into the binding's own
  slot (correct — preserves `` ` ``/`meta` visibility), so widening
  `is_whnf()` changes nothing about whether the underlying computation
  memoises.

## Fix

Route `body`'s resolution through the same enter/black-hole/`Update`-push
logic as `Atom{Ref::L}` (`enter_local`/`enter_global` in
`src/eval/bytecode/machine.rs`; a new `enter_meta_body` helper mirroring
them in `src/eval/machine/vm.rs`), leaving the Meta node's own slot,
`is_whnf()`, and `is_settled`/`create_arg_array` untouched.

## A second, ordering bug found during review

The first version of this fix called `enter_meta_body` *before* pushing the
outer continuation back onto the stack. If `body` names an updateable
thunk, `enter_meta_body` pushes its own `Update` continuation for that
thunk's slot — and that push must land *above* the outer continuation
(fire first, memoising the thunk), exactly as it would if the thunk had
been entered directly via `Atom{Ref::L}` with the outer continuation
already sitting on the stack underneath. Pushing the outer continuation
afterwards put it on top instead: the thunk's completion value went to the
outer consumer *first*, and only the *result of that* landed in the
thunk's own slot.

This explained both apparent "latent bugs" surfaced by review (filed as
eu-096pd, now closed as invalid — see that bead's notes): the `str`
lookup-failure false "infinite loop" (a re-entrant read hitting a slot
still black-holed because its `Update` never got the chance to fire first)
and the blob-mode `ys map(_+1) sum` "tried to call a list as a function"
(a function's own slot overwritten with the result of *calling* it).
`hoist.rs` was never at fault. Fix: push the outer continuation back
*before* calling `enter_meta_body`, not after — a one-line reorder in
each engine.

## Verification

- `tests/harness/errors/*.eu` diff (before/after): **0/194 differ**, across
  blob × source-prelude × three dispatch engines (bytecode pre-decoded,
  byte-dispatch, HeapSyn). Confirmed via `scratch/furnace-bc34x-errors-diff.sh`
  (local, not committed) against a clean `origin/master` (642b5e2d) baseline
  binary.
- `cargo test --release` (both `EU_HEAPSYN` unset and `=1`): full suite
  green, including all 539 harness tests, the pre-existing eu-wpswc growth
  gate (`fold_over_map_growth_test`), and all 8 canonical benches
  (015–022).
- Ticks, confirmed binding case (`slow(N)` captured by an annotated binding,
  read K times via `map`+`sum`) vs. an unannotated control, N=300 K=20:

  | mode                  | annotated | control | Δ    |
  |------------------------|-----------|---------|------|
  | blob + bytecode-pd     | 15,446    | 15,389  | +57  |
  | blob + byte-dispatch   | 17,477    | 17,420  | +57  |
  | blob + HeapSyn         | 22,116    | 22,056  | +60  |
  | source + bytecode-pd   | 13,834    | 13,777  | +57  |
  | source + byte-dispatch | 16,322    | 16,265  | +57  |
  | source + HeapSyn       | 21,090    | 21,030  | +60  |

  Was 206,212–207,977 ticks (multiplicative) pre-fix, against the same
  ~13,000–22,000 controls. Correct result value (6190) in every case.
  Blob bytes byte-identical before/after (601895 bytes) — pure runtime fix.

- `EU_GC_VERIFY=2 EU_GC_STRESS=1`: clean, all engines/modes.
- New regression test: `tests/harness/228_bc34x_meta_body_memoisation.eu` +
  `test_228_bc34x_meta_body_memoisation` in `tests/harness_test.rs`. Gates
  on a 30s wall-clock deadline (fixed binary: ~0.09s measured; pre-fix
  binary: still running past 40s, killed manually) plus five correctness
  checks, including that `x0 meta`/`x1 meta` are still readable after the
  bindings have been forced (metadata must not be stripped by memoising the
  underlying computation).
- **Fault injection** (both required and performed):
  1. Reverted just the push-order fix (restoring `enter_meta_body` before
     `push(other)`): reproduces the wrong-value crash
     ("tried to call a list as a function"); `test_228_...` FAILs. Restored;
     re-confirmed PASS.
  2. Ran the new fixture against the untouched pre-fix binary
     (`origin/master` 642b5e2d): reproduces the deadline timeout (>40s, no
     completion); would FAIL `test_228_...`. N/A to restore (separate
     binary).

## Files

- `src/eval/machine/vm.rs` — `return_meta`, new `enter_meta_body`
- `src/eval/bytecode/machine.rs` — `return_meta`, new `enter_meta_body`
- `tests/harness/228_bc34x_meta_body_memoisation.eu` — regression fixture
- `tests/harness_test.rs` — `test_228_bc34x_meta_body_memoisation`

Branch `fix/furnace-eu-bc34x-meta-memoisation`, PR to `master`, owner review
(touches GC/VM machinery per CLAUDE.md's recorded-review requirement).
