# Derived prelude demand signatures: implementation report

**Bead:** eu-oaz72.77 (AoC 2026 preparation epic eu-oaz72)
**Date:** 2026-11-13, branch `eu-oaz72.77/derived-prelude-signatures` from master `b8bfb43`
**Design:** step 2 of `2026-11-13-prelude-demand-signatures-design.md` (eu-oaz72.75)

## What changed

- `cargo xtask prelude-compile` runs `derive_top_level_signatures`
  (`src/core/analyse_demand.rs`) over the merged prelude core, after the
  SCC split source mode also applies. It keeps each top-level lambda's
  signature by binder (the pointer-keyed table), not the name-merged one.
  A name bound twice along the top-level chain, and every non-lambda
  binding, gets no signature. 233 of 367 slots carry one.
- The signatures are stored in the blob as `demand_signatures`, parallel
  to `binding_entries`. They are used to compile the blob's own bodies
  (intra-prelude calls) and, at run time, for user calls.
- `build_prelude_signatures` (the 64-entry hand table) is deleted.
- Compiler lookup is keyed by callee identity: intrinsic → catalogue;
  `Var::Bound` → the analysis' named table; `Var::Free` resolving to a
  prelude global → that global's slot. This closes L1 and L3 of the design
  report: a user helper's signature no longer reaches a same-named prelude
  global, and no prelude signature reaches a user binding.
- Blob format: wire-format version 9 → 10. The freshness hash also absorbs
  `src/core/analyse_demand.rs` and `src/core/demand.rs`
  (`DEMAND_ANALYSIS_PATHS`), so a blob built by an older analysis is stale
  and falls back to the source prelude rather than being misread.
- Intrinsic signatures are unchanged. Alias and partial-application
  propagation (`zip-with: map2`, `zip: zip-with(pair)`) is not done: those
  slots have no signature, which is sound.

## Correctness

All 23 programs in §2.2 of the design report, and L1–L3 of §2.3, give the
source-mode result in blob mode (L3 checked by hand with
`--suppress-demand-analysis`, a debug flag no harness test can pass). The
master binary panics on 22 of the 23 and reports the wrong error on `nth`.
Gating checks:

- `tests/harness/165_demand_annotation.eu`, targets `test-prelude-laziness`
  (§2.2) and `test-prelude-names` (L1, L2);
- `tests/harness/errors/274_nth_negative_lazy_list.eu` (the `nth` error);
- unit tests `top_level_signatures_are_per_binder`,
  `demand_signatures_round_trip` and `demand_analysis_changes_change_the_hash`.

Fault injection: (1) xtask marking every derived parameter strict fails
`test-prelude-laziness` and the 274 error test; (2) the compiler falling
back to the user name table for `Var::Free` callees fails
`test-prelude-names`; (3) dropping the duplicate-name rule fails the unit
test. Each passes again once restored. The regenerated blob is
byte-identical after restoring.

## Diagnostics

Two blob-mode snapshots change (re-blessed):

- `errors/error_176`: the blob rendering loses a "called from here"
  secondary label and now matches source mode on that fact.
- `errors/110_unknown_short_arg`: the blob trace gains one frame,
  `in 'lookup-or' (prelude)`, the `:boundary` combinator whose default
  raises the error. The primary location and message are unchanged. Source
  mode does not show the frame, so the divergence inventory goes from 43 to
  44 fixtures. The `lookup-or` global and the user STG are unchanged; the
  difference comes from intra-prelude calls in `parse-args` now being
  compiled with derived signatures.

## Performance (measured-single)

Ticks and max stack are deterministic. One run of each, release builds of
master `b8bfb43` and this branch, each with its own freshly generated
blob. Wall times are not quoted.

| Bench | Master ticks / stack | Derived ticks / stack | Ticks |
|---|---|---|---|
| 015 block_merge | 2,027,082 / 3,032 | 1,927,862 / 6,017 | −4.9% |
| 016 yaml | 5,547,807 / 25 | 5,400,781 / 2,175 | −2.7% |
| 017 toml | 5,491,964 / 25 | 5,344,938 / 2,175 | −2.7% |
| 018 string_scale | 2,429,830 / 14,017 | 2,345,806 / 14,017 | −3.5% |
| 019 list_scale | 1,079,258 / 16 | 971,230 / 12,009 | −10.0% |
| 020 lookup_curve | 7,895,001 / 47 | 7,887,163 / 304 | −0.1% |
| 021 io_loop | 1,213 / 27 | 1,213 / 27 | 0 |
| 022 hof_fold | 51,700,735 / 800,006 | 46,900,711 / 800,006 | −9.3% |

`range(0, 1000000) map(_ + 1) sum`: master 179,750,178 ticks / max stack
13; derived 161,750,150 / 2,000,006. Source mode on master was 145.8M /
2,000,007 (design report §4).

The stack increases are the cost the design report predicted (§4). They
come from removing unsound claims, mostly `take`'s list. Blob mode now
matches source mode's stack behaviour. eu-oaz72.78 (speculating cheap,
total expressions) is the bead that recovers constant stack soundly.

## Follow-ups

- Recover constant stack for `range`/`take` pipelines: eu-oaz72.78.
- Optionally propagate signatures through aliases and partial
  applications.
- The 110 trace divergence (one extra boundary frame in blob mode).
- Analysis precision gaps the old table masked (design report §5 step 4).
