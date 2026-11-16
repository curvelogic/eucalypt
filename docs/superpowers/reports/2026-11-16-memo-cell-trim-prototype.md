# Trimming stored thunks in `memo_cell`: prototype (eu-oaz72.84)

Date: 2026-11-16. Prototype for the owner to assess; not for merge without a
decision. It implements the narrower fix of §6 in
`2026-11-15-flat-closures-design.md` (PR #1237): no flat closures, no wire
format or compiler change.

## 0. Is `update-strict` still needed? Yes: counting (owner question on choice (c))

Question: is there a realistic case where `dict.update-strict` is still
required once `memo_cell` trims stored thunks? There is, and it is the
commonest dict idiom, counting. Release build of this branch, source prelude,
deterministic counters (`eu -S`); confidence measured-single (one run each,
counters are deterministic). Not re-run on master: the trim never fires on this
shape (see the mechanism), so master is expected to match, but that is
projected, not measured.

```
step(acc, i): acc dict.update(i % 10, + 1, 0)       # or dict.update-strict
out: range(0, 1000000) foldl-strict(step, dict.empty) dict.get(3)
```

| 10 keys, `foldl-strict` | peak blocks | outcome |
|---|---|---|
| `dict.update`, 200,000 items | 14,565 (30.4M ticks) | passes |
| `dict.update-strict`, 200,000 items | 1,027 (33.2M ticks) | passes |
| `dict.update`, 1,000,000 items | exceeds `--heap-limit-mib 2048` | fails |
| `dict.update-strict`, 1,000,000 items | 1,027 | passes |

A single key in a `seq`-forced loop
(`loop(i, n, acc): seq(acc, (i = n) then(acc, loop(i + 1, n, acc dict.update(:k, + 1, 0))))`)
shows the same: at 1,000,000 steps 65,811 peak blocks against 1,027, and under
`--heap-limit-mib 512` the lazy form fails and the strict form passes. The
committed bench `024_dict_counting` (strict) peaks at 2,836 blocks; with
`update` for `update-strict` it peaks at 29,161 (51.5M ticks both ways).
Retention grows linearly with the updates, so it is heap, not stack depth: a
1,000,000-deep thunk chain was forced without overflow.

**Mechanism.** The stored value is the thunk `f(old)`, where `old` is the
previous stored value of the same key. That is itself an unevaluated thunk, so
a free variable of the new thunk is not yet a value and `memo_cell` declines to
trim it (§5: a free variable that is an unevaluated thunk defeats the trim).
The result is a chain of N pending thunks per key. No later store can shorten
it without forcing `old`, which would change laziness. `update-strict` forces
`f(old)` at store time, so there is no chain. This is the same limit that
flat closures could not remove either.

**Uses of `update-strict` and whether `memo_cell` covers them** (all by
`grep`; none is covered):
- `lib/prelude.eu`: `dict.merge-with-strict` folds `update-strict` over `b`'s
  entries; the same chain forms on a key present in both dicts. Needed.
- `tests/harness/bench/024_dict_counting.eu`: counting; measured above. Needed.
- `tests/harness/250_dict.eu` (`strict-step`, `updates`) and
  `typecheck/116`, `typecheck/118`: counting and typing checks; the counting
  use is needed.
- `tests/harness/errors/250_dict_update_strict.eu`: pins that the error in
  `f(old)` surfaces when the dict is built. That is a semantic difference
  (eager evaluation), not a memory one, so no store-time trim can replace it.
- Docs: `docs/guide/dicts.md`, `docs/guide/advent-of-code.md` (three places,
  including the loop advice), `docs/appendices/cheat-sheet.md`,
  `docs/reference/agent-reference.md`, `docs/reference/prelude/dicts.md` and
  the generated `docs/llms-full.txt`. All teach counting loops and stay
  correct.

**Recommendation: keep `update-strict` (and `merge-with-strict`); do not
remove it.** Nothing about it is esoteric: it is the word-count and
frequency-table idiom, and removing it loses a 10x to unbounded retention
difference there. Cost of removing now: the prelude, bench 024, five test
files and six doc pages change, and every counting loop leaks unless users
write their own forcing. Cost of deprecate-then-remove: the same, a release
later, plus a deprecation path, for no gain. What `memo_cell` does fix is the
neighbouring case: values built over the loop's own evaluated variables
(`[i + 1, i]`, block literals), where the user has no function to reach for.
It complements `update-strict`; it does not replace it.

## 1. The problem, reproduced

A loop that threads a dict and stores a value built over the loop's own
variables (`acc dict.assoc(i % 1000, [i + 1, i])`) retains every earlier
version of the dict: the stored thunk, or the thunk in a stored constructor's
field, closes over the iteration's frame, which holds `acc`.
`assoc-chain-lazy` (`tests/harness/250_dict.eu`) stores `[i + 1, i]` and a
block literal 50,000 times.

| | master | branch |
|---|---|---|
| `--heap-limit-mib 64` | heap limit exceeded | passes |
| peak heap blocks (`-S`, limit 12288) | 6,912 | 1,079 |
| wall, 7 interleaved pairs | 1.42 s [1.23..1.59] | 0.80 s [0.59..0.91] |

Peak blocks: measured-verified (deterministic). Wall: measured-single.
The harness gate asserts peak < 2500 under a 64 MiB limit
(`test_250_dict`). The same loop storing each of `[i + 1, i]`, `{a: i, b: i}`, `i + 1`
and `[i % 7, i]` was checked ad hoc under `--heap-limit-mib 64`: master
exceeds the limit on all four, the branch peaks at 1,119 to 1,194 blocks.

## 2. What it does

`memo_cell` already stored an unevaluated argument as a cell around the
`OP_ATOM` alias the intrinsic received, which pins the alias's frame. Now,
before that, `share_trimmed_thunk` walks the alias to the slot `(E, k)` that
holds the real thunk `T`. If `T` is a plain unevaluated thunk (not a lambda,
an existing cell or a black hole) and its free variables allow it:

1. `free_slots` scans `T`'s bytecode for the flat environment indices it
   reads, tracking binder depth (`let`/`letrec` count, lambda arity, case
   branch constructor arity, fallback 1, `DeMeta` handler 2 / `or_else` 1).
2. `minimal_env` rebuilds the innermost 16 slots of `T`'s chain, one frame
   per original frame with the same size and annotation (so error traces
   are unchanged), at the same flat indices (so no code is renumbered),
   holding only the free
   values; every other slot is a black hole, so a missed read is a loud
   `<<loop>>` error, not a wrong value. Its parent is the original chain from
   the first outer frame on, shared, so a unit's top-level bindings are read
   through the same slots as before. A free variable in the copied region
   must already be a value (native, function, evaluated constructor, rebuilt
   by `detach` where possible); otherwise the whole thing bails and storing
   is as before.
3. A one-slot frame `F` holds `T` over that environment, the shared cell
   (`cell_template` over `F`) is written back into `E[k]` and returned as the
   stored value. The caller's slot and the dict then name the same `F[0]`.

Why a shared cell rather than copying `T`: a copy loses sharing. Storing one
thunk in N dicts, or storing it and reading it directly, would evaluate it
N times. `slot-shared` and `store-twice-shared` pin this: without the
write-back both double (4.07M ticks against 2.04M).

`detach` gains one case: an unevaluated constructor field (`[i + 1, i]` is an
evaluated cons with a thunk field) goes through the same
`share_trimmed_thunk`, against the constructor's own slot.

## 3. Benches 015–022 (plus dict benches 023, 024)

Two release builds, master `cb57a85` and the branch, each with a freshly
compiled blob (byte-identical, `prelude_config` blob), copied to paths of
equal length. rustc 1.99.1, Linux 6.18, 4 cores. `eu test --allow-io
--heap-limit-mib 12288`, interleaved pairs alternating order, 7 pairs per
session, two sessions (14 runs per cell). Load average was 1 to 2 (my own
runs), not under 1: wall figures carry that caveat.

**Ticks, allocs, peak blocks and collections are identical on master and
branch for all ten benches** (measured-verified: deterministic): none of
them reaches the changed path (they store natives or evaluated values), so
the change costs them nothing in work done.

| bench | master median s [min..max] | branch median s [min..max] | ratio | session 1 | session 2 |
|---|---|---|---|---|---|
| 015_block_merge | 0.822 [0.71..0.92] | 0.802 [0.69..0.98] | 0.976 | 0.943 | 1.004 |
| 016_import_export_yaml | 3.139 [2.84..3.53] | 3.213 [2.82..3.45] | 1.023 | 1.065 | 1.014 |
| 017_import_export_toml | 1.792 [1.60..2.15] | 1.804 [1.54..2.24] | 1.006 | 1.064 | 0.964 |
| 018_string_scale | 0.766 [0.66..0.89] | 0.805 [0.72..0.91] | 1.051 | 1.016 | 1.101 |
| 019_list_scale | 0.651 [0.59..0.85] | 0.686 [0.60..0.77] | 1.054 | 1.043 | 1.010 |
| 020_lookup_curve | 0.873 [0.82..0.94] | 0.887 [0.77..1.15] | 1.016 | 1.039 | 1.002 |
| 021_io_loop | 2.083 [1.94..2.27] | 2.056 [1.89..2.27] | 0.987 | 0.968 | 0.984 |
| 022_hof_fold | 5.423 [4.84..7.29] | 5.312 [4.68..6.11] | 0.980 | 0.993 | 1.065 |
| 023_dict_dijkstra | 4.302 [3.91..5.27] | 4.261 [3.89..5.07] | 0.990 | 0.998 | 1.013 |
| 024_dict_counting | 3.982 [3.30..4.51] | 3.617 [3.26..4.54] | 0.908 | 0.967 | 0.965 |

Verdict: **does not repeat eu-9tah.18's regression, and cannot, because the
work done is identical**. The wall ratios scatter 0.91 to 1.05 with sign
flips between sessions and spreads of 10 to 20%, so they are noise plus
binary layout (the two binaries differ). Confidence: measured-single for the
wall ratios (spread exceeds any effect; machine not quiet); a real 2 to 5%
change cannot be excluded by them, but the identical ticks and allocations
say there is no mechanism for one. `assoc-chain-lists` (evaluated values,
already detached) runs 0.54 s against 0.55 s (measured-single).

Not measured: workloads that store many small unevaluated thunks at high
rate with a deep inner region; the scan and one frame allocation are paid per
store there (bounded: 1,024 nodes, 64 slots).

## 4. Complexity

- Lines: `src/eval/bytecode/machine.rs` about +320/−6 (three methods, about
  60% comments), `src/eval/machine/env.rs` +6 (`next_frame` accessor),
  tests +62.
- New invariants: (a) `free_slots` must size every binder's frame exactly as
  the machine does. Neither direction is safe: every index read below a
  binder shifts with its frame size, so an under-count reports local slots
  as free and misses the true free index, and an over-count drops one. The
  missed index reads a black hole, which raises a spurious `<<loop>>` error
  at an unrelated location (loud, but wrong). A `debug_assert` in
  `return_data` now pins the case-branch link (constructor arity equals the
  node's argument count). (b) A copied slot is a value, never an
  unevaluated thunk. (c) Overwriting `E[k]` with the shared cell is invisible:
  the cell enters the same code over the same values. (d) Frames beyond slot
  16 are shared, not copied, so retention there is as before.
- GC and unsafe: no `unsafe` added. Allocation happens inside an intrinsic,
  which reaches no safe point between allocations (as `detach` already
  relies on). The one new write, `E[k] := cell`, is `EnvironmentFrame::update`,
  the call the `Update` continuation makes, so any write-barrier behaviour is
  that path's. Validated under `EU_GC_VERIFY=2 EU_GC_STRESS=1` on the whole
  harness (exit 0, no FAIL). One budget (`DETACH_BUDGET`, 64 constructors)
  is shared by `detach`, `share_trimmed_thunk` and `minimal_env`, which
  recurse into each other; a stored value that refers back to itself
  therefore ends the recursion (round-1 review blocker: with a fresh budget
  per `minimal_env` it overflowed the stack, exit 139).
- Tests: `cargo test` full, clippy `--workspace --all-targets -D warnings`
  and fmt clean. Fault injection (both reverted): disabling `minimal_env`
  makes `test_250_dict` FAIL on the new gate; skipping the write-back makes
  `slot-shared` and `store-twice-shared` double their ticks.

## 5. Limits and choices for the owner

- A free variable that is an unevaluated thunk is never copied (it must
  stay shared), so the loop variable must be evaluated at store time; a
  lazily-passed counter defeats the fix. This is inherent without flat
  closures.
- A free variable that is a function or an evaluated constructor keeps
  its own environment (constructors only when `detach` fails).
- Known limits (round-1/2 review): a second store of the same unevaluated
  value still leaks (the alias walk meets an already-trimmed cell and bails,
  so the second store cells the original chain; `slot-shared` and
  `store-twice-shared` pin correctness, not retention). The code is scanned
  twice per store when `share_trimmed_thunk` bails. The caller's slot keeps
  the shared cell for good, so later reads in that scope go through the cell
  and slot-inspecting fast paths (`LookupLit`, `cons_of` on raw slots) see a
  cell not a constructor; not exercised by benches 015–024. `detach` is no
  longer side-effect-free (it may have rewritten field slots before a later
  field fails; invisible to readers). The copy keeps one frame per original,
  so a deep inner region costs a frame header each (bounded by 64 slots).

## 6. Round 2: what changed after the two send-backs

- Blocker 1 (stack overflow on a self-referential stored value): one shared
  budget; harness target `cyc-values` in `250_dict.eu` (three repros), fault-injected
  (fresh budget per `minimal_env` aborts the test process; restored, PASS,
  also under `EU_GC_VERIFY=2 EU_GC_STRESS=1`).
- Blocker 2 (error traces): `minimal_env` keeps one frame and annotation per
  original frame. Errors-corpus tests `312_stored_value_trace` and
  `313_stored_list_field_trace` pin the trace; the output is byte-identical
  to master's on both, and both FAIL with the annotations dropped.
- Also folded in: duplicate free indices deduplicated, the cheap size check
  before the scan, the alias walk's last hop tested, `detach` doc comment,
  the arity `debug_assert`.

Round-2 review finding (both reviewers, independently): bounding the cycle
by the budget alone made a stored self-referential stream re-trim at every
level (about 8x ticks, 11x peak blocks against master; each level wrote its
own cell into the same slot). Fixed by refusing re-entry: `DetachState`
tracks the constructors (code and environment) whose rebuild is in progress,
and `detach` returns `None` at once for one already active. The stored
stream then costs exactly what it does without a dict (2,175,261 ticks both
ways, equal to master). `250_dict.eu` (`cyc-values`, `stream-stored-run`) gates it
(`stream-stored-run` against `stream-plain-run`, under 2x), fault-injected:
with the re-entry check disabled the stored stream takes 18,281,402 ticks.
The cycle checks live in `250_dict.eu` (`cyc-values`, `stream-stored-run`, `stream-plain-run`); the ticks gate is in `test_250_dict`.

Round-2 measurements (clean release builds of master and this head, source
prelude; deterministic counters, so confidence is measured-verified): benches
016–022 have identical ticks, allocations and peak blocks to master (015 as
well, measured on the pre-fix merge and this head); `assoc-chain-lazy` peaks
at 1,083 blocks against a master that exceeds `--heap-limit-mib 64`. Round 2
does not touch the hot path (the new code runs only inside `memo_cell`), so
no wall-clock rerun was made. Pitfall met on the way: a stale local binary
(left by a fault-injection build) showed spurious tick differences; the
figures above are from builds in a clean worktree.

## 7. Choices for the owner

(a) **May a store rewrite the caller's slot with the shared cell?** It is the
only way to keep one evaluation of a thunk that is both stored and read
directly; without it those cases evaluate twice (4.07M ticks against 2.04M).
Recommendation: yes, for this prototype, given the cost listed in §5 (the
slot keeps a cell) is small and unmeasured, not shown harmful.

(b) **The constants 16 / 64 / 1,024** (`MINIMAL_ENV_OUTER`,
`MINIMAL_ENV_SLOTS`, `FREE_VAR_SCAN_NODES`). They were fitted to the repro
and one unit-level frame of about 70 slots. Recommendation: keep them for
now and revisit with an AoC-shaped workload; they are bounds on cost, not
correctness.

(c) **Replace `update-strict` or sit beside it?** Recommendation: sit beside
it, and keep it. §0 gives the counting loop that still needs it: the stored
`f(old)` has an unevaluated free variable, which no store-time trim can fix
(65,811 against 1,027 peak blocks at 1,000,000 updates).
