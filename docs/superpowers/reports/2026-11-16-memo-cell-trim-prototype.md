# Trimming stored thunks in `memo_cell`: prototype (eu-oaz72.84)

Date: 2026-11-16. Prototype for the owner to assess; not for merge without a
decision. It implements the narrower fix of §6 in
`2026-11-15-flat-closures-design.md` (PR #1237): no flat closures, no wire
format or compiler change.

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
2. `minimal_env` builds one frame for the innermost 16 slots of `T`'s chain
   (flat indices unchanged, so no code is renumbered), holding only the free
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
- New invariants: (a) `free_slots` must size every binder's frame as the
  machine does (an under-count is safe, an over-count drops a free index);
  the hole values make a miss loud. (b) A copied slot is a value, never an
  unevaluated thunk. (c) Overwriting `E[k]` with the shared cell is invisible:
  the cell enters the same code over the same values. (d) Frames beyond slot
  16 are shared, not copied, so retention there is as before.
- GC and unsafe: no `unsafe` added. Allocation happens inside an intrinsic,
  which reaches no safe point between allocations (as `detach` already
  relies on). The one new write, `E[k] := cell`, is `EnvironmentFrame::update`,
  the call the `Update` continuation makes, so any write-barrier behaviour is
  that path's. Validated under `EU_GC_VERIFY=2 EU_GC_STRESS=1` on the whole
  harness (exit 0, no FAIL).
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
- Owner choices: (1) whether a store may rewrite the caller's slot with the
  shared cell (the only way to keep sharing); (2) the constants
  (`MINIMAL_ENV_OUTER` 16, `MINIMAL_ENV_SLOTS` 64, `FREE_VAR_SCAN_NODES`
  1,024), picked to fit the repro and a unit-level frame of about 70 slots;
  (3) whether this replaces the `update-strict` workaround or sits beside it.
- Bead eu-oaz72.84 is missing from this store, so no bead was written.
