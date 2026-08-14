# BV3 (register frames, eu-2sa6.2) — fresh assessment for the 0.15 scope decision

> Owner-accepted 2026-08-11. Produced for the ROADMAP §9 re-dating decision.

- **Date:** 2026-08-11. **Assessor:** Fable assessment agent (read-only).
- **Code state assessed:** origin/master @ 9f0a57a8 (post-#1121 single-engine collapse).
  Local checkout was behind (a561bb71); all source reads via `git show origin/master:`.
- **Measurement binary:** `/Users/greg/dev/curvelogic/eucalypt-worktrees/furnace-oufc/target/release/eu`
  (built 2026-08-06 at 991c21d5, the #1121 branch tip — engine-identical to origin/master).
  All runs under `timeout` with `--heap-limit-mib 12288`.
- **Machine conditions:** load average 2.6–4.8 during the session (protocol wants <1).
  All wall/profile figures below are **measured-single**; tick and lookup counts are
  deterministic and **measured-verified** in kind.

## 1. The problem BV3 targets

The runtime environment is a cactus chain of GC-heap `EnvironmentFrame<BcValue>`
nodes (`src/eval/machine/env.rs:313`, generic struct kept by #1121 solely for
`BcEnvFrame`). A local reference is a flat de Bruijn-style index resolved by
walking the chain subtracting frame lengths (`cell()`), through a `remap` indirection
for shared-backing frames. Every let/letrec/saturation/case-fallback allocates a
frame on the GC heap (`BcEnvBuilder`, `bytecode/env_builder.rs`); frames are
GC-scanned (`GcScannable for EnvironmentFrame`, including backing-array evacuation
handling). `enter_local` (`bytecode/machine.rs:1949`) adds thunk-check,
black-hole write and `Update`-continuation push per entry.

BV3 proposes flat register frames for the common case, spilling to heap frames only
where a closure genuinely escapes (shared escape analysis with CG4). It would attack:
(a) the per-lookup constant (chain hop, scoped-ptr construction, remap check),
(b) frame allocation churn and GC scan pressure for non-escaping frames.

The bead's history matters: **three successive motivating cases have now been
resolved by cheaper, targeted fixes**:
1. Prelude `foldl` O(n²) — solved by copy-specialisation (#1010, 0.13). Retired 2026-07-14.
2. `count/sum/filter/map`/`str.len` O(n²) — BV3 explicitly REFUTED as the fix;
   solved by eager_args/demand in blob compile (eu-e3c3i, #1068, ~85 lines, 2026-07-25).
3. **eu-famg stream O(N²)** — the only bead still `DEPENDS ON` BV3 — **resolved on
   current master** (this session, §2c below).

The surviving value case (eu-98zg corrected account, re-confirmed 2026-07-24) is a
**constant-factor** one: cross-function-call lookup cost in non-recursive, call-heavy
operator functions (day07 `row-step` calling sibling helpers as free variables),
structurally unreachable by copy-specialisation.

## 2. Evidence: the cost has collapsed again since the last re-eval

The 2026-07-24 re-eval (docs/superpowers/reports/2026-07-24-bv3-value-case-reeval.md)
predates #1068 (merged 2026-07-25), the meta-memoisation fix (eu-bc34x, 2026-07-31),
the let-annotation lookthrough (eu-7fjiq, 2026-07-31) and the #1121 collapse. Fresh
numbers on the post-collapse binary:

### (a) Deterministic layer — the lookup population is largely gone

| Workload | Ticks Jul-24 | Ticks now | Δ | Lookups Jul-24 | Lookups now | Δ | Mean walk depth now |
|---|--:|--:|--:|--:|--:|--:|--:|
| day07 part-1 | 42,350,644 | 15,236,325 | **−64%** | 31,529,683 | 11,472,712 | **−64%** | 0.29 ann / 0.00 unann (max 7) |
| 018_string_scale | 46,044,700 | 2,646,417 | **−94%** | 44,469,459 | 1,715,244 | **−96%** | 0.48 / 0.00 (max 5) |
| 019_list_scale | 51,219,477 | 1,390,918 | **−97%** | 50,460,211 | 1,080,244 | **−98%** | 0.24 / 0.00 (max 4) |

018 (71 ms mutator) and 019 (30 ms) are now **below the 200 ms profiling floor** —
both of the July re-eval's bench workloads have joined 022_hof_fold in the
"unprofilable because solved" bucket. The mean walk depth of ~0.2–0.5 means the
"walk" is essentially a single frame access already; there is no depth left to
flatten, only the per-lookup constant.

### (b) Profile layer — day07 residual share roughly halved

`sample` (1 ms, eval-thread leaves ≥5, `__ulock_wait`/`semaphore_wait_trap`
excluded), day07 part-1, 2 usable rounds (a third round under-sampled — the run
is only ~0.33 s now):

| Round | enter_local + EnvironmentFrame::get | Dispatch cluster (handle_op_predecoded + dispatch + step/run) | Alloc cluster (make_arg_array_pd + try_allocate + Array::push + bump + saturate_with_array) |
|---|--:|--:|--:|
| 1 | 16.3% (29/178) | ~41% | ~27% |
| 2 | 13.0% (25/192) | ~36% | ~25% |

Versus the July re-eval's day07 median of 30.5% (range 25.3–43.6, load ~25) and
eu-98zg's 20.5%. **The residual env share is now ~13–16%, third behind dispatch
(~36–41%) and allocation (~25%).** measured-single; load 2.6–4.8.

### (c) eu-famg's stream quadratic is fixed — BV3's last dependent bead

Reconstructed the eu-famg workload (jsonl-stream import → `map(lookup(:a))
map(_ + 1)` → `count`+`sum`, correct outputs verified):

| N | Wall (this session) | eu-famg baseline (2026-07-03) |
|--:|--:|--:|
| 4,000 | 0.05 s | 0.26 s |
| 8,000 | 0.08 s | 0.95 s (~3.7× per doubling) |
| 16,000 | 0.14 s | 4.13 s (~4.3× per doubling) |
| 32,000 | 0.27 s | — |

Per-doubling ratio 1.6→1.75→1.93: **linear**. The class bug eu-famg tracked is gone
on current master — the fix vector turned out to be eu-e3c3i's eager_args, not BV3
or lazy-streams. (Caveat: this is a reconstruction of the #960-review repro, not the
original script; but a 16k-element stream map/count/sum at 0.14 s vs the recorded
4.13 s baseline settles the complexity class.) **eu-famg's `DEPENDS ON eu-2sa6.2`
edge should be removed and the bead re-verified/closed independently.**

## 3. Design sketch on the current machine (what BV3 would actually mean)

- **Operand model:** `DecodedRef::Local(u32)` is a flat chain-relative index. A
  register-frame design needs the compiler (STG→bytecode emit, plus the blob
  wire format and `predecode.rs`) to distinguish register slots from spilled
  heap slots — an encoding change to the blob format (recorded-review surface,
  and a blob-compatibility/regeneration event).
- **Escape analysis:** does not exist. Must decide, per binding, whether any
  closure capturing the frame outlives the frame's dynamic extent. Interacts
  with the open eu-9tah.12 (soundness escape analysis for block bindings) —
  the ROADMAP requires the BV3 design note to settle whether they share one
  computation; that note has never been written.
- **Runtime:** a register file must still be a GC **root set** (slots hold
  `BcValue`s including heap pointers); black-holing and `Update` continuations
  currently address `(RefPtr<BcEnvFrame>, index)` pairs — thunk update targeting
  a register frame that may have been popped needs a new mechanism (this is the
  classic hard part: updateable thunks capturing not-yet-spilled frames).
- **The lesson already on record:** the reverted flat-closure attempt (ROADMAP
  §10) sank on a universal capture tax; BV3 is the selective relocation of that
  idea, which is why it is compiler-decision-heavy, not a local VM patch.

Effort: multi-week, cross-cutting (compiler emit + blob format + predecode IR +
GC roots + update machinery), on the highest-risk surface in the codebase
(memory/GC — six historical SIGSEGV root causes live in exactly this area).
The "high value, high risk" label was written when the expected win was
40–50% of self-time; the risk is unchanged, the value is now bounded by ~13–16%.

## 4. Sequencing vs eu-gmdl5 (CG4) and eu-1hcw

- **eu-gmdl5 (CG4 selective lambda-lifting):** still unimplemented; still the
  cheaper, compiler-only lever for the identical population, so the ordering
  "CG4 before BV3" stands. But CG4's own case has weakened in step (its
  2026-07-25 comment already lowered priority): it would also be chasing a
  ~13–16% share whose upper bound keeps falling every time a targeted fix
  lands. CG4 is worth keeping as an unscheduled spike, not a 0.15 item.
- **eu-1hcw (byte-dispatch deletion):** orthogonal axis (dispatch strategy
  inside the engine), already owner-sequenced last-in-cycle with a
  quiet-machine engine-ab session. That session is the natural place to bank a
  **measured-verified** enter_local/get baseline on the single engine, which
  is exactly the number any future BV3/CG4 revival should be gated on — no
  separate profiling effort needed.
- **What the profile actually says to do next (if perf work is wanted):** the
  top leaves are now dispatch (~36–41%) and allocation/arg-array churn (~25%).
  Frame-allocation reduction (the second half of BV3's pitch) overlaps the
  alloc cluster, but so do cheaper ideas (arg-array reuse, saturation
  fast-paths) that don't change the frame model or the GC contract.

## 5. Recommendation

**DEFER — unscheduled. Take BV3 out of 0.15 scope (re-parent off eu-sks3l); do
not implement, do not write the design note, do not retire the bead outright.**

Rationale in one line: BV3's value case has been overtaken by cheaper fixes for
the **third** time — its residual (a per-lookup constant on an already-flat walk,
~13–16% of self-time on the best remaining witness, behind both dispatch and
allocation) no longer justifies the machine's highest-risk structural change,
and its sole dependent bead (eu-famg) is measured fixed.

Concrete actions:
1. Remove eu-famg's dependency on eu-2sa6.2; independently re-verify eu-famg
   with the original #960 repro and close it (this session's reconstruction:
   linear at 4k–32k, ~30× faster at 16k).
2. Record the revival gate on eu-2sa6.2: a **measured-verified** (quiet-machine,
   PROTOCOL) profile showing `enter_local`+`get` median ≥ ~25–30% on a workload
   that matters, on post-#1068 master — naturally banked during eu-1hcw's
   owner-run engine-ab session at 0.15's close.
3. Keep eu-gmdl5 open as the designated first lever if the gate ever trips,
   at lowered priority; whichever of CG4/BV3 ever starts writes the shared
   escape-analysis note covering eu-9tah.12.
4. Fix the stale ROADMAP §9 row ("BV3 … 0.13") to match.

**No spike is needed:** eu-98zg (2026-07-14), the 2026-07-24 re-eval, and this
session's refresh constitute three profiling passes in four weeks, each finding
the target smaller than the last. The question is settled for this cycle.
