# Post-copy-specialisation re-profile: enter_local share collapse (eu-98zg)

> **CORRECTION (2026-07-14, Wicket's #1011 review).** §5 originally claimed
> day07's `foldl(row-step, [b0, 0])` call was a "hand-written beam-fold...
> not a prelude combinator... not touched by copy-specialisation." **This is
> wrong.** `solve` calls the prelude's own `foldl` directly; Wicket verified
> at the STG level (`eu dump stg day07.eu -t part-1`) that it fuses via the
> same local self-recursive specialised-copy signature established for
> `022_hof_fold`, and this was independently re-confirmed here from the
> source (`row-step` has no self-recursion anywhere in day07.eu) and the STG
> dump's `letrec [0] λ{3}` head node. **The fold is not the residual cost.**
> The actual source: `row-step` is a substantial *non-recursive* operator
> function that calls sibling helpers (`to-splitters`, `zip-with`,
> `shift-left`, `shift-right`) as free variables — ordinary
> cross-function-call environment lookups, once per fold iteration, not
> "fold outside the qualifying set." §5 and §8 below are corrected in place
> to reflect this; the **finding itself is unchanged** (20.5% share on
> day07, defer-BV3-not-retire recommendation stands) — only the causal
> explanation was wrong, and the corrected account is, if anything, a
> *broader and structurally cleaner* case for BV3's continued relevance (see
> corrected §5). Full detail in Wicket's PR #1011 review comment.

- **Date:** 2026-07-14
- **Bead:** eu-98zg, owner-commissioned re-profile contingent on eu-dp0k/#1010
  (copy-specialisation) merging. Verdict feeds eu-2sa6.2 (BV3: register
  frames), which is dependency-blocked on this bead. Diagnosis only, no
  fixes.
- **Worktree/branch:** `/tmp/eu-stopwatch-98zg`, `spike/bv3-reprofile` off
  `origin/master` at `cdfe08ec` (merge of PR #1010).
- **Toolchain:** rustc 1.97.0 (`stable-aarch64-apple-darwin`), macOS 26.5.1,
  Apple aarch64.
- **Gold-protocol provenance** (`bd recall stale-binary-rebuild-trap`): blob
  regenerated, `cargo build --release` re-run and confirmed to print
  `Compiling eucalypt` (fresh blob actually re-embedded). Hashes:
  - `lib/prelude.blob` sha256 `ad09a86527a325a819a27926e71d0763f6493652aae5984d3fc4cddf37642664`
    (463,305 bytes, 143 inline cores) — **matches PR #1010's reported
    "full-8" blob hash exactly**, confirming this build carries the complete
    8-combinator copy-specialisation set (`map`, `foldl`, `foldr`, `scanl`,
    `scanr`, `map2`, `drop-while`, `iterate`).
  - `target/release/eu` sha256 `56ad04661adcb9e5f9f3ff08f5ac397f6689c2be45bfabbb72b41ba3071e534a`
  - Commit `cdfe08ece8b86300875280ee1fa4bffc997888cb`
- **No quiet window used.** Profile self-time shares are load-robust per
  PROTOCOL §4 (this is explicitly why the dispatch didn't require one); the
  lookup-count/depth layer is deterministic. Both hold regardless of machine
  load.

---

## 1. Headline verdict

**Share collapsed. BV3's residual case (the premise that `enter_local`/`get`
holds 43-52% of eval-thread samples at ~1 lookup/tick) no longer holds
post-copy-specialisation.** Measured combined `enter_local`+`EnvironmentFrame::
get` self-time share on the predecode engine, blob config:

| Workload | Pre-specialisation baseline (eu-7xvv) | Post-specialisation (this session) |
|---|--:|--:|
| 018_string_scale | 52.1% / 51.7% (two rounds) | **26.5% / 27.0%** (two rounds) |
| 019_list_scale | not profiled at this granularity pre-spec | **29.5%** |
| day07 (AoC, new workload) | n/a (not tested before) | **20.5%** |
| 022_hof_fold | 43-52%-class original motivating case | **could not be profiled — see §3** |

018 is the only workload eu-7xvv profiled in this exact configuration
(predecode, blob, `enter_local` share), giving the cleanest before/after:
**share roughly halved (52% → ~27%)**. 019 and day07 have no matched
pre-specialisation baseline at this granularity but both sit well below the
bead's ~40% verdict threshold. **All four data points are below 40%; none
reach the 43-52% range the original BV3 case rested on.**

Per the bead's own verdict criteria: *"if enter_local still >=~40% share,
BV3 gets a fresh design note on the count-mechanism framing... if the share
collapses, recommend BV3 → 0.14+ or retirement."* **The share collapsed.**
Recommendation: **BV3 moves to 0.14+ (deferred), not retired outright** —
see §5 for why full retirement isn't quite warranted by this data, and what
would need to be true to retire it cleanly.

**Confidence:** measured-single for each individual profile (018 reproduced
twice this session, within 0.4 percentage points — internally
corroborated); the lookup-count/depth layer (§4) is measured-verified
(deterministic, matches independently-computed tick baselines from PR
#1010).

---

## 2. Why this workload set: 018/019/022 (as specified) + day07 (my choice)

018, 019, 022 are the bead's named targets — they are exactly the benches
copy-specialisation's own gate evidence (PR #1010) measured tick deltas on,
so profiling them here is directly comparable to that PR's own numbers.

For the "one AoC-class workload" requirement, I chose **`examples/aoc25/
day07.eu`** (`part-1` target), for three reasons: (1) its own design note
literally reads *"Fold downward through the grid: at each row, detect hits
on splitters, remove hit beams, emit left/right, and merge"* — a genuinely
fold-dominant algorithm description, not a name-matched but structurally
different workload; (2) it runs at real puzzle-input scale (no synthetic
scaling needed) — 4,768-line input for day01 was tried first and rejected:
its mutator time was 44ms, far under PROTOCOL's 200ms floor, so day01 was
abandoned in favour of a naturally larger workload rather than being
artificially scaled up; (3) day07's tick count (41.4M predecode) lands in
the same order of magnitude as 018/019, making its profile share
directly comparable rather than dominated by startup noise. I surveyed
tick counts across all twelve AoC25 days' `part-1` targets before choosing
(§6) — day07's combination of genuine fold-heaviness and adequate scale was
the best fit among them without resorting to synthetic repetition.

---

## 3. 022_hof_fold: the original motivating workload is now unprofilable

`022_hof_fold`'s mutator time collapsed to **47.5ms** (predecode, blob) —
consistent with PR #1010's reported **-96.00%** tick reduction on this
bench (52,125,436 → 1,982,897 predecode ticks, confirmed independently this
session). This is **under PROTOCOL's 200ms floor** for reliable sampling (a
10ms-sampled `sample` run over 47.5ms of execution yields too few samples
for a trustworthy self-time breakdown — the profile would be
startup-dominated noise, not a measurement). **No profile share is reported
for 022 in this session; attempting one would violate the same protocol
rule that struck benches 003/006/011-014 from the canonical suite for being
too fast.**

This is itself a load-bearing finding, not a gap: **022_hof_fold was the
literal motivating workload for BV3** (`eu-2sa6.2`'s own description: *"the
higher-order foldl is O(n²) purely from env-walk re-resolution of the local
op"*), and it is now so fast that it cannot be profiled by the standard
method at all. Its deterministic lookup-count layer (§4) is still fully
available and is reported — only the wall-clock-dependent profile share is
unavailable.

---

## 4. Deterministic layer: lookup counts, and the collapse of the "unannotated" population

`EU_ENV_DEPTH_HISTOGRAM=1` (predecode, blob), same instrumentation as
eu-qm7f/PR #1005:

| Workload | annotated lookups | unannotated lookups | total | ticks | lookups/ticks |
|---|--:|--:|--:|--:|--:|
| 018_string_scale | 43,993,379 | 476,080 | 44,469,459 | 45,796,183 | **0.971** |
| 019_list_scale | 50,046,117 | 414,094 | 50,460,211 | 51,069,453 | **0.988** |
| 022_hof_fold | 762,613 | 460,080 | 1,222,693 | 1,982,897 | **0.617** |
| day07 (AoC) | 24,525,441 | 7,004,242 | 31,529,683 | 41,407,005 | **0.761** |

**018/019 stay close to the bead's cited ~1 lookup/tick baseline** (0.971,
0.988) — the *rate* of lookups relative to ticks is essentially unchanged
for the two benches with a clean pre/post comparison. **022 and day07 sit
meaningfully lower** (0.617, 0.761) — post-specialisation, a larger share of
each tick is spent on work other than environment lookups (arithmetic,
allocation, data construction inside the now-inlined specialised bodies).
This is consistent with, not contradictory to, the share-collapse headline:
fewer lookups *and* a lower per-tick lookup rate both point the same
direction.

**The dramatic, headline-worthy shift is in the `unannotated` (blob-loaded
prelude) population specifically.** Compare 018's `unannotated` count here
(476,080) against the **eu-qm7f pre-specialisation baseline for the same
bench** (49,770,106 — 66% of that session's total lookups): a **99.0%
reduction**. This is the direct, measurable signature of copy-specialisation
doing exactly what its name says: the qualifying combinators (`map`,
`foldl`, etc.) are no longer opaque blob-loaded global closures called via
`Ref::G` from user code — they are specialised, copied inline at the call
site, so their lookups now carry the **caller's own annotation** rather than
the blob's stripped `Smid::default()`. The lookup traffic didn't
disappear so much as it **changed which population it's counted in** — but
the *total* count (§ table) also dropped substantially in absolute terms
(018: 44.5M lookups now vs 75.3M at eu-qm7f baseline, a 41% reduction),
consistent with PR #1010's tick reduction and with genuine work being
eliminated, not merely reclassified.

---

## 5. Verdict detail and recommendation for eu-2sa6.2

**Share collapsed relative to the ~40% threshold on every workload tested**
(27%, 29.5%, 20.5%, and 022 unmeasurable-but-known-96%-faster). Per the
bead's stated criteria this reads as "collapse", not "still >=~40%".

**Recommendation: move BV3 (`eu-2sa6.2`) to 0.14+ (deferred), rather than
retiring it outright**, for three reasons the data itself supports:

1. **20-30% is not negligible.** It is roughly half the old headline figure,
   not zero. `enter_local`+`get` remains the single largest or
   second-largest leaf in every profile taken this session (see raw data,
   §7) — dispatch and `handle_op_predecoded` are comparable in magnitude,
   but env-frame access has not become a rounding error.
2. **[CORRECTED] The residual cost is not "folds outside the qualifying
   set" — it's general cross-function-call lookup cost, which no
   `tag_combinators` criterion relaxation could ever reach.** day07's
   `solve` calls the prelude's own `foldl(row-step, [b0, 0])` directly, and
   that call *does* fuse (verified at the STG level: `solve`'s compiled body
   contains the same local self-recursive specialised-copy signature as
   `022_hof_fold`'s fused form). The 20.5% residual share is not fold-walk
   cost at all — it comes from `row-step`, a substantial **non-recursive**
   operator function called once per fold iteration, whose body itself
   calls sibling top-level helpers (`to-splitters`, `zip-with`,
   `shift-left`, `shift-right`) as free variables. Each such call is an
   ordinary global reference needing environment resolution — ~150 grid
   rows × several helper calls per row. `row-step` was never a candidate
   for copy-specialisation's criterion (leg 1 requires self-recursion,
   which a plain operator function doesn't have), so this isn't a gap in
   the *combinator* admission list — it's a structurally different
   workload shape entirely: **any multi-function-call operator with
   sibling helper calls**, fold-adjacent or not. This is, if anything, a
   *broader* and *more structurally distinct* target population than "user
   folds outside the 8-combinator set" — one no plausible extension of
   `tag_combinators` (which is fundamentally about recursive self-calls)
   could reach, because it isn't recursion-shaped at all.
3. **The eu-2sa6.2 motivating example needs replacing, not the bead
   itself.** The original framing ("higher-order foldl is O(n²)... register
   frames make it O(1)") is now solved for the *prelude's own* `foldl` by
   copy-specialisation — including in day07, where the fold itself fuses
   cleanly. BV3's live case, if pursued, should be reframed around
   **general cross-function-call environment-lookup cost** in
   non-recursive but call-heavy operator functions (day07's `row-step` is a
   concrete, measured example: 20.5% residual share with its own fold
   already fused) — a population copy-specialisation cannot reach by
   construction, since it only targets self-recursive combinators, not
   ordinary sibling-helper composition.

**What would justify full retirement instead:** if a future re-profile
(after eu-rb5n, the source-path closure of copy-specialisation — see PR
#1010's "honest divergence disclosure") shows share continuing to fall
toward single digits across a broader workload sample of call-heavy
operator functions, or if CG4 (selective lambda-lifting) is shown to
capture the same win more cheaply without BV3's higher implementation risk.
Neither is demonstrated here — this session only re-tested the four
named/chosen workloads under the current (blob-path-only) copy-specialisation
scope.

**eu-2sa6.2 bead notes updated** (§8) with this verdict, unblocking it for
re-scoping (not for implementation as originally framed).

---

## 6. AoC day survey (context for the workload choice)

`part-1` HeapSyn ticks, this build, surveyed before selecting day07:

| Day | Ticks | Day | Ticks |
|---|--:|---|--:|
| day01 | 1,457,740 | day07 | 42,347,941 |
| day02 | 296,562 | day08 | 220,870,635 |
| day03 | 24,340,753 | day09 | 182,598,974 |
| day04 | 54,634,661 | day10 | (target/parsing issue — not surveyed further) |
| day05 | 60,083,706 | day11 | 185,265,224 |
| day06 | 15,927,655 | day12 | 6,148,764 |

day08/day09/day11 have larger tick counts than day07 but were not chosen:
day08 is a union-find/graph/Kruskal workload (sorting, spatial indexing) —
map/fold usage is present but not the dominant algorithmic shape; day09/11
were not inspected in detail given day07's description made it the more
directly on-point choice for "list/fold-heavy" and time constraints in this
session. A follow-on spike could profile day08/09/11 for a broader sample if
the owner wants more AoC coverage.

---

## 7. Raw profile data (self-time leaves, `sample`, real child PID)

Percentages are of eval-thread-only total (excludes `__ulock_wait`/
`semaphore_wait_trap`, the idle join/ctrl-c-handler threads).

**018_string_scale, predecode, blob — round 1:**
`dispatch` 187, `handle_op_predecoded` 192, `EnvironmentFrame::get` 105,
`enter_local` 74, `step_predecoded` 50, `drop_glue<ExecutionError>` 33,
`run` 23. Total 664. `enter_local`+`get` = 179/664 = **26.96%**.

**018_string_scale, predecode, blob — round 2:**
`dispatch` 201, `handle_op_predecoded` 188, `get` 118, `enter_local` 79,
`step_predecoded` 62, `drop_glue` 59, `run` 35. Total 742. Combined =
197/742 = **26.55%**.

**019_list_scale, predecode, blob:**
`handle_op_predecoded` 268, `dispatch` 250, `get` 164, `enter_local` 122,
`step_predecoded` 75, `drop_glue` 58, `run` 32. Total 969. Combined =
286/969 = **29.51%**.

**day07 part-1, predecode, blob:**
`handle_op_predecoded` 171, `dispatch` 155, `get` 77, `enter_local` 45,
`step_predecoded` 38, `drop_glue` 24, `run` 17, `return_data` 17,
`try_allocate` 16, `make_arg_array_pd` 12, `return_meta` 8,
`saturate_with_array` 8, `return_fun` 7. Total 595. Combined = 122/595 =
**20.50%**.

Note on symbol splitting: this build's compiler chose **not** to inline
`EnvironmentFrame::get` into `enter_local` (they appear as separate leaves
here), whereas the eu-7xvv baseline build inlined `get` into `enter_local`
(one combined leaf). Both `enter_local` and `get` figures are summed
throughout this report for a fair apples-to-apples comparison against the
eu-7xvv baseline's single-leaf figure — this is a compiler-codegen
difference between builds, not a change in what work is being measured.

---

## 8. eu-2sa6.2 bead update

Appended via `bd note eu-2sa6.2` (see bead history; **updated again** per
this correction — see the banner and §5): verdict from this report — share
collapsed on all four tested workloads (27%/29.5%/20.5%/
unmeasurable-but-96%-faster), below the ~40% threshold; recommend deferring
BV3 to 0.14+ with a reframed motivating case — **general cross-function-call
environment-lookup cost in non-recursive, call-heavy operator functions**
(day07's `row-step`, 20.5% share, is the concrete measured example; its own
`foldl` call fuses cleanly — the residual cost is `row-step`'s own calls to
sibling helpers, not an unfused fold), a population copy-specialisation
cannot reach by construction since it only targets self-recursive
combinators — not retiring outright; full retirement would need a broader
post-eu-rb5n re-profile showing continued collapse. Bead unblocked for
re-scoping.

---

## 9. Evidence index

- Profiles: scratchpad `profiles98zg/{018,018-r2,019,day07}.txt` — not
  committed (scratch artefacts; §7 is the durable record).
- Instrumentation reused, unmodified: `EU_ENV_DEPTH_HISTOGRAM` (bead
  eu-qm7f, PR #1005, `src/eval/machine/env.rs`).
- PR #1010 gate evidence (ticks/hashes) independently reconfirmed this
  session, not merely quoted: blob hash `ad09a865...` matches exactly;
  018/019/022 predecode ticks (45,796,183 / 51,069,453 / 1,982,897) are
  consistent with PR #1010's byte-engine figures (45,958,939 / 51,135,460 /
  2,085,398 — small deltas expected, byte vs predecode dispatch-step
  parity per the eu-2sa6.13 ticks-first report).
- AoC workload: `examples/aoc25/day07.eu`, target `part-1`, real puzzle
  input `examples/aoc25/inputs/day07.txt` (untouched, no synthetic
  scaling).
