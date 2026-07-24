# BV3 (register frames) value-case re-evaluation on current master

- **Date:** 2026-07-24
- **Bead:** eu-2sa6.2 (BV3: register frames) — re-scoping spike, analysis only,
  no implementation. Phase 1 of the Stopwatch two-phase workflow.
- **Worktree/commit:** `.claude/worktrees/agent-ad384b3d2a93dcc7b`, `master` at
  `b9b34df4` ("bench: rescale hof_fold to N=400000 to clear engine-ab 200ms
  floor (eu-9vm0), #1056").
- **Toolchain:** rustc 1.97.0 (`stable-aarch64-apple-darwin`), macOS 26.5.1
  (25F80), Apple aarch64.
- **Build provenance** (per `bd recall stale-binary-rebuild-trap`): `cargo
  clean -p eucalypt --release`, `cargo run -p xtask --release --
  prelude-compile`, then a full `cargo build --release` that printed
  `Compiling eucalypt` (confirmed genuine recompile, not a cached relink).
  - `lib/prelude.blob` sha256 `de68f848184815ee88a50079ec0b98e353ced13b67692e47bf0b9cb2c944a665`
    (598,731 bytes)
  - `target/release/eu` sha256 `982c4c837382eac7d0cc64f89fa53fdda4166e9febfb672e0d8abfb6a485cce4`
  - Both engines share this one binary/one blob throughout (`EU_HEAPSYN=1`
    selects HeapSyn; default is bytecode/predecode).

## 0. Protocol-compliance disclosure — machine was NOT quiet

`uptime` read **load average 24–25** for most of this session (a shared,
heavily-loaded box — this worktree's session lists six other concurrently
active agents: main, clarion, clarion-classifier, furnace, lantern, wicket).
This is a direct violation of PROTOCOL §2's quiet-machine precondition
(load < 1). Per PROTOCOL's own instruction ("if the machine is loaded, say
so and label the numbers measured-single at best"), **every self-time
profile-share figure in this report is capped at measured-single, and in
practice the observed round-to-round spread is wide enough that I am
reporting it as a range with median, not a single trustworthy point** — see
§2. The deterministic layer (ticks, lookup counts, §1) is **not** sensitive
to load and is measured-verified.

I did not have authority to ask other agents to stop; I mitigated by taking
multiple interleaved-in-time rounds per workload (5–6 for the two most
important cells) instead of the protocol's normal single-session assumption,
and by cross-checking against the deterministic layer, which corroborates
that nothing in the engine itself has changed since the last (quieter)
baseline.

## 1. Deterministic layer: ticks and lookup counts — unchanged since eu-98zg

`env.rs` (the `EnvironmentFrame`/`enter_local` machinery) has **zero diff**
between the eu-98zg baseline commit (`cdfe08ec`) and current master
(confirmed via `git diff --stat cdfe08ec..HEAD -- src/eval`: only
`vm.rs`/`error.rs`/`bytecode/*`/`stg/*` changed, all diagnostics/Smid or
copy-specialisation-adjacent work, not the env-walk mechanism). The
deterministic counters corroborate this directly:

| Workload | Ticks (HeapSyn, `-S`) | eu-98zg baseline ticks | Δ |
|---|--:|--:|--:|
| 018_string_scale | 46,044,700 | 45,796,183 (predecode) | +0.54% |
| 019_list_scale | 51,219,477 | 51,069,453 (predecode) | +0.29% |
| day07 part-1 | 42,350,644 | 42,347,941 (AoC survey, §6 of eu-98zg) | +0.006% |
| 022_hof_fold (N=400000, rescaled by #1056) | 87,500,411 | n/a (bench rescaled since) | — |

Lookup counts (`EU_ENV_DEPTH_HISTOGRAM=1`, blob config):

| Workload | annotated | unannotated | total | eu-98zg total | Δ |
|---|--:|--:|--:|--:|--:|
| 018 | 44,063,395 | 406,064 | 44,469,459 | 44,469,459 | **0 (exact)** |
| 019 | 50,136,138 | 324,073 | 50,460,211 | 50,460,211 | **0 (exact)** |
| day07 | 25,631,763 | 5,897,920 | 31,529,683 | 31,529,683 | **0 (exact)** |
| 022 (N=400000) | 34,500,129 | 14,400,064 | 48,900,193 | n/a (rescaled) | — |

**Totals match eu-98zg exactly on all three workloads it also measured.**
Only the annotated/unannotated split shifted (more lookups now land in the
`annotated` bucket) — consistent with PR #1055 ("keep the call-site Ann on
DirectApp sites", eu-1tkk.7), which improved Smid coverage without changing
lookup volume or engine mechanics. **This is the load-bearing finding of this
section: the underlying quadratic-count-of-lookups mechanism and its total
volume are bit-for-bit unchanged since the eu-98zg re-profile.** Confidence:
**measured-verified** (deterministic, reproduced exactly).

022's lookups/ticks ratio is 0.559 (34.5M+14.4M / 87.5M), similar in kind to
eu-98zg's 022 finding (0.617) — a lower ratio than 018/019's ~0.97–0.99,
consistent with the specialised-copy body doing more non-lookup arithmetic
per tick.

## 2. Self-time profile shares — high variance under load, reported as ranges

Method: `sample <real-child-pid> 5 1ms`, PID recovered via `pgrep -P
<timeout-pid>` (never the wrapper), `--heap-limit-mib 12288`, predecode
engine, blob config. Percentages are of eval-thread-only self-time leaves
(`__ulock_wait`/`semaphore_wait_trap` — the join-wait/ctrl-c threads —
excluded), summing every leaf `sample` reports at its `>=5` threshold.
`enter_local` and `EnvironmentFrame::get` are separate un-inlined leaves in
this build (as in eu-98zg), summed for a fair comparison.

| Workload | Rounds (combined `enter_local+get` share) | Median | Range | eu-98zg figure |
|---|---|--:|--:|--:|
| 018_string_scale | 48.75%, 20.9%, 23.8%, 21.2%, 23.7%, 22.4% | **23.05%** (22.4/23.7 avg) | 20.9–48.75% | 26.5–27.0% |
| 019_list_scale | 22.9%, 24.4%, 21.1% | **22.9%** | 21.1–24.4% | 29.5% |
| day07 part-1 | 30.08%, 29.15%, 25.3%, 43.6%, 33.0%, 30.9% | **30.49%** (30.08/30.9 avg) | 25.3–43.6% | 20.5% |
| 022_hof_fold (N=400000, newly profilable) | 14.9% (1 round) | 14.9% | — | unmeasurable at eu-98zg (too fast) |

**Reading this honestly:** every workload has at least one round that lands
at or above the ~40% collapse threshold under load (018's 48.75%, day07's
43.6%), and every workload also has rounds clustering well below it. Given
§1 shows the deterministic tick/lookup layer is unchanged to within noise,
these swings are near-certainly sampling/scheduling artefacts of the
loaded shared machine, not a real engine change — `sample`'s 1 ms-interval
mach-thread-suspend approach is itself sensitive to contention for CPU, and
this session's profiled runs visibly ran 2–3× slower in wall time than their
own `-S`-measured mutator time (e.g. 018's `-S` mutator is ~1.03s but a
`sample`-profiled run's total samples imply ~3.1s of wall time at 1ms
intervals — the profiler's own overhead under contention, not new work).

**Confidence: measured-single at best, and I would not treat any individual
round (including the ones that clear 40%) as gating.** The **median across
rounds** for every workload sits **in the 15–30% band**, consistent with
"collapsed relative to the original 43–52% baseline" and in the same
ballpark as (018/019 slightly below, day07 somewhat above) the eu-98zg
figures. **None of the three required workloads shows a median anywhere
near the original 43–52% range**, and the newly-profilable 022 (the
original motivating case, now well above the 200ms floor after #1056's
rescale) sits at 14.9%, the lowest of all four — allocation/heap leaves
(`try_allocate`, `BumpBlock::bump`, `Array::push`,
`env_builder::saturate_with_array`) dominate its profile instead, consistent
with a large materialised range/list, not env-walk cost.

**day07's median (30.49%) is somewhat higher than eu-98zg's single-round
20.5%.** I looked for a source-level explanation and found none (§1 shows
ticks/lookups identical; §3 shows the fold still fuses identically) — I
attribute the difference to (a) eu-98zg reporting only one round for day07
(no reproducibility check was done there, unlike 018's two rounds), and (b)
this session's much higher load inflating variance generally (018 shows the
same order of spread, 20.9–48.75%, on a workload with an unchanged
deterministic layer). I am not confident enough in either figure to assert
a real shift; I flag it rather than picking one number to quote as the
truth.

## 3. Residual-population characterisation: still row-step, not the fold

Re-verified at the STG/inlined-core level, on current master, that the
corrected causal account from eu-98zg's Wicket-adjudicated correction still
holds exactly:

```
$ eu dump inlined -t part-1 examples/aoc25/day07.eu | grep -n 'foldl\|row-step'
138:    foldl = λ(op, i, l). IF(NILP(l), i, ... foldl(op, ..., TAIL(...)))     ; global foldl definition
223:      ...foldl(op, op(op(op(i,HEAD(l)),HEAD(TAIL(l))),HEAD(TAIL(TAIL(l)))),TAIL(TAIL(TAIL(l))))))))(row-step, [b0, 0])(rows));
```

Line 223 is `solve`'s body: a **local, self-recursive copy** of `foldl`
inlined and immediately applied to `(row-step, [b0, 0])(rows)` — the same
specialised-copy fusion signature established for `022_hof_fold`, unchanged
since eu-98zg's correction. The fold itself is not the residual cost.

`row-step`'s body (dumped alongside, `examples/aoc25/day07.eu` lines
204–213) confirms it still calls five sibling top-level helpers as free
variables once per iteration: `to-splitters`, `zip-with` (×3, for hits,
pass-through, and emission), `shift-left`, `shift-right`, plus `sum` at the
end of `solve`. This is **general cross-function-call environment-lookup
cost in a non-recursive, call-heavy operator function** — exactly the
re-scoped motivating case eu-98zg's correction established, and it is
structurally unreachable by copy-specialisation's `tag_combinators`
criterion (self-recursion only) by construction, on this or any future
version of that pass.

**No new population was found or should be claimed.** The residual is the
same one identified in July: `row-step`-shaped call-heavy, non-recursive
operator functions.

## 4. Has anything cheaper already reached this population?

- **Copy-specialisation (#1010/eu-dp0k, eu-rb5n):** targets only
  self-recursive combinators (`tag_combinators`'s leg 1). `row-step` is not
  self-recursive — provably out of scope, confirmed again in §3. Cannot
  reach this population by design, not by an accident of current scope.
- **CG4 (selective lambda-lifting / pre-projection), ROADMAP §7 Pillar CG:**
  this is the lever the ROADMAP explicitly says **"is what addresses the
  higher-order-fold O(n²)... not CG3"** and is described as feeding BV3. I
  searched the source tree for any implementation (`lambda_lift`,
  `lambda-lift`, `CG4`, `selective lifting`) and found **nothing** —
  confirmed also by `bd search` returning no beads for "CG4", "selective
  lambda", or "lambda-lifting". **CG4 does not exist in the codebase.** The
  ROADMAP index (§9) lists "CG1–4 | Type-free codegen... | 0.11" as if
  shipped, but that table entry is stale/aspirational for CG4 specifically —
  nothing under that name is implemented. This matters directly for the
  recommendation: **no cheaper lever has been tried and failed against the
  row-step population; nothing cheaper has been tried at all.**
- **eu-9tah.12 (escape analysis for block bindings):** still open,
  unscheduled, P2. Its scope is different and narrower than BV3's: it is
  about the soundness of `AtMostOnce`/update-elision for **dynamic `.key`
  lookups on blocks that escape their defining scope** (render-path
  correctness), not about register allocation for local env lookups. The
  ROADMAP's own 2026-07-13 note on that bead says explicitly: *"ROADMAP
  CG4/BV3 share escape analysis... The BV3 design note... MUST address this
  bead's scope — either subsuming it or explicitly separating it."* **That
  design note does not exist yet** (BV3 has never progressed past the
  value-case stage — this spike is precisely the step before a design note
  would be written), so **this integration question remains completely
  open**. My reading: eu-9tah.12's escape analysis (which flavour of a value
  escapes its scope, for block/lookup soundness) and BV3/CG4's escape
  analysis (which captures are hot/deep enough to warrant flattening) are
  related but not identical — both need "does this binding escape its
  defining scope", but eu-9tah.12's consumer is a **soundness** decision
  (Multi vs AtMostOnce cardinality) and BV3/CG4's consumer is a
  **performance** decision (register vs heap frame, direct-arg vs captured
  free variable). They could plausibly share the underlying flow-escape
  computation with different policies layered on top, but nobody has
  written that design, and I would not assume it without one.

## 5. Recommendation

**DEFER, re-scoped — do not retire, and do not implement BV3 now.**

**One-line:** the residual `enter_local`/`get` share on the re-scoped
motivating case (day07's `row-step`, general cross-function-call lookup
cost in a non-recursive, call-heavy operator) sits at a noisy but real
15–30%(-ish) band across every workload tested — never negligible, never
back at the original 43–52% baseline either — and **the cheaper lever the
ROADMAP itself names for exactly this population, CG4 selective
lambda-lifting, has never been attempted**, so BV3 (a new runtime frame
representation, "high value, high risk" by the bead's own words) cannot yet
be justified as the only or best way to reach it.

**Re-scoped motivating case (carried forward from eu-98zg, re-confirmed on
current master):** general cross-function-call environment-lookup cost in
non-recursive, call-heavy operator functions that call several sibling
top-level helpers as free variables per invocation — day07's `row-step` is
the concrete, measured instance (calls `to-splitters`, `zip-with`×3,
`shift-left`, `shift-right` once per grid row, ~150 rows). This population
is structurally unreachable by copy-specialisation (self-recursion-only
criterion) by construction, so it will not shrink further from that
direction.

**Rough expected-win range, if pursued:** register frames (or CG4's cheaper
lifting) attacking this population would be bounded above by the measured
share itself — roughly 20–30% of eval-thread self-time on a call-heavy,
non-recursive-operator-dominated workload like day07, with the usual caveat
that self-time share is not 1:1 with wall-time win (some of that cost would
move to argument-passing/register-shuffle cost under either approach, not
disappear). This is a real but moderate win, not a 40%+-class one.

**Recommended next step is NOT "start BV3's design."** It is: **attempt CG4
(selective lambda-lifting) first**, against exactly the `row-step`-shaped
population, as a cheaper, lower-risk, compiler-only transform (no new
runtime frame model, no interaction with the GC's frame-scanning contract)
before taking on BV3's "high value, high risk" register-frame runtime
change. If CG4 is built and shown to capture most of this win at
compiler-only cost, BV3 should very plausibly be retired outright at that
point (its remaining case would be gone). If CG4 turns out to be
insufficient (e.g. because the win genuinely requires eliminating the
env-walk indirection at the call site, not just avoiding closure capture),
*then* BV3 has a sharp, evidenced case to design against. Either way,
**whichever of CG4/BV3 is attempted first should write the shared
escape-analysis design note eu-9tah.12's own ROADMAP linkage calls for**,
settling whether the two beads' escape-analysis needs are one computation
with two policies or genuinely separate passes — that question is currently
unanswered and should not be re-litigated twice.

**What would change this recommendation:**
- A demonstrated CG4 prototype that closes most of the row-step-shaped gap
  → retire BV3.
- A clean-machine re-profile (this session's central defect) showing the
  median settling durably above ~35–40% on a broader sample of call-heavy
  operator workloads → would strengthen urgency, but the medians measured
  here (even under load) don't reach that bar.
- A broader survey beyond day07 (the AoC day survey in eu-98zg §6 already
  flagged day08/09/11 as unexplored, larger workloads) showing the
  population is bigger than one AoC day suggests.

## 6. Reproducibility

- Build: see header for exact commands and hashes.
- Bench files: `tests/harness/bench/018_string_scale.eu` (target
  `bench-string-scale`), `tests/harness/bench/019_list_scale.eu` (target
  `bench-list-scale`), `tests/harness/bench/022_hof_fold.eu` (target
  `bench-hof-fold`, N=400000 post-#1056), `examples/aoc25/day07.eu` (target
  `part-1`, real puzzle input `examples/aoc25/inputs/day07.txt`).
- Tick/lookup commands: `EU_HEAPSYN=1 eu --heap-limit-mib 12288 -S <file> -t
  <target>`; `EU_ENV_DEPTH_HISTOGRAM=1 eu --heap-limit-mib 12288 <file> -t
  <target>` (both under `timeout 60`).
- Profile capture script (used for every round in §2):
  `/private/tmp/claude-501/-Users-greg-dev-curvelogic-eucalypt/14419bff-1ff5-483c-805e-d935feddcda6/scratchpad/profile_one.sh`
  — launches `timeout 60 eu --heap-limit-mib 12288 <args>` in the
  background, polls `pgrep -P <timeout-pid>` for the real child, then runs
  `sample <child-pid> 5 1ms`. Raw `.sample.txt` outputs for all 18 rounds
  (six 018, three 019, six day07, one 022) are alongside it in
  `scratchpad/profiles/` (scratch artefacts, not committed — this report's
  §2 table is the durable record, per the eu-98zg precedent).
- Fusion check: `eu dump inlined -t part-1 examples/aoc25/day07.eu` (no
  `--heap-limit-mib` — `dump` is a separate subcommand from `run`/default
  evaluation and doesn't take that flag).
