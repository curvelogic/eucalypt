# Review D — Strategy, Decision-Criteria & Process Coherence

**Dimension D of the 0.13 gate review (bead eu-2sa6.4).** Scope: the decision
history of the bytecode transition, how the acceptance bar drifted, a
falsifiable Phase-4 gate, a benchmark/process standard, and the shape of 0.13.
Read-only pass over ROADMAP.md, the 0.12.1 spec/plan/reports, PRs #975–985, and
the bead trail (eu-cj1h*, eu-9mvh*, eu-adnu, eu-4zhi, eu-oufc, eu-2sa6, eu-1tkk).

---

## 0. Executive summary (the 10 lines)

1. The transition is directionally sound and honestly documented — but it is
   being steered by **reactive point-decisions**, which is exactly what the owner
   flagged (eu-2sa6.4).
2. Two levers (c: `Op::Seq`; a: global-form `FusedPrimop`) were **built before a
   cheap `eu dump` check that would have predicted their failure** — a repeatable,
   avoidable waste (PR #978, PR #980).
3. The counter-example is W3 (eu-4zhi): a **spike killed a large risky migration
   before building it** (PR #985). That discipline should be the default, not the
   exception.
4. The acceptance bar has drifted from BV0's "**≥2× win**" → "parity accepted" →
   "**≤1.25× / none >1.3×**" → "decode-bar met, **alloc residual accepted**" →
   "**Phase 4 resolves it by removing the baseline.**"
5. That last move is the central intellectual-honesty problem: **retiring the
   yardstick is not the same as beating it.** Deleting HeapSyn makes the ratio
   *undefined*, not favourable, and hides a real ~1.5× alloc-bound regression from
   the users who hit it.
6. Measurement quality is the silent blocker: **28% session tick-drift, 20%
   code-layout wall sensitivity, sub-200 ms noise, blob-fairness errors** — a
   per-case 1.25-vs-1.30 wall bar is barely resolvable against this.
7. Fusion (Option C, PR #982) genuinely closed the **decode** gap (fib ~1.96× →
   ~1.265× bc/hs, −23% ticks); the malloc lever (PR #984) took a partial
   **alloc** bite. Both real, both verified. The residual is structural decode
   tax relative to a zero-decode baseline.
8. **Phase-4 gate must be reframed** from "the gap is small enough / retirement
   hides it" to falsifiable, per-workload-class thresholds measured under a fixed
   protocol, *plus* a differential-oracle replacement plan (HeapSyn is today's
   correctness oracle — that cost is not yet priced in).
9. **0.13 should lead with the process artefacts** (a canonical benchmark suite +
   protocol, a workload taxonomy) and the highest-leverage engine item (**BV3
   register frames**, which attacks env-walk *and* the higher-order-fold O(n²)),
   not with the Phase-4 deletion.
10. Do **not** retire HeapSyn in 0.13 on current evidence. Gate it on a passed
    protocol + an oracle-replacement plan, or keep it behind `EU_HEAPSYN=1` one
    more release. (Sources throughout.)

---

## 1. Decision-history reconstruction

A timeline of the material decisions, each with the evidence it rested on, whether
that evidence held up, and what it cost or saved.

### D0 — The 0.10.0 reset: GC line retired, "win in compilation" thesis adopted
- **Evidence:** the prior plan assumed *mark > 95% of VM time* (0.7.1 baseline).
  Strict eager eval (0.10.0) removed the recurring collections; AoC-2025 went to
  **0 GC collections**; dispatch (60–81%) and env-walk (10–39%) became dominant
  (ROADMAP §2, §10.5).
- **Sound in hindsight?** Yes — and unusually well-evidenced. Three bets
  (generational GC, flat closures, update-elision) were built, measured, and
  reverted with post-mortems (§10.1–10.3). This is the *good* pattern: measure,
  kill, record.
- **Cost/saved:** cost = substantial engineering on three dead runtime bets.
  Saved = a decade of misdirected GC work; the reverts are documented so they are
  not repeated. **The lesson the roadmap draws — "the bytes we generate are the
  bottleneck" — is correct.**

### D1 — BV0 gate (≥2×) → BV1 default-on at parity
- **Evidence / criterion:** BV0 was defined as a *gate*: "**Go if ≥~2×**;
  reassess otherwise" (ROADMAP §6.4 item 1, §7 "BV0"). The success line for BV1
  was "day11-p1 dispatch improves by the BV0-measured factor."
- **What happened:** BV1 landed at **parity (0.93–0.97×), not a multiple**
  (ROADMAP §7 "Success", parenthetical; 2026-07-03 A/B §2a: day11-p1 bc/hs 0.97×).
  The leaner dispatch was offset by instruction-stream decode (~40% CPU on
  call-dense code) and per-instruction `ExecutionError` drop.
- **Sound in hindsight?** The *decision to ship BV1 default-on* is defensible on
  its other merits (code off the scanned heap; startup win; serialisability). But
  the **explicit ≥2× gate was not met and the bar was silently reinterpreted**
  from "≥2× or reassess" to "parity is fine because of the other wins." This is
  the first, and formative, criteria slip (§2 below). It was never recorded *as*
  a gate miss — the roadmap narrates it as a realised success with a caveat.
- **Cost/saved:** shipping at parity was probably right, but doing so without
  re-ratifying the gate set the pattern of "the bar moves to fit the result."

### D2 — The 1.25× / 1.3× bar (0.12.1)
- **Evidence:** the 2026-07-03 A/B measured bc **1.2–2.05× slower** on
  alloc+call-dense (report §0). The 0.12.1 delivery spec set: **every regression
  case ≤1.25×, none >1.3×** (spec §2).
- **Rationale given:** "full parity is likely unreachable because decode is
  structural to any bytecode VM; 1.1× was judged too tight for the most
  alloc-dense case" (spec §2 Rationale).
- **Sound?** As an *engineering* target, reasonable. As *strategy*, it is a
  quiet retreat: BV0 aspired to a **win**; 0.12.1 codifies "**get within 25% of
  HeapSyn**." Nothing in the spec reconciles this with the owner's "**exceed
  HeapSyn on all workloads**" target — the gap between "close" and "beat" is never
  named.
- **Cost/saved:** gave the 0.12.1 work a concrete finish line (good). Baked in
  "trailing is acceptable" as the working definition (the strategic debt).

### D3 — Lever (c): built, then inert (PR #978, CLOSED unmerged)
- **Evidence it rested on:** W0 spike (PR #976) recommended lever (c) —
  encode-time `uniform-branch Case → Op::Seq` — as "low-risk, targets the largest
  cost bucket." Owner signed off (eu-9mvh notes, 2026-07-09).
- **What happened:** sound but **inert**. `Op::Seq` forces for effect and does
  *not bind* the forced value; the profiled primop `Case`-of-`Case` chains exist
  precisely to *bind* the unboxed operand, so a sound rule must always decline on
  them (PR #978 body; fused-primop design §1). Zero downgrades fired on
  `binary_wrapper`.
- **Was the evidence sound?** No. The W0 spike profiled the pattern in depth but
  **did not test the one property that determines fusibility — does the shared
  body read the bound value?** For operand-forcing, the answer is trivially "yes,
  always," and that is checkable by reading `binary_wrapper` (arith.rs:452) or an
  `eu dump stg` *before writing an encoder pass*.
- **Cost/saved:** a full implement-measure-revert cycle for a null result. The
  branch is preserved; the finding ("Op::Seq can't bind") is now load-bearing for
  lever (a)'s design — so not *total* waste, but a cheap check would have reached
  the same conclusion at a fraction of the cost.

### D4 — Lever (a): design-note-first, built, inert on direct arithmetic (PR #979 → #980)
- **Evidence:** after (c), the owner required a **design note before code**
  (good corrective). The note (2026-07-10) proposed a generic `Op::FusedPrimop`
  intercepted at the **global-form** boundary, on the premise (§4) that `n <= 1`
  compiles to `App(Lte.gref, [n,1])` **entering the global form**.
- **What happened:** #980 shipped correct and GC-safe but **flat on fib
  (0.98×)** — because the STG compiler **inlines the wrapper body at direct call
  sites** (compiler.rs:1187), so direct arithmetic never enters the global form;
  the fused op only fired on first-class operator uses (`foldl(+, …)`), ~7% where
  it fired (PR #980 "Bellwether is flat").
- **Was the evidence sound?** No — again a **wrong lowering premise**. The note's
  central §4 assumption was falsifiable by one `eu dump stg` of `{x: 3 <= 4}` (25
  inlined `LTE(` leaves — exactly the dump later run in PR #981 to *prove Option A
  active*). The design note discussed the dump for the *global-form body* but
  never checked the *direct call site*.
- **Cost/saved:** the design-note gate caught a *different* class of bug (a
  miswired dispatch path, corrected in the note itself), which is a genuine win.
  But the same note shipped a wrong lowering premise a dump would have exposed.
  **Two levers, two wrong lowering premises, both catchable pre-build.**

### D5 — Option A vs Option C (PR #981 experiment CLOSED, PR #982 Option C MERGED)
- **Evidence:** owner directed measuring **Option A** (suppress inlining → route
  direct arith through the global fused form) purely as a *measurement*, against
  **Option C** (a `StgSyn::FusedPrimop` marker inlined directly, no App+enter).
- **What happened:** A hit the decode bar on fib (1.218× bc/hs, −19.9% wall) but
  **regressed protected cases** via App+enter overhead (drop_cons +14.2%,
  day07-p2 +6.4%). C matched the fib win (88.85M ticks, −33.6% wall, ~1.265×
  bc/hs) **without** the regressions. C merged (#982); A closed as an experiment.
- **Sound?** Yes — this is the process working. A/C were compared on evidence
  before committing, and the ship candidate was chosen on merit. The only critique
  is that A+C together are two more build-measure rounds that a sharper up-front
  model (App+enter *will* cost on alloc-light cases) could have collapsed to one.

### D6 — Malloc lever (spike PR #983 → impl PR #984)
- **Evidence:** with the decode gap closed on fib, alloc-bound cases stayed ~1.5×
  bc/hs. Spike (#983) found the gap is **not allocation count** (bc=hs identical,
  440,121=440,121) but **per-op wall cost: +14.5% architectural re-decode (zero
  on HeapSyn) + 7% malloc/free** (partially recoverable).
- **What happened:** #984 took `read_let`/`materialize_bif_args` off the C heap
  (managed-array read / SmallVec / pooled free-list); malloc share 4.6% → 0.2%;
  drop_cons ~0.92 wall. **Partial, explicitly not gap-closing.**
- **Sound?** Yes, and honestly scoped ("Partial win as projected — the residual
  is the fusion-immune decode envelope, architectural, out of scope"). Caught a
  real GC-UAF hazard (BifFrame.args) in passing.

### D7 — W3 block index: deferred/dropped on evidence (spike PR #985)
- **Evidence:** eu-4zhi is a large, GC-risky SynClosure-ABI migration. Spike
  (#985) quantified real upside: **~0 on the actual corpus** — large hot-lookup
  blocks are dynamic (index-immune), day11 already at parity, the static-large-
  block saturated-lookup pattern is absent. Synthetic upside (87×) only where the
  pattern never occurs.
- **What happened:** **Deferred (leaning drop)** out of 0.12.1, reparented to
  0.13's epic. No code written.
- **Sound?** **This is the model.** A cheap report killed an expensive, risky
  build. Contrast D3/D4, where the build came first.

### D8 — "Accept residual / Phase 4 resolves it"
- **The reframe:** eu-9mvh close: "DECODE gap closed … Allocation-bound residual
  is a SEPARATE gap." eu-cj1h.2 close: "do the malloc lever + **accept structural
  residual (Phase 4/0.13 resolves)**." The logic: the residual bc/hs ratio is the
  decode tax *relative to HeapSyn*; retire HeapSyn (Phase 4) and there is no
  baseline to trail.
- **Sound?** This is where strategy and honesty part company — see §2.

### The two honest lessons
1. **Two levers were built on wrong lowering premises that a `eu dump stg` would
   have falsified in minutes** (D3, D4). The project *has* the tool (CLAUDE.md
   mandates it for compiler investigation) and *used it after the fact* to explain
   both failures. The missing discipline is a mandatory **pre-build premise check**.
2. **The good pattern already exists in-house** (D0 post-mortems, D5 A/C
   measurement, D7 evidence-spike) — it is applied inconsistently. The fix is to
   make "spike/dump before build" the default gate, not an owner-remembered
   intervention after a null result.

---

## 2. Criteria-drift analysis

The acceptance bar for "bytecode is good enough" has moved five times, each
step locally reasonable, the cumulative drift away from the owner's target:

| Stage | Bar | Source |
|---|---|---|
| BV0 gate | dispatch **≥ ~2× win** or reassess | ROADMAP §6.4/§7 |
| BV1 shipped | **parity accepted** (0.93–0.97×) as success | ROADMAP §7 "Success" |
| 0.12.1 spec | **≤1.25× / none >1.3×** per regression case | spec §2 |
| W0 re-baseline | ratios drift **10–35% run-over-run**; direction only | decode spike §1.3 |
| decode/alloc split | **decode bar met (fib ~1.265×); alloc residual accepted** | eu-9mvh close |
| Phase-4 framing | **"removing the baseline resolves it"** | eu-cj1h.2 close, eu-oufc |

**Three things are wrong with the endpoint:**

1. **Retiring the yardstick ≠ beating it.** The owner's target (eu-2sa6.4) is
   "bytecode **exceeding** HeapSyn for all/nearly all workload types." Deleting
   HeapSyn does not make bytecode faster on drop_cons-class workloads; it makes
   the *comparison* impossible. A user whose workload is alloc-bound still pays
   the ~1.5× — they simply lose the ability to run `EU_HEAPSYN=1` and see it. "The
   residual is only visible because HeapSyn exists" is true and **irrelevant to
   the user's wall-clock**. Naming this precisely: *the residual is a property of
   bytecode's absolute performance, measured against HeapSyn but not caused by
   it.* Phase 4 removes the measurement, not the cost.

2. **The per-case wall bar is finer than the measurement can resolve.** The
   record documents **28% session-to-session tick drift** (PR #982), **20% wall
   swing from code layout alone** (PR #984), **sub-200 ms noise** on 005/007
   (PR #978), and a **blob-fairness error** that inflated master's numbers until
   caught (PR #981). A "1.25× pass / 1.30× fail" line cannot be adjudicated
   honestly against ±20–28% noise. The bar's *form* (per-case wall ratio) is
   mismatched to the instrument.

3. **"All workloads" was quietly replaced by "the regression set."** The five
   regression cases were chosen because they were the *worst* bc/hs offenders in
   one A/B. They are not a workload taxonomy. Passing them is not evidence about
   "all/nearly all workload types" — it is evidence about five hand-picked points,
   two of which (fib decode-bound, drop_cons alloc-bound) turned out to measure
   *different* phenomena that were conflated under one bar (eu-9mvh close: "the
   regression set conflated decode-bound and alloc-bound gaps").

**Is the current framing intellectually honest w.r.t. the target?** Partly. The
*reports* are scrupulously honest (every one flags its own noise, declines to
over-claim, distinguishes decode from alloc). The **strategic narrative built on
top of them is not**: "decode gap closed, alloc residual accepted, Phase 4
resolves it" reads as *done* while the owner's actual target (exceed HeapSyn) is
*unmet on an entire workload class* and the plan's answer is to stop measuring it.
The tension to state to the owner in one sentence: **you can retire HeapSyn, or
you can beat it — but the current plan does the first and describes it as the
second.**

---

## 3. Phase-4 gate proposal (falsifiable, serves "exceed HeapSyn")

Replace "the gap is small / retirement hides it" with concrete, measurable
criteria. HeapSyn retirement (eu-oufc) is authorised **only when all of the
following hold**, measured under the §4 protocol:

### 3a. Performance criteria — per **workload class**, not per cherry-picked case
Requires the §5 taxonomy first. Proposed classes and thresholds (bc/hs, lower =
bytecode faster):

| Workload class | Threshold | Rationale |
|---|---|---|
| Pure dispatch / env-walk (day11-p1, higher-order count/foldl) | **≤ 1.00×** (already met: 0.66–0.97×) | bytecode's home turf; must stay a win |
| Startup / config render (the centre of gravity) | **≤ 1.00×** | BV5 already delivers ~10 ms; this is where most users live |
| Decode-bound compute (fib-class) | **≤ 1.05×** | fusion took it to ~1.265×; needs one more lever OR explicit waiver |
| Alloc-bound compute (drop_cons/short_lived-class) | **≤ 1.15×** | malloc lever took the recoverable part; the rest needs a decode-envelope attack, not a waiver |
| Extreme allocation (day08-class) | **already ≫1× win** (~52×) | HeapSyn is GC-bound; non-issue |

- **"Nearly all" tolerance:** at most **one** named class may sit in
  `(threshold, 1.25×]` **and** only with a written waiver naming the structural
  cause and the post-retirement plan to close it. No class above **1.25×**.
- **Aggregate:** AoC-2025 + harness-bench **geomean ≤ 1.00×** (bytecode wins on
  balance), reported with confidence interval, not a point estimate.
- **No protected-win regression** beyond measurement noise, per the §4 protocol's
  own resolution.

### 3b. Non-performance criteria (currently unpriced — the real blockers)
1. **Differential-oracle replacement.** HeapSyn is today's correctness oracle —
   the entire BV1 validation strategy is "byte-identical on both engines" (spec
   §Global; every VM PR gates on it). **Deleting HeapSyn deletes the oracle.**
   The gate must name what replaces it: e.g. a frozen golden corpus with
   pre-recorded outputs promoted into W5 conformance, plus retained
   property/fuzz targets. *No retirement until the replacement oracle is in place
   and green.* This is arguably a harder blocker than the perf gap.
2. **Maintenance-cost accounting.** State the actual carrying cost of keeping
   HeapSyn behind `EU_HEAPSYN=1` (it is a flag + a code path already exercised by
   CI). If the cost is "it constrains no new work," the urgency to delete is low
   and the perf bar can be held firm rather than relaxed to force the deletion.
3. **Risk gate.** Retirement touches `GcScannable for HeapSyn`, the loader, and
   the dispatch. Require `EU_GC_VERIFY=2` + `EU_GC_POISON=1` green post-deletion
   and a one-release soak with the flag still *reintroducible* from git.

**Falsifiability:** each criterion is a measured number or a binary artefact
(oracle exists / doesn't). "Phase 4 resolves it" is explicitly **rejected** as a
criterion — it is a tautology, not a test.

---

## 4. Process standard proposal

### 4a. Written benchmark protocol (make it a checked-in doc, referenced by every perf bead)
The record already contains every ingredient; formalise it so it is not
re-derived per spike:
- **Quiet machine, foreground, interleaved pairs.** bc and hs runs interleaved
  (not batched) to cancel session drift; foreground; no concurrent load. (The
  28% session drift in PR #982 and 20% layout swing in PR #984 are why.)
- **Tick-first, wall-second.** VM **ticks** are the machine-independent primary
  metric for decode/dispatch work (fib 115.78M→88.85M is credible where the
  −33.6% wall is noisy). **Wall** is secondary, reported as median-of-N with
  spread, never a bare point. **Profile-share** (`sample` leaf self-time) is the
  reliable metric for alloc/malloc claims (PR #984's malloc-share is trusted where
  its wall is not).
- **Layout controls.** Build compared binaries **at the same filesystem path**
  (embedded-manifest-path length changes layout → ±20%, PR #984). Ideally pin with
  a layout-randomisation sweep or accept only tick/profile deltas for sub-second
  cases.
- **Fair-blob builds.** Both binaries built with their **own freshly regenerated
  prelude blob** (`cargo xtask prelude-compile`) — a master build without a blob
  silently runs source-prelude and inflates its numbers (caught in PR #981).
- **Minimum durations & N.** Exclude anything under ~200 ms from ratio analysis
  (startup/noise-dominated); N ≥ 7 medians for sub-second, ≥ 5 for multi-second;
  report the spread.
- **Where results live.** Every perf claim → a dated report under
  `docs/superpowers/reports/` with the exact commands; bead close reasons cite the
  report, not a bare ratio.

### 4b. Investigate-before-build rule (formalise the missing gate)
The lesson of D3/D4. Before any encoder/compiler/VM lever that rests on *how code
lowers*, a **premise check** is mandatory and cheap:
- State the lever's lowering premise explicitly ("`n <= 1` enters the LTE global
  form").
- **Falsify it with `eu dump stg` / `eu dump cooked` on the smallest example**
  before writing the pass. Attach the dump to the design note.
- Only then design/build. (This is a stricter form of the design-note-first gate
  the owner already imposed after D3 — it adds "and the note must contain the dump
  that proves the premise," which #979 did not.)
- This gate would have killed lever (c) and reshaped lever (a) at zero build cost.

### 4c. How perf claims enter CHANGELOG/roadmap — confidence labels
Every perf number carries a **confidence label**:
- **Measured-verified** (independently reproduced on a clean build under 4a) —
  the only kind allowed to gate a release or justify retirement.
- **Measured-single** (one run, one machine) — allowed in a spike, never in a
  gate.
- **Projected** (a spike's upside estimate) — never stated as achieved.
CHANGELOG/ROADMAP perf claims must be Measured-verified and cite the report.

### 4d. Decision-record hygiene
- **Bar changes are owner decisions, recorded in one place.** Today the bar's
  evolution is scattered across a spec, five bead close-reasons, and two report
  footnotes. Add a single **Decision Log** (a bead or an appended section of the
  strategic-review doc) with one row per bar change: date, from→to, evidence,
  owner sign-off. The BV0-gate reinterpretation (D1) is the cautionary example —
  it was never recorded *as* a change.
- **Null results are first-class.** PR #978 (inert) and PR #981 (rejected
  experiment) are valuable and correctly preserved; the Decision Log should record
  *why each was abandoned* so the premise error is not re-attempted.

---

## 5. 0.13 shape recommendation

**Principle: 0.13 leads with the artefacts that stop the reactive cycle, and the
one engine item with the highest structural leverage — not with the Phase-4
deletion.** 0.13 is already correctly *gated* on this review (eu-2sa6 note; the
P0 bead eu-2sa6.4). Given all the above, here is the order.

**Tier 0 — process artefacts (block everything; this review's output):**
1. Land the **benchmark protocol doc** (4a) and the **workload taxonomy + canonical
   suite** (dimension C's job; the enabler for a real Phase-4 gate). Without the
   taxonomy, §3a's per-class thresholds cannot be evaluated and the gate stays
   subjective.
2. Adopt the **Phase-4 gate criteria** (§3) and the **Decision Log** (4d) as the
   ratified decision framework the owner asked for.

**Tier 1 — engine-transition completion (the real work of "exceed HeapSyn"):**
3. **BV3 register frames + CG4 selective lifting** (eu-2sa6.2, P1). This is the
   highest-leverage item in the whole plan: it attacks `EnvironmentFrame::get`
   (10–39% of AoC profiles) *and* is the specified fix for the higher-order-fold
   **O(n²)** (ROADMAP §6.4 item 4, §7). It is on bytecode's *winning* side of the
   ledger (env-walk), so it widens the lead rather than chasing the alloc
   residual. **Precede it with the 4b premise check** — the env-walk lowering is
   exactly the kind of premise that has burned this project twice.
4. **BV2 side tables** (eu-2sa6.1) — additive, low-risk, retires the `Ann`
   dispatch step; good parallel track.
5. **One more decode-envelope lever OR an explicit waiver** for the fib/alloc
   classes if §3a thresholds aren't met by BV3's ride-along effects. Only after a
   4b-gated spike says a lever exists — do **not** open-endedly chase the residual.

**Tier 2 — gated on Tier 0+1:**
6. **Phase-4 collapse (eu-oufc)** — *only if* §3 is satisfied, including the
   **oracle-replacement** (promote the dual-engine golden corpus into W5 first).
   If the perf gate is met but the oracle isn't ready, **keep HeapSyn behind
   `EU_HEAPSYN=1` for one more release** — the flag's carrying cost is low and the
   differential oracle is worth more than the deletion.
7. **BV5-cache** (eu-lb0r) and **CG5 type-gated** (eu-u9xj.5) and **prefix-list**
   (eu-2sa6.3) are genuine 0.13 features but are **independent of the transition**
   — they can proceed in parallel once Tier 0 lands, and should not be held hostage
   to the Phase-4 decision.

**W3 block index (eu-4zhi):** keep deferred (D7) — reparent-only, no build, until
real static-large-block hot-lookup evidence appears. It is correctly parked.

**The one-line 0.13 thesis:** *finish widening bytecode's win where it already
wins (BV3/BV2), price the residual honestly against a real taxonomy, and retire
HeapSyn only when a fixed protocol says bytecode leads on nearly every class AND a
replacement correctness oracle is green — not because deleting the baseline makes
the number disappear.*

---

## Sources
ROADMAP.md §2, §4.5, §6.3–6.4, §7 (BV/CG pillars, "Success" parentheticals),
§10.1–10.5. `docs/superpowers/specs/2026-07-09-0.12.1-delivery-design.md` §2
(the bar + rationale), §4. `docs/superpowers/plans/2026-07-09-0.12.1-close-the-
engine-gap.md`. `docs/superpowers/specs/2026-07-10-fused-primop-superinstruction-
design.md` §1, §4 (the false lowering premise). Reports: `2026-07-03-bytecode-vs-
heapsyn-ab.md` §0–2; `2026-07-09-bytecode-decode-cost-spike.md` §1.3, §4;
`2026-07-12-allocation-gap-spike.md`; `2026-07-12-w3-lookup-evidence-spike.md`.
PRs #975–985 (bodies). Beads eu-cj1h(.1–.4), eu-9mvh(.1), eu-adnu, eu-4zhi,
eu-oufc, eu-2sa6, eu-2sa6.4, eu-1tkk, eu-lb0r.
