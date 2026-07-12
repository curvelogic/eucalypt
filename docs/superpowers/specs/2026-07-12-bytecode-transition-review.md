# Bytecode Transition — Strategic Review & Course to 0.13

- **Status:** For owner approval — this document gates the 0.13 release (bead eu-2sa6.4, P0)
- **Date:** 2026-07-12
- **Owner target:** a bytecode engine that **exceeds** HeapSyn performance for all
  (or nearly all) workload types
- **Inputs:** four commissioned dimension reviews (committed alongside):
  - [A — performance-evidence audit](../reports/2026-07-12-transition-review-A-perf-evidence.md)
  - [B — engine architecture & options](../reports/2026-07-12-transition-review-B-architecture.md)
  - [C — workload taxonomy & coverage](../reports/2026-07-12-transition-review-C-workloads.md)
  - [D — strategy, criteria & process](../reports/2026-07-12-transition-review-D-strategy.md)

  plus coordinator-independent verification of the material claims (each marked
  ✔ where re-verified by hand this session).

---

## 1. Verdicts

1. **The target is reachable.** "The per-op gap is architecturally irreducible" is
   **false** (review B). The gap is not caused by BV1's actual goal (code off the
   GC heap); it is caused by a second, separable choice — executing a *re-decoded
   byte stream*. A representation that is both **pre-decoded and off the GC heap**
   gets both wins and is a coherent end-state that beats HeapSyn on effectively
   every class. The ROADMAP's "flattened-node interpreter", currently framed as a
   rejected fallback (§7 decision 1), is in fact the destination.
2. **The current claim is not supportable.** Today's honest per-class map: parity
   on dispatch; **behind ~1.15–1.8× per-tick on alloc/op-dense compute**; a win
   only where HeapSyn's GC melts down (and the flagship 52× example was partly a
   blob artefact ✔); **four workload classes never measured at all — including
   the product's core** (import→transform→export, block merge, strings at scale,
   IO). Startup is engine-neutral (~3 ms delta).
3. **The evidence base needs one clean re-measurement.** The deterministic layer
   (ticks, alloc counts, STG shape) is sound and replicated. The wall-clock layer
   every release claim is written in is not: the headline gap figure cites a
   report that **does not exist** ✔ and contains a **unit error** ("2.05" is a
   wall-seconds minimum, not a ratio ✔); ratios drift 10–35% between sessions;
   the root-cause narrative changed three times; the "count 0.66×" guardrail wins
   were never measured.
4. **The process needs two rules, not heroics.** Two levers were built on wrong
   lowering premises that a five-minute `eu dump stg` would have falsified
   (lever c, lever a/global-form). The counter-example is in-house: the W3 spike
   killed a risky migration for the cost of a report. *Premise-check before
   build* and *a fixed measurement protocol* convert the current reactive loop
   into the methodical one the owner asked for.
5. **Phase 4 (retiring HeapSyn) must be re-gated.** "Phase 4 resolves the
   residual" is rejected: **retiring the yardstick is not beating it** — the
   alloc-bound user still pays the ~1.5×; deletion only removes the measurement.
   HeapSyn is also the differential-testing **oracle**; its replacement must
   exist before deletion. Falsifiable gate criteria are in §6.

### New defect found during this review ✔
The **source-prelude runtime path misses global-form fusion**: fib =
88,853,885 ticks with a regenerated blob vs **98,277,770** without one (plain
checkout — the blob is untracked). +10.6% ticks for anyone building without a
blob, and a hidden variable that contaminated between-session comparisons
(explains the 88.9M-vs-98.3M discrepancy in the record). Bead filed (P2, under
eu-2sa6). Tick parity blob-vs-source must become an asserted invariant.

---

## 2. What we actually know (evidence audit)

Confidence classes per review A (full ledger there):

**Trust (deterministic / replicated):**
- Allocation counts are identical or bytecode-lower on every probed case
  (drop_cons 440,121 = 440,121 ✔). The gap is **not** allocation volume.
- Per-tick cost is the invariant: bytecode ~1.6–1.8× HeapSyn ns/tick,
  decomposed as **+14.5% byte-stream re-decode** (zero HeapSyn analogue) inside
  a heavier dispatch envelope, with bytecode's *productive* work already
  **cheaper** than HeapSyn's (env-build 352 ms vs 690 ms — evidence the encoding
  is the cost, not the engine concept).
- Fusion (#980+#982) cut fib ticks 115.78M → 88.85M ✔ (with blob; see defect
  above).
- The 25-leaf uniform Case-of-Case primop expansion (`3 <= 4` → 25 `LTE(` ✔).
- The blob prelude over-allocated 1.2–5.65× at equal ticks (eu-fhoo) — the
  proven mechanism behind the day08 distortion.

**Do not build on (single noisy runs / asserted / wrong):**
- The "1.2–2.05×" headline (dead citation ✔ + unit error ✔; the real extant
  worst ratio was 2.14× fib / 2.28× foldl-10k).
- The "day08 >52× bytecode win" (blob artefact; source-vs-source HeapSyn 5.1 s
  *beats* bytecode 7.6 s).
- "count 0.66× / foldl 0.89× bytecode wins" (never sourced to a report; foldl is
  O(n²) on both engines with high variance).
- Any absolute wall ratio quoted without its session (10–35% drift documented;
  ~20% swings from binary path length alone; malloc share swung 3–10× between
  sessions).

**Measurement pitfalls now catalogued** (review A §2, twelve entries P1–P12) —
these become protocol requirements in §5.

---

## 3. Architecture: why the gap exists and the end-state that removes it

Per-op, both engines share the continuation model, cactus environment, and
deferred-BIF boundary. The *only* structural difference on the hot path: HeapSyn
dereferences a materialised typed node and `match`es typed fields; bytecode
re-reads bytes (`Op::from_u8`, bounds-checked operand reads, `FormHeader`
reconstruction) and **rebuilds Case branch-table arrays on every evaluation**
where HeapSyn clones a pre-built one. Bytecode *wins* where pre-encoding paid
off (arg-array build has no per-arg atom allocation).

**End-state (review B §3):**

> Decode **once at load** into a flat, fixed-width, **typed instruction array in
> a non-GC `Vec`** — the execution IR — fed by the compact byte stream as the
> **serialisation format only** (BV5 blob/cache unchanged), with fused primops
> (shipped) and, later, BV3 register frames.

Dominance argument: HeapSyn beats bytecode today *despite paying GC on its code
nodes* (hot profiles show zero GC frames on bytecode). A pre-decoded off-heap IR
has HeapSyn's walk cost without HeapSyn's GC-on-code — at or below HeapSyn on
op-dense/alloc-dense classes, unchanged on the GC-pressure classes bytecode
already wins, startup preserved via the blob.

**Options catalogue verdict** (review B §2): pre-decode (a) is the decisive
lever; more fusion (c) is complementary (tick-count vs per-tick); denser
encoding (b), dispatch mechanics (e), operand caching (g) are dominated by (a);
gc_poll (f) is symmetric; **BV3 register frames (d) is a both-engines win, not a
parity lever** — it attacks the shared env-walk/O(n²) ceiling and belongs after
(a).

**The honest caveat:** the *magnitude* of pre-decoding's win is a reasoned
projection, not a measurement. It is settled by a **1–2 day spike** (hand-rolled
pre-decoded inner loop for drop_cons's hot opcodes behind an env flag) **before**
any commitment. That spike is the falsification experiment "irreducible" never
got.

---

## 4. Workload truth map and the canonical suite

Review C's taxonomy (12 classes) with today's status:

| Class | Status | Confidence |
|---|---|---|
| A Dispatch-dense | **parity** (day11 0.97×) | HIGH |
| B Alloc-dense compute | **bc slower 1.15–1.8×/tick, closing** | HIGH |
| C Higher-order/env-walk | shared O(n²) ceiling (BV3 territory) | MED-HIGH |
| D **Block construction & merge** | **never measured** | — |
| E Lookup (large static) | synthetic-only blow-up; absent from corpus | HIGH/moot |
| F Lookup (dynamic) | parity | MED-HIGH |
| G **String/interp at scale** | **never measured** (existing benches are <50 ms noise) | — |
| H List/stream | shared O(n²) | MED |
| I **Import→transform→export** | **never measured — the product core** | — |
| J Startup | engine-neutral (~3 ms); front-end dominated | HIGH |
| K GC-pressure | bc wins where hs GC-fails (1 data point, partly artefact) | MED |
| L **IO/subprocess** | **never measured** (expect neutral) | — |

**Action:** adopt review C §3's canonical suite — per-class benches ≥1 s,
seven new benches to author (block-merge, import→export ×2, string-at-scale,
list-at-scale, frozen lookup curve, IO loop), every bench carrying a
PASS/FAIL assertion, whole suite <15 min, results appended to a checked-in
ledger (`docs/superpowers/engine-ab/results.jsonl`) with a small
`xtask engine-ab --check` regression flagger (review C §4). Engine claims in
CHANGELOG/ROADMAP must cite ledger rows.

---

## 5. The process standard (adopted rules)

1. **Measurement protocol** (review D §4a; requirements derived from pitfalls
   P1–P12): interleaved bc/hs pairs on a quiet machine; **ticks first**, wall
   median-of-N (≥7 sub-second, ≥5 multi-second) with spread, profile-share for
   allocation claims; binaries built **at the same filesystem path**; **both**
   binaries with their own regenerated blob (and, once fixed, tick parity
   blob-vs-source asserted); nothing under ~200 ms enters ratio analysis; every
   claim cites a dated report with exact commands.
2. **Premise-check before build**: any encoder/compiler/VM lever states its
   lowering premise and **falsifies it with `eu dump` on the smallest example
   before design sign-off**; the dump goes in the design note. (Would have
   killed lever c and reshaped lever a at zero build cost.)
3. **Confidence labels** on every perf number — *measured-verified* /
   *measured-single* / *projected*. Only measured-verified numbers may gate a
   release, enter CHANGELOG/ROADMAP, or justify retirement.
4. **Decision log**: bar changes are owner decisions recorded in one place
   (append-only section in this document). The BV0 "≥2× or reassess" gate that
   was silently reinterpreted as "parity is fine" is the cautionary example.
5. **Null results are first-class**: abandoned levers get their premise error
   recorded so they are not re-attempted (#978, #981 are the good examples).

---

## 6. Phase-4 gate (HeapSyn retirement) — falsifiable criteria

Phase 4 is authorised only when **all** of the following hold, measured under §5
on the §4 canonical suite:

**Performance (per class, bc/hs):** dispatch/env-walk ≤ 1.00; startup/config
render ≤ 1.00; decode-bound compute ≤ 1.05; alloc-bound compute ≤ 1.15;
GC-pressure remains a win. At most **one** class may sit above its threshold
(and below 1.25×) with a **written waiver** naming the structural cause and the
post-retirement plan. Aggregate suite geomean ≤ 1.00 with spread reported.

**Non-performance (the real blockers today):**
1. **Oracle replacement in place and green** — HeapSyn is the differential
   correctness oracle; before deletion, promote a frozen dual-engine golden
   corpus (recorded outputs) into conformance tests, retain fuzz/property
   targets.
2. **Risk gate** — `EU_GC_VERIFY=2` + `EU_GC_POISON=1` green post-deletion; one
   release of soak with HeapSyn still reintroducible from git.
3. **Carrying-cost statement** — if keeping `EU_HEAPSYN=1` one more release
   costs little (it is CI-exercised and constrains no new work), prefer holding
   the perf bar firm over relaxing it to force deletion.

"Phase 4 resolves the residual" is **rejected** as a criterion — a tautology,
not a test.

---

## 7. The course to 0.13

**Tier 0 — gate artefacts (block all 0.13 engine work):**
1. Ratify this document (owner). Record it as the plan of record for the
   transition; open the Decision Log.
2. Land the **measurement protocol** as a checked-in doc + the **canonical
   suite** (7 new benches) + results ledger + `xtask engine-ab`.
3. **Clean-room re-baseline** on a quiet machine under the protocol: the full
   suite, source-vs-source *and* blob-vs-blob, ticks+wall+alloc, establishing
   the single baseline all 0.13 work measures against.
4. **Truth-up the record**: fix the dead citation and the 1.2–2.05× unit error
   (CHANGELOG 0.12.0 note + ROADMAP), de-list day08 as a "52× win", rewrite the
   0.12.1 CHANGELOG placeholder line honestly, revisit ROADMAP §7 decision 1
   (byte stream = serialisation format; flattened/pre-decoded IR = execution
   candidate pending the spike).
5. **Fix the source-prelude fusion-parity defect** (+ tick-parity assertion
   blob-vs-source).

**Tier 1 — engine work (each item premise-checked per §5.2):**
6. **Pre-decode spike** (1–2 days): hand-rolled pre-decoded inner loop for
   drop_cons's hot opcodes behind an env flag; measure bc/hs under the protocol.
   *This is the decision point.* If confirmed (drop_cons moves decisively toward
   ≤1.0×), lever (a) is the 0.13 centrepiece; if refuted, the residual really is
   structural and §6's waiver path applies honestly.
7. **Lever (a): pre-decoded execution IR** — decode-once at load into the flat
   typed instruction array; byte stream retained as blob format; **BV2 side
   tables folded in** (same refactor: smids move to a record field/side table).
   Byte-identical + GC gates throughout.
8. **BV3 register frames** after (a) — absolute-throughput win on the shared
   env-walk ceiling (attacks the O(n²) fold pathology); sequenced after (a) so
   the frame-model change lands on the cheap dispatch loop.
9. Additional fusion (c) only where post-(a) re-profiling shows non-uniform
   Case traffic worth it.

**Tier 2 — gated:**
10. **Phase-4 collapse** if and only if §6 passes. Otherwise HeapSyn stays
    behind `EU_HEAPSYN=1` another release, and that is stated plainly.
11. Independent 0.13 features (BV5 unit cache eu-lb0r, CG5, SV prefix-list)
    proceed in parallel once Tier 0 lands — not hostage to the engine decision.

**Parked:** W3 block index (eu-4zhi) stays deferred pending real
static-large-block evidence. eu-famg (stream O(n²)) remains engine-independent
(BV3/lazy-streams territory).

**0.12.1 disposition:** unreleased. Fold in Tier-0 item 4's CHANGELOG truth-up
(and the fusion-parity fix if quick); then cut 0.12.1 framed honestly: decode
gap substantially closed by fusion, per-tick envelope remains, plan of record
for removing it is this document.

---

## 8. Decision Log

| Date | Decision | Evidence | Owner |
|---|---|---|---|
| 2026-07-12 | This review gates 0.13 (bead eu-2sa6.4) | — | greg |
| 2026-07-12 | §1–§7 ratified as the transition plan of record; pre-decode spike authorised to run in parallel with Tier 0 | this doc + reviews A–D | greg |
