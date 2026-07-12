# Review A — Performance-Evidence Audit (bytecode-vs-HeapSyn transition, gating 0.13)

**Author:** Review dimension A (performance evidence). **Date:** 2026-07-12.
**Scope:** audit the *reliability* of the benchmark evidence base behind every
consequential 0.12/0.12.1 engine decision. Read-only; no repo changes.
**Sources:** 5 perf reports in `docs/superpowers/reports/`, `CHANGELOG.md`
(0.10.0→0.12.1), `ROADMAP.md` §4.5/§6.4/§10, the 0.12.1 delivery spec, PR bodies
(#955–#985), and beads eu-9mvh/eu-adnu/eu-4zhi/eu-cj1h/eu-w2oy/eu-mhjz/eu-vi3a et al.

---

## Executive summary (≤10 lines)

1. The evidence base is **honestly self-documented but fragile**: every wall-clock
   number comes from one shared, loaded macOS laptop, and the reports say so.
2. **Tick counts and allocation counts are the only trustworthy layer** — they are
   deterministic, replicated across reports, and they carry the load-bearing findings
   (identical alloc counts bc=hs; fib −23% ticks from fusion).
3. The **headline "1.2–2.05×" bar that gates all of 0.12.1 is unsound**: it cites a
   report (`…-ab-rerun.md`) that **does not exist** in the repo or git history, and
   "2.05" is a wall-*seconds* value ("min 2.05") mis-transcribed as a *ratio*.
4. The **regression-set targets (fib 2.14×, drop_cons 2.13×, …) do not reproduce** —
   the 07-09 re-measure got 1.96/1.71/1.20/1.47/1.21 on the same code, a uniform
   10–35% lower. The 1.25× acceptance bar is set against numbers that drift.
5. The **root-cause story changed three times** (malloc-churn 19–21% → decode 46–65%,
   malloc 2–6% → decode 14.5% + malloc 7%), yet the shipped CHANGELOG bakes in only the
   final "decode cost" framing.
6. The **"day08 52× bytecode win" is a discredited blob artefact** (companion report:
   source-prelude HeapSyn 5.1s *beats* bytecode 7.6s), yet the delivery spec still
   repeats it as a bytecode dispatch win.
7. **Solid:** alloc-count parity, fib fusion tick reduction, day11 lookup parity,
   W3-index-is-worthless-on-real-code, the drop_cons per-tick 1.8× architecture tax.
8. **Shaky:** every absolute wall ratio, the "count 0.66×"/"foldl 0.89×" wins (asserted,
   no source report), the malloc-share figures (swung 3–10× between sessions).
9. **Before any 0.13 go/no-go:** re-run the whole A/B **source-vs-source**, interleaved,
   on a quiesced machine, reporting ticks + ns/tick + alloc counts, not bare wall.
10. Net: the *direction* (bytecode trails on alloc/call-dense, wins on GC-pressure) is
    robust; the *magnitudes* the 0.13 decision would rest on are not yet measured cleanly.

---

## 1. Claims ledger

Legend — **Method:** WALL = external wall-clock; TICK = deterministic VM tick count;
ALLOC = deterministic allocation count; PROF = `sample` CPU profile share.
**Verified:** independently re-measured in ≥2 sessions/reports (Y) or single run (N).
**Conf:** HIGH (deterministic/replicated) · MED (noisy but consistent direction) · LOW
(single noisy run / asserted / contradicted).

### 1a. Deterministic claims (tick / alloc counts — trustworthy)

| # | Claim | Source | Method | Verified | Conf |
|--|--|--|--|--|--|
| D1 | bytecode never allocates more than HeapSyn; alloc counts identical or bc-lower on all 6 probed cases (drop_cons 440,094=440,094; day03 bc 2.46M < hs 2.48M; fib 18.85M=18.85M) | alloc-gap report §2 | ALLOC | Y (cross-checked vs 07-03 tick table) | HIGH |
| D2 | fib direct-arith fusion: −23% VM ticks (115.8M→88.9M) | CHANGELOG:11 (Option C / #982) | TICK | partial | HIGH (but see P8: alloc-gap report shows 98.3M post-#982, not 88.9M — deterministic metric disagrees) |
| D3 | blob prelude allocates 1.20–5.65× more than source at ~equal ticks (day08 test path 82k→466k allocs, +8% ticks) | 0.11→0.12 regression report §"Mechanism" | ALLOC+TICK | Y (same-binary EU_SOURCE_PRELUDE proof) | HIGH |
| D4 | per-binop STG is a 25-leaf uniform-branch Case-of-Case; `grep -c "LTE(" = 25`, `SUB( = 50` for fib | decode-cost report §3.1 | static (grep on `eu dump stg`) | Y (reproducible command given) | HIGH |
| D5 | range foldl O(n²) on BOTH engines (ticks ≈3.8× for 2× N); engine-independent | 07-03 A/B §3 | TICK | Y | HIGH |
| D6 | ExecutionError boxed 128→40 bytes (68.75%) | CHANGELOG:12 (eu-adnu) | static size_of | Y (compile-time fact) | HIGH |
| D7 | per-tick cost: bc ~30.6 ns/tick vs hs ~17.0 ns/tick on drop_cons (bc 1.8×/tick, universal 1.6–1.8×) | alloc-gap report §2 | WALL÷TICK | N (single session, wall-derived) | MED |

### 1b. Wall-clock ratio claims (noisy — the load-bearing 0.12.1/0.13 numbers)

| # | Claim | Source | Method | Verified | Conf |
|--|--|--|--|--|--|
| W1 | Regression set: fib 2.14×, drop_cons 2.13×, short_lived 1.87×, day03-p2 1.67×, day09-p1 1.36× | 07-03 A/B §3; delivery-spec §2 acceptance bar | WALL 5-run median | **contradicted by W2** | LOW |
| W2 | Same cases re-measured: fib 1.96×, drop_cons 1.71×, short_lived 1.20×, day03-p2 1.47×, day09-p1 1.21× (uniform 10–35% lower, no code change) | decode-cost report §1.3 | WALL 7-run median | vs W1 (disagree) | LOW (both) |
| W3 | Post-fusion fib 1.275× (was ~1.96–2.14×) | alloc-gap report §2 | WALL 9-run interleaved median (VM-mutator) | N (single session) | MED |
| W4 | drop_cons post-#955/#982 1.765× (was 2.13×) | alloc-gap report §2 | WALL 9-run | N | MED |
| W5 | day11-p1 lookup parity: bc 1.50–1.60 vs hs 1.47–1.55 | W3-lookup report §4b | WALL 2 reps | Y (matches 07-03's 0.97×) | MED-HIGH |
| W6 | "count 0.66× bytecode win", "foldl 0.89×" | delivery-spec §1/§2 lines 17,45 | — | **no source report cited anywhere** | LOW |
| W7 | day08-p1 "52× bytecode win" (bc 9.6s vs hs >500s TO) | 07-03 A/B §2d,§3 | WALL single | **discredited** — blob artefact (see W8) | LOW |
| W8 | day08 with source prelude: HeapSyn 5.1s *beats* bytecode 7.6s (blob 9.9s) | regression report §"Impact" | WALL 5-run | Y (same-binary) | MED-HIGH |
| W9 | headline bar "bytecode 1.2–2.05× slower than HeapSyn" | CHANGELOG:21, ROADMAP:484, delivery-spec:14 | — | **cites nonexistent rerun report; 2.05 is a mis-read wall-second** | LOW |
| W10 | W3 synthetic block index up to 87× bc/hs at 1000 keys | W3-lookup report §2a | WALL | Y (reproducible) but **irrelevant to corpus** | HIGH (number) / LOW (relevance) |
| W11 | eu-w2oy/#955: malloc/free "19–21% CPU" on hot loop | 07-03 A/B §2b,c; eu-w2oy | PROF | **contradicted** — decode report §1.3 got 1.9–6.2% same code | LOW |
| W12 | src prelude ≈ 0.11.1 on every bench (ratio 0.90–1.02) → HeapSyn engine unchanged 0.11→0.12 | regression report §"decisive evidence" | WALL 5-run 3-way | Y | MED-HIGH |
| W13 | CHANGELOG:8 (eu-cj1h.3): drop_cons system malloc/free/memset 4.6%→0.2% mutator self-time (~8% mutator wall) | CHANGELOG:8 | PROF+WALL single | N | MED |

### 1c. Historical perf claims (0.10–0.11, pre-bytecode — mostly deterministic)

| # | Claim | Source | Method | Conf |
|--|--|--|--|--|
| H1 | CG3 eager arg resolution: 19× speedup foldl@N=10k (O(n²)→linear) | CHANGELOG 0.11 (#911/#915) | TICK+WALL | HIGH (algorithmic, complexity-class change) |
| H2 | W11 strict eager eval: −38% allocs/−34% ticks 010_prelude; −76% 008_folds | CHANGELOG 0.10 | ALLOC+TICK | HIGH |
| H3 | HeapNavigator ok_or→match: −20% mutator CPU | CHANGELOG 0.10 | PROF | MED (single, but matches bd memory re HeapNavigator drop_in_place) |
| H4 | AoC head-retention day01-p2 −95% wall (9.7s→0.46s) | CHANGELOG 0.10 | WALL | HIGH (10× is well above noise floor) |

---

## 2. Measurement-pitfall catalogue (with citations)

- **P1 — Nonexistent citation for the gating figure.** CHANGELOG.md:21 and ROADMAP.md:484
  both cite `docs/superpowers/reports/2026-07-03-bytecode-vs-heapsyn-ab-rerun.md` "for the
  full engine comparison". The file does not exist in the working tree or in
  `git log --all`. The extant report is the non-"rerun" `…-ab.md`. The single most-cited
  perf artefact in the release docs is a dead link.
- **P2 — "2.05" is a unit error.** The "1.2–2.05×" bar (CHANGELOG:21, delivery-spec:14)
  takes 2.05 as a slowdown *ratio*. Its only occurrence in the extant evidence is
  07-03 A/B §3 line 175: "10,000 | 5.005 (min **2.05**, high variance)" — i.e. the
  *minimum wall-seconds* of a high-variance range-foldl run, whose actual ratio column
  reads **2.28×**. Meanwhile fib's real ratio (2.14×) already exceeds 2.05, so the stated
  upper bound is simultaneously mis-sourced *and* lower than the worst extant case.
- **P3 — ~10–35% between-session drift, direction-consistent.** decode-cost report §1.3:
  every regression-set ratio is 10–35% lower on 2026-07-09 than 2026-07-03 with **no code
  change** (branch = unmodified origin/master). Explicitly attributed to machine noise +
  a newer macOS malloc-zone (`_xzm_*`) fast path. This alone invalidates comparing a
  0.12.1 "after" against a 0.12.0 "before" taken in a different session.
- **P4 — malloc-share swung 3–10× between sessions.** 07-03 reported malloc/free at
  ~18.8–20.7% of CPU (the entire root-cause of the regression, and the whole premise of
  eu-w2oy/#955); 07-09 measured 1.9–6.2% on identical code (decode report §1.3, §2.3);
  07-12 measured 7.0% (alloc-gap §3.2). The headline root cause was a moving target.
- **P5 — The blob-fairness mistake.** The eu-mhjz A/B ran **blob-vs-blob** but the blob
  prelude was itself regressed (D3): 1.2–5.65× more allocation at equal ticks. So the
  "HeapSyn" column was handicapped, most severely on day08. The 52× "win" (W7) was
  HeapSyn-crippled-by-blob, not a bytecode win (regression report §"Impact"). *Note the
  separate bisect trap:* plain `cargo build` falls back to source-prelude, so a git-bisect
  on day08 fingered the wrong (docs-only) commit (regression report §"Bisect note").
- **P6 — Sub-200ms / sub-50ms benches are noise.** 07-03 A/B §1.3 flags all `<50 ms`
  harness rows (003, 006, 011–014, day02-p1) as startup-dominated (~3ms startup edge),
  ratio = noise not engine result. 007_short_lived (~120ms) and 005_drop_cons (~65–140ms)
  show the largest run-to-run spread precisely because they are near the floor.
- **P7 — Cross-engine `-S "Total:"` is not comparable.** 07-03 A/B §1: HeapSyn eval work
  is double-counted (pipeline `stg-eval` + `VM` occupancy) → banner over-reports HeapSyn
  ~2×; bytecode reports **zero** Machine/Heap/GC/tick stats (counters wired to HeapSyn
  Machine). Only external wall (or the later `-S "VM Mutator"` figure) is valid; naive use
  of the banner would invert conclusions.
- **P8 — A deterministic metric disagrees across reports.** fib post-fusion ticks:
  CHANGELOG:11 says 88.9M; alloc-gap §2 says 98.3M (both post-#982). Tick counts should be
  identical; the ~10% gap suggests different harness target/N or a staged fusion landing —
  unresolved, and it undermines trust in the "−23%" figure's provenance.
- **P9 — Profiler-attach trap.** decode-cost report §2.1: `sample`-ing the `timeout`
  wrapper PID captures only its `sigsuspend` loop; must `pgrep -P` the real `eu` child.
  A methodology bug that would silently produce garbage profiles.
- **P10 — Shared/loaded machine.** decode-cost §1.2: load average swung **1.9–9.9** during
  one spike; the author ran two passes (high-load 5-run, low-load 7-run) and they differ
  materially. Multiple agents were concurrently active on the same host.
- **P11 — Toolchain/OS drift across the evidence set.** rustc 1.96.0 (07-03) vs 1.97.0
  (07-09/07-12); macOS malloc implementation changed (`malloc_zone_*` → `_xzm_*`). The
  "before/after" comparisons in the ledger straddle these changes.
- **P12 — Asserted-not-measured wins.** "count 0.66×" and "foldl 0.89×" (delivery-spec
  17,45) are the *guardrail* wins the bar says must not regress, yet no report in the tree
  measures them. Also the ~20% code-layout / binary-path-length sensitivity noted in the
  brief is plausible but I found **no report that quantifies it** — it appears to be an
  owner-held observation, not a documented measurement (flag: verify or write it up).

---

## 3. Solid vs shaky

### Rests on deterministic / replicated evidence (trust)
- **Alloc-count parity bc≤hs (D1)** — the single most important structural finding: the
  gap is *not* the bytecode engine allocating more. Deterministic, cross-checked.
- **Fusion reduces fib ticks (D2)** — deterministic (modulo the P8 88.9M/98.3M provenance
  wrinkle to resolve).
- **Blob prelude over-allocates (D3)** — deterministic alloc counts + same-binary proof.
  This *retroactively demolishes* the day08 52× headline.
- **25-leaf Case-of-Case pattern (D4)** — static grep, reproducible; the real justification
  for the fusion work.
- **day11 lookup parity / W3 index worthless on real corpus (W5+§4 of W3 report)** — the
  synthetic 87× is real but the corpus survey (prelude 291-key namespace, day11 589-node
  *dynamic rebuilt* tables) shows realistic upside ≈ 0. Well-argued, multiple angles.
- **range-foldl O(n²) engine-independent (D5)** and **CG3 19× / head-retention 95% (H1,H4)**
  — complexity-class / order-of-magnitude effects far above the noise floor.

### Rests on single noisy runs / assertion / contradiction (do not build on)
- **"fib ~1.265–1.275× bc/hs after fusion" (W3)** — single 9-run session, wall-derived; the
  *tick* reduction is solid but the *ratio* is one session on a loaded machine.
- **"drop_cons ~1.5–1.77× is the architectural decode tax" (W4, D7)** — direction solid
  (bc re-decodes what hs materialised once), magnitude is one session and moved (2.13→1.71
  →1.77 across three reports for "the same" case).
- **"day08 52× win" (W7)** — **discredited**; treat as a blob artefact. With source prelude
  HeapSyn wins (W8).
- **"count 0.66× win" (W6)** — asserted, unsourced. The whole "bytecode already wins on
  higher-order/env-walk" claim needs a real measurement.
- **The original 1.2–2.05× gap (W1/W9)** — the figure the entire 0.12.1 bar was set against
  is mis-sourced (P1/P2) and non-reproducing (P3). The 1.25× acceptance target is anchored
  to a baseline that shifts 10–35% between sessions.
- **malloc-churn as root cause (W11)** — the 07-03 diagnosis (19–21%) that motivated #955
  did not reproduce (1.9–7%). #955 was still worth doing (the transient Vecs are real waste)
  but its headline justification was noise-inflated.

---

## 4. Re-measurement list (ranked — required before any 0.13 go/no-go)

1. **Full A/B, source-vs-source, controlled protocol.** The blob contaminates every
   allocation-heavy row (P5). Re-run the regression set + wins **with the source prelude on
   both engines** (or after the blob-sharing fix), interleaved bc/hs, ≥9 runs, quiesced
   machine (load <1), reporting **ticks + ns/tick + alloc counts + wall median**, not bare
   wall. This is the gate; nothing else should be trusted until it exists. (Supersedes W1,
   W2, W7, W9.)
2. **Re-establish the actual current gap** on today's master (post #955/#980/#982/#984) —
   the "before" the 0.13 decision compares to must be re-taken, not inherited from 07-03.
   Ranked #2 because the 1.25× bar is meaningless against a non-reproducing baseline (P3).
3. **Resolve the fib tick discrepancy (P8):** 88.9M vs 98.3M post-#982. Pin the exact
   harness target/N and confirm the −23% claim deterministically.
4. **Measure the asserted wins (W6):** count, foldl, day07-p2, day11-p1, 004_generations —
   the guardrail cases the bar forbids regressing. They currently have no cited measurement.
5. **Blob-vs-source allocation-parity CI check (regression report rec #2):** assert allocs
   within a few % on a day08-class program, so a sharing-losing blob is caught automatically.
   The blob bug is the highest-value *and* most under-instrumented risk.
6. **Quantify the malloc-share honestly (P4):** with the eu-cj1h.3 fix landed, re-profile on
   a quiesced machine to get a stable malloc/free share (the 1.9–20.7% range across sessions
   is uninterpretable). Confirm CHANGELOG:8's 4.6%→0.2% on a repeat.
7. **Quantify the code-layout / binary-path-length sensitivity (P12)** if the ~20% figure is
   to be relied on — currently undocumented; either measure it or stop citing it.
8. **day08 decode-bound profiling target** (per delivery-spec) — legitimate as a *profiling*
   exemplar for the architectural decode cost, but must be de-listed as a "52× win".

---

## 5. Cross-cutting judgement for the 0.13 gate

The transition is being steered by a body of evidence whose *deterministic* layer (ticks,
alloc counts, static STG shape) is sound and tells a coherent story — **bytecode does the
same allocations but pays a ~1.6–1.8× per-tick decode/dispatch envelope, wins only where
HeapSyn's GC melts down, and that GC meltdown is itself partly a blob bug.** The *wall-clock*
layer that the release notes and the 1.25× acceptance bar are written in is **not yet
measured cleanly even once** at the standard the decision deserves: the gating figure is a
dead citation with a unit error, the regression targets drift 10–35% between sessions, the
root cause was rewritten three times, and the most dramatic headline (52×) is an artefact.
None of this means the direction is wrong — it means the **magnitudes the 0.13 collapse
decision would rest on must be re-measured under one controlled protocol before the decision
is made.** Re-measurement item #1 is the gate.

---

## Appendix — PR-level and bead-level perf claims

*(Populated from the two gatherer agents; see below.)*
