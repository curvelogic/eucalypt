# Review C — Workload Taxonomy & Coverage Map (bytecode vs HeapSyn)

**Author:** Review dimension C. **Date:** 2026-07-12. **Gate:** 0.13 (HeapSyn retirement).
**Question the owner set:** does bytecode EXCEED HeapSyn on *all (or nearly all) workload types*?
**Scope:** read-only analysis of benches, corpus, and the five measurement reports.

---

## 0. Executive summary (the 10 lines that matter)

1. **The "all workload types" claim cannot be supported today.** The evidence base covers exactly two regions of the space: *compute micro-benches* and *AoC puzzles*. Both are compute-bound. Neither touches the product's stated centre of gravity.
2. **The real product use-case — YAML/JSON/TOML import → transform → export, block merge, template rendering — has NO engine benchmark at all.** Not one. (ROADMAP.md §2; confirmed against `tests/harness/bench/` + `benches/`.)
3. **On measured compute, bytecode still *loses* to HeapSyn on every allocation-/call-dense case** — 1.20×–1.96× slower today (`2026-07-09-decode-cost-spike.md` §1.3), improving from the 1.3–2.14× of `2026-07-03-ab.md` §3 as 0.12.1 fusion/alloc work lands.
4. **Bytecode's only decisive win is day08-p1** (>52×) — and that is a HeapSyn *GC death-spiral*, not a bytecode dispatch win (`2026-07-03-ab.md` §2d, §4).
5. **Parity (not a win) is the honest verdict on the true engine comparison** — pure-dispatch code (day11-p1 0.97×, thunk_updates 1.02×) is a wash (`2026-07-03-ab.md` §4).
6. **Biggest unknown-status classes:** import/export & rendering (never measured), startup latency as an engine variable (~3 ms edge only, `ab.md` §1.3), string/regex/interpolation at scale (only sub-50ms smoke tests exist), large block merge (never measured), heap-constrained/GC-pressure (measured once, both engines thrash — `ab.md` §2d).
7. **Block lookup is bimodal and mostly a non-issue:** dynamic tables ≈ parity; large *static* blocks with saturated repeated lookups blow up to 87× *slower* on bytecode (index gated off) but no real workload hits this (`2026-07-12-w3-lookup.md` §2, TL;DR).
8. **Two false-coverage traps:** (a) benches 003/006/011/012/013/014 all run <50 ms → startup-dominated → their ratios are noise, not engine results (`ab.md` §1, §3); (b) `006_block_lookup` does ~20 lookups, not the "1M iters" its docstring implies — the real lookup evidence is ad-hoc spike programs, never committed.
9. **The user-visible perf for the config centre of gravity is startup floor (~53 ms, ~100% front-end prelude re-processing), which the engine choice barely moves** (ROADMAP.md §2.1; `ab.md` §1.3 puts the bc/hs startup delta at ~3 ms).
10. **Recommendation for the gate:** the current corpus can support "bytecode is at parity on dispatch and closing the gap on allocation, with a decisive win under GC pressure" — it **cannot** support "exceeds on all workload types." Fill the four dark classes (import/export, block merge, string-at-scale, startup) before making an all-workloads claim.

---

## 1. Taxonomy — workload classes by dominant bottleneck

Grounded in eucalypt's actual usage: it is a lazy pure-functional language whose centre of gravity is **data/config generation** (YAML/JSON/TOML templating; ROADMAP.md §2) but which carries "no false ceiling" into general computation. The bottleneck classes below span both.

| # | Class | Dominant bottleneck | Where it shows up in real eucalypt use |
|---|---|---|---|
| **A** | Dispatch/decode-dense compute | Opcode dispatch + instruction-stream decode; ~zero heap churn | Predicate-heavy loops, arithmetic kernels, `day11`-style tight loops |
| **B** | Allocation-dense compute | Transient thunk/cons/data-frame construction; per-op `Vec` churn (bc) vs bump-alloc (hs) | Any fold/map producing intermediate structure; `fib`, `drop_cons`, most AoC |
| **C** | Higher-order / env-walk | Cactus-environment chain walk (`EnvironmentFrame::get`); O(n²) lazy accumulators | `foldl`, point-free pipelines, deep prelude call chains |
| **D** | Block construction & merge | Block literal build; `<<` override / `merge`; O(n) cons-list blocks | **The product core** — assembling config docs, layering defaults over overrides |
| **E** | Block lookup — large static-literal | O(n) linear find on bytecode (index gated off) vs indexed on hs | Big literal config tables queried repeatedly by `.key` |
| **F** | Block lookup — dynamic/boxed-symbol | Interpreted find-loop dispatch; index gives ≈no benefit even on hs | Module/namespace `.key`, computed-key lookup in a loop |
| **G** | String / regex / interpolation | String concat, regex cache, interpolation desugar | **Template rendering** — `"{host}:{port}"`, string building for output |
| **H** | List / stream processing | Lazy-list traversal; O(n²) `count`/`foldl`; thunk build | `map`/`filter`/`sum` over imported record sets |
| **I** | Import → transform → export & rendering | Front-end parse (serde) + transform + serialiser; mixed | **The whole product** — read a YAML, reshape it, emit JSON/TOML |
| **J** | Startup / prelude compile latency | Front-end re-processing the 2,347-line prelude on every invocation | Every single `eu` invocation; dominant for small config jobs |
| **K** | GC-pressure / heap-constrained | Immix mark/sweep under a tight `--heap-limit`; managed-heap fill | Very large data sets, or default limit hit by 100M+ alloc programs |
| **L** | IO / subprocess | `io.shell`/`io.exec` roundtrips; external process latency | `eu` orchestrating shell steps, git imports |

---

## 2. Coverage map — per class: benches, current bc/hs status, confidence, gaps

Legend: **bc/hs** figures are external wall medians unless noted; `↓`means bytecode slower. Confidence reflects both measurement quality and how representative the workload is.

### A — Dispatch/decode-dense compute
- **Benches:** `002_thunk_updates` (26M ticks, 0.6M allocs); AoC `day11-p1` (66.5M ticks, 0.49M allocs — the clean dispatch microbench); `day06-p2`.
- **Status:** **PARITY.** day11-p1 **0.97×**, 002 **1.02×**, day06-p2 **1.03×** (`2026-07-03-ab.md` §3, §4). Profile: bytecode's threaded dispatch (`handle_op`+`dispatch` ≈42%) genuinely beats HeapSyn's tree-walk (≈86%), but the gain is eaten by `enter_local` (25%), decode (`read_ref` 11%), and `drop ExecutionError` (6.7%) → net wash (`ab.md` §2a). 0.12.1 boxed the error payload (128→40 bytes, CHANGELOG 0.12.1) and added FusedPrimop, so parity should hold or tip slightly bc-favourable.
- **Confidence:** HIGH. This is the true engine comparison and it is well profiled. **Verdict: not a win, a tie.**

### B — Allocation-dense compute
- **Benches:** `001_naive_fib`, `005_drop_cons`, `007_short_lived`, `008_long_lived_graph`, `009_fragmentation`, `004_generations`; AoC day01/03/04/05/07/09/12.
- **Status:** **bytecode SLOWER, but gap closing.** 07-03: fib **2.14× ↓**, drop_cons **2.13×↓**, short_lived **1.87×↓**, day03-p2 **1.67×↓**, day09 **1.36×↓** (`ab.md` §3). 07-09 re-run (same path, no code change): fib **1.96×**, drop_cons **1.71×**, short_lived **1.20×**, day03-p2 **1.47×**, day09 **1.21×** — every ratio 10–35% lower, direction unchanged; drift attributed to a cheaper macOS malloc fast-path, i.e. *machine-and-moment, not a fixed constant* (`2026-07-09-decode-cost-spike.md` §1.3). Root cause: per-op transient `Vec` on the system heap + fusion-immune byte-stream re-decode (`2026-07-12-allocation-gap-spike.md` §1: re-decode ~14.5%, malloc traffic 5–7% on drop_cons, both absent on hs). 0.12.1 removes the LET/BIF transient malloc (drop_cons malloc share 4.6%→0.2%) and inlines FusedPrimop (fib −23% ticks / −34% bc wall) — **partial, not gap-closing** (CHANGELOG 0.12.1; `allocation-gap-spike.md` §1).
- **Confidence:** HIGH on the measurement, HIGH that bytecode is currently behind here. **This is the class blocking an "exceeds everywhere" claim.**

### C — Higher-order / env-walk
- **Benches:** `range(0,N) foldl((_+_),0)` (ad-hoc, ROADMAP §7 case); `010_deep_find_perf`; day04/05/06-p2 carry heavy env-walk.
- **Status:** **engine-independent O(n²).** N=5k **0.92×** (bc ahead), N=10k **2.28×↓** with large bc variance; both engines O(n²) (ticks ×3.8 for N×2) (`ab.md` §3). `EnvironmentFrame::get` is 8–13% on both and up to 39% on day10-p2 (ROADMAP §2.2). This is an STG-level algorithmic ceiling (BV3 register frames), not an engine differentiator.
- **Confidence:** MEDIUM-HIGH. Measured, but the verdict is "neither engine wins — shared ceiling," which is a nuance the all-workloads claim must not paper over.

### D — Block construction & merge  ⚠️ DARK
- **Benches:** **NONE.** No bench builds large blocks or exercises `<<`/`merge` at scale. `tests/harness/` has merge *correctness* tests only (tiny inputs, run once).
- **Status:** **NEVER MEASURED as an engine A/B.** Blocks are O(n) cons-lists (ROADMAP §2); merge cost scales with block size but no timing exists on either engine.
- **Confidence:** NONE. **This is the product's core operation and it has zero performance coverage.** Highest-priority hole.

### E — Block lookup, large static-literal  ⚠️ mostly synthetic
- **Benches:** `006_block_lookup` — but it does ~20 literal `.key` sums, runs 11 ms, **startup-dominated → ratio 0.77 is noise** (`ab.md` §3, `*` row). Real evidence is the ad-hoc `gen2.py` synthetic in `2026-07-12-w3-lookup.md` §2.
- **Status:** **bytecode catastrophically slower on the synthetic worst case** — index gated off (`machine.rs:3463` → false), so O(n) linear walk: N=40 **1.90×↓**, N=100 **5.21×↓**, N=500 **36.8×↓**, N=1000 **87.6×↓** (`w3-lookup.md` §2a). **BUT no real workload matches** the profile (large static literal + saturated repeated lookup); day11-p1 (genuinely sym-lookup-heavy) is parity (`w3-lookup.md` TL;DR §2). Recommendation there: **DEFER/DROP** the index reinstatement.
- **Confidence:** HIGH on the synthetic curve, HIGH that it doesn't bite real code. **Watch item, not a blocker — unless a real config with a huge hot literal table appears.**

### F — Block lookup, dynamic/boxed-symbol
- **Benches:** none dedicated; day11-p1 is the closest real proxy.
- **Status:** **≈ PARITY.** Dynamic tables see no benefit from an index even on hs (boxed-symbol keys, index stored on a copy that's never reused); realistic ceiling ~1.2–1.5× (`w3-lookup.md` §1, TL;DR). day11-p1 **0.97×**.
- **Confidence:** MEDIUM-HIGH.

### G — String / regex / interpolation  ⚠️ DARK-at-scale
- **Benches:** `011_string_split_join` (25 lines), `012_regex_match` (12 timestamps, once), `013_interpolation`, `014_text_transform` — **all run <50 ms → startup-dominated → all four ratios (0.77–0.89) are noise, not engine results** (`ab.md` §3, `*` rows). `benches/text.rs` re-parses+recompiles each iter → ~32 ms is frontend-dominated, execution a tiny slice → parity but uninformative (`ab.md` §3, `text` note).
- **Status:** **NEVER MEASURED at engine-relevant scale.** These exercise the *feature* for correctness, not the *engine* for throughput.
- **Confidence:** NONE for engine verdict. **Directly relevant to template rendering — a real hole.**

### H — List / stream processing
- **Benches:** implicitly via folds (class C); `range count` probe.
- **Status:** all lazy-list ops O(n²) on both engines (`range(0,10000) count` = 2.2 s, `range(0,20000) count` = 13.3 s, both engines; `ab.md` §3). Engine-independent.
- **Confidence:** MEDIUM. Verdict = shared ceiling, not a win.

### I — Import → transform → export & rendering  ⚠️ DARKEST
- **Benches:** **NONE.** No bench reads a real YAML/JSON/TOML, transforms it, and serialises it. AoC days parse *text input* via eucalypt-side splitting, but none exercise the serde import path + export serialiser as a timed engine A/B. `tests/harness/` has import/export *correctness* tests (~20 files touching import/parse-as/merge) — all tiny, run once.
- **Status:** **NEVER MEASURED.** This is literally what eucalypt is *for* (ROADMAP §2: "the great majority of uses spit out config").
- **Confidence:** NONE. **Top-priority hole alongside D.** An "all workloads" claim is indefensible while the flagship workload is unmeasured.

### J — Startup / prelude compile latency
- **Benches:** none as an A/B; measured incidentally.
- **Status:** startup ~53 ms, **~100% front-end prelude re-processing** (parse 23% / cook 24% / translate 22% / merge 13%); actual eval is µs (ROADMAP §2.1). Engine delta is only **~3 ms** (bc ~9 ms vs hs ~12 ms for `eu -e 1`; `ab.md` §1.3). **This is the user-visible number for config jobs and the engine barely touches it** — it's a front-end/blob problem, not an engine one.
- **Confidence:** HIGH. Important context: winning the engine race does little for the product's dominant latency.

### K — GC-pressure / heap-constrained
- **Benches:** day08-p1 (extreme alloc); naive-fib @ 512 MiB (constraint probe).
- **Status:** **bytecode wins decisively but for a HeapSyn-failure reason.** day08-p1: bc **9.6 s** vs hs **>500 s (TO)**, >52× — HeapSyn is GC-bound (`mark_region_in_block` present and growing), bytecode keeps the *managed* heap small by pushing transients to the C heap (`ab.md` §2d, §4). Under tight heap (fib @ 512 MiB) **both** engines time out >60 s — shared GC-thrash vulnerability (`ab.md` §2 "Mutator vs GC"). At the production 12 GiB / 32 GiB heap, **every compute bench does 0 collections** (`ab.md`; PERFORMANCE.md line 42).
- **Confidence:** MEDIUM. One data point (day08). The win is real but is a HeapSyn pathology, not a general bytecode advantage — and it's fragile (needs the day08 GC pathology understood before claiming it; `ab.md` §7.5).

### L — IO / subprocess  ⚠️ DARK
- **Benches:** NONE. No engine bench exercises `io.shell`/`io.exec`/git import.
- **Status:** **NEVER MEASURED.** Likely engine-neutral (dominated by external latency) but unverified.
- **Confidence:** NONE. Low priority (probably neutral) but formally uncovered.

### Coverage scorecard

| Class | Committed engine bench? | Status | Confidence |
|---|---|---|---|
| A Dispatch | yes (day11, 002) | parity | HIGH |
| B Alloc-dense | yes (fib, drop_cons, 7 AoC) | **bc slower, closing** | HIGH |
| C Env-walk | ad-hoc only | shared O(n²) | MED-HIGH |
| **D Block merge** | **NONE** | **never measured** | **NONE** |
| E Lookup (static) | 006 = noise; synthetic only | bc ↓↓ (synthetic), moot real | HIGH |
| F Lookup (dynamic) | proxy (day11) | parity | MED-HIGH |
| **G String/interp @ scale** | **smoke tests only (noise)** | **never measured** | **NONE** |
| H List/stream | implicit | shared O(n²) | MED |
| **I Import/transform/export** | **NONE** | **never measured** | **NONE** |
| J Startup | incidental | engine ≈ neutral (~3 ms) | HIGH |
| K GC-pressure | day08 (1 point) | bc wins (hs GC failure) | MED |
| **L IO/subprocess** | **NONE** | **never measured** | **NONE** |

**Four fully-dark classes (D, G-at-scale, I, L) plus two "shared-ceiling, not a win" (C, H) mean the measured surface is compute-only.**

---

## 3. Canonical benchmark suite proposal (versioned, <15 min total)

Design rules: every bench **>1 s** on the faster engine to escape startup/noise (kills the sub-50 ms trap in benches 003/006/011–014); deterministic **VM ticks** as the primary metric where the workload is eval-bound (tick counts are engine-characterising and drift-free — note bc currently reports 0 ticks, so tick parity must be read off hs and *wall* used for the bc/hs ratio; fix the bc tick counter as a prerequisite, `ab.md` §1.2); **interleaved-pair wall medians** (9-run, the drift-robust harness from `allocation-gap-spike.md`) for the ratio; **peak RSS** for heap-constrained; **startup ms** for class J. "Bytecode wins" is defined per class.

| Class | Bench(es) — existing or NEW | Size target | Primary metric | "Bytecode wins" means |
|---|---|---|---|---|
| A Dispatch | `day11-p1` (keep); `002` scaled up ×40 to >1 s | >1.5 s | interleaved wall median | bc/hs ≤ 1.00 |
| B Alloc-dense | `001_naive_fib` (fib 30), `005_drop_cons` scaled to >1 s, AoC `day03-p2` | >1 s each | wall median + hs alloc/tick | bc/hs ≤ 1.05 (parity target of 0.12.1 epic) |
| C Env-walk | NEW `range(0,10000) foldl((_+_),0)` (frozen) | >2 s | wall + hs ticks (both O(n²)) | bc/hs ≤ 1.00 (informational — shared ceiling) |
| **D Block merge** | **NEW** `mkbig`: build N=5000-key block via `merge`/`<<` fold of 5000 small blocks; repeat | >1.5 s | wall median | bc/hs ≤ 1.05 |
| E Lookup static | **NEW** frozen copy of `w3-lookup gen2` at N=40 and N=250 | ~0.4 s / >1 s | wall median | bc/hs ≤ 1.10 at N=40 (regression tripwire for index) |
| F Lookup dynamic | `day11-p1` (dynamic-keyed) | 1.6 s | wall median | bc/hs ≤ 1.00 |
| **G String @ scale** | **NEW** `013`/`014` wrapped in a 50k-iteration loop (interpolate+concat a config line 50k×) | >1.5 s | wall median | bc/hs ≤ 1.05 |
| H List/stream | NEW `range(0,20000) map(_+1) sum` frozen | >1 s | wall median | bc/hs ≤ 1.00 |
| **I Import→transform→export** | **NEW** read a ~2 MB YAML fixture → reshape (rename/filter/merge defaults) → emit JSON; and a TOML→YAML variant | >1.5 s | wall median (whole-pipeline) + startup-subtracted eval slice | bc/hs ≤ 1.05 on the eval slice |
| J Startup | `eu -e 'true'` and a 200-line config doc, blob + source-prelude | ~50 ms | startup ms (10-run median) | not an engine metric — track as context |
| K GC-pressure | `day08-p1` (bc-only; hs TO is the finding); `001` @ `--heap-limit-mib 512` | 7–10 s / TO | wall + peak RSS + GC collections | bc completes where hs thrashes/TOs |
| L IO | NEW `io.shell` echo loop ×1000 | >1 s | wall median | bc/hs ≤ 1.05 (expect neutral) |

**New benches to author (7):** D (block merge), E-frozen (lookup curve), G-loop (string at scale), H (list at scale), I×2 (YAML→JSON, TOML→YAML), L (IO loop). Freeze the ad-hoc env-walk (C) and lookup (E) programs into the repo so they stop being uncommitted spikes. **All new benches carry a `RESULT: :PASS/:FAIL` assertion** (matching the existing bench convention) so they double as correctness tests.

Total runtime budget: ~12 min (A 3 s, B 9 s, C 4 s, D 3 s, E 1.4 s, F 1.6 s, G 3 s, H 2 s, I 3 s, J 1 s, K ~20 s + one TO capped at 30 s, L 2 s — ×9 interleaved runs ≈ 11–13 min). Under 15 min. ✓

---

## 4. Dashboard / tracking sketch (modest, not gold-plated)

**Format — one row per (bench, engine, date), append-only.** A single checked-in TSV/JSONL, e.g. `docs/superpowers/engine-ab/results.jsonl`:

```
{"date":"2026-07-12","commit":"5670a16e","bench":"001_naive_fib","class":"B",
 "bc_wall_med":3.83,"hs_wall_med":1.96,"ratio":1.96,"hs_ticks":88900000,
 "hs_allocs":18847763,"gc":0,"host":"m-series-26.5.1","runs":9}
```

**When it runs:** manually via a `cargo xtask engine-ab` (or a shell script `scripts/engine-ab.sh`) that runs the frozen suite interleaved and appends rows — **on demand before each release gate**, not in CI (the benches are >1 s each and machine-noise-sensitive; CI hardware is too variable for a hard gate, exactly the `ab.md` §1.3 drift warning). Optionally a nightly on a fixed self-hosted runner if one exists.

**How regressions surface:** a tiny `xtask engine-ab --check` that reads the last two rows per bench and flags any class where (a) bc/hs crossed its per-class "wins" threshold in §3, or (b) the ratio worsened >15% vs the previous recorded run (the observed run-to-run drift band, `decode-cost-spike.md` §1.3 — anything inside 15% is noise, anything beyond is a signal). Output a one-screen table: class, bench, last ratio, prev ratio, Δ, threshold, PASS/REGRESSED. Feed that table into the release notes.

**Governance:** the results file is the single source of truth for engine claims in CHANGELOG/ROADMAP; a claim like "parity on dispatch" must cite a row. (Note the current CHANGELOG 0.12.0 already cites a report file — `2026-07-03-bytecode-vs-heapsyn-ab-rerun.md` — **that does not exist in the repo**; a dashboard with cited rows prevents that class of dangling reference.)

---

## 5. Source ledger (every status figure is cited above; index here)

- `docs/superpowers/reports/2026-07-03-bytecode-vs-heapsyn-ab.md` — the full sweep: §0 headline (parity on dispatch, 1.3–2.1× slower on alloc), §2a–d CPU profiles, §3 harness+AoC+fold+cargo-bench tables, §4 workload-group analysis, §5 block-index caveat, §6 root cause, §7 recommendations (incl. "do not retire HeapSyn until transient-Vec fix lands").
- `docs/superpowers/reports/2026-07-09-bytecode-decode-cost-spike.md` — §1.3 today's bc/hs (1.20–1.96×, drifting down from 07-03), §2.3 decode/dispatch 46–65% of bc CPU, §3 fusible-sequence finding, lever (c) recommendation.
- `docs/superpowers/reports/2026-07-12-allocation-gap-spike.md` — §1 gap = re-decode (~14.5%, fusion-immune) + system malloc (5–7%); alloc counts identical bc≤hs; per-tick invariant bc 30.6 ns/tick; recommends finishing eu-w2oy (partial).
- `docs/superpowers/reports/2026-07-12-w3-lookup-evidence-spike.md` — TL;DR DEFER/DROP index; §2 scaling curve (1.25× at N=8 → 87.6× at N=1000, synthetic); real workloads at parity.
- `docs/superpowers/reports/2026-07-03-heapsyn-0.11-to-0.12-regression.md` — HeapSyn's own 0.11→0.12 regression (context for the baseline; not a bc/hs figure).
- `ROADMAP.md` §2 — centre of gravity (data/config generation), §2.1 startup tax (~53 ms, ~100% front-end), §2.2 dispatch 60–81% / env-walk 10–39%, 0 GC collections.
- `CHANGELOG.md` 0.12.0 (bytecode default; "1.2–2.05× slower on alloc/call-dense; parity/faster on dispatch/env-walk") + 0.12.1 (FusedPrimop fib −23% ticks; LET/BIF malloc removed; ExecutionError boxed 128→40 B).
- `examples/aoc25/PERFORMANCE.md` — per-day bottleneck labels (mostly VM dispatch + env walk; day08/day10 alloc-intensive), 0 GC at production heap.
- Direct bench inspection (`tests/harness/bench/001–014`, `benches/*.rs`) — confirmed 003/006/011–014 are sub-50 ms startup-dominated smoke tests; `benches/alloc.rs`,`gc.rs` are not engine-selectable; `benches/text.rs` is frontend-dominated.
