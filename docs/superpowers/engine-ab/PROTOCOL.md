# Engine A/B Measurement Protocol

- **Status:** the checked-in measurement standard for all bytecode-vs-HeapSyn
  performance claims (bead eu-2sa6.6; ratified by the 0.13 transition review,
  `docs/superpowers/specs/2026-07-12-bytecode-transition-review.md` §5.1).
- **Scope:** any engine performance number that enters CHANGELOG, ROADMAP, a
  release gate, a bead, or a report **must** be produced under this protocol and
  cite a dated report or a `results.jsonl` row.

This document is operational, not an essay: it is the checklist an agent
follows. The reasons behind each rule are the twelve measurement pitfalls
catalogued in review A §2 (`docs/superpowers/reports/2026-07-12-transition-review-A-perf-evidence.md`);
each rule cites the pitfall(s) it neutralises as `[P#]`.

---

## 1. What to measure, in order

1. **Ticks first (deterministic).** VM ticks and allocation counts are the only
   drift-free layer `[P3, P4, P10, P11]`. Read them from a HeapSyn `-S` run
   (`EU_HEAPSYN=1 eu -S …`). Note: the **bytecode engine currently reports 0
   ticks** — tick/alloc parity is read off HeapSyn and the bc/hs *ratio* is a
   wall figure `[P7]`. Ticks are engine-characterising; a change in ticks is a
   real change, a change in wall alone may be noise.
2. **Wall median-of-N (comparative).** Interleaved bc/hs pairs, median with
   spread (see §3). Only the ratio is reported, never a bare wall second `[P2]`.
3. **Profile share (only for allocation/CPU-attribution claims).** `sample` the
   real `eu` child (see §4), never the `timeout` wrapper `[P9]`. Malloc share
   swung 3–10× between sessions `[P4]` — a single profile is never load-bearing.

---

## 2. Build and environment (do this once per session)

- [ ] **Same binary, same path, for both engines.** Build **one** release
      binary and select the engine with the `EU_HEAPSYN=1` environment variable
      `[P5, and the ~20% binary-path-length sensitivity in P12]`. This is
      stricter than "two binaries at the same path": with one binary there is no
      path-length or link-order skew possible. `cargo xtask engine-ab` does
      exactly this.
- [ ] **Regenerated blob present.** Run `cargo xtask prelude-compile` so
      `lib/prelude.blob` is fresh. Both engines then share the *same* blob (one
      binary), removing the blob-fairness handicap that produced the discredited
      "day08 52×" `[P5]`. Once the source-prelude fusion-parity defect (eu-2sa6
      P2 bead) is fixed, additionally assert tick parity blob-vs-source.
- [ ] **Record `prelude_config`** (`blob` or `source`) in every row — never
      compare a blob run against a source run `[P5]`.
- [ ] **Quiet machine.** Load average < 1 before starting; no other agents
      building or benchmarking `[P10]`. If the machine is loaded, say so and
      label the numbers *measured-single* at best.
- [ ] **Record the toolchain.** rustc version and OS in the report; do not
      compare "before/after" across a rustc or OS-malloc change `[P11]`.
- [ ] **Wrap every `eu` in `timeout` with `--heap-limit-mib 12288`** (12 GiB —
      the compute suite does 0 collections at this limit, matching production).

---

## 3. Sampling and statistics

- [ ] **Interleave** bc and hs run-for-run (bc, hs, bc, hs, …), never all-bc
      then all-hs — session drift is 10–35% `[P3]` and interleaving cancels it.
- [ ] **Median-of-N**, N ≥ **7 for sub-second** benches, N ≥ **5 for
      multi-second** benches `[P6, P10]`.
- [ ] **Report spread** (min..max, or IQR) alongside the median. A median
      without spread hides variance `[P6]`.
- [ ] **Nothing under ~200 ms enters ratio analysis.** Sub-200 ms (and
      certainly sub-50 ms) benches are startup-dominated noise, not engine
      results `[P6]` — this is why benches 003/006/011–014 were struck. Every
      canonical bench is **> 1 s on the faster engine** (one documented
      exception, §6).
- [ ] **The `-S` pass is separate** from the timed runs — its reporting overhead
      must not enter the wall medians.

---

## 4. Profiling (only when attributing cost)

- [ ] `sample` the **real child PID**: `pgrep -P <timeout-pid>` (or run `eu`
      without the wrapper) — sampling the `timeout` wrapper captures only its
      `sigsuspend` loop and yields a garbage profile `[P9]`.
- [ ] Treat any single profile's malloc/CPU share as *projected*, not
      *measured-verified* `[P4]` — it moves 3–10× between sessions.

---

## 5. Confidence labels (mandatory on every number)

Every reported figure carries exactly one label (review §5.3):

| Label | Meaning | May it gate a release / enter CHANGELOG? |
|---|---|---|
| **measured-verified** | deterministic (ticks/allocs) **or** wall re-measured in ≥2 interleaved sessions under this protocol | **yes** |
| **measured-single** | one clean interleaved session, protocol-compliant | no — context only |
| **projected** | reasoned from architecture / a single profile / extrapolation | no |

Only **measured-verified** numbers may gate Phase 4, enter CHANGELOG/ROADMAP, or
justify retirement. A number with no label is treated as *projected*.

---

## 6. The canonical suite and its one exception

The suite is the eight benches in `tests/harness/bench/` covering the previously
dark/under-measured workload classes (review C §3):

| Bench | Class | What it stresses |
|---|---|---|
| `015_block_merge` | D | block construction + `<<` deep-merge |
| `016_import_export_yaml` | I | YAML import → reshape → export |
| `017_import_export_toml` | I | TOML import → reshape → export |
| `018_string_scale` | G | interpolation/concat at scale |
| `019_list_scale` | H | range → map → sum (shared O(n²)) |
| `020_lookup_curve` | E | static large-block lookup (N=40, N=250) |
| `021_io_loop` | L | 1000 × io.shell echo |
| `022_hof_fold` | C | frozen env-walk fold (shared O(n²)) |

Rules:

- [ ] Every bench is **deterministic** and carries a `RESULT: :PASS/:FAIL`
      assertion gated by explicit `verify: ["default-expectation"]` metadata (a
      bare `bench-*` target auto-passes — see `lib/test.eu` `bench-expectation`).
      They run under `cargo test` (bytecode) and `EU_HEAPSYN=1 cargo test`
      (HeapSyn).
- [ ] Benches must produce **byte-identical rendered output** on both engines.
- [ ] Total suite wall (interleaved, both engines) stays **< 15 min**.
- [ ] Engine claims cite a `results.jsonl` row, never a bare recollection `[P1]`
      (the release docs cited a report that does not exist — never again).

**The one exception to "> 1 s on the faster engine": `020_lookup_curve` (class
E).** Its entire purpose is the bc/hs *asymmetry* — the bytecode linear find
loop vs the HeapSyn key index. Making the indexed HeapSyn side exceed 1 s would
force the bytecode side to ~15 s and break the suite budget. So bc sits ~2.5 s
(well above the 200 ms floor) while hs is intrinsically ~0.3 s (still ~5× the
startup floor, so the ratio is sound). Documented here, in the bench docstring,
and in the ledger. No other bench may claim this exception without an equally
explicit, owner-visible justification.

**A live defect the suite surfaced:** the **bytecode engine panics** with
`bytecode: invalid opcode` on TOML imports above ~1900 full-schema records (the
code section from the embedded literal overruns a jump-offset width; YAML nests
into smaller scopes and survives further). `017_import_export_toml` is
deliberately capped at 1600 records below this so it passes on both engines;
both import benches reach > 1.5 s via reshape rounds instead of record count.
This is a real P0/P1 class-I finding for the transition, not a bench artefact.

---

## 7. The ledger and the runner

- `docs/superpowers/engine-ab/results.jsonl` — append-only, one row per (bench,
  run). Row schema (review C §4):

  ```json
  {"date":"2026-07-12","commit":"7a82ad2f","bench":"022_hof_fold","class":"C",
   "bc_wall_med":1.47,"hs_wall_med":1.66,"ratio":0.886,"hs_ticks":51985369,
   "hs_allocs":210167,"gc":0,"host":"Darwin-25.5.0-arm64","runs":5,
   "prelude_config":"blob"}
  ```

- `cargo xtask engine-ab [--runs N] [--eu PATH] [--dry-run]` runs the full suite
  interleaved per §2–§3 and appends one row per bench.
- `cargo xtask engine-ab --check` reads the last two rows per bench and prints a
  one-screen table (bench, class, prev ratio, last ratio, Δ%, class threshold,
  status). It exits **1** on a **regression** (ratio worsened > 15% vs the
  previous row — the review-A P3 noise band). A per-class threshold crossing is
  reported as **WATCH** (informational); class E (the tripwire) is always WATCH,
  never a hard failure.
- **Not wired into CI.** The benches are > 1 s and machine-noise-sensitive; CI
  hardware is too variable for a hard gate `[P10, P11]`. Run on demand before a
  release gate on a quiet machine, and paste the table into the release notes.
