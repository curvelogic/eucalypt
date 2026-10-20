---
name: eucalypt-performance
description: Performance work on eucalypt — profiling, finding bottlenecks, optimising the engine, GC or prelude, benchmarking, or making any claim about speed. Use before profiling or changing code for speed, before running or reporting benchmarks, and before putting a performance number in a PR, bead, CHANGELOG, ROADMAP or report.
---

# eucalypt performance work

Performance work runs in two phases. Do not skip to the second.

## Phase 1 — profile and hypothesise (report only)

1. Profile the targets you were given (`cargo bench`, the
   `tests/harness/bench/` programs, `-S`/`--statistics`, `scripts/gc-bench.sh`;
   see `BENCHMARKING.md` and `docs/development/gc-benchmarking.md`).
2. Form specific, testable hypotheses: where the time goes, why, and what
   change should move it by roughly how much.
3. Report them, and **stop**. No branch, no code, no PR until the
   hypotheses are approved by whoever dispatched the work.

## Phase 2 — implement (only after approval)

- One optimisation per PR, with before/after benchmarks and regression
  data across the full test suite.
- Never change observable behaviour: output, error paths, evaluation
  order and source locations stay identical; no `Smid::default()` in new
  code; edge-case inputs tested.
- Never replace a prelude function with a native Rust intrinsic (and a new
  intrinsic is a change the owner reviews, `REVIEW.md`).
- Engine defaults and GC or memory management are sensitive areas with a
  review rule of their own; backend changes also follow the
  `eucalypt-backend` skill (`EU_GC_VERIFY=2` + `EU_GC_STRESS=1`).
- Wrap every `eu` process in `timeout`.

A PR from an open-ended "find things to improve" dispatch is one the owner
reviews (`REVIEW.md`); a directed task is reviewed normally.

- Run the `CLAUDE.md` pre-commit checklist (`cargo fmt --all`, `cargo clippy
  --workspace --all-targets -- -D warnings`, `cargo test`) before every PR.
- Review and merge: `REVIEW.md` on `master`. Until it lands, the same rules
  are in `CLAUDE.md` ("Recorded review before merge", "Owner-hold PRs") and
  the current gatekeeper's owner review filter.

## Claims about performance — mandatory protocol

`CLAUDE.md` "Engine Performance Claims — Mandatory Protocol" is binding.
In short:

- Measure under `docs/superpowers/engine-ab/PROTOCOL.md` or a protocol
  like it: interleaved pairs on a quiet machine, ticks first, wall
  median-of-N with spread, one binary and one blob for both
  configurations, nothing under ~200 ms in ratio analysis.
- Label every figure measured-verified, measured-single or projected.
- A number comparing execution configurations that enters CHANGELOG,
  ROADMAP, a release gate, a bead or a report cites a dated report or a
  ledger row (`docs/superpowers/engine-ab/results.jsonl`).
- The canonical suite is `tests/harness/bench/{015..022}`.
- Benchmark numbers reported by an agent are verified independently before
  they are accepted, from a clean build
  (`cargo clean && cargo build --release`).
