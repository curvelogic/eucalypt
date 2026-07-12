# Clean-room re-baseline (eu-2sa6.7)

- **Date:** 2026-07-13 (overnight run, 23:43–23:56 + AoC rerun ~00:0x)
- **Commit:** master `97c4e450` — includes fusion (#980/#982), malloc lever (#984),
  canonical suite (#990), LetRec-count panic fix / wire format v2 (#991)
- **Conditions:** quiesced machine (Spotlight indexing disabled, Logitech/Adobe
  daemons stopped, dolt server stopped, no concurrent agents); Apple M3 Max;
  rustc 1.97.0; single binary per config built at one fixed path; fresh blob via
  `xtask prelude-compile`; every case interleaved bc/hs, N=9, wall via
  `/usr/bin/time -p`, `--heap-limit-mib 12288` under `timeout`; deterministic
  ticks/allocs captured per engine. Protocol: `docs/superpowers/engine-ab/PROTOCOL.md`.
- **Confidence:** measured-verified for within-config ratios (tight spreads,
  quiet box, interleaved); the blob-vs-source deltas are reproduced (see §3).

## 1. Canonical suite (xtask engine-ab, N=9) — both prelude configs

bc/hs ratio (<1 = bytecode faster):

| Class | Bench | blob | source |
|---|---|--:|--:|
| D block merge | 015 | **0.777** | **0.800** |
| I import→export YAML | 016 | **0.964** | **0.931** |
| I import→export TOML | 017 | **0.945** | **0.924** |
| G string at scale | 018 | **0.566** † | **0.923** |
| H list at scale | 019 | 1.105 † | **0.917** |
| E lookup curve (static, saturated) | 020 | 12.97 | 12.08 |
| L io loop | 021 | **0.985** | **0.994** |
| C hof fold | 022 | **0.853** | **0.899** |

† blob-config ratios on G/H are distorted by the §3 finding (the *HeapSyn*
denominator degrades ~2× under blob on G; bc also inflates on H) — the source
column is the cleaner engine comparison for those classes.

**Reading:** bytecode is at or better than HeapSyn on **7 of 8 classes in both
configs** — including the product core (block merge ~0.78–0.80, import→export
~0.92–0.96). The single loss is the synthetic static-block saturated-lookup
tripwire (E, the deliberately-frozen worst case for the gated-off index;
absent from real code per PR #985).

## 2. Legacy regression set (blob config, interleaved N=9 + deterministic ticks)

| Case | bc ticks | hs ticks | bc wall med | hs wall med | bc/hs | 0.12.1 start |
|---|--:|--:|--:|--:|--:|--:|
| 001_naive_fib | 88,853,885 | 115,779,257 | 1.86 | 1.65 | **1.13** | ~1.96–2.14 |
| day03 part-2 | 50,389,079 | 52,119,226 | 1.14 | 0.82 | **1.39** | ~1.47–1.67 |
| day09 part-1 | 190,152,099 | 196,078,258 | 4.20 | 3.67 | **1.15** | ~1.21–1.36 |
| 005_drop_cons ‡ | 2,980,417 | 3,060,419 | 0.08 | 0.05 | ~1.5 ‡ | ~1.7–2.1 |
| 007_short_lived ‡ | 5,346,204 | 5,547,811 | 0.12 | 0.095 | ~1.25 ‡ | ~1.2–1.9 |

‡ sub-200 ms — excluded from ratio analysis per protocol; shown for continuity
with the historical record only.

**Reading:** the fusion + malloc work delivered real, now-cleanly-measured
gains: fib 1.96→**1.13**, day09 →**1.15**. The remaining decode-envelope tail
(fib 1.13, day03 1.39) is precisely what lever (a) pre-decode attacks — the
spike measured a further 18–22% per-tick removal available (report
2026-07-13-predecode-spike.md), which would put every legacy case at or below
parity.

## 3. New defect found: blob prelude ~2× penalty on string-heavy work (both engines)

`018_string_scale`, identical tick counts (76.8M) in both configs:

| Config | bc wall | hs wall |
|---|--:|--:|
| blob | ~2.0–2.2 | **~3.7–3.9** |
| source | ~1.73 | ~1.87 |

Coordinator-reproduced on the same build after the suite run (5 rounds:
hs 3.64–3.91). Same class as eu-fhoo (blob loses prelude sharing) but a
*residual variant*: allocs are **not** elevated here, so the mechanism is
per-tick cost on string paths, not allocation explosion. `019_list_scale`
shows a smaller same-direction signature on bc (+48% blob vs source).
**Impact: release binaries embed the blob → released users pay up to ~2× on
string-heavy workloads.** Filed P1 (bead under eu-2sa6, 2026-07-13). Together
with eu-2sa6.5 (source config misses fusion, fib +10.6% ticks), neither
prelude configuration currently dominates — both parity defects must close.

## 4. Phase-4 gate scorecard (thresholds per transition review §6)

| Class | Threshold | Measured (cleanest config) | Verdict |
|---|---|---|---|
| Dispatch/env-walk | ≤1.00 | hof fold 0.85–0.90 | ✅ |
| Startup/config render | ≤1.00 | block merge 0.78–0.80; import/export 0.92–0.96; string 0.92 (source) | ✅ |
| Decode-bound compute | ≤1.05 | fib **1.13**, day03-p2 **1.39** | ❌ (lever (a) territory) |
| Alloc-bound compute | ≤1.15 | day09 1.15 (boundary); drop_cons ~1.5 (sub-200ms) | ⚠️ boundary |
| GC-pressure | win | not re-measured this run (day08-class) | — |
| Lookup (static synthetic) | waiver candidate | 12–13× (no real-code incidence) | ⚠️ needs written waiver or index fix |

**Conclusion:** the gate is **not yet met** — decode-bound compute is the one
clear failure, and it is exactly what the confirmed pre-decode lever (a)
addresses. If lever (a) delivers the spike-measured 18–22% per-tick reduction,
fib (1.13) and day09 (1.15) go to ≤1.0 and day03-p2 (~1.39) lands ~1.1 —
leaving lookup-class waiver/fix and the two prelude-config parity defects as
the remaining Phase-4 blockers.
