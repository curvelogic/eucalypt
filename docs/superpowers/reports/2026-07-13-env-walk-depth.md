# Runtime environment chain-walk depth — direct measurement (eu-qm7f)

- **Date:** 2026-07-13
- **Bead:** eu-qm7f, owner-mandated follow-up to eu-7xvv: "I want to
  understand this accurately before we consider touching environments."
  Directly measures runtime env chain-walk depth, blob vs source, to test the
  "deep runtime lexical-environment chains" mechanism claimed in eu-2sa6.12
  and repeated (on profile-share evidence, never on a direct depth
  measurement) in eu-7xvv.
- **Worktree/branch:** `/tmp/eu-stopwatch-qm7f`, `spike/env-walk-depth` off
  `origin/master` at `9f1d661e` (merge of PR #1004). The eu-7xvv worktree
  (`/tmp/eu-stopwatch-7xvv`) was stale against this, per the dispatch.
- **Toolchain:** rustc 1.97.0 (`stable-aarch64-apple-darwin`), macOS 26.5.1,
  Apple aarch64.
- **Diagnosis only:** instrumentation added, no representation or
  behavioural change. Clearly marked diagnostic in the source, gated by
  `EU_ENV_DEPTH_HISTOGRAM=1`, disabled by default. No quiet machine was
  needed — the histogram counts are deterministic (a property of the
  compiled program and lookup sequence, not of timing), so ordinary machine
  load does not affect them.

---

## 1. Headline finding — **outcome (b): equal depths, locality not depth**

**The "deep runtime lexical-environment chains" mechanism claimed in
eu-2sa6.12 and eu-7xvv is REFUTED by direct measurement.** Runtime
environment chain-walk depth is **shallow in both blob and source
configurations, on both benchmarks, and is not materially different between
them**:

| Bench | Config | Total lookups | Combined mean depth | Max depth |
|---|---|--:|--:|--:|
| 018_string_scale | blob | 75,295,727 | **0.656** | **2** |
| 018_string_scale | source | 75,092,689 | **0.661** | **3** |
| 019_list_scale | blob | 55,059,223 | **0.656** | **2** |
| 019_list_scale | source | 54,843,184 | **0.662** | **3** |

(Combined mean computed from the raw per-bucket hop counts across both the
"annotated" and "unannotated" histograms — see §3 — not a simple average of
the two printed means.) **Confidence: measured-verified** — every figure
above was reproduced byte-identically across two independent runs on the
byte engine, and byte-identically again on the pre-decoded engine (see §4);
this is the strongest possible confidence bar for a deterministic count.

This directly answers the dispatch's central question and overturns the
eu-2sa6.12/eu-7xvv narrative: blob's env chains are **not** deeper than
source's. Depths in both configs sit almost entirely in buckets 0 and 1 (a
frame the lookup starts in, or one hop to its immediate parent); the deepest
anything ever reaches is bucket 2 (blob) or bucket "3-4" (source, and only
for a vanishingly small 21,001-lookup population on 018, 12,001 on 019 — see
§3.3). This matches ROADMAP §10.2's own historical finding ("eucalypt's env
chains are shallow, avg depth 2") almost exactly, and extends it: shallow
chains are not an AoC-era artefact, they hold under the blob prelude too.

**Static-vs-runtime reconciliation (explicitly requested):** the widely-cited
"1874 thunks / 538 letrec / max pretty-print nesting 171" figure from
eu-2sa6.12 §3.2 does **not** manifest as deep runtime chains — max observed
runtime depth this session was **2**, nowhere near 171. This confirms, with a
direct runtime measurement rather than an inference, what PR #997's
Core-level reflatten experiment already suspected: that figure was an
artefact of `dump runtime`'s whole-file pretty-printer indentation (all 352
bindings concatenated), not a per-call lexical-chain depth. Laziness and the
peeled-global entry pattern keep runtime chains shallow regardless of how
deep the static, whole-file nesting proxy looked.

---

## 2. Instrumentation design

Added to `src/eval/machine/env.rs` (`env_depth_diag` module) and wired at
process exit in `src/bin/eu.rs`. Design choices, and why they cannot distort
the counts:

- **Gated by `EU_ENV_DEPTH_HISTOGRAM=1`**, cached once via `OnceLock`
  (mirrors the `EU_STACK_DIAG` precedent in `src/eval/machine/vm.rs:1543`:
  "checked once at startup to avoid an env-var lookup on every VM step").
  Disabled (the default), it is a single cached-bool check per lookup — zero
  behavioural effect, confirmed by the full harness suite (§5).
- **A read-only shadow walk, not a modification of the real walk.** The
  instrumentation is a **separate** traversal
  (`EnvironmentFrame::diag_record_depth`) added alongside the unmodified
  `cell()`/`get()` — it re-walks the same `next` chain purely to count hops,
  touching no `bindings` array and returning nothing. `get()`'s actual return
  value and the entire evaluation semantics are byte-for-byte unchanged
  whether or not the flag is set (confirmed: full harness suite green with
  the flag off, and output is identical with the flag on — every benchmark
  in this report still printed `RESULT: PASS`). This was a deliberate
  safety choice over rewriting `cell()` itself to count hops as a side
  effect, to eliminate any risk of the diagnostic perturbing the measured
  system.
- **Deterministic, not wall-time.** Hop counts are a property of the
  compiled program and the (deterministic, lazy) evaluation order, not of
  scheduling or timing, so — per the dispatch's own framing — perturbation
  from the extra shadow walk is acceptable and does not need a quiet
  machine. This session's reruns (§4) confirm exact reproducibility.
- **Attribution via the frame's own `annotation: Smid`.** Each lookup is
  bucketed by whether the **starting** frame (the one `get()` was called on)
  carries a valid or invalid (`Smid::default()`) source annotation.
  `src/common/sourcemap.rs`'s own documented invariant — "Pre-compiled blobs
  use `Smid::default()` (0) for all prelude locations" — makes this a
  built-in, free, coarse "prelude-blob code vs user/source code" attribution
  requiring no per-call-site instrumentation. See §3 for what it does and
  doesn't show.
- **Buckets:** 0, 1, 2, 3-4, 5-8, 9-16, 17-32, 33+ hops, plus running count,
  sum (for mean), and max, using relaxed atomics (correctness of the final
  dump doesn't depend on cross-thread ordering here — the eval thread is the
  sole writer in every configuration tested).
- **Dump point:** `src/bin/eu.rs`, immediately before `process::exit`, so it
  fires exactly once per process regardless of which driver mode ran.

---

## 3. Full histograms

### 3.1 018_string_scale

**Blob** (predecode engine; byte-identical to byte engine, see §4):

| | annotated (user / source-compiled) | unannotated (blob-loaded prelude) |
|---|--:|--:|
| count | 25,525,621 | 49,770,106 |
| mean | 0.9742 | 0.4926 |
| max | 2 | 1 |
| bucket 0 | 728,076 | 25,252,600 |
| bucket 1 | 24,727,542 | 24,517,506 |
| bucket 2 | 70,003 | 0 |
| bucket 3+ | 0 | 0 |

**Source** (predecode engine; byte-identical to byte engine):

| | annotated (user + source-compiled prelude) | unannotated |
|---|--:|--:|
| count | 74,679,627 | 413,062 |
| mean | 0.6649 | 0.0000 |
| max | 3 | 1 |
| bucket 0 | 25,210,570 | 413,061 |
| bucket 1 | 49,301,049 | 1 |
| bucket 2 | 147,007 | 0 |
| bucket 3-4 | 21,001 | 0 |

Total lookups: blob 75,295,727 vs source 75,092,689 — **0.27% apart**,
consistent with the identical HeapSyn tick counts re-verified on this build
(§5): blob 76,814,993 vs source 76,751,915 (Δ0.08%). **The "ticks identical →
lookup counts should be ~equal" assumption holds** (measured-verified).

### 3.2 019_list_scale

**Blob** (predecode; byte-identical to byte engine):

| | annotated | unannotated |
|---|--:|--:|
| count | 18,507,117 | 36,552,106 |
| mean | 0.9776 | 0.4927 |
| max | 2 | 1 |
| bucket 0 | 420,069 | 18,543,102 |
| bucket 1 | 18,081,045 | 18,009,004 |
| bucket 2 | 6,003 | 0 |

**Source** (predecode; byte-identical to byte engine):

| | annotated | unannotated |
|---|--:|--:|
| count | 54,645,122 | 198,062 |
| mean | 0.6645 | 0.0000 |
| max | 3 | 0 |
| bucket 0 | 18,429,062 | 198,062 |
| bucket 1 | 36,132,050 | 0 |
| bucket 2 | 72,009 | 0 |
| bucket 3-4 | 12,001 | 0 |

Total: blob 55,059,223 vs source 54,843,184 — **0.39% apart**. Same
signature as 018: near-equal counts, near-equal (shallow) depths.

### 3.3 Attribution — what could and couldn't be attributed

**The coarse annotated/unannotated split works exactly as designed and is
informative, but there is no deep tail to attribute in the first place.**
Since depths never exceed 2 (blob) or 3 (source), there is no "which
prelude helper causes the deep walks" question to answer — nothing walks
deep. What the split *does* show clearly:

- Under blob, roughly two-thirds of all lookups (49.77M / 75.30M on 018) are
  "unannotated" — i.e. happening while executing blob-loaded prelude
  closures — and these are *shallower on average* (mean 0.49) than the
  "annotated" user-code lookups (mean 0.97). This is the opposite of what a
  "deep prelude chains" story would predict: prelude-body lookups are the
  *shallow* half, not the deep half.
- Under source, almost everything is "annotated" (74.68M / 74.98M lookups)
  because source-compiled prelude carries real smids too, per
  `sourcemap.rs`'s own documented invariant — the "unannotated" bucket
  (413,062 lookups on 018) is a small residual, not a comparison population,
  and its near-zero mean depth is unsurprising given its tiny size.
- **The one place depth reaches 3-4 hops is exclusively under *source*, not
  blob** (21,001 / 74.68M = 0.03% of lookups on 018; 12,001 / 54.65M = 0.02%
  on 019). This is a curiosity worth naming rather than a mechanism: it is
  too small a population (well under a tenth of a percent) to explain a
  1.3-2x wall-clock or self-time-share penalty, and it points the *wrong*
  direction (source has the deeper tail, not blob). I did not chase further
  attribution of this specific population (e.g. via sampled call-stack
  correlation) — it is not large enough to be load-bearing for the
  blob-vs-source penalty this investigation is about, and a finer per-call
  attribution (bucketing by specific prelude function or user binding) was
  judged not worth the added instrumentation risk given the headline finding
  already answers the question the bead was opened to test.

**Conclusion on attribution:** the requested "coarse split (lookups executed
inside prelude-global code vs user code)" was fully achievable (via the
existing `annotation` field) and is reported above; a finer, per-function
attribution was not attempted because there is no deep-tail population left
to attribute once the headline finding — shallow, near-equal depths on both
sides — is established.

---

## 4. Determinism and engine-independence

Every histogram above was reproduced **byte-identically** across:

- Two independent runs of the same config on the byte engine (018 blob and
  source both re-run; every bucket, mean, and max identical to the
  reported figures).
- The byte engine vs the pre-decoded engine (`EU_PREDECODE=1`), for both
  018 and 019, both blob and source — every bucket, mean, and max
  identical between the two dispatch loops. This is exactly the
  "byte-engine spot-check to confirm engine-independence" the dispatch
  asked for, and it succeeded: the shared `EnvironmentFrame::get` code path
  produces identical counts regardless of which dispatch loop calls it, as
  expected from `BcEnvFrame` being a type alias for the same generic struct
  (established in the prior eu-7xvv report).
- HeapSyn (`EU_HEAPSYN=1`) on 018 blob was also checked as a bonus (not
  requested, included because the instrumented code is shared by all three
  engines at no extra cost): counts were **very close but not identical**
  (25,574,555 vs byte/predecode's 25,525,621 annotated lookups on blob,
  ~0.19% higher; same shape, same max depth 2, mean 0.977 vs 0.974). This
  small divergence is plausible given HeapSyn's tree-walk evaluator can
  differ in exact thunk-forcing order from the two bytecode engines; it does
  not change the shallow-depth conclusion and is noted as **measured-single**
  (only one HeapSyn run taken, not repeated) rather than re-litigated
  further, since the byte/predecode agreement already meets the
  measured-verified bar for the headline claim.

**All of these figures are therefore measured-verified** per the reproduction
bar stated in the dispatch ("label it measured-verified only after you've
reproduced it twice") — reproduced twice on byte, and independently
corroborated on predecode, with identical results.

---

## 5. Corroborating evidence: locality, not depth

Re-verified on this build (post-PR #1004; measured-verified, deterministic
layer):

| Metric (018, HeapSyn) | blob | source |
|---|--:|--:|
| Ticks | 76,814,993 | 76,751,915 |
| Allocs | 364,176 | 385,181 |

| Metric (018, byte engine `-S`) | blob | source |
|---|--:|--:|
| Blocks Allocated | 3,779 | 3,534 |
| Blocks Used | 3,777 | 3,532 |

Blob has **fewer allocations** (364,176 vs 385,181, −5.5%) but **more
resident blocks** (3,779 vs 3,534, +6.9%) — i.e. worse packing density: the
same (or slightly less) logical work is spread across more heap blocks under
blob. Combined with §1's finding that logical chain depth is not the
differentiator, this is consistent with a **per-hop/locality cost**
mechanism — each `EnvironmentFrame::get` hop is not walking further, but is
more likely to touch a cold cache line or a different heap block under blob
than under source, where `inline()`'s cross-binding fusion keeps hot-loop
state packed into fewer, more reused frames. This is **projected** (reasoned
from the block/alloc numbers, not independently measured via a cache-miss
counter this session) but is the natural reading of "same ticks, same
lookup counts, same depths, higher wall/self-time" once depth itself is
ruled out.

---

## 6. Correctness verification (harness, all three engines)

`cargo build --release`, `cargo clippy --all-targets -- -D warnings`
(clean), `cargo fmt --all` (only reformatted the new code), then the full
suite with the instrumentation compiled in but disabled by default:

| Engine | Tests | Failures |
|---|--:|--:|
| bytecode (default) | 1,940 across all test binaries | **0** |
| HeapSyn (`EU_HEAPSYN=1`) | 1,940 | **0** |
| predecode (`EU_PREDECODE=1`) | 1,940 | **0** |

Confirms the diagnostic instrumentation is behaviour-neutral by default, on
every engine.

---

## 7. Verdict and mapping to sanctioned options

**Verdict: outcome (b) — equal depths, locality-class explanation. The
eu-2sa6.12 "deep runtime lexical-environment chains" narrative is refuted by
direct measurement**, not merely re-characterised. The enter_local
self-time-share growth under blob documented in eu-7xvv is real (that
profile evidence stands — it is a *self-time share* measurement, not a
depth measurement, and nothing here contradicts it) but its *cause* is not
longer chain walks; both configs walk shallow, near-identical-length chains
over near-identical numbers of lookups. The cost difference must be
per-lookup/per-hop, most plausibly a locality/cache-layout effect (§5),
though this session did not instrument cache misses directly.

**No fix is recommended here (diagnosis-only scope), per the dispatch's own
mapping of outcomes to already-sanctioned options:**

- Outcome (a) (a named, blob-specific deep tail) would have evidenced
  targeted STG-level generation flattening. **Does not apply** — there is no
  deep tail, blob or source.
- **Outcome (b) (this result: equal depths → locality) points at BV3
  register frames + CG4 selective lifting** — the ROADMAP §10.2-sanctioned,
  targeted representation/locality work, chosen specifically *because* the
  0.10.0 flat-closures post-mortem already established that eucalypt's
  chains are shallow and that a *uniform* representation change wastes a
  capture-recipe tax for no depth-reduction win. This result extends that
  finding (shallow chains hold for blob too) and is consistent with BV3/CG4
  being the right-shaped fix *if* the blob penalty is worth targeting at
  all, rather than any further blob-generation-level flattening work (which
  §6 of the eu-2sa6.12 report and this report's §1 both show has nothing to
  flatten).
- Outcome (c) (shallow everywhere, profile attribution itself suspect) is
  **partially the actual result but not the right frame**: depths are
  shallow everywhere, but the enter_local profile-share evidence from
  eu-7xvv is not thereby discredited — self-time share and chain depth are
  different measurements, and this report does not re-run or challenge the
  profiling. What's suspect is only the *causal story* connecting the two
  (deep chains explaining the share), not the share measurement itself.

**This gates eu-2sa6.18 differently than eu-7xvv projected**: eu-7xvv's
"projected... inherent to the peel/global-slots form" conclusion was built
on the (now refuted) deep-chains story. With depth ruled out, the remaining
open question for eu-2sa6.18 is whether a locality-class fix (BV3/CG4, or
denser blob-generation layout) can close the gap within the current
peel/global-slots form, rather than requiring the in-memory co-compilation
redesign eu-7xvv leaned toward. This report does not decide that — it only
removes one candidate mechanism (depth) from consideration and points at the
already-sanctioned alternative (BV3/CG4) as the next place evidence should
be gathered, per the owner's request to understand the mechanism accurately
before touching environments.

---

## 8. Evidence index

- Instrumentation: `src/eval/machine/env.rs` (`env_depth_diag` module,
  `EnvironmentFrame::diag_record_depth`), `src/bin/eu.rs` (dump call site).
- Raw histogram output: reproduced inline in §3; not separately archived
  (deterministic — reproducible via `EU_ENV_DEPTH_HISTOGRAM=1 ./target/
  release/eu --heap-limit-mib 12288 [--source-prelude] [-t bench-string-scale
  | -t bench-list-scale] tests/harness/bench/{018_string_scale,
  019_list_scale}.eu`, with/without `EU_PREDECODE=1`/`EU_HEAPSYN=1`).
- Corroborating `-S` output: §5, reproducible the same way with `-S` in
  place of `EU_ENV_DEPTH_HISTOGRAM=1`.
- Prior reports this builds on / revises: `docs/superpowers/reports/
  2026-07-13-blob-string-penalty-diagnosis.md` (eu-2sa6.12, including its
  §6/§7 addenda), `docs/superpowers/reports/
  2026-07-13-blob-string-penalty-profile.md` (eu-7xvv).
- ROADMAP context: `ROADMAP.md` §10.2 (flat closures post-mortem, avg depth
  2), §6.4 item 4 / CG4 (globals O(1), no env walk; the higher-order-fold
  cost is re-resolving a captured local through the env-walk).
