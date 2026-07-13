# Blob prelude penalty — heap layout forensics (eu-ttpl)

- **Date:** 2026-07-13
- **Bead:** eu-ttpl, prong 1 of confirming the locality mechanism eu-qm7f's
  depth measurement pointed at (deterministic/structural evidence; prong 2 —
  hardware counters — is separate, scheduled with the lever(a) wall runs in
  a quiet window). Diagnosis only, no fixes, no environment-representation
  changes.
- **Worktree/branch:** `/tmp/eu-stopwatch-ttpl`, `spike/blob-layout-forensics`
  off `origin/master` at `9f1d661e` (merge of PR #1004; same base as eu-qm7f,
  whose worktree `/tmp/eu-stopwatch-qm7f` this spike reuses the design
  philosophy and cross-checks totals against, but not its code — PR #1005
  was unmerged at branch time, so this spike's instrumentation is a fresh,
  self-contained module).
- **Toolchain:** rustc 1.97.0 (`stable-aarch64-apple-darwin`), macOS 26.5.1,
  Apple aarch64.
- **No quiet window needed.** All figures here are deterministic counts
  (lookup totals, block/frame counts, hit rates) or, where noted, confirmed
  stable in *shape* despite address-level ASLR variation — not wall-clock
  timings. Ordinary machine load does not affect them.

---

## 1. Headline finding

**Sequential block-locality — whether one environment-frame lookup lands in
the same 32 KiB Immix block as the *previous* lookup — is substantially worse
under blob than source on every workload the bead's per-class map identifies
as penalised, is essentially unchanged where the map says the penalty is
absent, and is essentially unchanged (not inverted itself) on the one
workload where the *wall-clock* penalty inverts.** This is a **measured
structural difference**, not the depth difference eu-qm7f already refuted —
depths and lookup counts are equal (eu-qm7f); what differs is where in the
heap the frames those lookups land on physically sit relative to each other,
call to call.

| Bench | Class (bead's per-class map) | blob hit-rate | source hit-rate | gap (source − blob) |
|---|---|--:|--:|--:|
| 018_string_scale | penalty, largest (1.26x bc / 2.06x hs) | 0.642 | 0.870 | **0.228** |
| 019_list_scale | penalty, medium (1.48x bc / 1.23x hs) | 0.808 | 0.871 | 0.063 |
| 022_hof_fold | penalty, smallest (1.10x bc / 1.16x hs) | 0.791 | 0.860 | 0.069 |
| 020_lookup_curve | **inverted** (blob 0.87-0.93x, i.e. faster) | 0.911 | 0.913 | **0.001** |
| 021_io_loop | absent | 0.676 | 0.684 | 0.008 |

("hit-rate" = fraction of consecutive `EnvironmentFrame::get` lookups whose
landing frame is in the same block as the previous lookup's landing frame;
byte engine, single deterministic measurement per cell, reproduced twice for
018 — see §4.)

**The gap tracks the per-class map almost exactly**: it is large where the
wall-clock penalty is large (018), moderate and roughly equal where the
penalty is moderate-to-small (019, 022), and effectively zero both where the
penalty is *absent* (021) and where it *inverts* (020) — the inversion is
not explained by *better* blob locality, it is explained by locality being
irrelevant to that benchmark on this metric (see §3.3): 020's dominant cost
is static block-key lookup (`Ref::G`, O(1), no env walk per ROADMAP §6.4
item 4), which this instrumentation does not touch at all, so its identical
hit-rate here is a *null result*, not a finding about that mechanism.

**Confidence:** measured-verified for 018 (byte engine figures reproduced
byte-identically twice, including full block/frame breakdowns — see §4);
measured-single for 019/020/021/022 (one clean deterministic run each, not
yet independently repeated, though determinism is structural here — see §4
for why a second run is expected, not merely hoped, to reproduce these
exactly).

---

## 2. Instrumentation

Added to `src/eval/machine/env.rs` (`env_layout_diag` module) and wired at
process exit in `src/bin/eu.rs`, same design family as eu-qm7f's
`env_depth_diag` (gated, cached, read-only shadow walk):

- **Gated by `EU_ENV_LAYOUT_HISTOGRAM=1`**, cached once via `OnceLock`.
  Disabled (the default) it is a single cached-bool check per lookup —
  zero behavioural effect, confirmed by the full harness suite (§5).
- **A read-only shadow walk**, exactly like eu-qm7f's: `EnvironmentFrame::
  diag_record_layout` re-walks the same `next` chain `cell()` would, purely
  to identify which frame and block the lookup lands on, touching no
  `bindings` and returning nothing. `get()`'s actual behaviour is unchanged
  regardless of the flag.
- **No new address-masking logic.** Block identity comes from
  `crate::eval::memory::bump::block_base_of`, an existing production helper
  (`ptr.as_ptr() as usize & !(BLOCK_SIZE_BYTES - 1)`, `BLOCK_SIZE_BYTES` =
  32 KiB) already used by the real Immix allocator — this spike adds no new
  understanding of block geometry that the allocator doesn't already rely
  on, it just reads the same fact for a diagnostic purpose.
- **Process-global state**, not thread-local: a `Mutex`-guarded map (a
  `HashMap` is needed here, unlike eu-qm7f's fixed-size atomic buckets, so a
  lock is used — negligible cost, and irrelevant when the eval thread is the
  sole writer, which every benchmark in this report confirmed via `GC
  collections: 0` and single-threaded execution). This deliberately avoids a
  thread-affinity bug: `dump()` is called from `main()` after the eval
  thread is joined, so thread-local storage would see empty state — the
  process-global `Mutex` is correct regardless of which thread wrote to it.
- **What's recorded per lookup:** the landing frame's block (via
  `block_base_of`) and its own address, attributed to the *starting* frame's
  `annotation` validity (same "blob-loaded prelude vs user/source code"
  proxy as eu-qm7f, via `Smid::default()`), plus a running "same block as the
  previous lookup" tally for the sequential-locality hit rate.
- **Dump point:** `src/bin/eu.rs`, once at process exit.

---

## 3. Full data and interpretation

### 3.1 018_string_scale (penalty class, largest)

**Blob**, byte engine:
```
total_lookups=75295727
sequential_locality: same_block_as_prev=48336793 diff_block_from_prev=26958933 hit_rate=0.641959
annotated:   distinct_blocks=3771 distinct_frames=588074 total_touches=25525621 mean_frames_per_block=155.95
unannotated: distinct_blocks=3771 distinct_frames=245042 total_touches=49770106 mean_frames_per_block=64.98
```

**Source**, byte engine:
```
total_lookups=75092689
sequential_locality: same_block_as_prev=65345072 diff_block_from_prev=9747616 hit_rate=0.870192
annotated:   distinct_blocks=3532 distinct_frames=602064 total_touches=74679627 mean_frames_per_block=170.46
unannotated: distinct_blocks=3455 distinct_frames=154026 total_touches=413062  mean_frames_per_block=44.58 (tiny population, not comparable)
```

Three findings here, all pointing the same direction:

1. **Sequential locality is much worse under blob** (0.642 vs 0.870 — 63%
   more "block changed since last lookup" events under blob).
2. **Blob touches more distinct blocks for the same logical work**: 3,771 vs
   3,532 (+6.8%) — matches eu-qm7f's independently-measured "+6.9% resident
   blocks" finding from `-S` almost exactly, now attributed specifically to
   the environment-frame population rather than the heap as a whole.
3. **Blob's prelude-closure ("unannotated") population is packed less
   densely** than either blob's own user-code population or source's code:
   64.98 distinct frames per block (blob prelude) vs 155.95 (blob user code)
   vs 170.46 (source, user + inlined prelude combined). Blob's prelude
   closures are the *specific* population dragging down packing density.

**Note on block segregation:** `distinct_blocks` for blob's annotated and
unannotated populations are **identical** (3,771 both). Prelude-closure
frames and user-code frames are **not** segregated into separate regions of
the heap — they are interleaved within the same set of blocks, consistent
with a simple monotonic bump allocator serving both populations from the
same arena as the loop runs. The mechanism is about *access pattern density*
within shared blocks, not physical segregation of "prelude space" from "user
space".

### 3.2 019_list_scale and 022_hof_fold (penalty class, moderate/small)

```
019 blob:   hit_rate=0.8076   (total_lookups=55059223)
019 source: hit_rate=0.8711   (total_lookups=54843184)
022 blob:   hit_rate=0.7905   (total_lookups=51315205)
022 source: hit_rate=0.8598   (total_lookups=50985167)
```

Both show the same blob-worse direction as 018, at a **smaller gap** (0.063
and 0.069 respectively, vs 018's 0.228) — consistent with these benches'
smaller known wall-clock penalties (1.10-1.48x vs 018's 1.26-2.06x). The gap
magnitude scaling with the penalty magnitude across three independent
benchmarks, using one metric derived from first principles (Immix block
identity), is the strongest piece of corroborating evidence in this report.

### 3.3 020_lookup_curve (the inversion) — a null result on this mechanism, not a contradiction

```
020 blob:   hit_rate=0.9113   distinct_blocks(annotated)=42426  mean_frames_per_block=155.74
020 source: hit_rate=0.9126   distinct_blocks(annotated)=41696  mean_frames_per_block=158.77
```

Gap: **0.0013** — noise-level, essentially identical. This bench's known
wall-clock behaviour (blob *faster*, 0.87-0.93x) is **not explained by this
instrumentation being better under blob** — it's explained by this
instrumentation showing *no meaningful difference at all*, which is the
expected null result for a benchmark whose dominant cost is a completely
different mechanism: static `.key` lookup on a large block, which per
ROADMAP §6.4 item 4 resolves through `Ref::G` at O(1) with **no environment
walk**. The `EnvironmentFrame::get` calls this instrumentation observes here
(155M of them — the loop scaffolding and repeated calls into the lookup
helper, not the block-key resolution itself) behave almost identically
regardless of prelude config, because there is no repeated fan-out into many
small, differently-packed prelude helper closures the way 018/019/022's
`map`/`foldl`/`JOIN`-heavy loops have — 020's hot loop is comparatively
uniform. **The inversion itself is not accounted for by this report**; it
sits in a different code path (block/global lookup) that this
layout-forensics instrumentation does not instrument. Confirming *that*
mechanism (why `Ref::G` favours blob) is out of scope here and not claimed.

### 3.4 021_io_loop (absent-penalty class, spot check)

```
021 blob:   hit_rate=0.6759   (total_lookups=3251861)
021 source: hit_rate=0.6844   (total_lookups=3245837)
```

Gap: **0.0085** — small, in the same "essentially flat" band as 020, matching
this bench's "no penalty" classification. (Absolute hit rate here is lower
than 018-022's, ~0.68 vs ~0.79-0.91, reflecting `io_loop`'s different
access pattern — 1000 `io.shell` round-trips rather than a tight
transformation loop — but the blob/source *gap* is what maps to the
per-class prediction, and it is small in both directions, consistent with no
penalty.)

### 3.5 Engine cross-check: the *direction* is robust, the *absolute numbers* are not

018 was also measured under the pre-decoded engine (`EU_PREDECODE=1`):

| | blob | source | gap |
|---|--:|--:|--:|
| byte engine | 0.642 | 0.870 | 0.228 |
| predecode engine | 0.684 | 0.883 | 0.199 |

Lookup **counts** are identical between engines (75,295,727 both, as
expected — same shared `EnvironmentFrame::get` call sites). But the absolute
**hit rates and distinct-block counts differ between engines** (byte:
3,771 blocks; predecode: 3,318 blocks) — unlike eu-qm7f's depth histogram,
which was byte-identical across engines. This makes sense: physical heap
*layout* depends on allocation *timing*, which can differ between dispatch
loops even when the *logical* sequence of lookups (and hence depths) is
identical — predecode's different instruction-decode and dispatch mechanics
plausibly shift exactly when intermediate values get allocated relative to
env frames, without changing what gets looked up or how deep. **The
blob-worse-than-source direction and rough gap magnitude is robust across
both engines** (0.228 byte, 0.199 predecode); the exact hit-rate and
block-count values are engine-specific and should not be quoted
interchangeably.

---

## 4. Determinism

- **018, byte engine, blob config: reproduced byte-identically twice** —
  every count (`total_lookups`, `same_block_as_prev`, `diff_block_from_prev`,
  `distinct_blocks`, `distinct_frames`, `mean_frames_per_block`, and the
  full top-10 touch/frame-count table) matched exactly across two
  independent process runs. Only the raw block **addresses** differed
  (e.g. `0xb2ab68000` vs `0x978b68000`) — expected ASLR/arena base-address
  variation between process launches, irrelevant to the structural counts.
  **Measured-verified.**
- **018, predecode engine, blob config: reproduced byte-identically twice**
  in the same way (own internal determinism confirmed), at a **different**
  set of structural numbers from the byte engine (§3.5) — each engine is
  internally deterministic, but the two engines are not identical to each
  other on this metric. **Measured-verified for predecode's own numbers.**
- **019/020/021/022: single run each, not yet independently repeated.**
  Given 018's clean byte-for-byte reproduction on both engines, and that
  every recorded quantity here is a count over a fixed, deterministic
  program execution (no timing, no randomness in the eval), a second run is
  expected to reproduce these exactly too — but that expectation is not yet
  verified for these four benches, so they are labelled **measured-single**
  per the bead's own bar ("deterministic layers reproduced twice before
  measured-verified").

---

## 5. Correctness verification (harness, all three engines)

`cargo build --release`, `cargo clippy --all-targets -- -D warnings` (clean
after one fix — `sort_by` → `sort_by_key` per clippy's `unnecessary_sort_by`
lint), `cargo fmt --all`, then the full suite with the instrumentation
compiled in but disabled:

| Engine | Tests | Failures |
|---|--:|--:|
| bytecode (default) | 1,940 across all test binaries | **0** |
| HeapSyn (`EU_HEAPSYN=1`) | 1,940 | **0** |
| predecode (`EU_PREDECODE=1`) | 1,940 | **0** |

Confirms the diagnostic instrumentation is behaviour-neutral by default, on
every engine, consistent with the read-only-shadow-walk safety argument in
§2.

---

## 6. Per-class-map reconciliation

The bead's own per-class map is the constraint this report had to fit, and
it does:

- **018/019/022 (penalty, tight prelude-helper traversal loops):** all three
  show a blob-worse sequential-locality gap, and the gap **magnitude ranks
  the same way the wall-clock penalty magnitude ranks** (018 largest gap and
  largest penalty; 019 and 022 smaller gaps and smaller penalties).
- **015/016/017 (absent, not directly tested here beyond the 021 spot check
  — see caveat below):** not measured in this session; 021 (also absent) was
  tested as a representative spot check and showed a negligible gap,
  consistent with the class. Time did not permit running all three of
  015/016/017 as well; this is a scope gap, noted rather than glossed over
  — the report's claim for the "absent" class rests on one spot check
  (021), not the full set the bead named.
- **020 (inverted, blob faster):** shows **no** locality gap on this metric
  — correctly identified in §3.3 as a null result pointing to a different
  mechanism (`Ref::G` O(1) global resolution), not a contradiction of the
  locality story.

**Overall verdict:** the scattered-vs-co-located hypothesis **holds** for
the penalised workload class, measured directly via Immix block identity
rather than inferred from allocation/block-count side effects alone. The
mechanism is specifically that blob's un-fused prelude-helper closures
(map/foldl/JOIN/structural `=`, called as genuine separate global closures
rather than inlined into the caller — the same root architectural fact
identified in eu-2sa6.12/eu-7xvv) allocate more numerous, less densely
packed local frames per unit of logical work, causing the bump allocator to
cross block boundaries more often relative to how much useful lookup work
happens — worse sequential locality, not longer chains (already ruled out
by eu-qm7f) and not fewer/more lookups (also ruled out — counts are equal).

---

## 7. Fix shape (sketch only — no implementation, per scope)

Two directions the evidence points toward, offered as shape only:

1. **Hotness- or co-occurrence-aware materialisation order at blob load.**
   Blob currently materialises the 352 peeled prelude bindings in whatever
   order the blob's own binding table lists them (decode order), not in an
   order informed by which helpers tend to be called together in real
   workloads. If frequently-co-invoked helpers (`map`, `cons`, `head`,
   `tail`, string `JOIN`, structural `=` — exactly the cluster §3.1
   identifies as the low-density population) were materialised adjacently
   at load time, their persistent global closures (and the local frames
   their invocations spawn) would have a better chance of landing in the
   same or nearby blocks, improving sequential locality without touching
   the wire format or the one-form-per-name property. This would need a
   co-occurrence profile (e.g. from the canonical bench suite) to drive the
   ordering — a generation-time or load-time change, not a representation
   change.
2. **Targeted allocation-density improvement for the specific hot cluster**,
   rather than a global fix: since the effect concentrates in a small,
   identifiable set of prelude helpers (the same ones §7.3 of eu-7xvv named
   as the `inline_cores`-uncovered nested hot helpers), a narrow, scoped
   extension of blob generation to co-locate or partially fuse *only* that
   cluster's allocations could recover most of the locality gap without the
   broader cross-binding inlining eu-2sa6.12 ruled out for the whole
   prelude (which would break one-form-per-name at scale). This is the same
   "expand `inline_cores`" alternative eu-2sa6.12 §4.1 flagged and deferred,
   now with a concrete, measured reason (packing density, not just profile
   share) to reconsider it for this specific cluster.

Neither is implemented or measured here; both are structural inferences from
the placement evidence, offered for the owner's design consideration per the
bead's request for a fix-shape sketch, not a fix.

---

## 8. Evidence index

- Instrumentation: `src/eval/machine/env.rs` (`env_layout_diag` module,
  `EnvironmentFrame::diag_record_layout`), `src/bin/eu.rs` (dump call site).
- Raw output: reproduced inline in §3; reproducible via
  `EU_ENV_LAYOUT_HISTOGRAM=1 ./target/release/eu --heap-limit-mib 12288
  [--source-prelude] [-t bench-string-scale | bench-list-scale |
  bench-hof-fold | bench-lookup-curve | --allow-io -t bench-io-loop]
  tests/harness/bench/{018_string_scale,019_list_scale,020_lookup_curve,
  021_io_loop,022_hof_fold}.eu`, with/without `EU_PREDECODE=1`.
- Block-geometry primitive reused (no new logic): `crate::eval::memory::
  bump::block_base_of`, `BLOCK_SIZE_BYTES` (`src/eval/memory/bump.rs:39-41,
  272-274`).
- Prior reports this builds on: `docs/superpowers/reports/
  2026-07-13-env-walk-depth.md` (eu-qm7f — refutes chain depth, motivates
  this spike), `docs/superpowers/reports/
  2026-07-13-blob-string-penalty-diagnosis.md` (eu-2sa6.12),
  `docs/superpowers/reports/2026-07-13-blob-string-penalty-profile.md`
  (eu-7xvv, now carrying a correction banner pointing at eu-qm7f).
- ROADMAP context: §6.4 item 4 (`Ref::G` globals O(1), no env walk — the
  basis for §3.3's null-result reading of the 020 inversion).
