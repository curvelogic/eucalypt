# n8c5e blob-vs-source prelude perf spike: the un-inlined blob is both a material perf cost and (very likely) the mechanism behind eu-e3c3i's named-function-HOF quadratic

- **Date:** 2026-07-24
- **Bead:** eu-n8c5e — profiling spike, analysis only, no xtask/prelude/engine
  code changes. Phase 1 of the Stopwatch two-phase workflow.
- **Companion bead:** eu-e3c3i (EF2 ingestion spike, `stopwatch-ef2`,
  `docs/superpowers/reports/2026-07-24-ef2-ingestion-profile.md`, commit
  `dcb886be`, not yet on this worktree's `master` — content reproduced from
  that commit for reference in §2).
- **Worktree/commit:** `.claude/worktrees/agent-ad8c7ab80d845105e`, `master`
  at `694784ce`.
- **Toolchain:** rustc 1.97.0 (`stable-aarch64-apple-darwin`), Darwin 25.5.0
  arm64.
- **Build provenance:** `cargo clean`, `cargo build --release`, `cargo run -p
  xtask --release -- prelude-compile` (fresh `lib/prelude.blob`, 598,731
  bytes), then a second `cargo build --release` (confirmed "Compiling
  eucalypt" — genuine re-embed). One binary, one blob; `EU_SOURCE_PRELUDE=1`
  forces the full source-compiled prelude path on the *same* binary (see
  `src/driver/eval.rs:32-38`, `src/driver/prepare.rs:34-46`) — this is the
  cleanest possible blob-vs-source A/B, strictly stronger than the protocol's
  "one binary, one blob" bc/hs rule since here even the *prelude* artefact
  selection is a single env var on one binary.

## 0. Protocol-compliance disclosure — machine was not quiet

`uptime` load average was **4.8–13.0** throughout this session (multiple
other agents active in this session's worktrees, per the coordinator's
dispatch). This is well above PROTOCOL §2's load<1 precondition. Accordingly:

- All **ticks/allocation counts** below (read via `EU_HEAPSYN=1 -S`) are
  **measured-verified** per protocol §5 — the deterministic layer is immune
  to load, and every one of the curves reported here is either machine-exact
  (linear ×2.00 per doubling, five points) or has a clean, near-monotonic
  approach to the ×4 quadratic prediction. This is the primary evidence.
- The **one wall-clock figure** in §4 is reported as context only, capped at
  **measured-single at best** (in practice, given load 5.6–8.0 at the time,
  even that label is generous) — it corroborates the tick finding but is not
  load-bearing on its own.

## 1. One-line verdict

**Fixing eu-n8c5e is a genuine perf win, not merely diagnostics-only — and
the ticks strongly implicate it as (at minimum, a necessary and currently
sufficient) cause of eu-e3c3i's named-function-HOF quadratic**: with the full
pipeline running over the prelude (source mode, `EU_SOURCE_PRELUDE=1`, on
*today's* master, no fix needed to observe it), `count`/`sum`/`filter` scale
perfectly linearly (×2.00 per doubling, N=1,000→16,000) where the shipped
blob is quadratic (×3.4→×3.9 per doubling, converging on the ×4 quadratic
prediction). This is not confined to the narrow bench-022 shape — it holds
generally, and it correlates cleanly with prelude-combinator density across
the canonical suite. n8c5e and eu-2sa6.18 are related but **not** a hard
gate on each other — n8c5e's fix (run inline/demand/reflatten inside xtask's
per-binding prelude compile) does not obviously require eu-2sa6.18's broader
"generalise blob-form compilation to arbitrary units" architecture; it looks
implementable as a narrower, targeted xtask change. CG4 (eu-gmdl5) and BV3
(eu-2sa6.2) remain the *general* fix for user-authored recursive HOFs and
cross-function-call env-walk cost outside the prelude — n8c5e's fix does not
subsume them, but it plausibly eliminates the *specific*, currently
user-visible eu-e3c3i symptom (prelude `count`/`sum`/`filter`/`map`, hence
`str.len`) for the default (blob) product experience, because the mechanism
that fixes it in source mode already exists on master (`tag_combinators` /
copy-specialisation, `src/core/inline/tag.rs`) and simply never runs over
blob-compiled prelude bindings today.

## 2. Method

### 2.1 Canonical suite: blob vs source, ticks (Q1)

Ran all eight canonical benches (`tests/harness/bench/015..022`) under
`EU_HEAPSYN=1 -S --heap-limit-mib 12288` (plus `--allow-io` for 021), once in
default (blob) config and once with `EU_SOURCE_PRELUDE=1` (source config),
same binary/blob, mirroring `xtask/src/engine_ab.rs`'s exact invocation shape
minus the wall-timing loop (ticks only, since ticks are the load-independent
layer). Confirmed byte-identical rendered output blob vs source for
`019_list_scale` (no behavioural change, perf only).

### 2.2 Named-function-HOF scaling: blob vs source (Q2)

Built a small synthetic (`n: <N>` + four `:target`s: `bench-count`,
`bench-sum`, `bench-filter-count`, `bench-foldl-lit`) at N ∈
{1000,2000,4000,8000,16000}, run under the same `EU_HEAPSYN=1 -S
--heap-limit-mib 12288` harness, blob vs `EU_SOURCE_PRELUDE=1`:

```eu
n: <N>
` :target
bench-count: range(0, n) count
` :target
bench-sum: range(0, n) sum
` :target
bench-filter-count: range(0, n) filter(> (n / 2)) count
` :target
bench-foldl-lit: range(0, n) foldl((_+_), 0)
```

`bench-foldl-lit` is the bench-022 shape (literal lambda at the user's own
call site) — the control showing where PR #1010/#1016/#1008's
copy-specialisation already applies regardless of blob/source, per Q3.

### 2.3 Mechanism check (`eu dump stg`)

Dumped compiled STG for a minimal `count` caller in both configs to see
*what the compiler actually ships*, not just the Core-level `dump inlined`
view (which, in blob mode, reconstructs from blob-embedded
`desugared_unit_cores` and can be misleading about what's truly compiled).

## 3. Results

### 3.1 Canonical suite ticks, blob vs source (measured-verified)

| Bench | blob ticks | source ticks | blob excess | Attribution |
|---|--:|--:|--:|---|
| 015_block_merge | 96,128,347 | 85,326,774 | **+12.7%** | block/record combinators |
| 016_import_export_yaml | 56,113,902 | 31,750,007 | **+76.7%** | reshape via prelude combinators |
| 017_import_export_toml | 55,940,578 | 31,576,627 | **+77.2%** | reshape via prelude combinators |
| 018_string_scale | 46,044,700 | 45,699,853 | **+0.8%** | native interpolation/concat — control |
| 019_list_scale | 51,219,477 | 32,871,336 | **+55.8%** | `range→map→sum` |
| 020_lookup_curve | 1,500,879 | 1,395,844 | **+7.5%** | static block lookup |
| 021_io_loop | 3,971,302 | 3,038,310 | **+30.7%** | `io.shell` loop combinator |
| 022_hof_fold | 87,500,411 | 66,500,321 | **+31.6%** | bench-022's literal-lambda foldl |

**This answers Q1 directly: yes, materially, and the size of the delta
tracks prelude-combinator density in the workload.** `018_string_scale` —
almost entirely native string intrinsics, minimal prelude combinator use —
shows essentially **zero** delta (0.8%, noise-band). `016`/`017` (heavy
block-reshape via prelude combinators) and `019` (a `map`/`sum` pipeline)
show 56–77%. This is the clean control that rules out "blob vs source is
just generally different for unrelated reasons" — the delta is
attributable to prelude inlining, not some blanket blob-path effect.

### 3.2 Named-function-HOF scaling curves (measured-verified)

| N | bench-count blob | bench-count source | bench-sum blob | bench-sum source | bench-filter-count blob | bench-filter-count source | bench-foldl-lit blob | bench-foldl-lit source |
|--:|--:|--:|--:|--:|--:|--:|--:|--:|
| 1,000 | 710,723 | 124,373 | 722,729 | 164,339 | 911,957 | 220,204 | 218,941 | 166,351 |
| 2,000 | 2,421,223 | 248,623 | 2,445,229 | 328,589 | 3,073,707 | 440,329 | 437,691 | 332,601 |
| 4,000 | 8,842,223 | 497,123 | 8,890,229 | 657,089 | 11,147,207 | 880,579 | 875,191 | 665,101 |
| 8,000 | 33,684,223 | 994,123 | 33,780,229 | 1,314,089 | 42,294,207 | 1,761,079 | 1,750,191 | 1,330,101 |
| 16,000 | 131,368,223 | 1,988,123 | 131,560,229 | 2,628,089 | 164,588,207 | 3,522,079 | 3,500,191 | 2,660,101 |

Per-doubling ratio (ideal linear = ×2.00, ideal quadratic = ×4.00):

| Function | blob ratios (1k→2k→4k→8k→16k) | source ratios |
|---|---|---|
| `count` | 3.41, 3.65, 3.81, 3.90 (→ quadratic) | 2.00, 2.00, 2.00, 2.00 (**exactly linear**) |
| `sum` | 3.38, 3.64, 3.80, 3.89 (→ quadratic) | 2.00, 2.00, 2.00, 2.00 (**exactly linear**) |
| `filter`+`count` | 3.37, 3.63, 3.79, 3.89 (→ quadratic) | 2.00, 2.00, 2.00, 2.00 (**exactly linear**) |
| `foldl` (literal, user call site) | 2.00, 2.00, 2.00, 2.00 (linear in **both**) | 2.00, 2.00, 2.00, 2.00 |

**This answers Q2 directly and unambiguously: the quadratic is present in
blob and ABSENT in source, for every one of `count`/`sum`/`filter` — the
exact prelude functions eu-e3c3i names.** Blob-mode `count`/`sum`/
`filter+count` all converge toward ×4 (true O(n²)) as N grows; source-mode
scaling is *exactly* ×2.00 at every step (O(n), to measurement precision).
No behavioural difference — both configs render `16000`/the same numeric
results for every N tested.

### 3.3 Copy-specialisation control (Q3)

`bench-foldl-lit` — the bench-022 shape, literal lambda written directly at
the user's own call site — is **linear in both blob and source**, with a
**constant, N-independent** blob-excess of **31.6%** at every N from 1,000
to 16,000 (218,941 vs 166,351 at N=1,000; 3,500,191 vs 2,660,101 at
N=16,000 — both exactly 31.6%). This constant-factor gap matches
`022_hof_fold`'s canonical-suite blob-excess (**31.6%**, §3.1) almost to the
decimal — a clean independent cross-check that the two measurements are
seeing the same mechanism (a residual per-call opaque-`Ref::G` tax that
survives even where copy-specialisation has already collapsed the
complexity class).

**Answering Q3 directly: yes — the blob-vs-source delta shrinks from a
complexity-class difference (quadratic vs linear, up to 6,500% at N=16,000)
down to a flat ~31.6% constant-factor tax, exactly where copy-specialisation
already applies (the literal-lambda-at-user-call-site shape).** It persists
unshrunk (and complexity-class-altering) for `count`/`sum`/`filter`, where
the qualifying literal-lambda-foldl shape sits inside the *prelude's own*
source, not the user's call site — so copy-specialisation's compile-time
recognition of it never gets a chance to run in blob mode.

### 3.4 Mechanism (`eu dump stg`, why source is linear)

For a minimal `n: 5` / `bench-count: range(0,n) count` program:

- **Blob mode STG dump is 9 lines total** — the user's compiled unit
  contains only `bench-count = ⊗448(⊗447(...))`-shape calls to opaque global
  refs (`⊗447`/`⊗448`). `count`'s and `foldl`'s bodies are **not present** in
  this unit at all — they live as separately precompiled globals baked into
  the blob, invisible to (and unreachable by) any further optimisation pass
  run over the user's unit.
- **Source mode STG dump is 1,007 lines** — `foldl`'s self-recursive,
  4-way-unrolled body is fully inlined as local closures (`✳`-numbered local
  refs, not `⊗`-numbered globals) directly inside the compiled unit,
  including nested nullary/unary/binary specialisations. This is precisely
  the shape `src/core/inline/tag.rs`'s `tag_combinators` pass (structural:
  self-recursive, closed body, ≤48 nodes, applies its own parameter —
  *not* a hardcoded name list) is built to recognise and specialise — and it
  runs as part of the standard inline pipeline, which blob-compiled prelude
  bindings skip entirely (bead eu-n8c5e's core factual claim, now confirmed
  at the STG level, not just the `dump inlined` Core level bead eu-7x0r
  originally used).

This is the causal mechanism: `count(l): foldl({...}, 0, l)` (and `sum`,
and `filter`'s `foldr`-based body) each contain, **inside the prelude's own
source**, exactly the self-recursive-combinator-with-closed-body shape that
`tag_combinators` targets. Source-mode compilation runs the full pipeline
over `lib/prelude.eu` (merged with user code) and this pass fires on
`foldl`'s own definition, collapsing it to O(n). Blob-mode compilation
(xtask, per eu-n8c5e) compiles each peeled prelude binding straight from
COOKED, **skipping the inline pass entirely** — so `tag_combinators` never
sees `foldl`'s body, and `count`/`sum`/`filter` ship in the blob exactly as
un-specialised, O(n²) global forms. This is a structural, general-purpose
compiler pass already on master, not something that needs building — n8c5e's
fix is "let it run over the prelude too."

## 4. Wall-clock corroboration (measured-single at best; load 5.6–8.0)

`bench-count` at N=16,000, bytecode engine, `--heap-limit-mib 12288`, 3 runs
each, un-interleaved (context only, not a protocol-compliant comparative
figure):

- **blob:** 3.95s, 4.16s, 5.93s
- **source:** 0.11s, 0.11s, 0.10s

A ~40–50× wall-clock gap at this one N. The effect size is far larger than
plausible load noise (within-group spread is small relative to the
between-group gap), so it corroborates the tick finding, but per protocol it
does not itself gate any claim — the ticks in §3.2 are the load-bearing
evidence.

## 5. Gating and relationship to other beads

- **Not gated on eu-2sa6.18.** eu-2sa6.18 is the *architectural* end-state
  ("generalise blob-form unit compilation... to arbitrary units... connecting
  to BV5 unit cache") — a much larger, explicitly-deferred piece of work
  (owner-reframed 2026-07-13, P3, no active work). n8c5e's fix as scoped
  ("make xtask run inline (+ demand/reflatten) before STG-compiling each
  peeled binding") only needs to apply the existing inline pipeline
  (including `tag_combinators`) to `lib/prelude.eu`'s own peeled bindings
  during `xtask prelude-compile` — it does not require making blob-form
  compilation generic over arbitrary units first. It is a narrower, more
  targeted change than eu-2sa6.18's scope, and eu-2sa6.18's own text treats
  the two as related-but-distinct ("connecting to eu-2sa6.18" in n8c5e's
  description reads as a cross-reference, not a dependency edge — there is
  no `DEPENDS ON`/`BLOCKED BY` link between them in beads).
- **Does not subsume CG4 (eu-gmdl5) / BV3 (eu-2sa6.2).** Those beads target
  a *different, broader* population: user-authored recursive HOFs and
  general cross-function-call env-walk cost *outside* the prelude (e.g.
  AoC day07's hand-written beam-fold, or sibling-helper calls inside a
  user's own operator functions) — populations `tag_combinators` cannot
  reach by construction (it only fires within a single compiled unit's own
  source, and only on structurally-qualifying self-recursive combinators).
  n8c5e's fix is specific to the prelude's *own* combinators shipping in
  blob form un-specialised; it is the cheaper, narrower, already-mechanism-
  exists fix for exactly the population eu-e3c3i complains about
  (`count`/`sum`/`filter`/`map`, hence `str.len`), not a substitute for
  CG4/BV3's larger scope.
- **Causation, stated carefully:** what is *directly measured* is a clean
  config-level causal A/B (only the prelude compilation path changes; same
  binary, same user code, byte-identical output) showing the complexity
  class flips from O(n²) to O(n). What is *inferred, not directly measured*
  is that implementing eu-n8c5e's specific proposed fix (add
  inline/demand/reflatten to xtask's per-binding prelude compile) would
  reproduce this source-mode result inside the shipped blob — this is
  well-supported (the mechanism identified in §3.4 operates purely within
  `lib/prelude.eu`'s own peeled-binding bodies, which is exactly what
  xtask's fix would newly process) but has not been verified by actually
  building the fix, which is out of scope for this analysis-only phase.

## 6. Recommendation to the coordinator

Fixing eu-n8c5e (making xtask's per-binding prelude compile run
inline/demand/reflatten, at minimum including the `tag_combinators` pass)
is recommended as real, scoped, implementable work — not diagnostics-only,
and not blocked on eu-2sa6.18. It plausibly eliminates eu-e3c3i's
user-facing symptom (`count`/`sum`/`filter`/`map`, hence `str.len`'s hang on
large strings) for the **default** blob-based product experience, since the
mechanism that already does this (source mode, on master, today) requires
no new compiler work — only extending an existing pipeline stage to a path
that currently skips it. Recommended next step: a Phase-1 scoping spike
(separate from this report) on exactly what `cargo xtask prelude-compile`
needs to change, followed by owner approval before implementation, per the
two-phase workflow — implementation should re-run this same blob-vs-source
tick comparison as its before/after evidence, plus the full canonical suite
(§3.1) to confirm no regression on the benches that already look fine, and a
diff of shipped blob size/compile time.

## 7. Artefacts

- Ticks: `/private/tmp/.../scratchpad/bench_ticks.tsv` (canonical suite),
  `/private/tmp/.../scratchpad/scale_ticks.tsv` (count/sum/filter/foldl-lit
  scaling curves) — scratchpad, not checked in; raw data reproduced in
  tables above.
- Synthetic programs: `scale_1000.eu` … `scale_16000.eu`, `mini.eu` —
  scratchpad, not checked in; full source given in §2.2.
- STG dumps: `mini_blob_stg.txt` (9 lines), `mini_source_stg.txt` (1,007
  lines) — scratchpad, not checked in.
- No source, xtask, or prelude files changed. No new intrinsics proposed.
