# Sequencing — a route through the twenty proposals

> **Consolidated into [`ROADMAP.md`](ROADMAP.md)** (the plan of record). This
> document remains the longer-form dependency analysis the roadmap distils; where
> the two differ on a release assignment, ROADMAP.md wins (it re-spaces these
> horizons across more minor releases, and folds in 0021 and the `0000` fixes).

- **Status:** Draft synthesis for review (superseded as the schedule by ROADMAP.md)
- **Date:** 2026-06-01
- **Reads:** [`README.md`](README.md) (the portfolio), the twenty proposals, and
  [`type-system-evolution.md`](../development/type-system-evolution.md) (the
  existing type-system roadmap these build around).

The twenty proposals are deliberately independent arguments. This document is
the connective tissue: which depend on which, what to do in which release, where
the genuinely hard forks are, and which decisions the maintainer must take before
the rest can move. It recommends a path; it does not assume one.

A reminder of the frame. 0.7 has **shipped** — it delivered HKT and the rest of
type-system Phase B (2026-06-04), and 0.6.2 delivered Stage A. The type-system
roadmap these proposals were written around is now largely *built*, which only
sharpens their point: the remaining 1.0 work is mostly the whitespace they
target. These proposals start at **0.8 onward**. Everything respects the
non-negotiables (syntactic conservatism, structural-over-nominal, the
single-threaded lazy-pure runtime, the tool-first framing).

---

## 1. The portfolio answers four questions

| Question | Proposals | The decision it forces |
|---|---|---|
| **What *is* 1.0?** (the commitment) | 0001, 0002, 0003 | What surfaces are frozen; what soundness is promised; how it is proven. |
| **Is the runtime 1.0-ready?** | 0004, 0005, 0006 — then forks 0007, 0008, 0020 | How much we care about performance (open Q4); which one big systems bet, if any. |
| **How far does the language go?** | 0009, 0011, 0013 — then forks 0010, 0012 | No-nominal (Q1), metadata-DSL (Q2), user monads (Q3), MLsub (Q5), boundary (Q6), metadata typing (Q7). |
| **What is the adoption surface?** | 0014, 0015, 0016, 0017, 0018 — then 0019 | How much tooling/ecosystem investment precedes vs follows 1.0. |

## 2. The dependency spine

The edges that actually constrain ordering:

- **0001 (editions) underpins 0002 and 0003.** Editions are how the pending
  deep-merge default ships safely; the conformance corpus (0003) is the
  *executable* form of the stable surfaces 0001 enumerates; per-edition golden
  output is a 1.0 gate.
- **0002 (boundary policy) gates 0007.** Type-directed compilation may only
  optimise on *proven-concrete* types; it cannot trust an annotation that reached
  it through `any`. So the soundness contract must be decided before the runtime
  starts relying on it.
- **0009 (contracts) is the runtime dual of 0002**, reuses the type-DSL
  vocabulary (reusing the shipped Phase-A shapes: rows, `Dict(T)`, literals), and
  feeds 0019 (an ingested schema *is* a contract).
- **0004, 0014 and TS-B7 are one family.** TS-B7 caches prelude *types*; 0004
  caches compiled *code*; 0014 generalises both into a demand-driven query graph.
  They should share a cache-key discipline; 0014 is the architecture the other
  two are point-solutions of.
- **0005 and 0020 are the same scarce resource** (deep GC expertise) and must be
  *sequenced, not parallelised*.
- **0015 Phase 1 (delete the parser shim) unblocks** better diagnostics and the
  resilient front-end that 0014 wants.
- **0011 builds on the now-shipped Phase B** (HKT-typed `monad()`, structural constraints); it is
  mostly a *position* to ratify, not new machinery.
- **0017 leans on 0004** (per-prompt latency) and **0014** (reactive notebook).
- **0019 leans on 0009, 0016, and recursive types (A3).**

## 3. Recommended release plan

Horizons are suggestions; the gates are the real content.

### 0.8 — "say what 1.0 means, and stop the bleeding on latency"
Cheap, high-leverage, low-risk. Mostly design and front-end work.

- **0001** — ratify the charter; implement edition plumbing + the first edition
  delta (deep-merge-as-default gated on `{ edition: "2026" }`).
- **0004** — compiled-unit/prelude cache. The most *visible* perf win; attacks
  the ~500–700 ms cold-compile cost directly.
- **0015 (Phase 1)** — delete the all-or-nothing parser shim; always surface the
  Rowan partial tree. ~150 lines; immediate diagnostic improvement.
- **0003 (begin)** — stand up the conformance corpus and seed it from the 315
  existing harness tests; wire `EU_GC_VERIFY=2`/`EU_GC_STRESS=1` as a CI gate.

### 0.9 — "make the runtime and the front-end 1.0-grade"
The substantive engineering release.

- **0005** — generational nursery + smarter trigger + finish the stubbed Immix
  defrag tier. The highest-impact memory work; attacks the >95 % mark cost.
- **0002** — *decide and document* the boundary policy (recommended: advisory
  default + opt-in `--strict-boundary`; bless `eu check --strict` as the CI gate).
- **0006** — strictness/demand analysis (reinforces 0005: fewer thunks → less to
  mark).
- **0013** — *decide* to keep the string type-DSL through 1.0 (A7 covers the
  tooling friction); a near-free decision that closes open Q2.
- **0016** — `eu doc` (reuses the LSP extraction already in
  `symbol_table.rs`); regenerate the hand-written prelude reference.
- **0014 (begin)** — wrap the front-end passes as queries incrementally.
- **0020** — *only if the GC budget allows after 0005*: persistent blocks via an
  inline-GC-heap CHAMP. Otherwise defer to post-1.0.

### 1.0 — "the commitment, the data-correctness story, and the ecosystem floor"

- **0009** — structural contracts / runtime validation at data ingress. The
  flagship competitive feature and the pragmatic resolution of the boundary
  tension.
- **0018 (core cut)** — integrity-hashed imports + an eucalypt-syntax manifest +
  namespace isolation. (Registry deferred.)
- **0017 (Phase 1)** — `eu watch` (Unison-style); a thin REPL over the cache.
- **0003 (bar met)** — per-edition golden corpus green; property tests and the
  three fuzz targets running.
- **Type-system Stage A/B** already shipped (0.6.2 / 0.7.0); **0011**
  ratified as the standing position on ad-hoc polymorphism.

### post-1.0 — the big bets and the breadth
Pick deliberately; see §4. **0007** (type-directed compilation), **0008**
(parallel evaluation), **0010** (capability/determinism *types*; the *lint* can
come earlier), **0012** (MLsub — probably never), **0019** (host-language
codegen; JSON-Schema interop could be pulled toward 1.0), **0018** (a registry),
**0014** (incremental *evaluation*, beyond analysis).

## 4. The competing "big rebuilds"

Five proposals each want a sustained, expert, deep-systems effort, and they draw
on overlapping skills (the GC, the core checker, the STG machine). **They cannot
all be done at once, and most are post-1.0.**

| Fork | What it buys | Recommendation |
|---|---|---|
| **0005** generational GC | Cuts the dominant mark cost; benefits *every* program | **Do this one.** Highest value, pre-1.0, no policy prerequisite. |
| **0020** persistent blocks / GC finalisation | O(log n) blocks for large/merge-heavy configs | Second GC item; only after 0005, and only if O(n) blocks demonstrably bite. |
| **0007** type-directed compilation | 5–10× on typed numeric code; types become a runtime asset | Post-1.0; gated on 0002 and on caring about perf (Q4). |
| **0008** parallel evaluation | Multi-core for big maps/multi-doc renders | Post-1.0, furthest out; needs a Send/Sync heap — a GC rebuild in its own right. |
| **0012** MLsub core swap | Principal types; rows/unions/constraints "for free" | **Probably never** — conflicts with the now-shipped HKT. Reassess only if the hand-rolled core visibly creaks. |

The throughline: **invest the scarce systems effort in the GC (0005, then maybe
0020) before 1.0**, and treat 0007/0008/0012 as mutually-exclusive post-1.0 bets
to be chosen one at a time.

## 5. Cheap wins vs big bets

| Leverage / cost | Proposals |
|---|---|
| **Cheap & high-leverage** (do early) | 0004, 0015, 0016, 0013, 0001 (design-heavy, not code-heavy) |
| **High-leverage, real engineering** | 0005, 0020, 0003, 0009, 0014, 0018, 0002 |
| **Worthwhile, medium** | 0006, 0011, 0017 |
| **Big bets / forks** (deliberate, mostly post-1.0) | 0007, 0008, 0010, 0012, 0019 |

## 6. Decisions the portfolio forces

These are the maintainer's calls; the proposals argue them but do not pre-empt
them. Several are the open questions from `type-system-evolution.md` §5.

1. **Do we commit to 1.0 stability at all, and via editions?** (0001) —
   everything in Track A assumes yes.
2. **What soundness does 1.0 promise?** (0002, open Q6) — recommended:
   optimistic/advisory by default, opt-in checks; *not* whole-program sound casts.
3. **Is "no nominal" inviolable?** (0011, open Q1) — recommended: yes, stay
   structural; a nominal newtype would itself be edition-gated (0001).
4. **Keep the string type-DSL?** (0013, open Q2) — recommended: yes through 1.0.
5. **How much do we care about runtime perf?** (open Q4) — determines whether
   0007 is ever in scope, and how much of 0005/0006 lands pre-1.0.
6. **Which big rebuild, if any, post-1.0?** (§4) — 0007 vs 0008 vs 0012, plus
   the 0005→0020 GC line.
7. **How much ecosystem before 1.0?** — the 0018 core and 0009 are recommended
   *in* 1.0; the registry (0018) and host-language codegen (0019) are after.

## 7. The critical path, in one line

**0001 (what 1.0 means) → 0004 + 0015 (stop the obvious pain) → 0005 (fix the
runtime) → 0002 (decide the contract) → 0009 + 0018 (data-correctness +
ecosystem floor) → 0003 green (prove it) → ship 1.0.** Everything else is either
a cheap win that slots alongside, or a post-1.0 bet to be chosen with eyes open.

## 8. Bottom line

The type-system future is already well-specified; the work this portfolio adds is
mostly **outside** it — defining the 1.0 commitment, paying down the runtime's
latency and GC costs, giving the config use case a runtime-validation and
reproducibility story, and building the tooling/ecosystem surface that adoption
needs. The single most important sequencing judgement is **§4**: guard the scarce
deep-systems effort, spend it on the GC before 1.0, and keep the three glamorous
rebuilds (type-directed compilation, parallelism, MLsub) as disciplined,
one-at-a-time, post-1.0 choices.
