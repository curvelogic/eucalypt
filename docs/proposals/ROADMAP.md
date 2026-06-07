# Eucalypt ROADMAP — the road to 1.0 (and just beyond)

- **Status:** Plan of record (draft for review)
- **Date:** 2026-06-07
- **Baseline:** eucalypt **0.7.0** (type-system Stage A shipped 0.6.2; Stage B +
  HKT shipped 0.7.0)
- **Consolidates:** the proposal portfolio [`README.md`](README.md), the
  [`SEQUENCING.md`](SEQUENCING.md) analysis, and the priority-fix backlog
  [`0000-priority-fixes.md`](0000-priority-fixes.md) into a single release plan.

This is the **plan of record**. The numbered proposals (`0001`–`0021`) remain the
detailed rationale — the *why* and the *how* of each item; `0000` is the
fix backlog (the `F`-items that proposals depend on). This document is the *what,
in what order, and why then*. Where a release assignment here differs from a
proposal's own "Suggested horizon", **this document wins** — the per-proposal
horizons were each argued in isolation; this re-spaces them across the release
series as a whole.

## The frame

0.7 shipped the type system the earlier roadmap was built around (HKT, rows,
`Dict`, recursive types, literal types, `Partial(T)`, structural operator
constraints). The remaining road to 1.0 is therefore mostly *outside* the type
system: defining what 1.0 commits to, paying down compile latency and GC cost,
giving the config use case a validation-and-reproducibility story, and building
the tooling/ecosystem surface adoption needs.

**There is no decimal constraint.** The 0-series can run **0.8, 0.9, 0.10, 0.11,
0.12 …** for as many minor releases as the work needs. This roadmap uses that
headroom deliberately: the scarce deep-systems effort (the GC especially) is the
binding constraint, so each release carries **at most one** big-systems thrust
plus cheaper wins, rather than overloading a single milestone.

Everything honours the **non-negotiables** ([`_house-style.md`](_house-style.md)
§1): syntactic conservatism, structural-over-nominal, gradual/inference-first,
the single-threaded lazy-pure runtime, the tool-first framing, UK English.

## The six families (what groups with what, and why)

The releases group along six commonalities. These are the real reason items sit
together.

| Family | Members | The shared thing |
|---|---|---|
| **Cross-unit interface** | F2, **F3**, 0004, 0014, 0018 | One *Unit Interface* (F3) seeds every phase's cross-unit contribution; separate compilation, incremental queries and module imports all consume it. |
| **Caching / incremental** | TS-B7 (shipped), 0004, 0014 | Type-cache → code-cache → demand-driven query graph; one cache-key discipline. 0014 is the architecture the other two are point-solutions of. |
| **GC / memory** | **0005**, 0020, F6 | Generational nursery first; then persistent blocks (CHAMP) and vec-of-values, both on the GC-scannable-array machinery. One scarce skill set — *sequence, never parallelise*. |
| **Strictness / demand** | F4, 0006, 0007 | One demand annotation (F4) feeds the analysis (0006) feeds type-directed compilation (0007). |
| **Types-as-validation** | 0013, 0009, 0021, 0016, 0019 | The `s"…"` type-DSL vocabulary is reused by runtime validation, presence-typed fields, doc/schema extraction and schema interop. |
| **1.0 commitment** | 0001, 0002, 0003, 0010 | Editions, boundary policy, conformance corpus, reproducibility — what "1.0" *means* and how it is *proven*. |

## The dependency spine

The edges that actually constrain ordering (`→` = "must precede"):

- **F3** (Unit Interface) **→ 0004, 0014, 0018**; and F3 **subsumes F2**.
- **F4** (demand annotation) **→ 0006**.
- **F5** (git import backend) **→ 0018**.
- **0001** (editions) **→ 0002, 0003** (editions are how stable surfaces freeze).
- **0002** (boundary policy) **→ 0007** (type-directed compilation may only trust
  *proven-concrete* types).
- **0005** (generational GC) **→ 0020** (same scarce GC expertise; 0005's nursery
  also makes CHAMP's short-lived nodes cheap).
- **0013** (`s"…"` type-DSL) **→ 0009, 0016** (the shared vocabulary).
- **0021** (presence) **→ 0019**; **enriches 0009** (`match?`/`me?` optional arm).
- **0009, 0016 → 0019** (an ingested schema *is* a contract; doc extraction feeds
  codegen).
- **0004 → 0017, 0008-A**; **0014 → 0017** (per-prompt latency + reactivity).
- **F6** (vec) shares 0020's GC-array machinery but is otherwise **independent**.

## The release plan

Each release is a theme, a set of items (one line each, with the gating
dependency), and a **gate** — what "done" means.

### Foundations — the `0000` fixes feed the releases

| Fix | Lands | Unblocks |
|---|---|---|
| **F1** Eliminate the double STG compile of plain documents | 0.8 | cheap latency win (P1) |
| **F2** Block-bracket content-mode across imports | 0.8 | *(via F3)* |
| **F3** Unify cross-unit contributions into a Unit Interface | 0.8 | 0004, 0014, 0018; fixes F2 |
| **F4** One demand annotation behind the strictness heuristics | 0.9 | 0006 |
| **F5** Restore git imports (fetch-backend-under-a-hash) | 0.10 | 0018 (P1; interim docs fix already shipped) |
| **F6** Generalise `vec` to arbitrary values (`Array<Closure>`) | 0.11 | pairs with 0020 (P2, conditional) |

### 0.8 — "Define 1.0; cheap latency & diagnostics"
*Design-heavy, front-end, low-risk. Say what 1.0 means and stop the obvious pain.*

- **0001** — ratify the charter; edition plumbing + first edition delta
  (deep-merge-as-default gated on `{ edition: "2026" }`).
- **0015 (Phase 1)** — delete the all-or-nothing parser shim; always surface the
  Rowan partial tree (~150 lines; immediate diagnostic win).
- **0011** — ratify "typeclasses without classes" as the standing position on
  ad-hoc polymorphism (built on shipped Phase B; mostly a position, not new code).
- **0003 (begin)** — stand up the conformance corpus from the existing harness;
  wire `EU_GC_VERIFY=2`/`EU_GC_STRESS=1` as a CI gate.
- **F1**, **F3** (with **F2**) — one compile per plain document; one cross-unit
  interface mechanism instead of four.

**Gate:** the 1.0 commitment is written down; editions work; a partial parse tree
is always available; CI runs under GC verification.

### 0.9 — "Compile latency, authoring & incremental groundwork"
*Cheap-to-medium. Attack the most *visible* perf problem and the authoring surface.*

- **0004 (prelude floor)** — prelude/compiled-unit cache (on **F3**); attacks the
  ~500–700 ms cold-compile cost head-on.
- **0014 (begin)** — wrap the front-end passes as queries (on **F3**).
- **0013** — *decide* to keep the `s"…"` string type-DSL through 1.0 (near-free;
  underpins 0009 and 0016).
- **0016** — `eu doc`: documentation & schema extraction (reads `s"…"`);
  regenerate the hand-written prelude reference.
- **0015 (Phase 2)** — parser error-recovery / resilient front-end (bracket
  recovery couples to F2).
- **F4** — the demand annotation (unblocks 0006).

**Gate:** cold-compile latency materially down; `eu doc` regenerates the prelude
reference; the front-end is queryable; one demand annotation drives the
thunk/update decisions.

### 0.10 — "The runtime: GC & the soundness contract"  *(big-systems thrust #1)*
*The substantive engineering release. One major GC effort, plus its multipliers.*

- **0005** — generational nursery + smarter trigger + finish the stubbed Immix
  defrag tier. The highest-impact memory work (>95 % of traversal-heavy time is
  mark cost).
- **0006** — strictness/demand analysis (on **F4**); fewer thunks → less to mark,
  reinforcing 0005.
- **0002** — *decide and document* the boundary policy (recommended: advisory
  default + opt-in `--strict-boundary`; bless `eu check --strict` as the gate).
  Gates 0007.
- **F5** — restore git imports as a fetch backend beneath a content hash (the
  foundation 0018 builds on).

**Gate:** mark cost down via the nursery; the soundness contract is decided and
documented; git imports work, pinned by commit and verified by content hash.

### 0.11 — "Blocks, value model & presence types"  *(big-systems thrust #2)*
*The second GC item — only after 0005 — plus the value-model and type enrichments.*

- **0020** — persistent O(log n) blocks via an inline-GC-heap **CHAMP key-index +
  order sequence** (after **0005**; only if O(n) blocks demonstrably bite
  large/merge-heavy configs).
- **F6** — generalise `vec` to hold arbitrary values (`Array<Closure>`; shares
  0020's GC-scannable-array machinery).
- **0021** — optional (presence-annotated) record fields, `name?: T` (annotated
  form; presence *inference* deferred post-1.0). Enriches 0009; unblocks 0019.

**Gate:** large/merge-heavy blocks are O(log n) with no GC regression; `vec` holds
records/lists; `name?: T` checks and renders.

### 0.12 — "Data-correctness & reproducibility"
*The config story that distinguishes a 1.0 data tool.*

- **0009** — structural contracts / spec / runtime validation at data ingress
  (uses `s"…"` from 0013; enriched by 0021). The flagship competitive feature and
  the pragmatic resolution of the boundary tension.
- **0010** — hermetic mode for reproducible rendering (capability *types* are
  **won't-do**; the determinism *mode* is the keeper).

**Gate:** ingress validation with clear blame; byte-reproducible renders under
hermetic mode.

### 1.0 — "Ecosystem floor, prove it, freeze it"

- **0018 (core cut)** — integrity-hashed git imports + an eucalypt-syntax manifest
  + namespace isolation (on **F5**, **F3**). *Registry deferred — see Non-goals.*
- **0017 (Phase 1)** — `eu watch` (Unison-style) + a thin REPL over the cache (on
  **0004**, **0014**).
- **0003 (bar met)** — per-edition golden corpus green; property tests and the
  fuzz targets running.
- **0001** — stable surfaces frozen per edition; `requires` guard enforced.
- **0011** — standing position ratified in the stability docs.

**Gate:** the 1.0 commitments hold; conformance is green; modules are hermetic and
shareable by git URL + hash, with no registry.

### post-1.0 — breadth and the big bets *(chosen one at a time)*

- **0004 (full)** — separate compilation + general unit cache (the rest of 0004,
  on the F3 interface).
- **0014 (full)** — incremental *evaluation*, beyond front-end analysis.
- **0019** — host-language & schema interop (on 0009, 0016, 0021). *The
  JSON-Schema import/export core is 1.0-adjacent and may be pulled forward.*
- **0017 (full)** — the full notebook surface.
- **0021 (inference)** — presence *inference* (beyond the annotated form).
- **0018 (depth)** — deeper version selection / lockfile closure (**not** a
  registry).
- **The mutually-exclusive forks** (deliberate, one at a time):
  - **0007** type-directed compilation (gated on 0002; 5–10× on typed numeric
    code).
  - **0008** parallel evaluation — *A* isolated workers (on 0004), then *B*
    shared-heap (needs a `Send`/`Sync` GC rebuild; maybe never).
- **alternative-backends** (core→WASM and friends) — the consumer of 0003's
  conformance contract; forthcoming as its own proposal.

## The GC line, and the glamorous rebuilds

Five efforts each want sustained, expert, deep-systems work drawing on the same
overlapping skills (GC, the core checker, the STG machine). **They cannot all
happen at once, and most are post-1.0.**

| Effort | Buys | Verdict |
|---|---|---|
| **0005** generational GC | cuts the dominant mark cost; benefits *every* program | **Do it — 0.10.** Highest value, pre-1.0, no policy prerequisite. |
| **0020** persistent blocks | O(log n) for large/merge-heavy configs | **0.11, after 0005**, only if O(n) blocks bite. |
| **0007** type-directed compilation | types as a runtime asset | post-1.0; gated on 0002 and on caring about perf. |
| **0008** parallel evaluation | multi-core for big maps/multi-doc | post-1.0, furthest out; *B* is a GC rebuild in itself. |
| **0012** MLsub core swap | principal types "for free" | **Won't-do** — collides with the shipped HKT keystone. |

The throughline: **spend the scarce systems effort on the GC (0005, then maybe
0020) before 1.0**, and treat 0007/0008 as mutually-exclusive post-1.0 bets taken
one at a time.

## Non-goals (won't-do)

Recorded so they are not re-litigated:

- **A package registry.** All distribution goals are met with **git + GitHub /
  GitLab** alone (content-addressed imports, manifest, version selection). No
  index, no hosted registry. (0018)
- **Capability *types*.** The hermetic/determinism *mode* (0010) ships; a
  capability *type system* does not — too much machinery for the tool-first remit.
- **Algebraic subtyping / MLsub** (0012). The hand-rolled bidirectional checker
  plus shipped HKT is the committed core; an MLsub swap conflicts with HKT and is
  reassessed only if the current core visibly creaks.
- **Nominal types / classes.** Stay structural. Any nominal newtype, if ever,
  would itself be edition-gated (0001).

## The critical path, in one line

**0001 (what 1.0 means) + F1/F3 (cheap latency + the interface) → 0004 (latency) →
0005 (the runtime) → 0002 (the contract) → 0009 + 0018 (data-correctness +
ecosystem floor, on F5) → 0003 green (prove it) → ship 1.0.** Everything else is
a cheap win that slots alongside, or a post-1.0 bet taken with eyes open.

## Decisions still open (maintainer's calls)

Several of `type-system-evolution.md` §5's questions are now resolved in this
plan (registries, MLsub, capability types: won't-do; `s"…"` kept; structural
stays). The live ones:

1. **How much runtime perf before 1.0?** — determines how much of 0005/0006 lands
   in 0.10 and whether 0007 is ever in scope.
2. **Which post-1.0 big bet, if any?** — 0007 vs 0008 (and the 0005→0020 GC line
   within pre-1.0).
3. **Does 0019's JSON-Schema core pull into 1.0?** — it is 1.0-adjacent; pulling
   it in widens the 1.0 interop story at the cost of scope.
4. **Are 0020 + F6 worth their GC risk for 1.0?** — yes only if large/merge-heavy
   configs *and* random-access-over-records are target classes; otherwise both
   defer post-1.0 cleanly.

## Index — every item, where it lands

| # | Item | Family | Release | Notes |
|---|------|:------:|:-------:|-------|
| F1 | Eliminate double STG compile | caching | 0.8 | P1 |
| F2 | Bracket content-mode across imports | x-unit | 0.8 | via F3 |
| F3 | Unit Interface | x-unit | 0.8 | foundation for 0004/0014/0018 |
| F4 | Demand annotation | strictness | 0.9 | foundation for 0006 |
| F5 | Restore git imports | x-unit | 0.10 | P1; foundation for 0018 |
| F6 | `vec` of arbitrary values | GC | 0.11 | P2, conditional |
| 0001 | Charter & editions | 1.0-commit | 0.8 | ★ |
| 0015 | Parser error-recovery | (tooling) | 0.8 (Ph1) / 0.9 (Ph2) | |
| 0011 | Typeclasses without classes | types-valid. | 0.8 | ratify |
| 0003 | Conformance, property tests, fuzzing | 1.0-commit | 0.8 begin → 1.0 bar | |
| 0004 | Compiled-unit / prelude caching | caching | 0.9 floor → post-1.0 | ★ |
| 0014 | Incremental query core | caching | 0.9 begin → post-1.0 | |
| 0013 | `s"…"` string type-DSL | types-valid. | 0.9 | decide-to-keep |
| 0016 | `eu doc` | types-valid. | 0.9 | |
| 0005 | Generational GC | GC | 0.10 | ★ |
| 0006 | Strictness/demand analysis | strictness | 0.10 | |
| 0002 | Gradual-typing boundary policy | 1.0-commit | 0.10 | gates 0007 |
| 0020 | Persistent O(log n) blocks | GC | 0.11 | after 0005 |
| 0021 | Optional record fields | types-valid. | 0.11 | inference post-1.0 |
| 0009 | Structural contracts / validation | types-valid. | 0.12 | flagship |
| 0010 | Hermetic mode | 1.0-commit | 0.12 | cap. types won't-do |
| 0018 | Module & package system | x-unit | 1.0 | ★; registry won't-do |
| 0017 | REPL / notebook | caching | 1.0 (Ph1) → post-1.0 | |
| 0019 | Host-language & schema interop | types-valid. | post-1.0 | JSON-Schema core 1.0-adjacent |
| 0007 | Type-directed compilation | strictness | post-1.0 | fork |
| 0008 | Parallel evaluation | GC | post-1.0 | fork (A then B) |
| 0012 | Algebraic-subtyping fork | — | won't-do | collides with HKT |

`★` = highest-leverage for 1.0. The proposals remain the detailed argument for
each row; this table is the schedule.
