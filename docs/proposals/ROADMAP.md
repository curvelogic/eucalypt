# Eucalypt ROADMAP — the road to 1.0 (and just beyond)

- **Status:** Plan of record
- **Date:** 2026-06-07
- **Baseline:** eucalypt **0.7.0** (gradual type system complete: rows, `Dict`,
  recursive types, literal types, `Partial(T)`, higher-kinded types and
  structural operator constraints all shipped by 0.7.0).

This is the single plan of record for eucalypt's evolution to **1.0** and the
shape of the work just beyond it. It is self-contained: everything needed to
understand and schedule the work is here. Item identifiers (`00NN` for
enhancements, `FN` for foundational fixes) are stable handles; their long-form
design write-ups live in a separate working set and are deliberately *not* part
of this document.

## The frame

0.7 completed the gradual type system the earlier roadmap was built toward, so
the road to 1.0 is mostly *outside* the type system: define what 1.0 commits to, pay down compile
latency and GC cost, give the config use case a validation-and-reproducibility
story, and build the tooling/ecosystem surface adoption needs.

The scarce deep-systems effort — the garbage collector above all — is the
binding constraint, so each release carries **at most one** big-systems thrust
plus cheaper wins, rather than overloading a single milestone.

### Principles (non-negotiable)

1. **Syntactic conservatism.** The surface syntax stays as it is. New machinery
   lives in metadata (backtick blocks), symbols, strings, blocks, operators or
   idiot-bracket pairs — never in new keywords, statement forms, lambda arrows,
   or `let … in` / `match … with`.
2. **Structural over nominal.** No named classes, no `implements`, no nominal
   types. Constraints reference shapes and functions, not names.
3. **Gradual and inference-first.** Types are advisory by default; a feature must
   deliver value when only the prelude is annotated.
4. **Single-threaded lazy-pure runtime.** Evaluation is lazy, pure and
   single-threaded over an `UnsafeCell` heap whose soundness depends on
   stop-the-world access. IO is an explicit monad interpreted by the driver; the
   pure core performs no effects.
5. **Tool-first.** Eucalypt generates, templates and transforms YAML/JSON/TOML
   before it is a language. Its peers are configuration/data languages. Weigh
   every idea against that use.

## Decisions & policies

These are resolved, and recorded so they are not re-litigated.

| Decision | Resolution |
|---|---|
| **Versioning & stability** | Adopt real semver at 1.0 (the four-part build number becomes `+build.N` metadata). 1.0 freezes an enumerated stable surface in tiers — Stable / Experimental / Not-covered — with the **type system Experimental**, so it does not block a lang/prelude 1.0. Breaking changes need a **MAJOR**; the `requires` guard fails loud on an incompatible binary. The prelude evolves via an **opt-in v2** (frozen v1 coexists), not in place. Pending breaking changes (e.g. deep-merge-as-default) ship as a MAJOR or behind prelude v2, never a silent default flip. **No editions** (see Non-goals). |
| **Boundary soundness** | Types stay **advisory by default** (silent `any → T`); whole-program checking is **opt-in** via `eu check --strict`. No blame-tracked casts, no whole-program sound-cast guarantee. |
| **String type-DSL** | Keep the `s"…"` metadata type-DSL through 1.0. No reserved type bracket; the metadata-string surface is the committed form. |
| **Ad-hoc polymorphism** | "Typeclasses without classes": structural operator constraints over shapes and functions, no named classes. This is the standing position. |
| **Runtime performance is a 1.0 goal** | The generational GC (0005) **and** strictness/demand analysis (0006) both ship before 1.0. |
| **Post-1.0 big rebuilds are curated, not pre-committed** | The roadmap commits through 1.0. The glamorous rebuilds (0007 type-directed compilation, 0008 parallelism) are deliberate, **one-at-a-time** candidates chosen *at* 1.0 — never parallelised, never assumed. |

### Non-goals (won't-do)

- **A package registry.** All distribution goals are met with **git + GitHub /
  GitLab** alone — content-addressed imports, an in-language manifest, version
  selection. No index, no hosted registry.
- **Capability *types*.** The hermetic/determinism *mode* (0010) ships; a
  capability *type system* does not — too much machinery for the tool-first remit.
- **Algebraic subtyping / MLsub.** The bidirectional checker plus shipped HKT is
  the committed core; an MLsub swap conflicts with HKT. Reassess only if the
  current core visibly creaks.
- **Nominal types / classes.** Stay structural. A nominal newtype, if ever, would
  itself be opt-in (a MAJOR or prelude v2), never a default.
- **Rust-style editions.** Carrying every old semantic lowering in the desugarer
  forever is over-commitment at current scale, for a benefit the ecosystem does
  not warrant — there is no body of deployed third-party `.eu` files to keep
  working across breaking changes. The `requires` guard and opt-in prelude v2
  cover the real need; revisit only if such an ecosystem appears.

## The six families (what groups with what, and why)

Releases group along six commonalities — the real reason items sit together.

| Family | Members | The shared thing |
|---|---|---|
| **Cross-unit interface** | F2, **F3**, 0004, 0014, 0018 | One *Unit Interface* (F3) seeds every phase's cross-unit contribution; separate compilation, incremental queries and module imports all consume it. |
| **Caching / incremental** | 0004, 0014 | Type-cache → code-cache → demand-driven query graph; one cache-key discipline. 0014 is the architecture the other is a point-solution of. |
| **GC / memory** | **0005**, 0020, F6 | Generational nursery first; then persistent blocks and vec-of-values on the GC-scannable-array machinery. One scarce skill set — *sequence, never parallelise*. |
| **Strictness / demand** | F4, 0006, 0007 | One demand annotation (F4) feeds the analysis (0006) feeds type-directed compilation (0007, post-1.0). |
| **Types-as-validation** | 0009, 0021, 0016, 0019 | The `s"…"` type vocabulary is reused by runtime validation, presence-typed fields, doc/schema extraction and schema interop. |
| **1.0 commitment** | 0001, 0003, 0010 | Versioning & stable-surface tiers, a conformance corpus, reproducibility — what "1.0" *means* and how it is *proven*. |

## The dependency spine

The edges that actually constrain ordering (`→` = "must precede"):

- **F3** (Unit Interface) **→ 0004, 0014, 0018**; and F3 **subsumes F2**.
- **F4** (demand annotation) **→ 0006**.
- **F5** (git import backend) **→ 0018**.
- **0005** (generational GC) **→ 0020** (same scarce GC expertise; 0005's nursery
  also makes the persistent structures' short-lived nodes cheap).
- **0021** (presence) **→ 0019**; and it **enriches 0009** (the optional `match?`
  arm).
- **0009, 0016 → 0019** (an ingested schema *is* a contract; doc extraction feeds
  codegen).
- **0004 → 0017, 0008-A**; **0014 → 0017** (per-prompt latency + reactivity).
- **F6** (vec) shares 0020's GC-array machinery but is otherwise **independent**.

## The foundations — the `F`-fixes that feed the releases

Concrete fixes that proposals depend on. Each is summarised here so the plan
stands alone.

| Fix | What it is | Lands | Unblocks |
|---|---|---|---|
| **F1** | Eliminate the double STG compile of plain documents (one compile, one machine; removes a GC-lifecycle landmine) | 0.8 | a cheap latency win |
| **F2** | Make block-bracket content-mode compose across imports (today a definition that works in one file silently mis-parses when imported) | 0.8 | *(folded into F3)* |
| **F3** | Unify the four inconsistent cross-unit contributions (brackets, monad namespaces, operator table, type schemes) into one **Unit Interface** | 0.8 | 0004, 0014, 0018; fixes F2 |
| **F4** | Unify the ~six strictness/update heuristics behind **one demand annotation** per binding | 0.9 | 0006 |
| **F5** | Restore git imports as a **fetch backend beneath a content hash** (a documented feature dropped in the Haskell→Rust rewrite) | 0.10 | 0018 |
| **F6** | Generalise `vec` to hold arbitrary values (`Array<Closure>`, GC-traced); also fixes a small latent backing leak | 0.11 | pairs with 0020 |

## The release plan

Each release is a theme, the engineering it ships, and a **gate** — what "done"
means.

### 0.8 — "Versioning, foundations & front-end hygiene"
*Design-heavy, front-end, low-risk. Make 1.0's shape real and stop the obvious pain.*

- **0001** — versioning & stability discipline: adopt real semver (build number
  → `+build.N` metadata), document the `requires` range-pin guard, ship opt-in
  prelude versioning (frozen **v1** + in-development **v2**, selected per-unit),
  add the deprecation lifecycle, and ratify the stable-surface tiers.
- **0015 (Phase 1)** — delete the all-or-nothing parser shim; always surface the
  Rowan partial tree (~150 lines; immediate diagnostic win).
- **0003 (begin)** — stand up a conformance corpus from the existing harness;
  wire `EU_GC_VERIFY=2` / `EU_GC_STRESS=1` as a CI gate.
- **F1** — one compile per plain document.
- **F3** (with **F2**) — one cross-unit interface mechanism instead of four.

**Gate:** real semver in force and the `requires` guard fails loud; the
prelude-v2 selector works with v1 as default; a partial parse tree is always
available; CI runs under GC verification; plain documents compile once.

### 0.9 — "Compile latency, docs & incremental groundwork"
*Cheap-to-medium. Attack the most visible perf problem and the authoring surface.*

- **0004 (prelude floor)** — prelude/compiled-unit cache (on **F3**); attacks the
  ~500–700 ms cold-compile cost head-on.
- **0014 (begin)** — wrap the front-end passes as queries (on **F3**).
- **0016** — `eu doc`: documentation & schema extraction (reads the `s"…"`
  surface); regenerate the hand-written prelude reference.
- **0015 (Phase 2)** — parser error-recovery / resilient front-end.
- **F4** — the single demand annotation (unblocks 0006).

**Gate:** cold-compile latency materially down; `eu doc` regenerates the prelude
reference; the front-end is queryable; one demand annotation drives the
thunk/update decisions.

### 0.10 — "The runtime: GC & demand"  *(big-systems thrust #1)*
*The substantive engineering release. One major GC effort, plus its multiplier.*

- **0005** — generational nursery + smarter trigger + finish the stubbed Immix
  defrag tier. The highest-impact memory work (>95 % of traversal-heavy time is
  mark cost).
- **0006** — strictness/demand analysis (on **F4**); fewer thunks → less to mark,
  reinforcing 0005.
- **F5** — restore git imports as a fetch backend beneath a content hash (the
  foundation 0018 builds on).

**Gate:** mark cost down via the nursery; the analysis cuts thunk churn on real
workloads; git imports work, pinned by commit and verified by content hash.

### 0.11 — "Blocks, value model & presence types"  *(big-systems thrust #2)*
*The second GC item — only after 0005 — plus the value-model and type enrichments.*

- **0020** — persistent O(log n) blocks via an inline-GC-heap **CHAMP key-index +
  insertion-order sequence** (after **0005**). Backs large/merge-heavy configs.
- **F6** — generalise `vec` to hold arbitrary values (shares 0020's
  GC-scannable-array machinery). Backs random-access over arrays-of-records.
- **0021** — optional (presence-annotated) record fields, `name?: T` (annotated
  form; presence *inference* deferred post-1.0). Enriches 0009; unblocks 0019.

**Gate:** large/merge-heavy blocks are O(log n) with no GC regression; `vec` holds
records/lists; `name?: T` checks and renders; full harness green under
`EU_GC_VERIFY=2` / `EU_GC_POISON=1`.

### 0.12 — "Data-correctness & reproducibility"
*The config story that distinguishes a 1.0 data tool.*

- **0009** — structural contracts / spec / runtime validation at data ingress
  (uses the `s"…"` vocabulary; enriched by 0021). The flagship competitive
  feature and the pragmatic resolution of the boundary tension.
- **0010** — hermetic mode for reproducible rendering (the determinism *mode*; the
  capability *type system* is a non-goal).

**Gate:** ingress validation with clear blame; byte-reproducible renders under
hermetic mode.

### 1.0 — "Ecosystem floor, prove it, freeze it"

- **0018 (core cut)** — integrity-hashed git imports + an eucalypt-syntax manifest
  + namespace isolation (on **F5**, **F3**). No registry.
- **0017 (Phase 1)** — `eu watch` (Unison-style) + a thin REPL over the cache (on
  **0004**, **0014**).
- **0003 (bar met)** — golden corpus green (incl. that a v1-prelude file is
  unchanged by a binary that also ships v2); property tests and the fuzz targets
  running.
- **0001 (freeze)** — freeze the enumerated stable surface at the ratified tiers;
  real semver in force; the `requires` guard enforced; prelude v1 frozen (v2
  opt-in).

**Gate:** the 1.0 commitments hold; conformance is green; modules are hermetic and
shareable by git URL + hash, with no registry.

### post-1.0 — breadth and the curated bets
*The roadmap commits through 1.0. Beyond it, these are chosen deliberately, one at a time.*

- **0004 (full)** — separate compilation + general unit cache (the rest of 0004,
  on the F3 interface).
- **0014 (full)** — incremental *evaluation*, beyond front-end analysis.
- **0019** — host-language & schema interop (on 0009, 0016, 0021); its
  JSON-Schema import/export core is the natural first increment.
- **0017 (full)** — the full notebook surface.
- **0021 (inference)** — presence *inference* (beyond the annotated form).
- **0018 (depth)** — deeper version selection / lockfile closure (still no
  registry).
- **The curated forks** (mutually exclusive; chosen at 1.0):
  - **0007** type-directed compilation (gated on the advisory-boundary policy;
    5–10× on typed numeric code).
  - **0008** parallel evaluation — *A* isolated workers (on 0004), then *B*
    shared-heap (needs a `Send`/`Sync` GC rebuild; maybe never).
- **alternative-backends** (core→WASM and friends) — the consumer of 0003's
  conformance contract.

## Candidates not yet planned

Areas worth a proposal but not yet drafted — recorded so they are not forgotten,
and able to swap into the plan if priorities shift. (The **alternative-backends**
work — core→WASM and other targets — is the most developed of these and is noted
under post-1.0 above.)

- **Debugging a lazy language** — a DAP server / value-provenance for stepping
  lazy evaluation.
- **Streaming & large-data scaling** — bounded-memory processing of inputs that
  don't fit the heap.
- **Distribution & packaging** — nixpkgs / Homebrew / a WASM CDN build.
- **Homoiconicity & macros** — compile-time metaprogramming over the core.
- **First-class type values** — types as ordinary values, beyond the `s"…"`
  surface.
- **Algebraic effect-rows** — typed effects beyond the IO monad.

## The GC line, and the curated rebuilds

Five efforts each want sustained, expert, deep-systems work drawing on the same
overlapping skills (GC, the core checker, the STG machine). They **cannot all
happen at once**, and most are post-1.0.

| Effort | Buys | Verdict |
|---|---|---|
| **0005** generational GC | cuts the dominant mark cost; benefits *every* program | **Do it — 0.10.** |
| **0020** persistent blocks | O(log n) for large/merge-heavy configs | **0.11, after 0005.** |
| **0007** type-directed compilation | types as a runtime asset | post-1.0 candidate; gated on the boundary policy. |
| **0008** parallel evaluation | multi-core for big maps/multi-doc renders | post-1.0 candidate; *B* is a GC rebuild in itself. |
| **0012** MLsub core swap | principal types "for free" | **Won't-do** — collides with shipped HKT. |

The throughline: **spend the scarce systems effort on the GC (0005, then 0020)
before 1.0**, and take 0007/0008 as one-at-a-time post-1.0 bets.

## Leverage vs cost

A second lens on the same work — bang-for-buck, orthogonal to release order.
Settled decisions and non-goals are excluded.

| Bucket | Items |
|---|---|
| **Cheap & high-leverage** (design-heavy, do early) | F1, 0001 (versioning), 0004 (prelude floor), 0015, 0016 |
| **High-leverage, real engineering** | F3, 0005, 0009, 0014, 0018, 0020 |
| **Worthwhile, medium** | F4, F5, F6, 0006, 0010, 0017, 0021 |
| **Big bets / forks** (deliberate, post-1.0, one at a time) | 0007, 0008, 0019 |

## The critical path, in one line

**0001 (versioning) + F1/F3 (cheap latency + the interface) → 0004 (latency) → 0005
(the runtime) → 0009 + 0018 (data-correctness + ecosystem floor, on F5) → 0003
green (prove it) → ship 1.0.** Everything else is a cheap win that slots
alongside, or a curated post-1.0 bet.

## Index — every item, where it lands

| # | Item | Release |
|---|------|:-------:|
| F1 | Eliminate double STG compile | 0.8 |
| F2 | Bracket content-mode across imports | 0.8 (via F3) |
| F3 | Unit Interface | 0.8 |
| F4 | Demand annotation | 0.9 |
| F5 | Restore git imports | 0.10 |
| F6 | `vec` of arbitrary values | 0.11 |
| 0001 | Versioning & stability discipline | 0.8 → 1.0 freeze |
| 0015 | Parser error-recovery | 0.8 (Ph1) / 0.9 (Ph2) |
| 0003 | Conformance, property tests, fuzzing | 0.8 begin → 1.0 bar |
| 0004 | Compiled-unit / prelude caching | 0.9 floor → post-1.0 full |
| 0014 | Incremental query core | 0.9 begin → post-1.0 full |
| 0016 | `eu doc` | 0.9 |
| 0005 | Generational GC | 0.10 |
| 0006 | Strictness/demand analysis | 0.10 |
| 0020 | Persistent O(log n) blocks | 0.11 |
| 0021 | Optional record fields | 0.11 (inference post-1.0) |
| 0009 | Structural contracts / validation | 0.12 |
| 0010 | Hermetic mode | 0.12 |
| 0018 | Module & package system (no registry) | 1.0 |
| 0017 | REPL / notebook | 1.0 (Ph1) → post-1.0 |
| 0019 | Host-language & schema interop | post-1.0 |
| 0007 | Type-directed compilation | post-1.0 (curated) |
| 0008 | Parallel evaluation | post-1.0 (curated) |
| 0002 | Gradual-typing boundary policy | *decision (settled)* |
| 0011 | Typeclasses without classes | *decision (settled)* |
| 0013 | `s"…"` string type-DSL | *decision (settled)* |
| 0012 | Algebraic-subtyping fork | *won't-do* |
