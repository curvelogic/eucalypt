# Eucalypt evolution proposals — toward v1.0 and beyond

- **Status:** Draft portfolio for review
- **Date:** 2026-06-01
- **Baseline:** eucalypt 0.7.0 (type-system Stage A shipped in 0.6.2; Stage B + HKT shipped in 0.7.0)

This directory collects the strategic proposals for how eucalypt should
evolve over the next few minor versions on the road to **1.0**, and where it
should head afterwards. They are written to be reviewed *as a set* when
deciding the next stages of the language's evolution after 0.7. Each is an
argument, not a commitment; each is sized to be read in one sitting (~2,000
words) and to support a yes / no / defer decision.

**The consolidated plan of record is [`ROADMAP.md`](ROADMAP.md)** — the
release-by-release schedule (0.8 → 1.0 → post-1.0) that folds this portfolio, the
sequencing analysis, and the `0000` fixes into one plan. This index and the
proposals remain the detailed rationale behind it.

For how the documents are written, see [`_house-style.md`](_house-style.md).
For the longer-form dependency analysis the roadmap distils, see
[`SEQUENCING.md`](SEQUENCING.md). For high-priority **fixes** to
land before/alongside these proposals (surfaced during review, written
bead-ready for import), see [`0000-priority-fixes.md`](0000-priority-fixes.md).

---

## Why now, and why these

Eucalypt is a **tool first** — for generating, templating and transforming
YAML/JSON/TOML — and a lazy, pure, functional language second. Its peers are
configuration/data languages (Jsonnet, Dhall, CUE, Nickel, Pkl, KCL, Starlark),
not general-purpose languages. It already has a great deal: a gradual type
checker with higher-kinded types (0.7.0), a rich LSP, an Immix-style GC, an STG VM, a tree-sitter
grammar, and editor support.

Two facts shape this portfolio:

1. **The type-system roadmap is now largely shipped.** Stage A landed in
   **0.6.2** (`Dict`, equirecursive types, literal types, flow narrowing,
   `NonEmpty`, first-class alias references) and Stage B + **HKT** in **0.7.0**
   (`Con`/`App` kinds, HKT-typed `monad()`, dependent indexed access, the prelude
   type cache, `Partial(T)`, full row inference, structural operator
   constraints). The nineteen-hypothesis
   [`type-system-evolution.md`](../development/type-system-evolution.md) and its
   [`type-system-bead-plan.md`](../development/type-system-bead-plan.md) are now a
   *delivered* plan, with a sketched Stage C ("radical options") remaining. These
   proposals therefore **build on a shipped type system**, and engage the seven
   open questions the maintainer left in §5 of that document — several of which
   the delivered work now informs.
2. **The largest gaps are not in the type system.** There is no definition of
   what "1.0" commits to; no package/module system beyond file imports; no
   doc-generation; no concurrency story; an unresolved O(n)-block / GC-finalisation
   wall (ADR-001); and an uncached ~500–700 ms compile latency that is the most
   *visible* performance problem. Most of the whitespace proposals live here.

Every proposal honours the **non-negotiables** (see `_house-style.md` §1):
syntactic conservatism, structural-over-nominal, gradual/inference-first, the
single-threaded lazy-pure runtime, the tool-first framing, and UK English.

---

## The portfolio

`Class`: **E**xtends-roadmap · **W**hitespace · **F** Stage-C fork · **P**rocess.
`★` marks the highest-leverage items for 1.0.

### Track A — v1.0 readiness & process

| # | Proposal | Class | Horizon |
|---|----------|:-----:|:-------:|
| [0001](0001-v1-charter.md) | Versioning & stability discipline: semver, `requires` guard, opt-in prelude v2 | W★ | 0.8 |
| [0002](0002-gradual-typing-boundary-policy.md) | Gradual-typing soundness & boundary policy for 1.0 | P | 0.9 |
| [0003](0003-conformance-testing-fuzzing.md) | Testing for 1.0: harness-as-conformance-contract, property tests, fuzzing | W | 0.8–1.0 |

### Track B — performance, runtime & concurrency

| # | Proposal | Class | Horizon |
|---|----------|:-----:|:-------:|
| [0004](0004-compiled-unit-caching.md) | Compile latency: separate compilation, prelude embedding & (optional) caching | F★ | 0.9 → post-1.0 |
| [0005](0005-generational-gc.md) | Generational nursery & GC trigger/Immix-defrag completion | W★ | 0.9 |
| [0006](0006-strictness-analysis.md) | Strictness & demand analysis (cut thunk churn) | W | 0.9 |
| [0007](0007-type-directed-compilation.md) | Type-directed compilation (H11) — types as a runtime asset | F | post-1.0 |
| [0008](0008-parallel-evaluation.md) | Parallelism: isolated workers first (JS model), shared-heap fork later | F | post-1.0 |

### Track C — type system & language (beyond the roadmap)

| # | Proposal | Class | Horizon |
|---|----------|:-----:|:-------:|
| [0009](0009-structural-contracts-validation.md) | Structural contracts/spec: unify `match?`, types & runtime validation (Clojure-spec model) | W | 1.0 |
| [0010](0010-capability-determinism-types.md) | Hermetic mode for reproducible rendering (H6c); capability types won't-do | E | 1.0 |
| [0011](0011-typeclasses-without-classes.md) | "Typeclasses without classes" maturation (beyond H10) | E | 0.7+ |
| [0012](0012-algebraic-subtyping-fork.md) | The algebraic-subtyping fork (H8 MLsub/MLstruct): won't-do (too cutting-edge with HKT) | F | won't-do |
| [0013](0013-type-dsl-embedding.md) | String type-DSL for 1.0 (H12), refined by the `s"…"` prefix (H12e) for 0009's value-context refs; reserved bracket rejected | E | 0.9 / 1.0 |

### Track D — tooling

| # | Proposal | Class | Horizon |
|---|----------|:-----:|:-------:|
| [0014](0014-incremental-query-core.md) | Incremental, query-based core (Salsa/Adapton) for LSP & re-eval | W | 0.9–1.0 |
| [0015](0015-parser-error-recovery.md) | Parser error-recovery & resilient front-end (Rowan) | W | 0.8 |
| [0016](0016-eu-doc.md) | `eu doc` — documentation & schema extraction | W | 0.9 |
| [0017](0017-repl-notebook.md) | An interactive surface: REPL / notebook for data exploration | W | 1.0 |

### Track E — ecosystem, interop & safety

| # | Proposal | Class | Horizon |
|---|----------|:-----:|:-------:|
| [0018](0018-module-package-system.md) | Module & package system: versioned, content-addressed, hermetic imports | W★ | 1.0 |
| [0019](0019-host-language-interop.md) | Host-language & schema interop: codegen + JSON-Schema/CRD ingest | W | post-1.0 |
| [0020](0020-persistent-blocks-gc-finalisation.md) | Persistent O(log n) blocks & resolving the GC-finalisation wall (ADR-001) | W | 0.9 |
| [0021](0021-optional-record-fields.md) | Optional (presence-annotated) record fields (`name?: T`) — prerequisite for 0019, enrichment for 0009 | E | 1.0 |

**Bench (alternates that can swap in):** debugging a lazy language (DAP /
value-provenance); streaming & large-data scaling; distribution & packaging
(nixpkgs / Homebrew / WASM CDN); homoiconicity & macros; first-class type
values; algebraic effect-rows.

**Planned additions (not yet drafted):** an **alternative-backends** proposal —
core→WASM compilation and other execution targets — is forthcoming. These are
genuinely independent implementations, so they are the consumer of 0003's
conformance contract; they also interact with
[0008](0008-parallel-evaluation.md) and [0019](0019-host-language-interop.md).

---

## How these engage the maintainer's seven open questions

(from [`type-system-evolution.md`](../development/type-system-evolution.md) §5)

| Open question | Addressed mainly in |
|---|---|
| 1. Is "no nominal" inviolable? | 0011, 0001 (export contracts) |
| 2. Is the metadata type-DSL acceptable indefinitely? | 0013 |
| 3. Encourage user-defined monads (pulls in HKT)? | 0011 |
| 4. How much do we care about runtime performance? | 0004, 0005, 0006, 0007 |
| 5. Invest in algebraic subtyping (MLsub)? | 0012 |
| 6. Policy on boundary unsoundness? | 0002, 0009 |
| 7. Is typing the metadata channel worth it? | 0009, 0013 (revisits the won't-do) |

---

## Reading order

- Deciding **what 1.0 *is***: 0001 → 0002 → 0003.
- Deciding **whether the runtime is 1.0-ready**: 0004 → 0005 → 0006, then the
  forks 0007 → 0008.
- Deciding **how far the language goes**: 0009 → 0011 → 0010, with the 0012
  and 0013 forks.
- Deciding **the adoption surface**: 0014 → 0015 → 0016 → 0017 → 0018 → 0019.

`SEQUENCING.md` proposes a concrete release-by-release ordering.
