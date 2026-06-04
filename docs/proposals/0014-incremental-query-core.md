# 0014 — Incremental, query-based core (Salsa/Adapton) for LSP & re-eval

- **Status:** Draft proposal for review
- **Track:** D — tooling
- **Classification:** Whitespace
- **Suggested horizon:** 0.9–1.0
- **Related:** TS-B7 (prelude-type-cache-spec.md); sibling proposals
  [0004 — compiled-unit & prelude caching](0004-compiled-unit-caching.md),
  [0015 — parser error-recovery & resilient front-end](0015-parser-error-recovery.md),
  [0008 — parallel & speculative evaluation](0008-parallel-evaluation.md);
  also [0017 — REPL / notebook](0017-repl-notebook.md)

---

## Summary

Every invocation of eucalypt, and every LSP keystroke, recomputes the
whole front end from scratch — parse, desugar, cook, eliminate,
type-check — over the prelude *and* the open files. The 0.7.0 LSP uses
a background "pipeline backend" running this on a worker thread with a
300 ms debounce and per-document cancellation, but it is not incremental
in the demand-driven sense: each run builds a fresh `SourceLoader` and
redoes everything. This proposal argues for re-architecting the analysis
front end as **memoised, dependency-tracked queries** in the style of
Salsa (rust-analyzer/rustc) and Adapton, keyed by content hash and
connected by the existing import graph. The payoff is instant LSP
feedback on large projects, incremental type-checking, and a foundation
for a responsive REPL — and eucalypt's **purity makes it an unusually
good fit**: referential transparency means a query's output is a pure
function of its inputs, so it is trivially and soundly memoisable.

---

## Motivation

### The front end recomputes everything, every time

The CLI pipeline (`src/driver/prepare.rs:24`) is a straight-line
sequence — parse, translate, merge, retarget, cook, eliminate, inline
×2, fuse, eliminate, verify — with no result reused across invocations.
A `SourceLoader` is a fresh, blank object every run
(`src/driver/source.rs:97`, `:120`); its `asts`, `cores` and
`translation_units` maps (`source.rs:64–70`) memoise *within* a single
load but are discarded at process exit. [0004] addresses the **CLI**
side (an on-disk `StgSyn` cache for the prelude); this is its
**in-process, incremental** cousin, aimed at the interactive front end
where the recompute happens dozens of times a second.

The LSP is where the cost bites hardest. On every change,
`on_document_changed` (`src/driver/lsp/mod.rs:731`) re-parses, diffs the
Rowan green node for an actual change (`mod.rs:753`), then spawns
`run_pipeline` (`mod.rs:1119`). That function constructs a brand-new
`SourceLoader::new(vec![])` (`mod.rs:1146`) and re-runs **load →
translate → merge → cook → eliminate → `type_check_full`**
(`mod.rs:1148–1182`) over `[prelude, document]` — the ~2,200-line
prelude included — for a single edited file. The only concession to
incrementality is the *text* sync (`apply_content_change`,
`mod.rs:246`) and the green-node change-detector that suppresses no-op
re-runs (`mod.rs:753`). Everything downstream of "did the tree change?"
is recomputed wholesale.

The result is the latency profile the recon flags for the CLI
(~500–700 ms compile pipeline; parse ~120 ms, cook ~190 ms over the
prelude) re-incurred on a 300 ms debounce. For a small file the debounce
hides it; for a multi-file project with real imports it will not.
TS-B7 (`docs/development/prelude-type-cache-spec.md` §B7.1) names this
exactly: "The LSP today re-checks the whole world (prelude + open files)
on every change; the prelude … dominates that cost." Its own framing is
the thesis of this proposal — "the type checker exists to *support
writing eucalypt* … LSP responsiveness is its main job."

### Point caches are partial answers a query core generalises

One shipped cache and one specced cache each remove one fixed stage from the hot path:

| Cache | Stage removed | Scope | Status | Where |
|---|---|---|---|---|
| TS-B7 prelude type summary | type-check(prelude) | prelude only | **Shipped 0.7.0** | `src/driver/check.rs:44` |
| [0004] compiled-unit cache | compile(prelude)→`StgSyn` | prelude only | Specced | `0004` |

TS-B7 **shipped in 0.7.0**: the prelude is type-checked once per
process via `build_prelude_summary` and the result stored in a
`static PRELUDE_CACHE: OnceLock<PreludeSummary>`
(`src/driver/check.rs:44`); subsequent checks seed a fresh `Checker`
from the cached `PreludeSummary` via `Checker::with_seed`
(`src/core/typecheck/check.rs:382`), skipping the ~2,200-line prelude
re-walk entirely. This is concrete proof the demand-driven caching thesis
is correct here — it pays off, it is sound, and it has shipped.

Both caches are sound for the same reason: the prelude is **fixed,
merged first, and self-contained** (B7 §B7.1; 0004 "Why the prelude is
the ideal target"). Both stop there for the same honest reason: a *user*
unit's exported types are not stable under positional merge-override (B7
§B7.9), so caching user units soundly needs a composition model neither
cache pins down. A query core is the structure that *makes user-unit
incrementality tractable* — it tracks, per query, exactly which inputs
were read, so it can invalidate precisely rather than reason about
stability up front. The two point caches then become two **base-case
query results** in one database, not two bespoke mechanisms (0004
already says as much: "the prelude's compiled form is the base case of
the incremental graph").

### Why this is the right foundation for a tooling-first language

The checker is advisory and erased; it earns its keep by powering hover,
inlay hints, completion and diagnostics in an editor. A tool that is
"tool-first" should make its *editing* surface fast before its batch
surface. The infrastructure built here is also what a credible REPL
([0017]) and a resilient front end ([0015]) both need — see §5.

---

## Prior art & landscape

**Salsa** (the engine under rust-analyzer, and the lineage of rustc's
query system). The organising idea: *model the program as queries*, each
a function `K → V`. Queries are of two kinds — **inputs** (the mutable
base facts: file contents) and **tracked functions** / derived queries
(pure transforms of inputs and other queries). Dependencies are recorded
*automatically*: while a derived query runs, every query it reads is
logged, building a dependency graph with no manual wiring. Re-running
after an edit uses the **red-green algorithm**: bump a global revision,
flood forward to the changed inputs, and only re-execute a derived query
("red") if a recorded dependency actually changed value; otherwise its
memoised result is reused ("green") by bumping a verified-at revision.
Salsa adds a **durability** optimisation — inputs are tagged by how
likely they are to change (library code high, the file you are typing in
low) so high-durability subgraphs are skipped wholesale when only
low-durability inputs move. Salsa calls itself "inspired by adapton,
glimmer, and rustc's query system."

**Adapton** (Hammer, Acar et al.; the λ\_icc calculus, OCaml/Rust
libraries) is **demand-driven**: it builds a *demanded computation graph*
(DCG) of only the computations a query actually forced, and combines
**memoisation** with **change propagation** — mutating an input dirties
the DCG edges that depended on it, and re-demanding a value repairs just
the dirtied sub-graph. The emphasis on *demand* maps neatly onto
eucalypt, itself a lazy, force-driven language. **Self-adjusting
computation** (Acar's thesis) is the underlying theory both draw on:
record a trace, re-run only the affected parts. **Observable**'s
reactive runtime is the same idea in a notebook — each cell a dataflow
node, editing one re-runs only its dependents — directly relevant to
[0017].

**What transfers, and what does not.** What transfers: queries-as-`K→V`,
automatic dependency recording, content-hash inputs, and red-green reuse
— proven for *front-end/analysis* work, which is exactly where the LSP
benefit lives and where Salsa is battle-tested. What does **not**
transfer cleanly is incremental *evaluation*: rust-analyzer never runs
the program; eucalypt does, on a stop-the-world, non-`Sync`
`UnsafeCell` heap (`src/eval/memory/heap.rs:8–11`). The query model wants
to cache and reuse arbitrary intermediate values; the VM's values are
heap pointers tied to a live GC (the same hazard 0004 hits with
`HeapSyn`). So we borrow Salsa for the **pure analysis front end** and
treat evaluation incrementality as a separate, harder problem (§4,
stretch goal).

---

## Proposed design

The proposal touches **no surface syntax** — it is an internal
re-architecture of `src/driver/` and the analysis passes. Nothing in
this document adds a keyword, operator or metadata key.

### The query graph over the front end

Model the analysis front end as derived queries over content-hashed
inputs. Concretely (names illustrative):

| Query | Key | Reads (deps) | Value |
|---|---|---|---|
| `source(loc)` | locator | — (input) | source text + content hash |
| `parse(loc)` | content hash | `source` | Rowan green node + parse errors |
| `imports(loc)` | content hash | `parse` | resolved import `Input`s |
| `desugar(loc)` | content hash + dep hashes | `parse`, `imports` | unit `RcExpr` |
| `cook(unit)` | dep hashes | `desugar`* | cooked core |
| `check(unit)` | dep hashes | `cook`*, prelude summary | warnings + types + lambda params |

`*` over the unit's import closure. The **inputs** are document texts
(the open buffers the editor owns) and the embedded prelude; everything
else is a pure derived query. Keys are **content hashes**, not paths or
mtimes — the discipline 0004 adopts from Unison, so a cache hit is
*semantically* guaranteed. Editing a string literal changes the hash and
correctly invalidates (matching today's green-node check at
`mod.rs:753`); editing whitespace the lexer discards does not.

**Inter-unit dependencies reuse the import graph.** The dependency edges
between unit-level queries are exactly those already computed by
`ImportGraph` (`src/syntax/import.rs:18`): `unit_inputs` BFSes the import
closure (`import.rs:93`), and dedup is by locator (`import.rs:118`) — so
the diamond case (`lens=lens.eu`, `optics=lens.eu` → one node,
`import.rs:117`) already collapses to a single query computed once. A
`check(unit)` query depends on each imported unit's `check`/summary; an
edit to a leaf import dirties only the units on the path back to the
root, leaving siblings green. Cycle rejection already exists
(`import.rs:83`).

### Scope the front end first; the prelude is the proof

The first deliverable is the **analysis** subset above, with the prelude
as the canonical high-durability input. This *generalises* the shipped
TS-B7 and the analysis half of 0004: the prelude's
`parse`/`desugar`/`cook`/`check` results are queries whose input
(prelude bytes) never changes within a build, so they compute once and
stay green for the life of the process — precisely what B7 achieves
with `OnceLock<PreludeSummary>` (`src/driver/check.rs:44`), but now
emergent from the query graph rather than a bespoke static. The LSP's
existing per-URI `cached` and `last_green`
(`mod.rs:277`, `:280`) are the embryo of this database; the work is to
generalise them from "one cached blob per file" to a graph of
fine-grained, individually-invalidated results, and to make the prelude
a shared root rather than re-loaded per document (`mod.rs:1128`).

### Where evaluation resists the model (stretch goal)

Incremental *re-rendering* is explicitly a stretch goal, not 0.9 scope.
The obstruction is the heap: VM values are GC-managed pointers
(`heap.rs:8–11`), so a query cannot simply memoise "the value of
sub-expression *e*" across edits the way it memoises a parse tree.
Purity keeps the door open: because the pure core is referentially
transparent, the *value of a closed sub-expression is a pure function of
its free variables' values* — the necessary (not sufficient) condition
for sub-expression memoisation. Realising it needs a serialisable,
heap-independent value representation (the same need 0004 meets for
*code* with `StgSyn` rather than `HeapSyn`), plus content-addressing of
core sub-terms. This is the meeting point with [0008]: the purity
invariants that proposal records to make *parallel* speculative
evaluation sound are the very ones that make *incremental* re-evaluation
sound. Both are post-1.0 and gated on a cacheable value representation;
this proposal commits only to the analysis front end.

---

## Interaction with the existing roadmap

- **TS-B7** *shipped in 0.7.0* and is the query core's first concrete
  predecessor: the `OnceLock<PreludeSummary>` at
  `src/driver/check.rs:44` is a bespoke, single-stage query result.
  The query core generalises it — the `PreludeSummary` becomes the
  memoised value of the `check(prelude)` query, and the same
  dependency-tracking that makes user-unit invalidation tractable makes
  the prelude result *emergently* green forever. The
  behaviour-preservation property B7 already validates (§B7.7 — the
  from-scratch merged-pipeline fallback at `src/driver/check.rs:307`)
  becomes the correctness oracle for the query port: a query result must
  equal the from-scratch result.
- **[0004]** is the cross-process sibling: it persists the prelude's
  compiled `StgSyn` to disk; this proposal keeps the prelude's *analysis*
  results live in memory. They share the content-hash keying discipline
  and the "prelude is the base case" framing 0004 already states. 0004's
  Phase-2 daemon and this query core converge on one long-running
  process serving both CLI and editor.
- **[0015]** (resilient front end) is the natural *feeder*: error-recovery
  produces a partial-but-usable tree on every keystroke, which is exactly
  what a `parse` query needs to yield a `V` even when the input is
  mid-edit. 0015 supplies the robust inputs; 0014 supplies the
  incremental graph that consumes them. Sequence 0015 before, or with,
  0014.
- **[0017]** (REPL/notebook) is the downstream beneficiary: a notebook is
  Observable's dataflow graph, and a query database is its engine —
  re-evaluating only the cells (queries) a change reaches.
- **[0008]** shares the purity-as-substrate argument for the eval stretch
  goal (§4). Adopting `salsa` would add the project's first persistent
  cross-invocation in-memory dependency graph, but introduces **no**
  threading into `src/eval/`; the heap's single-thread invariant
  (`heap.rs:8`) is untouched.

---

## Implementation sketch

**Adopt `salsa` rather than build.** `salsa` is the proven engine and is
not currently a dependency (Cargo.toml carries `petgraph`, `sha2`,
`serde`, but no `salsa`/`adapton`), so this is a genuine new dependency
and a real cost; an in-house red-green engine is the fallback if the
macro-heavy `salsa` API proves a poor fit (§7). Phases below are each
independently shippable, wrapping existing passes as queries **one at a
time** so the migration is itself incremental:

1. **Database skeleton + inputs (0.9).** Introduce a query database
   alongside `SourceLoader`; model document texts and the prelude as
   inputs; wrap `parse` (`syntax::rowan::parse_unit`) and `imports`
   (`import.rs`) as the first derived queries. No behaviour change; the
   CLI still calls the straight-line pipeline.
2. **Analysis queries (0.9).** Wrap `desugar`, `cook`, and
   `type_check_full` (`check.rs:1149`) as derived queries keyed by
   content + import-closure hashes. Re-point the LSP `run_pipeline`
   (`mod.rs:1119`) at the database so an edit re-executes only the dirty
   queries; the prelude stays green. Supersedes B7's bespoke
   `OnceLock` (`check.rs:44`) with an emergent query result.
3. **Editor surfaces on the graph (1.0).** Serve hover/inlay/completion
   /diagnostics (`src/driver/lsp/*`) from query results, with multi-file
   projects sharing one database so cross-file navigation and
   diagnostics update incrementally.
4. **Stretch (post-1.0):** content-addressed core sub-terms and a
   cacheable value representation for incremental re-render; converges
   with [0004] Phase 2 and [0008].

**Size/risk:** medium-large. Phases 1–2 are contained in `src/driver/`
and are off the critical path for batch `eu` (the straight-line pipeline
can remain the CLI default until the query path is proven). The main
risks are `salsa`'s `'static`/`Send` requirements colliding with
existing non-`Send` types (the comment at `mod.rs:1089` notes
`SourceLoader` is not `Send`) and the discipline of threading hashes
through pass boundaries.

---

## Alternatives considered

- **Keep extending point caches.** TS-B7 has shipped and 0004 will
  complete the pair; stopping there is the cheap path and genuinely
  removes the dominant prelude cost. It does **not** make *user-file*
  and *multi-file* analysis incremental, which is where large-project
  LSP latency will live once the prelude is cached. The point caches are
  necessary but not sufficient; a query core is what makes user-unit
  invalidation sound rather than out-of-scope.
- **Hand-rolled memoisation in the LSP.** Extend the existing `cached`
  map with manual invalidation keyed on the import graph. Feasible for a
  few stages, but re-implements red-green badly; the value of Salsa is
  *automatic* dependency tracking — manual invalidation is exactly the
  bug-prone part.
- **Build an in-house engine.** Lower dependency risk, full control, but
  reinvents a well-trodden wheel; reserve for the case where `salsa`'s
  ergonomics fail on eucalypt's types.
- **Incremental evaluation first.** Tempting for a *rendering* tool, but
  it is the part the heap model resists most (§4); doing analysis first
  banks the LSP win on proven ground.

---

## Risks & what would kill this

1. **The LSP is already "fast enough".** B7 shipped in 0.7.0 and 0004
   will cache the compiled prelude; single-file edits may be
   imperceptible without a query core, making this over-engineering for
   a config tool. *Falsifier:* measure re-check latency on a realistic
   multi-file project (10–20 imported units) with B7 live and 0004 in
   place; if it is already < ~30 ms, defer 0014.
2. **`salsa` impedance mismatch.** Its derive-macro model and `'static`
   bounds may not sit well with `RcExpr`/Rowan/`SourceLoader`; the
   adaptation cost could exceed an in-house engine. *Mitigation:*
   spike Phase 1 on `parse`+`imports` only before committing.
3. **Correctness drift.** A stale query served when it should have been
   invalidated yields wrong diagnostics silently. *Mitigation:* B7's
   behaviour-preservation oracle (§B7.7) generalised — a debug mode that
   recomputes from scratch and asserts equality with the query result.
4. **Evaluation never follows.** If the cacheable-value representation
   proves infeasible, the eval stretch goal dies — but the analysis win
   stands alone and is the justification, so this is survivable.

---

## Success criteria

- **Incremental LSP latency:** after the first check, an edit to one unit
  in a 10–20-unit project re-checks in time proportional to the edited
  unit's closure, not the whole project; target median re-check
  < 30 ms, versus today's full re-run.
- **Prelude computed once:** the prelude's parse/desugar/cook/check
  queries execute once per session and stay green across all subsequent
  edits (the B7 property, now emergent).
- **Behaviour preservation:** for the harness corpus, query-served
  diagnostics and types are *identical* to the from-scratch pipeline
  (`cargo test` green; the recompute-and-compare debug mode asserts it).
- **Diamond correctness:** a shared import (`lens=lens.eu`,
  `optics=lens.eu`) is analysed once and invalidated once on edit.
- **Stretch:** a closed sub-expression's value is reused across an edit
  that does not touch its free variables (post-1.0).

---

## References

- `src/driver/prepare.rs:24` — straight-line CLI pipeline, no cross-run reuse.
- `src/driver/source.rs:97,120` — `SourceLoader` is fresh per run;
  `:64–70` — per-loader `asts`/`cores`/`translation_units` memo maps.
- `src/driver/check.rs:44` — `static PRELUDE_CACHE: OnceLock<PreludeSummary>`
  (TS-B7 shipped point solution); `:51` `get_or_build_prelude_summary`;
  `:70` `build_prelude_summary`; `:259` fast-path prelude-cached check;
  `:307` fallback merged-pipeline (behaviour-preservation oracle).
- `src/core/typecheck/check.rs:197` — `PreludeSummary` struct; `:382`
  `Checker::with_seed` (seeding a user-file check from cached prelude).
- `src/driver/lsp/mod.rs` — `:731` `on_document_changed`; `:753`
  green-node change detection; `:1060–1081` `spawn_pipeline` 300 ms
  debounce; `:1119–1182` `run_pipeline` rebuilds a fresh `SourceLoader`
  over `[prelude, document]`; `:277,280` per-URI `cached`/`last_green`
  (the embryonic database); `:246` `apply_content_change` text sync;
  `:1089` `SourceLoader` is not `Send` (threading constraint).
- `src/syntax/import.rs` — `:18,83,93,117,118` `ImportGraph`: cycle
  check, `unit_inputs` BFS closure, locator-keyed diamond dedup.
- `src/core/typecheck/check.rs:1149` — `type_check_full`.
- `src/eval/memory/heap.rs:8–11` — single-threaded non-`Sync`
  `UnsafeCell` heap (why eval resists the model).
- `docs/development/prelude-type-cache-spec.md` (TS-B7) — prelude-only
  type-summary cache (spec that preceded the 0.7.0 implementation);
  §B7.1 LSP-responsiveness framing; §B7.7 behaviour-preservation;
  §B7.9 user-unit out-of-scope rationale.
- [0004 — compiled-unit & prelude caching](0004-compiled-unit-caching.md);
  [0015 — parser error-recovery](0015-parser-error-recovery.md);
  [0008 — parallel & speculative evaluation](0008-parallel-evaluation.md);
  [0017 — REPL / notebook](0017-repl-notebook.md).
- Salsa — query model, red-green algorithm, durability:
  https://github.com/salsa-rs/salsa ·
  https://salsa-rs.github.io/salsa/reference/algorithm.html ·
  https://rust-analyzer.github.io/blog/2023/07/24/durable-incrementality.html
- Rustc query system / Salsa:
  https://rustc-dev-guide.rust-lang.org/queries/salsa.html
- Adapton — demanded computation graph, demand-driven incremental
  computation (Hammer, Acar et al.):
  https://www.cs.tufts.edu/~jfoster/papers/cs-tr-5027.pdf
- Acar, *Self-Adjusting Computation* (CMU-CS-05-129):
  https://www.cs.cmu.edu/~rwh/students/acar.pdf
