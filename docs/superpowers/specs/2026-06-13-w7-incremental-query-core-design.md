# W7: Incremental, Query-Based Core

- **Date:** 2026-06-13
- **Status:** Draft
- **Bead:** eu-kgsi.2
- **Roadmap:** W7 — Incremental, query-based core

---

## 1. Problem

The LSP re-runs the entire front-end pipeline on every keystroke (after a
300ms debounce): load the prelude from source, parse, translate, merge, cook,
eliminate, type-check — even when only a single character changed in one file.
The prelude alone accounts for the majority of this work, but even with W6
eliminating prelude compilation, every pipeline pass re-runs from scratch on
every edit.

For multi-file workspaces, editing an imported file does not trigger
re-checking of files that depend on it. There is no dependency tracking, no
memoisation of intermediate results, and no invalidation discipline.

The CLI is one-shot and gets its latency win from W6. This work is
**primarily an LSP optimisation**.

## 2. Goal

A demand-driven, memoised query graph over the front-end pipeline so that an
edit to one file re-runs only the queries whose inputs actually changed. Parse
and desugar results for unchanged files are reused; downstream passes re-run
only when their inputs (content hashes of upstream results) differ.

## 3. Design

### 3.1 Query layer

A bespoke, single-threaded memoisation framework modelled on Salsa's semantics
but without the `Send`/`Sync` requirement. The core types are `Rc`-based
(`RcExpr`, `StgSyn`) and not `Send`; a bespoke layer avoids forcing a
migration to `Arc` that would be needed for Salsa adoption.

The query layer provides:

- **Content-keyed memoisation**: each query's result is stored alongside the
  content hash of its inputs. On re-execution, if all input hashes match, the
  cached result is returned.
- **Dependency tracking**: when query A calls query B during execution, the
  dependency is recorded. When B's result changes, A is invalidated.
- **Revision counter**: a global revision number increments on each input
  change (file edit). Queries are re-verified (inputs re-hashed) lazily on
  access, not eagerly on change.

The query signatures are designed so that Salsa could replace this layer
later (e.g. for W21 parallel evaluation) as a refactor, not a redesign.

**Estimated size**: ~500–800 lines for the core framework.

```rust
/// A content hash identifying a query result.
type ContentHash = u64;

/// Global revision counter, incremented on each input change.
type Revision = u64;

struct QueryStore {
    revision: Revision,
    /// Per-query-key cached results with their input hashes and
    /// the revision at which they were last verified.
    entries: HashMap<QueryKey, CachedEntry>,
}

struct CachedEntry {
    /// Hash of all inputs at computation time.
    input_hash: ContentHash,
    /// Revision at which this entry was last verified still-valid.
    verified_at: Revision,
    /// The cached result (type-erased, downcast on access).
    value: Box<dyn Any>,
    /// Keys of queries this entry depends on.
    dependencies: Vec<QueryKey>,
}
```

### 3.2 Query keys and the file model

The primary input to the query graph is a **file**: its path (or URI) and
content. Files are the unit of invalidation — when a file's content hash
changes, all queries keyed on that file are invalidated.

```rust
/// Identifies a file in the query store.
#[derive(Clone, Hash, Eq, PartialEq)]
struct FileId {
    /// Canonical path or URI.
    uri: Url,
}

/// Input query: the source text of a file.
/// Set externally by the LSP on `didChange`.
fn file_text(store: &QueryStore, file: FileId) -> Arc<String>;
```

`Arc<String>` is used for file text (the one exception to the `Rc` rule)
because it is a leaf input, never embedded in the `Rc`-based expression
types, and sharing the text across the query boundary avoids cloning large
buffers.

### 3.3 Query definitions

Each front-end pass becomes a query. Queries form a DAG:

```
file_text(file)                              [input, set by LSP]
    │
    ▼
parse(file) → SyntaxTree                     [keyed on hash(file_text)]
    │
    ▼
desugar(file) → CoreExpr                     [depends on parse]
    │
    ├──▶ unit_interface(file) → UnitInterface [depends on desugar]
    │        │
    │        ▼
    │    import_interfaces(file) → Vec<UnitInterface>
    │        │                               [depends on unit_interface
    │        │                                of each transitive import]
    │        ▼
    ▼
merge(file) → CoreExpr                       [depends on desugar(file)
    │                                         + import_interfaces(file)
    │                                         + prelude blob]
    ▼
cook(file) → CoreExpr                        [depends on merge
    │                                         + prelude operators]
    ▼
eliminate(file) → CoreExpr                   [depends on cook]
    │
    ▼
inline(file) → CoreExpr                     [depends on eliminate]
    │
    ▼
type_check(file) → TypeCheckResult          [depends on inline
    │                                        + prelude type summary]
    ▼
diagnostics(file) → Vec<Diagnostic>         [depends on parse errors
                                             + type_check warnings]
```

**Note on the prelude**: the prelude is **not** a query input. Its operators,
type summary, and compiled STG come from W6's pre-compiled blob, injected as
static configuration into the query store. When the prelude changes (blob
regenerated), the entire query store is invalidated (new binary).

### 3.4 Content hashing

Each query result is hashed to detect whether downstream queries need to
re-run. Two strategies:

- **Input queries** (`file_text`): hash the raw bytes (fast, SHA-256 or
  xxhash).
- **Derived queries** (`parse`, `desugar`, etc.): hash the structural content
  of the result. For core expressions this means a recursive hash of the
  `RcExpr` tree. The hash is computed once when the query produces a result
  and stored alongside it.

A derived query is **re-verified** (not re-executed) when its revision is
stale: re-hash its inputs, compare to the stored input hash. If unchanged,
bump `verified_at` to the current revision and return the cached result. If
changed, re-execute.

**Parse change detection**: the LSP already compares green nodes to detect
structural parse changes (`mod.rs` lines 737–760). This serves as a fast
path: if the green node is identical, skip even the hash comparison for
downstream queries.

### 3.5 Cross-file invalidation

When file B is edited and file A imports B:

1. `file_text(B)` changes → `parse(B)` re-runs → `desugar(B)` re-runs →
   `unit_interface(B)` re-runs.
2. `import_interfaces(A)` depends on `unit_interface(B)`. On A's next access,
   re-verification finds B's interface hash changed → `merge(A)` re-runs →
   cook/eliminate/inline/type_check for A re-run.

This requires **reverse dependency tracking**: the query store knows that
`import_interfaces(A)` depends on `unit_interface(B)`, so when B changes, A's
downstream queries are marked for re-verification.

**Import discovery**: the set of files that A imports is itself derived from
`parse(A)` (import declarations are in the syntax tree). A new import triggers
a new `unit_interface` dependency; a removed import drops the dependency. This
is handled naturally by re-recording dependencies on each re-execution of
`import_interfaces`.

### 3.6 Integration with the LSP

The `QueryStore` is a new field on `ServerState`, replacing the current
`cached: HashMap<Url, CachedPipeline>` and parts of `last_green` /
`last_parse_errors`.

**On `didChange`**:
1. Update `file_text(uri)` in the query store, bumping the global revision.
2. After debounce, request `diagnostics(uri)`.
3. The query layer lazily re-verifies the chain, re-executing only what
   changed.
4. Also request `diagnostics` for files known to import the changed file
   (reverse dependency lookup from the query store).

**On LSP requests** (hover, completion, go-to-definition):
These currently use the `CachedPipeline`'s type env and source map. They
become queries themselves or consumers of the `type_check` query's result.
The details are an implementation concern — the key property is that they
read from the query store rather than triggering a full pipeline run.

### 3.7 What the CLI does

The CLI does not use the query store. It runs the pipeline once (one-shot)
using W6's pre-compiled prelude. The pipeline functions are the same code
that the queries wrap, so there is no duplication.

When `eu watch` (W19) lands, it will create a long-lived `QueryStore` to
benefit from cross-invocation caching — but that is not in scope for 0.9.

### 3.8 What this does NOT do

- **Incremental evaluation**: the VM runs from scratch on each invocation.
  Incremental evaluation (re-running only the parts of the VM affected by a
  change) is post-1.0.
- **Incremental STG compilation**: STG compilation runs on the full merged
  expression. Per-file STG compilation requires general separate compilation
  (post-1.0).
- **Multi-threading**: the query store is single-threaded. Parallel query
  evaluation is a post-1.0 concern (W21).
- **Salsa adoption**: the bespoke layer is designed with compatible semantics
  so Salsa could replace it later, but this work does not adopt Salsa.

## 4. Implementation sketch

### Phase 1: Query framework
- Implement `QueryStore`, `CachedEntry`, revision tracking, dependency
  recording, and re-verification logic.
- Content hashing utilities for file text and core expressions.
- Unit tests for the framework: basic memoisation, invalidation, diamond
  dependencies.

### Phase 2: Input and parse queries
- Define `file_text` and `parse` queries.
- Integrate with `ServerState`: `didChange` updates `file_text`.
- Integrate the existing green-node comparison as a fast path.

### Phase 3: Desugar and interface queries
- Define `desugar`, `unit_interface`, `import_interfaces` queries.
- Import discovery from the parse tree.
- Reverse dependency tracking for cross-file invalidation.

### Phase 4: Merge and downstream queries
- Define `merge`, `cook`, `eliminate`, `inline`, `type_check` queries.
- Prelude operators and type summary injected as static config (from W6 blob
  or source prelude fallback).
- `diagnostics` query aggregating parse errors and type-check warnings.

### Phase 5: LSP integration
- Replace `CachedPipeline` with query-store reads.
- Wire `didChange` to set `file_text` + request diagnostics.
- Wire cross-file invalidation: on change to file B, re-request diagnostics
  for files importing B.
- Wire hover/completion/go-to-definition to read from query results.

### Phase 6: Validation
- Diagnostic parity: identical warnings/errors compared to the current
  pipeline on the full test harness.
- Edit simulation tests: apply a sequence of edits, verify that only the
  expected queries re-execute.
- Multi-file tests: edit an imported file, verify the importer's diagnostics
  update.
- Performance: measure LSP response latency on a large workspace before/after.

## 5. Dependencies

- **W6 (pre-compiled prelude)**: the prelude's operators and type summary are
  injected as static config, not as queries. W7 works without W6 (the prelude
  would just be another set of queries), but W6 eliminates the dominant cost
  and simplifies the query graph.
- **W2 (Unit Interface)**: already shipped in 0.7.1. The `UnitInterface`
  struct is the cross-file boundary the query graph consumes.

## 6. Test plan

- **Diagnostic parity**: run `eu check --strict` on the full harness and
  compare output to the current pipeline. Results must be identical.
- **Memoisation correctness**: a test that edits file A, verifies that
  `parse(B)` is NOT re-executed (for unrelated file B).
- **Cross-file invalidation**: edit file B (imported by A), verify A's
  diagnostics update and B's parse/desugar re-run.
- **New import**: add an import to file A, verify the new file's interface
  enters the dependency graph.
- **Removed import**: remove an import from file A, verify the old file's
  interface is no longer a dependency.
- **Shadowing**: a file that redefines a prelude name produces correct
  diagnostics after incremental re-check.
- **Content-identical edit**: an edit that produces the same parse tree
  (e.g. adding then removing a space) does NOT trigger downstream queries.
- **LSP responsiveness**: measure time-to-diagnostics on a multi-file
  workspace with repeated edits.

## 7. Risks

- **Content hashing cost**: hashing large `RcExpr` trees on every query
  verification could be expensive. Mitigate: use the green-node comparison as
  a fast path for parse; consider caching hashes on `RcExpr` nodes
  (hash-consing).
- **Merge as bottleneck**: the merge query depends on all imports' interfaces.
  In a large workspace, a change to a widely-imported file invalidates many
  merge queries. Mitigate: this is correct behaviour (those files genuinely
  need re-checking); the per-file early-pass caching still saves work.
- **Dependency tracking overhead**: recording and checking dependencies adds
  bookkeeping to every query. Mitigate: the number of queries per file is
  small (~8) and the number of files in typical eucalypt workspaces is modest
  (tens, not thousands).
- **Framework complexity**: a bespoke query layer is non-trivial to get right
  (cycle detection, partial re-verification). Mitigate: keep it simple —
  single-threaded, no cycles (the pipeline is a DAG), no parallel
  verification.

## 8. Success criteria

- **Unchanged-file queries are not re-executed**: editing file A does not
  re-parse or re-desugar file B.
- **Diagnostic latency drops** on repeated edits to a single file in a
  multi-file workspace (the prelude and unchanged imports are cached).
- **Cross-file diagnostics update correctly**: editing an imported file
  triggers re-checking of importers.
- **Diagnostic parity**: identical results to the current full-pipeline
  approach across the harness.
- **No regression** in single-file LSP responsiveness compared to today.
