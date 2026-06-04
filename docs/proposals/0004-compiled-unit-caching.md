# 0004 — Compiled-unit & prelude caching (CLI latency)

- **Status:** Draft proposal for review
- **Track:** B — performance, runtime & concurrency
- **Classification:** Whitespace
- **Suggested horizon:** 0.8
- **Related:** TS-B7 (shipped 0.7.0, `src/driver/check.rs`), sibling proposals
  [0014 — incremental, query-based core](0014-incremental-query-core.md),
  [0005 — generational GC](0005-generational-gc.md)

---

## Summary

Every invocation of `eu` re-parses, re-cooks, and re-compiles the
entire prelude from source, despite the prelude being a fixed,
binary-embedded asset that never changes within a given build. For a
CLI config tool used repeatedly in CI and scripts, this ~500–700 ms
compile-pipeline overhead dominates wall-clock time for small queries on
large data, dwarfing actual VM execution. In 0.7.0 the *type-side* of
this gap was closed: the prelude type-summary cache (TS-B7) now caches
the checker's `PreludeSummary` in `src/driver/check.rs` so that type
checking never re-walks the prelude. This proposal is the compiled-*code*
analogue — the remaining half of the caching story — and specifies a
content-addressed on-disk cache of compiled units keyed on source hash,
compiler version, and edition, serialising the `StgSyn` intermediate
representation. It also describes a path toward a persistent process
(daemon/server) as the more ambitious form of caching, and proposes
eliminating the redundant headless→RENDER_DOC double-compilation that
currently afflicts all plain-document programs.

---

## Motivation

### Measured pipeline overhead

The compile pipeline is constant regardless of query complexity. From
`docs/development/deep-find-performance-baseline.md`, measured on a
release build against ~1 MB fixtures:

| Phase | Time |
|-------|------|
| Parse | ~120 ms |
| Cook | ~190 ms |
| Eliminate | ~110 ms |
| STG compile | ~90 ms |
| **Total pipeline** | **~500–700 ms** |

A simple lookup (`data.total`) on a 1.15 MB input runs the VM in 1 ms.
The 500+ ms of compile overhead is therefore 500× the cost of the work
itself. For a CI pipeline that invokes `eu` ten times over a large
configuration tree, this is several seconds of wall-clock time that
carries no information.

### Why the prelude is the ideal target

The prelude (`lib/prelude.eu`, ~2,264 lines) is included verbatim in
every binary via `include_bytes!` in `src/driver/resources.rs:16`:

```rust
content.insert(
    "prelude".to_string(),
    String::from_utf8(include_bytes!("../../lib/prelude.eu").to_vec())
        …
);
```

It is loaded as `Locator::Resource("prelude")`, merged first as the
base of every invocation, and its bindings reference only the prelude
itself — no user file can rebind what the prelude's own definitions see.
These are exactly the properties that make it a sound cache target:

- **Fixed**: same bytes for a given binary; cannot change between
  invocations.
- **Self-contained**: the prelude's compiled output does not depend on
  the user's input files.
- **Dominant cost**: ~half the total pipeline budget is attributable to
  the prelude's parse and cook phases (parse = ~120 ms, cook = ~190 ms
  over ~2,200 lines of prelude + headers).

This is precisely the argument TS-B7 made for the *type-checking* side —
and TS-B7 shipped in 0.7.0 (`src/driver/check.rs:44`):

```rust
static PRELUDE_CACHE: OnceLock<PreludeSummary> = OnceLock::new();
```

The type-side cache avoids re-walking the ~2,200-line prelude on every
`eu check` invocation. This proposal caches the *compiled code* side;
the two are direct analogues and share the same keying rationale.
The runtime re-parse/cook/STG-compile path through `src/driver/eval.rs`
is entirely separate from the type-checker path and remains uncached:
`stg::compile` at `src/driver/eval.rs:187` runs unconditionally on every
invocation. The 0.7.0 HeapSyn-clone avoidance (~9% faster VM execution)
is orthogonal — it accelerates the VM tick loop, not the compile pipeline,
and does not reduce the ~500–700 ms startup overhead this proposal targets.

### The double-compilation problem

For plain-document programs (the overwhelmingly common case), `eu`
currently calls `stg::compile` twice (`src/driver/eval.rs:187` and
`src/driver/eval.rs:282`): once headless to detect IO versus
pure-document, and then again with `RenderType::RenderDoc` on a fresh
machine. The comment at `eval.rs:271–275` explains the reason — stale
string pointers from the first run cause GC crashes — but the
recompilation is itself ~90 ms of avoidable cost. Any caching scheme
must be aware of this; the fix is addressed in the design below.

---

## Prior art & landscape

### Rustc incremental compilation (query system)

Rustc's incremental backend (rustc-dev-guide §"Incremental compilation")
builds a dependency graph of queries: every access between query
functions is recorded, so the compiler knows exactly which cached
results are still valid. On re-run it replays the dep-graph, re-executes
only changed queries, and reads everything else from an on-disk
compilation database (the `incremental/` directory). The key insight is
the **red-green** algorithm: a query result is "green" (reusable) if
none of its transitive inputs changed hash.

What to borrow: the content-addressed key scheme (source hash + version
metadata) and the discipline of marking cache entries with a compiler
version to prevent silent stale-result bugs. What *not* to borrow: the
full query DAG is overkill for eucalypt's current architecture. The
prelude is the only unit that is genuinely stable across invocations;
user units change constantly. A simplified scheme that caches only the
prelude achieves most of the gain.

### Bazel action cache

Bazel computes a digest over an action's inputs (source files, command
line, environment) and looks up the result in a content-addressed store
(CAS). Matching entries are downloaded rather than recomputed; the CAS
can be remote (shared across a CI fleet) or local. The design is
explicitly build-tool-agnostic and has been standardised as the Remote
Execution API (REAPI).

What to borrow: the concept of the *action key* — (inputs hash +
tool/version identity + flags) → serialised outputs — maps directly
onto (prelude source hash + `eu` binary version + edition flag) →
serialised `StgSyn`. What *not* to borrow: remote CAS and REAPI
infrastructure are far beyond the scope of a CLI config tool's needs.
A local `~/.cache/eucalypt/` directory is sufficient.

### Bloop (Scala build server)

Bloop is a long-running daemon that keeps compiled Scala modules in
memory across multiple tool invocations. The first compile pays the
full cost; subsequent invocations (from the IDE, the CLI, or CI) reuse
the in-memory result. It communicates over the Build Server Protocol
(BSP). The daemon approach delivers sub-millisecond re-evaluation for
unchanged modules.

What to borrow: the observation that a long-running process eliminates
the serialisation round-trip entirely. Eucalypt's LSP server already
holds pipeline state across file changes; the same idea applies to a
`eu daemon` or `eu server` mode. This is the more ambitious path and is
addressed in the design below as Phase 2.

### Unison: the codebase *is* the cache

Unison's most radical idea is that definitions are identified by the
hash of their AST, and the "codebase" is a content-addressed store of
already-typechecked, already-compiled definitions. Because names are
just labels attached to hashes, renaming never invalidates the cache;
because definitions are identified by content, you never compile the
same code twice — ever, across all users and machines. The compilation
cache is not a build artefact; it *is* the program.

What to borrow: the insight that the cache key should be the *content
hash*, not the file path or a timestamp, so that cache hits are
semantically guaranteed, not merely "probably still valid". Eucalypt
should adopt content-hash keying rather than mtime-based invalidation.
What *not* to borrow (for now): making the cache the primary source of
truth for code identity would require eucalypt to adopt content-addressed
imports and a codebase-format store. That is a viable path but it is a
major architectural commitment, properly scoped to
[0018 — module & package system](0018-module-package-system.md) rather
than this proposal. Eucalypt should borrow Unison's *keying discipline*
without adopting its *storage model*.

---

## Proposed design

### Cache representation: StgSyn, not HeapSyn

There are two candidate serialisation targets:

| Representation | Pros | Cons |
|---|---|---|
| `HeapSyn` (in-heap, post-load) | One load step saved | Heap-bound; contains raw pointers; GC-tied; not serialisable |
| `StgSyn` (off-heap, `Rc<StgSyn>`) | Serialisable; GC-independent; avoids stale-pointer hazard | Must still load into heap on each run (~10 ms, acceptable) |

`HeapSyn` is unsuitable: `src/eval/memory/loader.rs` shows that loading
converts `StgSyn` references into raw heap pointers, interns symbols
into a `SymbolPool`, and allocates `HeapString` objects — the result
is inextricably bound to a live GC heap. Serialising or reusing
`HeapSyn` across process boundaries would reintroduce exactly the
stale-pointer hazards that motivated the `fresh machine` pattern in
`src/driver/eval.rs:271–287`.

`StgSyn` (`src/eval/stg/syntax.rs`) is an `Rc`-based tree with no raw
pointers. It can be serialised with `serde` + a binary format (e.g.
`bincode` or `postcard`). Cache retrieval replaces parse → translate →
merge → cook → eliminate → inline → STG-compile; the loader step
(`src/eval/memory/loader.rs::load`) still runs per invocation,
taking the clean `StgSyn` and building a fresh heap — preserving
GC safety and the `SymbolPool` contract.

### Cache key

```
key = sha256(prelude_source_bytes)
    ∥ eu_version_string          # from build-meta.yaml
    ∥ edition_flag               # "default" until 0001 lands
```

All three fields must match. A mismatch on any field is a cache miss,
triggering a full recompile and a new cache write. The source hash
alone would guard against stale prelude bytes; the version string
guards against compiler changes that alter `StgSyn` semantics or
serialised shape without touching the prelude source.

### On-disk layout

```
~/.cache/eucalypt/compiled-units/
  <key-hex>.stgsyn        # serialised StgSyn
  <key-hex>.meta          # JSON: {version, edition, source_hash, created_at}
```

The meta file enables inspection and programmatic pruning. No locking
protocol is required for the prelude cache: the prelude is immutable
within a build, so two concurrent `eu` processes writing the same key
are writing identical bytes — the last writer wins with no corruption
risk (provided atomic rename is used on write).

### Eliminating the double-compile

The headless→RENDER_DOC double-compile (`src/driver/eval.rs:163–295`)
exists because re-running the existing machine after IO detection causes
GC crashes from stale string pointers. With a cached `StgSyn` the fix
is straightforward: store both the headless-compiled and RenderDoc-compiled
variants under the same key, so on the next invocation neither
`stg::compile` call is needed. In practice, a given `eu` invocation
for a given input always follows the same path (IO or plain document);
the second compile disappears from the hot path after the first run.
A deeper restructure — removing the two-mode scheme entirely by using a
sentinel value — is a separate improvement the cache design should
accommodate but not require.

### `--no-cache` and cache-dir configuration

- `--no-cache`: bypass cache reads and writes entirely. Useful for
  debugging or when the cache directory is on a slow filesystem.
- `--cache-dir <path>`: override `~/.cache/eucalypt/`. Useful in CI
  environments where `$HOME` is ephemeral but a shared cache volume is
  mounted elsewhere.
- `EU_CACHE_DIR` environment variable: equivalent to `--cache-dir` for
  scripting.
- Cache entries are never automatically evicted (the prelude is tiny
  serialised); a `eu cache clean` subcommand provides manual control.

### Phase 2: persistent process (daemon/server)

A daemon that loads and compiles the prelude once on first connect, then
serves subsequent invocations over a socket (similar to Bloop), would
eliminate even the `StgSyn` deserialisation step. The `eu` CLI would
act as a thin client: send source bytes, receive rendered output. The
in-memory prelude `StgSyn` (and potentially the loaded `HeapSyn`) would
persist across invocations.

The LSP server (`src/driver/lsp/`) already holds pipeline state across
edits — it is effectively already a long-running process serving
incremental re-evaluations. The daemon mode would generalise this to
CLI usage, sharing the same infrastructure. See
[0014 — incremental, query-based core](0014-incremental-query-core.md)
for the fuller vision; the prelude cache proposed here is a prerequisite
and a natural precursor.

The daemon introduces operational complexity (process lifecycle,
socket hygiene, CI environments that do not persist across steps) that
makes it unsuitable as the primary deliverable for 0.8. The on-disk
`StgSyn` cache is the pragmatic first step.

---

## Interaction with the existing roadmap

**TS-B7 (prelude type-summary cache — shipped 0.7.0)**: B7 landed in
0.7.0 as `static PRELUDE_CACHE: OnceLock<PreludeSummary>` in
`src/driver/check.rs:44`. It caches the type-checker's `PreludeSummary`
(binding types + aliases); this proposal caches the compiled `StgSyn`.
They are parallel caches for parallel pipeline stages, and the shipped
TS-B7 implementation sets a direct keying-discipline precedent: the
cache is process-scoped, built once on first use, and keyed implicitly
on the binary (same prelude bytes, same build). The natural question for
the on-disk `StgSyn` cache is whether they can share a formal key
struct. The answer is yes: both would use `sha256(prelude_source) ∥
eu_version ∥ edition`. A shared `PreludeCacheKey` struct in
`src/driver/` would serve both, and the two on-disk cache files could
sit alongside each other under the same hash prefix. Neither depends on
the other at the API level: TS-B7 serves the type-checker path;
this proposal serves the eval path. They are implemented independently
but should coordinate on the key format.

**0014 — incremental, query-based core**: 0014 proposes a Salsa-style
query engine for the LSP and re-evaluation. The prelude cache proposed
here is a simpler, non-incremental subset: it caches one unit (the
prelude) whose content never changes within a session. When 0014 lands,
the prelude cache becomes one entry in the broader query database — the
prelude's compiled form is the base case of the incremental graph.
There is no conflict; this proposal creates a cache entry that 0014
will eventually subsume.

**0005 — generational GC**: the GC work does not interact with the
cache directly. However, if the compile-pipeline overhead falls by
500 ms via caching, the remaining cost profile shifts toward VM
execution, making 0005's GC improvements proportionally more visible.
Implement 0004 first, then profile to confirm 0005 targets correctly.

**0001 — 1.0 charter / editions**: the edition field in the cache key
anticipates the edition mechanism from 0001. Until editions exist the
field is a constant (`"default"`); it is reserved so that the key
format does not need to change when 0001 ships.

---

## Implementation sketch

### Phase 1: on-disk StgSyn cache (0.8)

**Files changed:**

| File | Change |
|------|--------|
| `Cargo.toml` | Add `serde` feature on `stg::syntax` types; add `bincode` or `postcard` |
| `src/eval/stg/syntax.rs` | Derive `Serialize`, `Deserialize` on `StgSyn`, `LambdaForm`, `Ref`, `Native` |
| `src/driver/cache.rs` *(new)* | `CacheKey` struct; `CompiledUnitCache`: read/write `StgSyn` to `~/.cache/eucalypt/`; atomic write via temp-file rename |
| `src/driver/eval.rs` | Before `stg::compile`, check cache; on miss, compile and write; on hit, deserialise and skip pipeline |
| `src/driver/options.rs` | Add `--no-cache`, `--cache-dir` flags |

**Risk**: `StgSyn` serialisation is the main unknown. The type is
`Rc`-shared, so a serialisation library must handle DAG structure
(shared nodes). `bincode` does not; `serde` with a custom visitor or
`postcard` with index-based flattening can. The simplest approach is to
clone into a tree (no sharing) before serialising — acceptable given the
prelude is small. Estimate: 3–5 days for a working prototype; 1–2 weeks
to production quality with tests.

**Size/risk rating**: medium. The cache logic is self-contained and
off the critical path when disabled; failure modes are a slow
`--no-cache` fallback, never a crash.

### Phase 2: daemon mode (post-0.8)

Extend the LSP server infrastructure in `src/driver/lsp/` to accept
non-LSP evaluation requests. Add `eu daemon start/stop/status` and a
thin client protocol. Estimate: 2–4 weeks; gated on 0014's query
infrastructure.

---

## Alternatives considered

**Mtime-based invalidation**: simpler to implement but fragile — a
file copied from a build artefact has a new mtime without changed
content; a prelude that is regenerated to identical bytes would
invalidate the cache. Content hashing costs one `sha256` call (~1 ms
for 2,200 lines) and is strictly more correct.

**Caching user units as well**: user files are typically short, the
savings are small, and soundness is harder to establish under positional
merge-override (the same argument TS-B7 makes for excluding user units
from the type-summary cache). Exclude for now; revisit when 0014's
module graph makes per-unit stability analysable.

**Caching `CoreExpr` (post-cook, pre-STG)**: saves the STG-compile
step but not parse/cook, which dominate. `CoreExpr` is also larger and
harder to serialise than `StgSyn`. Not worth the trade-off.

**Build-time embedding**: compute the prelude's `StgSyn` at `cargo build`
time and embed via `include_bytes!`. Attractive in principle but
entangles the build system with the runtime serialisation format and
requires re-embedding on every toolchain update. An on-disk cache gives
the same steady-state performance with simpler build machinery.

---

## Risks & what would kill this

1. **StgSyn serialisation complexity**: if the `Rc`-sharing in `StgSyn`
   is pervasive enough that a tree clone is semantically wrong (e.g.
   back-references or cycles), the naive clone approach fails. Audit
   `src/eval/stg/syntax.rs` before committing to this approach.

2. **Cache poisoning / correctness**: a serialisation bug that produces
   a subtly wrong `StgSyn` could cause silent incorrect output. Mitigate
   with a round-trip test suite: serialise, deserialise, compare against
   the freshly-compiled result.

3. **Key collision**: SHA-256 truncated to a filename is collision-free
   in practice; use the full 32 bytes (64 hex chars) to be safe.

4. **CI cache isolation**: many CI systems run jobs in ephemeral
   containers with no shared home directory. The on-disk cache delivers
   no benefit unless the cache directory is on a persisted or shared
   volume. Document this explicitly; the `EU_CACHE_DIR` override is the
   escape hatch.

5. **The double-compile does not disappear automatically**: caching
   `StgSyn` allows caching the headless-compiled and RenderDoc-compiled
   variants separately, but the trigger logic in `src/driver/eval.rs`
   must be updated to use the cache for the second compile. If that
   change is not made, the second `stg::compile` call remains.

---

## Success criteria

- **Latency**: repeated `eu` invocations on a fixed input and fixed
  binary show ≥ 400 ms wall-clock reduction (from ~600 ms to ≤ 200 ms)
  after the first (cache-priming) run, measured with `hyperfine` on a
  release build.
- **Correctness**: all harness tests (`cargo test --test harness_test`)
  pass identically with the cache enabled and disabled. A dedicated
  round-trip test confirms `deserialise(serialise(stgsyn)) == stgsyn`
  for the prelude.
- **Cache safety**: `--no-cache` disables the cache entirely; the
  output is bit-for-bit identical to the cached path.
- **Key integrity**: changing a single byte of `lib/prelude.eu`
  (without a binary rebuild) causes a cache miss, not a silent stale
  hit.
- **Daemon mode (Phase 2)**: first invocation after daemon start
  ≤ 50 ms for a trivial query on a 1 MB input, measured with `hyperfine`.

---

## References

- `docs/development/deep-find-performance-baseline.md` — pipeline timing
  table (Parse ~120, Cook ~190, Eliminate ~110, STG ~90 ms).
- `src/driver/check.rs:44` — `static PRELUDE_CACHE: OnceLock<PreludeSummary>`:
  the shipped TS-B7 type-side prelude cache (0.7.0); keying rationale
  and out-of-scope analysis apply directly to this proposal.
- `src/driver/resources.rs:16` — `include_bytes!("../../lib/prelude.eu")`.
- `src/driver/eval.rs:163–295` — headless→RENDER_DOC double-compilation
  and the "stale string pointers" comment at `eval.rs:271–275`.
- `src/eval/memory/loader.rs` — `load()`: `StgSyn` → `HeapSyn`
  translation, symbol interning, `HeapString` allocation; explains why
  `HeapSyn` cannot be cached across processes.
- `src/eval/stg/mod.rs:310–324` — `stg::compile` signature; takes
  `RcExpr` and `&dyn Runtime`, returns `Rc<StgSyn>`.
- Rustc developer guide — "Incremental compilation in detail":
  https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation-in-detail.html
- Bazel remote caching — action cache and CAS:
  https://bazel.build/remote/caching
- Bloop build server:
  https://scalacenter.github.io/bloop/
- Unison — the big idea (codebase as cache):
  https://www.unison-lang.org/docs/the-big-idea/
