# Deep Find — Data Querying for Eucalypt

## Problem

Navigating deeply nested data in eucalypt requires spelling out full
paths. When exploring unfamiliar JSON/YAML or extracting values from
complex structures, this is tedious and brittle:

```
data.response.body.items.0.pricing.price
```

## Design Decisions

### No root reference

We deliberately do not provide a way to reference the entire top-level
namespace (the feature proposed in eu-lt0). Users must use named
imports, e.g. `data=response.json`, keeping the reachability scope
bounded. This preserves the compiler's ability to perform dead code
elimination — a root reference would hold everything live and defeat
DCE entirely.

Since eucalypt intentionally blurs the line between data and code
(blocks can contain lambdas, YAML files can contain executable
eucalypt), any reference to the root is effectively a reference to all
code. Named imports are sufficient and DCE-safe.

### Library functions, not syntax

Deep find is implemented as prelude library functions, not new syntax.
This avoids grammar changes, is consistent with eucalypt's existing
style, and can be extended incrementally. Note that `//` syntax would
conflict with existing metadata/assertion functionality.

### Prelude first, intrinsics if needed

Initial implementation is pure eucalypt using existing block
intrinsics (`elements`, `keys`, recursive walk). Rust intrinsics are a
future optimisation only if profiling shows performance problems on
realistic data sizes.

## Phase 1 — Core Functions

Three prelude functions for searching block structures by key name at
any depth:

- **`deep-find(key)`** — returns a list of all values matching the
  key, depth-first document order
- **`deep-find-first(key, default)`** — returns the first match, or
  the default if none found (follows the `lookup-or` pattern)
- **`deep-find-paths(key)`** — returns a list of paths to matching
  keys

All operate on a named import or any block value.

```bash
eu data=response.json -e "data deep-find(\"price\")"
# [10, 20, 30]

eu data=response.json -e "data deep-find-first(\"email\", \"unknown\")"
# "alice@example.com"
```

### Traversal

Depth-first, document order — matches appear in the order their keys
would be encountered reading the source top to bottom.

## Phase 2 — Query String DSL

A `deep-query` function that accepts a simple path expression as a
string, parsed at runtime:

```
data deep-query("items.*.price")
```

### Query language (minimal)

| Pattern      | Meaning                                        |
|--------------|------------------------------------------------|
| `foo`        | Match key `foo` at any depth (same as `deep-find`) |
| `foo.bar`    | Match `bar` directly inside `foo`, at any depth |
| `foo.*.bar`  | Match `bar` inside any direct child of `foo`   |
| `foo.**.bar` | Match `bar` at any depth inside `foo`          |

`*` matches a single level. `**` matches zero or more levels. Bare
`foo` is sugar for `**.foo`.

### Variants

- **`deep-query(pattern)`** — list of values
- **`deep-query-first(pattern, default)`** — first match or default
- **`deep-query-paths(pattern)`** — list of paths

### Implementation

Phase 2 is a pure eucalypt implementation that parses the pattern
string into segments, then uses the phase 1 primitives as building
blocks for the recursive walk. Pattern parsing happens on each call —
no caching. A Rust intrinsic (phase 3) could follow if profiling shows
the string parsing overhead matters in hot loops.

## Phase 3 — Rust Intrinsics (if needed)

Only pursued if profiling shows performance problems. Would replace
the prelude implementations with equivalent Rust intrinsics registered
via the standard intrinsic catalogue.

## Interaction with Dead Code Elimination

These functions do not worsen DCE because they operate on named
imports or explicit block values — the compiler already considers
everything reachable from that binding as live.

eu-8d8 (dead code elimination inside blocks) remains an independent
compiler concern. The deep-find functions are consumers of block data,
not a factor in what the compiler can prune.

## Testing

- Harness tests with nested JSON/YAML fixtures
- Edge cases: empty blocks, lists within blocks, no matches, duplicate
  keys at different depths
- Performance: test against a moderately large JSON document (~1MB) to
  establish baseline
