# Architectural Decisions

This log records significant architectural decisions, their rationale,
and any future implications.

## ADR-001: Defer Persistent Block Representation (March 2026)

> **Amended 2026-11-12.** The finaliser-table option listed under
> Consequences is superseded by ADR-002: every heap value's storage lives
> in the GC heap, and no finalisers are added. Persistent blocks remain
> deferred. The new `dict` is a separate, opt-in GC-native ordered tree
> and does not change the block representation.

### Context

Eucalypt blocks (ordered key-value maps) are represented as cons-lists
of key-value pairs, giving O(n) lookup. For large blocks and
merge-heavy workloads, an O(log n) persistent data structure
(`im_rc::OrdMap`) was investigated as a replacement.

### Decision

**Deferred.** The persistent block implementation has been removed from
the codebase. The cons-list representation remains the sole block
representation.

### Rationale

A full implementation was developed across two phases:

1. **Phase 1-2 (infrastructure)**: `HeapBlock` type backed by
   `im_rc::OrdMap<SymbolId, BlockEntry>`, six `PBLOCK_*` intrinsics,
   GC scanning support, and an experimental `pb:` prelude namespace.
   This was merged to master as opt-in experimental code.

2. **Phase 3 (compiler switch)**: `compile_block()` modified to emit
   `PBLOCK_FROM_PAIRS` instead of `dsl::block()`, making persistent
   blocks the default for all block literals.

Phase 3 revealed a fundamental incompatibility with the garbage
collector:

- The GC uses a bump allocator with block-granular recycling. When a
  memory block is recycled, its cells are simply overwritten — no
  `Drop` is called on individual objects.

- `im_rc::OrdMap` uses reference-counted (`Rc`) internal nodes on the
  Rust heap, outside the GC's managed memory. When the GC recycles a
  cell containing a `HeapBlock`, the `OrdMap` and its `Rc` nodes are
  never freed, causing a memory leak proportional to the number of
  blocks created.

- This leak caused 220-580% regressions on GC-intensive benchmarks
  (thunk updates, short-lived allocations, fragmentation), even for
  workloads that do not use blocks heavily.

- An attempt to add finalisation (tracking `HeapBlock` pointers and
  dropping them during the mark phase) reduced regressions but did not
  eliminate them. The overhead of maintaining a finaliser list and
  scanning for dead blocks remained significant.

### Consequences

- Block lookup remains O(n). For most eucalypt programs this is
  acceptable as blocks are typically small.

- The `pb:` prelude namespace and `PBLOCK_*` intrinsics have been
  removed.

- Any future attempt to introduce persistent blocks must address the
  GC finalisation problem. Possible approaches:
  - Add a general finaliser mechanism to the GC (finaliser table
    scanned during sweep)
  - Use arena-allocated nodes instead of `Rc`-based `OrdMap`
  - Store the map data structure inline in the GC heap rather than on
    the Rust heap
  - Move to a different GC strategy that supports `Drop` (e.g.
    mark-sweep with per-object deallocation)

### Related

- Branch `feat/furnace-persistent-blocks-v2` preserves the full
  implementation for reference
- Beads: eu-smiz, eu-t5mc, eu-3gjv, eu-7enl, eu-mwys, eu-vdn2

## ADR-002: Every Heap Value's Storage Lives in the GC Heap (November 2026)

Supersedes the finaliser options in ADR-001's Consequences.

### Context

ADR-001 deferred persistent blocks because Rust-owned `Rc` nodes inside
GC heap objects were never freed: the collector recycles memory by
overwrite and never runs `Drop`. The same root cause remained in shipped
code. `HeapSet` (a Rust `HashSet`), `HeapVec` (a `Vec`, including owned
`String`s) and `HeapNdArray` own Rust allocations that leak when their
heap object is swept; set add/remove churn reached 1.79 GB RSS under a
512 MB managed heap (measured-single, eu-oaz72.5).

### Decision

**Every heap value's storage lives in the GC heap.** No GC heap object owns
a Rust-heap allocation (`Rc`, `Vec`, `HashMap`, `HashSet`, `String`, …),
persistent-collection crates (`im`, `rpds`, `imbl`) are not used for heap
values, and no finaliser mechanism is added — neither a general finaliser
table nor one restricted to a few types.

- Set, vec and n-d array storage moves into heap arrays (strings as heap
  string references, so vec contents are traced); large storage uses
  large-object blocks as usual (eu-oaz72.5).
- The new `dict` (eu-oaz72.21) is a persistent ordered tree whose nodes are
  traced GC heap objects. It is a separate, opt-in value type: blocks are
  unchanged, and sets are rebuilt on its nodes.
- Persistent blocks (ROADMAP DS1, eu-iv2j.1) remain deferred. When
  scheduled they choose their own structure under this rule; they may
  reuse the dict's nodes if that fits.

### Rationale

- Finalisation was measured as too costly for blocks (ADR-001), and a
  restricted finaliser table would still add a sweep-time cost and a
  second lifetime mechanism to reason about in a sensitive area.
- GC-native storage keeps one ownership model: everything reachable is
  marked, scanned and evacuated by the existing collector, and everything
  unreachable is reclaimed by it. The GC verification modes
  (`EU_GC_VERIFY`, `EU_GC_POISON`, `EU_GC_STRESS`) then cover it.

### Consequences

- Any future value type with variable-size storage (persistent vector
  eu-oaz72.23, arbitrary-precision integers eu-kc98.2, mutable storage
  eu-kc98.1) stores it in the GC heap.
- GC-area changes under this rule need two independent reviews and the GC
  verification modes.

### Related

- Plan: `docs/superpowers/plans/2026-11-12-aoc-2026-preparation.md`
- Beads: eu-oaz72.5, eu-oaz72.21, eu-iv2j.1, eu-oaz72.33
