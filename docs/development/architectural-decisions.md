# Architectural Decisions

This log records significant architectural decisions, their rationale,
and any future implications.

## ADR-001: Defer Persistent Block Representation (March 2026)

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
