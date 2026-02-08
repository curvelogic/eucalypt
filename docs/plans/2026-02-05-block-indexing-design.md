# Block Indexing — Design

## Problem

Block lookup is O(n) — LOOKUPOR walks a cons-list comparing keys on each
access. For medium/large blocks with repeated lookups, this is slow.

## Solution

Lazy indexing — blocks build a hash index on first lookup if they exceed a
size threshold. Index is stored in the heap alongside the block.

## Key Properties

- **Transparent to users** — no API changes
- **Construction unchanged** — blocks still built as lists
- **Small blocks** (< threshold) use linear search — fast enough
- **Large blocks** build index on first lookup — O(1) thereafter
- **Purely functional** — new blocks from merge start without index
- **Depends on eu-4af** (symbol interning) for efficient `HashMap<SymbolId, usize>`

## Heap Representation

**Current Block structure**:
```
Block(list)  — single field pointing to cons-list of key-value pairs
```

**New Block structure**:
```
Block(list, index)  — two fields:
  - list: cons-list of key-value pairs (unchanged)
  - index: null OR pointer to NativeIndex
```

**NativeIndex**: A new Native variant holding `HashMap<SymbolId, usize>`:
- Key: interned symbol ID
- Value: position in the list (0-indexed)

**Threshold**: 16 keys. Blocks with fewer than 16 elements use linear
search; 16+ elements trigger index building on first lookup.

## Index Building

On first LOOKUPOR call, if block has 16+ elements and no index:

1. Walk the list once, recording (symbol_id → position) mappings
2. Allocate NativeIndex on heap
3. Store reference in block's index field
4. Use index for this and subsequent lookups

## LOOKUPOR Implementation

**Current flow** (in `src/eval/stg/block.rs`):
1. Unbox the key symbol
2. Walk list with `MatchesKey` on each element
3. Return value when found, or default if exhausted

**New flow**:
1. Check if block has index
2. If indexed: hash lookup → position → fetch value at position
3. If not indexed:
   - If size < threshold: linear search (as now)
   - If size >= threshold: build index, store in block, then hash lookup

**Position-based access**: The index maps symbol → position. Initial
implementation walks list to position. Array-based optimisation can follow
if profiling shows benefit.

**LOOKUP**: Delegates to LOOKUPOR with a panic default, so gets indexing
for free.

## Dependencies

- **eu-4af (symbol interning)** — required for `HashMap<SymbolId, usize>`.
  Without interning, we'd hash strings which is slower.

## Testing

**Correctness**: Existing harness tests should pass unchanged — behaviour
is identical, only performance differs.

**Threshold boundary**: Test blocks with 15, 16, 17 keys.

**Repeated lookups**: Verify second lookup is faster (index built).

**Miss behaviour**: Lookup of non-existent key returns default.

**After merge**: Merged block lookups work (new block, no index).

## Benchmarking

- Lookup time for blocks of various sizes (10, 50, 100, 500 keys)
- Single lookup vs repeated lookups on same block
- Before/after comparison for representative workloads

## Out of Scope

- Array-based positional access (future optimisation)
- Index for blocks below threshold
- Explicit IOSM API (prelude already has `iosm` namespace for explicit use)
