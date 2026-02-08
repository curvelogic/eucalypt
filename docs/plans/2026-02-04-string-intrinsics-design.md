# String Intrinsics Optimisation — Design

## Problem

String intrinsics that return lists (SPLIT, MATCH, MATCHES, LETTERS)
materialise the entire result as a `Vec<String>` in Rust, then copy
every string onto the heap. This creates N intermediate Rust `String`
allocations that are immediately discarded after being copied to the
heap.

## Approach: Two Phases

**Phase 1 (now):** Eliminate the intermediate `Vec<String>` by
providing a streaming return path. Instead of collecting all results
into a Vec then building the heap list, build the heap list directly
from an iterator. The input string is still copied from the heap into
Rust via `str_arg()`.

**Phase 2 (after GC evacuation):** Add a block pinning mechanism to
the GC, then introduce `str_arg_ref()` which returns a `&str`
borrowed directly from the heap. This eliminates the input copy.
Requires pinning because evacuation (GC phase 3) can move objects,
and concurrent mutators could trigger GC while the intrinsic holds the
borrow.

## Why Not Zero-Copy Now?

During intrinsic execution, output allocations can trigger GC. With
the planned evacuation optimisation (eu-181d phase 3), GC could move
the input string while the intrinsic holds a `&str` into it. A
pinning mechanism is needed to prevent this, and that belongs in the
GC epic. Future concurrency would also require pinning for the same
reason.

## Phase 1: Streaming Return

### New helper: `machine_return_str_iter()`

Replace the current `machine_return_str_list(machine, view, vec)`
pattern with `machine_return_str_iter(machine, view, iter)` that
accepts an iterator and builds the heap cons list directly.

The current `machine_return_str_list` builds the list in reverse from
a Vec. The streaming version collects the iterator into a temporary
`Vec<Ref>` of heap-allocated string refs (small pointers, not full
strings), then builds the cons list in reverse as now. This eliminates
the `Vec<String>` — the strings go straight from the iterator to the
heap.

### Intrinsic migration

| Intrinsic | Current | After |
|-----------|---------|-------|
| SPLIT | `re.split().map(to_string).collect()` then `machine_return_str_list(vec)` | `re.split()` fed to `machine_return_str_iter(iter)` |
| MATCH | `re.captures().map(to_string).collect()` then `machine_return_str_list(vec)` | `re.captures()` fed to `machine_return_str_iter(iter)` |
| MATCHES | `re.find_iter().map(to_string).collect()` then `machine_return_str_list(vec)` | `re.find_iter()` fed to `machine_return_str_iter(iter)` |
| LETTERS | `chars().map(to_string).collect()` then `machine_return_str_list(vec)` | `chars()` fed to `machine_return_str_iter(iter)` |

JOIN, UPPER, LOWER, FMT, STR produce single strings — no change
needed.

## Phase 2: Zero-Copy Input (After GC Epic)

### Block pinning mechanism

Add a pin/unpin API to `MutatorHeapView`:

- `pin(ref)` returns a `PinGuard` — marks the ref's block as
  non-evacuatable
- Guard unpins on drop (RAII pattern)
- Evacuation skips pinned blocks

### New helper: `str_arg_ref()`

Returns `&str` borrowed directly from the heap string, with the block
pinned for the duration. The intrinsic flow becomes:

```
pin input block → &str from heap → regex → &str slices → heap allocations → drop pin
```

Zero intermediate Rust `String` allocations for the entire pipeline.

**Dependency:** Blocked by GC Immix phase 3 (evacuation), since
pinning is only necessary once objects can move.

**Decision needed**: The pinning API must be designed explicitly before
phase 2 work begins. The GC Immix design does not yet specify the
pin/unpin API. Key questions: RAII guard lifetime, nesting semantics
(can an intrinsic allocate while holding a pin?), interaction with
evacuation's reference-updating scan.

## Out of Scope

- JOIN optimisation (single string output, already efficient)
- UPPER/LOWER/FMT/STR (single string output)
- General heap-backed allocation for Rust structs (separate future
  concern)

## Testing

- Phase 1: Existing harness tests cover all string intrinsics. Verify
  no regressions. Use bench targets (once eu-01v lands) to measure
  ticks/allocs reduction.
- Phase 2: GC stress tests with string-heavy workloads to verify
  pinning works under evacuation.
