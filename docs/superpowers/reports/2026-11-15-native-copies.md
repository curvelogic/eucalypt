# Native containers: per-call `get` view and per-GC vec walk (eu-oaz72.52)

## Done
- `HeapNdArray::get` computes the row-major offset directly from the shape
  and bounds-checks each coordinate; it no longer builds an `ArrayViewD`
  (shape validation, stride allocation) on every call. Same result and
  `None` cases as before (rank mismatch, out of range); unit test compares
  against view indexing for every cell of a 2x3x4 array.
- `HeapVec` records `has_strings` at construction (vecs are immutable).
  GC scan and update skip the per-element walk when no element is a string;
  the backing buffer is still marked and evacuated. Sets are now dict trees
  (eu-oaz72.21), so finding (4) applies to vecs only.
- New bench `tests/harness/bench/025_array_grid_get.eu`: 10 sweeps of a
  60x60 grid read with `arr.get` (total checked against an independent sum).

## Measured (not claimed as a win)
Release binary before/after, same machine, interleaved, wall ms, 5 to 7 runs:
- 10 sweeps: before median 454, after 461 (ranges overlap).
- 150 sweeps: before median 4004, after 4041 (ranges overlap).
Ticks are identical (7,947,543; intrinsic internals are not ticked). Label:
measured-single; no difference distinguishable from noise. Evaluation
overhead per `arr.get` call (list build, `SeqNumList`, boxing) dwarfs the
view rebuild, so (2) is a correctness-neutral simplification, not a speedup.
(4) is not exercised by this bench (no vecs); no GC figure is claimed.

## Left, with reasons
- (1) Double copy via a Rust `Vec`/`ArrayD`: removing it needs a
  write-into-heap-buffer allocation API in `Array`, which is unsafe-adjacent
  and touches every ndarray intrinsic; not cheap. Follow-up bead suggested.
- (3) `set.sample` string comparison: sets are now dict trees; re-assess.
