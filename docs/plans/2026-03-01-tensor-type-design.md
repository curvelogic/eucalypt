# N-Dimensional Array (Tensor) Native Type Design

**Bead:** eu-atzn
**Date:** 2026-03-01
**Status:** Approved

## 1. Overview

Add n-dimensional numeric arrays as a native type in eucalypt, backed by
`ndarray::ArrayD<f64>`. Arrays provide O(1) element access for 1D and 2D
data (grids, matrices, vectors) while supporting arbitrary dimensionality.
Arithmetic operators dispatch polymorphically on arrays via the existing
intrinsic infrastructure.

### Goals

- O(1) indexed access to 1D and 2D numeric data (primary use case: AoC
  grid puzzles)
- Element-wise arithmetic via existing `+`, `-`, `*`, `/` operators
- Broadcasting (scalar-array, compatible-shape array-array)
- Generality to n-dimensional arrays without sacrificing 1D/2D performance
- WASM-safe implementation (no BLAS, no C deps)

### Non-Goals

- Integer arrays (f64-only for now)
- GPU acceleration
- Sparse arrays
- New literal syntax (construction via functions; idiot brackets can
  provide sugar later)

## 2. Data Representation

### Backing Library

`ndarray` crate (v0.17+), using `ArrayD<f64>` (dynamic-rank,
f64-element). Selected over:

- **nalgebra** — faster for 2D but hard-limited to 1D/2D, no generality
- **polars/arrow** — wrong abstraction (DataFrames, not arrays)
- **roll-your-own** — zero deps but must implement
  slicing/broadcasting/arithmetic from scratch

ndarray provides broadcasting, zero-copy slicing, views, reshape, and
transpose out of the box. Its dependency chain (num-traits, num-integer,
num-complex, rawpointer, matrixmultiply) is all pure Rust and
WASM-compatible. Matrix multiply uses a portable fallback kernel on WASM
(no SIMD, but correct).

### Heap Object

```rust
// src/eval/memory/ndarray.rs
pub struct HeapNdArray {
    inner: ndarray::ArrayD<f64>,
}
```

Allocated on the GC heap as an opaque blob. `ArrayD<f64>` owns a single
contiguous `Vec<f64>` internally — no GC-scannable internal pointers.
Dropping the `HeapNdArray` frees the backing storage via the standard Rust
allocator. Integration follows the same pattern as `HeapString` and
`HeapSet`.

### Native Enum

```rust
pub enum Native {
    Sym(SymbolId),
    Str(RefPtr<HeapString>),
    Num(Number),
    Zdt(DateTime<FixedOffset>),
    Index(Rc<BlockIndex>),
    Set(RefPtr<HeapSet>),
    NdArray(RefPtr<HeapNdArray>),  // new
}
```

No new data constructor tag needed. Arrays are accessed purely through
intrinsics, like sets.

### IntrinsicType

```rust
pub enum IntrinsicType {
    // ... existing variants ...
    NdArray,
}
```

## 3. Polymorphic Arithmetic

Arithmetic intrinsics (Add, Sub, Mul, Div, etc.) gain array dispatch.
Before the existing `num_arg()` extraction, check for `NdArray` variants:

| Left | Right | Result |
|------|-------|--------|
| Num | Num | Numeric (existing path, unchanged) |
| NdArray | Num | Element-wise scalar op |
| Num | NdArray | Element-wise scalar op |
| NdArray | NdArray | Element-wise with broadcasting |

Dispatch logic:

```rust
match (ndarray_arg(&nav, x), ndarray_arg(&nav, y)) {
    (Some(a), Some(b)) => /* element-wise with broadcasting */,
    (Some(a), None)     => /* array op scalar */,
    (None, Some(b))     => /* scalar op array */,
    (None, None)        => /* existing numeric path */,
}
```

The hot path (Num, Num) is a single failed match branch. The `ndarray_arg`
check inspects the `Native` enum discriminant — a byte comparison already
in cache.

### Operators Extended

**Arithmetic (element-wise):** `+`, `-`, `*`, `/`, `÷`, `%`, `negate`

**Comparison (return 0.0/1.0 arrays):** `<`, `>`, `<=`, `>=`, `==`

**Unary math (element-wise):** `abs`, `floor`, `ceil`

### Not Extended

- String ops
- Boolean ops (AND/OR)
- Bitwise ops (meaningless on f64)

### Products (Separate Functions)

`*` on arrays is element-wise (Hadamard product), consistent with `+`
and `-` being element-wise. This follows the numpy convention.

Linear algebra products are explicit function calls:

- `dot(a, b)` — dot product / matrix multiply (ndarray's `.dot()`)
- `cross(a, b)` — cross product (3-element vectors only)
- `outer(a, b)` — outer product (1D × 1D → 2D)

## 4. Intrinsic Functions

### Construction

- **`ARRAY_FROM(nested_list)`** — infer shape from nesting depth.
  Flat list → 1D, list of lists → 2D, etc.
  `array-from([1, 2, 3])` → shape [3].
  `array-from([[1, 2], [3, 4]])` → shape [2, 2].
- **`ZEROS(shape_list)`** — create array filled with zeros.
  `zeros([3, 4])` → shape [3, 4].
- **`RESHAPE(array, shape_list)`** — reshape to new dimensions.

### Access

- **`ARRAY_GET(array, index_or_list)`** — O(1) element access.
  1D: single numeric index. nD: list of indices.
  The `!!` operator is extended to dispatch here when the left operand
  is an array.
- **`ARRAY_SET(array, index_or_list, value)`** — immutable update,
  returns a new array with one element changed.
- **`ARRAY_SHAPE(array)`** — returns shape as a list of numbers.
- **`ARRAY_LENGTH(array)`** — total element count.

### Transformation

- **`ARRAY_SLICE(array, ranges)`** — extract a sub-array.
- **`ARRAY_TRANSPOSE(array)`** — transpose.
- **`ARRAY_MAP(f, array)`** — apply function element-wise, returns array.
- **`ARRAY_TO_LIST(array)`** — convert to flat list.
- **`ARRAY_ROW(array, i)`** — extract row i as 1D array.
- **`ARRAY_COL(array, j)`** — extract column j as 1D array.

### Prelude Wrappers

User-facing names in `lib/prelude.eu`:

```eucalypt
array-from(xs): __ARRAY_FROM(xs)
zeros(shape): __ZEROS(shape)
reshape(arr, shape): __RESHAPE(arr, shape)
array-shape(arr): __ARRAY_SHAPE(arr)
array-length(arr): __ARRAY_LENGTH(arr)
array-set(arr, idx, val): __ARRAY_SET(arr, idx, val)
array-slice(arr, ranges): __ARRAY_SLICE(arr, ranges)
transpose(arr): __ARRAY_TRANSPOSE(arr)
array-map(f, arr): __ARRAY_MAP(f, arr)
array-to-list(arr): __ARRAY_TO_LIST(arr)
array-row(arr, i): __ARRAY_ROW(arr, i)
array-col(arr, j): __ARRAY_COL(arr, j)
dot(a, b): __DOT(a, b)
cross(a, b): __CROSS(a, b)
outer(a, b): __OUTER(a, b)
```

The `!!` operator dispatches to `ARRAY_GET` automatically when the
left operand is an array — no prelude wrapper needed.

## 5. GC Integration

Follows the `HeapSet` pattern exactly:

1. `NdArray` variant added to `Native` enum
2. `HeapNdArray` struct allocated on GC heap
3. Implements the same heap object traits as `HeapString` and `HeapSet`
4. `MutatorHeapView` gains `alloc_ndarray()` (following `alloc_set()`)
5. No internal pointers need GC scanning
6. Drop frees the backing `Vec<f64>` via standard Rust allocator
7. No changes to GC algorithm

## 6. Rendering and Export

Arrays serialise as nested lists matching their shape:

```yaml
# 1D array [1.0, 2.0, 3.0]
[1.0, 2.0, 3.0]

# 2D array shape [2, 3]
[[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]
```

Round-tripping through `array-from` and export is transparent — arrays
render identically to the nested lists they were constructed from. The
export code converts back to nested lists via the existing list rendering
path.

For `eu --dump` and debug output, arrays display with shape:
`<array [2,3] f64>`.

## 7. Error Handling

### New Error Types

- **`ArrayElementType`** — non-numeric element during construction.
  `array-from([1, "x", 3])` → `expected Num, got Str at index 1`
- **`ArrayShapeMismatch`** — incompatible shapes for broadcasting.
  `[3,4] + [3,5]` → `cannot broadcast shapes [3,4] and [3,5]`

### Existing Error Patterns

- **`TypeMismatch`** — wrong type passed to array intrinsic.
  `dot(42, [1,2,3])` → `expected NdArray, got Num`
- Type-checking helper `ndarray_arg()` in `support.rs`, mirroring
  `num_arg()`, `str_arg()`, etc.

## 8. WASM Compatibility

ndarray without `blas` or `rayon` features compiles to
`wasm32-unknown-unknown`. The Burn ML framework uses ndarray on WASM
in production. Matrix multiply falls back to a portable pure-Rust
kernel (no SIMD, but correct). All transitive dependencies (num-traits,
num-integer, num-complex, rawpointer, matrixmultiply) are pure Rust
and WASM-safe.

Constraints:
- Do not enable `blas` feature (requires libc/cblas-sys)
- Do not enable `rayon` feature (thread spawning panics on wasm32)
- ndarray's upstream CI does not test WASM, so pin to a known-good
  version and test in eucalypt's own CI when WASM target is added

## 9. Future Extensions

- **Integer arrays** — add `ArrayD<i64>` variant to `HeapNdArray` with
  promotion rules (int + float → float)
- **Idiot bracket sugar** — `⟨1 2 3⟩` as shorthand for
  `array-from([1, 2, 3])` once idiot brackets land
- **Boolean mask indexing** — `arr !! mask` where mask is a 0.0/1.0 array
- **Sparse arrays** — for large grids with mostly-zero cells
- **SIMD on WASM** — when matrixmultiply adds wasm32 SIMD kernels
