# N-Dimensional Array (Tensor) Type Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `ndarray::ArrayD<f64>` as a native type in eucalypt with O(1) access, element-wise arithmetic via polymorphic operator dispatch, and prelude wrappers.

**Architecture:** `HeapNdArray` wraps `ArrayD<f64>` on the GC heap, stored as a `Native::NdArray` variant (same pattern as `Set`). Arithmetic intrinsics gain type dispatch to support array-array, array-scalar, and scalar-array operations. New intrinsic functions provide construction, access, and transformation. Prelude wrappers expose the user-facing API.

**Tech Stack:** Rust, ndarray crate, existing STG intrinsic infrastructure

---

### Task 1: Add ndarray dependency

**Files:**
- Modify: `Cargo.toml` (dependencies section, after line ~36)

**Step 1: Add ndarray to Cargo.toml**

Add to the `[dependencies]` section:

```toml
ndarray = { version = "0.17", default-features = false }
```

No `blas` or `rayon` features — keeps it WASM-safe.

**Step 2: Verify it compiles**

```bash
cargo check
```

Expected: compiles with ndarray downloaded.

**Step 3: Commit**

```bash
git add Cargo.toml Cargo.lock
git commit -m "deps: add ndarray crate for n-dimensional array support"
```

---

### Task 2: Create HeapNdArray heap object

**Files:**
- Create: `src/eval/memory/ndarray.rs`
- Modify: `src/eval/memory/mod.rs` (add `pub mod ndarray;`)

**Step 1: Read HeapSet for reference**

Read `src/eval/memory/set.rs` to understand the pattern: struct with `StgObject` impl, `Clone`, `PartialEq`, `Eq`.

**Step 2: Create HeapNdArray**

Create `src/eval/memory/ndarray.rs`:

```rust
//! Native n-dimensional array type for the eucalypt VM.
//!
//! Arrays contain f64 elements in a contiguous buffer with
//! n-dimensional shape and O(1) indexed access via ndarray.

use ndarray::ArrayD;

use super::alloc::StgObject;

/// An n-dimensional array of f64 values, stored on the heap.
///
/// GC does not trace into array contents because f64 values contain
/// no heap references. The backing Vec<f64> is freed via the standard
/// Rust allocator when the HeapNdArray is dropped.
#[derive(Debug)]
pub struct HeapNdArray {
    inner: ArrayD<f64>,
}

impl StgObject for HeapNdArray {}

impl HeapNdArray {
    /// Create from an owned ArrayD.
    pub fn new(inner: ArrayD<f64>) -> Self {
        HeapNdArray { inner }
    }

    /// Access the inner ndarray.
    pub fn inner(&self) -> &ArrayD<f64> {
        &self.inner
    }

    /// Consume and return the inner ndarray.
    pub fn into_inner(self) -> ArrayD<f64> {
        self.inner
    }

    /// Return the shape as a slice of dimensions.
    pub fn shape(&self) -> &[usize] {
        self.inner.shape()
    }

    /// Return total number of elements.
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    /// Check if the array is empty.
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// O(1) element access by flat index (1D).
    pub fn get_flat(&self, index: usize) -> Option<f64> {
        self.inner.as_slice().and_then(|s| s.get(index).copied())
    }

    /// O(1) element access by n-dimensional index.
    pub fn get_nd(&self, index: &[usize]) -> Option<f64> {
        self.inner.get(index).copied()
    }
}

impl Clone for HeapNdArray {
    fn clone(&self) -> Self {
        HeapNdArray {
            inner: self.inner.clone(),
        }
    }
}

impl PartialEq for HeapNdArray {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl Eq for HeapNdArray {}

#[cfg(test)]
mod tests {
    use super::*;
    use ndarray::arr2;

    #[test]
    fn create_and_access_2d() {
        let a = HeapNdArray::new(arr2(&[[1.0, 2.0], [3.0, 4.0]]).into_dyn());
        assert_eq!(a.shape(), &[2, 2]);
        assert_eq!(a.len(), 4);
        assert_eq!(a.get_nd(&[0, 1]), Some(2.0));
        assert_eq!(a.get_nd(&[1, 0]), Some(3.0));
    }

    #[test]
    fn create_1d() {
        let a = HeapNdArray::new(ndarray::arr1(&[10.0, 20.0, 30.0]).into_dyn());
        assert_eq!(a.shape(), &[3]);
        assert_eq!(a.get_flat(1), Some(20.0));
    }

    #[test]
    fn equality() {
        let a = HeapNdArray::new(ndarray::arr1(&[1.0, 2.0]).into_dyn());
        let b = HeapNdArray::new(ndarray::arr1(&[1.0, 2.0]).into_dyn());
        assert_eq!(a, b);
    }

    #[test]
    fn clone_is_independent() {
        let a = HeapNdArray::new(ndarray::arr1(&[1.0]).into_dyn());
        let b = a.clone();
        assert_eq!(a, b);
    }
}
```

**Step 3: Add module to mod.rs**

Read `src/eval/memory/mod.rs` and add `pub mod ndarray;` alongside
the existing `pub mod set;`.

**Step 4: Verify**

```bash
cargo test --lib -p eucalypt -- memory::ndarray
```

Expected: all 4 tests pass.

**Step 5: Commit**

```bash
git add src/eval/memory/ndarray.rs src/eval/memory/mod.rs
git commit -m "feat: add HeapNdArray heap object backed by ndarray"
```

---

### Task 3: Add NdArray variant to Native enum and GC integration

**Files:**
- Modify: `src/eval/memory/syntax.rs` (lines 34-92, 270-315)
- Modify: `src/eval/types.rs` (IntrinsicType enum, lines 7-19)

**Step 1: Read syntax.rs**

Read `src/eval/memory/syntax.rs` to understand:
- `Native` enum (lines 34-48)
- `PartialEq` impl (lines 50-63)
- `Display` impl (lines 69-92)
- `mark_ref_heap_pointers` (lines 274-284)
- `update_ref_heap_pointers` (lines 294-308)

**Step 2: Add NdArray variant to Native**

In the `Native` enum, add after the `Set` variant:

```rust
/// An n-dimensional array of f64 values
NdArray(RefPtr<HeapNdArray>),
```

Add the import at the top of the file:

```rust
use super::ndarray::HeapNdArray;
```

**Step 3: Update PartialEq impl**

Add to the match in `PartialEq for Native`:

```rust
(Native::NdArray(a), Native::NdArray(b)) => a == b,
```

**Step 4: Update Display impl**

Add to the match in `Display for Native`:

```rust
Native::NdArray(arr) => {
    let shape = unsafe { &*arr.as_ptr() }.shape();
    write!(f, "<array {:?} f64>", shape)
}
```

**Step 5: Update GC scanning**

In `mark_ref_heap_pointers`, add:

```rust
Ref::V(Native::NdArray(ptr)) => {
    marker.mark(*ptr);
}
```

In `update_ref_heap_pointers`, add:

```rust
Ref::V(Native::NdArray(ptr)) => {
    if let Some(new_ptr) = heap.forwarded_to(*ptr) {
        *ptr = new_ptr;
    }
}
```

**Step 6: Add NdArray to IntrinsicType**

Read `src/eval/types.rs` and add `NdArray` variant to the
`IntrinsicType` enum.

**Step 7: Verify**

```bash
cargo check
cargo test --lib
```

Expected: compiles, all existing tests pass.

**Step 8: Commit**

```bash
git add src/eval/memory/syntax.rs src/eval/types.rs
git commit -m "feat: add NdArray variant to Native enum with GC support"
```

---

### Task 4: Add array support helpers to support.rs

**Files:**
- Modify: `src/eval/stg/support.rs` (after `set_arg` at ~line 399)

**Step 1: Read existing helpers**

Read `src/eval/stg/support.rs` lines 380-415 to see `set_arg` and
`machine_return_set` patterns.

**Step 2: Add ndarray helpers**

Add after the set helpers:

```rust
use crate::eval::memory::ndarray::HeapNdArray;

/// Helper for intrinsics to access an ndarray arg
pub fn ndarray_arg<'guard>(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'guard>,
    arg: &Ref,
) -> Result<ScopedPtr<'guard, HeapNdArray>, ExecutionError> {
    let native = machine.nav(view).resolve_native(arg)?;
    if let Native::NdArray(ptr) = native {
        Ok(view.scoped(ptr))
    } else {
        Err(ExecutionError::TypeMismatch(
            Smid::default(),
            IntrinsicType::NdArray,
            // TODO: infer actual type from native
            IntrinsicType::Unknown,
        ))
    }
}

/// Try to extract an ndarray — returns None instead of error if wrong type
pub fn try_ndarray_arg(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<Option<RefPtr<HeapNdArray>>, ExecutionError> {
    let native = machine.nav(view).resolve_native(arg)?;
    if let Native::NdArray(ptr) = native {
        Ok(Some(ptr))
    } else {
        Ok(None)
    }
}

/// Return an ndarray from intrinsic
pub fn machine_return_ndarray(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
    array: HeapNdArray,
) -> Result<(), ExecutionError> {
    let ptr = view.alloc(array)?.as_ptr();
    machine.set_closure(SynClosure::new(
        view.alloc(HeapSyn::Atom {
            evaluand: Ref::V(Native::NdArray(ptr)),
        })?
        .as_ptr(),
        machine.root_env(),
    ))
}
```

**Step 3: Verify**

```bash
cargo check
```

**Step 4: Commit**

```bash
git add src/eval/stg/support.rs
git commit -m "feat: add ndarray helper functions in support.rs"
```

---

### Task 5: Implement array construction intrinsics

**Files:**
- Create: `src/eval/stg/ndarray.rs`
- Modify: `src/eval/stg/mod.rs` (add `pub mod ndarray;`)
- Modify: `src/eval/intrinsics.rs` (add catalogue entries, indices 131+)

**Step 1: Read set.rs for the intrinsic pattern**

Read `src/eval/stg/set.rs` to understand how `SetFromList`, `SetEmpty`
etc. are structured: struct, `StgIntrinsic` impl with `name()`,
`index()`, `wrapper()`, and `execute()`, plus the `CallGlobalN` trait.

Also read how `data_list_arg` works in `support.rs` — this converts a
cons-list on the heap into a `Vec<Ref>`.

**Step 2: Create ndarray intrinsics file**

Create `src/eval/stg/ndarray.rs` with the construction intrinsics:

```rust
//! Array intrinsics for the eucalypt VM.
//!
//! These intrinsics operate on `Native::NdArray` values, providing
//! construction, element access, and transformation.

use ndarray::ArrayD;
use serde_json::Number;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{
            CallGlobal1, CallGlobal2, CallGlobal3, IntrinsicMachine, StgIntrinsic,
        },
        memory::{
            mutator::MutatorHeapView,
            ndarray::HeapNdArray,
            syntax::{Native, Ref},
        },
    },
};

use super::support::{
    data_list_arg, machine_return_ndarray, machine_return_num, ndarray_arg,
    num_arg, try_ndarray_arg,
};

/// ARRAY.FROM(nested_list) - create array from nested list, inferring shape
pub struct ArrayFrom;

impl StgIntrinsic for ArrayFrom {
    fn name(&self) -> &str {
        "ARRAY.FROM"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // Collect the list into Vec<Ref>
        let items = data_list_arg(machine, view, &args[0])?;

        if items.is_empty() {
            let arr = HeapNdArray::new(ArrayD::zeros(ndarray::IxDyn(&[0])));
            return machine_return_ndarray(machine, view, arr);
        }

        // Check if first element is a list (2D+) or a number (1D)
        let first_is_list = matches!(
            machine.nav(view).resolve_native_or_data(&items[0])?,
            NativeOrData::Data(_)
        );

        if first_is_list {
            // 2D: each item is a list of numbers
            let mut rows = Vec::new();
            let mut ncols = None;
            for item in &items {
                let row_items = data_list_arg(machine, view, item)?;
                let row: Vec<f64> = row_items
                    .iter()
                    .map(|r| {
                        num_arg(machine, view, r)
                            .and_then(|n| n.as_f64().ok_or_else(||
                                ExecutionError::Panic("non-finite number in array".to_string())))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                if let Some(nc) = ncols {
                    if row.len() != nc {
                        return Err(ExecutionError::Panic(format!(
                            "ragged array: row has {} elements, expected {}",
                            row.len(), nc
                        )));
                    }
                } else {
                    ncols = Some(row.len());
                }
                rows.push(row);
            }
            let nrows = rows.len();
            let ncols = ncols.unwrap_or(0);
            let flat: Vec<f64> = rows.into_iter().flatten().collect();
            let arr = ArrayD::from_shape_vec(
                ndarray::IxDyn(&[nrows, ncols]),
                flat,
            ).map_err(|e| ExecutionError::Panic(format!("array shape error: {e}")))?;
            machine_return_ndarray(machine, view, HeapNdArray::new(arr))
        } else {
            // 1D: each item is a number
            let data: Vec<f64> = items
                .iter()
                .map(|r| {
                    num_arg(machine, view, r)
                        .and_then(|n| n.as_f64().ok_or_else(||
                            ExecutionError::Panic("non-finite number in array".to_string())))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let n = data.len();
            let arr = ArrayD::from_shape_vec(
                ndarray::IxDyn(&[n]),
                data,
            ).map_err(|e| ExecutionError::Panic(format!("array shape error: {e}")))?;
            machine_return_ndarray(machine, view, HeapNdArray::new(arr))
        }
    }
}

impl CallGlobal1 for ArrayFrom {}

/// ARRAY.ZEROS(shape_list) - create array filled with zeros
pub struct ArrayZeros;

impl StgIntrinsic for ArrayZeros {
    fn name(&self) -> &str {
        "ARRAY.ZEROS"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let shape = list_to_shape(machine, view, &args[0])?;
        let arr = HeapNdArray::new(ArrayD::zeros(ndarray::IxDyn(&shape)));
        machine_return_ndarray(machine, view, arr)
    }
}

impl CallGlobal1 for ArrayZeros {}

/// ARRAY.RESHAPE(array, shape_list) - reshape to new dimensions
pub struct ArrayReshape;

impl StgIntrinsic for ArrayReshape {
    fn name(&self) -> &str {
        "ARRAY.RESHAPE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let arr = ndarray_arg(machine, view, &args[0])?;
        let shape = list_to_shape(machine, view, &args[1])?;
        let reshaped = arr.inner()
            .clone()
            .into_shape_with_order(ndarray::IxDyn(&shape))
            .map_err(|e| ExecutionError::Panic(format!("reshape error: {e}")))?;
        machine_return_ndarray(machine, view, HeapNdArray::new(reshaped))
    }
}

impl CallGlobal2 for ArrayReshape {}

/// Helper: convert a list arg to a Vec<usize> shape
fn list_to_shape(
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    arg: &Ref,
) -> Result<Vec<usize>, ExecutionError> {
    let items = data_list_arg(machine, view, arg)?;
    items
        .iter()
        .map(|r| {
            let n = num_arg(machine, view, r)?;
            n.as_u64()
                .map(|v| v as usize)
                .or_else(|| n.as_i64().and_then(|v| if v >= 0 { Some(v as usize) } else { None }))
                .ok_or_else(|| ExecutionError::Panic(
                    format!("shape dimension must be non-negative integer, got {n}")
                ))
        })
        .collect()
}
```

Note: `NativeOrData` and `resolve_native_or_data` may need to be
added to the navigator or the detection can use a simpler approach —
try `data_list_arg` on the first element and check for error to
determine 1D vs 2D. Adjust based on what the navigator already
provides.

**Step 3: Add module to stg/mod.rs**

Add `pub mod ndarray;` to `src/eval/stg/mod.rs` alongside existing
modules (e.g. after `pub mod null;` at line 18).

**Step 4: Add catalogue entries to intrinsics.rs**

Read `src/eval/intrinsics.rs` and add entries starting at index 131:

```rust
Intrinsic { // 131
    name: "ARRAY.FROM",
    ty: function(vec![list(), unk()]).unwrap(),
    strict: vec![0],
},
Intrinsic { // 132
    name: "ARRAY.ZEROS",
    ty: function(vec![list(), unk()]).unwrap(),
    strict: vec![0],
},
Intrinsic { // 133
    name: "ARRAY.RESHAPE",
    ty: function(vec![unk(), list(), unk()]).unwrap(),
    strict: vec![0, 1],
},
```

**Step 5: Register in make_standard_runtime**

In `src/eval/stg/mod.rs`, add to `make_standard_runtime()` after the
existing `set::*` registrations:

```rust
rt.add(Box::new(ndarray::ArrayFrom));
rt.add(Box::new(ndarray::ArrayZeros));
rt.add(Box::new(ndarray::ArrayReshape));
```

**Step 6: Verify**

```bash
cargo check
```

**Step 7: Commit**

```bash
git add src/eval/stg/ndarray.rs src/eval/stg/mod.rs src/eval/intrinsics.rs
git commit -m "feat: add array construction intrinsics (ARRAY.FROM, ARRAY.ZEROS, ARRAY.RESHAPE)"
```

---

### Task 6: Implement array access and query intrinsics

**Files:**
- Modify: `src/eval/stg/ndarray.rs`
- Modify: `src/eval/intrinsics.rs`
- Modify: `src/eval/stg/mod.rs` (runtime registration)

**Step 1: Add access intrinsics to ndarray.rs**

Add the following structs to `src/eval/stg/ndarray.rs`:

- **`ArrayGet`** — `ARRAY.GET(array, index_or_list)` — O(1) access.
  If index arg is a number, use as flat/1D index. If it's a list,
  use as n-dimensional index. Return a Num.

- **`ArraySet`** — `ARRAY.SET(array, index_or_list, value)` — clone
  the array, set one element, return the new array.

- **`ArrayShape`** — `ARRAY.SHAPE(array)` — return shape as a list
  of numbers.

- **`ArrayLength`** — `ARRAY.LENGTH(array)` — return total element
  count as a number.

- **`ArrayToList`** — `ARRAY.TO_LIST(array)` — convert to a flat
  cons-list of numbers.

For `ArrayGet`, the index dispatch:

```rust
// Try as number first (1D/flat index)
if let Ok(n) = num_arg(machine, view, &args[1]) {
    let idx = n.as_u64().unwrap() as usize;
    let val = arr.get_flat(idx).ok_or_else(|| ...)?;
    return machine_return_num(machine, view, Number::from_f64(val).unwrap());
}
// Otherwise treat as list of indices (nD)
let indices = list_to_shape(machine, view, &args[1])?;
let val = arr.get_nd(&indices).ok_or_else(|| ...)?;
machine_return_num(machine, view, Number::from_f64(val).unwrap())
```

For `ArrayToList`, build a cons-list from the flat iterator using
the existing list construction helpers (see how `SetToList` works
in `set.rs`).

**Step 2: Add catalogue entries**

Add to `src/eval/intrinsics.rs` at indices 134-138:

```rust
Intrinsic { // 134
    name: "ARRAY.GET",
    ty: function(vec![unk(), unk(), num()]).unwrap(),
    strict: vec![0, 1],
},
Intrinsic { // 135
    name: "ARRAY.SET",
    ty: function(vec![unk(), unk(), num(), unk()]).unwrap(),
    strict: vec![0, 1, 2],
},
Intrinsic { // 136
    name: "ARRAY.SHAPE",
    ty: function(vec![unk(), list()]).unwrap(),
    strict: vec![0],
},
Intrinsic { // 137
    name: "ARRAY.LENGTH",
    ty: function(vec![unk(), num()]).unwrap(),
    strict: vec![0],
},
Intrinsic { // 138
    name: "ARRAY.TO_LIST",
    ty: function(vec![unk(), list()]).unwrap(),
    strict: vec![0],
},
```

**Step 3: Register in runtime**

Add to `make_standard_runtime()`:

```rust
rt.add(Box::new(ndarray::ArrayGet));
rt.add(Box::new(ndarray::ArraySet));
rt.add(Box::new(ndarray::ArrayShape));
rt.add(Box::new(ndarray::ArrayLength));
rt.add(Box::new(ndarray::ArrayToList));
```

**Step 4: Verify**

```bash
cargo check
```

**Step 5: Commit**

```bash
git add src/eval/stg/ndarray.rs src/eval/intrinsics.rs src/eval/stg/mod.rs
git commit -m "feat: add array access intrinsics (GET, SET, SHAPE, LENGTH, TO_LIST)"
```

---

### Task 7: Implement array transformation intrinsics

**Files:**
- Modify: `src/eval/stg/ndarray.rs`
- Modify: `src/eval/intrinsics.rs`
- Modify: `src/eval/stg/mod.rs`

**Step 1: Add transformation intrinsics**

Add to `src/eval/stg/ndarray.rs`:

- **`ArrayTranspose`** — `ARRAY.TRANSPOSE(array)` — uses
  `ndarray::ArrayD::reversed_axes()`.

- **`ArrayRow`** — `ARRAY.ROW(array, i)` — extract row i as 1D
  array. Uses `arr.inner().row(i).to_owned().into_dyn()`.

- **`ArrayCol`** — `ARRAY.COL(array, j)` — extract column j as 1D
  array. Uses `arr.inner().column(j).to_owned().into_dyn()`.

- **`ArraySlice`** — `ARRAY.SLICE(array, ranges_list)` — extract
  sub-array. Each range is a list `[start, end]`. Uses ndarray
  slicing. (Can be deferred to a later task if complex.)

- **`ArrayDot`** — `DOT(a, b)` — dot product / matrix multiply.
  1D·1D → scalar, 2D·1D → 1D, 2D·2D → 2D. Uses `a.dot(&b)`.

- **`ArrayCross`** — `CROSS(a, b)` — cross product for 3-element
  1D arrays.

- **`ArrayOuter`** — `OUTER(a, b)` — outer product. Uses
  element-wise multiplication with broadcasting after reshaping
  a to column and b to row.

**Step 2: Add catalogue entries (indices 139-145)**

**Step 3: Register in runtime**

**Step 4: Verify**

```bash
cargo check
```

**Step 5: Commit**

```bash
git add src/eval/stg/ndarray.rs src/eval/intrinsics.rs src/eval/stg/mod.rs
git commit -m "feat: add array transformation intrinsics (TRANSPOSE, ROW, COL, SLICE, DOT, CROSS, OUTER)"
```

---

### Task 8: Make arithmetic intrinsics polymorphic

**Files:**
- Modify: `src/eval/stg/arith.rs` (Add, Sub, Mul, Div, Mod, Floor, Ceil, ~lines 75-290)
- Modify: `src/eval/stg/eq.rs` (Eq intrinsic)

**Step 1: Read current Add implementation**

Read `src/eval/stg/arith.rs` lines 75-115 to see the current pattern:
`num_arg` → number dispatch → `machine_return_num`.

**Step 2: Add array dispatch to Add**

Modify `Add::execute()` to check for arrays before numbers:

```rust
fn execute(
    &self,
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    _emitter: &mut dyn Emitter,
    args: &[Ref],
) -> Result<(), ExecutionError> {
    let x_arr = try_ndarray_arg(machine, view, &args[0])?;
    let y_arr = try_ndarray_arg(machine, view, &args[1])?;

    match (x_arr, y_arr) {
        (Some(a), Some(b)) => {
            // array + array (element-wise with broadcasting)
            let a = unsafe { &*a.as_ptr() };
            let b = unsafe { &*b.as_ptr() };
            let result = (&a.inner() + &b.inner())
                .map_err(|e| ExecutionError::Panic(
                    format!("array broadcast error: {e}")
                ))?;
            machine_return_ndarray(machine, view, HeapNdArray::new(result))
        }
        (Some(a), None) => {
            // array + scalar
            let a = unsafe { &*a.as_ptr() };
            let s = num_arg(machine, view, &args[1])?
                .as_f64()
                .ok_or_else(|| ExecutionError::Panic("non-finite scalar".into()))?;
            let result = &a.inner() + s;
            machine_return_ndarray(machine, view, HeapNdArray::new(result))
        }
        (None, Some(b)) => {
            // scalar + array
            let s = num_arg(machine, view, &args[0])?
                .as_f64()
                .ok_or_else(|| ExecutionError::Panic("non-finite scalar".into()))?;
            let b = unsafe { &*b.as_ptr() };
            let result = s + &b.inner();
            machine_return_ndarray(machine, view, HeapNdArray::new(result))
        }
        (None, None) => {
            // existing numeric path (unchanged)
            let x = num_arg(machine, view, &args[0])?;
            let y = num_arg(machine, view, &args[1])?;
            // ... existing i64/u64/f64 dispatch ...
        }
    }
}
```

**Step 3: Apply same pattern to Sub, Mul, Div, Mod**

Each uses the corresponding ndarray operator (`-`, `*`, `/`, `%`).
For Div, use floor division semantics on arrays (map element-wise).
For Mod, use floor mod semantics.

**Step 4: Add unary array support to Floor, Ceil, Negate**

For `Floor::execute()`, check if arg is NdArray and apply
`arr.mapv(f64::floor)`. Same for Ceil with `f64::ceil` and Negate
with negation.

**Step 5: Update Eq for array comparison**

In `src/eval/stg/eq.rs`, add array equality check: two arrays are
equal if shapes match and all elements are equal.

**Step 6: Update comparison operators (Gt, Lt, Gte, Lte)**

For array comparisons, return 0.0/1.0 arrays via element-wise
comparison with ndarray's `mapv` and zip operations.

**Step 7: Verify**

```bash
cargo check
cargo test --lib
```

**Step 8: Commit**

```bash
git add src/eval/stg/arith.rs src/eval/stg/eq.rs
git commit -m "feat: polymorphic arithmetic dispatch for arrays"
```

---

### Task 9: Extend !! operator for array indexing

**Files:**
- Modify: `lib/prelude.eu` (lines 875-877)

**Step 1: Read current !! definition**

Read `lib/prelude.eu` lines 872-877:

```eucalypt
nth(n, l): l drop(n) head
(l !! n): l nth(n)
```

**Step 2: Make !! dispatch on type**

Replace the `!!` definition to dispatch between list and array:

```eucalypt
(l !! n): if(is-array(l), __ARRAY.GET(l, n), l nth(n))
```

This requires `is-array` — add it (see Task 10).

Alternatively, keep `!!` as-is for lists and add array access via
the prelude `array-get` wrapper. The user can use either. This is
simpler and avoids making every `!!` call pay for a type check.

Decision: keep `!!` for lists, provide `array-get` in prelude.
Users who want `!!` on arrays can override it themselves. This
avoids the type-check overhead on every list index.

**Step 3: Commit (if any prelude changes made)**

---

### Task 10: Add prelude wrappers and type checking

**Files:**
- Modify: `lib/prelude.eu`
- Modify: `src/eval/stg/ndarray.rs` (IsArray intrinsic)
- Modify: `src/eval/intrinsics.rs`
- Modify: `src/eval/stg/mod.rs`

**Step 1: Add IsArray intrinsic**

Add to `src/eval/stg/ndarray.rs`:

```rust
/// ISARRAY(x) - check if x is an ndarray
pub struct IsArray;

impl StgIntrinsic for IsArray {
    fn name(&self) -> &str { "ISARRAY" }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let is_arr = try_ndarray_arg(machine, view, &args[0])?.is_some();
        machine_return_bool(machine, view, is_arr)
    }
}

impl CallGlobal1 for IsArray {}
```

Add catalogue entry and register in runtime.

**Step 2: Add prelude wrappers**

Add an `array` section to `lib/prelude.eu` (after the `set` section):

```eucalypt
# ---- Arrays (n-dimensional) ----

` "`is-array(x)` - return true if `x` is an n-dimensional array."
is-array(x): __ISARRAY(x)

` "`array-from(xs)` - create array from a (nested) list of numbers."
array-from(xs): __ARRAY.FROM(xs)

` "`zeros(shape)` - create array of zeros with given shape (list of dimensions)."
zeros(shape): __ARRAY.ZEROS(shape)

` "`reshape(arr, shape)` - reshape array to new dimensions."
reshape(arr, shape): __ARRAY.RESHAPE(arr, shape)

` "`array-get(arr, idx)` - O(1) element access. `idx` is a number (1D) or list (nD)."
array-get(arr, idx): __ARRAY.GET(arr, idx)

` "`array-set(arr, idx, val)` - return new array with element at `idx` set to `val`."
array-set(arr, idx, val): __ARRAY.SET(arr, idx, val)

` "`array-shape(arr)` - return shape of array as list of dimensions."
array-shape(arr): __ARRAY.SHAPE(arr)

` "`array-length(arr)` - return total number of elements in array."
array-length(arr): __ARRAY.LENGTH(arr)

` "`array-to-list(arr)` - convert array to flat list of numbers."
array-to-list(arr): __ARRAY.TO_LIST(arr)

` "`transpose(arr)` - transpose array (reverse axis order)."
transpose(arr): __ARRAY.TRANSPOSE(arr)

` "`array-row(arr, i)` - extract row `i` as a 1D array."
array-row(arr, i): __ARRAY.ROW(arr, i)

` "`array-col(arr, j)` - extract column `j` as a 1D array."
array-col(arr, j): __ARRAY.COL(arr, j)

` "`dot(a, b)` - dot product or matrix multiply."
dot(a, b): __DOT(a, b)

` "`cross(a, b)` - cross product (3-element vectors)."
cross(a, b): __CROSS(a, b)

` "`outer(a, b)` - outer product of two 1D arrays."
outer(a, b): __OUTER(a, b)
```

Note: check if `transpose` already exists in the prelude (it was
mentioned in a grep hit). If so, rename or conditionally define.

**Step 3: Verify**

```bash
cargo check
```

**Step 4: Commit**

```bash
git add lib/prelude.eu src/eval/stg/ndarray.rs src/eval/intrinsics.rs src/eval/stg/mod.rs
git commit -m "feat: add prelude wrappers and is-array type check for arrays"
```

---

### Task 11: Add array rendering in export layer

**Files:**
- Modify: `src/eval/stg/render.rs` or `src/eval/stg/emit.rs`
  (wherever Native values are emitted)

**Step 1: Understand current rendering**

Read the render/emit code to find where `Native::Set` or
`Native::Num` / `Native::Str` are handled during export. Sets are
converted to lists via `SET.TO_LIST` in the prelude before export.

Arrays should follow the same approach: the prelude should convert
arrays to nested lists before export. This means the render layer
doesn't need to know about arrays at all — if an array reaches the
renderer unconverted, it should panic with a clear message.

**Step 2: Add render fallback**

If the emitter encounters `Native::NdArray`, emit it as a nested
list structure. Alternatively, add a match arm that calls
`array-to-list` or panics with "array must be converted to list
before export".

The simpler approach: have the emitter convert arrays inline to
nested lists. Walk the array shape, emit `SeqStart`/`SeqEnd` for
each dimension, emit `Native::Num` for each element.

**Step 3: Verify**

```bash
cargo check
```

**Step 4: Commit**

```bash
git add src/eval/stg/render.rs src/eval/stg/emit.rs
git commit -m "feat: array rendering as nested lists in export layer"
```

---

### Task 12: Harness tests — basic array operations

**Files:**
- Create: `harness/test/082_arrays.eu`
- Modify: `tests/harness_test.rs`

**Step 1: Create array test file**

Create `harness/test/082_arrays.eu` with tests covering:

```eucalypt
test-array-from-1d:
  ` { target: :test }
  array-to-list(array-from([1, 2, 3]))

test-array-from-2d:
  ` { target: :test }
  array-to-list(array-from([[1, 2], [3, 4]]))

test-array-zeros:
  ` { target: :test }
  array-length(zeros([3, 4]))

test-array-shape:
  ` { target: :test }
  array-shape(array-from([[1, 2, 3], [4, 5, 6]]))

test-array-get-1d:
  ` { target: :test }
  array-get(array-from([10, 20, 30]), 1)

test-array-get-2d:
  ` { target: :test }
  array-get(array-from([[1, 2], [3, 4]]), [1, 0])

test-array-reshape:
  ` { target: :test }
  array-shape(reshape(array-from([1, 2, 3, 4, 5, 6]), [2, 3]))

test-array-add-scalar:
  ` { target: :test }
  array-to-list(array-from([1, 2, 3]) + 10)

test-array-add-array:
  ` { target: :test }
  array-to-list(array-from([1, 2, 3]) + array-from([10, 20, 30]))

test-array-mul-scalar:
  ` { target: :test }
  array-to-list(array-from([1, 2, 3]) * 2)

test-array-transpose:
  ` { target: :test }
  array-shape(transpose(array-from([[1, 2, 3], [4, 5, 6]])))

test-array-dot-1d:
  ` { target: :test }
  dot(array-from([1, 2, 3]), array-from([4, 5, 6]))

test-is-array:
  ` { target: :test }
  [is-array(array-from([1])), is-array(42), is-array("hello")]
```

**Step 2: Add harness test**

Add to `tests/harness_test.rs`:

```rust
harness_test!(test_harness_082, "082_arrays");
```

**Step 3: Run tests**

```bash
cargo test test_harness_082 -- --nocapture
```

Review output, fix any failures.

**Step 4: Commit**

```bash
git add harness/test/082_arrays.eu tests/harness_test.rs
git commit -m "test: add harness tests for array operations"
```

---

### Task 13: Harness tests — error cases

**Files:**
- Create: `harness/test/errors/030_array_errors.eu`
- Create: `harness/test/errors/030_array_errors.expect`
- Modify: `tests/harness_test.rs`

**Step 1: Create error test**

Create `harness/test/errors/030_array_errors.eu`:

```eucalypt
# Attempt to create array from non-numeric list
bad: array-from(["a", "b", "c"])
```

Create `harness/test/errors/030_array_errors.expect`:

```
exit: 1
stderr: "type"
```

**Step 2: Add harness test**

```rust
harness_error_test!(test_error_030, "errors/030_array_errors");
```

**Step 3: Run**

```bash
cargo test test_error_030 -- --nocapture
```

**Step 4: Commit**

```bash
git add harness/test/errors/030_array_errors.eu harness/test/errors/030_array_errors.expect tests/harness_test.rs
git commit -m "test: add array error harness tests"
```

---

### Task 14: Run full test suite and clippy

**Step 1: Run clippy**

```bash
cargo clippy --all-targets -- -D warnings
```

Fix any warnings.

**Step 2: Run fmt**

```bash
cargo fmt --all
```

**Step 3: Run full test suite**

```bash
cargo test
```

Verify all tests pass including existing ones (no regressions).

**Step 4: Commit any fixes**

```bash
git add -A
git commit -m "chore: fix clippy and formatting for array feature"
```

---

### Task 15: Close bead

**Step 1: Close the bead**

```bash
bd close eu-atzn --reason="N-dimensional array native type implemented with ndarray backing, polymorphic arithmetic, and prelude API"
bd sync
```

**Step 2: Push**

```bash
git push
```
