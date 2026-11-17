//! N-dimensional array (tensor) type for the eucalypt VM.
//!
//! Arrays store `f64` values in a flat row-major buffer with shape
//! metadata. They provide O(1) coordinate-based indexing via stride
//! arithmetic.
//!
//! All storage lives in the GC heap (eu-oaz72.5): the shape and the
//! values are heap [`Array`]s of plain scalars, so the collector marks
//! and evacuates the buffers but traces nothing inside them. Operations
//! read through an `ndarray` view over the heap buffer and write their
//! result straight into a fresh heap buffer (eu-oaz72.159): no Rust-owned
//! intermediate array exists, so the result's storage is accounted by the
//! managed heap from the start.

use ndarray::{ArrayViewD, IxDyn};

use super::{
    alloc::{ScopedAllocator, StgObject},
    array::Array,
    collect::{CollectorHeapView, CollectorScope, GcScannable, OpaqueHeapBytes, ScanPtr},
};
use crate::eval::error::ExecutionError;

/// An n-dimensional array of f64 values, stored entirely in the GC heap.
pub struct HeapNdArray {
    shape: Array<usize>,
    data: Array<f64>,
}

impl StgObject for HeapNdArray {}

/// Mark a scalar heap array's backing buffer and queue it as opaque
/// bytes so it can be evacuated.
fn scan_scalar_array<'a, T: Clone>(
    arr: &'a Array<T>,
    scope: &'a dyn CollectorScope,
    marker: &mut CollectorHeapView<'a>,
    out: &mut Vec<ScanPtr<'a>>,
) {
    if marker.mark_array(arr) {
        if let Some(backing) = arr.allocated_data() {
            out.push(ScanPtr::from_non_null(
                scope,
                backing.cast::<OpaqueHeapBytes>(),
            ));
        }
    }
}

/// Rewrite a scalar heap array's backing pointer if it was evacuated.
fn update_scalar_array<T: Clone>(arr: &mut Array<T>, heap: &CollectorHeapView<'_>) {
    if let Some(old_ptr) = arr.allocated_data() {
        if let Some(new_ptr) = heap.forwarded_to(old_ptr) {
            // SAFETY: new_ptr is the evacuated copy of the same backing
            // allocation.
            unsafe { arr.set_backing_ptr(new_ptr.cast()) };
        }
    }
}

impl GcScannable for HeapNdArray {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        scan_scalar_array(&self.shape, scope, marker, out);
        scan_scalar_array(&self.data, scope, marker, out);
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        update_scalar_array(&mut self.shape, heap);
        update_scalar_array(&mut self.data, heap);
    }
}

impl HeapNdArray {
    /// Build an array of `shape` whose row-major elements come from
    /// `iter`, written directly into the heap. The iterator must yield
    /// exactly the product of `shape` items (callers guarantee it).
    fn build<'guard, A: ScopedAllocator<'guard>, I: IntoIterator<Item = f64>>(
        mem: &A,
        shape: &[usize],
        iter: I,
    ) -> Result<Self, ExecutionError> {
        let len: usize = shape.iter().product();
        let shape = Array::try_from_slice(mem, shape)?;
        let data = Array::try_from_exact_iter(mem, len, iter)?;
        Ok(HeapNdArray { shape, data })
    }

    /// An array of `shape` with every element `value`.
    pub fn filled<'guard, A: ScopedAllocator<'guard>>(
        mem: &A,
        shape: &[usize],
        value: f64,
    ) -> Result<Self, ExecutionError> {
        Self::build(mem, shape, std::iter::repeat(value))
    }

    /// An array of `shape` from flat row-major `values`, or `None` if the
    /// lengths disagree.
    pub fn from_flat<'guard, A: ScopedAllocator<'guard>>(
        mem: &A,
        shape: &[usize],
        values: &[f64],
    ) -> Result<Option<Self>, ExecutionError> {
        if shape.iter().product::<usize>() != values.len() {
            return Ok(None);
        }
        Self::build(mem, shape, values.iter().copied()).map(Some)
    }

    /// A read-only ndarray view over the heap buffer.
    pub fn view(&self) -> ArrayViewD<'_, f64> {
        ArrayViewD::from_shape(IxDyn(self.shape()), self.data.as_slice())
            .expect("HeapNdArray: shape and data length agree by construction")
    }

    /// Get the shape as a slice.
    pub fn shape(&self) -> &[usize] {
        self.shape.as_slice()
    }

    /// Get the number of dimensions (rank).
    pub fn rank(&self) -> usize {
        self.shape.len()
    }

    /// Get total number of elements.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Whether the array is empty.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Get element at the given coordinates. Returns None if out of bounds.
    pub fn get(&self, coords: &[usize]) -> Option<f64> {
        self.offset(coords)
            .and_then(|o| self.data.as_slice().get(o).copied())
    }

    /// The row-major offset of `coords`, or `None` if out of bounds or of
    /// the wrong rank.
    fn offset(&self, coords: &[usize]) -> Option<usize> {
        // Direct row-major offset: no view (and no shape validation) is
        // rebuilt per call. Shape and data agree by construction.
        let shape = self.shape();
        if coords.len() != shape.len() {
            return None;
        }
        let mut offset = 0usize;
        for (&c, &extent) in coords.iter().zip(shape) {
            if c >= extent {
                return None;
            }
            offset = offset * extent + c;
        }
        Some(offset)
    }

    /// A copy with the element at coords set to value, or `None` if
    /// `coords` is out of bounds.
    pub fn with_set<'guard, A: ScopedAllocator<'guard>>(
        &self,
        mem: &A,
        coords: &[usize],
        value: f64,
    ) -> Result<Option<Self>, ExecutionError> {
        let Some(offset) = self.offset(coords) else {
            return Ok(None);
        };
        let data = self.data.as_slice();
        Self::build(
            mem,
            self.shape(),
            data.iter()
                .enumerate()
                .map(|(i, &x)| if i == offset { value } else { x }),
        )
        .map(Some)
    }

    /// A copy reshaped to the given dimensions, or `None` if the element
    /// counts disagree.
    pub fn reshape<'guard, A: ScopedAllocator<'guard>>(
        &self,
        mem: &A,
        new_shape: &[usize],
    ) -> Result<Option<Self>, ExecutionError> {
        Self::from_flat(mem, new_shape, self.data.as_slice())
    }

    /// The transpose (axes reversed), stored row-major.
    pub fn transpose<'guard, A: ScopedAllocator<'guard>>(
        &self,
        mem: &A,
    ) -> Result<Self, ExecutionError> {
        let t = self.view().reversed_axes();
        Self::build(mem, t.shape(), t.iter().copied())
    }

    /// The sub-array at `index` along `axis` (rank reduced by one), or
    /// `None` if either is out of bounds.
    pub fn slice_along<'guard, A: ScopedAllocator<'guard>>(
        &self,
        mem: &A,
        axis: usize,
        index: usize,
    ) -> Result<Option<Self>, ExecutionError> {
        if axis >= self.rank() || index >= self.shape()[axis] {
            return Ok(None);
        }
        let full = self.view();
        let sub = full.index_axis(ndarray::Axis(axis), index);
        Self::build(mem, sub.shape(), sub.iter().copied()).map(Some)
    }

    /// Convert to a flat Vec of f64.
    pub fn to_flat_vec(&self) -> Vec<f64> {
        self.data.as_slice().to_vec()
    }

    /// Return all coordinate lists in row-major order.
    ///
    /// For an array of shape [d0, d1, ..., dn], returns a Vec of Vec<usize>
    /// where each inner Vec has length n.
    pub fn indices(&self) -> Vec<Vec<usize>> {
        let shape = self.shape();
        if shape.is_empty() {
            return vec![vec![]];
        }
        let n = self.len();
        // Compute strides: stride[k] = product(shape[k+1..])
        let mut strides = vec![1usize; shape.len()];
        for k in (0..shape.len().saturating_sub(1)).rev() {
            strides[k] = strides[k + 1] * shape[k + 1];
        }
        (0..n)
            .map(|flat| {
                shape
                    .iter()
                    .zip(strides.iter())
                    .map(|(&dim, &stride)| (flat / stride) % dim)
                    .collect()
            })
            .collect()
    }

    /// Return values at valid neighbouring coordinates.
    ///
    /// Given a coordinate list and a list of offset lists, adds each offset
    /// to the coordinate and returns the value at that position if it is
    /// within bounds (out-of-bounds neighbours are silently skipped).
    pub fn neighbours_values(&self, coords: &[usize], offsets: &[Vec<i64>]) -> Vec<f64> {
        let shape = self.shape();
        let view = self.view();
        offsets
            .iter()
            .filter_map(|offset| {
                if offset.len() != coords.len() {
                    return None;
                }
                // Compute neighbour coords, checking bounds
                let neighbour: Vec<usize> = coords
                    .iter()
                    .zip(offset.iter())
                    .zip(shape.iter())
                    .map(|((&c, &o), &dim)| {
                        // An overflowing sum is out of bounds too.
                        let nc = i64::try_from(c).ok()?.checked_add(o)?;
                        if nc < 0 || nc >= dim as i64 {
                            None
                        } else {
                            Some(nc as usize)
                        }
                    })
                    .collect::<Option<Vec<usize>>>()?;
                view.get(IxDyn(&neighbour)).copied()
            })
            .collect()
    }

    /// Element-wise map with a function.
    pub fn map<'guard, A: ScopedAllocator<'guard>, F: Fn(f64) -> f64>(
        &self,
        mem: &A,
        f: F,
    ) -> Result<Self, ExecutionError> {
        Self::build(
            mem,
            self.shape(),
            self.data.as_slice().iter().map(|&x| f(x)),
        )
    }

    /// Element-wise binary operation, or `None` if the shapes differ.
    pub fn zip_with<'guard, A: ScopedAllocator<'guard>, F: Fn(f64, f64) -> f64>(
        &self,
        mem: &A,
        other: &HeapNdArray,
        f: F,
    ) -> Result<Option<Self>, ExecutionError> {
        if self.shape() != other.shape() {
            return Ok(None);
        }
        Self::build(
            mem,
            self.shape(),
            self.data
                .as_slice()
                .iter()
                .zip(other.data.as_slice())
                .map(|(&a, &b)| f(a, b)),
        )
        .map(Some)
    }

    /// Scalar broadcast: apply `f(element, scalar)` to every element.
    pub fn scalar_op<'guard, A: ScopedAllocator<'guard>, F: Fn(f64, f64) -> f64>(
        &self,
        mem: &A,
        scalar: f64,
        f: F,
    ) -> Result<Self, ExecutionError> {
        self.map(mem, |x| f(x, scalar))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::memory::{heap::Heap, mutator::MutatorHeapView};

    fn heap_arr(view: &MutatorHeapView, shape: &[usize], vals: Vec<f64>) -> HeapNdArray {
        HeapNdArray::from_flat(view, shape, &vals).unwrap().unwrap()
    }

    #[test]
    fn zeros_creates_correct_shape() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let a = HeapNdArray::filled(&view, &[2, 3], 0.0).unwrap();
        assert_eq!(a.shape(), &[2, 3]);
        assert_eq!(a.len(), 6);
        assert_eq!(a.rank(), 2);
    }

    #[test]
    fn get_and_set() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let a = heap_arr(&view, &[2, 2], vec![1.0, 2.0, 3.0, 4.0]);
        assert_eq!(a.get(&[0, 0]), Some(1.0));
        assert_eq!(a.get(&[1, 1]), Some(4.0));
        assert_eq!(a.get(&[2, 0]), None);

        let b = a.with_set(&view, &[0, 1], 99.0).unwrap().unwrap();
        assert!(a.with_set(&view, &[2, 0], 1.0).unwrap().is_none());
        assert_eq!(b.get(&[0, 1]), Some(99.0));
        assert_eq!(a.get(&[0, 1]), Some(2.0)); // original unchanged
    }

    #[test]
    fn reshape() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let a = heap_arr(&view, &[2, 3], vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
        let b = a.reshape(&view, &[3, 2]).unwrap().unwrap();
        assert_eq!(b.shape(), &[3, 2]);
        assert_eq!(b.get(&[0, 0]), Some(1.0));
        assert_eq!(b.get(&[2, 1]), Some(6.0));
        assert!(a.reshape(&view, &[4, 2]).unwrap().is_none());
    }

    #[test]
    fn transpose_is_stored_row_major() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let a = heap_arr(&view, &[2, 3], vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
        let t = a.transpose(&view).unwrap();
        assert_eq!(t.shape(), &[3, 2]);
        assert_eq!(t.get(&[0, 1]), Some(4.0));
        assert_eq!(t.to_flat_vec(), vec![1.0, 4.0, 2.0, 5.0, 3.0, 6.0]);
    }

    #[test]
    fn slice_along_axis() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let a = heap_arr(&view, &[2, 3], vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
        let row0 = a.slice_along(&view, 0, 0).unwrap().unwrap();
        assert_eq!(row0.shape(), &[3]);
        assert_eq!(row0.to_flat_vec(), vec![1.0, 2.0, 3.0]);
        let col1 = a.slice_along(&view, 1, 1).unwrap().unwrap();
        assert_eq!(col1.to_flat_vec(), vec![2.0, 5.0]);
        assert!(a.slice_along(&view, 2, 0).unwrap().is_none());
    }

    #[test]
    fn zip_with_and_scalar_op() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let a = heap_arr(&view, &[2], vec![1.0, 2.0]);
        let b = heap_arr(&view, &[2], vec![10.0, 20.0]);
        let c = a.zip_with(&view, &b, |x, y| x + y).unwrap().unwrap();
        assert_eq!(c.to_flat_vec(), vec![11.0, 22.0]);
        let d = a.scalar_op(&view, 10.0, |x, s| x * s).unwrap();
        assert_eq!(d.to_flat_vec(), vec![10.0, 20.0]);
    }

    #[test]
    fn get_matches_view_indexing() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let vals: Vec<f64> = (0..24).map(f64::from).collect();
        let arr = heap_arr(&view, &[2, 3, 4], vals);
        for i in 0..2 {
            for j in 0..3 {
                for k in 0..4 {
                    assert_eq!(
                        arr.get(&[i, j, k]),
                        arr.view().get(IxDyn(&[i, j, k])).copied()
                    );
                }
            }
        }
        assert_eq!(arr.get(&[2, 0, 0]), None);
        assert_eq!(arr.get(&[0, 3, 0]), None);
        assert_eq!(arr.get(&[0, 0]), None);
        assert_eq!(arr.get(&[0, 0, 0, 0]), None);
    }

    #[test]
    fn rank_zero_array() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let a = heap_arr(&view, &[], vec![7.0]);
        assert_eq!(a.rank(), 0);
        assert_eq!(a.len(), 1);
        assert_eq!(a.get(&[]), Some(7.0));
    }

    #[test]
    fn exact_iter_writes_in_place() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let a = Array::try_from_exact_iter(&view, 2, [1.0f64, 2.0, 3.0]).unwrap();
        assert_eq!(a.as_slice(), &[1.0, 2.0]);
        let b = Array::<f64>::try_from_exact_iter(&view, 0, std::iter::empty()).unwrap();
        assert!(b.is_empty());
    }
}
