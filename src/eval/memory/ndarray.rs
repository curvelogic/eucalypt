//! N-dimensional array (tensor) type for the eucalypt VM.
//!
//! Arrays store `f64` values in a flat row-major buffer with shape
//! metadata. They provide O(1) coordinate-based indexing via stride
//! arithmetic.
//!
//! All storage lives in the GC heap (eu-oaz72.5): the shape and the
//! values are heap [`Array`]s of plain scalars, so the collector marks
//! and evacuates the buffers but traces nothing inside them. Operations
//! compute on an `ndarray` view over the heap buffer and produce a
//! transient (Rust-owned, never heap-stored) `ArrayD<f64>`, which the
//! caller copies into a fresh `HeapNdArray`.

use ndarray::{ArrayD, ArrayViewD, IxDyn};

use super::{
    alloc::{ScopedAllocator, StgObject},
    array::Array,
    collect::{CollectorHeapView, CollectorScope, GcScannable, OpaqueHeapBytes, ScanPtr},
};
use crate::eval::error::ExecutionError;

/// An array of zeros with the given shape (transient).
pub fn zeros(shape: &[usize]) -> ArrayD<f64> {
    ArrayD::zeros(IxDyn(shape))
}

/// An array filled with a constant value (transient).
pub fn filled(shape: &[usize], value: f64) -> ArrayD<f64> {
    ArrayD::from_elem(IxDyn(shape), value)
}

/// An array from a flat row-major vec of values and a shape, or `None`
/// if the lengths disagree (transient).
pub fn from_flat(shape: &[usize], values: Vec<f64>) -> Option<ArrayD<f64>> {
    ArrayD::from_shape_vec(IxDyn(shape), values).ok()
}

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
    /// Copy an (owned, transient) ndarray into the heap in row-major order.
    pub fn from_ndarray<'guard, A: ScopedAllocator<'guard>>(
        mem: &A,
        arr: &ArrayD<f64>,
    ) -> Result<Self, ExecutionError> {
        let shape = Array::try_from_slice(mem, arr.shape())?;
        let data = match arr.as_slice() {
            Some(flat) => Array::try_from_slice(mem, flat)?,
            None => {
                let flat: Vec<f64> = arr.iter().copied().collect();
                Array::try_from_slice(mem, &flat)?
            }
        };
        Ok(HeapNdArray { shape, data })
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
        self.view().get(IxDyn(coords)).copied()
    }

    /// A copy with the element at coords set to value.
    pub fn with_set(&self, coords: &[usize], value: f64) -> Option<ArrayD<f64>> {
        let mut new_data = self.view().to_owned();
        let elem = new_data.get_mut(IxDyn(coords))?;
        *elem = value;
        Some(new_data)
    }

    /// A copy reshaped to the given dimensions.
    pub fn reshape(&self, new_shape: &[usize]) -> Option<ArrayD<f64>> {
        from_flat(new_shape, self.to_flat_vec())
    }

    /// The transpose (reverses all axes).
    pub fn transpose(&self) -> ArrayD<f64> {
        self.view().reversed_axes().to_owned()
    }

    /// Extract a slice along a given dimension at a given index.
    /// Reduces rank by 1.
    pub fn slice_along(&self, axis: usize, index: usize) -> Option<ArrayD<f64>> {
        if axis >= self.rank() || index >= self.shape()[axis] {
            return None;
        }
        Some(
            self.view()
                .index_axis(ndarray::Axis(axis), index)
                .to_owned(),
        )
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
                        let nc = c as i64 + o;
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
    pub fn map<F: Fn(f64) -> f64>(&self, f: F) -> ArrayD<f64> {
        self.view().mapv(&f)
    }

    /// Element-wise binary operation. Arrays must have the same shape.
    pub fn zip_with<F: Fn(f64, f64) -> f64>(
        &self,
        other: &HeapNdArray,
        f: F,
    ) -> Option<ArrayD<f64>> {
        if self.shape() != other.shape() {
            return None;
        }
        Some(
            ndarray::Zip::from(&self.view())
                .and(&other.view())
                .map_collect(|&a, &b| f(a, b)),
        )
    }

    /// Scalar broadcast: apply operation with a scalar to every element.
    pub fn scalar_op<F: Fn(f64, f64) -> f64>(&self, scalar: f64, f: F) -> ArrayD<f64> {
        self.view().mapv(|x| f(x, scalar))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::memory::{heap::Heap, mutator::MutatorHeapView};

    fn heap_arr(view: &MutatorHeapView, shape: &[usize], vals: Vec<f64>) -> HeapNdArray {
        HeapNdArray::from_ndarray(view, &from_flat(shape, vals).unwrap()).unwrap()
    }

    #[test]
    fn zeros_creates_correct_shape() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let a = HeapNdArray::from_ndarray(&view, &zeros(&[2, 3])).unwrap();
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

        let b = HeapNdArray::from_ndarray(&view, &a.with_set(&[0, 1], 99.0).unwrap()).unwrap();
        assert_eq!(b.get(&[0, 1]), Some(99.0));
        assert_eq!(a.get(&[0, 1]), Some(2.0)); // original unchanged
    }

    #[test]
    fn reshape() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let a = heap_arr(&view, &[2, 3], vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
        let b = a.reshape(&[3, 2]).unwrap();
        assert_eq!(b.shape(), &[3, 2]);
        assert_eq!(b[[0, 0]], 1.0);
        assert_eq!(b[[2, 1]], 6.0);
    }

    #[test]
    fn transpose_is_stored_row_major() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let a = heap_arr(&view, &[2, 3], vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
        let t = HeapNdArray::from_ndarray(&view, &a.transpose()).unwrap();
        assert_eq!(t.shape(), &[3, 2]);
        assert_eq!(t.get(&[0, 1]), Some(4.0));
        assert_eq!(t.to_flat_vec(), vec![1.0, 4.0, 2.0, 5.0, 3.0, 6.0]);
    }

    #[test]
    fn slice_along_axis() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let a = heap_arr(&view, &[2, 3], vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0]);
        let row0 = a.slice_along(0, 0).unwrap();
        assert_eq!(row0.shape(), &[3]);
        assert_eq!(
            row0.iter().copied().collect::<Vec<_>>(),
            vec![1.0, 2.0, 3.0]
        );
    }

    #[test]
    fn zip_with_and_scalar_op() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let a = heap_arr(&view, &[2], vec![1.0, 2.0]);
        let b = heap_arr(&view, &[2], vec![10.0, 20.0]);
        let c = a.zip_with(&b, |x, y| x + y).unwrap();
        assert_eq!(c.iter().copied().collect::<Vec<_>>(), vec![11.0, 22.0]);
        let d = a.scalar_op(10.0, |x, s| x * s);
        assert_eq!(d.iter().copied().collect::<Vec<_>>(), vec![10.0, 20.0]);
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
}
