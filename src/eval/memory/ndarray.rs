//! N-dimensional array (tensor) type for the eucalypt VM.
//!
//! Arrays store `f64` values in a flat buffer with shape metadata,
//! backed by the `ndarray` crate. They provide O(1) coordinate-based
//! indexing via stride arithmetic.
//!
//! GC does not trace into array contents because `f64` values contain
//! no heap references.

use ndarray::{ArrayD, IxDyn};

use super::alloc::StgObject;

/// An n-dimensional array of f64 values, stored on the heap.
#[derive(Debug, Clone)]
pub struct HeapNdArray {
    data: ArrayD<f64>,
}

impl StgObject for HeapNdArray {}

impl HeapNdArray {
    /// Create from an existing ndarray.
    pub fn from_ndarray(data: ArrayD<f64>) -> Self {
        HeapNdArray { data }
    }

    /// Create an array of zeros with the given shape.
    pub fn zeros(shape: &[usize]) -> Self {
        HeapNdArray {
            data: ArrayD::zeros(IxDyn(shape)),
        }
    }

    /// Create an array filled with a constant value.
    pub fn filled(shape: &[usize], value: f64) -> Self {
        HeapNdArray {
            data: ArrayD::from_elem(IxDyn(shape), value),
        }
    }

    /// Create from a flat vec of f64 and a shape.
    pub fn from_flat(shape: &[usize], values: Vec<f64>) -> Option<Self> {
        ArrayD::from_shape_vec(IxDyn(shape), values)
            .ok()
            .map(|data| HeapNdArray { data })
    }

    /// Access the underlying ndarray.
    pub fn data(&self) -> &ArrayD<f64> {
        &self.data
    }

    /// Get the shape as a slice.
    pub fn shape(&self) -> &[usize] {
        self.data.shape()
    }

    /// Get the number of dimensions (rank).
    pub fn rank(&self) -> usize {
        self.data.ndim()
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
        self.data.get(IxDyn(coords)).copied()
    }

    /// Return a new array with the element at coords set to value.
    pub fn with_set(&self, coords: &[usize], value: f64) -> Option<Self> {
        let mut new_data = self.data.clone();
        let elem = new_data.get_mut(IxDyn(coords))?;
        *elem = value;
        Some(HeapNdArray { data: new_data })
    }

    /// Return a new array reshaped to the given dimensions.
    pub fn reshape(&self, new_shape: &[usize]) -> Option<Self> {
        let flat: Vec<f64> = self.data.iter().copied().collect();
        Self::from_flat(new_shape, flat)
    }

    /// Return the transpose (reverses all axes).
    pub fn transpose(&self) -> Self {
        let view = self.data.view();
        let transposed = view.reversed_axes();
        HeapNdArray {
            data: transposed.to_owned(),
        }
    }

    /// Extract a slice along a given dimension at a given index.
    /// Reduces rank by 1.
    pub fn slice_along(&self, axis: usize, index: usize) -> Option<Self> {
        if axis >= self.rank() || index >= self.shape()[axis] {
            return None;
        }
        let view = self.data.index_axis(ndarray::Axis(axis), index);
        Some(HeapNdArray {
            data: view.to_owned(),
        })
    }

    /// Convert to a flat Vec of f64.
    pub fn to_flat_vec(&self) -> Vec<f64> {
        self.data.iter().copied().collect()
    }

    /// Return all coordinate lists in row-major order.
    ///
    /// For an array of shape [d0, d1, ..., dn], returns a Vec of Vec<usize>
    /// where each inner Vec has length n.
    pub fn indices(&self) -> Vec<Vec<usize>> {
        let shape = self.data.shape();
        if shape.is_empty() {
            return vec![vec![]];
        }
        let n = self.data.len();
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
        let shape = self.data.shape();
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
                self.data.get(IxDyn(&neighbour)).copied()
            })
            .collect()
    }

    /// Element-wise map with a function.
    pub fn map<F: Fn(f64) -> f64>(&self, f: F) -> Self {
        HeapNdArray {
            data: self.data.mapv(&f),
        }
    }

    /// Element-wise binary operation. Arrays must have the same shape.
    pub fn zip_with<F: Fn(f64, f64) -> f64>(&self, other: &HeapNdArray, f: F) -> Option<Self> {
        if self.shape() != other.shape() {
            return None;
        }
        let result = ndarray::Zip::from(&self.data)
            .and(&other.data)
            .map_collect(|&a, &b| f(a, b));
        Some(HeapNdArray { data: result })
    }

    /// Scalar broadcast: apply operation with a scalar to every element.
    pub fn scalar_op<F: Fn(f64, f64) -> f64>(&self, scalar: f64, f: F) -> Self {
        HeapNdArray {
            data: self.data.mapv(|x| f(x, scalar)),
        }
    }
}

impl PartialEq for HeapNdArray {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl Eq for HeapNdArray {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zeros_creates_correct_shape() {
        let a = HeapNdArray::zeros(&[2, 3]);
        assert_eq!(a.shape(), &[2, 3]);
        assert_eq!(a.len(), 6);
        assert_eq!(a.rank(), 2);
    }

    #[test]
    fn get_and_set() {
        let a = HeapNdArray::from_flat(&[2, 2], vec![1.0, 2.0, 3.0, 4.0]).unwrap();
        assert_eq!(a.get(&[0, 0]), Some(1.0));
        assert_eq!(a.get(&[1, 1]), Some(4.0));
        assert_eq!(a.get(&[2, 0]), None);

        let b = a.with_set(&[0, 1], 99.0).unwrap();
        assert_eq!(b.get(&[0, 1]), Some(99.0));
        assert_eq!(a.get(&[0, 1]), Some(2.0)); // original unchanged
    }

    #[test]
    fn reshape() {
        let a = HeapNdArray::from_flat(&[2, 3], vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0]).unwrap();
        let b = a.reshape(&[3, 2]).unwrap();
        assert_eq!(b.shape(), &[3, 2]);
        assert_eq!(b.get(&[0, 0]), Some(1.0));
        assert_eq!(b.get(&[2, 1]), Some(6.0));
    }

    #[test]
    fn transpose_2d() {
        let a = HeapNdArray::from_flat(&[2, 3], vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0]).unwrap();
        let t = a.transpose();
        assert_eq!(t.shape(), &[3, 2]);
        assert_eq!(t.get(&[0, 0]), Some(1.0));
        assert_eq!(t.get(&[0, 1]), Some(4.0));
    }

    #[test]
    fn slice_along_axis() {
        let a = HeapNdArray::from_flat(&[2, 3], vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0]).unwrap();
        let row0 = a.slice_along(0, 0).unwrap();
        assert_eq!(row0.shape(), &[3]);
        assert_eq!(row0.to_flat_vec(), vec![1.0, 2.0, 3.0]);
    }

    #[test]
    fn zip_with_add() {
        let a = HeapNdArray::from_flat(&[2], vec![1.0, 2.0]).unwrap();
        let b = HeapNdArray::from_flat(&[2], vec![10.0, 20.0]).unwrap();
        let c = a.zip_with(&b, |x, y| x + y).unwrap();
        assert_eq!(c.to_flat_vec(), vec![11.0, 22.0]);
    }

    #[test]
    fn scalar_op_multiply() {
        let a = HeapNdArray::from_flat(&[3], vec![1.0, 2.0, 3.0]).unwrap();
        let b = a.scalar_op(10.0, |x, s| x * s);
        assert_eq!(b.to_flat_vec(), vec![10.0, 20.0, 30.0]);
    }
}
