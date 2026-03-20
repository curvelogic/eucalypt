//! Native vec type for the eucalypt VM.
//!
//! Vecs contain only primitive values (numbers, strings, symbols) and
//! provide O(1) indexed access, slicing, and random sampling.

use super::alloc::StgObject;
use super::set::Primitive;

/// A vector of primitive values, stored on the heap.
///
/// GC does not trace into vec contents because primitives contain no
/// heap references (strings are owned, symbols are interned IDs).
#[derive(Debug, Clone)]
pub struct HeapVec {
    elements: Vec<Primitive>,
}

impl StgObject for HeapVec {}

impl HeapVec {
    /// Create an empty vec.
    pub fn empty() -> Self {
        HeapVec {
            elements: Vec::new(),
        }
    }

    /// Create a vec from an iterator of primitives.
    pub fn from_primitives(iter: impl Iterator<Item = Primitive>) -> Self {
        HeapVec {
            elements: iter.collect(),
        }
    }

    /// Return the number of elements.
    pub fn len(&self) -> usize {
        self.elements.len()
    }

    /// Return whether the vec is empty.
    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }

    /// Return the element at the given index, or None if out of bounds.
    pub fn get(&self, index: usize) -> Option<&Primitive> {
        self.elements.get(index)
    }

    /// Return a sub-vec containing elements in the range `[from, to)`.
    ///
    /// Indices are clamped to the vec length.
    pub fn slice(&self, from: usize, to: usize) -> Self {
        let from = from.min(self.elements.len());
        let to = to.min(self.elements.len()).max(from);
        HeapVec {
            elements: self.elements[from..to].to_vec(),
        }
    }

    /// Return all elements as a slice.
    pub fn elements(&self) -> &[Primitive] {
        &self.elements
    }
}

impl PartialEq for HeapVec {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

impl Eq for HeapVec {}

#[cfg(test)]
mod tests {
    use super::*;
    use ordered_float::OrderedFloat;

    #[test]
    fn empty_vec_has_no_elements() {
        let v = HeapVec::empty();
        assert!(v.is_empty());
        assert_eq!(v.len(), 0);
    }

    #[test]
    fn from_primitives_preserves_order() {
        let v = HeapVec::from_primitives(
            vec![
                Primitive::Num(OrderedFloat(1.0)),
                Primitive::Num(OrderedFloat(2.0)),
                Primitive::Num(OrderedFloat(3.0)),
            ]
            .into_iter(),
        );
        assert_eq!(v.len(), 3);
        assert_eq!(v.get(0), Some(&Primitive::Num(OrderedFloat(1.0))));
        assert_eq!(v.get(2), Some(&Primitive::Num(OrderedFloat(3.0))));
    }

    #[test]
    fn get_returns_none_out_of_bounds() {
        let v = HeapVec::from_primitives(vec![Primitive::Num(OrderedFloat(1.0))].into_iter());
        assert!(v.get(1).is_none());
    }

    #[test]
    fn slice_clamps_to_bounds() {
        let v = HeapVec::from_primitives(
            vec![
                Primitive::Num(OrderedFloat(1.0)),
                Primitive::Num(OrderedFloat(2.0)),
                Primitive::Num(OrderedFloat(3.0)),
            ]
            .into_iter(),
        );
        let s = v.slice(1, 3);
        assert_eq!(s.len(), 2);
        assert_eq!(s.get(0), Some(&Primitive::Num(OrderedFloat(2.0))));
        // Clamping beyond end
        let s2 = v.slice(2, 100);
        assert_eq!(s2.len(), 1);
        // Empty when from >= to
        let s3 = v.slice(3, 3);
        assert_eq!(s3.len(), 0);
    }
}
