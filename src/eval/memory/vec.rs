//! Native vec type for the eucalypt VM.
//!
//! Vecs contain only primitive values (numbers, strings, symbols) and
//! provide O(1) indexed access, slicing, and random sampling.
//!
//! All storage lives in the GC heap (eu-oaz72.5): elements are a heap
//! [`Array`] of [`Primitive`]s whose string elements are
//! `RefPtr<HeapString>`s traced by the collector.

use super::{
    alloc::{ScopedAllocator, StgObject},
    array::Array,
    collect::{CollectorHeapView, CollectorScope, GcScannable, ScanPtr},
    set::{scan_primitive_array, update_primitive_array, Primitive},
};
use crate::eval::error::ExecutionError;

/// A vector of primitive values, stored entirely in the GC heap.
pub struct HeapVec {
    elements: Array<Primitive>,
}

impl StgObject for HeapVec {}

impl GcScannable for HeapVec {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        scan_primitive_array(&self.elements, scope, marker, out);
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        update_primitive_array(&mut self.elements, heap);
    }
}

impl HeapVec {
    /// Create a vec by copying a slice of primitives into the heap.
    pub fn from_slice<'guard, A: ScopedAllocator<'guard>>(
        mem: &A,
        elements: &[Primitive],
    ) -> Result<Self, ExecutionError> {
        Ok(HeapVec {
            elements: Array::try_from_slice(mem, elements)?,
        })
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
    pub fn get(&self, index: usize) -> Option<Primitive> {
        self.elements().get(index).copied()
    }

    /// Return the elements in the range `[from, to)`, clamped to the
    /// vec length.
    pub fn slice(&self, from: usize, to: usize) -> &[Primitive] {
        let elems = self.elements();
        let from = from.min(elems.len());
        let to = to.min(elems.len()).max(from);
        &elems[from..to]
    }

    /// Return all elements as a slice.
    pub fn elements(&self) -> &[Primitive] {
        self.elements.as_slice()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::memory::{heap::Heap, mutator::MutatorHeapView};
    use ordered_float::OrderedFloat;

    fn num(n: f64) -> Primitive {
        Primitive::Num(OrderedFloat(n))
    }

    fn val(p: Option<Primitive>) -> Option<f64> {
        p.map(|p| match p {
            Primitive::Num(n) => n.into_inner(),
            _ => panic!("not a number"),
        })
    }

    #[test]
    fn empty_vec_has_no_elements() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let v = HeapVec::from_slice(&view, &[]).unwrap();
        assert!(v.is_empty());
        assert_eq!(v.len(), 0);
    }

    #[test]
    fn preserves_order_and_bounds() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let v = HeapVec::from_slice(&view, &[num(3.0), num(1.0), num(2.0)]).unwrap();
        assert_eq!(v.len(), 3);
        assert_eq!(val(v.get(0)), Some(3.0));
        assert_eq!(val(v.get(2)), Some(2.0));
        assert!(v.get(3).is_none());
    }

    #[test]
    fn slice_clamps_to_bounds() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let v = HeapVec::from_slice(&view, &[num(1.0), num(2.0), num(3.0)]).unwrap();
        assert_eq!(v.slice(1, 3).len(), 2);
        assert_eq!(v.slice(2, 100).len(), 1);
        assert_eq!(v.slice(3, 3).len(), 0);
        assert_eq!(v.slice(5, 1).len(), 0);
    }
}
