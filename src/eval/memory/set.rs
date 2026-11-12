//! Native set type for the eucalypt VM.
//!
//! Sets contain only primitive values (numbers, strings, symbols).
//!
//! All storage lives in the GC heap (eu-oaz72.5): a set is a sorted,
//! deduplicated heap [`Array`] of [`Primitive`] elements, and string
//! elements are `RefPtr<HeapString>`s traced by the collector. No Rust
//! allocation is owned by a heap object, so nothing leaks when a set is
//! swept. Membership is a binary search; add and remove copy into a
//! fresh array (O(n)); union, intersection and difference are linear
//! merges.

use std::cmp::Ordering;

use ordered_float::OrderedFloat;
use serde_json::Number;

use super::{
    alloc::{MutatorScope, ScopedAllocator, StgObject},
    array::Array,
    collect::{CollectorHeapView, CollectorScope, GcScannable, OpaqueHeapBytes, ScanPtr},
    string::HeapString,
    symbol::SymbolId,
    syntax::RefPtr,
};
use crate::eval::error::ExecutionError;

/// A primitive value that can be stored in a set or vec.
///
/// `Copy` and free of Rust-owned storage: a string element points at an
/// immutable `HeapString` in the GC heap.
#[derive(Clone, Copy, Debug)]
pub enum Primitive {
    Num(OrderedFloat<f64>),
    Str(RefPtr<HeapString>),
    Sym(SymbolId),
}

impl Primitive {
    /// Convert a `serde_json::Number` to a set-safe `OrderedFloat<f64>`.
    pub fn from_number(n: &Number) -> Self {
        Primitive::Num(OrderedFloat(n.as_f64().unwrap_or(0.0)))
    }

    /// Convert back to a `serde_json::Number` for rendering.
    pub fn to_number(&self) -> Option<Number> {
        match self {
            Primitive::Num(n) => Number::from_f64(n.into_inner()),
            _ => None,
        }
    }

    /// The string content of a string element.
    ///
    /// The guard ties the result to a mutator scope, during which the
    /// collector cannot run and move or reclaim the string.
    pub fn str_in<'guard>(&self, _guard: &'guard dyn MutatorScope) -> Option<&'guard str> {
        match self {
            // SAFETY: the pointer refers to a live, immutable HeapString:
            // it is reachable from a live set/vec or argument, and no
            // collection can run while the mutator scope `'guard` is held.
            Primitive::Str(p) => Some(unsafe { &*p.as_ptr() }.as_str()),
            _ => None,
        }
    }

    /// Total order: numbers before strings before symbols; numbers by
    /// `OrderedFloat`, strings by content, symbols by id.
    pub fn cmp_in(&self, other: &Primitive, guard: &dyn MutatorScope) -> Ordering {
        match (self, other) {
            (Primitive::Num(a), Primitive::Num(b)) => a.cmp(b),
            (Primitive::Str(_), Primitive::Str(_)) => self.str_in(guard).cmp(&other.str_in(guard)),
            (Primitive::Sym(a), Primitive::Sym(b)) => a.cmp(b),
            (Primitive::Num(_), _) => Ordering::Less,
            (_, Primitive::Num(_)) => Ordering::Greater,
            (Primitive::Str(_), Primitive::Sym(_)) => Ordering::Less,
            (Primitive::Sym(_), Primitive::Str(_)) => Ordering::Greater,
        }
    }

    /// Mark a string element's `HeapString` and queue it for scanning.
    pub(crate) fn scan<'a>(
        &self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        if let Primitive::Str(p) = self {
            if marker.mark(*p) {
                out.push(ScanPtr::from_non_null(scope, *p));
            }
        }
    }

    /// Rewrite a string element's pointer if its `HeapString` moved.
    pub(crate) fn update(&mut self, heap: &CollectorHeapView<'_>) {
        if let Primitive::Str(p) = self {
            if let Some(new_ptr) = heap.forwarded_to(*p) {
                *p = new_ptr;
            }
        }
    }
}

/// Sort and deduplicate a vector of primitives into set order.
pub fn sort_dedup(guard: &dyn MutatorScope, mut elems: Vec<Primitive>) -> Vec<Primitive> {
    elems.sort_by(|a, b| a.cmp_in(b, guard));
    elems.dedup_by(|a, b| a.cmp_in(b, guard) == Ordering::Equal);
    elems
}

/// Mark a heap array of primitives: its backing buffer (queued as opaque
/// bytes so it can be evacuated) and every string element.
pub(crate) fn scan_primitive_array<'a>(
    elements: &'a Array<Primitive>,
    scope: &'a dyn CollectorScope,
    marker: &mut CollectorHeapView<'a>,
    out: &mut Vec<ScanPtr<'a>>,
) {
    if marker.mark_array(elements) {
        if let Some(backing) = elements.allocated_data() {
            out.push(ScanPtr::from_non_null(
                scope,
                backing.cast::<OpaqueHeapBytes>(),
            ));
        }
    }
    for elem in elements.as_slice() {
        elem.scan(scope, marker, out);
    }
}

/// Rewrite the backing pointer of a heap array of primitives, then the
/// string pointers of its elements (read at the new location).
pub(crate) fn update_primitive_array(
    elements: &mut Array<Primitive>,
    heap: &CollectorHeapView<'_>,
) {
    if let Some(old_ptr) = elements.allocated_data() {
        if let Some(new_ptr) = heap.forwarded_to(old_ptr) {
            // SAFETY: new_ptr is the evacuated copy of the same backing
            // allocation, holding `len` initialised elements.
            unsafe { elements.set_backing_ptr(new_ptr.cast()) };
        }
    }
    for elem in elements.as_mut_slice() {
        elem.update(heap);
    }
}

/// A set of primitive values, stored entirely in the GC heap.
///
/// Elements are sorted by [`Primitive::cmp_in`] and contain no
/// duplicates.
pub struct HeapSet {
    elements: Array<Primitive>,
}

impl StgObject for HeapSet {}

impl GcScannable for HeapSet {
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

impl HeapSet {
    /// Create a set from elements already sorted and deduplicated.
    pub fn from_sorted<'guard, A: ScopedAllocator<'guard>>(
        mem: &A,
        sorted: &[Primitive],
    ) -> Result<Self, ExecutionError> {
        Ok(HeapSet {
            elements: Array::try_from_slice(mem, sorted)?,
        })
    }

    /// The elements, in sorted order.
    pub fn elements(&self) -> &[Primitive] {
        self.elements.as_slice()
    }

    /// Binary search for an element: `Ok(index)` if present, else
    /// `Err(insertion_index)`.
    pub fn find(&self, guard: &dyn MutatorScope, elem: &Primitive) -> Result<usize, usize> {
        self.elements()
            .binary_search_by(|probe| probe.cmp_in(elem, guard))
    }

    /// Check if the set contains an element.
    pub fn contains(&self, guard: &dyn MutatorScope, elem: &Primitive) -> bool {
        self.find(guard, elem).is_ok()
    }

    /// Elements of this set with `elem` added, or `None` if already present.
    pub fn with_added(&self, guard: &dyn MutatorScope, elem: Primitive) -> Option<Vec<Primitive>> {
        match self.find(guard, &elem) {
            Ok(_) => None,
            Err(at) => {
                let old = self.elements();
                let mut out = Vec::with_capacity(old.len() + 1);
                out.extend_from_slice(&old[..at]);
                out.push(elem);
                out.extend_from_slice(&old[at..]);
                Some(out)
            }
        }
    }

    /// Elements of this set with `elem` removed, or `None` if absent.
    pub fn with_removed(
        &self,
        guard: &dyn MutatorScope,
        elem: &Primitive,
    ) -> Option<Vec<Primitive>> {
        match self.find(guard, elem) {
            Err(_) => None,
            Ok(at) => {
                let old = self.elements();
                let mut out = Vec::with_capacity(old.len() - 1);
                out.extend_from_slice(&old[..at]);
                out.extend_from_slice(&old[at + 1..]);
                Some(out)
            }
        }
    }

    /// Return the number of elements.
    pub fn len(&self) -> usize {
        self.elements.len()
    }

    /// Return whether the set is empty.
    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }

    /// Sorted elements of the union of two sets.
    pub fn union(&self, guard: &dyn MutatorScope, other: &HeapSet) -> Vec<Primitive> {
        merge(guard, self.elements(), other.elements(), true, true, true)
    }

    /// Sorted elements of the intersection of two sets.
    pub fn intersect(&self, guard: &dyn MutatorScope, other: &HeapSet) -> Vec<Primitive> {
        merge(guard, self.elements(), other.elements(), false, true, false)
    }

    /// Sorted elements of the difference (self - other).
    pub fn diff(&self, guard: &dyn MutatorScope, other: &HeapSet) -> Vec<Primitive> {
        merge(guard, self.elements(), other.elements(), true, false, false)
    }
}

/// Linear merge of two sorted, deduplicated slices, keeping elements only
/// in `a` (`left`), in both (`both`), and only in `b` (`right`).
fn merge(
    guard: &dyn MutatorScope,
    a: &[Primitive],
    b: &[Primitive],
    left: bool,
    both: bool,
    right: bool,
) -> Vec<Primitive> {
    let mut out = Vec::new();
    let (mut i, mut j) = (0, 0);
    while i < a.len() && j < b.len() {
        match a[i].cmp_in(&b[j], guard) {
            Ordering::Less => {
                if left {
                    out.push(a[i]);
                }
                i += 1;
            }
            Ordering::Greater => {
                if right {
                    out.push(b[j]);
                }
                j += 1;
            }
            Ordering::Equal => {
                if both {
                    out.push(a[i]);
                }
                i += 1;
                j += 1;
            }
        }
    }
    if left {
        out.extend_from_slice(&a[i..]);
    }
    if right {
        out.extend_from_slice(&b[j..]);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::memory::{
        heap::Heap, mutator::MutatorHeapView, symbol::SymbolPool, syntax::StgBuilder,
    };

    fn num(n: f64) -> Primitive {
        Primitive::Num(OrderedFloat(n))
    }

    fn set(view: &MutatorHeapView, elems: Vec<Primitive>) -> HeapSet {
        HeapSet::from_sorted(view, &sort_dedup(view, elems)).unwrap()
    }

    fn nums(s: &[Primitive]) -> Vec<f64> {
        s.iter()
            .map(|p| match p {
                Primitive::Num(n) => n.into_inner(),
                _ => panic!("not a number"),
            })
            .collect()
    }

    #[test]
    fn sort_dedup_orders_and_deduplicates() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let mut pool = SymbolPool::new();
        let z = pool.intern("z");
        let b = view.str("b").unwrap().as_ptr();
        let a = view.str("a").unwrap().as_ptr();
        let a2 = view.str("a").unwrap().as_ptr();
        let s = set(
            &view,
            vec![
                Primitive::Sym(z),
                Primitive::Str(b),
                num(3.0),
                Primitive::Str(a),
                num(1.0),
                Primitive::Str(a2),
                num(1.0),
            ],
        );
        let e = s.elements();
        assert_eq!(e.len(), 5);
        assert_eq!(nums(&e[..2]), vec![1.0, 3.0]);
        assert_eq!(e[2].str_in(&view), Some("a"));
        assert_eq!(e[3].str_in(&view), Some("b"));
        assert!(matches!(e[4], Primitive::Sym(_)));
    }

    #[test]
    fn add_remove_contains() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let s = set(&view, vec![num(1.0), num(3.0)]);
        assert!(s.with_added(&view, num(3.0)).is_none());
        let added = s.with_added(&view, num(2.0)).unwrap();
        assert_eq!(nums(&added), vec![1.0, 2.0, 3.0]);
        assert!(s.with_removed(&view, &num(2.0)).is_none());
        assert_eq!(nums(&s.with_removed(&view, &num(1.0)).unwrap()), vec![3.0]);
        assert!(s.contains(&view, &num(3.0)));
        assert!(!s.contains(&view, &num(2.0)));
        let hello = view.str("hello").unwrap().as_ptr();
        let hello2 = view.str("hello").unwrap().as_ptr();
        let t = HeapSet::from_sorted(&view, &[Primitive::Str(hello)]).unwrap();
        assert!(t.contains(&view, &Primitive::Str(hello2)));
    }

    #[test]
    fn set_algebra() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let a = set(&view, vec![num(1.0), num(2.0), num(4.0)]);
        let b = set(&view, vec![num(2.0), num(3.0), num(4.0), num(5.0)]);
        assert_eq!(nums(&a.union(&view, &b)), vec![1.0, 2.0, 3.0, 4.0, 5.0]);
        assert_eq!(nums(&a.intersect(&view, &b)), vec![2.0, 4.0]);
        assert_eq!(nums(&a.diff(&view, &b)), vec![1.0]);
        assert_eq!(nums(&b.diff(&view, &a)), vec![3.0, 5.0]);
    }

    #[test]
    fn empty_set() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let s = HeapSet::from_sorted(&view, &[]).unwrap();
        assert!(s.is_empty());
        assert_eq!(s.len(), 0);
        assert!(!s.contains(&view, &num(1.0)));
    }

    #[test]
    fn primitive_number_round_trip() {
        let p = Primitive::from_number(&Number::from(42));
        assert!(matches!(p, Primitive::Num(n) if n.into_inner() == 42.0));
        assert!(Primitive::Num(OrderedFloat(7.5)).to_number().is_some());
    }
}
