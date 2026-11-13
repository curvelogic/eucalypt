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
    dict::cmp_int_float,
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
    Num(ExactNum),
    Str(RefPtr<HeapString>),
    Sym(SymbolId),
}

/// A number element, kept exact: integers are not rounded through `f64`,
/// so distinct integers above 2^53 stay distinct (as dict keys do).
#[derive(Clone, Copy, Debug)]
pub enum ExactNum {
    /// An integer in `i64` range.
    Int(i64),
    /// An integer above `i64::MAX` (never used for values in `i64` range).
    UInt(u64),
    /// A non-integer number (or one stored as a float), ordered totally.
    Float(OrderedFloat<f64>),
}

impl ExactNum {
    /// Convert a number, keeping integers exact.
    pub fn from_number(n: &Number) -> Self {
        if let Some(i) = n.as_i64() {
            ExactNum::Int(i)
        } else if let Some(u) = n.as_u64() {
            ExactNum::UInt(u)
        } else {
            ExactNum::Float(OrderedFloat(n.as_f64().unwrap_or(f64::NAN)))
        }
    }

    /// Convert back to a number (`None` for a non-finite float, which a
    /// `Number` cannot hold).
    pub fn to_number(&self) -> Option<Number> {
        match self {
            ExactNum::Int(i) => Some(Number::from(*i)),
            ExactNum::UInt(u) => Some(Number::from(*u)),
            ExactNum::Float(f) => Number::from_f64(f.into_inner()),
        }
    }

    /// The value as an `f64` (rounded for integers beyond 2^53).
    pub fn as_f64(&self) -> f64 {
        match self {
            ExactNum::Int(i) => *i as f64,
            ExactNum::UInt(u) => *u as f64,
            ExactNum::Float(f) => f.into_inner(),
        }
    }
}

impl From<f64> for ExactNum {
    fn from(f: f64) -> Self {
        ExactNum::Float(OrderedFloat(f))
    }
}

impl From<i64> for ExactNum {
    fn from(i: i64) -> Self {
        ExactNum::Int(i)
    }
}

impl PartialEq for ExactNum {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for ExactNum {}

impl PartialOrd for ExactNum {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ExactNum {
    /// Exact numeric order: integers and floats compared by value, so
    /// `1` and `1.0` are equal; `-0.0` = `0.0`; NaN greatest.
    fn cmp(&self, other: &Self) -> Ordering {
        use ExactNum::*;
        match (self, other) {
            (Int(x), Int(y)) => x.cmp(y),
            (UInt(x), UInt(y)) => x.cmp(y),
            (Int(x), UInt(y)) => (*x as i128).cmp(&(*y as i128)),
            (UInt(x), Int(y)) => (*x as i128).cmp(&(*y as i128)),
            (Float(x), Float(y)) => x.cmp(y),
            (Int(x), Float(y)) => cmp_int_float(*x as i128, y.into_inner()),
            (UInt(x), Float(y)) => cmp_int_float(*x as i128, y.into_inner()),
            (Float(x), Int(y)) => cmp_int_float(*y as i128, x.into_inner()).reverse(),
            (Float(x), UInt(y)) => cmp_int_float(*y as i128, x.into_inner()).reverse(),
        }
    }
}

// Exact numbers must not grow set and vec elements.
const _: () = assert!(std::mem::size_of::<Primitive>() == 16);

impl Primitive {
    /// Convert a `serde_json::Number` to a set element, keeping integers
    /// exact.
    pub fn from_number(n: &Number) -> Self {
        Primitive::Num(ExactNum::from_number(n))
    }

    /// Convert back to a `serde_json::Number` for rendering.
    pub fn to_number(&self) -> Option<Number> {
        match self {
            Primitive::Num(n) => n.to_number(),
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
    /// exact value ([`ExactNum`]), strings by content, symbols by id.
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
        Primitive::Num(n.into())
    }

    fn set(view: &MutatorHeapView, elems: Vec<Primitive>) -> HeapSet {
        HeapSet::from_sorted(view, &sort_dedup(view, elems)).unwrap()
    }

    fn nums(s: &[Primitive]) -> Vec<f64> {
        s.iter()
            .map(|p| match p {
                Primitive::Num(n) => n.as_f64(),
                _ => panic!("not a number"),
            })
            .collect()
    }

    #[test]
    fn exact_num_keeps_integers_above_2_pow_53_distinct() {
        let big = Number::from(9_007_199_254_740_992_u64);
        let big1 = Number::from(9_007_199_254_740_993_u64);
        let a = ExactNum::from_number(&big);
        let b = ExactNum::from_number(&big1);
        assert_eq!(a.cmp(&b), Ordering::Less);
        assert_eq!(b.to_number(), Some(big1));
        let u = ExactNum::from_number(&Number::from(u64::MAX));
        assert!(matches!(u, ExactNum::UInt(u64::MAX)));
        assert_eq!(ExactNum::Int(i64::MAX).cmp(&u), Ordering::Less);
        assert_eq!(u.to_number(), Some(Number::from(u64::MAX)));
    }

    #[test]
    fn exact_num_compares_integers_and_floats_by_value() {
        assert_eq!(ExactNum::Int(1), ExactNum::from(1.0));
        assert_eq!(ExactNum::Int(0), ExactNum::from(-0.0));
        assert!(ExactNum::Int(2) < ExactNum::from(2.5));
        assert!(ExactNum::from(2.5) < ExactNum::Int(3));
        assert!(ExactNum::Int(-3) < ExactNum::from(-2.5));
        // 2^53 + 1 is not equal to the float 2^53 it would round to.
        assert!(ExactNum::Int(9_007_199_254_740_993) > ExactNum::from(9_007_199_254_740_992.0));
        assert!(ExactNum::UInt(u64::MAX) < ExactNum::from(f64::NAN));
        assert!(ExactNum::from(f64::INFINITY) > ExactNum::UInt(u64::MAX));
        assert!(ExactNum::from(f64::NEG_INFINITY) < ExactNum::Int(i64::MIN));
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
        assert!(matches!(p, Primitive::Num(ExactNum::Int(42))));
        assert!(Primitive::Num(7.5.into()).to_number().is_some());
    }
}
