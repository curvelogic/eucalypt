//! Native set type for the eucalypt VM.
//!
//! Sets contain only primitive values (numbers, strings, symbols).
//!
//! A set is a header over the dict's persistent ordered tree (eu-oaz72.21,
//! design note §6): the elements are the tree's keys and every value is an
//! inert unit. All storage lives in the GC heap and is traced by the
//! collector (ADR-002). Add, remove and membership are O(log n), size is
//! O(1), and union, intersection and difference are join-based.
//!
//! [`Primitive`] remains the element type of vecs, which store heap arrays
//! of primitives; the array helpers below serve them.

use serde_json::Number;

use super::{
    alloc::{MutatorScope, StgObject},
    array::Array,
    collect::{CollectorHeapView, CollectorScope, GcScannable, OpaqueHeapBytes, ScanPtr},
    dict::{Dict, DictKey, Tree},
    number::ExactNum,
    string::HeapString,
    symbol::SymbolId,
    syntax::{Native, RefPtr},
};
use crate::eval::bytecode::BcValue;
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

// Exact numbers must not grow set and vec elements.
const _: () = assert!(std::mem::size_of::<Primitive>() <= 16);

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

    /// The dict key for this element (sets store elements as keys).
    pub fn to_key(self) -> DictKey {
        match self {
            Primitive::Num(n) => DictKey::Num(n),
            Primitive::Str(p) => DictKey::Str(p),
            Primitive::Sym(id) => DictKey::Sym(id),
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

/// Mark a heap array of primitives: its backing buffer (queued as opaque
/// bytes so it can be evacuated) and, when `has_strings`, every string
/// element. With `has_strings` false the element walk is skipped.
pub(crate) fn scan_primitive_array<'a>(
    elements: &'a Array<Primitive>,
    has_strings: bool,
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
    if has_strings {
        for elem in elements.as_slice() {
            elem.scan(scope, marker, out);
        }
    }
}

/// Rewrite the backing pointer of a heap array of primitives, then, when
/// `has_strings`, the string pointers of its elements (read at the new
/// location).
pub(crate) fn update_primitive_array(
    elements: &mut Array<Primitive>,
    has_strings: bool,
    heap: &CollectorHeapView<'_>,
) {
    if let Some(old_ptr) = elements.allocated_data() {
        if let Some(new_ptr) = heap.forwarded_to(old_ptr) {
            // SAFETY: new_ptr is the evacuated copy of the same backing
            // allocation, holding `len` initialised elements.
            unsafe { elements.set_backing_ptr(new_ptr.cast()) };
        }
    }
    if has_strings {
        for elem in elements.as_mut_slice() {
            elem.update(heap);
        }
    }
}

/// The value every set entry holds: inert (no heap pointer), so a set
/// node's value needs no tracing and no per-element allocation.
pub fn set_unit() -> BcValue {
    BcValue::Native(Native::Num(Number::from(0)))
}

/// A set of primitive values and lists of them: a header over a persistent weight-balanced
/// tree of [`DictNode`](super::dict::DictNode)s whose keys are the
/// elements and whose values are [`set_unit`] (design note §6).
///
/// Every set operation returns a fresh header, so set `=`, which compares
/// headers by identity, behaves as it did before sets moved onto tree
/// nodes. The header is the only part not shared between versions.
pub struct HeapSet {
    root: Tree,
}

impl StgObject for HeapSet {}

impl GcScannable for HeapSet {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        // Push the root so `DictNode::scan` traces the whole tree.
        if let Some(p) = self.root {
            if marker.mark(p) {
                out.push(ScanPtr::from_non_null(scope, p));
            }
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        if let Some(p) = &mut self.root {
            if let Some(new_ptr) = heap.forwarded_to(*p) {
                *p = new_ptr;
            }
        }
    }
}

impl HeapSet {
    /// A set over `root`, whose values must all be [`set_unit`].
    pub fn new(root: Tree) -> Self {
        HeapSet { root }
    }

    /// Build a set from elements in any order, with duplicates (of equal
    /// elements the integer form is kept, see `Dict::canonical_key`).
    pub fn from_elements(d: &Dict<'_>, elems: Vec<Primitive>) -> Result<Self, ExecutionError> {
        Self::from_keys(d, elems.into_iter().map(Primitive::to_key).collect())
    }

    /// Build a set from element keys in any order, with duplicates (of
    /// equal elements the integer form is kept). Keys may be lists (design
    /// note OPEN 8).
    pub fn from_keys(d: &Dict<'_>, keys: Vec<DictKey>) -> Result<Self, ExecutionError> {
        let entries = keys.into_iter().map(|k| (k, set_unit())).collect();
        let sorted = d.sort_last_wins(entries);
        Ok(HeapSet::new(d.build_sorted(&sorted)?))
    }

    /// The root of the element tree.
    pub fn root(&self) -> Tree {
        self.root
    }

    /// The elements, in sorted order.
    pub fn elements(&self, d: &Dict<'_>) -> Vec<DictKey> {
        let mut out = Vec::with_capacity(self.len());
        d.for_each(self.root, |n| out.push(n.key()));
        out
    }

    /// Return the number of elements.
    pub fn len(&self) -> usize {
        self.root.map_or(0, |p| {
            // SAFETY: a set root points at a live, immutable node.
            unsafe { p.as_ref() }.size()
        })
    }

    /// Return whether the set is empty.
    pub fn is_empty(&self) -> bool {
        self.root.is_none()
    }
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

    fn nums(keys: &[DictKey]) -> Vec<f64> {
        keys.iter()
            .map(|k| match k {
                DictKey::Num(n) => n.as_f64(),
                _ => panic!("not a number"),
            })
            .collect()
    }

    #[test]
    fn from_elements_orders_and_deduplicates() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let mut pool = SymbolPool::new();
        // Interned first, so `z` has the lower id: symbols order by name.
        let z = pool.intern("z");
        let y = pool.intern("y");
        let b = view.str("b").unwrap().as_ptr();
        let a = view.str("a").unwrap().as_ptr();
        let a2 = view.str("a").unwrap().as_ptr();
        let d = Dict::new(view, &pool);
        let s = HeapSet::from_elements(
            &d,
            vec![
                Primitive::Sym(z),
                Primitive::Str(b),
                num(3.0),
                Primitive::Str(a),
                num(1.0),
                Primitive::Sym(y),
                Primitive::Str(a2),
                num(1.0),
            ],
        )
        .unwrap();
        let e = s.elements(&d);
        assert_eq!(s.len(), 6);
        assert_eq!(nums(&e[..2]), vec![1.0, 3.0]);
        assert_eq!(e[2].str_in(&view), Some("a"));
        assert_eq!(e[3].str_in(&view), Some("b"));
        assert!(matches!(e[4], DictKey::Sym(id) if id == y));
        assert!(matches!(e[5], DictKey::Sym(id) if id == z));
        d.check_invariants(s.root()).unwrap();
    }

    #[test]
    fn empty_set() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let pool = SymbolPool::new();
        let d = Dict::new(view, &pool);
        let s = HeapSet::from_elements(&d, vec![]).unwrap();
        assert!(s.is_empty());
        assert_eq!(s.len(), 0);
        assert!(s.elements(&d).is_empty());
    }

    #[test]
    fn primitive_number_round_trip() {
        let p = Primitive::from_number(&Number::from(42));
        assert!(matches!(p, Primitive::Num(ExactNum::Int(42))));
        assert!(Primitive::Num(7.5.into()).to_number().is_some());
        assert!(matches!(p.to_key(), DictKey::Num(ExactNum::Int(42))));
    }
}
