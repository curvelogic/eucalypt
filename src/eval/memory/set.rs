//! Native set type for the eucalypt VM.
//!
//! Sets contain only primitive values (numbers, strings, symbols) and
//! provide O(1) membership, insertion, and removal via `HashSet`.

use std::collections::HashSet;
use std::hash::{Hash, Hasher};

use ordered_float::OrderedFloat;
use serde_json::Number;

use super::alloc::StgObject;
use super::symbol::SymbolId;

/// A hashable primitive value that can be stored in a set.
///
/// Sets are restricted to primitives — no nested blocks, lists, or
/// lambdas.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Primitive {
    Num(OrderedFloat<f64>),
    Str(String),
    Sym(SymbolId),
}

impl Hash for Primitive {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Primitive::Num(n) => n.hash(state),
            Primitive::Str(s) => s.hash(state),
            Primitive::Sym(id) => id.hash(state),
        }
    }
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
}

impl PartialOrd for Primitive {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Primitive {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering;
        match (self, other) {
            (Primitive::Num(a), Primitive::Num(b)) => a.cmp(b),
            (Primitive::Str(a), Primitive::Str(b)) => a.cmp(b),
            (Primitive::Sym(a), Primitive::Sym(b)) => a.cmp(b),
            // Numbers before strings before symbols
            (Primitive::Num(_), _) => Ordering::Less,
            (_, Primitive::Num(_)) => Ordering::Greater,
            (Primitive::Str(_), Primitive::Sym(_)) => Ordering::Less,
            (Primitive::Sym(_), Primitive::Str(_)) => Ordering::Greater,
        }
    }
}

/// A set of primitive values, stored on the heap.
///
/// GC does not trace into set contents because primitives contain no
/// heap references (strings are owned, symbols are interned IDs).
#[derive(Debug)]
pub struct HeapSet {
    elements: HashSet<Primitive>,
}

impl StgObject for HeapSet {}

impl HeapSet {
    /// Create an empty set.
    pub fn empty() -> Self {
        HeapSet {
            elements: HashSet::new(),
        }
    }

    /// Create a set from an iterator of primitives.
    pub fn from_primitives(iter: impl Iterator<Item = Primitive>) -> Self {
        HeapSet {
            elements: iter.collect(),
        }
    }

    /// Add an element to the set, returning a new set.
    pub fn with_added(&self, elem: Primitive) -> Self {
        let mut new = self.elements.clone();
        new.insert(elem);
        HeapSet { elements: new }
    }

    /// Remove an element from the set, returning a new set.
    pub fn with_removed(&self, elem: &Primitive) -> Self {
        let mut new = self.elements.clone();
        new.remove(elem);
        HeapSet { elements: new }
    }

    /// Check if the set contains an element.
    pub fn contains(&self, elem: &Primitive) -> bool {
        self.elements.contains(elem)
    }

    /// Return the number of elements.
    pub fn len(&self) -> usize {
        self.elements.len()
    }

    /// Return whether the set is empty.
    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }

    /// Return the union of two sets.
    pub fn union(&self, other: &HeapSet) -> Self {
        HeapSet {
            elements: self.elements.union(&other.elements).cloned().collect(),
        }
    }

    /// Return the intersection of two sets.
    pub fn intersect(&self, other: &HeapSet) -> Self {
        HeapSet {
            elements: self
                .elements
                .intersection(&other.elements)
                .cloned()
                .collect(),
        }
    }

    /// Return the difference (self - other).
    pub fn diff(&self, other: &HeapSet) -> Self {
        HeapSet {
            elements: self.elements.difference(&other.elements).cloned().collect(),
        }
    }

    /// Return elements sorted for deterministic rendering.
    pub fn sorted_elements(&self) -> Vec<&Primitive> {
        let mut elems: Vec<_> = self.elements.iter().collect();
        elems.sort();
        elems
    }
}

impl Clone for HeapSet {
    fn clone(&self) -> Self {
        HeapSet {
            elements: self.elements.clone(),
        }
    }
}

impl PartialEq for HeapSet {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

impl Eq for HeapSet {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_set_has_no_elements() {
        let s = HeapSet::empty();
        assert!(s.is_empty());
        assert_eq!(s.len(), 0);
    }

    #[test]
    fn from_primitives_deduplicates() {
        let s = HeapSet::from_primitives(
            vec![
                Primitive::Num(OrderedFloat(1.0)),
                Primitive::Num(OrderedFloat(1.0)),
                Primitive::Num(OrderedFloat(2.0)),
            ]
            .into_iter(),
        );
        assert_eq!(s.len(), 2);
    }

    #[test]
    fn with_added_and_contains() {
        let s = HeapSet::empty();
        let elem = Primitive::Str("hello".to_string());
        let s2 = s.with_added(elem.clone());
        assert!(s2.contains(&elem));
        assert!(!s.contains(&elem));
    }

    #[test]
    fn with_removed() {
        let elem = Primitive::Num(OrderedFloat(42.0));
        let s = HeapSet::empty().with_added(elem.clone());
        let s2 = s.with_removed(&elem);
        assert!(!s2.contains(&elem));
        assert!(s2.is_empty());
    }

    #[test]
    fn union_combines_sets() {
        let a = HeapSet::from_primitives(
            vec![
                Primitive::Num(OrderedFloat(1.0)),
                Primitive::Num(OrderedFloat(2.0)),
            ]
            .into_iter(),
        );
        let b = HeapSet::from_primitives(
            vec![
                Primitive::Num(OrderedFloat(2.0)),
                Primitive::Num(OrderedFloat(3.0)),
            ]
            .into_iter(),
        );
        let u = a.union(&b);
        assert_eq!(u.len(), 3);
    }

    #[test]
    fn intersect_finds_common() {
        let a = HeapSet::from_primitives(
            vec![
                Primitive::Num(OrderedFloat(1.0)),
                Primitive::Num(OrderedFloat(2.0)),
            ]
            .into_iter(),
        );
        let b = HeapSet::from_primitives(
            vec![
                Primitive::Num(OrderedFloat(2.0)),
                Primitive::Num(OrderedFloat(3.0)),
            ]
            .into_iter(),
        );
        let i = a.intersect(&b);
        assert_eq!(i.len(), 1);
        assert!(i.contains(&Primitive::Num(OrderedFloat(2.0))));
    }

    #[test]
    fn diff_removes_other() {
        let a = HeapSet::from_primitives(
            vec![
                Primitive::Num(OrderedFloat(1.0)),
                Primitive::Num(OrderedFloat(2.0)),
            ]
            .into_iter(),
        );
        let b = HeapSet::from_primitives(vec![Primitive::Num(OrderedFloat(2.0))].into_iter());
        let d = a.diff(&b);
        assert_eq!(d.len(), 1);
        assert!(d.contains(&Primitive::Num(OrderedFloat(1.0))));
    }

    #[test]
    fn sorted_elements_deterministic_order() {
        use super::super::symbol::SymbolPool;
        let mut pool = SymbolPool::new();
        let sym_id = pool.intern("z");
        let s = HeapSet::from_primitives(
            vec![
                Primitive::Sym(sym_id),
                Primitive::Str("b".to_string()),
                Primitive::Num(OrderedFloat(3.0)),
                Primitive::Str("a".to_string()),
                Primitive::Num(OrderedFloat(1.0)),
            ]
            .into_iter(),
        );
        let sorted = s.sorted_elements();
        // Numbers first, then strings, then symbols
        assert!(matches!(sorted[0], Primitive::Num(_)));
        assert!(matches!(sorted[1], Primitive::Num(_)));
        assert!(matches!(sorted[2], Primitive::Str(_)));
        assert!(matches!(sorted[3], Primitive::Str(_)));
        assert!(matches!(sorted[4], Primitive::Sym(_)));
    }

    #[test]
    fn primitive_from_number() {
        let n = Number::from(42);
        let p = Primitive::from_number(&n);
        assert_eq!(p, Primitive::Num(OrderedFloat(42.0)));
    }

    #[test]
    fn primitive_to_number_round_trip() {
        let p = Primitive::Num(OrderedFloat(7.5));
        let n = p.to_number();
        assert!(n.is_some());
    }

    #[test]
    fn equality() {
        let a = HeapSet::from_primitives(
            vec![
                Primitive::Num(OrderedFloat(1.0)),
                Primitive::Num(OrderedFloat(2.0)),
            ]
            .into_iter(),
        );
        let b = HeapSet::from_primitives(
            vec![
                Primitive::Num(OrderedFloat(2.0)),
                Primitive::Num(OrderedFloat(1.0)),
            ]
            .into_iter(),
        );
        assert_eq!(a, b);
    }
}
