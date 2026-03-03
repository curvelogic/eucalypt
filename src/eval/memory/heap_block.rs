//! Persistent block type for the eucalypt VM.
//!
//! Blocks are ordered maps from symbol keys to closure values,
//! backed by `im::OrdMap` for O(log n) lookup and structural
//! sharing on merge.

use std::ptr::NonNull;

use im_rc::OrdMap;

use super::alloc::StgObject;
use super::symbol::SymbolId;
use super::syntax::RefPtr;

/// An entry in a persistent block: the value is a pointer to a
/// HeapSyn closure on the managed heap.
///
/// The pointer targets a HeapSyn node that forms the root of a
/// closure (paired with an environment at lookup time).
#[derive(Clone, Debug)]
pub struct BlockEntry {
    /// Pointer to the HeapSyn code for this value
    pub code: RefPtr<super::syntax::HeapSyn>,
}

impl PartialEq for BlockEntry {
    fn eq(&self, other: &Self) -> bool {
        self.code == other.code
    }
}

impl Eq for BlockEntry {}

impl PartialOrd for BlockEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BlockEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.code.as_ptr() as usize).cmp(&(other.code.as_ptr() as usize))
    }
}

/// A persistent ordered map from symbol keys to block entries.
///
/// Lives off the managed heap (like HeapSet), held via `RefPtr`
/// in `Native::Block`. GC must trace through the `code` pointers
/// in each `BlockEntry` to keep referenced closures alive.
#[derive(Clone, Debug)]
pub struct HeapBlock {
    entries: OrdMap<SymbolId, BlockEntry>,
}

impl StgObject for HeapBlock {}

impl HeapBlock {
    /// Create an empty block.
    pub fn empty() -> Self {
        HeapBlock {
            entries: OrdMap::new(),
        }
    }

    /// Create a block from an iterator of key-value pairs.
    pub fn from_pairs(iter: impl Iterator<Item = (SymbolId, BlockEntry)>) -> Self {
        HeapBlock {
            entries: iter.collect(),
        }
    }

    /// Look up a key, returning a reference to the entry if found.
    pub fn get(&self, key: &SymbolId) -> Option<&BlockEntry> {
        self.entries.get(key)
    }

    /// Insert or update a key-value pair, returning a new block.
    pub fn with_insert(&self, key: SymbolId, entry: BlockEntry) -> Self {
        HeapBlock {
            entries: self.entries.update(key, entry),
        }
    }

    /// Remove a key, returning a new block.
    pub fn without(&self, key: &SymbolId) -> Self {
        HeapBlock {
            entries: self.entries.without(key),
        }
    }

    /// Merge another block into this one (right-biased: entries from
    /// `other` override entries in `self`).
    pub fn merge(&self, other: &HeapBlock) -> Self {
        HeapBlock {
            entries: self.entries.clone().union(other.entries.clone()),
        }
    }

    /// Return the number of entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Return whether the block is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Iterate over entries in key order.
    pub fn iter(&self) -> impl Iterator<Item = (&SymbolId, &BlockEntry)> {
        self.entries.iter()
    }

    /// Iterate over keys in order.
    pub fn keys(&self) -> impl Iterator<Item = &SymbolId> {
        self.entries.keys()
    }

    /// Iterate over values in order.
    pub fn values(&self) -> impl Iterator<Item = &BlockEntry> {
        self.entries.values()
    }

    /// Collect all HeapSyn pointers for GC tracing.
    pub fn heap_pointers(&self) -> Vec<NonNull<super::syntax::HeapSyn>> {
        self.entries.values().map(|e| e.code).collect()
    }

    /// Update forwarded pointers after GC evacuation.
    pub fn update_pointers<F>(&mut self, forward: F)
    where
        F: Fn(NonNull<super::syntax::HeapSyn>) -> Option<NonNull<super::syntax::HeapSyn>>,
    {
        let mut new_entries = OrdMap::new();
        for (k, v) in &self.entries {
            let new_code = forward(v.code).unwrap_or(v.code);
            new_entries.insert(*k, BlockEntry { code: new_code });
        }
        self.entries = new_entries;
    }
}

impl PartialEq for HeapBlock {
    fn eq(&self, other: &Self) -> bool {
        self.entries.len() == other.entries.len()
            && self
                .entries
                .iter()
                .zip(other.entries.iter())
                .all(|((k1, v1), (k2, v2))| k1 == k2 && v1 == v2)
    }
}

impl Eq for HeapBlock {}

#[cfg(test)]
mod tests {
    use super::super::symbol::SymbolPool;
    use super::*;

    #[test]
    fn empty_block() {
        let b = HeapBlock::empty();
        assert!(b.is_empty());
        assert_eq!(b.len(), 0);
    }

    #[test]
    fn insert_and_lookup() {
        let mut pool = SymbolPool::new();
        let k = pool.intern("x");
        // Use a dummy pointer for testing
        let dummy = NonNull::dangling();
        let entry = BlockEntry { code: dummy };
        let b = HeapBlock::empty().with_insert(k, entry.clone());
        assert_eq!(b.len(), 1);
        assert_eq!(b.get(&k), Some(&entry));
    }

    #[test]
    fn merge_right_biased() {
        let mut pool = SymbolPool::new();
        let k1 = pool.intern("a");
        let k2 = pool.intern("b");
        let dummy1 = NonNull::dangling();
        let dummy2 = NonNull::dangling();
        let e1 = BlockEntry { code: dummy1 };
        let e2 = BlockEntry { code: dummy2 };

        let left = HeapBlock::empty().with_insert(k1, e1.clone());
        let right = HeapBlock::empty()
            .with_insert(k1, e2.clone())
            .with_insert(k2, e1.clone());

        let merged = left.merge(&right);
        assert_eq!(merged.len(), 2);
        // Right-biased: k1 should have e2
        assert_eq!(merged.get(&k1), Some(&e2));
    }

    #[test]
    fn without_removes_key() {
        let mut pool = SymbolPool::new();
        let k = pool.intern("x");
        let dummy = NonNull::dangling();
        let entry = BlockEntry { code: dummy };
        let b = HeapBlock::empty().with_insert(k, entry);
        let b2 = b.without(&k);
        assert!(b2.is_empty());
        assert_eq!(b.len(), 1); // original unchanged
    }
}
