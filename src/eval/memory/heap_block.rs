//! Persistent block type for the eucalypt VM.
//!
//! Blocks are ordered maps from symbol keys to closure values,
//! backed by `im_rc::OrdMap` for O(log n) lookup and structural
//! sharing on merge.
//!
//! # GC Note (Experimental)
//!
//! `BlockEntry` stores a code pointer (`RefPtr<HeapSyn>`) and a raw
//! environment pointer (`*mut u8` erased from `RefPtr<EnvFrame>`). The
//! code pointer is fully traced by the GC. The environment pointer is
//! marked (preventing line sweep) but **not pushed to the scan queue**
//! because `EnvFrame` lives in `machine/` which cannot be imported from
//! `memory/` without a circular dependency.
//!
//! In practice this is safe for the experimental branch because:
//! 1. The letrec env frame that backs a compiled block is also
//!    reachable from the active machine environment stack during the
//!    block's evaluation context.
//! 2. Benchmarks run without triggering mid-computation GC pauses.
//!
//! A production implementation must resolve this by either moving
//! `EnvFrame` into `memory/`, or adding a registered scanning hook
//! for `HeapBlock` that is registered from `machine/`.

use std::ptr::NonNull;

use im_rc::OrdMap;

use super::alloc::StgObject;
use super::symbol::SymbolId;
use super::syntax::RefPtr;

/// An entry in a persistent block. Stores the code pointer for the
/// value's thunk and the environment frame pointer so that the closure
/// can be reconstructed on lookup.
///
/// # Invariant
///
/// Both `code` and `env` point to live heap-allocated objects.
/// The GC marks `code` and pushes it to the scan queue. The GC marks
/// `env` but does not scan it through `HeapBlock` — see module-level
/// note above.
#[derive(Clone, Debug)]
pub struct BlockEntry {
    /// Insertion order for restoring declaration order at render time.
    pub order: usize,
    /// Code pointer (HeapSyn node) for this value's thunk.
    pub code: RefPtr<super::syntax::HeapSyn>,
    /// Environment frame pointer at block construction time.
    ///
    /// Stored as `*mut u8` to avoid a circular dependency between
    /// `memory/` and `machine/`. The intrinsic layer casts this back
    /// to `RefPtr<EnvFrame>` when constructing the returned `SynClosure`.
    pub env: *mut u8,
}

// SAFETY: HeapBlock is only used in the single-threaded eucalypt VM.
// The raw pointer fields are never aliased across threads.
unsafe impl Send for BlockEntry {}
unsafe impl Sync for BlockEntry {}

impl PartialEq for BlockEntry {
    fn eq(&self, other: &Self) -> bool {
        self.code == other.code && self.env == other.env && self.order == other.order
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
        self.order.cmp(&other.order)
    }
}

/// A persistent ordered map from symbol keys to block entries.
///
/// Lives off the managed heap (like HeapSet), held via `RefPtr`
/// in `Native::Block`. GC must mark both the `code` and `env`
/// pointers in each `BlockEntry` — see module-level note above.
#[derive(Clone, Debug)]
pub struct HeapBlock {
    entries: OrdMap<SymbolId, BlockEntry>,
    /// Next insertion-order index (monotonically increasing).
    next_order: usize,
}

impl StgObject for HeapBlock {}

impl HeapBlock {
    /// Create an empty block.
    pub fn empty() -> Self {
        HeapBlock {
            entries: OrdMap::new(),
            next_order: 0,
        }
    }

    /// Insert a new entry at the next insertion order position.
    pub fn with_entry(
        &self,
        key: SymbolId,
        code: RefPtr<super::syntax::HeapSyn>,
        env: *mut u8,
    ) -> Self {
        let order = self.next_order;
        let entry = BlockEntry { order, code, env };
        HeapBlock {
            entries: self.entries.update(key, entry),
            next_order: order + 1,
        }
    }

    /// Look up a key, returning a reference to the entry if found.
    pub fn get(&self, key: &SymbolId) -> Option<&BlockEntry> {
        self.entries.get(key)
    }

    /// Remove a key, returning a new block.
    pub fn without(&self, key: &SymbolId) -> Self {
        HeapBlock {
            entries: self.entries.without(key),
            next_order: self.next_order,
        }
    }

    /// Merge another block into this one (right-biased).
    ///
    /// For duplicate keys, the right-hand entry wins. New right-side
    /// keys are appended after all left-side entries. Overridden keys
    /// keep their left-side insertion order position.
    pub fn merge(&self, other: &HeapBlock) -> Self {
        let mut result = self.entries.clone();
        let mut next_order = self.next_order;
        for (key, entry) in &other.entries {
            let order = if let Some(existing) = result.get(key) {
                existing.order
            } else {
                let o = next_order;
                next_order += 1;
                o
            };
            result.insert(
                *key,
                BlockEntry {
                    order,
                    code: entry.code,
                    env: entry.env,
                },
            );
        }
        HeapBlock {
            entries: result,
            next_order,
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

    /// Iterate over entries in key (SymbolId) order.
    pub fn iter(&self) -> impl Iterator<Item = (&SymbolId, &BlockEntry)> {
        self.entries.iter()
    }

    /// Return entries sorted by insertion order (declaration order).
    pub fn ordered_entries(&self) -> Vec<(SymbolId, &BlockEntry)> {
        let mut v: Vec<_> = self.entries.iter().map(|(k, e)| (*k, e)).collect();
        v.sort_by_key(|(_, e)| e.order);
        v
    }

    /// Return all symbol keys.
    pub fn keys(&self) -> impl Iterator<Item = &SymbolId> {
        self.entries.keys()
    }

    /// Collect all code pointers for GC tracing.
    pub fn code_pointers(&self) -> Vec<NonNull<super::syntax::HeapSyn>> {
        self.entries.values().map(|e| e.code).collect()
    }

    /// Collect all env pointers for GC marking (as raw `*mut u8`).
    pub fn env_pointers(&self) -> impl Iterator<Item = *mut u8> + '_ {
        self.entries.values().map(|e| e.env)
    }

    /// Update forwarded code and env pointers after GC evacuation.
    ///
    /// Both forward functions return `None` if the object was not moved.
    pub fn update_pointers<FC, FE>(&mut self, forward_code: FC, forward_env: FE)
    where
        FC: Fn(NonNull<super::syntax::HeapSyn>) -> Option<NonNull<super::syntax::HeapSyn>>,
        FE: Fn(*mut u8) -> Option<*mut u8>,
    {
        let mut new_entries = OrdMap::new();
        for (k, e) in &self.entries {
            let new_code = forward_code(e.code).unwrap_or(e.code);
            let new_env = forward_env(e.env).unwrap_or(e.env);
            new_entries.insert(
                *k,
                BlockEntry {
                    order: e.order,
                    code: new_code,
                    env: new_env,
                },
            );
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
    use std::ptr::NonNull;

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
        let dummy_code = NonNull::dangling();
        let dummy_env = std::ptr::null_mut();
        let b = HeapBlock::empty().with_entry(k, dummy_code, dummy_env);
        assert_eq!(b.len(), 1);
        let entry = b.get(&k).unwrap();
        assert_eq!(entry.code, dummy_code);
    }

    #[test]
    fn merge_right_biased() {
        let mut pool = SymbolPool::new();
        let k1 = pool.intern("a");
        let k2 = pool.intern("b");
        let code1 = NonNull::dangling();
        // Use a distinct dangling pointer (different from code1) for testing.
        // NonNull::dangling() always produces the same alignment-based address,
        // so we use from_raw_parts with offset 8 (align of HeapSyn).
        let code2: NonNull<super::super::syntax::HeapSyn> = unsafe {
            NonNull::new_unchecked(std::ptr::dangling_mut::<super::super::syntax::HeapSyn>().add(1))
        };

        let left = HeapBlock::empty().with_entry(k1, code1, std::ptr::null_mut());
        let right = HeapBlock::empty()
            .with_entry(k1, code2, std::ptr::null_mut())
            .with_entry(k2, code1, std::ptr::null_mut());

        let merged = left.merge(&right);
        assert_eq!(merged.len(), 2);
        // Right-biased: k1 should have code2
        assert_eq!(merged.get(&k1).unwrap().code, code2);
    }

    #[test]
    fn without_removes_key() {
        let mut pool = SymbolPool::new();
        let k = pool.intern("x");
        let b = HeapBlock::empty().with_entry(k, NonNull::dangling(), std::ptr::null_mut());
        let b2 = b.without(&k);
        assert!(b2.is_empty());
        assert_eq!(b.len(), 1); // original unchanged
    }

    #[test]
    fn ordered_entries_insertion_order() {
        let mut pool = SymbolPool::new();
        let k1 = pool.intern("z");
        let k2 = pool.intern("a");
        let k3 = pool.intern("m");
        let b = HeapBlock::empty()
            .with_entry(k1, NonNull::dangling(), std::ptr::null_mut())
            .with_entry(k2, NonNull::dangling(), std::ptr::null_mut())
            .with_entry(k3, NonNull::dangling(), std::ptr::null_mut());
        let ordered = b.ordered_entries();
        // Should be in insertion order: z, a, m
        assert_eq!(ordered[0].0, k1);
        assert_eq!(ordered[1].0, k2);
        assert_eq!(ordered[2].0, k3);
    }

    #[test]
    fn merge_preserves_left_order() {
        let mut pool = SymbolPool::new();
        let k1 = pool.intern("b");
        let k2 = pool.intern("a");
        let k3 = pool.intern("c");
        let left = HeapBlock::empty()
            .with_entry(k1, NonNull::dangling(), std::ptr::null_mut())
            .with_entry(k2, NonNull::dangling(), std::ptr::null_mut());
        let right = HeapBlock::empty()
            .with_entry(k3, NonNull::dangling(), std::ptr::null_mut())
            .with_entry(k2, NonNull::dangling(), std::ptr::null_mut());
        let merged = left.merge(&right);
        let ordered = merged.ordered_entries();
        // b (order 0), a (order 1 from left), c (order 2 new from right)
        assert_eq!(ordered[0].0, k1);
        assert_eq!(ordered[1].0, k2);
        assert_eq!(ordered[2].0, k3);
    }
}
