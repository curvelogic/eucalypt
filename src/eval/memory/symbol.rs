//! Symbol interning pool for efficient symbol storage and comparison.
//!
//! Symbols are interned into a per-machine pool so that each unique
//! symbol string is stored once. Symbols are referenced by compact
//! `SymbolId` values, enabling fast integer equality checks instead of
//! string comparison.

use std::collections::HashMap;
use std::fmt;

/// A compact identifier for an interned symbol.
///
/// Wraps a `u32` index into the symbol pool. Two symbols with the
/// same string content always have the same `SymbolId`, so equality
/// comparison is a single integer comparison.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct SymbolId(u32);

impl SymbolId {
    /// Return the raw index for serialisation or diagnostics.
    pub fn as_u32(self) -> u32 {
        self.0
    }
}

impl fmt::Display for SymbolId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "sym#{}", self.0)
    }
}

/// A pool of interned symbol strings.
///
/// Each unique symbol string is stored once. The pool provides
/// bidirectional lookup: string to `SymbolId` and `SymbolId` to
/// string slice.
///
/// The pool is owned by the machine and has the same lifetime.
/// It is not thread-safe — eucalypt execution is single-threaded.
pub struct SymbolPool {
    /// String to ID lookup for interning.
    to_id: HashMap<String, SymbolId>,
    /// ID to string lookup for resolving.
    to_str: Vec<String>,
}

impl SymbolPool {
    /// Create an empty symbol pool.
    pub fn new() -> Self {
        SymbolPool {
            to_id: HashMap::new(),
            to_str: Vec::new(),
        }
    }

    /// Intern a symbol string, returning its `SymbolId`.
    ///
    /// If the string has already been interned, returns the existing
    /// ID. Otherwise, allocates a new ID and stores the string.
    pub fn intern(&mut self, s: &str) -> SymbolId {
        if let Some(&id) = self.to_id.get(s) {
            return id;
        }
        let id = SymbolId(self.to_str.len() as u32);
        self.to_str.push(s.to_string());
        self.to_id.insert(s.to_string(), id);
        id
    }

    /// Resolve a `SymbolId` back to its string.
    ///
    /// # Panics
    ///
    /// Panics if the ID is not valid for this pool.
    pub fn resolve(&self, id: SymbolId) -> &str {
        &self.to_str[id.0 as usize]
    }

    /// Return the number of unique symbols in the pool.
    pub fn len(&self) -> usize {
        self.to_str.len()
    }

    /// Return whether the pool is empty.
    pub fn is_empty(&self) -> bool {
        self.to_str.is_empty()
    }
}

impl Default for SymbolPool {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intern_returns_same_id_for_same_string() {
        let mut pool = SymbolPool::new();
        let id1 = pool.intern("foo");
        let id2 = pool.intern("foo");
        assert_eq!(id1, id2);
    }

    #[test]
    fn intern_returns_different_ids_for_different_strings() {
        let mut pool = SymbolPool::new();
        let id1 = pool.intern("foo");
        let id2 = pool.intern("bar");
        assert_ne!(id1, id2);
    }

    #[test]
    fn resolve_round_trip() {
        let mut pool = SymbolPool::new();
        let id = pool.intern("hello");
        assert_eq!(pool.resolve(id), "hello");
    }

    #[test]
    fn len_tracks_unique_symbols() {
        let mut pool = SymbolPool::new();
        assert_eq!(pool.len(), 0);
        assert!(pool.is_empty());

        pool.intern("a");
        pool.intern("b");
        pool.intern("a"); // duplicate
        assert_eq!(pool.len(), 2);
        assert!(!pool.is_empty());
    }

    #[test]
    fn ids_are_sequential() {
        let mut pool = SymbolPool::new();
        let id0 = pool.intern("x");
        let id1 = pool.intern("y");
        let id2 = pool.intern("z");
        assert_eq!(id0.as_u32(), 0);
        assert_eq!(id1.as_u32(), 1);
        assert_eq!(id2.as_u32(), 2);
    }

    #[test]
    fn symbol_id_display() {
        let mut pool = SymbolPool::new();
        let id = pool.intern("test");
        assert_eq!(format!("{id}"), "sym#0");
    }

    #[test]
    fn symbol_id_is_copy() {
        let mut pool = SymbolPool::new();
        let id = pool.intern("copy_test");
        let id_copy = id;
        // Both should be usable after copy
        assert_eq!(id, id_copy);
        assert_eq!(pool.resolve(id), pool.resolve(id_copy));
    }

    #[test]
    fn symbol_id_ordering() {
        let mut pool = SymbolPool::new();
        let first = pool.intern("first");
        let second = pool.intern("second");
        assert!(first < second);
    }

    #[test]
    fn symbol_id_hash_works() {
        use std::collections::HashSet;
        let mut pool = SymbolPool::new();
        let id1 = pool.intern("a");
        let id2 = pool.intern("b");
        let id1_again = pool.intern("a");

        let mut set = HashSet::new();
        set.insert(id1);
        set.insert(id2);
        set.insert(id1_again);
        assert_eq!(set.len(), 2);
    }
}
