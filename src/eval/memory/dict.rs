//! Persistent ordered dictionary nodes for the eucalypt VM (eu-oaz72.21).
//!
//! A dict is a weight-balanced binary search tree (Adams' trees, the
//! structure behind Haskell's `Data.Map`) of immutable [`DictNode`] heap
//! objects. Every node, key string and key list lives in the GC heap and is
//! traced by the collector; no node owns Rust-heap storage (ADR-002), so
//! nothing leaks when a node is swept. The empty tree is `None`.
//!
//! Nodes are never mutated after construction: every update path-copies
//! from the root and shares untouched subtrees, so there is no write
//! barrier question.
//!
//! Keys are numbers (integers kept exact), strings, symbols, and lists of
//! those, under one total order (numbers < strings < symbols < lists; see
//! [`Dict::cmp_keys`]). Values are arbitrary [`BcValue`]s, stored as given;
//! laziness and memoisation are the intrinsic layer's concern.
//!
//! Balance parameters are (Δ, Γ) = (3, 2) (Hirai and Yamamoto, "Balancing
//! weight-balanced trees", JFP 2011). Split and join follow the join-based
//! formulation (Blelloch, Ferizovic and Sun, "Just join for parallel ordered
//! sets", SPAA 2016), in `Data.Map`'s `link`/`merge` form.
//!
//! See `docs/superpowers/plans/2026-11-12-dict-design.md`.

use std::cmp::Ordering;
use std::mem::{align_of, size_of};
use std::ptr::NonNull;

use ordered_float::OrderedFloat;
use serde_json::Number;

use super::{
    alloc::{MutatorScope, ScopedAllocator, StgObject},
    collect::{CollectorHeapView, CollectorScope, GcScannable, ScanPtr},
    mutator::MutatorHeapView,
    string::HeapString,
    symbol::{SymbolId, SymbolPool},
    syntax::RefPtr,
};
use crate::eval::bytecode::BcValue;
use crate::eval::error::ExecutionError;

/// Weight-balance factor Δ: a subtree may be at most Δ times the size of
/// its sibling.
const DELTA: u64 = 3;

/// Rotation selector Γ: choose a single rotation when the inner grandchild
/// is less than Γ times the outer one, otherwise a double rotation.
const RATIO: u64 = 2;

/// A dictionary key: a deep-forced primitive or list of primitives.
///
/// `Copy` and free of Rust-owned storage: a string key points at an
/// immutable `HeapString` and a list key at an immutable [`DictKeyList`],
/// both in the GC heap and traced through the node that holds the key.
#[derive(Clone, Copy, Debug)]
pub enum DictKey {
    /// An integer in `i64` range.
    Int(i64),
    /// An integer above `i64::MAX` (never used for values in `i64` range).
    UInt(u64),
    /// A non-integer number (or one stored as a float), ordered totally.
    Float(OrderedFloat<f64>),
    /// A string key.
    Str(RefPtr<HeapString>),
    /// A symbol key.
    Sym(SymbolId),
    /// A list key.
    List(RefPtr<DictKeyList>),
}

impl DictKey {
    /// Convert a number to a key, keeping integers exact.
    pub fn from_number(n: &Number) -> Self {
        if let Some(i) = n.as_i64() {
            DictKey::Int(i)
        } else if let Some(u) = n.as_u64() {
            DictKey::UInt(u)
        } else {
            DictKey::Float(OrderedFloat(n.as_f64().unwrap_or(f64::NAN)))
        }
    }

    /// Convert a number key back to a number (`None` for other keys or a
    /// non-finite float, which a `Number` cannot hold).
    pub fn to_number(&self) -> Option<Number> {
        match self {
            DictKey::Int(i) => Some(Number::from(*i)),
            DictKey::UInt(u) => Some(Number::from(*u)),
            DictKey::Float(f) => Number::from_f64(f.into_inner()),
            _ => None,
        }
    }

    /// Rank of the key's type in the cross-type order.
    fn type_rank(&self) -> u8 {
        match self {
            DictKey::Int(_) | DictKey::UInt(_) | DictKey::Float(_) => 0,
            DictKey::Str(_) => 1,
            DictKey::Sym(_) => 2,
            DictKey::List(_) => 3,
        }
    }

    /// The string content of a string key.
    ///
    /// The guard ties the result to a mutator scope, during which the
    /// collector cannot run and move or reclaim the string.
    pub fn str_in<'guard>(&self, _guard: &'guard dyn MutatorScope) -> Option<&'guard str> {
        match self {
            // SAFETY: the pointer refers to a live, immutable HeapString
            // reachable from a live node or argument; no collection can
            // run while the mutator scope `'guard` is held.
            DictKey::Str(p) => Some(unsafe { &*p.as_ptr() }.as_str()),
            _ => None,
        }
    }

    /// The elements of a list key.
    pub fn list_in<'guard>(&self, _guard: &'guard dyn MutatorScope) -> Option<&'guard [DictKey]> {
        match self {
            // SAFETY: as `str_in`: a live, immutable key list.
            DictKey::List(p) => Some(unsafe { &*p.as_ptr() }.elements()),
            _ => None,
        }
    }

    /// Mark the key's heap object (string or key list) and queue it for
    /// scanning.
    pub(crate) fn scan<'a>(
        &self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        match self {
            // Push so HeapString::scan marks the backing bytes.
            DictKey::Str(p) if marker.mark(*p) => {
                out.push(ScanPtr::from_non_null(scope, *p));
            }
            // A key list is variable-size: mark every line its allocation
            // spans (the header records its length).
            DictKey::List(p) if marker.mark_raw_bytes(p.cast()) => {
                out.push(ScanPtr::from_non_null(scope, *p));
            }
            _ => {}
        }
    }

    /// Rewrite the key's heap pointer if its object moved.
    pub(crate) fn update(&mut self, heap: &CollectorHeapView<'_>) {
        match self {
            DictKey::Str(p) => {
                if let Some(new_ptr) = heap.forwarded_to(*p) {
                    *p = new_ptr;
                }
            }
            DictKey::List(p) => {
                if let Some(new_ptr) = heap.forwarded_to(*p) {
                    *p = new_ptr;
                }
            }
            _ => {}
        }
    }
}

/// Exact comparison of an integer (within ±2^64) with a float. NaN is
/// greater than every integer.
fn cmp_int_float(i: i128, f: f64) -> Ordering {
    const TWO_64: f64 = 18_446_744_073_709_551_616.0;
    if f.is_nan() || f >= TWO_64 {
        return Ordering::Less;
    }
    if f < -TWO_64 {
        return Ordering::Greater;
    }
    let t = f.trunc();
    // Exact: |t| < 2^64, and every float of that magnitude with no
    // fractional part is an integer representable in i128.
    let ti = t as i128;
    match i.cmp(&ti) {
        Ordering::Equal => {
            if f > t {
                Ordering::Less
            } else if f < t {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        }
        o => o,
    }
}

// Allocations are 16-byte aligned, enough for the header and elements.
const _: () = assert!(align_of::<DictKeyList>() <= 16 && align_of::<DictKey>() <= 16);

/// Immutable list key: a length followed inline by that many [`DictKey`]s,
/// in one variable-size heap allocation (no separate backing buffer).
#[repr(C)]
pub struct DictKeyList {
    len: u64,
    // [DictKey; len] follows
}

impl StgObject for DictKeyList {}

impl DictKeyList {
    /// Byte offset of the first element from the start of the object.
    const ELEMENTS_OFFSET: usize = {
        let header = size_of::<DictKeyList>();
        let align = align_of::<DictKey>();
        header.div_ceil(align) * align
    };

    /// Allocate a key list holding `elements`.
    pub fn alloc<'guard, A: ScopedAllocator<'guard>>(
        mem: &A,
        elements: &[DictKey],
    ) -> Result<RefPtr<DictKeyList>, ExecutionError> {
        let bytes = Self::ELEMENTS_OFFSET + std::mem::size_of_val(elements);
        let raw = mem.alloc_bytes_uninit(bytes)?;
        // The allocator aligns every allocation to 16 bytes, which covers
        // the alignment of both the header and the elements.
        let list = raw.cast::<DictKeyList>();
        // SAFETY: `raw` is a fresh, exclusively owned allocation of
        // `bytes` bytes, 16-byte aligned: room for the header followed
        // (at `ELEMENTS_OFFSET`) by `elements.len()` keys. Every byte read
        // later (the header and `len` keys) is written here.
        unsafe {
            std::ptr::write(
                list.as_ptr(),
                DictKeyList {
                    len: elements.len() as u64,
                },
            );
            let dst = raw.as_ptr().add(Self::ELEMENTS_OFFSET) as *mut DictKey;
            std::ptr::copy_nonoverlapping(elements.as_ptr(), dst, elements.len());
        }
        Ok(list)
    }

    /// Number of elements.
    pub fn len(&self) -> usize {
        self.len as usize
    }

    /// Whether the list is empty.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn elements_ptr(&self) -> *const DictKey {
        // SAFETY: the elements start `ELEMENTS_OFFSET` bytes into this
        // object's allocation (see `alloc`).
        unsafe { (self as *const Self as *const u8).add(Self::ELEMENTS_OFFSET) as *const DictKey }
    }

    /// The elements, in order.
    pub fn elements(&self) -> &[DictKey] {
        // SAFETY: `alloc` initialised `len` keys at `elements_ptr`, and the
        // allocation lives at least as long as `self`.
        unsafe { std::slice::from_raw_parts(self.elements_ptr(), self.len()) }
    }

    fn elements_mut(&mut self) -> &mut [DictKey] {
        let len = self.len();
        // SAFETY: as `elements`; `&mut self` gives exclusive access (the
        // collector's update phase).
        unsafe { std::slice::from_raw_parts_mut(self.elements_ptr() as *mut DictKey, len) }
    }
}

impl GcScannable for DictKeyList {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        for key in self.elements() {
            key.scan(scope, marker, out);
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        for key in self.elements_mut() {
            key.update(heap);
        }
    }
}

/// A weight-balanced tree node. The empty tree is a null child pointer.
#[repr(C)]
pub struct DictNode {
    /// Number of entries in this subtree (the balance invariant's weight).
    size: u64,
    left: Option<RefPtr<DictNode>>,
    right: Option<RefPtr<DictNode>>,
    key: DictKey,
    value: BcValue,
}

impl StgObject for DictNode {}

impl DictNode {
    /// Number of entries in this subtree.
    pub fn size(&self) -> usize {
        self.size as usize
    }

    /// Left subtree (keys less than this node's key).
    pub fn left(&self) -> Tree {
        self.left
    }

    /// Right subtree (keys greater than this node's key).
    pub fn right(&self) -> Tree {
        self.right
    }

    /// This node's key.
    pub fn key(&self) -> DictKey {
        self.key
    }

    /// This node's value, as stored.
    pub fn value(&self) -> &BcValue {
        &self.value
    }
}

impl GcScannable for DictNode {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        for child in [self.left, self.right].into_iter().flatten() {
            if marker.mark(child) {
                out.push(ScanPtr::from_non_null(scope, child));
            }
        }
        self.key.scan(scope, marker, out);
        self.value.scan(scope, marker, out);
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        for p in [&mut self.left, &mut self.right].into_iter().flatten() {
            if let Some(new_ptr) = heap.forwarded_to(*p) {
                *p = new_ptr;
            }
        }
        self.key.update(heap);
        self.value.scan_and_update(heap);
    }
}

/// A (possibly empty) tree.
pub type Tree = Option<RefPtr<DictNode>>;

/// The result of [`Dict::split`]: the entries below a key, the node
/// holding it (if any) and the entries above it.
pub type Split = (Tree, Option<RefPtr<DictNode>>, Tree);

/// An entry: a key and its value.
pub type Entry = (DictKey, BcValue);

/// Tree operations over a mutator view and the symbol pool (symbol keys
/// order by name).
///
/// Collection runs only at the machine's safepoints, never inside an
/// allocation call, so an operation may hold raw node pointers across its
/// own allocations.
#[derive(Clone, Copy)]
pub struct Dict<'a> {
    view: MutatorHeapView<'a>,
    symbols: &'a SymbolPool,
}

impl<'a> Dict<'a> {
    /// Tree operations allocating through `view`.
    pub fn new(view: MutatorHeapView<'a>, symbols: &'a SymbolPool) -> Self {
        Dict { view, symbols }
    }

    /// Borrow a node.
    pub fn node(&self, p: RefPtr<DictNode>) -> &'a DictNode {
        // SAFETY: tree pointers refer to live, immutable nodes reachable
        // from a live dict or argument; no collection runs while the
        // mutator view is held.
        unsafe { &*p.as_ptr() }
    }

    /// Number of entries in a tree.
    pub fn size(&self, t: Tree) -> u64 {
        t.map_or(0, |p| self.node(p).size)
    }

    /// The total key order: numbers (exactly, integers and floats compared
    /// by value, `-0.0` = `0.0`, NaN greatest) < strings (by UTF-8 bytes) <
    /// symbols (by name) < lists (lexicographically, a prefix first).
    pub fn cmp_keys(&self, a: &DictKey, b: &DictKey) -> Ordering {
        use DictKey::*;
        match (a, b) {
            (Int(x), Int(y)) => x.cmp(y),
            (UInt(x), UInt(y)) => x.cmp(y),
            (Int(x), UInt(y)) => (*x as i128).cmp(&(*y as i128)),
            (UInt(x), Int(y)) => (*x as i128).cmp(&(*y as i128)),
            (Float(x), Float(y)) => x.cmp(y),
            (Int(x), Float(y)) => cmp_int_float(*x as i128, y.into_inner()),
            (UInt(x), Float(y)) => cmp_int_float(*x as i128, y.into_inner()),
            (Float(x), Int(y)) => cmp_int_float(*y as i128, x.into_inner()).reverse(),
            (Float(x), UInt(y)) => cmp_int_float(*y as i128, x.into_inner()).reverse(),
            (Str(x), Str(y)) => {
                if x == y {
                    Ordering::Equal
                } else {
                    a.str_in(&self.view)
                        .unwrap_or_default()
                        .as_bytes()
                        .cmp(b.str_in(&self.view).unwrap_or_default().as_bytes())
                }
            }
            (Sym(x), Sym(y)) => {
                if x == y {
                    Ordering::Equal
                } else {
                    self.symbols.resolve(*x).cmp(self.symbols.resolve(*y))
                }
            }
            (List(x), List(y)) => {
                if x == y {
                    return Ordering::Equal;
                }
                let xs = a.list_in(&self.view).unwrap_or_default();
                let ys = b.list_in(&self.view).unwrap_or_default();
                for (p, q) in xs.iter().zip(ys) {
                    match self.cmp_keys(p, q) {
                        Ordering::Equal => {}
                        o => return o,
                    }
                }
                xs.len().cmp(&ys.len())
            }
            _ => a.type_rank().cmp(&b.type_rank()),
        }
    }

    /// Allocate a node over `l` and `r` (no rebalancing).
    fn bin(&self, key: DictKey, value: BcValue, l: Tree, r: Tree) -> Result<Tree, ExecutionError> {
        let size = self.size(l) + self.size(r) + 1;
        let node = self.view.alloc(DictNode {
            size,
            left: l,
            right: r,
            key,
            value,
        })?;
        Ok(Some(node.as_ptr()))
    }

    /// Build a node over `l` and `r`, restoring balance with a single or
    /// double rotation when one side has grown or shrunk by at most one
    /// entry (or, in `link`/`merge`, by the amounts those recursions
    /// produce).
    fn balance(
        &self,
        key: DictKey,
        value: BcValue,
        l: Tree,
        r: Tree,
    ) -> Result<Tree, ExecutionError> {
        let (sl, sr) = (self.size(l), self.size(r));
        if sl + sr <= 1 {
            self.bin(key, value, l, r)
        } else if sr > DELTA * sl {
            self.rotate_left(key, value, l, r)
        } else if sl > DELTA * sr {
            self.rotate_right(key, value, l, r)
        } else {
            self.bin(key, value, l, r)
        }
    }

    fn rotate_left(
        &self,
        key: DictKey,
        value: BcValue,
        l: Tree,
        r: Tree,
    ) -> Result<Tree, ExecutionError> {
        // `r` is heavy, so non-empty; if it is ever not, fall back to a
        // plain node (correct, only unbalanced) rather than panic.
        let Some(rp) = r else {
            return self.bin(key, value, l, r);
        };
        let rn = self.node(rp);
        if self.size(rn.left) < RATIO * self.size(rn.right) {
            // Single: (l k (rl rk rr)) => ((l k rl) rk rr)
            let inner = self.bin(key, value, l, rn.left)?;
            self.bin(rn.key, rn.value.clone(), inner, rn.right)
        } else {
            // Double: (l k ((rll rlk rlr) rk rr)) => ((l k rll) rlk (rlr rk rr))
            let Some(rlp) = rn.left else {
                return self.bin(key, value, l, r);
            };
            let rl = self.node(rlp);
            let a = self.bin(key, value, l, rl.left)?;
            let b = self.bin(rn.key, rn.value.clone(), rl.right, rn.right)?;
            self.bin(rl.key, rl.value.clone(), a, b)
        }
    }

    fn rotate_right(
        &self,
        key: DictKey,
        value: BcValue,
        l: Tree,
        r: Tree,
    ) -> Result<Tree, ExecutionError> {
        let Some(lp) = l else {
            return self.bin(key, value, l, r);
        };
        let ln = self.node(lp);
        if self.size(ln.right) < RATIO * self.size(ln.left) {
            // Single: ((ll lk lr) k r) => (ll lk (lr k r))
            let inner = self.bin(key, value, ln.right, r)?;
            self.bin(ln.key, ln.value.clone(), ln.left, inner)
        } else {
            // Double: ((ll lk (lrl lrk lrr)) k r) => ((ll lk lrl) lrk (lrr k r))
            let Some(lrp) = ln.right else {
                return self.bin(key, value, l, r);
            };
            let lr = self.node(lrp);
            let a = self.bin(ln.key, ln.value.clone(), ln.left, lr.left)?;
            let b = self.bin(key, value, lr.right, r)?;
            self.bin(lr.key, lr.value.clone(), a, b)
        }
    }

    /// Find the node holding `key`.
    pub fn lookup(&self, t: Tree, key: &DictKey) -> Option<RefPtr<DictNode>> {
        let mut cur = t;
        while let Some(p) = cur {
            let n = self.node(p);
            match self.cmp_keys(key, &n.key) {
                Ordering::Less => cur = n.left,
                Ordering::Greater => cur = n.right,
                Ordering::Equal => return Some(p),
            }
        }
        None
    }

    /// The tree with `key` mapped to `value`. If an equal key is present,
    /// the stored key is kept and only the value replaced.
    pub fn insert(&self, t: Tree, key: DictKey, value: BcValue) -> Result<Tree, ExecutionError> {
        let Some(p) = t else {
            return self.bin(key, value, None, None);
        };
        let n = self.node(p);
        match self.cmp_keys(&key, &n.key) {
            Ordering::Less => {
                let l = self.insert(n.left, key, value)?;
                self.balance(n.key, n.value.clone(), l, n.right)
            }
            Ordering::Greater => {
                let r = self.insert(n.right, key, value)?;
                self.balance(n.key, n.value.clone(), n.left, r)
            }
            Ordering::Equal => self.bin(n.key, value, n.left, n.right),
        }
    }

    /// The tree without `key` (the same tree if it is absent).
    pub fn delete(&self, t: Tree, key: &DictKey) -> Result<Tree, ExecutionError> {
        let Some(p) = t else {
            return Ok(None);
        };
        let n = self.node(p);
        match self.cmp_keys(key, &n.key) {
            Ordering::Less => {
                let l = self.delete(n.left, key)?;
                if l == n.left {
                    return Ok(t);
                }
                self.balance(n.key, n.value.clone(), l, n.right)
            }
            Ordering::Greater => {
                let r = self.delete(n.right, key)?;
                if r == n.right {
                    return Ok(t);
                }
                self.balance(n.key, n.value.clone(), n.left, r)
            }
            Ordering::Equal => self.glue(n.left, n.right),
        }
    }

    /// The node with the least key.
    pub fn min(&self, t: Tree) -> Option<RefPtr<DictNode>> {
        let mut p = t?;
        while let Some(l) = self.node(p).left {
            p = l;
        }
        Some(p)
    }

    /// The node with the greatest key.
    pub fn max(&self, t: Tree) -> Option<RefPtr<DictNode>> {
        let mut p = t?;
        while let Some(r) = self.node(p).right {
            p = r;
        }
        Some(p)
    }

    /// The least entry's node and the tree without it.
    pub fn delete_min(&self, t: Tree) -> Result<Option<(RefPtr<DictNode>, Tree)>, ExecutionError> {
        let Some(p) = t else {
            return Ok(None);
        };
        let n = self.node(p);
        match n.left {
            None => Ok(Some((p, n.right))),
            Some(_) => {
                let Some((m, l)) = self.delete_min(n.left)? else {
                    return Ok(Some((p, n.right)));
                };
                Ok(Some((m, self.balance(n.key, n.value.clone(), l, n.right)?)))
            }
        }
    }

    /// The greatest entry's node and the tree without it.
    pub fn delete_max(&self, t: Tree) -> Result<Option<(RefPtr<DictNode>, Tree)>, ExecutionError> {
        let Some(p) = t else {
            return Ok(None);
        };
        let n = self.node(p);
        match n.right {
            None => Ok(Some((p, n.left))),
            Some(_) => {
                let Some((m, r)) = self.delete_max(n.right)? else {
                    return Ok(Some((p, n.left)));
                };
                Ok(Some((m, self.balance(n.key, n.value.clone(), n.left, r)?)))
            }
        }
    }

    /// Join two balanced trees whose sizes differ by at most Δ (every key
    /// of `l` below every key of `r`), taking the root from the larger.
    fn glue(&self, l: Tree, r: Tree) -> Result<Tree, ExecutionError> {
        match (l, r) {
            (None, _) => Ok(r),
            (_, None) => Ok(l),
            _ => {
                if self.size(l) > self.size(r) {
                    match self.delete_max(l)? {
                        Some((m, l2)) => {
                            let mn = self.node(m);
                            self.balance(mn.key, mn.value.clone(), l2, r)
                        }
                        None => Ok(r),
                    }
                } else {
                    match self.delete_min(r)? {
                        Some((m, r2)) => {
                            let mn = self.node(m);
                            self.balance(mn.key, mn.value.clone(), l, r2)
                        }
                        None => Ok(l),
                    }
                }
            }
        }
    }

    /// Join with a middle entry: every key of `l` < `key` < every key of
    /// `r`; the trees may differ arbitrarily in size. O(log n).
    pub fn link(
        &self,
        l: Tree,
        key: DictKey,
        value: BcValue,
        r: Tree,
    ) -> Result<Tree, ExecutionError> {
        match (l, r) {
            (None, _) => self.insert_min(key, value, r),
            (_, None) => self.insert_max(key, value, l),
            (Some(lp), Some(rp)) => {
                let (ln, rn) = (self.node(lp), self.node(rp));
                if DELTA * ln.size < rn.size {
                    let inner = self.link(l, key, value, rn.left)?;
                    self.balance(rn.key, rn.value.clone(), inner, rn.right)
                } else if DELTA * rn.size < ln.size {
                    let inner = self.link(ln.right, key, value, r)?;
                    self.balance(ln.key, ln.value.clone(), ln.left, inner)
                } else {
                    self.bin(key, value, l, r)
                }
            }
        }
    }

    /// Join without a middle entry: every key of `l` < every key of `r`.
    /// O(log n).
    pub fn merge(&self, l: Tree, r: Tree) -> Result<Tree, ExecutionError> {
        match (l, r) {
            (None, _) => Ok(r),
            (_, None) => Ok(l),
            (Some(lp), Some(rp)) => {
                let (ln, rn) = (self.node(lp), self.node(rp));
                if DELTA * ln.size < rn.size {
                    let inner = self.merge(l, rn.left)?;
                    self.balance(rn.key, rn.value.clone(), inner, rn.right)
                } else if DELTA * rn.size < ln.size {
                    let inner = self.merge(ln.right, r)?;
                    self.balance(ln.key, ln.value.clone(), ln.left, inner)
                } else {
                    self.glue(l, r)
                }
            }
        }
    }

    fn insert_min(&self, key: DictKey, value: BcValue, t: Tree) -> Result<Tree, ExecutionError> {
        match t {
            None => self.bin(key, value, None, None),
            Some(p) => {
                let n = self.node(p);
                let l = self.insert_min(key, value, n.left)?;
                self.balance(n.key, n.value.clone(), l, n.right)
            }
        }
    }

    fn insert_max(&self, key: DictKey, value: BcValue, t: Tree) -> Result<Tree, ExecutionError> {
        match t {
            None => self.bin(key, value, None, None),
            Some(p) => {
                let n = self.node(p);
                let r = self.insert_max(key, value, n.right)?;
                self.balance(n.key, n.value.clone(), n.left, r)
            }
        }
    }

    /// Split around `key`: the entries below it, the node holding it (if
    /// any) and the entries above it. O(log n).
    pub fn split(&self, t: Tree, key: &DictKey) -> Result<Split, ExecutionError> {
        let Some(p) = t else {
            return Ok((None, None, None));
        };
        let n = self.node(p);
        match self.cmp_keys(key, &n.key) {
            Ordering::Less => {
                let (ll, found, lr) = self.split(n.left, key)?;
                let r = self.link(lr, n.key, n.value.clone(), n.right)?;
                Ok((ll, found, r))
            }
            Ordering::Greater => {
                let (rl, found, rr) = self.split(n.right, key)?;
                let l = self.link(n.left, n.key, n.value.clone(), rl)?;
                Ok((l, found, rr))
            }
            Ordering::Equal => Ok((n.left, Some(p), n.right)),
        }
    }

    /// The sub-tree of entries with `lo <= key < hi`. O(log n).
    pub fn range(&self, t: Tree, lo: &DictKey, hi: &DictKey) -> Result<Tree, ExecutionError> {
        if self.cmp_keys(lo, hi) != Ordering::Less {
            return Ok(None);
        }
        let (_, found, above) = self.split(t, lo)?;
        let from_lo = match found {
            Some(f) => {
                let fnode = self.node(f);
                self.insert_min(fnode.key, fnode.value.clone(), above)?
            }
            None => above,
        };
        let (below_hi, _, _) = self.split(from_lo, hi)?;
        Ok(below_hi)
    }

    /// Build a perfectly balanced tree from entries in strictly increasing
    /// key order. O(n).
    pub fn build_sorted(&self, entries: &[Entry]) -> Result<Tree, ExecutionError> {
        if entries.is_empty() {
            return Ok(None);
        }
        let mid = entries.len() / 2;
        let l = self.build_sorted(&entries[..mid])?;
        let r = self.build_sorted(&entries[mid + 1..])?;
        let (k, v) = &entries[mid];
        self.bin(*k, v.clone(), l, r)
    }

    /// Sort entries by key (stably) and keep, for each run of equal keys,
    /// the last value under the first key — the result repeated
    /// [`Dict::insert`]s would give (an equal key keeps the stored key and
    /// replaces the value) — ready for [`Dict::build_sorted`].
    pub fn sort_last_wins(&self, mut entries: Vec<Entry>) -> Vec<Entry> {
        entries.sort_by(|a, b| self.cmp_keys(&a.0, &b.0));
        let mut out: Vec<Entry> = Vec::with_capacity(entries.len());
        for (k, v) in entries {
            match out.last_mut() {
                Some(last) if self.cmp_keys(&last.0, &k) == Ordering::Equal => last.1 = v,
                _ => out.push((k, v)),
            }
        }
        out
    }

    /// Visit every node in key order.
    pub fn for_each<F: FnMut(&'a DictNode)>(&self, t: Tree, mut f: F) {
        let mut stack: Vec<RefPtr<DictNode>> = Vec::new();
        let mut cur = t;
        loop {
            while let Some(p) = cur {
                stack.push(p);
                cur = self.node(p).left;
            }
            let Some(p) = stack.pop() else {
                return;
            };
            let n = self.node(p);
            f(n);
            cur = n.right;
        }
    }

    /// All entries in key order.
    pub fn entries(&self, t: Tree) -> Vec<Entry> {
        let mut out = Vec::with_capacity(self.size(t) as usize);
        self.for_each(t, |n| out.push((n.key, n.value.clone())));
        out
    }

    /// Check the tree's invariants (sizes, weight balance, strict key
    /// order), returning a description of the first violation.
    pub fn check_invariants(&self, t: Tree) -> Result<(), String> {
        fn go(
            d: &Dict<'_>,
            t: Tree,
            lo: Option<DictKey>,
            hi: Option<DictKey>,
        ) -> Result<u64, String> {
            let Some(p) = t else {
                return Ok(0);
            };
            let n = d.node(p);
            if let Some(lo) = lo {
                if d.cmp_keys(&lo, &n.key) != Ordering::Less {
                    return Err(format!("key order violated at {:?}", n.key));
                }
            }
            if let Some(hi) = hi {
                if d.cmp_keys(&n.key, &hi) != Ordering::Less {
                    return Err(format!("key order violated at {:?}", n.key));
                }
            }
            let sl = go(d, n.left, lo, Some(n.key))?;
            let sr = go(d, n.right, Some(n.key), hi)?;
            if n.size != sl + sr + 1 {
                return Err(format!("size {} != {} + {} + 1", n.size, sl, sr));
            }
            if sl + sr > 1 && (sl > DELTA * sr || sr > DELTA * sl) {
                return Err(format!("unbalanced node: sizes {sl} and {sr}"));
            }
            Ok(n.size)
        }
        go(self, t, None, None).map(|_| ())
    }
}

/// A non-null pointer to a dict node, as stored in roots.
pub type NodePtr = NonNull<DictNode>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::bytecode::{BcClosure, BcEnvFrame};
    use crate::eval::machine::metrics::{Clock, ThreadOccupation};
    use crate::eval::memory::collect::{collect, collect_with_evacuation};
    use crate::eval::memory::heap::Heap;
    use crate::eval::memory::syntax::{Native, StgBuilder};

    fn int(i: i64) -> DictKey {
        DictKey::Int(i)
    }

    fn float(f: f64) -> DictKey {
        DictKey::Float(OrderedFloat(f))
    }

    fn val(i: i64) -> BcValue {
        BcValue::Native(Native::Num(Number::from(i)))
    }

    fn val_of(v: &BcValue) -> i64 {
        match v {
            BcValue::Native(Native::Num(n)) => n.as_i64().expect("integer value"),
            _ => panic!("not a number value"),
        }
    }

    fn int_of(k: &DictKey) -> i64 {
        match k {
            DictKey::Int(i) => *i,
            other => panic!("not an int key: {other:?}"),
        }
    }

    /// A deterministic pseudo-random permutation of 0..n.
    fn shuffled(n: i64) -> Vec<i64> {
        let mut xs: Vec<i64> = (0..n).collect();
        let mut s: u64 = 0x9E37_79B9_7F4A_7C15;
        for i in (1..xs.len()).rev() {
            s ^= s << 13;
            s ^= s >> 7;
            s ^= s << 17;
            xs.swap(i, (s % (i as u64 + 1)) as usize);
        }
        xs
    }

    #[test]
    fn number_keys_compare_exactly() {
        let heap = Heap::new();
        let pool = SymbolPool::new();
        let d = Dict::new(MutatorHeapView::new(&heap), &pool);
        // 1 = 1.0, -0.0 = 0.0
        assert_eq!(d.cmp_keys(&int(1), &float(1.0)), Ordering::Equal);
        assert_eq!(d.cmp_keys(&float(-0.0), &int(0)), Ordering::Equal);
        assert_eq!(d.cmp_keys(&float(-0.0), &float(0.0)), Ordering::Equal);
        // Exact above 2^53: 2^53 + 1 is not the float 2^53.
        let big = 9_007_199_254_740_993_i64;
        assert_eq!(
            d.cmp_keys(&int(big), &float(9_007_199_254_740_992.0)),
            Ordering::Greater
        );
        assert_eq!(d.cmp_keys(&int(big), &int(big - 1)), Ordering::Greater);
        // Fractions between integers.
        assert_eq!(d.cmp_keys(&int(2), &float(2.5)), Ordering::Less);
        assert_eq!(d.cmp_keys(&float(-2.5), &int(-2)), Ordering::Less);
        assert_eq!(d.cmp_keys(&float(-2.5), &int(-3)), Ordering::Greater);
        // u64 above i64::MAX.
        let u = DictKey::UInt(u64::MAX);
        assert_eq!(d.cmp_keys(&int(i64::MAX), &u), Ordering::Less);
        assert_eq!(d.cmp_keys(&u, &float(1e30)), Ordering::Less);
        assert_eq!(d.cmp_keys(&float(-1e30), &int(i64::MIN)), Ordering::Less);
        // NaN is one key, greatest of all numbers.
        assert_eq!(
            d.cmp_keys(&float(f64::NAN), &float(f64::NAN)),
            Ordering::Equal
        );
        assert_eq!(d.cmp_keys(&float(f64::NAN), &u), Ordering::Greater);
        assert_eq!(
            d.cmp_keys(&float(f64::INFINITY), &float(f64::NAN)),
            Ordering::Less
        );
        // Conversion keeps integers exact.
        assert!(matches!(DictKey::from_number(&Number::from(big)), DictKey::Int(i) if i == big));
        assert!(matches!(
            DictKey::from_number(&Number::from(u64::MAX)),
            DictKey::UInt(u64::MAX)
        ));
        assert!(matches!(
            DictKey::from_number(&Number::from_f64(2.5).unwrap()),
            DictKey::Float(f) if f.into_inner() == 2.5
        ));
    }

    #[test]
    fn cross_type_and_list_order() {
        let heap = Heap::new();
        let mut pool = SymbolPool::new();
        // Interned in reverse name order: order must follow the name.
        let zed = pool.intern("zed");
        let abe = pool.intern("abe");
        let view = MutatorHeapView::new(&heap);
        let d = Dict::new(view, &pool);
        let s_b = DictKey::Str(view.str("b").unwrap().as_ptr());
        let s_a = DictKey::Str(view.str("a").unwrap().as_ptr());
        let s_a2 = DictKey::Str(view.str("a").unwrap().as_ptr());
        let s_long = DictKey::Str(view.str("ab").unwrap().as_ptr());
        let l = |xs: &[DictKey]| DictKey::List(DictKeyList::alloc(&view, xs).unwrap());
        let l_2_10 = l(&[int(2), int(10)]);
        let l_10_2 = l(&[int(10), int(2)]);
        let l_1 = l(&[int(1)]);
        let l_1_0 = l(&[int(1), int(0)]);
        let l_1_0b = l(&[int(1), float(0.0)]);
        let l_empty = l(&[]);

        assert_eq!(d.cmp_keys(&int(1_000_000), &s_a), Ordering::Less);
        assert_eq!(d.cmp_keys(&s_b, &DictKey::Sym(abe)), Ordering::Less);
        assert_eq!(d.cmp_keys(&DictKey::Sym(zed), &l_empty), Ordering::Less);
        assert_eq!(d.cmp_keys(&s_a, &s_a2), Ordering::Equal);
        assert_eq!(d.cmp_keys(&s_a, &s_long), Ordering::Less);
        assert_eq!(d.cmp_keys(&s_long, &s_b), Ordering::Less);
        assert_eq!(
            d.cmp_keys(&DictKey::Sym(abe), &DictKey::Sym(zed)),
            Ordering::Less
        );
        assert_eq!(d.cmp_keys(&l_2_10, &l_10_2), Ordering::Less);
        assert_eq!(d.cmp_keys(&l_1, &l_1_0), Ordering::Less);
        assert_eq!(d.cmp_keys(&l_empty, &l_1), Ordering::Less);
        assert_eq!(d.cmp_keys(&l_1_0, &l_1_0b), Ordering::Equal);
        let nested = l(&[l_1, s_a]);
        let nested2 = l(&[l_1_0, s_a]);
        assert_eq!(d.cmp_keys(&nested, &nested2), Ordering::Less);
        assert_eq!(nested.list_in(&view).unwrap().len(), 2);
    }

    #[test]
    fn insert_lookup_delete_keep_balance() {
        let heap = Heap::new();
        let pool = SymbolPool::new();
        let d = Dict::new(MutatorHeapView::new(&heap), &pool);
        let n = 2000;
        let mut t: Tree = None;
        for i in shuffled(n) {
            t = d.insert(t, int(i), val(i * 10)).unwrap();
        }
        d.check_invariants(t).unwrap();
        assert_eq!(d.size(t), n as u64);
        for i in 0..n {
            let p = d.lookup(t, &int(i)).expect("present");
            assert_eq!(val_of(d.node(p).value()), i * 10);
        }
        assert!(d.lookup(t, &int(n)).is_none());
        assert!(d.lookup(t, &float(0.5)).is_none());
        // In-order walk is sorted.
        let keys: Vec<i64> = d.entries(t).iter().map(|e| int_of(&e.0)).collect();
        assert_eq!(keys, (0..n).collect::<Vec<_>>());

        // Replacing keeps the stored key and size; old version unchanged.
        let t2 = d.insert(t, float(5.0), val(-1)).unwrap();
        assert_eq!(d.size(t2), n as u64);
        let p = d.lookup(t2, &int(5)).unwrap();
        assert!(matches!(d.node(p).key(), DictKey::Int(5)));
        assert_eq!(val_of(d.node(p).value()), -1);
        assert_eq!(val_of(d.node(d.lookup(t, &int(5)).unwrap()).value()), 50);

        // Delete every even key, in shuffled order.
        let mut u = t;
        for i in shuffled(n) {
            if i % 2 == 0 {
                u = d.delete(u, &int(i)).unwrap();
                d.check_invariants(u).unwrap();
            }
        }
        assert_eq!(d.size(u), (n / 2) as u64);
        assert!(d.lookup(u, &int(4)).is_none());
        assert!(d.lookup(u, &int(5)).is_some());
        // Deleting an absent key returns the same tree.
        assert_eq!(d.delete(u, &int(4)).unwrap(), u);
        // The original is untouched (persistence).
        assert_eq!(d.size(t), n as u64);
        d.check_invariants(t).unwrap();

        // Ascending and descending insertion stay balanced.
        let mut asc: Tree = None;
        let mut desc: Tree = None;
        for i in 0..n {
            asc = d.insert(asc, int(i), val(i)).unwrap();
            desc = d.insert(desc, int(n - i), val(i)).unwrap();
        }
        d.check_invariants(asc).unwrap();
        d.check_invariants(desc).unwrap();
    }

    #[test]
    fn min_max_and_pops() {
        let heap = Heap::new();
        let pool = SymbolPool::new();
        let d = Dict::new(MutatorHeapView::new(&heap), &pool);
        assert!(d.min(None).is_none());
        assert!(d.delete_min(None).unwrap().is_none());
        let mut t: Tree = None;
        for i in shuffled(300) {
            t = d.insert(t, int(i), val(i)).unwrap();
        }
        assert_eq!(int_of(&d.node(d.min(t).unwrap()).key()), 0);
        assert_eq!(int_of(&d.node(d.max(t).unwrap()).key()), 299);
        let mut popped = vec![];
        let mut cur = t;
        while let Some((m, rest)) = d.delete_min(cur).unwrap() {
            popped.push(int_of(&d.node(m).key()));
            d.check_invariants(rest).unwrap();
            cur = rest;
        }
        assert_eq!(popped, (0..300).collect::<Vec<_>>());
        let mut popped = vec![];
        let mut cur = t;
        while let Some((m, rest)) = d.delete_max(cur).unwrap() {
            popped.push(int_of(&d.node(m).key()));
            d.check_invariants(rest).unwrap();
            cur = rest;
        }
        assert_eq!(popped, (0..300).rev().collect::<Vec<_>>());
    }

    #[test]
    fn split_link_merge_range() {
        let heap = Heap::new();
        let pool = SymbolPool::new();
        let d = Dict::new(MutatorHeapView::new(&heap), &pool);
        let mut t: Tree = None;
        for i in shuffled(500) {
            t = d.insert(t, int(i * 2), val(i)).unwrap();
        }
        // Split at a present and an absent key.
        let (l, found, r) = d.split(t, &int(400)).unwrap();
        d.check_invariants(l).unwrap();
        d.check_invariants(r).unwrap();
        assert_eq!(int_of(&d.node(found.unwrap()).key()), 400);
        assert_eq!(d.size(l), 200);
        assert_eq!(d.size(r), 299);
        let (l2, found2, r2) = d.split(t, &int(401)).unwrap();
        assert!(found2.is_none());
        assert_eq!(d.size(l2) + d.size(r2), 500);

        // Link very unequal trees.
        let small = d.build_sorted(&[(int(-5), val(0))]).unwrap();
        let linked = d.link(small, int(-1), val(1), r).unwrap();
        d.check_invariants(linked).unwrap();
        assert_eq!(d.size(linked), 301);
        // Merge the two sides of a split back together.
        let merged = d.merge(l, r).unwrap();
        d.check_invariants(merged).unwrap();
        assert_eq!(d.size(merged), 499);
        assert!(d.lookup(merged, &int(400)).is_none());
        let merged = d.merge(None, r).unwrap();
        assert_eq!(merged, r);

        // Half-open range.
        let rg = d.range(t, &int(10), &int(20)).unwrap();
        d.check_invariants(rg).unwrap();
        let keys: Vec<i64> = d.entries(rg).iter().map(|e| int_of(&e.0)).collect();
        assert_eq!(keys, vec![10, 12, 14, 16, 18]);
        let rg = d.range(t, &float(9.5), &float(12.5)).unwrap();
        let keys: Vec<i64> = d.entries(rg).iter().map(|e| int_of(&e.0)).collect();
        assert_eq!(keys, vec![10, 12]);
        assert!(d.range(t, &int(20), &int(10)).unwrap().is_none());
        assert!(d.range(t, &int(20), &int(20)).unwrap().is_none());
        assert_eq!(d.size(d.range(t, &int(-100), &int(10_000)).unwrap()), 500);
    }

    /// `link` and `merge` keep the balance invariant for every pair of
    /// operand sizes up to 64, whatever shape the operands were built in.
    #[test]
    fn link_and_merge_all_size_pairs() {
        let heap = Heap::new();
        let pool = SymbolPool::new();
        let d = Dict::new(MutatorHeapView::new(&heap), &pool);
        // For each size, a bulk-built tree and an ascending-insert tree.
        let build = |lo: i64, n: i64, bulk: bool| -> Tree {
            if bulk {
                let es: Vec<Entry> = (lo..lo + n).map(|i| (int(i), val(i))).collect();
                d.build_sorted(&es).unwrap()
            } else {
                let mut t = None;
                for i in lo..lo + n {
                    t = d.insert(t, int(i), val(i)).unwrap();
                }
                t
            }
        };
        for nl in 0..64 {
            for nr in 0..64 {
                for (bl, br) in [(true, true), (false, true), (true, false), (false, false)] {
                    let l = build(0, nl, bl);
                    let r = build(1000, nr, br);
                    let linked = d.link(l, int(500), val(500), r).unwrap();
                    d.check_invariants(linked).unwrap();
                    assert_eq!(d.size(linked), (nl + nr + 1) as u64);
                    let merged = d.merge(l, r).unwrap();
                    d.check_invariants(merged).unwrap();
                    assert_eq!(d.size(merged), (nl + nr) as u64);
                }
            }
        }
    }

    #[test]
    fn bulk_build_last_wins() {
        let heap = Heap::new();
        let pool = SymbolPool::new();
        let d = Dict::new(MutatorHeapView::new(&heap), &pool);
        let mut entries: Vec<Entry> = shuffled(1000)
            .into_iter()
            .map(|i| (int(i), val(i)))
            .collect();
        entries.push((float(7.0), val(-7)));
        entries.push((int(3), val(-3)));
        let sorted = d.sort_last_wins(entries);
        assert_eq!(sorted.len(), 1000);
        let t = d.build_sorted(&sorted).unwrap();
        d.check_invariants(t).unwrap();
        assert_eq!(d.size(t), 1000);
        assert_eq!(val_of(d.node(d.lookup(t, &int(7)).unwrap()).value()), -7);
        // The first key of an equal run is kept, as repeated inserts keep it.
        assert!(matches!(
            d.node(d.lookup(t, &int(7)).unwrap()).key(),
            DictKey::Int(7)
        ));
        assert_eq!(val_of(d.node(d.lookup(t, &int(3)).unwrap()).value()), -3);
        assert_eq!(val_of(d.node(d.lookup(t, &int(4)).unwrap()).value()), 4);
        assert!(d.build_sorted(&[]).unwrap().is_none());
    }

    /// Addresses of every key string, including strings inside key lists.
    fn key_strings(d: &Dict<'_>, t: Tree) -> Vec<usize> {
        let mut out = vec![];
        d.for_each(t, |nd| match nd.key() {
            DictKey::Str(p) => out.push(p.as_ptr() as usize),
            k @ DictKey::List(_) => {
                for e in k.list_in(&d.view).unwrap() {
                    if let DictKey::Str(p) = e {
                        out.push(p.as_ptr() as usize);
                    }
                }
            }
            _ => {}
        });
        out
    }

    /// Addresses of the env frames of every closure value.
    fn closure_envs(d: &Dict<'_>, t: Tree) -> Vec<usize> {
        let mut out = vec![];
        d.for_each(t, |nd| {
            if let BcValue::Closure(c) = nd.value() {
                out.push(c.env().as_ptr() as usize);
            }
        });
        out
    }

    /// Build trees with string, list and string-valued entries, then run a
    /// full evacuation (every block a candidate) and a mark-in-place
    /// collection, rooting only the tree roots. Every node, key string,
    /// key list and value string must be forwarded and read back intact.
    #[test]
    fn trees_survive_gc_evacuation() {
        let mut heap = Heap::new();
        let mut clock = Clock::default();
        clock.switch(ThreadOccupation::Mutator);
        let mut pool = SymbolPool::new();
        let sym = pool.intern("sym");
        let n = 400;

        let (mut roots, old_nodes, old_strs, old_envs) = {
            let view = MutatorHeapView::new(&heap);
            let d = Dict::new(view, &pool);
            let mut t: Tree = None;
            for i in shuffled(n) {
                let s = view.str(format!("k{i:04}")).unwrap().as_ptr();
                // A distinct string, reachable only through the key list.
                let ls = view.str(format!("k{i:04}")).unwrap().as_ptr();
                let lst = DictKeyList::alloc(&view, &[int(i), DictKey::Str(ls), DictKey::Sym(sym)])
                    .unwrap();
                let vstr = view.str(format!("v{i}")).unwrap().as_ptr();
                t = d
                    .insert(t, DictKey::Str(s), BcValue::Native(Native::Str(vstr)))
                    .unwrap();
                t = d.insert(t, DictKey::List(lst), val(i)).unwrap();
                // A closure value (its env must be traced and forwarded)
                // under a nested list key.
                let env = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
                let inner = DictKeyList::alloc(&view, &[int(i)]).unwrap();
                let nested = DictKeyList::alloc(&view, &[DictKey::List(inner)]).unwrap();
                let clo = BcValue::Closure(BcClosure::new(i as u32, env));
                t = d.insert(t, DictKey::List(nested), clo).unwrap();
                // Garbage interleaved with live data.
                let _ = view.str("garbage").unwrap();
            }
            let pruned = d
                .delete(t, &DictKey::Str(view.str("k0000").unwrap().as_ptr()))
                .unwrap();
            let roots: Vec<NodePtr> = vec![t.unwrap(), pruned.unwrap()];
            let mut nodes = vec![];
            d.for_each(t, |nd| nodes.push(nd as *const DictNode as usize));
            (roots, nodes, key_strings(&d, t), closure_envs(&d, t))
        };

        let rest = heap.rest_block_count();
        let candidates: Vec<usize> = (0..2 + rest).collect();
        collect_with_evacuation(&mut roots, &mut heap, &mut clock, &candidates, false);
        collect(&mut roots, &mut heap, &mut clock, false);
        heap.flush_unswept();

        // Pressure-fill any hole a missed mark would have left.
        clock.switch(ThreadOccupation::Mutator);
        {
            let view = MutatorHeapView::new(&heap);
            for i in 0..5000 {
                let _ = view.str(format!("filler{i}")).unwrap();
            }
        }

        let view = MutatorHeapView::new(&heap);
        let d = Dict::new(view, &pool);
        let t = Some(roots[0]);
        let pruned = Some(roots[1]);
        d.check_invariants(t).unwrap();
        d.check_invariants(pruned).unwrap();
        assert_eq!(d.size(t), 3 * n as u64);
        assert_eq!(d.size(pruned), 3 * n as u64 - 1);

        let mut new_nodes = vec![];
        d.for_each(t, |nd| new_nodes.push(nd as *const DictNode as usize));
        let moved = new_nodes
            .iter()
            .zip(&old_nodes)
            .filter(|(a, b)| a != b)
            .count();
        assert_eq!(moved, old_nodes.len(), "every node must be forwarded");
        let new_strs = key_strings(&d, t);
        let moved = new_strs
            .iter()
            .zip(&old_strs)
            .filter(|(a, b)| a != b)
            .count();
        assert_eq!(
            moved,
            old_strs.len(),
            "every key string (and key-list string) must be forwarded"
        );

        let new_envs = closure_envs(&d, t);
        assert_eq!(new_envs.len(), n as usize);
        let moved = new_envs
            .iter()
            .zip(&old_envs)
            .filter(|(a, b)| a != b)
            .count();
        assert_eq!(
            moved,
            old_envs.len(),
            "every closure value's env must be forwarded"
        );

        // Contents intact: string keys map to their value strings, list
        // keys hold their elements and map to their numbers.
        let mut strs = 0;
        let mut lists = 0;
        let mut nested = 0;
        d.for_each(t, |nd| match nd.key() {
            DictKey::Str(_) => {
                let k = nd.key().str_in(&view).unwrap();
                let i: i64 = k[1..].parse().unwrap();
                let BcValue::Native(Native::Str(v)) = nd.value() else {
                    panic!("string value expected");
                };
                assert_eq!(view.scoped(*v).as_str(), format!("v{i}"));
                strs += 1;
            }
            DictKey::List(_) if nd.key().list_in(&view).unwrap().len() == 1 => {
                // Nested list key [[i]] with a closure value of code i.
                let outer = nd.key().list_in(&view).unwrap();
                let inner = outer[0].list_in(&view).expect("nested list key");
                let i = int_of(&inner[0]);
                let BcValue::Closure(c) = nd.value() else {
                    panic!("closure value expected");
                };
                assert_eq!(c.code(), i as u32);
                nested += 1;
            }
            DictKey::List(_) => {
                let elems = nd.key().list_in(&view).unwrap();
                assert_eq!(elems.len(), 3);
                let i = int_of(&elems[0]);
                assert_eq!(elems[1].str_in(&view).unwrap(), format!("k{i:04}"));
                assert!(matches!(elems[2], DictKey::Sym(s) if s == sym));
                assert_eq!(val_of(nd.value()), i);
                lists += 1;
            }
            other => panic!("unexpected key {other:?}"),
        });
        assert_eq!((strs, lists, nested), (n, n, n));
        let probe = DictKey::Str(view.str("k0123").unwrap().as_ptr());
        assert!(d.lookup(t, &probe).is_some());
        let probe0 = DictKey::Str(view.str("k0000").unwrap().as_ptr());
        assert!(d.lookup(pruned, &probe0).is_none());
    }
}
