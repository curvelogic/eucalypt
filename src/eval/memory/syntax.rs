//! Value representation shared by the STG runtime, allocated in
//! eucalypt-managed memory.
//!
//! Used to also define `HeapSyn` — the legacy HeapSyn tree-walk engine's
//! in-heap compiled syntax representation, plus its `GcScannable` impl and
//! the `StgBuilder` methods that built it at runtime — deleted by the Phase
//! 4 collapse (eu-oufc). `Native`, `Reference`/`Ref` and `RefPtr` below are
//! shared value-representation types the bytecode engine (the sole
//! execution engine since that collapse) uses directly; `StgBuilder` is
//! trimmed to the two methods (`str`, `str_ref`, `sym_ref`) that allocate a
//! native value rather than build HeapSyn code.

use crate::eval::error::ExecutionError;
use chrono::{DateTime, FixedOffset};
use serde_json::Number;
use std::{collections::HashMap, fmt, ptr::NonNull, rc::Rc};

use super::alloc::{ScopedPtr, StgObject};
use super::ndarray::HeapNdArray;
use super::set::HeapSet;
use super::string::HeapString;
use super::symbol::SymbolId;
use super::vec::HeapVec;

/// References between allocated objects use RefPtr
pub type RefPtr<T> = NonNull<T>;

/// Block index mapping interned symbol IDs to list positions.
pub type BlockIndex = HashMap<SymbolId, usize>;

/// Enum based primitive storage.
#[derive(Clone, Debug)]
pub enum Native {
    /// An interned symbol, referenced by compact ID
    Sym(SymbolId),
    /// A string
    Str(RefPtr<HeapString>),
    /// A number
    Num(Number),
    /// A zoned datetime
    Zdt(DateTime<FixedOffset>),
    /// A block index (cache, not semantic content)
    Index(Rc<BlockIndex>),
    /// A set of primitive values
    Set(RefPtr<HeapSet>),
    /// An n-dimensional array of f64 values
    NdArray(RefPtr<HeapNdArray>),
    /// A vector of primitive values (O(1) indexed access)
    Vec(RefPtr<HeapVec>),
    /// An opaque PRNG stream state (SplitMix64 state word)
    Prng(u64),
    /// A handle into the ProducerTable for lazy producers
    Producer(u32),
}

impl PartialEq for Native {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Native::Sym(a), Native::Sym(b)) => a == b,
            (Native::Str(a), Native::Str(b)) => a == b,
            (Native::Num(a), Native::Num(b)) => a == b,
            (Native::Zdt(a), Native::Zdt(b)) => a == b,
            // Index is a cache — always equal to another Index
            (Native::Index(_), Native::Index(_)) => true,
            (Native::Set(a), Native::Set(b)) => a == b,
            (Native::NdArray(a), Native::NdArray(b)) => a == b,
            (Native::Vec(a), Native::Vec(b)) => a == b,
            (Native::Prng(a), Native::Prng(b)) => a == b,
            (Native::Producer(a), Native::Producer(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Native {}

impl Native {
    /// Return a human-readable type name for use in error messages
    pub fn type_description(&self) -> &'static str {
        match self {
            Native::Sym(_) => "symbol",
            Native::Str(_) => "string",
            Native::Num(_) => "number",
            Native::Zdt(_) => "datetime",
            Native::Index(_) => "block index",
            Native::Set(_) => "set",
            Native::NdArray(_) => "array",
            Native::Vec(_) => "vec",
            Native::Prng(_) => "prng",
            Native::Producer(_) => "producer",
        }
    }
}

impl StgObject for Native {}

impl fmt::Display for Native {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Native::Sym(id) => {
                write!(f, ":{id}")
            }
            Native::Str(s) => {
                write!(f, "\"<{s:p}>\"")
            }
            Native::Num(n) => {
                write!(f, "{n}")
            }
            Native::Zdt(t) => {
                write!(f, "☽{t}")
            }
            Native::Index(idx) => {
                write!(f, "<index:{}>", idx.len())
            }
            Native::Set(_) => {
                write!(f, "<set>")
            }
            Native::NdArray(_) => {
                write!(f, "<array>")
            }
            Native::Vec(_) => {
                write!(f, "<vec>")
            }
            Native::Prng(_) => {
                write!(f, "<prng>")
            }
            Native::Producer(_) => {
                write!(f, "<producer>")
            }
        }
    }
}

/// A reference into environments or a value
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Reference<T: Clone> {
    /// Local index into environment
    L(usize),
    /// Global index
    G(usize),
    /// Value
    V(T),
}

impl<T: Clone> StgObject for Reference<T> {}

impl<T: Clone> Reference<T> {
    /// A local reference
    pub fn lref(n: usize) -> Self {
        Reference::L(n)
    }

    /// A global reference
    pub fn gref(n: usize) -> Self {
        Reference::L(n)
    }

    /// A native embedded in a ref
    pub fn vref(native: T) -> Self {
        Reference::V(native)
    }

    /// Return a local reference one deeper
    pub fn bump(&self, delta: usize) -> Reference<T> {
        match self {
            Reference::L(n) => Reference::L(n + delta),
            _ => (*self).clone(),
        }
    }
}

impl<T: Clone> fmt::Display for Reference<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Reference::L(i) => {
                write!(f, "✳{i}")
            }
            Reference::G(i) => {
                write!(f, "⊗{i}")
            }
            Reference::V(n) => {
                write!(f, "!{n}")
            }
        }
    }
}

pub type Ref = Reference<Native>;

impl Ref {
    /// A native number atom
    pub fn num<N>(n: N) -> Ref
    where
        N: Into<Number>,
    {
        Self::vref(Native::Num(n.into()))
    }

    /// Create a zoned datetime
    pub fn zdt(dt: DateTime<FixedOffset>) -> Ref {
        Self::vref(Native::Zdt(dt))
    }
}

/// A reference to the allocator for allocating native values that need no
/// runtime code — a string on the heap, or an interned symbol. Trimmed from
/// its original scope (which also built the HeapSyn tree-walk engine's
/// in-heap code nodes) by the Phase 4 collapse, which deleted HeapSyn
/// (eu-oufc).
pub trait StgBuilder<'scope> {
    /// Intern a symbol and wrap as ref
    fn sym_ref<T: AsRef<str>>(
        &'scope self,
        pool: &mut super::symbol::SymbolPool,
        s: T,
    ) -> Result<Ref, ExecutionError>;

    /// Allocate a string in the heap
    fn str<T: AsRef<str>>(
        &'scope self,
        s: T,
    ) -> Result<ScopedPtr<'scope, HeapString>, ExecutionError>;

    /// Allocate a string in the heap and wrap as ref
    fn str_ref<T: AsRef<str>>(&'scope self, s: T) -> Result<Ref, ExecutionError>;
}
