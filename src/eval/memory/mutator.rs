//! Support mutator access to heap and machine

use std::marker::PhantomData;

use crate::{
    common::sourcemap::Smid,
    eval::{
        error::ExecutionError,
        memory::{
            self,
            alloc::{Allocator, MutatorScope, ScopedAllocator, ScopedPtr, StgObject},
            array::Array,
            heap::Heap,
            syntax::{HeapSyn, Ref, RefPtr, StgBuilder},
        },
        stg::tags::{DataConstructor, Tag},
    },
};

use super::{string::HeapString, syntax::Native};

/// RAII guard that keeps a heap block pinned (non-evacuatable).
pub struct PinGuard {
    base_address: usize,
    heap: *const super::heap::Heap,
}

impl Drop for PinGuard {
    fn drop(&mut self) {
        let heap = unsafe { &*self.heap };
        heap.unpin_block(self.base_address);
    }
}

impl std::fmt::Debug for PinGuard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "PinGuard(block @ {:#x})", self.base_address)
    }
}

/// A view onto the heap for code that needs mutator access (as
/// opposed to collector access)
///
/// MutatorHeapView provides a scope for dereferencing heap pointers,
/// the means of allocation and convenience constructors for
/// allocating in-heap syntax.
///
/// Internally stores a raw pointer rather than a reference so that a
/// view can be constructed alongside a `&mut` borrow of the same
/// `MachineCore` struct (which also contains the `Heap`).  The
/// phantom lifetime `'guard` upholds the borrow-checker invariant
/// that the heap lives at least as long as the view.  This is safe
/// because `Heap` is entirely `UnsafeCell`/`Cell`-based and all heap
/// mutations go through interior mutability.
#[derive(Copy, Clone)]
pub struct MutatorHeapView<'guard> {
    heap: *const Heap,
    _phantom: PhantomData<&'guard Heap>,
}

impl<'guard> MutatorHeapView<'guard> {
    pub fn new(heap: &'guard Heap) -> Self {
        MutatorHeapView {
            heap: heap as *const _,
            _phantom: PhantomData,
        }
    }

    /// Construct a view from a raw heap pointer.
    ///
    /// # Safety
    ///
    /// `heap` must be a valid, aligned pointer to a `Heap` that remains live
    /// for the lifetime `'guard`.  The caller must ensure no conflicting
    /// exclusive Rust references to the `Heap` (or fields that alias it) exist
    /// for the duration of `'guard`.
    pub unsafe fn from_raw_heap(heap: *const Heap) -> Self {
        MutatorHeapView {
            heap,
            _phantom: PhantomData,
        }
    }

    #[inline(always)]
    fn heap_ref(&self) -> &'guard Heap {
        // SAFETY: the phantom lifetime 'guard guarantees the heap is alive.
        unsafe { &*self.heap }
    }

    /// Obtain a scoped pointer from a RefPtr for dereferencing
    pub fn scoped<T: Sized>(self, ptr: RefPtr<T>) -> ScopedPtr<'guard, T> {
        ScopedPtr::from_non_null(self.heap_ref(), ptr)
    }

    /// Allocate an array, copying from a slice
    pub fn array<T: Sized + Clone>(self, data: &[T]) -> Array<T> {
        Array::from_slice(&self, data)
    }

    /// Allocate a singleton array
    pub fn singleton<T: Sized + Clone>(self, object: T) -> Array<T> {
        let mut array = Array::with_capacity(&self, 1);
        array.push(&self, object);
        array
    }

    /// Pin the block containing `ptr`, preventing evacuation.
    pub fn pin<T>(&self, ptr: std::ptr::NonNull<T>) -> PinGuard {
        self.heap_ref().pin_block(ptr);
        PinGuard {
            base_address: super::bump::block_base_of(ptr),
            heap: self.heap,
        }
    }
}

/// Allow allocation in a mutator scope
impl<'guard> ScopedAllocator<'guard> for MutatorHeapView<'guard> {
    /// Allocate and return scoped pointer
    fn alloc<T>(&'guard self, object: T) -> Result<ScopedPtr<'guard, T>, ExecutionError>
    where
        T: StgObject,
    {
        self.heap_ref()
            .alloc(object)
            .map(|p| self.scoped(p))
            .map_err(Into::into)
    }

    /// Allocate and return region of bytes
    fn alloc_bytes(&self, size_bytes: usize) -> Result<std::ptr::NonNull<u8>, ExecutionError> {
        self.heap_ref().alloc_bytes(size_bytes).map_err(Into::into)
    }
}

/// Build an indexed branch table from tagged branch pairs
fn build_branch_table<'guard>(
    mem: &'guard impl ScopedAllocator<'guard>,
    branches: &[(Tag, ScopedPtr<'guard, HeapSyn>)],
) -> (Tag, Array<Option<RefPtr<HeapSyn>>>) {
    if branches.is_empty() {
        return (0, Array::default());
    }
    let min_tag = branches.iter().map(|(t, _)| *t).min().unwrap();
    let max_tag = branches.iter().map(|(t, _)| *t).max().unwrap();
    let table_size = (max_tag - min_tag + 1) as usize;
    let mut table = Array::with_capacity(mem, table_size);
    for _ in 0..table_size {
        table.push(mem, None);
    }
    for (t, p) in branches {
        let index = (*t - min_tag) as usize;
        // SAFETY: index is within [0, table_size) by construction
        unsafe { table.set_unchecked(index, Some(p.as_ptr())) };
    }
    (min_tag, table)
}

/// Provide convencience methods for allocating heap syntax
impl<'guard> StgBuilder<'guard> for MutatorHeapView<'guard> {
    /// Allocate a reference as an atom
    fn atom(&'guard self, r: Ref) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.alloc(HeapSyn::Atom { evaluand: r })
    }

    /// Allocate a function application
    fn app(
        &'guard self,
        r: Ref,
        args: Array<Ref>,
    ) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.alloc(HeapSyn::App { callable: r, args })
    }

    /// Allocate an intrinsic application
    fn app_bif(
        &'guard self,
        index: u8,
        args: Array<Ref>,
    ) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.alloc(HeapSyn::Bif {
            intrinsic: index,
            args,
        })
    }

    /// Allocate a data node
    fn data(
        &'guard self,
        tag: Tag,
        args: Array<Ref>,
    ) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.alloc(HeapSyn::Cons { tag, args })
    }

    fn sym_ref<T: AsRef<str>>(
        &'guard self,
        pool: &mut memory::symbol::SymbolPool,
        s: T,
    ) -> Result<Ref, ExecutionError> {
        let id = pool.intern(s.as_ref());
        Ok(Ref::V(Native::Sym(id)))
    }

    fn str<T: AsRef<str>>(
        &'guard self,
        s: T,
    ) -> Result<ScopedPtr<'guard, memory::string::HeapString>, ExecutionError> {
        self.alloc(HeapString::from_str(self, s.as_ref()))
    }

    fn str_ref<T: AsRef<str>>(&'guard self, s: T) -> Result<Ref, ExecutionError> {
        Ok(Ref::V(Native::Str(self.str(s)?.as_ptr())))
    }

    /// Boolean true
    fn t(&'guard self) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.data(DataConstructor::BoolTrue.tag(), Array::default())
    }

    /// Boolean false
    fn f(&'guard self) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.data(DataConstructor::BoolFalse.tag(), Array::default())
    }

    /// To STG boolean
    fn bool_(&'guard self, b: bool) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        if b {
            self.t()
        } else {
            self.f()
        }
    }

    /// Unit / null
    fn unit(&'guard self) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.data(DataConstructor::Unit.tag(), Array::default())
    }

    /// Empty list
    fn nil(&'guard self) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.data(DataConstructor::ListNil.tag(), Array::default())
    }

    /// List cons
    fn cons(&'guard self, h: Ref, t: Ref) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.data(DataConstructor::ListCons.tag(), self.array(&[h, t]))
    }

    /// Block pair
    fn pair<T: AsRef<str>>(
        &'guard self,
        pool: &mut memory::symbol::SymbolPool,
        k: T,
        v: Ref,
    ) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.data(
            DataConstructor::BlockPair.tag(),
            self.array(&[self.sym_ref(pool, k.as_ref())?, v]),
        )
    }

    /// Block wrapper (list + no-index sentinel)
    fn block(&'guard self, inner: Ref) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        let no_index = Ref::V(Native::Num(serde_json::Number::from(0)));
        self.data(DataConstructor::Block.tag(), self.array(&[inner, no_index]))
    }

    /// Simple let
    fn let_(
        &'guard self,
        bindings: Array<memory::syntax::LambdaForm>,
        body: ScopedPtr<'guard, HeapSyn>,
    ) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.alloc(HeapSyn::Let {
            bindings,
            body: body.as_ptr(),
        })
    }

    /// Recursive let
    fn letrec(
        &'guard self,
        bindings: Array<memory::syntax::LambdaForm>,
        body: ScopedPtr<'guard, HeapSyn>,
    ) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.alloc(HeapSyn::LetRec {
            bindings,
            body: body.as_ptr(),
        })
    }

    /// Case statement, evaluate scrutinee then branch
    fn case(
        &'guard self,
        scrutinee: ScopedPtr<'guard, HeapSyn>,
        branches: &[(Tag, ScopedPtr<'guard, HeapSyn>)],
        fallback: ScopedPtr<'guard, HeapSyn>,
    ) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        let (min_tag, branch_table) = build_branch_table(self, branches);
        self.alloc(HeapSyn::Case {
            scrutinee: scrutinee.as_ptr(),
            min_tag,
            branch_table,
            fallback: Some(fallback.as_ptr()),
            suppress_update: false,
        })
    }

    /// Case statement without default
    fn switch(
        &'guard self,
        scrutinee: ScopedPtr<'guard, HeapSyn>,
        branches: &[(Tag, ScopedPtr<'guard, HeapSyn>)],
    ) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        let (min_tag, branch_table) = build_branch_table(self, branches);
        self.alloc(HeapSyn::Case {
            scrutinee: scrutinee.as_ptr(),
            min_tag,
            branch_table,
            fallback: None,
            suppress_update: false,
        })
    }

    /// Force evaluation of scrutinee then continue
    fn force(
        &'guard self,
        scrutinee: ScopedPtr<'guard, HeapSyn>,
        then: ScopedPtr<'guard, HeapSyn>,
    ) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.case(scrutinee, &[], then)
    }

    /// Add metadata to an expression
    fn with_meta(
        &'guard self,
        meta: Ref,
        body: Ref,
    ) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.alloc(HeapSyn::Meta { meta, body })
    }

    /// Retrieve metadata from an expression (or unit)
    fn demeta(
        &'guard self,
        scrutinee: ScopedPtr<'guard, HeapSyn>,
        handler: ScopedPtr<'guard, HeapSyn>,
        or_else: ScopedPtr<'guard, HeapSyn>,
    ) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.alloc(HeapSyn::DeMeta {
            scrutinee: scrutinee.as_ptr(),
            handler: handler.as_ptr(),
            or_else: or_else.as_ptr(),
        })
    }

    /// Add a source code annotation around an expression
    fn ann(
        &'guard self,
        smid: Smid,
        body: ScopedPtr<'guard, HeapSyn>,
    ) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.alloc(HeapSyn::Ann {
            smid,
            body: body.as_ptr(),
        })
    }
}

impl MutatorScope for MutatorHeapView<'_> {}

/// Implement mutator to get access to the heap as a mutator
pub trait Mutator: Sized {
    type Input;
    type Output;

    fn run(
        &self,
        view: &MutatorHeapView,
        input: Self::Input,
    ) -> Result<Self::Output, ExecutionError>;
}
