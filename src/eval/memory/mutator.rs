//! Support mutator access to heap and machine

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

/// A view onto the heap for code that needs mutator access (as
/// opposed to collector access)
///
/// MutatorHeapView provides a scope for dereferencing heap pointers,
/// the means of allocation and convenience constructors for
/// allocating in-heap syntax
#[derive(Copy, Clone)]
pub struct MutatorHeapView<'guard> {
    heap: &'guard Heap,
}

impl<'guard> MutatorHeapView<'guard> {
    pub fn new(heap: &'guard Heap) -> Self {
        MutatorHeapView { heap }
    }

    /// Obtain a scoped pointer from a RefPtr for dereferencing
    pub fn scoped<T: Sized>(self, ptr: RefPtr<T>) -> ScopedPtr<'guard, T> {
        ScopedPtr::from_non_null(self.heap, ptr)
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
}

/// Allow allocation in a mutator scope
impl<'guard> ScopedAllocator<'guard> for MutatorHeapView<'guard> {
    /// Allocate and return scoped pointer
    fn alloc<T>(&'guard self, object: T) -> Result<ScopedPtr<'guard, T>, ExecutionError>
    where
        T: StgObject,
    {
        self.heap
            .alloc(object)
            .map(|p| self.scoped(p))
            .map_err(Into::into)
    }

    /// Allocate and return region of bytes
    fn alloc_bytes(&self, size_bytes: usize) -> Result<std::ptr::NonNull<u8>, ExecutionError> {
        self.heap.alloc_bytes(size_bytes).map_err(Into::into)
    }
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

    // /// Allocate a local ref in an atom
    // fn local(&'guard self, index: usize) -> ScopedPtr<'guard, HeapSyn> {
    //     self.atom(Ref::lref(index))
    // }

    // /// Allocate a global ref in an atom
    // fn global(&'guard self, index: usize) -> ScopedPtr<'guard, HeapSyn> {
    //     self.atom(Ref::gref(index))
    // }

    // /// A boxed number
    // fn box_num<N>(&'guard self, n: N) -> ScopedPtr<'guard, HeapSyn>
    // where
    //     N: Into<Number>,
    // {
    //     self.data(
    //         DataConstructor::BoxedNumber.tag(),
    //         self.singleton(Ref::num(n)),
    //     )
    // }

    // /// Create a string
    // fn box_str<T: AsRef<str>>(&'guard self, s: T) -> ScopedPtr<'guard, HeapSyn> {
    //     self.data(
    //         DataConstructor::BoxedString.tag(),
    //         self.singleton(Ref::str(s)),
    //     )
    // }

    // /// Create a symbol
    // fn box_sym<T: AsRef<str>>(&'guard self, s: T) -> ScopedPtr<'guard, HeapSyn> {
    //     self.data(
    //         DataConstructor::BoxedSymbol.tag(),
    //         self.singleton(Ref::sym(s)),
    //     )
    // }

    // /// Create a boxed zoned datetime
    // fn box_zdt(&'guard self, dt: DateTime<FixedOffset>) -> ScopedPtr<'guard, HeapSyn> {
    //     self.data(
    //         DataConstructor::BoxedZdt.tag(),
    //         self.singleton(Ref::zdt(dt)),
    //     )
    // }

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
        k: T,
        v: Ref,
    ) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.data(
            DataConstructor::BlockPair.tag(),
            self.array(&[Ref::sym(k.as_ref()), v]),
        )
    }

    /// Block wrapper
    fn block(&'guard self, inner: Ref) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        self.data(DataConstructor::Block.tag(), self.singleton(inner))
    }

    // /// A lambda form
    // fn lambda(&'guard self, bound: u8, body: ScopedPtr<'guard, HeapSyn>) -> LambdaForm {
    //     LambdaForm::new(bound, body.as_ptr(), Smid::default())
    // }

    // /// An annotated lambda form
    // fn annotated_lambda(
    //     &'guard self,
    //     bound: u8,
    //     body: ScopedPtr<'guard, HeapSyn>,
    //     annotation: Smid,
    // ) -> LambdaForm {
    //     LambdaForm::new(bound, body.as_ptr(), annotation)
    // }

    // /// A thunk lambda form
    // fn thunk(&'guard self, body: ScopedPtr<'guard, HeapSyn>) -> LambdaForm {
    //     LambdaForm::thunk(body.as_ptr())
    // }

    // /// A value lambda form
    // fn value(&'guard self, body: ScopedPtr<'guard, HeapSyn>) -> LambdaForm {
    //     LambdaForm::value(body.as_ptr())
    // }

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
        let mut array = Array::with_capacity(self, branches.len());
        for (t, p) in branches {
            array.push(self, (*t, p.as_ptr()));
        }
        self.alloc(HeapSyn::Case {
            scrutinee: scrutinee.as_ptr(),
            branches: array,
            fallback: Some(fallback.as_ptr()),
        })
    }

    /// Case statement without default
    fn switch(
        &'guard self,
        scrutinee: ScopedPtr<'guard, HeapSyn>,
        branches: &[(Tag, ScopedPtr<'guard, HeapSyn>)],
    ) -> Result<ScopedPtr<'guard, HeapSyn>, ExecutionError> {
        let mut array = Array::with_capacity(self, branches.len());
        for (t, p) in branches {
            array.push(self, (*t, p.as_ptr()));
        }
        self.alloc(HeapSyn::Case {
            scrutinee: scrutinee.as_ptr(),
            branches: array,
            fallback: None,
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

impl<'guard> MutatorScope for MutatorHeapView<'guard> {}

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
