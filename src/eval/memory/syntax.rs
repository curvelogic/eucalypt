//! Compiled syntax of the STG machine represented in
//! eucalypt-allocated memory

use crate::common::sourcemap::Smid;
use crate::eval::{error::ExecutionError, stg::tags::Tag};
use chrono::{DateTime, FixedOffset};
use serde_json::Number;
use std::{fmt, ptr::NonNull, rc::Rc};

use super::string::HeapString;
use super::{
    alloc::{ScopedPtr, StgObject},
    array::Array,
};

/// Convert in-heap representation back into language syntax (for
/// display and debug purposes).
pub trait Repr {
    fn repr(&self) -> Rc<crate::eval::stg::syntax::StgSyn>;
}

/// References between allocated objects use RefPtr
pub type RefPtr<T> = NonNull<T>;

/// For now keep enum based primitive storage.
///
/// TODO: this will be replaced by direct storage in the heap with
/// object headers recording type information
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Native {
    /// A symbol
    Sym(RefPtr<HeapString>),
    /// A string
    Str(RefPtr<HeapString>),
    /// A number
    Num(Number),
    /// A zoned datetime
    Zdt(DateTime<FixedOffset>),
}

impl StgObject for Native {}

impl fmt::Display for Native {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self {
            Native::Sym(s) => {
                write!(f, ":<{:p}>", s)
            }
            Native::Str(s) => {
                write!(f, "\"<{:p}>\"", s)
            }
            Native::Num(n) => {
                write!(f, "{}", n)
            }
            Native::Zdt(t) => {
                write!(f, "☽{}", t)
            }
        }
    }
}

/// A reference into environments or a value
///
/// TODO: `V` will become RefPtr into heap and L/G can be encoded
/// using tagged pointer.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Reference<T: Clone> {
    /// Local index into environment
    L(usize),
    /// Global index
    G(usize),
    /// Value (TODO: next this becomes a heap ref)
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
        match &*self {
            Reference::L(i) => {
                write!(f, "✳{}", i)
            }
            Reference::G(i) => {
                write!(f, "⊗{}", i)
            }
            Reference::V(n) => {
                write!(f, "!{}", n)
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

    // /// Create a string
    // pub fn str<T: AsRef<str>>(s: T) -> Ref {
    //     Self::vref(Native::Str(s.as_ref().into()))
    // }

    // /// Create a symbol
    // pub fn sym<T: AsRef<str>>(s: T) -> Ref {
    //     Self::vref(Native::Sym(s.as_ref().into()))
    // }

    /// Create a zoned datetime
    pub fn zdt(dt: DateTime<FixedOffset>) -> Ref {
        Self::vref(Native::Zdt(dt))
    }
}

/// Compiled STG syntax
#[derive(Debug, Clone)]
pub enum HeapSyn {
    /// A single thing - either a reference into env or a native
    Atom { evaluand: Ref },
    /// Case - the only form which actually evaluates
    Case {
        /// Form to be evaluated
        scrutinee: RefPtr<HeapSyn>,
        /// Data type handlers
        branches: Array<(Tag, RefPtr<HeapSyn>)>,
        /// Default handler
        fallback: Option<RefPtr<HeapSyn>>,
    },
    /// Saturated data constructor
    Cons { tag: Tag, args: Array<Ref> },
    /// Function application
    App { callable: Ref, args: Array<Ref> },
    /// Saturated intrinsic application
    Bif { intrinsic: u8, args: Array<Ref> },
    /// Let bindings
    Let {
        bindings: Array<LambdaForm>,
        body: RefPtr<HeapSyn>,
    },
    /// Recursive let bindings -
    LetRec {
        bindings: Array<LambdaForm>,
        body: RefPtr<HeapSyn>,
    },
    /// Call-stack / source location annotation
    Ann { smid: Smid, body: RefPtr<HeapSyn> },
    /// Wrap metadata around an expression
    ///
    /// Transparency: immediately check stack, if its a demeta,
    /// continue else enter body.
    Meta { meta: Ref, body: Ref },
    /// Destructure metadata into a lambda form which receives two
    /// args, meta and body
    DeMeta {
        scrutinee: RefPtr<HeapSyn>,
        handler: RefPtr<HeapSyn>,
        or_else: RefPtr<HeapSyn>,
    },
    /// Blackhole - invalid / uninitialised code
    BlackHole,
}

impl StgObject for HeapSyn {}

impl Default for HeapSyn {
    fn default() -> Self {
        HeapSyn::BlackHole
    }
}

impl HeapSyn {
    /// Used to determine when to create thunks and when not
    pub fn is_whnf(&self) -> bool {
        matches!(
            &*self,
            HeapSyn::Cons { .. }
                | HeapSyn::Meta { .. }
                | HeapSyn::Atom {
                    evaluand: Reference::V(_)
                }
        )
    }
}

/// The code form of a lambda - which becomes a pure StgSyn against an
/// environment when "allocated" in an environment frame.
///
/// When "allocated" as an environment value, the bound references
/// become refs into the top environment frame which represents
/// args and the free references become refs that point deeper
/// into the environment stack
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LambdaForm {
    Lambda {
        bound: u8,
        body: RefPtr<HeapSyn>,
        annotation: Smid,
    },
    Thunk {
        body: RefPtr<HeapSyn>,
    },
    Value {
        body: RefPtr<HeapSyn>,
    },
}

impl LambdaForm {
    /// Create new lambda form - local vars < `bound` are bound vars.
    pub fn new(bound: u8, body: RefPtr<HeapSyn>, annotation: Smid) -> Self {
        LambdaForm::Lambda {
            bound,
            body,
            annotation,
        }
    }

    /// A lambda form that will be updated after evaluation
    pub fn thunk(body: RefPtr<HeapSyn>) -> Self {
        LambdaForm::Thunk { body }
    }

    /// A lambda form that is effectively a value - not worth updating
    pub fn value(body: RefPtr<HeapSyn>) -> Self {
        LambdaForm::Value { body }
    }

    /// Reference the body of the lambda form
    pub fn body(&self) -> RefPtr<HeapSyn> {
        match *self {
            LambdaForm::Lambda { ref body, .. }
            | LambdaForm::Thunk { ref body, .. }
            | LambdaForm::Value { ref body } => *body,
        }
    }

    /// Source annotation to stamp on environment
    pub fn annotation(&self) -> Smid {
        match *self {
            LambdaForm::Lambda { annotation, .. } => annotation,
            _ => Smid::default(),
        }
    }

    /// The arity of the the lambda form
    pub fn arity(&self) -> u8 {
        match *self {
            LambdaForm::Lambda { ref bound, .. } => *bound,
            _ => 0,
        }
    }

    /// Whether lambda form is a thunk to be updated in place
    pub fn update(&self) -> bool {
        matches!(*self, LambdaForm::Thunk { .. })
    }
}

/// Support for representing in-heap code as STG language syntax for
/// debugging / display
pub mod repr {

    use std::rc::Rc;

    use crate::eval::{
        memory::{
            self,
            alloc::{MutatorScope, ScopedPtr},
            array::Array,
        },
        stg::{self, syntax::StgSyn},
    };

    use super::{HeapSyn, RefPtr, Repr};

    /// Convert heap representation of reference to syntax representation
    pub fn heap_to_stg(guard: &dyn MutatorScope, r: &memory::syntax::Ref) -> stg::syntax::Ref {
        match r {
            memory::syntax::Ref::L(n) => stg::syntax::Ref::L(*n),
            memory::syntax::Ref::G(n) => stg::syntax::Ref::G(*n),
            memory::syntax::Ref::V(memory::syntax::Native::Sym(s)) => {
                let ptr = ScopedPtr::from_non_null(guard, *s);
                stg::syntax::Ref::V(stg::syntax::Native::Sym((*ptr).as_str().to_string()))
            }
            memory::syntax::Ref::V(memory::syntax::Native::Str(s)) => {
                let ptr = ScopedPtr::from_non_null(guard, *s);
                stg::syntax::Ref::V(stg::syntax::Native::Str((*ptr).as_str().to_string()))
            }
            memory::syntax::Ref::V(memory::syntax::Native::Num(n)) => {
                stg::syntax::Ref::V(stg::syntax::Native::Num(n.clone()))
            }
            memory::syntax::Ref::V(memory::syntax::Native::Zdt(d)) => {
                stg::syntax::Ref::V(stg::syntax::Native::Zdt(*d))
            }
        }
    }

    /// Represent in-heap branch table in syntax form
    pub fn repr_branches(
        guard: &dyn MutatorScope,
        branches: Array<(u8, RefPtr<HeapSyn>)>,
    ) -> Vec<(u8, Rc<StgSyn>)> {
        branches
            .iter()
            .map(|(t, ptr)| (*t, ScopedPtr::from_non_null(guard, *ptr).repr()))
            .collect()
    }

    /// Represent in-heap ref vector in syntax form
    pub fn repr_refarray(
        guard: &dyn MutatorScope,
        refs: Array<memory::syntax::Ref>,
    ) -> Vec<stg::syntax::Ref> {
        refs.iter().map(|r| heap_to_stg(guard, r)).collect()
    }

    /// Represent in-heap bindingn vector in syntax form
    pub fn repr_bindings(
        guard: &dyn MutatorScope,
        bindings: Array<memory::syntax::LambdaForm>,
    ) -> Vec<stg::syntax::LambdaForm> {
        let mut v = Vec::with_capacity(bindings.len());
        for pc in bindings.iter() {
            let binding = match pc {
                memory::syntax::LambdaForm::Lambda {
                    bound,
                    body,
                    annotation,
                } => stg::syntax::LambdaForm::Lambda {
                    bound: *bound,
                    body: ScopedPtr::from_non_null(guard, *body).repr(),
                    annotation: *annotation,
                },
                memory::syntax::LambdaForm::Thunk { body } => stg::syntax::LambdaForm::Thunk {
                    body: ScopedPtr::from_non_null(guard, *body).repr(),
                },
                memory::syntax::LambdaForm::Value { body } => stg::syntax::LambdaForm::Value {
                    body: ScopedPtr::from_non_null(guard, *body).repr(),
                },
            };
            v.push(binding);
        }
        v
    }
}

/// Convert in-heap HeapSyn back to StgSyn for debugging
impl<'guard> Repr for ScopedPtr<'guard, HeapSyn> {
    fn repr(&self) -> Rc<crate::eval::stg::syntax::StgSyn> {
        use crate::eval::stg::syntax::StgSyn;

        match &**self {
            HeapSyn::Atom { evaluand } => Rc::new(StgSyn::Atom {
                evaluand: repr::heap_to_stg(self, evaluand),
            }),
            HeapSyn::Case {
                scrutinee,
                branches,
                fallback,
            } => Rc::new(StgSyn::Case {
                scrutinee: ScopedPtr::from_non_null(self, *scrutinee).repr(),
                branches: repr::repr_branches(self, branches.clone()),
                fallback: fallback
                    .as_ref()
                    .map(|f| ScopedPtr::from_non_null(self, *f).repr()),
            }),
            HeapSyn::Cons { tag, args } => Rc::new(StgSyn::Cons {
                tag: *tag,
                args: repr::repr_refarray(self, args.clone()),
            }),
            HeapSyn::App { callable, args } => Rc::new(StgSyn::App {
                callable: repr::heap_to_stg(self, callable),
                args: repr::repr_refarray(self, args.clone()),
            }),
            HeapSyn::Bif { intrinsic, args } => Rc::new(StgSyn::Bif {
                intrinsic: *intrinsic,
                args: repr::repr_refarray(self, args.clone()),
            }),
            HeapSyn::Let { bindings, body } => Rc::new(StgSyn::Let {
                bindings: repr::repr_bindings(self, bindings.clone()),
                body: ScopedPtr::from_non_null(self, *body).repr(),
            }),
            HeapSyn::LetRec { bindings, body } => Rc::new(StgSyn::LetRec {
                bindings: repr::repr_bindings(self, bindings.clone()),
                body: ScopedPtr::from_non_null(self, *body).repr(),
            }),
            HeapSyn::Ann { smid, body } => Rc::new(StgSyn::Ann {
                smid: *smid,
                body: ScopedPtr::from_non_null(self, *body).repr(),
            }),
            HeapSyn::Meta { meta, body } => Rc::new(StgSyn::Meta {
                meta: repr::heap_to_stg(self, &*meta),
                body: repr::heap_to_stg(self, &*body),
            }),
            HeapSyn::DeMeta {
                scrutinee,
                handler,
                or_else,
            } => Rc::new(StgSyn::DeMeta {
                scrutinee: ScopedPtr::from_non_null(self, *scrutinee).repr(),
                handler: ScopedPtr::from_non_null(self, *handler).repr(),
                or_else: ScopedPtr::from_non_null(self, *or_else).repr(),
            }),
            HeapSyn::BlackHole => Rc::new(StgSyn::BlackHole {}),
        }
    }
}

impl<'guard> fmt::Display for ScopedPtr<'guard, HeapSyn> {
    /// Defer to STG syntax implementation
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.repr().fmt(f)
    }
}

/// A reference to the allocator and the means to express STG
pub trait StgBuilder<'scope> {
    /// Allocate a reference as an atom
    fn atom(&'scope self, r: Ref) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Allocate a function application
    fn app(
        &'scope self,
        r: Ref,
        args: Array<Ref>,
    ) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Allocate an intrinsic application
    fn app_bif(
        &'scope self,
        index: u8,
        args: Array<Ref>,
    ) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Allocate a data node
    fn data(
        &'scope self,
        tag: Tag,
        args: Array<Ref>,
    ) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Allocate a symbol in the heap
    fn sym<T: AsRef<str>>(
        &'scope self,
        s: T,
    ) -> Result<ScopedPtr<'scope, HeapString>, ExecutionError>;

    /// Allocate a symbol in the heap and wrap as ref
    fn sym_ref<T: AsRef<str>>(&'scope self, s: T) -> Result<Ref, ExecutionError>;

    /// Allocate a string in the heap
    fn str<T: AsRef<str>>(
        &'scope self,
        s: T,
    ) -> Result<ScopedPtr<'scope, HeapString>, ExecutionError>;

    /// Allocate a string in the heap and wrap as ref
    fn str_ref<T: AsRef<str>>(&'scope self, s: T) -> Result<Ref, ExecutionError>;

    // /// Allocate a local ref in an atom
    // fn local(&'scope self, index: usize) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    // /// Allocate a global ref in an atom
    // fn global(&'scope self, index: usize) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    // /// A boxed number
    // fn box_num<N>(&'scope self, n: N) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>
    // where
    //     N: Into<Number>;
    // /// Create a string
    // fn box_str<T: AsRef<str>>(
    //     &'scope self,
    //     s: T,
    // ) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    // /// Create a symbol
    // fn box_sym<T: AsRef<str>>(
    //     &'scope self,
    //     s: T,
    // ) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    // /// Create a boxed zoned datetime
    // fn box_zdt(
    //     &'scope self,
    //     dt: DateTime<FixedOffset>,
    // ) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Boolean true
    fn t(&'scope self) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Boolean false
    fn f(&'scope self) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// To STG boolean
    fn bool_(&'scope self, b: bool) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Unit / null
    fn unit(&'scope self) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Empty list
    fn nil(&'scope self) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// List cons
    fn cons(&'scope self, h: Ref, t: Ref) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Block pair
    fn pair<T: AsRef<str>>(
        &'scope self,
        k: T,
        v: Ref,
    ) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Block wrapper
    fn block(&'scope self, inner: Ref) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    // /// A lambda form
    // fn lambda(&'scope self, bound: u8, body: ScopedPtr<'scope, HeapSyn>) -> LambdaForm;

    // /// An annotated lambda form
    // fn annotated_lambda(
    //     &'scope self,
    //     bound: u8,
    //     body: ScopedPtr<'scope, HeapSyn>,
    //     annotation: Smid,
    // ) -> LambdaForm;

    // /// A thunk lambda form
    // fn thunk(&'scope self, body: ScopedPtr<'scope, HeapSyn>) -> LambdaForm;

    // /// A value lambda form
    // fn value(&'scope self, body: ScopedPtr<'scope, HeapSyn>) -> LambdaForm;

    /// Simple let
    fn let_(
        &'scope self,
        bindings: Array<LambdaForm>,
        body: ScopedPtr<'scope, HeapSyn>,
    ) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Recursive let
    fn letrec(
        &'scope self,
        bindings: Array<LambdaForm>,
        body: ScopedPtr<'scope, HeapSyn>,
    ) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Case statement, evaluate scrutinee then branch
    fn case(
        &'scope self,
        scrutinee: ScopedPtr<'scope, HeapSyn>,
        branches: &[(Tag, ScopedPtr<'scope, HeapSyn>)],
        fallback: ScopedPtr<'scope, HeapSyn>,
    ) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Case statement without default
    fn switch(
        &'scope self,
        scrutinee: ScopedPtr<'scope, HeapSyn>,
        branches: &[(Tag, ScopedPtr<'scope, HeapSyn>)],
    ) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Force evaluation of scrutinee then continue
    fn force(
        &'scope self,
        scrutinee: ScopedPtr<'scope, HeapSyn>,
        then: ScopedPtr<'scope, HeapSyn>,
    ) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Add metadata to an expression
    fn with_meta(
        &'scope self,
        meta: Ref,
        body: Ref,
    ) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Retrieve metadata from an expression (or unit)
    fn demeta(
        &'scope self,
        scrutinee: ScopedPtr<'scope, HeapSyn>,
        handler: ScopedPtr<'scope, HeapSyn>,
        or_else: ScopedPtr<'scope, HeapSyn>,
    ) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;

    /// Add a source code annotation around an expression
    fn ann(
        &'scope self,
        smid: Smid,
        body: ScopedPtr<'scope, HeapSyn>,
    ) -> Result<ScopedPtr<'scope, HeapSyn>, ExecutionError>;
}

#[cfg(test)]
pub mod tests {

    use crate::eval::memory::{heap::Heap, mutator::MutatorHeapView};

    use super::*;

    #[test]
    pub fn test_atom() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);

        view.atom(Ref::lref(4)).unwrap();
        view.app(Ref::lref(1), view.array(&[Ref::lref(2)])).unwrap();
        view.app_bif(77, view.array(&[Ref::lref(2)])).unwrap();
        // dsl.local(2);
        // dsl.global(3);
        // dsl.box_num(99);
        // dsl.box_str("hello");
        // dsl.box_sym("foo");
        view.t().unwrap();
        view.f().unwrap();
        view.bool_(false).unwrap();
        view.nil().unwrap();
        let id = LambdaForm::new(1, view.atom(Ref::L(0)).unwrap().as_ptr(), Smid::default());
        view.let_(
            view.singleton(id.clone()),
            view.app(Ref::L(0), view.singleton(view.sym_ref("foo").unwrap()))
                .unwrap(),
        )
        .unwrap();
        view.letrec(
            view.singleton(id.clone()),
            view.app(Ref::L(0), view.singleton(view.sym_ref("foo").unwrap()))
                .unwrap(),
        )
        .unwrap();
    }
}
