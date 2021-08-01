//! Compiled syntax of the STG machine represented in
//! eucalypt-allocated memory

use crate::eval::stg::tags::Tag;
use crate::{common::sourcemap::Smid, eval::stg::tags::DataConstructor};
use chrono::{DateTime, FixedOffset};
use serde_json::Number;
use std::{fmt, ptr::NonNull};

use super::{
    alloc::{Allocator, MutatorScope, ScopedPtr, StgObject},
    array::Array,
};

/// References between allocated objects use RefPtr
pub type RefPtr<T> = NonNull<T>;

/// For now keep enum based primitive storage.
///
/// TODO: this will be replaced by direct storage in the heap with
/// object headers recording type information
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Native {
    /// A symbol
    Sym(String),
    /// A string
    Str(String),
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
                write!(f, ":{}", s)
            }
            Native::Str(s) => {
                write!(f, "\"{}\"", s)
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

    /// Create a string
    pub fn str<T: AsRef<str>>(s: T) -> Ref {
        Self::vref(Native::Str(s.as_ref().into()))
    }

    /// Create a symbol
    pub fn sym<T: AsRef<str>>(s: T) -> Ref {
        Self::vref(Native::Sym(s.as_ref().into()))
    }

    /// Create a zoned datetime
    pub fn zdt(dt: DateTime<FixedOffset>) -> Ref {
        Self::vref(Native::Zdt(dt))
    }
}

/// Compiled STG syntax
#[derive(Debug)]
pub enum StgSyn {
    /// A single thing - either a reference into env or a native
    Atom { evaluand: Ref },
    /// Case - the only form which actually evaluates
    Case {
        /// Form to be evaluated
        scrutinee: RefPtr<StgSyn>,
        /// Data type handlers
        branches: Array<(Tag, RefPtr<StgSyn>)>,
        /// Default handler
        fallback: Option<RefPtr<StgSyn>>,
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
        body: RefPtr<StgSyn>,
    },
    /// Recursive let bindings -
    LetRec {
        bindings: Array<LambdaForm>,
        body: RefPtr<StgSyn>,
    },
    /// Call-stack / source location annotation
    Ann { smid: Smid, body: RefPtr<StgSyn> },
    /// Wrap metadata around an expression
    ///
    /// Transparency: immediately check stack, if its a demeta,
    /// continue else enter body.
    Meta { meta: Ref, body: Ref },
    /// Destructure metadata into a lambda form which receives two
    /// args, meta and body
    DeMeta {
        scrutinee: RefPtr<StgSyn>,
        handler: RefPtr<StgSyn>,
        or_else: RefPtr<StgSyn>,
    },
    /// Machine instruction
    Pragma {},
    /// Blackhole - invalid / uninitialised code
    BlackHole,
}

impl StgObject for StgSyn {}

impl Default for StgSyn {
    fn default() -> Self {
        StgSyn::BlackHole
    }
}

impl StgSyn {
    /// Used to determine when to create thunks and when not
    pub fn is_whnf(&self) -> bool {
        matches!(
            &*self,
            StgSyn::Cons { .. }
                | StgSyn::Meta { .. }
                | StgSyn::Atom {
                    evaluand: Reference::V(_)
                }
        )
    }
}

/// Provides access to memory for displaying allocated STG syntax
pub struct StgSynDisplayer<'scope> {
    syntax: &'scope StgSyn,
    view: &'scope dyn MutatorScope,
}

impl<'scope> StgSynDisplayer<'scope> {
    pub fn scoped<T>(&self, ptr: &RefPtr<T>) -> ScopedPtr<'scope, T> {
        ScopedPtr::new(self.view, unsafe { &*ptr.as_ptr() })
    }

    pub fn syn(&'scope self, synptr: &'scope StgSyn) -> Self {
        StgSynDisplayer {
            syntax: &*synptr,
            view: self.view,
        }
    }
}

impl<'scope> fmt::Display for StgSynDisplayer<'scope> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.syntax {
            StgSyn::Atom { evaluand } => {
                write!(f, "{}", evaluand)
            }
            StgSyn::Case {
                scrutinee,
                branches,
                fallback,
            } => {
                let mut tags: Vec<String> = branches.iter().map(|b| format!("{}", b.0)).collect();
                if fallback.is_some() {
                    tags.push("…".to_string());
                }
                let desc = &tags.join(",");
                write!(f, "CASE({}⑂<{}>)", self.syn(&*self.scoped(scrutinee)), desc)
            }
            StgSyn::Cons { tag, args } => {
                write!(f, "DATA[{}](×{})", tag, args.len())
            }
            StgSyn::App { callable, args } => {
                write!(f, "{}(×{})", callable, args.len())
            }
            StgSyn::Bif { intrinsic, args } => {
                write!(f, "BIF[{}](×{})", intrinsic, args.len())
            }
            StgSyn::Let { bindings, body } => {
                write!(
                    f,
                    "LET[×{}]({})",
                    bindings.len(),
                    self.syn(&*self.scoped(body))
                )
            }
            StgSyn::LetRec { bindings, body } => {
                write!(
                    f,
                    "LETREC[×{}]({})",
                    bindings.len(),
                    self.syn(&*self.scoped(body))
                )
            }
            StgSyn::Ann { smid, body } => {
                write!(f, "♪{}:{}", smid, self.syn(&*self.scoped(body)))
            }
            StgSyn::Meta { meta, body } => {
                write!(f, "`{}{}", meta, body)
            }
            StgSyn::DeMeta { .. } => {
                write!(f, "ƒ(`,•)")
            }
            StgSyn::Pragma {} => {
                write!(f, "PRAG")
            }
            StgSyn::BlackHole => {
                write!(f, "⊙")
            }
        }
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
        body: RefPtr<StgSyn>,
        annotation: Smid,
    },
    Thunk {
        body: RefPtr<StgSyn>,
    },
    Value {
        body: RefPtr<StgSyn>,
    },
}

impl LambdaForm {
    /// Create new lambda form - local vars < `bound` are bound vars.
    pub fn new(bound: u8, body: RefPtr<StgSyn>, annotation: Smid) -> Self {
        LambdaForm::Lambda {
            bound,
            body,
            annotation,
        }
    }

    /// A lambda form that will be updated after evaluation
    pub fn thunk(body: RefPtr<StgSyn>) -> Self {
        LambdaForm::Thunk { body }
    }

    /// A lambda form that is effectively a value - not worth updating
    pub fn value(body: RefPtr<StgSyn>) -> Self {
        LambdaForm::Value { body }
    }

    /// Reference the body of the lambda form
    pub fn body(&self) -> &RefPtr<StgSyn> {
        match *self {
            LambdaForm::Lambda { ref body, .. }
            | LambdaForm::Thunk { ref body, .. }
            | LambdaForm::Value { ref body } => &body,
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

/// A reference to the allocator and the means to express STG
pub struct StgDsl<'scope, A: Allocator> {
    heap: &'scope A,
}

impl<'scope, A: Allocator> MutatorScope for StgDsl<'scope, A> {}

impl<'scope, A: Allocator> StgDsl<'scope, A> {
    /// Wrap an allocator in a StgDsl for scoped access to allocate
    /// new expressions
    pub fn new(heap: &'scope A) -> Self {
        StgDsl { heap }
    }

    /// Allocate and return a scoped pointer
    pub fn alloc_scoped<T: StgObject>(&'scope self, object: T) -> ScopedPtr<'scope, T> {
        let ptr = self.heap.alloc(object).expect("allocation error");
        ScopedPtr::new(self, unsafe { &*ptr.as_ptr() })
    }

    /// Allocate a single item array
    pub fn singleton<T: Sized + Clone>(&'scope self, object: T) -> Array<T> {
        let mut array = Array::with_capacity(self.heap, 1);
        array.push(self.heap, object);
        array
    }

    pub fn array<T: Sized + Clone>(&'scope self, data: &[T]) -> Array<T> {
        Array::from_slice(self.heap, data)
    }

    /// Allocate a reference as an atom
    pub fn atom(&'scope self, r: Ref) -> ScopedPtr<'scope, StgSyn> {
        self.alloc_scoped(StgSyn::Atom { evaluand: r })
    }

    /// Allocate a function application
    pub fn app(&'scope self, r: Ref, args: Array<Ref>) -> ScopedPtr<'scope, StgSyn> {
        self.alloc_scoped(StgSyn::App { callable: r, args })
    }

    /// Allocate an intrinsic application
    pub fn app_bif(&'scope self, index: u8, args: Array<Ref>) -> ScopedPtr<'scope, StgSyn> {
        self.alloc_scoped(StgSyn::Bif {
            intrinsic: index,
            args,
        })
    }

    /// Allocate a data node
    pub fn data(&'scope self, tag: Tag, args: Array<Ref>) -> ScopedPtr<'scope, StgSyn> {
        self.alloc_scoped(StgSyn::Cons { tag, args })
    }

    /// Allocate a local ref in an atom
    pub fn local(&'scope self, index: usize) -> ScopedPtr<'scope, StgSyn> {
        self.atom(Ref::lref(index))
    }

    /// Allocate a global ref in an atom
    pub fn global(&'scope self, index: usize) -> ScopedPtr<'scope, StgSyn> {
        self.atom(Ref::gref(index))
    }

    /// A boxed number
    pub fn box_num<N>(&'scope self, n: N) -> ScopedPtr<'scope, StgSyn>
    where
        N: Into<Number>,
    {
        self.data(
            DataConstructor::BoxedNumber.tag(),
            self.singleton(Ref::num(n)),
        )
    }

    /// Create a string
    pub fn box_str<T: AsRef<str>>(&'scope self, s: T) -> ScopedPtr<'scope, StgSyn> {
        self.data(
            DataConstructor::BoxedString.tag(),
            self.singleton(Ref::str(s)),
        )
    }

    /// Create a symbol
    pub fn box_sym<T: AsRef<str>>(&'scope self, s: T) -> ScopedPtr<'scope, StgSyn> {
        self.data(
            DataConstructor::BoxedSymbol.tag(),
            self.singleton(Ref::sym(s)),
        )
    }

    /// Create a boxed zoned datetime
    pub fn box_zdt(&'scope self, dt: DateTime<FixedOffset>) -> ScopedPtr<'scope, StgSyn> {
        self.data(
            DataConstructor::BoxedZdt.tag(),
            self.singleton(Ref::zdt(dt)),
        )
    }

    /// Boolean true
    pub fn t(&'scope self) -> ScopedPtr<'scope, StgSyn> {
        self.data(DataConstructor::BoolTrue.tag(), Array::default())
    }

    /// Boolean false
    pub fn f(&'scope self) -> ScopedPtr<'scope, StgSyn> {
        self.data(DataConstructor::BoolFalse.tag(), Array::default())
    }

    /// To STG boolean
    pub fn bool_(&'scope self, b: bool) -> ScopedPtr<'scope, StgSyn> {
        if b {
            self.t()
        } else {
            self.f()
        }
    }

    /// Unit / null
    pub fn unit(&'scope self) -> ScopedPtr<'scope, StgSyn> {
        self.data(DataConstructor::Unit.tag(), Array::default())
    }

    /// Empty list
    pub fn nil(&'scope self) -> ScopedPtr<'scope, StgSyn> {
        self.data(DataConstructor::ListNil.tag(), Array::default())
    }

    /// List cons
    pub fn cons(&'scope self, h: Ref, t: Ref) -> ScopedPtr<'scope, StgSyn> {
        self.data(DataConstructor::ListCons.tag(), self.array(&[h, t]))
    }

    /// Block pair
    pub fn pair<T: AsRef<str>>(&'scope self, k: T, v: Ref) -> ScopedPtr<'scope, StgSyn> {
        self.data(
            DataConstructor::BlockPair.tag(),
            self.array(&[Ref::sym(k.as_ref()), v]),
        )
    }

    /// Block wrapper
    pub fn block(&'scope self, inner: Ref) -> ScopedPtr<'scope, StgSyn> {
        self.data(DataConstructor::Block.tag(), self.singleton(inner))
    }

    /// A lambda form
    pub fn lambda(&'scope self, bound: u8, body: ScopedPtr<'scope, StgSyn>) -> LambdaForm {
        LambdaForm::new(bound, body.as_ptr(), Smid::default())
    }

    /// An annotated lambda form
    pub fn annotated_lambda(
        &'scope self,
        bound: u8,
        body: ScopedPtr<'scope, StgSyn>,
        annotation: Smid,
    ) -> LambdaForm {
        LambdaForm::new(bound, body.as_ptr(), annotation)
    }

    /// A thunk lambda form
    pub fn thunk(&'scope self, body: ScopedPtr<'scope, StgSyn>) -> LambdaForm {
        LambdaForm::thunk(body.as_ptr())
    }

    /// A value lambda form
    pub fn value(&'scope self, body: ScopedPtr<'scope, StgSyn>) -> LambdaForm {
        LambdaForm::value(body.as_ptr())
    }

    /// Simple let
    pub fn let_(
        &'scope self,
        bindings: Array<LambdaForm>,
        body: ScopedPtr<'scope, StgSyn>,
    ) -> ScopedPtr<'scope, StgSyn> {
        self.alloc_scoped(StgSyn::Let {
            bindings,
            body: body.as_ptr(),
        })
    }

    /// Recursive let
    pub fn letrec_(
        &'scope self,
        bindings: Array<LambdaForm>,
        body: ScopedPtr<'scope, StgSyn>,
    ) -> ScopedPtr<'scope, StgSyn> {
        self.alloc_scoped(StgSyn::LetRec {
            bindings,
            body: body.as_ptr(),
        })
    }

    /// Case statement, evaluate scrutinee then branch
    pub fn case(
        &'scope self,
        scrutinee: ScopedPtr<'scope, StgSyn>,
        branches: &[(Tag, ScopedPtr<'scope, StgSyn>)],
        fallback: ScopedPtr<'scope, StgSyn>,
    ) -> ScopedPtr<'scope, StgSyn> {
        let mut array = Array::with_capacity(self.heap, branches.len());
        for (t, p) in branches {
            array.push(self.heap, (*t, p.as_ptr()));
        }
        self.alloc_scoped(StgSyn::Case {
            scrutinee: scrutinee.as_ptr(),
            branches: array,
            fallback: Some(fallback.as_ptr()),
        })
    }

    /// Case statement without default
    pub fn switch(
        &'scope self,
        scrutinee: ScopedPtr<'scope, StgSyn>,
        branches: &[(Tag, ScopedPtr<'scope, StgSyn>)],
    ) -> ScopedPtr<'scope, StgSyn> {
        let mut array = Array::with_capacity(self.heap, branches.len());
        for (t, p) in branches {
            array.push(self.heap, (*t, p.as_ptr()));
        }
        self.alloc_scoped(StgSyn::Case {
            scrutinee: scrutinee.as_ptr(),
            branches: array,
            fallback: None,
        })
    }

    /// Force evaluation of scrutinee then continue
    pub fn force(
        &'scope self,
        scrutinee: ScopedPtr<'scope, StgSyn>,
        then: ScopedPtr<'scope, StgSyn>,
    ) -> ScopedPtr<'scope, StgSyn> {
        self.case(scrutinee, &[], then)
    }

    /// Add metadata to an expression
    pub fn with_meta(&'scope self, meta: Ref, body: Ref) -> ScopedPtr<'scope, StgSyn> {
        self.alloc_scoped(StgSyn::Meta { meta, body })
    }

    /// Retrieve metadata from an expression (or unit)
    pub fn demeta(
        &'scope self,
        scrutinee: ScopedPtr<'scope, StgSyn>,
        handler: ScopedPtr<'scope, StgSyn>,
        or_else: ScopedPtr<'scope, StgSyn>,
    ) -> ScopedPtr<'scope, StgSyn> {
        self.alloc_scoped(StgSyn::DeMeta {
            scrutinee: scrutinee.as_ptr(),
            handler: handler.as_ptr(),
            or_else: or_else.as_ptr(),
        })
    }

    /// Add a source code annotation around an expression
    pub fn ann(
        &'scope self,
        smid: Smid,
        body: ScopedPtr<'scope, StgSyn>,
    ) -> ScopedPtr<'scope, StgSyn> {
        self.alloc_scoped(StgSyn::Ann {
            smid,
            body: body.as_ptr(),
        })
    }

    /// Unbox a number
    pub fn unbox_num(
        &'scope self,
        scrutinee: ScopedPtr<'scope, StgSyn>,
        then: ScopedPtr<'scope, StgSyn>,
    ) -> ScopedPtr<'scope, StgSyn> {
        self.switch(scrutinee, &[(DataConstructor::BoxedNumber.tag(), then)])
    }

    /// Unbox a string
    pub fn unbox_str(
        &'scope self,
        scrutinee: ScopedPtr<'scope, StgSyn>,
        then: ScopedPtr<'scope, StgSyn>,
    ) -> ScopedPtr<'scope, StgSyn> {
        self.switch(scrutinee, &[(DataConstructor::BoxedString.tag(), then)])
    }

    /// Unbox a symbol
    pub fn unbox_sym(
        &'scope self,
        scrutinee: ScopedPtr<'scope, StgSyn>,
        then: ScopedPtr<'scope, StgSyn>,
    ) -> ScopedPtr<'scope, StgSyn> {
        self.switch(scrutinee, &[(DataConstructor::BoxedSymbol.tag(), then)])
    }

    /// Unbox a symbol
    pub fn unbox_zdt(
        &'scope self,
        scrutinee: ScopedPtr<'scope, StgSyn>,
        then: ScopedPtr<'scope, StgSyn>,
    ) -> ScopedPtr<'scope, StgSyn> {
        self.switch(scrutinee, &[(DataConstructor::BoxedZdt.tag(), then)])
    }
}

#[cfg(test)]
pub mod tests {

    use crate::eval::memory::heap::Heap;

    use super::*;

    #[test]
    pub fn test_atom() {
        let heap = Heap::new();
        let dsl = StgDsl::new(&heap);

        dsl.atom(Ref::lref(4));
        dsl.app(Ref::lref(1), dsl.array(&[Ref::lref(2)]));
        dsl.app_bif(77, dsl.array(&[Ref::lref(2)]));
        dsl.local(2);
        dsl.global(3);
        dsl.box_num(99);
        dsl.box_str("hello");
        dsl.box_sym("foo");
        dsl.t();
        dsl.f();
        dsl.bool_(false);
        dsl.nil();
        let id = dsl.lambda(1, dsl.local(0));
        dsl.let_(
            dsl.singleton(id.clone()),
            dsl.app(Ref::lref(0), dsl.singleton(Ref::sym("foo"))),
        );
        dsl.letrec_(
            dsl.singleton(id.clone()),
            dsl.app(Ref::lref(0), dsl.singleton(Ref::sym("foo"))),
        );
    }
}

/// Example STG expression for use in tests
#[cfg(test)]
pub mod ex {

    use super::*;

    pub fn i<'a, T: Allocator>(dsl: &StgDsl<'a, T>) -> LambdaForm {
        dsl.lambda(1, dsl.local(0))
    }

    pub fn k<'a, T: Allocator>(dsl: &StgDsl<'a, T>) -> LambdaForm {
        dsl.lambda(2, dsl.local(0))
    }

    pub fn s<'a, T: Allocator>(dsl: &StgDsl<'a, T>) -> LambdaForm {
        dsl.lambda(
            3, // f, g, x
            dsl.let_(
                dsl.array(&[
                    dsl.value(dsl.app(Ref::lref(1), dsl.singleton(Ref::lref(2)))), // g(x)
                    dsl.value(dsl.app(Ref::lref(0), dsl.singleton(Ref::lref(2)))), // f(x)
                ]),
                dsl.app(Ref::lref(0), dsl.singleton(Ref::lref(1))),
            ),
        )
    }

    pub fn compose<'a, T: Allocator>(dsl: &StgDsl<'a, T>) -> LambdaForm {
        dsl.lambda(
            2, // [f g]
            dsl.let_(
                dsl.array(&[dsl.lambda(1, dsl.app(Ref::lref(2), dsl.singleton(Ref::lref(0))))]), // [x] [f g]
                dsl.app(Ref::lref(1), dsl.singleton(Ref::lref(0))), // [gx] [f g]
            ),
        )
    }

    pub fn not<'a, T: Allocator>(dsl: &StgDsl<'a, T>) -> LambdaForm {
        dsl.lambda(
            1,
            dsl.switch(
                dsl.local(0),
                &[
                    (DataConstructor::BoolFalse.tag(), dsl.t()),
                    (DataConstructor::BoolTrue.tag(), dsl.f()),
                ],
            ),
        )
    }

    /// A LambdaForm which retrieves metadata of its argument or ()
    pub fn meta<'a, T: Allocator>(dsl: &StgDsl<'a, T>) -> LambdaForm {
        dsl.lambda(
            1,
            dsl.demeta(
                dsl.local(0),
                dsl.local(0), // [meta body] [...]
                dsl.unit(),
            ),
        )
    }
}
