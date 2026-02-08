//! Compiled syntax of the STG machine represented in
//! eucalypt-allocated memory

use crate::common::sourcemap::Smid;

use crate::eval::{error::ExecutionError, stg::tags::Tag};
use chrono::{DateTime, FixedOffset};
use serde_json::Number;
use std::{collections::HashMap, fmt, ptr::NonNull, rc::Rc};

use super::collect::{CollectorHeapView, CollectorScope, GcScannable, ScanPtr};
use super::infotable::InfoTagged;
use super::set::HeapSet;
use super::string::HeapString;
use super::symbol::SymbolId;
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
            _ => false,
        }
    }
}

impl Eq for Native {}

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

/// Add arity, update flag etc. to HeapSyn to make LambdaForm
pub type LambdaForm = InfoTagged<RefPtr<HeapSyn>>;

/// Compiled STG syntax
#[derive(Debug, Clone, Default)]
pub enum HeapSyn {
    /// A single thing - either a reference into env or a native
    Atom { evaluand: Ref },
    /// Case - the only form which actually evaluates
    Case {
        /// Form to be evaluated
        scrutinee: RefPtr<HeapSyn>,
        /// Lowest tag in the branch table
        min_tag: Tag,
        /// Indexed branch table: entry at `[tag - min_tag]` holds the
        /// handler for that tag, or `None` if no branch exists
        branch_table: Array<Option<RefPtr<HeapSyn>>>,
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
    #[default]
    BlackHole,
}

impl StgObject for HeapSyn {}

impl HeapSyn {
    /// Used to determine when to create thunks and when not
    pub fn is_whnf(&self) -> bool {
        matches!(
            self,
            HeapSyn::Cons { .. }
                | HeapSyn::Meta { .. }
                | HeapSyn::Atom {
                    evaluand: Reference::V(_)
                }
        )
    }
}

impl GcScannable for LambdaForm {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        let body = self.body();
        if marker.mark(body) {
            out.push(ScanPtr::from_non_null(scope, body));
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        if let Some(new_body) = heap.forwarded_to(self.body()) {
            self.set_body(new_body);
        }
    }
}

/// Mark any heap pointers embedded in a Ref value.
///
/// Native::Str and Native::Set contain heap pointers that must be
/// traced to ensure their lines are marked. Without this, evacuation
/// cannot discover and update these pointers.
fn mark_ref_heap_pointers(r: &Ref, marker: &mut CollectorHeapView<'_>) {
    match r {
        Ref::V(Native::Str(ptr)) => {
            marker.mark(*ptr);
        }
        Ref::V(Native::Set(ptr)) => {
            marker.mark(*ptr);
        }
        _ => {}
    }
}

/// Mark heap pointers in all elements of a Ref array.
fn mark_ref_array_heap_pointers(args: &Array<Ref>, marker: &mut CollectorHeapView<'_>) {
    for r in args.iter() {
        mark_ref_heap_pointers(r, marker);
    }
}

/// Update any forwarded heap pointers in a Ref value.
fn update_ref_heap_pointers(r: &mut Ref, heap: &CollectorHeapView<'_>) {
    match r {
        Ref::V(Native::Str(ptr)) => {
            if let Some(new_ptr) = heap.forwarded_to(*ptr) {
                *ptr = new_ptr;
            }
        }
        Ref::V(Native::Set(ptr)) => {
            if let Some(new_ptr) = heap.forwarded_to(*ptr) {
                *ptr = new_ptr;
            }
        }
        _ => {}
    }
}

/// Update forwarded heap pointers in all elements of a Ref array.
fn update_ref_array_heap_pointers(args: &mut Array<Ref>, heap: &CollectorHeapView<'_>) {
    for r in args.iter_mut() {
        update_ref_heap_pointers(r, heap);
    }
}

impl GcScannable for HeapSyn {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        match self {
            HeapSyn::Atom { evaluand } => {
                mark_ref_heap_pointers(evaluand, marker);
            }
            HeapSyn::Case {
                scrutinee,
                branch_table,
                fallback,
                ..
            } => {
                if marker.mark(*scrutinee) {
                    out.push(ScanPtr::from_non_null(scope, *scrutinee));
                }
                if marker.mark_array(branch_table) {
                    for b in branch_table.iter().flatten() {
                        if marker.mark(*b) {
                            out.push(ScanPtr::from_non_null(scope, *b));
                        }
                    }
                }
                if let Some(f) = fallback {
                    if marker.mark(*f) {
                        out.push(ScanPtr::from_non_null(scope, *f));
                    }
                }
            }
            HeapSyn::Cons { tag: _, args } => {
                marker.mark_array(args);
                mark_ref_array_heap_pointers(args, marker);
            }
            HeapSyn::App { callable, args } => {
                mark_ref_heap_pointers(callable, marker);
                marker.mark_array(args);
                mark_ref_array_heap_pointers(args, marker);
            }
            HeapSyn::Bif { intrinsic: _, args } => {
                marker.mark_array(args);
                mark_ref_array_heap_pointers(args, marker);
            }
            HeapSyn::Let { bindings, body } => {
                if marker.mark_array(bindings) {
                    for bindings in bindings.iter() {
                        out.push(ScanPtr::new(scope, bindings));
                    }
                }

                if marker.mark(*body) {
                    out.push(ScanPtr::from_non_null(scope, *body));
                }
            }
            HeapSyn::LetRec { bindings, body } => {
                if marker.mark_array(bindings) {
                    for bindings in bindings.iter() {
                        out.push(ScanPtr::new(scope, bindings));
                    }
                }

                if marker.mark(*body) {
                    out.push(ScanPtr::from_non_null(scope, *body));
                }
            }
            HeapSyn::Ann { smid: _, body } => {
                if marker.mark(*body) {
                    out.push(ScanPtr::from_non_null(scope, *body));
                }
            }
            HeapSyn::Meta { meta, body } => {
                mark_ref_heap_pointers(meta, marker);
                mark_ref_heap_pointers(body, marker);
            }
            HeapSyn::DeMeta {
                scrutinee,
                handler,
                or_else,
            } => {
                if marker.mark(*scrutinee) {
                    out.push(ScanPtr::from_non_null(scope, *scrutinee));
                }
                if marker.mark(*handler) {
                    out.push(ScanPtr::from_non_null(scope, *handler));
                }
                if marker.mark(*or_else) {
                    out.push(ScanPtr::from_non_null(scope, *or_else));
                }
            }
            HeapSyn::BlackHole => {}
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        match self {
            HeapSyn::Atom { evaluand } => {
                update_ref_heap_pointers(evaluand, heap);
            }
            HeapSyn::Case {
                scrutinee,
                branch_table,
                fallback,
                ..
            } => {
                if let Some(new) = heap.forwarded_to(*scrutinee) {
                    *scrutinee = new;
                }
                for ptr in branch_table.iter_mut().flatten() {
                    if let Some(new) = heap.forwarded_to(*ptr) {
                        *ptr = new;
                    }
                }
                if let Some(ref mut f) = fallback {
                    if let Some(new) = heap.forwarded_to(*f) {
                        *f = new;
                    }
                }
            }
            HeapSyn::Cons { args, .. } => {
                update_ref_array_heap_pointers(args, heap);
            }
            HeapSyn::App { callable, args } => {
                update_ref_heap_pointers(callable, heap);
                update_ref_array_heap_pointers(args, heap);
            }
            HeapSyn::Bif { args, .. } => {
                update_ref_array_heap_pointers(args, heap);
            }
            HeapSyn::Let { bindings, body } | HeapSyn::LetRec { bindings, body } => {
                for lf in bindings.iter_mut() {
                    lf.scan_and_update(heap);
                }
                if let Some(new) = heap.forwarded_to(*body) {
                    *body = new;
                }
            }
            HeapSyn::Ann { body, .. } => {
                if let Some(new) = heap.forwarded_to(*body) {
                    *body = new;
                }
            }
            HeapSyn::Meta { meta, body } => {
                update_ref_heap_pointers(meta, heap);
                update_ref_heap_pointers(body, heap);
            }
            HeapSyn::DeMeta {
                scrutinee,
                handler,
                or_else,
            } => {
                if let Some(new) = heap.forwarded_to(*scrutinee) {
                    *scrutinee = new;
                }
                if let Some(new) = heap.forwarded_to(*handler) {
                    *handler = new;
                }
                if let Some(new) = heap.forwarded_to(*or_else) {
                    *or_else = new;
                }
            }
            HeapSyn::BlackHole => {}
        }
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
            infotable::InfoTable,
        },
        stg::{self, syntax::StgSyn},
    };

    use super::{HeapSyn, RefPtr, Repr};

    /// Convert heap representation of reference to syntax representation
    pub fn heap_to_stg(guard: &dyn MutatorScope, r: &memory::syntax::Ref) -> stg::syntax::Ref {
        match r {
            memory::syntax::Ref::L(n) => stg::syntax::Ref::L(*n),
            memory::syntax::Ref::G(n) => stg::syntax::Ref::G(*n),
            memory::syntax::Ref::V(memory::syntax::Native::Sym(id)) => {
                // Without pool access, use the ID for debug representation
                stg::syntax::Ref::V(stg::syntax::Native::Sym(format!("sym#{}", id.as_u32())))
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
            memory::syntax::Ref::V(memory::syntax::Native::Index(_)) => {
                stg::syntax::Ref::V(stg::syntax::Native::Sym("<index>".to_string()))
            }
            memory::syntax::Ref::V(memory::syntax::Native::Set(_)) => {
                stg::syntax::Ref::V(stg::syntax::Native::Sym("<set>".to_string()))
            }
        }
    }

    /// Represent in-heap branch table in syntax form
    pub fn repr_branch_table(
        guard: &dyn MutatorScope,
        min_tag: u8,
        branch_table: Array<Option<RefPtr<HeapSyn>>>,
    ) -> Vec<(u8, Rc<StgSyn>)> {
        branch_table
            .iter()
            .enumerate()
            .filter_map(|(i, entry)| {
                entry.map(|ptr| {
                    let tag = min_tag + i as u8;
                    (tag, ScopedPtr::from_non_null(guard, ptr).repr())
                })
            })
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
            let binding = if pc.arity() > 0 {
                stg::syntax::LambdaForm::Lambda {
                    bound: pc.arity(),
                    body: ScopedPtr::from_non_null(guard, pc.body()).repr(),
                    annotation: pc.annotation(),
                }
            } else if pc.update() {
                stg::syntax::LambdaForm::Thunk {
                    body: ScopedPtr::from_non_null(guard, pc.body()).repr(),
                }
            } else {
                stg::syntax::LambdaForm::Value {
                    body: ScopedPtr::from_non_null(guard, pc.body()).repr(),
                }
            };
            v.push(binding);
        }
        v
    }
}

/// Convert in-heap HeapSyn back to StgSyn for debugging
impl Repr for ScopedPtr<'_, HeapSyn> {
    fn repr(&self) -> Rc<crate::eval::stg::syntax::StgSyn> {
        use crate::eval::stg::syntax::StgSyn;

        match &**self {
            HeapSyn::Atom { evaluand } => Rc::new(StgSyn::Atom {
                evaluand: repr::heap_to_stg(self, evaluand),
            }),
            HeapSyn::Case {
                scrutinee,
                min_tag,
                branch_table,
                fallback,
            } => Rc::new(StgSyn::Case {
                scrutinee: ScopedPtr::from_non_null(self, *scrutinee).repr(),
                branches: repr::repr_branch_table(self, *min_tag, branch_table.clone()),
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
                meta: repr::heap_to_stg(self, meta),
                body: repr::heap_to_stg(self, body),
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

impl fmt::Display for ScopedPtr<'_, HeapSyn> {
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
        pool: &mut super::symbol::SymbolPool,
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

    use crate::eval::memory::{heap::Heap, mutator::MutatorHeapView, symbol::SymbolPool};

    use super::*;

    #[test]
    pub fn test_atom() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let mut pool = SymbolPool::new();

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
            view.singleton(id),
            view.app(
                Ref::L(0),
                view.singleton(view.sym_ref(&mut pool, "foo").unwrap()),
            )
            .unwrap(),
        )
        .unwrap();
        view.letrec(
            view.singleton(id),
            view.app(
                Ref::L(0),
                view.singleton(view.sym_ref(&mut pool, "foo").unwrap()),
            )
            .unwrap(),
        )
        .unwrap();
    }
}
