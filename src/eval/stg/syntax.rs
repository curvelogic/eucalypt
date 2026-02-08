//! Compiled syntax of the STG machine which is executed directly
use super::tags::Tag;
use crate::common::sourcemap::Smid;
use chrono::{DateTime, FixedOffset};
use std::{fmt, rc::Rc};

/// The unboxed native (non algebraic) data types
use serde_json::Number;

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

impl fmt::Display for Native {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Native::Sym(s) => {
                write!(f, ":{s}")
            }
            Native::Str(s) => {
                write!(f, "\"{s}\"")
            }
            Native::Num(n) => {
                write!(f, "{n}")
            }
            Native::Zdt(t) => {
                write!(f, "☽{t}")
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

impl<T: Clone> Reference<T> {
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

/// Compiled STG syntax
#[derive(Debug, PartialEq, Eq, Default)]
pub enum StgSyn {
    /// A single thing - either a reference into env or a native
    Atom { evaluand: Ref },
    /// Case - the only form which actually evaluates
    Case {
        /// Form to be evaluated
        scrutinee: Rc<StgSyn>,
        /// Data type handlers
        branches: Vec<(Tag, Rc<StgSyn>)>,
        /// Default handler
        fallback: Option<Rc<StgSyn>>,
    },
    /// Saturated data constructor
    Cons { tag: Tag, args: Vec<Ref> },
    /// Function application
    App { callable: Ref, args: Vec<Ref> },
    /// Saturated intrinsic application
    Bif { intrinsic: u8, args: Vec<Ref> },
    /// Let bindings
    Let {
        bindings: Vec<LambdaForm>,
        body: Rc<StgSyn>,
    },
    /// Recursive let bindings
    ///
    /// Bindings reference each other via de Bruijn indices, not Rc
    /// pointers, so there is no circular Rc at the compiled syntax
    /// level. Self-reference is resolved at runtime via the heap
    /// environment, managed by the GC.
    LetRec {
        bindings: Vec<LambdaForm>,
        body: Rc<StgSyn>,
    },
    /// Call-stack / source location annotation
    Ann { smid: Smid, body: Rc<StgSyn> },
    /// Wrap metadata around an expression
    ///
    /// Transparency: immediately check stack, if its a demeta,
    /// continue else enter body.
    Meta { meta: Ref, body: Ref },
    /// Destructure metadata into a lambda form which receives two
    /// args, meta and body
    DeMeta {
        scrutinee: Rc<StgSyn>,
        handler: Rc<StgSyn>,
        or_else: Rc<StgSyn>,
    },
    /// Blackhole - invalid / uninitialised code
    #[default]
    BlackHole,
}

impl StgSyn {
    /// Used to determine when to create thunks and when not
    pub fn is_whnf(&self) -> bool {
        matches!(
            self,
            StgSyn::Cons { .. }
                | StgSyn::Meta { .. }
                | StgSyn::Atom {
                    evaluand: Reference::V(_)
                }
        )
    }
}

impl fmt::Display for StgSyn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StgSyn::Atom { evaluand } => {
                write!(f, "{evaluand}")
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
                write!(f, "CASE({scrutinee}⑂<{desc}>)")
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
                write!(f, "LET[×{}]({})", bindings.len(), body)
            }
            StgSyn::LetRec { bindings, body } => {
                write!(f, "LETREC[×{}]({})", bindings.len(), body)
            }
            StgSyn::Ann { smid, body } => {
                write!(f, "♪{smid}:{body}")
            }
            StgSyn::Meta { meta, body } => {
                write!(f, "`{meta}{body}")
            }
            StgSyn::DeMeta { .. } => {
                write!(f, "ƒ(`,•)")
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
#[derive(Debug, PartialEq, Eq)]
pub enum LambdaForm {
    Lambda {
        bound: u8,
        body: Rc<StgSyn>,
        annotation: Smid,
    },
    Thunk {
        body: Rc<StgSyn>,
    },
    Value {
        body: Rc<StgSyn>,
    },
}

impl LambdaForm {
    /// Create new lambda form - local vars < `bound` are bound vars.
    pub fn new(bound: u8, body: Rc<StgSyn>, annotation: Smid) -> Self {
        LambdaForm::Lambda {
            bound,
            body,
            annotation,
        }
    }

    /// A lambda form that will be updated after evaluation
    pub fn thunk(body: Rc<StgSyn>) -> Self {
        LambdaForm::Thunk { body }
    }

    /// A lambda form that is effectively a value - not worth updating
    pub fn value(body: Rc<StgSyn>) -> Self {
        LambdaForm::Value { body }
    }

    /// Reference the body of the lambda form
    pub fn body(&self) -> &Rc<StgSyn> {
        match *self {
            LambdaForm::Lambda { ref body, .. }
            | LambdaForm::Thunk { ref body, .. }
            | LambdaForm::Value { ref body } => body,
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

pub mod dsl {
    use std::rc::Rc;

    use chrono::{DateTime, FixedOffset};
    use serde_json::Number;

    use crate::{common::sourcemap::Smid, eval::stg::tags::DataConstructor};

    use super::{LambdaForm, Native, Ref, Reference, StgSyn, Tag};

    pub fn atom(r: Ref) -> Rc<StgSyn> {
        Rc::new(StgSyn::Atom { evaluand: r })
    }

    pub fn app(r: Ref, args: Vec<Ref>) -> Rc<StgSyn> {
        Rc::new(StgSyn::App { callable: r, args })
    }

    pub fn app_bif(index: u8, args: Vec<Ref>) -> Rc<StgSyn> {
        Rc::new(StgSyn::Bif {
            intrinsic: index,
            args,
        })
    }

    /// A data type value
    pub fn data(tag: Tag, args: Vec<Ref>) -> Rc<StgSyn> {
        Rc::new(StgSyn::Cons { tag, args })
    }

    /// Local ref
    pub fn lref(index: usize) -> Ref {
        Reference::L(index)
    }

    /// Global ref
    pub fn gref(index: usize) -> Ref {
        Reference::G(index)
    }

    /// Unboxed native embedded in ref
    pub fn vref(n: Native) -> Ref {
        Reference::V(n)
    }

    /// Reference into environment
    pub fn local(index: usize) -> Rc<StgSyn> {
        atom(Reference::L(index))
    }

    /// Reference into globals
    pub fn global(index: usize) -> Rc<StgSyn> {
        atom(Reference::G(index))
    }

    /// A native number atom
    pub fn num<N>(n: N) -> Ref
    where
        N: Into<Number>,
    {
        vref(Native::Num(n.into()))
    }

    /// A boxed number
    pub fn box_num<N>(n: N) -> Rc<StgSyn>
    where
        N: Into<Number>,
    {
        data(DataConstructor::BoxedNumber.tag(), vec![num(n)])
    }

    /// Create a string
    pub fn str<T: AsRef<str>>(s: T) -> Ref {
        vref(Native::Str(s.as_ref().into()))
    }

    /// Create a string
    pub fn box_str<T: AsRef<str>>(s: T) -> Rc<StgSyn> {
        data(DataConstructor::BoxedString.tag(), vec![str(s)])
    }

    /// Create a symbol
    pub fn sym<T: AsRef<str>>(s: T) -> Ref {
        vref(Native::Sym(s.as_ref().into()))
    }

    /// Create a symbol
    pub fn box_sym<T: AsRef<str>>(s: T) -> Rc<StgSyn> {
        data(DataConstructor::BoxedSymbol.tag(), vec![sym(s)])
    }

    /// Create a zoned datetime
    pub fn zdt(dt: DateTime<FixedOffset>) -> Ref {
        vref(Native::Zdt(dt))
    }

    /// Create a boxed zoned datetime
    pub fn box_zdt(dt: DateTime<FixedOffset>) -> Rc<StgSyn> {
        data(DataConstructor::BoxedZdt.tag(), vec![zdt(dt)])
    }

    /// Boolean true
    pub fn t() -> Rc<StgSyn> {
        data(DataConstructor::BoolTrue.tag(), vec![])
    }

    /// Boolean false
    pub fn f() -> Rc<StgSyn> {
        data(DataConstructor::BoolFalse.tag(), vec![])
    }

    /// To STG boolean
    pub fn bool_(b: bool) -> Rc<StgSyn> {
        if b {
            t()
        } else {
            f()
        }
    }

    /// Unit / null
    pub fn unit() -> Rc<StgSyn> {
        data(DataConstructor::Unit.tag(), vec![])
    }

    /// Empty list
    pub fn nil() -> Rc<StgSyn> {
        data(DataConstructor::ListNil.tag(), vec![])
    }

    /// List cons
    pub fn cons(h: Ref, t: Ref) -> Rc<StgSyn> {
        data(DataConstructor::ListCons.tag(), vec![h, t])
    }

    /// Block pair
    pub fn pair<T: AsRef<str>>(k: T, v: Ref) -> Rc<StgSyn> {
        data(
            DataConstructor::BlockPair.tag(),
            vec![vref(Native::Sym(k.as_ref().to_string())), v],
        )
    }

    /// Sentinel for "no block index" (boxed zero)
    pub fn no_index() -> Ref {
        num(0)
    }

    /// Block wrapper (list + no-index sentinel)
    pub fn block(inner: Ref) -> Rc<StgSyn> {
        data(DataConstructor::Block.tag(), vec![inner, no_index()])
    }

    /// Simple let
    pub fn let_(bindings: Vec<LambdaForm>, body: Rc<StgSyn>) -> Rc<StgSyn> {
        Rc::new(StgSyn::Let { bindings, body })
    }

    /// Recursive let
    pub fn letrec_(bindings: Vec<LambdaForm>, body: Rc<StgSyn>) -> Rc<StgSyn> {
        Rc::new(StgSyn::LetRec { bindings, body })
    }

    /// A lambda form
    pub fn lambda(bound: u8, body: Rc<StgSyn>) -> LambdaForm {
        LambdaForm::new(bound, body, Smid::default())
    }

    /// An annotated lambda form
    pub fn annotated_lambda(bound: u8, body: Rc<StgSyn>, annotation: Smid) -> LambdaForm {
        LambdaForm::new(bound, body, annotation)
    }

    /// A thunk lambda form
    pub fn thunk(body: Rc<StgSyn>) -> LambdaForm {
        LambdaForm::thunk(body)
    }

    /// A value lambda form
    pub fn value(body: Rc<StgSyn>) -> LambdaForm {
        LambdaForm::value(body)
    }

    /// Case statement, evaluate scrutinee then branch
    pub fn case(
        scrutinee: Rc<StgSyn>,
        branches: Vec<(Tag, Rc<StgSyn>)>,
        fallback: Rc<StgSyn>,
    ) -> Rc<StgSyn> {
        Rc::new(StgSyn::Case {
            scrutinee,
            branches,
            fallback: Some(fallback),
        })
    }

    /// Case statement without default
    pub fn switch(scrutinee: Rc<StgSyn>, branches: Vec<(Tag, Rc<StgSyn>)>) -> Rc<StgSyn> {
        Rc::new(StgSyn::Case {
            scrutinee,
            branches,
            fallback: None,
        })
    }

    /// Force evaluation of scrutinee then continue
    pub fn force(scrutinee: Rc<StgSyn>, then: Rc<StgSyn>) -> Rc<StgSyn> {
        case(scrutinee, vec![], then)
    }

    /// Unbox a number
    pub fn unbox_num(scrutinee: Rc<StgSyn>, then: Rc<StgSyn>) -> Rc<StgSyn> {
        switch(scrutinee, vec![(DataConstructor::BoxedNumber.tag(), then)])
    }

    /// Unbox a string
    pub fn unbox_str(scrutinee: Rc<StgSyn>, then: Rc<StgSyn>) -> Rc<StgSyn> {
        switch(scrutinee, vec![(DataConstructor::BoxedString.tag(), then)])
    }

    /// Unbox a symbol
    pub fn unbox_sym(scrutinee: Rc<StgSyn>, then: Rc<StgSyn>) -> Rc<StgSyn> {
        switch(scrutinee, vec![(DataConstructor::BoxedSymbol.tag(), then)])
    }

    /// Unbox a symbol
    pub fn unbox_zdt(scrutinee: Rc<StgSyn>, then: Rc<StgSyn>) -> Rc<StgSyn> {
        switch(scrutinee, vec![(DataConstructor::BoxedZdt.tag(), then)])
    }

    /// Add metadata to an expression
    pub fn with_meta(meta: Ref, body: Ref) -> Rc<StgSyn> {
        Rc::new(StgSyn::Meta { meta, body })
    }

    /// Retrieve metadata from an expression (or unit)
    pub fn demeta(scrutinee: Rc<StgSyn>, handler: Rc<StgSyn>, or_else: Rc<StgSyn>) -> Rc<StgSyn> {
        Rc::new(StgSyn::DeMeta {
            scrutinee,
            handler,
            or_else,
        })
    }

    /// Add a source code annotation around an expression
    pub fn ann(smid: Smid, body: Rc<StgSyn>) -> Rc<StgSyn> {
        Rc::new(StgSyn::Ann { smid, body })
    }
}

/// Example STG expression for use in tests
#[cfg(test)]
pub mod ex {
    use crate::eval::stg::tags::DataConstructor;

    use super::dsl::*;
    use super::LambdaForm;

    pub fn i() -> LambdaForm {
        lambda(1, local(0))
    }

    pub fn k() -> LambdaForm {
        lambda(2, local(0))
    }

    pub fn s() -> LambdaForm {
        lambda(
            3, // f, g, x
            let_(
                vec![
                    value(app(lref(1), vec![lref(2)])), // g(x)
                    value(app(lref(0), vec![lref(2)])), // f(x)
                ],
                app(lref(0), vec![lref(1)]),
            ),
        )
    }

    pub fn compose() -> LambdaForm {
        lambda(
            2, // [f g]
            let_(
                vec![lambda(1, app(lref(2), vec![lref(0)]))], // [x] [f g]
                app(lref(1), vec![lref(0)]),                  // [gx] [f g]
            ),
        )
    }

    pub fn not() -> LambdaForm {
        lambda(
            1,
            switch(
                local(0),
                vec![
                    (DataConstructor::BoolFalse.tag(), t()),
                    (DataConstructor::BoolTrue.tag(), f()),
                ],
            ),
        )
    }

    /// A LambdaForm which retrieves metadata of its argument or ()
    pub fn meta() -> LambdaForm {
        lambda(
            1,
            demeta(
                local(0),
                local(0), // [meta body] [...]
                unit(),
            ),
        )
    }
}
