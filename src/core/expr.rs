#![allow(clippy::type_complexity)]
//! Core expression syntax
use crate::common::environment::SimpleEnvironment;
use crate::common::sourcemap::HasSmid;
use crate::common::sourcemap::*;
use crate::core::error::CoreError;
use crate::syntax::input::*;
use indexmap::IndexMap;
use moniker::Var::Free;
use moniker::*;
use serde_json::Number;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;
use std::iter::FromIterator;
use std::rc::Rc;
use std::str::FromStr;

macro_rules! impl_bound_term_ignore {
    ($T:ty) => {
        impl<N: Clone + PartialEq> BoundTerm<N> for $T {
            fn term_eq(&self, _: &$T) -> bool {
                true
            }

            fn close_term(&mut self, _: ScopeState, _: &impl OnFreeFn<N>) {}

            fn open_term(&mut self, _: ScopeState, _: &impl OnBoundFn<N>) {}

            fn visit_vars(&self, _: &mut impl FnMut(&Var<N>)) {}

            fn visit_mut_vars(&mut self, _: &mut impl FnMut(&mut Var<N>)) {}
        }
    };
}

/// Primitive types in core
///
/// NB. Boolean and Unit ("null") are primitive in Core but user types
/// in STG.
#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Str(String),
    Sym(String),
    Num(Number),
    Bool(bool),
    Null,
}

impl_bound_term_ignore!(Primitive);

/// Fixity of operator
#[derive(Debug, Clone, BoundTerm, Copy, PartialEq)]
pub enum Fixity {
    UnaryPrefix,
    UnaryPostfix,
    InfixLeft,
    InfixRight,
}

impl Fixity {
    pub fn arity(&self) -> u8 {
        match self {
            Fixity::UnaryPrefix | Fixity::UnaryPostfix => 1,
            Fixity::InfixLeft | Fixity::InfixRight => 2,
        }
    }
}

impl Display for Fixity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Fixity::UnaryPrefix => write!(f, "prefix"),
            Fixity::UnaryPostfix => write!(f, "postfix"),
            Fixity::InfixLeft => write!(f, "infixl"),
            Fixity::InfixRight => write!(f, "infixr"),
        }
    }
}

/// Precedenr (0-99)
pub type Precedence = i32;

/// Blocks are implemented as insert-ordered hash map
#[derive(Debug, Clone, PartialEq)]
pub struct BlockMap<T: BoundTerm<String>>(IndexMap<String, T>);

impl<T: BoundTerm<String>> BoundTerm<String> for BlockMap<T> {
    fn term_eq(&self, other: &BlockMap<T>) -> bool {
        self.0.len() == other.0.len()
            && <_>::zip(self.0.values(), other.0.values()).all(|(l, r)| <T>::term_eq(l, r))
    }

    fn close_term(&mut self, state: ScopeState, on_free: &impl OnFreeFn<String>) {
        for v in self.0.values_mut() {
            v.close_term(state, on_free);
        }
    }

    fn open_term(&mut self, state: ScopeState, on_bound: &impl OnBoundFn<String>) {
        for v in self.0.values_mut() {
            v.open_term(state, on_bound);
        }
    }

    fn visit_vars(&self, on_var: &mut impl FnMut(&Var<String>)) {
        for v in self.0.values() {
            v.visit_vars(on_var);
        }
    }

    fn visit_mut_vars(&mut self, on_var: &mut impl FnMut(&mut Var<String>)) {
        for v in self.0.values_mut() {
            v.visit_mut_vars(on_var);
        }
    }
}

impl<'a, T: BoundTerm<String>> FromIterator<(String, T)> for BlockMap<T> {
    fn from_iter<U>(iter: U) -> Self
    where
        U: IntoIterator<Item = (String, T)>,
    {
        BlockMap(IndexMap::from_iter(iter))
    }
}

impl<T: BoundTerm<String> + Clone> BlockMap<T> {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, key: &str) -> Option<&T> {
        self.0.get(key)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut T> {
        self.0.get_mut(key)
    }

    pub fn keys(&self) -> indexmap::map::Keys<String, T> {
        self.0.keys()
    }

    pub fn values(&self) -> indexmap::map::Values<String, T> {
        self.0.values()
    }

    pub fn values_mut(&mut self) -> indexmap::map::ValuesMut<String, T> {
        self.0.values_mut()
    }

    pub fn iter(&self) -> indexmap::map::Iter<String, T> {
        self.0.iter()
    }

    /// Apply a possibly-failing function to the values to return a
    /// new block map
    pub fn map_values<E, F: FnMut(T) -> Result<T, E>>(&self, mut f: F) -> Result<BlockMap<T>, E> {
        self.iter()
            .map(|(k, v)| match f(v.clone()) {
                Ok(vv) => Ok((k.to_string(), vv)),
                Err(e) => Err(e),
            })
            .collect::<Result<IndexMap<String, T>, E>>()
            .map(BlockMap)
    }
}

impl<T: BoundTerm<String> + Clone> IntoIterator for BlockMap<T> {
    type Item = (String, T);
    type IntoIter = indexmap::map::IntoIter<String, T>;
    fn into_iter(self) -> indexmap::map::IntoIter<String, T> {
        self.0.into_iter()
    }
}

/// Used to tag lets that have a default block as body, to support
/// optimisations that don't then need to work with the body.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LetType {
    DefaultBlockLet,
    OtherLet,
}

impl_bound_term_ignore!(LetType);

/// Which side of a source item are we inserting an implicit anaphor
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ImplicitAnaphorSide {
    /// Anaphor to the left of the source item
    Left,
    /// Anaphor to the right of the source item
    Right,
}

impl PartialOrd for ImplicitAnaphorSide {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ImplicitAnaphorSide {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if *self == ImplicitAnaphorSide::Left {
            if *other == ImplicitAnaphorSide::Right {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Equal
            }
        } else if *other == ImplicitAnaphorSide::Left {
            std::cmp::Ordering::Equal
        } else {
            std::cmp::Ordering::Greater
        }
    }
}

/// An anaphor
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Anaphor<T, N>
where
    T: Hash + Eq + Clone,
    N: Hash + Eq + Clone,
{
    /// Explicit numberless anaphor (e.g. `_`, `•`) with occurrence
    /// token
    ExplicitAnonymous(T),
    /// Explicit numbered anaphor (e.g. `_2`, `•0`)
    ExplicitNumbered(N),
    /// Implicit section anaphor (e.g. implied by `(+2)`, `(5-)`)
    Implicit(T, ImplicitAnaphorSide),
}

impl<T, N> Anaphor<T, N>
where
    T: Hash + Eq + Clone,
    N: Hash + Eq + Clone,
{
    pub fn number(&self) -> Option<N> {
        match self {
            Anaphor::ExplicitNumbered(n) => Some(n.clone()),
            _ => None,
        }
    }
}

impl<T, N> Ord for Anaphor<T, N>
where
    T: Hash + Eq + Clone + Ord,
    N: Hash + Eq + Clone + Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (&*self, other) {
            (Anaphor::ExplicitNumbered(n), Anaphor::ExplicitNumbered(p)) => n.cmp(p),
            (Anaphor::ExplicitAnonymous(smid_a), Anaphor::ExplicitAnonymous(smid_b)) => {
                smid_a.cmp(smid_b)
            }
            (Anaphor::Implicit(smid_a, side_a), Anaphor::Implicit(smid_b, side_b)) => {
                match smid_a.cmp(smid_b) {
                    std::cmp::Ordering::Equal => side_a.cmp(side_b),
                    ret => ret,
                }
            }
            (Anaphor::ExplicitNumbered(_), _) => std::cmp::Ordering::Less,
            (Anaphor::Implicit(_, _), _) => std::cmp::Ordering::Greater,
            (_, Anaphor::ExplicitNumbered(_)) => std::cmp::Ordering::Greater,
            (_, Anaphor::Implicit(_, _)) => std::cmp::Ordering::Less,
        }
    }
}

impl<T, N> PartialOrd for Anaphor<T, N>
where
    T: Hash + Eq + Clone + Ord,
    N: Hash + Eq + Clone + Ord,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T, N> Display for Anaphor<T, N>
where
    T: Display + Hash + Eq + Clone,
    N: Display + Hash + Eq + Clone,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self {
            Anaphor::ExplicitAnonymous(smid) => write!(f, "_a{}", smid),
            Anaphor::ExplicitNumbered(n) => write!(f, "_n{}", n),
            Anaphor::Implicit(smid, ImplicitAnaphorSide::Left) => write!(f, "_il{}", smid),
            Anaphor::Implicit(smid, ImplicitAnaphorSide::Right) => write!(f, "_ir{}", smid),
        }
    }
}

impl_bound_term_ignore!(Anaphor<Smid, i32>);

impl<N> HasSmid for Anaphor<Smid, N>
where
    N: Display + Hash + Eq + Clone,
{
    fn smid(&self) -> Smid {
        match self {
            Anaphor::ExplicitAnonymous(smid) => *smid,
            Anaphor::Implicit(smid, _) => *smid,
            _ => Smid::default(),
        }
    }
}

pub type LetScope<T> = Scope<Rec<Vec<(Binder<String>, Embed<T>)>>, T>;
pub type LamScope<T> = Scope<Vec<Binder<String>>, T>;

/// The main core expression type
#[derive(Debug, Clone, BoundTerm, PartialEq)]
pub enum Expr<T>
where
    T: BoundTerm<String>,
{
    /// Variable (free or bound)
    Var(Smid, Var<String>),
    /// Recursive let
    Let(Smid, LetScope<T>, LetType),
    /// Reference to built-in
    Intrinsic(Smid, String),
    /// Primitive core value
    Literal(Smid, Primitive),
    /// Lookup (with optional fallback)
    Lookup(Smid, T, String, Option<T>),
    /// Name (to become symbol for lookup or variable for evaluation)
    Name(Smid, String),
    /// Block anaphor (implicit block parameter)
    BlockAnaphor(Smid, Anaphor<Smid, i32>),
    /// Expression anaphor (implicit expression parameter)
    ExprAnaphor(Smid, Anaphor<Smid, i32>),
    /// Literal list expression
    List(Smid, Vec<T>),
    /// Block (in contrast to the haskell implementation we're storing
    /// an ordered record right here)
    Block(Smid, BlockMap<T>),
    /// Metadata annotation (span, expr, meta) - TODO: struct?
    Meta(Smid, T, T),
    /// Tuple of arguments to apply
    ArgTuple(Smid, Vec<T>),
    /// A multi-argument lambda
    Lam(Smid, bool, LamScope<T>),
    /// Application of lambda or builtin (or block)
    App(Smid, T, Vec<T>),
    /// Operator soup awaiting precedence / fixity processing
    Soup(Smid, Vec<T>),
    /// Operator precedence / fixity metadata (may surround definition
    /// or call)
    Operator(Smid, Fixity, Precedence, T),
    /// Marker for unresolved variable
    ErrUnresolved(Smid, String),
    /// Marker for redeclaration
    ErrRedeclaration(Smid, String),
    /// Eliminated
    ErrEliminated,
    /// Marks pseudo dot operator eliminated before compile
    ErrPseudoDot,
    /// Marks pseudo call operator eliminated before compile
    ErrPseudoCall,
    /// Marks pseudo cat operator eliminated before compile
    ErrPseudoCat,
}

pub type CoreExpr = Expr<RcExpr>;

impl<T: BoundTerm<String>> HasSmid for Expr<T> {
    fn smid(&self) -> Smid {
        use self::Expr::*;
        match *self {
            Var(s, _) => s,
            Let(s, _, _) => s,
            Intrinsic(s, _) => s,
            Literal(s, _) => s,
            Lookup(s, _, _, _) => s,
            Name(s, _) => s,
            List(s, _) => s,
            Block(s, _) => s,
            Meta(s, _, _) => s,
            ArgTuple(s, _) => s,
            Lam(s, _, _) => s,
            App(s, _, _) => s,
            Soup(s, _) => s,
            Operator(s, _, _, _) => s,
            ErrUnresolved(s, _) => s,
            ErrRedeclaration(s, _) => s,
            _ => Smid::default(),
        }
    }
}

impl<T: BoundTerm<String>> Expr<T> {
    pub fn is_name(&self) -> bool {
        matches!(self, Expr::Name(_, _))
    }

    pub fn is_operator(&self) -> bool {
        matches!(self, Expr::Operator(_, _, _, _))
    }

    pub fn is_pseudodot(&self) -> bool {
        matches!(self, Expr::ErrPseudoDot)
    }

    pub fn is_pseudocat(&self) -> bool {
        matches!(self, Expr::ErrPseudoCat)
    }

    pub fn is_pseudocall(&self) -> bool {
        matches!(self, Expr::ErrPseudoCall)
    }

    pub fn is_default_let(&self) -> bool {
        matches!(self, Expr::Let(_, _, LetType::DefaultBlockLet))
    }

    pub fn is_expr_anaphor(&self) -> bool {
        matches!(self, Expr::ExprAnaphor(_, _))
    }
}

/// Map one type of Expr (typically an Expr<RcExpr>) into an Expr
/// wired together with other pointer type instead.
///
/// This is typically used to substitue a fat pointer type (see
/// `RcFatExpr`) which maintains some bookkeeping data for analysis
/// purposes.
pub fn fmap<'src, U, V>(expr: &'src Expr<U>) -> Expr<V>
where
    U: 'src + BoundTerm<String> + Clone,
    V: BoundTerm<String> + Clone + From<&'src U>,
{
    match expr {
        Expr::Lookup(s, e, n, fb) => {
            Expr::Lookup(*s, V::from(e), n.clone(), fb.as_ref().map(|x| V::from(x)))
        }
        Expr::List(s, xs) => Expr::List(*s, xs.iter().map(V::from).collect()),
        Expr::Block(s, block_map) => Expr::Block(
            *s,
            block_map
                .iter()
                .map(|(k, v)| (k.clone(), V::from(v)))
                .collect(),
        ),
        Expr::Meta(s, e, m) => Expr::Meta(*s, V::from(e), V::from(m)),
        Expr::ArgTuple(s, xs) => Expr::ArgTuple(*s, xs.iter().map(V::from).collect()),
        Expr::App(s, f, xs) => Expr::App(*s, V::from(f), xs.iter().map(V::from).collect()),
        Expr::Soup(s, xs) => Expr::Soup(*s, xs.iter().map(V::from).collect()),
        Expr::Operator(s, fx, p, e) => Expr::Operator(*s, *fx, *p, V::from(e)),
        Expr::Let(s, scope, t) => Expr::Let(
            *s,
            Scope {
                unsafe_pattern: Rec {
                    unsafe_pattern: scope
                        .unsafe_pattern
                        .unsafe_pattern
                        .iter()
                        .map(|&(ref n, Embed(ref value))| (n.clone(), Embed(V::from(value))))
                        .collect(),
                },
                unsafe_body: V::from(&scope.unsafe_body),
            },
            *t,
        ),
        Expr::Lam(s, inl, scope) => Expr::Lam(
            *s,
            *inl,
            Scope {
                unsafe_pattern: scope.unsafe_pattern.clone(),
                unsafe_body: V::from(&scope.unsafe_body),
            },
        ),
        Expr::Var(s, v) => Expr::Var(*s, v.clone()),
        Expr::Intrinsic(s, name) => Expr::Intrinsic(*s, name.clone()),
        Expr::Literal(s, p) => Expr::Literal(*s, p.clone()),
        Expr::Name(s, n) => Expr::Name(*s, n.clone()),
        Expr::BlockAnaphor(s, i) => Expr::BlockAnaphor(*s, *i),
        Expr::ExprAnaphor(s, ana) => Expr::ExprAnaphor(*s, *ana),
        Expr::ErrUnresolved(s, n) => Expr::ErrUnresolved(*s, n.clone()),
        Expr::ErrRedeclaration(s, n) => Expr::ErrRedeclaration(*s, n.clone()),
        Expr::ErrEliminated => Expr::ErrEliminated,
        Expr::ErrPseudoDot => Expr::ErrPseudoDot,
        Expr::ErrPseudoCall => Expr::ErrPseudoCall,
        Expr::ErrPseudoCat => Expr::ErrPseudoCat,
    }
}

/// The main form in which core expressions are passed around
#[derive(Debug, Clone, BoundTerm, PartialEq)]
pub struct RcExpr {
    pub inner: Rc<CoreExpr>,
}

impl From<CoreExpr> for RcExpr {
    fn from(src: CoreExpr) -> RcExpr {
        RcExpr {
            inner: Rc::new(src),
        }
    }
}

impl HasSmid for RcExpr {
    fn smid(&self) -> Smid {
        self.inner.smid()
    }
}

impl Default for RcExpr {
    fn default() -> Self {
        RcExpr::from(Expr::Block(Smid::default(), BlockMap(IndexMap::new())))
    }
}

impl RcExpr {
    /// True iff the expression is a call pseudo operator
    pub fn is_pseudocall(&self) -> bool {
        self.inner.is_pseudocall()
    }

    /// True iff the expression is a dot pseudo operator
    pub fn is_pseudodot(&self) -> bool {
        self.inner.is_pseudodot()
    }

    /// True iff the expression is a cat pseudo operator
    pub fn is_pseudocat(&self) -> bool {
        self.inner.is_pseudocat()
    }

    /// Apply a name to an expression using a top level let.
    pub fn apply_name<S: AsRef<str>>(&self, smid: Smid, name: S) -> RcExpr {
        core::default_let(smid, vec![(free(name.as_ref()), self.clone())])
    }

    /// Replace the body of a let (or nested lets) with the specified body
    pub fn rebody(&self, new_body: RcExpr) -> RcExpr {
        self.rebody_int(&mut SimpleEnvironment::new(), new_body)
    }

    /// Retrieve the actual freevars corresponding to bound names in a
    /// let binding so we can prepare to capture using them
    fn prepare_capture_vars(
        scope: &Scope<Rec<Vec<(Binder<String>, Embed<RcExpr>)>>, RcExpr>,
    ) -> HashMap<String, FreeVar<String>> {
        scope
            .unsafe_pattern
            .unsafe_pattern
            .iter()
            .map(|(k, _)| {
                let fv = k.0.clone();
                let name = fv.clone().pretty_name.unwrap();
                (name, fv)
            })
            .collect()
    }

    /// Replace the body of a let (or nested lets with the specified
    /// body) after altering freevars to clones of the version bound
    /// in the lets so that they can be bound.
    fn rebody_int(
        &self,
        env: &mut SimpleEnvironment<String, FreeVar<String>>,
        new_body: RcExpr,
    ) -> RcExpr {
        match &*self.inner {
            Expr::Let(s, scope, let_type) => {
                let p = scope.clone().unsafe_pattern;

                let bind_vars = Self::prepare_capture_vars(scope);
                env.push(bind_vars);

                let t = scope.clone().unsafe_body;
                let rebodied = t.rebody_int(env, new_body);
                let ret = RcExpr::from(Expr::Let(*s, Scope::new(p, rebodied), *let_type));
                env.pop();
                ret
            }
            Expr::Meta(s, e, m) => {
                RcExpr::from(Expr::Meta(*s, e.rebody_int(env, new_body), m.clone()))
            }
            _ => {
                // ensure any free vars in new_body can be captured by surroundings
                new_body.substs_free(&|n: &str| {
                    env.get(&n.to_string())
                        .map(|v| core::var(Smid::default(), v.clone()))
                })
            }
        }
    }

    /// Helper for methods that recurse down through expressions. Not
    /// necessarily cheap as rebuilds all composite expressions
    /// regardless of whether changes are required.
    ///
    /// Any transformation in f should be structural, leaving var
    /// binding status intact as no moniker .unbinds are called. This
    /// makes free use of unsafe_pattern and unsafe_body
    pub fn walk<F: Fn(RcExpr) -> RcExpr>(&self, f: &F) -> RcExpr {
        match &*self.inner {
            Expr::Lookup(s, e, n, fb) => {
                RcExpr::from(Expr::Lookup(*s, f(e.clone()), n.clone(), fb.clone().map(f)))
            }
            Expr::List(s, xs) => {
                RcExpr::from(Expr::List(*s, xs.iter().map(|x| f(x.clone())).collect()))
            }
            Expr::Block(s, block_map) => {
                let bm = block_map
                    .iter()
                    .map(|(k, v)| (k.clone(), f(v.clone())))
                    .collect();
                RcExpr::from(Expr::Block(*s, bm))
            }
            Expr::Meta(s, e, m) => RcExpr::from(Expr::Meta(*s, f(e.clone()), f(m.clone()))),
            Expr::ArgTuple(s, xs) => RcExpr::from(Expr::ArgTuple(
                *s,
                xs.iter().map(|x| f(x.clone())).collect(),
            )),
            Expr::App(s, g, xs) => RcExpr::from(Expr::App(
                *s,
                f(g.clone()),
                xs.iter().map(|x| f(x.clone())).collect(),
            )),
            Expr::Soup(s, xs) => {
                RcExpr::from(Expr::Soup(*s, xs.iter().map(|x| f(x.clone())).collect()))
            }
            Expr::Operator(s, fx, p, e) => RcExpr::from(Expr::Operator(*s, *fx, *p, f(e.clone()))),
            Expr::Let(s, scope, t) => RcExpr::from(Expr::Let(
                *s,
                Scope {
                    unsafe_pattern: Rec {
                        unsafe_pattern: scope
                            .unsafe_pattern
                            .unsafe_pattern
                            .iter()
                            .map(|&(ref n, Embed(ref value))| (n.clone(), Embed(f(value.clone()))))
                            .collect(),
                    },
                    unsafe_body: f(scope.unsafe_body.clone()),
                },
                *t,
            )),
            Expr::Lam(s, inl, scope) => RcExpr::from(Expr::Lam(
                *s,
                *inl,
                Scope {
                    unsafe_pattern: scope.unsafe_pattern.clone(),
                    unsafe_body: f(scope.unsafe_body.clone()),
                },
            )),
            _ => self.clone(),
        }
    }

    /// Helper for methods that recurse down through expressions. Not
    /// necessarily cheap as rebuilds all composite expressions
    /// regardless of whether changes are required.
    ///
    /// Any transformation in f should be structural, leaving var
    /// binding status intact as no moniker .unbinds are called. This
    /// makes free use of unsafe_pattern and unsafe_body
    pub fn walk_safe<E, F: FnMut(RcExpr) -> Result<RcExpr, E>>(
        &self,
        f: &mut F,
    ) -> Result<RcExpr, E> {
        Ok(match &*self.inner {
            Expr::Lookup(s, e, n, fb) => {
                let fallback = match fb.clone() {
                    Some(expr) => Some(f(expr)?),
                    None => None,
                };
                RcExpr::from(Expr::Lookup(*s, f(e.clone())?, n.clone(), fallback))
            }
            Expr::List(s, xs) => RcExpr::from(Expr::List(
                *s,
                xs.iter()
                    .map(|x| f(x.clone()))
                    .collect::<Result<Vec<RcExpr>, E>>()?,
            )),
            Expr::Block(s, block_map) => RcExpr::from(Expr::Block(*s, block_map.map_values(f)?)),
            Expr::Meta(s, e, m) => RcExpr::from(Expr::Meta(*s, f(e.clone())?, f(m.clone())?)),
            Expr::ArgTuple(s, xs) => RcExpr::from(Expr::ArgTuple(
                *s,
                xs.iter()
                    .map(|x| f(x.clone()))
                    .collect::<Result<Vec<RcExpr>, E>>()?,
            )),
            Expr::App(s, g, xs) => RcExpr::from(Expr::App(
                *s,
                f(g.clone())?,
                xs.iter()
                    .map(|x| f(x.clone()))
                    .collect::<Result<Vec<RcExpr>, E>>()?,
            )),
            Expr::Soup(s, xs) => RcExpr::from(Expr::Soup(
                *s,
                xs.iter()
                    .map(|x| f(x.clone()))
                    .collect::<Result<Vec<RcExpr>, E>>()?,
            )),
            Expr::Operator(s, fx, p, e) => RcExpr::from(Expr::Operator(*s, *fx, *p, f(e.clone())?)),
            Expr::Let(s, scope, t) => {
                let bindings = scope
                    .unsafe_pattern
                    .unsafe_pattern
                    .iter()
                    .map(|&(ref n, Embed(ref value))| {
                        let val = f(value.clone());
                        match val {
                            Ok(v) => Ok((n.clone(), Embed(v))),
                            Err(e) => Err(e),
                        }
                    })
                    .collect::<Result<Vec<(Binder<String>, Embed<RcExpr>)>, E>>()?;
                RcExpr::from(Expr::Let(
                    *s,
                    Scope {
                        unsafe_pattern: Rec {
                            unsafe_pattern: bindings,
                        },
                        unsafe_body: f(scope.unsafe_body.clone())?,
                    },
                    *t,
                ))
            }
            Expr::Lam(s, inl, scope) => RcExpr::from(Expr::Lam(
                *s,
                *inl,
                Scope {
                    unsafe_pattern: scope.unsafe_pattern.clone(),
                    unsafe_body: f(scope.unsafe_body.clone())?,
                },
            )),
            _ => self.clone(),
        })
    }

    /// Substitute expression for free variables as specified by mappings
    pub fn substs<N: PartialEq<Var<String>>>(&self, mappings: &[(N, RcExpr)]) -> RcExpr {
        match &*self.inner {
            Expr::Var(_, v) => match mappings.iter().find(|&(n, _)| n == v) {
                Some((_, replacement)) => replacement.clone(),
                None => self.clone(),
            },
            _ => self.walk(&|e: RcExpr| e.substs(mappings)),
        }
    }

    /// Substitute expressions for free variabbles based on name only
    /// (allowing free vars minted in other core translations to be
    /// bound) by binders in different core translations.
    pub fn substs_free<F: Fn(&str) -> Option<RcExpr>>(&self, substitute: &F) -> RcExpr {
        match &*self.inner {
            Expr::Var(_, Free(f)) => {
                if let Some(ref name) = f.pretty_name {
                    if let Some(replacement) = substitute(name) {
                        replacement
                    } else {
                        self.clone()
                    }
                } else {
                    self.clone()
                }
            }
            _ => self.walk(&|e: RcExpr| e.substs_free(substitute)),
        }
    }

    /// Discards metadata to retrieve top-level let expression in
    /// self.
    fn top_let(
        &self,
    ) -> Option<(
        &Smid,
        &Scope<Rec<Vec<(Binder<String>, Embed<RcExpr>)>>, RcExpr>,
        &LetType,
    )> {
        match &*self.inner {
            Expr::Let(s, scope, lt) => Some((s, scope, lt)),
            Expr::Meta(_, expr, _) => expr.top_let(),
            _ => None,
        }
    }

    /// Instantiate top-level let bindings in body
    pub fn instantiate_lets(self) -> RcExpr {
        if let Some((_, scope, _)) = self.top_let() {
            let (binders, body) = scope.clone().unbind();
            let mappings = binders
                .unrec()
                .into_iter()
                .map(|(binder, Embed(binding))| (binder, binding))
                .collect::<Vec<_>>();
            body.substs(&mappings)
        } else {
            self.clone()
        }
    }

    /// Merge two units together respecting final unit metadata.
    ///
    /// Really just a rebody operation: bindings from self may capture
    /// free variables in other.
    pub fn merge_in(self, other: RcExpr) -> Result<RcExpr, CoreError> {
        let (other_meta, other_let) = match &*other.inner {
            Expr::Meta(_, expr, meta) => (Some(meta), expr.clone()),
            _ => (None, other),
        };

        let mut expr = self.rebody(other_let);
        if let Some(m) = other_meta {
            expr = RcExpr::from(Expr::Meta(m.smid(), expr, m.clone()));
        }
        Ok(expr)
    }

    /// Merge several core expressions together, provided they have
    /// let bindings at the top level.
    pub fn merge<I>(lets: I) -> Result<Self, CoreError>
    where
        I: IntoIterator<Item = RcExpr>,
    {
        // not efficient
        let mut iter = lets.into_iter();
        let first: Result<RcExpr, CoreError> = iter.next().ok_or(CoreError::EmptyMerge());
        iter.fold(first, |l, r| l.and_then(|llet| llet.merge_in(r)))
    }
}

/// An Rc-type to wire together expressions but carrying a mark that
/// can determine whether bindings are referenced.
///
/// To create a data-carrying overlay over a `CoreExpr`, declare type
/// aliases for the `Rc` and `Expr` types and a `From<&RcExpr>`
/// implementation. For example:
///
/// ```ignore
/// # use crate::eucalypt::core::expr::*;
/// # use std::rc::Rc;
/// pub struct MyData();
/// pub type RcMarkExpr = RcFatExpr<MyData>;
/// pub type MarkExpr = Expr<RcMarkExpr>;
///
/// impl From<&RcExpr> for RcMarkExpr {
///     fn from(expr: &RcExpr) -> RcMarkExpr {
///         RcMarkExpr {
///             inner: Rc::new(fmap(&expr.inner)),
///             data: MyData()
///         }
///     }
/// }
/// ```
#[derive(Debug, Clone, BoundTerm)]
pub struct RcFatExpr<T>
where
    T: BoundTerm<String> + Clone + Default,
{
    pub inner: Rc<Expr<Self>>,
    pub data: T,
}

impl<T> From<Expr<RcFatExpr<T>>> for RcFatExpr<T>
where
    T: BoundTerm<String> + Clone + Default,
{
    fn from(expr: Expr<RcFatExpr<T>>) -> Self {
        RcFatExpr {
            inner: Rc::new(expr),
            data: T::default(),
        }
    }
}

/// Extract data from an expression using only basic inline /
/// substitution techniques such as are available during core
/// processing and transformation. These require mutable self as
/// transformations applied are kept.
pub trait Extract<T> {
    fn extract(&self) -> Option<T>;
}

impl Extract<bool> for RcExpr {
    fn extract(&self) -> Option<bool> {
        match *self.inner {
            Expr::Literal(_, Primitive::Bool(b)) => Some(b),
            _ => None,
        }
    }
}

/// TODO: symbol new type to discriminate
impl Extract<String> for RcExpr {
    fn extract(&self) -> Option<String> {
        match &*self.inner {
            Expr::Literal(_, Primitive::Str(s)) => Some(s.to_string()),
            Expr::Literal(_, Primitive::Sym(s)) => Some(s.to_string()),
            _ => None,
        }
    }
}

impl Extract<i32> for RcExpr {
    fn extract(&self) -> Option<i32> {
        match &*self.inner {
            Expr::Literal(_, Primitive::Num(n)) => n.as_i64().map(|x| x as i32),
            _ => None,
        }
    }
}

impl Extract<Number> for RcExpr {
    fn extract(&self) -> Option<Number> {
        match &*self.inner {
            Expr::Literal(_, Primitive::Num(n)) => Some(n.clone()),
            _ => None,
        }
    }
}

impl Extract<Fixity> for RcExpr {
    fn extract(&self) -> Option<Fixity> {
        if let Some(assoc) = (self as &dyn Extract<String>).extract() {
            if assoc == "left" {
                Some(Fixity::InfixLeft)
            } else {
                Some(Fixity::InfixRight)
            }
        } else {
            None
        }
    }
}

impl Extract<Input> for RcExpr {
    fn extract(&self) -> Option<Input> {
        let repr: Option<String> = self.extract();
        repr.and_then(|s| Input::from_str(s.as_ref()).ok())
    }
}

impl<P> Extract<Vec<P>> for RcExpr
where
    RcExpr: Extract<P>,
{
    fn extract(&self) -> Option<Vec<P>> {
        // single first:
        if let Some(v) = self.extract() {
            Some(vec![v])
        } else {
            match &*self.inner {
                Expr::List(_, xs) => xs.iter().map(|x| x.extract()).collect(),
                _ => None,
            }
        }
    }
}

pub fn free(n: &str) -> FreeVar<String> {
    FreeVar::fresh_named(n)
}

pub mod ops {
    use super::*;

    /// Pseudo "dot" operator which is manipulated during core
    /// processing but ultimately replaced with Core::Lookup
    pub fn dot() -> CoreExpr {
        Expr::Operator(
            Smid::default(),
            Fixity::InfixLeft,
            90,
            RcExpr::from(Expr::ErrPseudoDot),
        )
    }

    /// Pseudo "call" operator (connecting fn with arg tuple), which
    /// is manipulated during core processing but ultimately replaced
    /// with Core::Apply
    pub fn call() -> CoreExpr {
        Expr::Operator(
            Smid::default(),
            Fixity::InfixLeft,
            90,
            RcExpr::from(Expr::ErrPseudoCall),
        )
    }

    /// Pseudo "cat" operator (representing catenation), which
    /// is manipulated during core processing during cooking phase
    /// but ultimately replaced with Core::Apply
    pub fn cat() -> CoreExpr {
        Expr::Operator(
            Smid::default(),
            Fixity::InfixLeft,
            20,
            RcExpr::from(Expr::ErrPseudoCat),
        )
    }
}

/// A collection of functions for concisely creating expressions
pub mod core {
    use super::*;

    /// Create a variable expression
    pub fn var(smid: Smid, n: FreeVar<String>) -> RcExpr {
        RcExpr::from(Expr::Var(smid, Var::Free(n)))
    }

    /// Create a string
    pub fn str<T: AsRef<str>>(smid: Smid, s: T) -> RcExpr {
        RcExpr::from(Expr::Literal(smid, Primitive::Str(s.as_ref().to_string())))
    }

    /// Create a symbol
    pub fn sym<T: AsRef<str>>(smid: Smid, s: T) -> RcExpr {
        RcExpr::from(Expr::Literal(smid, Primitive::Sym(s.as_ref().to_string())))
    }

    /// Create a Core number literal
    pub fn num<N>(smid: Smid, n: N) -> RcExpr
    where
        N: Into<Number>,
    {
        RcExpr::from(Expr::Literal(smid, Primitive::Num(n.into())))
    }

    /// Return core null
    pub fn null(smid: Smid) -> RcExpr {
        RcExpr::from(Expr::Literal(smid, Primitive::Null))
    }

    /// Return a core boolean
    pub fn bool_(smid: Smid, b: bool) -> RcExpr {
        RcExpr::from(Expr::Literal(smid, Primitive::Bool(b)))
    }

    /// A reference to an intrinsic
    pub fn bif<T: AsRef<str>>(smid: Smid, name: T) -> RcExpr {
        RcExpr::from(Expr::Intrinsic(smid, name.as_ref().to_string()))
    }

    /// Create a lambda expression
    pub fn lam(smid: Smid, binders: Vec<FreeVar<String>>, body: RcExpr) -> RcExpr {
        RcExpr::from(Expr::Lam(
            smid,
            false,
            Scope::new(binders.into_iter().map(Binder).collect(), body),
        ))
    }

    /// Create an inlineable lambda expression
    pub fn inline(smid: Smid, binders: Vec<FreeVar<String>>, body: RcExpr) -> RcExpr {
        RcExpr::from(Expr::Lam(
            smid,
            true,
            Scope::new(binders.into_iter().map(Binder).collect(), body),
        ))
    }

    /// Create operator soup
    pub fn soup(smid: Smid, exprs: Vec<RcExpr>) -> RcExpr {
        RcExpr::from(Expr::Soup(smid, exprs))
    }

    /// Call pseudo-operator
    pub fn call() -> RcExpr {
        RcExpr::from(ops::call())
    }

    /// Dot pseudo-operator
    pub fn dot() -> RcExpr {
        RcExpr::from(ops::dot())
    }

    /// Cat pseudo-operator
    pub fn cat() -> RcExpr {
        RcExpr::from(ops::cat())
    }

    /// Create an argument tuple
    pub fn arg_tuple(smid: Smid, names: Vec<RcExpr>) -> RcExpr {
        RcExpr::from(Expr::ArgTuple(smid, names))
    }

    ///  Create a name (for lookup)
    pub fn name<T: AsRef<str>>(smid: Smid, name: T) -> RcExpr {
        RcExpr::from(Expr::Name(smid, name.as_ref().to_string()))
    }

    /// Create a list
    pub fn list(smid: Smid, elements: Vec<RcExpr>) -> RcExpr {
        RcExpr::from(Expr::List(smid, elements))
    }

    /// Create a block
    pub fn block(smid: Smid, decls: impl IntoIterator<Item = (String, RcExpr)>) -> RcExpr {
        RcExpr::from(Expr::Block(smid, BlockMap(decls.into_iter().collect())))
    }

    /// Create a default let
    pub fn default_let(smid: Smid, bindings: Vec<(FreeVar<String>, RcExpr)>) -> RcExpr {
        let free_vars: Vec<FreeVar<String>> = bindings.iter().map(|(k, _)| k.clone()).collect();

        let block_map = free_vars.iter().map(|fv| {
            (
                fv.pretty_name.as_ref().unwrap().clone(),
                RcExpr::from(Expr::Var(Smid::default(), Var::Free(fv.clone()))),
            )
        });

        let body_block = block(smid, block_map);

        let binders = bindings
            .iter()
            .zip(free_vars)
            .map(|((_, v), ref fv)| (Binder(fv.clone()), Embed(v.clone())))
            .collect();

        RcExpr::from(Expr::Let(
            smid,
            Scope::new(Rec::new(binders), body_block),
            LetType::DefaultBlockLet,
        ))
    }

    /// Create a let
    pub fn let_(smid: Smid, bindings: Vec<(FreeVar<String>, RcExpr)>, body: RcExpr) -> RcExpr {
        let binders = bindings
            .iter()
            .map(|(k, v)| (Binder(k.clone()), Embed(v.clone())))
            .collect();

        RcExpr::from(Expr::Let(
            smid,
            Scope::new(Rec::new(binders), body),
            LetType::OtherLet,
        ))
    }

    /// Create an operator
    pub fn op(smid: Smid, fixity: Fixity, precedence: Precedence, expr: RcExpr) -> RcExpr {
        RcExpr::from(Expr::Operator(smid, fixity, precedence, expr))
    }

    /// Wrap with metadata
    pub fn meta(smid: Smid, expr: RcExpr, metadata: RcExpr) -> RcExpr {
        RcExpr::from(Expr::Meta(smid, expr, metadata))
    }

    /// Create a lookup
    pub fn lookup(smid: Smid, target: RcExpr, name: &str, fallback: Option<RcExpr>) -> RcExpr {
        RcExpr::from(Expr::Lookup(smid, target, name.to_string(), fallback))
    }

    /// Create a function application
    pub fn app(smid: Smid, f: RcExpr, xs: Vec<RcExpr>) -> RcExpr {
        RcExpr::from(Expr::App(smid, f, xs))
    }

    /// Create an infix left associating operator
    pub fn infixl(smid: Smid, prec: Precedence, def: RcExpr) -> RcExpr {
        RcExpr::from(Expr::Operator(smid, Fixity::InfixLeft, prec, def))
    }

    /// Create an infix right associating operator
    pub fn infixr(smid: Smid, prec: Precedence, def: RcExpr) -> RcExpr {
        RcExpr::from(Expr::Operator(smid, Fixity::InfixRight, prec, def))
    }

    /// Create a prefix operator
    pub fn prefix(smid: Smid, prec: Precedence, def: RcExpr) -> RcExpr {
        RcExpr::from(Expr::Operator(smid, Fixity::UnaryPrefix, prec, def))
    }

    /// Create a postfix operator
    pub fn postfix(smid: Smid, prec: Precedence, def: RcExpr) -> RcExpr {
        RcExpr::from(Expr::Operator(smid, Fixity::UnaryPostfix, prec, def))
    }

    /// Create an section anaphor on the left of an operator
    pub fn section_anaphor_left(smid: Smid) -> RcExpr {
        RcExpr::from(Expr::ExprAnaphor(
            smid,
            Anaphor::Implicit(smid, ImplicitAnaphorSide::Left),
        ))
    }

    /// Create an section anaphor on the right of an operator
    pub fn section_anaphor_right(smid: Smid) -> RcExpr {
        RcExpr::from(Expr::ExprAnaphor(
            smid,
            Anaphor::Implicit(smid, ImplicitAnaphorSide::Right),
        ))
    }

    /// Create an expression anaphor
    pub fn expr_anaphor(smid: Smid, index: Option<i32>) -> RcExpr {
        if let Some(i) = index {
            RcExpr::from(Expr::ExprAnaphor(smid, Anaphor::ExplicitNumbered(i)))
        } else {
            RcExpr::from(Expr::ExprAnaphor(smid, Anaphor::ExplicitAnonymous(smid)))
        }
    }

    /// Target path to a lookup expression - None if empty path
    pub fn path(smid: Smid, path: &[String]) -> Option<RcExpr> {
        let mut it = path.iter();
        if let Some(base) = it.next() {
            let body = var(smid, free(base));
            Some(it.fold(body, |e, n| lookup(smid, e, n, None)))
        } else {
            None
        }
    }
}

/// Variants of dsl function with no source map ids
pub mod acore {
    use super::*;

    /// Create a variable expression
    pub fn var(n: FreeVar<String>) -> RcExpr {
        core::var(Smid::default(), n)
    }

    /// Create a string
    pub fn str<T: AsRef<str>>(s: T) -> RcExpr {
        core::str(Smid::default(), s)
    }

    /// Create a symbol
    pub fn sym<T: AsRef<str>>(s: T) -> RcExpr {
        core::sym(Smid::default(), s)
    }

    /// Create a number literal
    pub fn num<N>(n: N) -> RcExpr
    where
        N: Into<Number>,
    {
        core::num(Smid::default(), n)
    }

    /// Return core null
    pub fn null() -> RcExpr {
        core::null(Smid::default())
    }

    /// Return core boolean
    pub fn bool_(b: bool) -> RcExpr {
        core::bool_(Smid::default(), b)
    }

    /// A reference to an intrinsic
    pub fn bif<T: AsRef<str>>(name: T) -> RcExpr {
        core::bif(Smid::default(), name)
    }

    /// Create a lambbda
    pub fn lam(binders: Vec<FreeVar<String>>, body: RcExpr) -> RcExpr {
        core::lam(Smid::default(), binders, body)
    }

    /// Create an inlineable lambbda
    pub fn inline(binders: Vec<FreeVar<String>>, body: RcExpr) -> RcExpr {
        core::inline(Smid::default(), binders, body)
    }

    /// Create operator soup
    pub fn soup(exprs: Vec<RcExpr>) -> RcExpr {
        core::soup(Smid::default(), exprs)
    }

    /// Call pseudo-operator
    pub fn call() -> RcExpr {
        core::call()
    }

    /// Dot pseudo-operator
    pub fn dot() -> RcExpr {
        core::dot()
    }

    /// Cat pseudo-operator
    pub fn cat() -> RcExpr {
        core::cat()
    }

    /// Create an argument tuple
    pub fn arg_tuple(names: Vec<RcExpr>) -> RcExpr {
        core::arg_tuple(Smid::default(), names)
    }

    ///  Create a name (for lookup)
    pub fn name<T: AsRef<str>>(name: T) -> RcExpr {
        core::name(Smid::default(), name)
    }

    /// Create a list
    pub fn list(elements: Vec<RcExpr>) -> RcExpr {
        core::list(Smid::default(), elements)
    }

    /// Create a block
    pub fn block(decls: impl IntoIterator<Item = (String, RcExpr)>) -> RcExpr {
        core::block(Smid::default(), decls)
    }

    /// Create a default let
    pub fn default_let(bindings: Vec<(FreeVar<String>, RcExpr)>) -> RcExpr {
        core::default_let(Smid::default(), bindings)
    }

    /// Create a let
    pub fn let_(bindings: Vec<(FreeVar<String>, RcExpr)>, body: RcExpr) -> RcExpr {
        core::let_(Smid::default(), bindings, body)
    }

    /// Create an operator
    pub fn op(fixity: Fixity, precedence: Precedence, expr: RcExpr) -> RcExpr {
        core::op(Smid::default(), fixity, precedence, expr)
    }

    /// Wrap with metadata
    pub fn meta(expr: RcExpr, metadata: RcExpr) -> RcExpr {
        core::meta(Smid::default(), expr, metadata)
    }

    /// Create a lookup
    pub fn lookup(target: RcExpr, name: &str, fallback: Option<RcExpr>) -> RcExpr {
        core::lookup(Smid::default(), target, name, fallback)
    }

    /// Create a function application
    pub fn app(f: RcExpr, xs: Vec<RcExpr>) -> RcExpr {
        core::app(Smid::default(), f, xs)
    }

    /// Create an infix left associating operator
    pub fn infixl(prec: Precedence, def: RcExpr) -> RcExpr {
        core::infixl(Smid::default(), prec, def)
    }

    /// Create an infix right associating operator
    pub fn infixr(prec: Precedence, def: RcExpr) -> RcExpr {
        core::infixr(Smid::default(), prec, def)
    }

    /// Create a prefix operator
    pub fn prefix(prec: Precedence, def: RcExpr) -> RcExpr {
        core::prefix(Smid::default(), prec, def)
    }

    /// Create a postfix operator
    pub fn postfix(prec: Precedence, def: RcExpr) -> RcExpr {
        core::postfix(Smid::default(), prec, def)
    }

    /// Target path to a lookup expression
    pub fn path(path: &[String]) -> Option<RcExpr> {
        core::path(Smid::default(), path)
    }
}

/// Bind all free vars of an expression to test alpha equivalence
pub fn bound(expr: RcExpr) -> RcExpr {
    let fvs = expr.free_vars();
    if fvs.is_empty() {
        expr
    } else {
        let mut binders: Vec<Binder<String>> = fvs.iter().map(|v| Binder(v.clone())).collect();
        binders.sort_by_key(|b| b.0.pretty_name.as_ref().unwrap().to_string());
        RcExpr::from(Expr::Lam(Smid::default(), false, Scope::new(binders, expr)))
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    pub fn test_substs() {
        use super::acore::*;
        let x = free("x");
        let y = free("y");
        assert_term_eq!(var(x.clone()).substs(&[(x, var(y.clone()))]), var(y));

        let a = free("a");
        let b = free("b");
        let c = free("c");
        let d = free("d");
        let e = free("e");

        assert_term_eq!(
            lam(vec![a.clone(), b.clone(), c.clone()], var(d.clone()))
                .substs(&[(d.clone(), var(e.clone()))]),
            lam(vec![a.clone(), b.clone(), c.clone()], var(e.clone())),
        );
    }

    #[test]
    pub fn test_substs_free() {
        use super::acore::*;

        let a = free("a");
        let b = free("b");
        let c = free("c");
        let d = free("d");
        let e = free("e");

        let sub = |n: &str| if n == "d" { Some(var(e.clone())) } else { None };

        assert_term_eq!(
            lam(vec![a.clone(), b.clone(), c.clone()], var(d.clone())).substs_free(&sub),
            lam(vec![a.clone(), b.clone(), c.clone()], var(e.clone())),
        );
    }

    #[test]
    pub fn test_simple_rebody() {
        use super::acore::*;

        let x = free("x");
        let y = free("y");

        let original = default_let(vec![(x.clone(), num(22)), (y.clone(), num(23))]);
        let expected = let_(
            vec![(x.clone(), num(22)), (y.clone(), num(23))],
            var(y.clone()),
        );

        assert_term_eq!(original.rebody(var(y.clone())), expected);
    }

    #[test]
    pub fn test_rebody() {
        use super::acore::*;

        let x = free("x");
        let y = free("y");
        let z = free("z");

        let original = let_(
            vec![(x.clone(), num(22)), (y.clone(), num(23))],
            default_let(vec![(z.clone(), num(24))]),
        );
        let expected = let_(
            vec![(x.clone(), num(22)), (y.clone(), num(23))],
            let_(vec![(z.clone(), num(24))], var(x.clone())),
        );

        assert_term_eq!(original.rebody(var(x.clone())), expected);
    }

    #[test]
    pub fn test_rebody_metadata_transparency() {
        use super::acore::*;

        let x = free("x");
        let y = free("y");

        let original = meta(
            let_(
                vec![(x.clone(), num(22)), (y.clone(), num(23))],
                meta(var(x.clone()), str("inner meta")),
            ),
            str("outer meta"),
        );

        let expected = meta(
            let_(
                vec![(x, num(22)), (y.clone(), num(23))],
                meta(var(y.clone()), str("inner meta")),
            ),
            str("outer meta"),
        );

        assert_term_eq!(original.rebody(var(y)), expected);
    }

    #[test]
    pub fn test_rebody_with_alien_fresh_vars() {
        use super::acore::*;

        let x = free("x");
        let y = free("y");

        let original = let_(
            vec![(x.clone(), num(22)), (y.clone(), num(23))],
            var(x.clone()),
        );

        let expected = let_(vec![(x, num(22)), (y.clone(), num(23))], var(y));

        let y_other = free("y");

        assert_term_eq!(original.rebody(var(y_other)), expected);
    }

    #[test]
    pub fn test_merge_in_captures_vars_in_other() {
        use super::acore::*;

        let x = free("x");
        let y = free("y");
        let z = free("z");

        let a = free("a");
        let b = free("b");
        let c = free("c");
        let x_other = free("x");
        let y_other = free("y");

        let unit_a = let_(
            vec![(x.clone(), num(22)), (y.clone(), var(x.clone()))],
            let_(vec![(z.clone(), num(24))], var(x.clone())),
        );

        let unit_b = let_(
            vec![
                (a.clone(), num(1)),
                (b.clone(), var(a.clone())),
                (c.clone(), var(y_other.clone())),
            ],
            var(x_other.clone()),
        );

        let unit_c = unit_a.merge_in(unit_b);

        let expected = let_(
            vec![(x.clone(), num(22)), (y.clone(), var(x.clone()))],
            let_(
                vec![(z.clone(), num(24))],
                let_(
                    vec![
                        (a.clone(), num(1)),
                        (b.clone(), var(a.clone())),
                        (c.clone(), var(y.clone())),
                    ],
                    var(x.clone()),
                ),
            ),
        );

        assert_term_eq!(unit_c.unwrap(), expected);
    }

    #[test]
    pub fn test_merge_in_captures_vars_in_self_not_captured_by_other_bindings() {
        use super::acore::*;

        let x = free("x");
        let y = free("y");
        let z = free("z");
        let a_other = free("a");

        let a = free("a");
        let b = free("b");
        let c = free("c");
        let x_other = free("x");
        let y_other = free("y");

        let unit_a = let_(
            vec![(x.clone(), num(22)), (y.clone(), var(a_other.clone()))],
            let_(vec![(z.clone(), num(24))], var(x.clone())),
        );

        let unit_b = let_(
            vec![
                (a.clone(), num(1)),
                (b.clone(), var(a.clone())),
                (c.clone(), var(y_other.clone())),
            ],
            var(x_other.clone()),
        );

        let unit_c = unit_a.merge_in(unit_b);

        let expected = let_(
            vec![(x.clone(), num(22)), (y.clone(), var(a_other.clone()))],
            let_(
                vec![(z.clone(), num(24))],
                let_(
                    vec![
                        (a.clone(), num(1)),
                        (b.clone(), var(a.clone())),
                        (c.clone(), var(y.clone())),
                    ],
                    var(x.clone()),
                ),
            ),
        );

        assert_term_eq!(unit_c.unwrap(), expected);
    }

    #[test]
    pub fn test_merge_sequence_captures_left_to_right() {
        use super::acore::*;

        let x = free("x");
        let y = free("y");

        let a = free("a");
        let b = free("b");
        let x_other = free("x");

        let m = free("m");
        let n = free("n");
        let b_other = free("b");
        let y_other = free("y");

        let unit_a = let_(
            vec![(x.clone(), num(1)), (y.clone(), var(x.clone()))],
            var(x.clone()),
        );

        let unit_b = let_(
            vec![(a.clone(), num(2)), (b.clone(), var(x_other.clone()))],
            var(a.clone()),
        );

        let unit_c = let_(
            vec![(m.clone(), num(3)), (n.clone(), var(b_other.clone()))],
            var(y_other.clone()),
        );

        let expected = let_(
            vec![(x.clone(), num(1)), (y.clone(), var(x.clone()))],
            let_(
                vec![(a.clone(), num(2)), (b.clone(), var(x.clone()))],
                let_(
                    vec![(m.clone(), num(3)), (n.clone(), var(b.clone()))],
                    var(y.clone()),
                ),
            ),
        );

        assert_term_eq!(
            RcExpr::merge(vec![unit_a, unit_b, unit_c]).unwrap(),
            expected
        );
    }
}
