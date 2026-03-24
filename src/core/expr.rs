#![allow(clippy::type_complexity)]
//! Core expression syntax
use crate::common::environment::SimpleEnvironment;
use crate::common::sourcemap::HasSmid;
use crate::common::sourcemap::*;
use crate::core::binding::{BoundVar, Scope, Var};
use crate::core::error::CoreError;
use crate::syntax::input::*;
use indexmap::IndexMap;
use serde_json::Number;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;
use std::iter::FromIterator;
use std::rc::Rc;
use std::str::FromStr;

pub use crate::core::binding::Var::Free;

/// Primitive types in core
///
/// NB. Boolean and Unit ("null") are primitive in Core but user types
/// in STG.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primitive {
    Str(String),
    Sym(String),
    Num(Number),
    Bool(bool),
    Null,
}

/// Fixity of operator
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Fixity {
    Nullary,
    UnaryPrefix,
    UnaryPostfix,
    InfixLeft,
    InfixRight,
}

impl Fixity {
    pub fn arity(&self) -> u8 {
        match self {
            Fixity::Nullary => 0,
            Fixity::UnaryPrefix | Fixity::UnaryPostfix => 1,
            Fixity::InfixLeft | Fixity::InfixRight => 2,
        }
    }
}

impl Display for Fixity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Fixity::Nullary => write!(f, "nullary"),
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockMap<T>(IndexMap<String, T>);

impl<T> FromIterator<(String, T)> for BlockMap<T> {
    fn from_iter<U>(iter: U) -> Self
    where
        U: IntoIterator<Item = (String, T)>,
    {
        BlockMap(IndexMap::from_iter(iter))
    }
}

impl<T: Clone> BlockMap<T> {
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

    pub fn keys(&self) -> indexmap::map::Keys<'_, String, T> {
        self.0.keys()
    }

    pub fn values(&self) -> indexmap::map::Values<'_, String, T> {
        self.0.values()
    }

    pub fn values_mut(&mut self) -> indexmap::map::ValuesMut<'_, String, T> {
        self.0.values_mut()
    }

    pub fn iter(&self) -> indexmap::map::Iter<'_, String, T> {
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

impl<T: Clone> IntoIterator for BlockMap<T> {
    type Item = (String, T);
    type IntoIter = indexmap::map::IntoIter<String, T>;
    fn into_iter(self) -> indexmap::map::IntoIter<String, T> {
        self.0.into_iter()
    }
}

/// Used to tag lets that have a default block as body, to support
/// optimisations that don't then need to work with the body.
///
/// `DestructureBlockLet` and `DestructureListLet` mark lets generated
/// by desugaring a destructuring parameter pattern.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LetType {
    DefaultBlockLet,
    OtherLet,
    /// Let generated from a block destructuring parameter `{x y}`
    DestructureBlockLet,
    /// Let generated from a list destructuring parameter `[a, b, c]`
    DestructureListLet,
}

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
        match (self, other) {
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
        match self {
            Anaphor::ExplicitAnonymous(smid) => write!(f, "_a{smid}"),
            Anaphor::ExplicitNumbered(n) => write!(f, "_n{n}"),
            Anaphor::Implicit(smid, ImplicitAnaphorSide::Left) => write!(f, "_il{smid}"),
            Anaphor::Implicit(smid, ImplicitAnaphorSide::Right) => write!(f, "_ir{smid}"),
        }
    }
}

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

pub type LetScope<T> = Scope<Vec<(String, T)>, T>;
pub type LamScope<T> = Scope<Vec<String>, T>;

/// The main core expression type
///
/// Each variant can be embedded using the core embedding syntax:
///
/// - `Var`: `[:c-var "name"]` - Variable reference
/// - `Let`: `[:c-let {bindings} body]` - Let binding  
/// - `Intrinsic`: `[:c-bif :NAME]` - Built-in function
/// - `Literal`: `[:c-lit value]` - Primitive value
/// - `Lookup`: `[:c-lookup obj "key" fallback]` - Object property lookup
/// - `Name`: `[:c-name "identifier"]` - Unresolved name
/// - `BlockAnaphor`: `[:c-bk-ana ...]` - Block anaphora (•, •0, •1, etc.)
/// - `ExprAnaphor`: `[:c-ex-ana ...]` - Expression anaphora  
/// - `List`: `[:c-list item1 item2 ...]` - List construction
/// - `Block`: `[:c-block {key: val, ...}]` - Block/object construction
/// - `Meta`: `[:c-meta expr metadata]` - Metadata attachment
/// - `ArgTuple`: `[:c-args arg1 arg2 ...]` - Function arguments
/// - `Lam`: `[:c-lam ["param1" "param2"] body]` - Lambda function
/// - `App`: `[:c-app func [arg1 arg2]]` - Function application
/// - `Soup`: `[:c-soup item1 item2 ...]` - Operator soup (unresolved precedence)
/// - `Operator`: `[:c-op :fixity precedence expr]` - Operator with fixity info
/// - `ErrUnresolved`: `[:e-unresolved "name"]` - Unresolved variable error
/// - `ErrRedeclaration`: `[:e-redeclaration "name"]` - Redeclared variable error
/// - `ErrEliminated`: `[:e-eliminated]` - Eliminated code marker
/// - `ErrPseudoDot`: `[:e-pseudodot]` - Pseudo dot operator error
/// - `ErrPseudoCall`: `[:e-pseudocall]` - Pseudo call operator error  
/// - `ErrPseudoCat`: `[:e-pseudocat]` - Pseudo concatenation error
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<T>
where
    T: Clone,
{
    /// Variable (free or bound) - Embedding: `[:c-var "name"]`
    Var(Smid, Var),
    /// Recursive let - Embedding: `[:c-let {bindings} body]`
    Let(Smid, LetScope<T>, LetType),
    /// Reference to built-in - Embedding: `[:c-bif :NAME]`
    Intrinsic(Smid, String),
    /// Primitive core value - Embedding: `[:c-lit value]`
    Literal(Smid, Primitive),
    /// Lookup (with optional fallback) - Embedding: `[:c-lookup obj "key" fallback]`
    Lookup(Smid, T, String, Option<T>),
    /// Name (to become symbol for lookup or variable for evaluation) - Embedding: `[:c-name "identifier"]`
    Name(Smid, String),
    /// Block anaphor (implicit block parameter) - Embedding: `[:c-bk-ana ...]`
    BlockAnaphor(Smid, Anaphor<Smid, i32>),
    /// Expression anaphor (implicit expression parameter) - Embedding: `[:c-ex-ana ...]`
    ExprAnaphor(Smid, Anaphor<Smid, i32>),
    /// Literal list expression - Embedding: `[:c-list item1 item2 ...]`
    List(Smid, Vec<T>),
    /// Block (in contrast to the haskell implementation we're storing
    /// an ordered record right here) - Embedding: `[:c-block {key: val, ...}]`
    Block(Smid, BlockMap<T>),
    /// Metadata annotation (span, expr, meta) - Embedding: `[:c-meta expr metadata]`
    Meta(Smid, T, T),
    /// Tuple of arguments to apply - Embedding: `[:c-args arg1 arg2 ...]`
    ArgTuple(Smid, Vec<T>),
    /// A multi-argument lambda - Embedding: `[:c-lam ["param1" "param2"] body]`
    Lam(Smid, bool, LamScope<T>),
    /// Application of lambda or builtin (or block) - Embedding: `[:c-app func [arg1 arg2]]`
    App(Smid, T, Vec<T>),
    /// Operator soup awaiting precedence / fixity processing - Embedding: `[:c-soup item1 item2 ...]`
    /// The bool flag indicates bracket content that should be collected as a list.
    Soup(Smid, Vec<T>, bool),
    /// Operator precedence / fixity metadata (may surround definition
    /// or call) - Embedding: `[:c-op :fixity precedence expr]`
    Operator(Smid, Fixity, Precedence, T),
    /// Marker for unresolved variable - Embedding: `[:e-unresolved "name"]`
    ErrUnresolved(Smid, String),
    /// Marker for redeclaration - Embedding: `[:e-redeclaration "name"]`
    ErrRedeclaration(Smid, String),
    /// Eliminated - Embedding: `[:e-eliminated]`
    ErrEliminated,
    /// Marks pseudo dot operator eliminated before compile - Embedding: `[:e-pseudodot]`
    ErrPseudoDot,
    /// Marks pseudo call operator eliminated before compile - Embedding: `[:e-pseudocall]`
    ErrPseudoCall,
    /// Marks pseudo cat operator eliminated before compile - Embedding: `[:e-pseudocat]`
    ErrPseudoCat,
}

pub type CoreExpr = Expr<RcExpr>;

impl<T: Clone> HasSmid for Expr<T> {
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
            Soup(s, _, _) => s,
            Operator(s, _, _, _) => s,
            ErrUnresolved(s, _) => s,
            ErrRedeclaration(s, _) => s,
            _ => Smid::default(),
        }
    }
}

impl<T: Clone> Expr<T> {
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
    U: 'src + Clone,
    V: Clone + From<&'src U>,
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
        Expr::Soup(s, xs, bk) => Expr::Soup(*s, xs.iter().map(V::from).collect(), *bk),
        Expr::Operator(s, fx, p, e) => Expr::Operator(*s, *fx, *p, V::from(e)),
        Expr::Let(s, scope, t) => Expr::Let(
            *s,
            Scope {
                pattern: scope
                    .pattern
                    .iter()
                    .map(|(n, value)| (n.clone(), V::from(value)))
                    .collect(),
                body: V::from(&scope.body),
            },
            *t,
        ),
        Expr::Lam(s, inl, scope) => Expr::Lam(
            *s,
            *inl,
            Scope {
                pattern: scope.pattern.clone(),
                body: V::from(&scope.body),
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
#[derive(Debug, Clone, PartialEq)]
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
    /// Check if two RcExpr point to the same allocation.
    /// Used to detect when a transformation returned the same expression.
    #[inline]
    pub fn ptr_eq(&self, other: &RcExpr) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }

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
        core::default_let(smid, vec![(name.as_ref().to_string(), self.clone())])
    }

    /// Replace the body of a let (or nested lets) with the specified body
    pub fn rebody(&self, new_body: RcExpr) -> RcExpr {
        self.rebody_int(&mut SimpleEnvironment::new(), new_body)
    }

    /// Retrieve the bound names in a let binding so we can prepare to capture using them
    fn prepare_capture_vars(scope: &LetScope<RcExpr>) -> HashMap<String, String> {
        scope
            .pattern
            .iter()
            .map(|(k, _)| (k.clone(), k.clone()))
            .collect()
    }

    /// Replace the body of a let (or nested lets with the specified
    /// body) after altering freevars to clones of the version bound
    /// in the lets so that they can be bound.
    fn rebody_int(&self, env: &mut SimpleEnvironment<String, String>, new_body: RcExpr) -> RcExpr {
        match &*self.inner {
            Expr::Let(s, scope, let_type) => {
                let bind_vars = Self::prepare_capture_vars(scope);
                env.push(bind_vars);

                let t = scope.body.clone();
                let rebodied = t.rebody_int(env, new_body);
                // Bind any free vars in rebodied that match the binding names.
                // Use bind_free_vars (not close_expr_vars) to avoid shifting
                // existing de Bruijn indices that are already correct from a
                // previous merge step.
                let names: Vec<&str> = scope.pattern.iter().map(|(n, _)| n.as_str()).collect();
                let closed_body = bind_free_vars(&names, 0, &rebodied);
                let ret = RcExpr::from(Expr::Let(
                    *s,
                    Scope {
                        pattern: scope.pattern.clone(),
                        body: closed_body,
                    },
                    *let_type,
                ));
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
                        .map(|v| RcExpr::from(Expr::Var(Smid::default(), Var::Free(v.clone()))))
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
            Expr::Soup(s, xs, bk) => RcExpr::from(Expr::Soup(
                *s,
                xs.iter().map(|x| f(x.clone())).collect(),
                *bk,
            )),
            Expr::Operator(s, fx, p, e) => RcExpr::from(Expr::Operator(*s, *fx, *p, f(e.clone()))),
            Expr::Let(s, scope, t) => RcExpr::from(Expr::Let(
                *s,
                Scope {
                    pattern: scope
                        .pattern
                        .iter()
                        .map(|(n, value)| (n.clone(), f(value.clone())))
                        .collect(),
                    body: f(scope.body.clone()),
                },
                *t,
            )),
            Expr::Lam(s, inl, scope) => RcExpr::from(Expr::Lam(
                *s,
                *inl,
                Scope {
                    pattern: scope.pattern.clone(),
                    body: f(scope.body.clone()),
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
    /// binding status intact.
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
            Expr::Soup(s, xs, bk) => RcExpr::from(Expr::Soup(
                *s,
                xs.iter()
                    .map(|x| f(x.clone()))
                    .collect::<Result<Vec<RcExpr>, E>>()?,
                *bk,
            )),
            Expr::Operator(s, fx, p, e) => RcExpr::from(Expr::Operator(*s, *fx, *p, f(e.clone())?)),
            Expr::Let(s, scope, t) => {
                let bindings = scope
                    .pattern
                    .iter()
                    .map(|(n, value)| f(value.clone()).map(|v| (n.clone(), v)))
                    .collect::<Result<Vec<(String, RcExpr)>, E>>()?;
                RcExpr::from(Expr::Let(
                    *s,
                    Scope {
                        pattern: bindings,
                        body: f(scope.body.clone())?,
                    },
                    *t,
                ))
            }
            Expr::Lam(s, inl, scope) => RcExpr::from(Expr::Lam(
                *s,
                *inl,
                Scope {
                    pattern: scope.pattern.clone(),
                    body: f(scope.body.clone())?,
                },
            )),
            _ => self.clone(),
        })
    }

    /// Optimized walk that only rebuilds when children actually change.
    /// Uses pointer equality to detect unchanged subtrees, avoiding
    /// unnecessary allocations during tree transformations.
    ///
    /// This is more efficient than `walk` when most subtrees remain unchanged,
    /// as it avoids creating new Rc allocations for unchanged nodes.
    pub fn try_walk<F: Fn(&RcExpr) -> RcExpr>(&self, f: &F) -> RcExpr {
        match &*self.inner {
            Expr::Lookup(s, e, n, fb) => {
                let new_e = f(e);
                let new_fb = fb.as_ref().map(f);
                let e_same = new_e.ptr_eq(e);
                let fb_same = match (&new_fb, fb) {
                    (Some(a), Some(b)) => a.ptr_eq(b),
                    (None, None) => true,
                    _ => false,
                };
                if e_same && fb_same {
                    self.clone()
                } else {
                    RcExpr::from(Expr::Lookup(*s, new_e, n.clone(), new_fb))
                }
            }
            Expr::List(s, xs) => {
                let new_xs: Vec<_> = xs.iter().map(f).collect();
                let all_same = xs.iter().zip(new_xs.iter()).all(|(a, b)| a.ptr_eq(b));
                if all_same {
                    self.clone()
                } else {
                    RcExpr::from(Expr::List(*s, new_xs))
                }
            }
            Expr::Block(s, block_map) => {
                let new_items: Vec<_> = block_map.iter().map(|(k, v)| (k, f(v))).collect();
                let all_same = block_map
                    .iter()
                    .zip(new_items.iter())
                    .all(|((_, v), (_, new_v))| v.ptr_eq(new_v));
                if all_same {
                    self.clone()
                } else {
                    RcExpr::from(Expr::Block(
                        *s,
                        new_items.into_iter().map(|(k, v)| (k.clone(), v)).collect(),
                    ))
                }
            }
            Expr::Meta(s, e, m) => {
                let new_e = f(e);
                let new_m = f(m);
                if new_e.ptr_eq(e) && new_m.ptr_eq(m) {
                    self.clone()
                } else {
                    RcExpr::from(Expr::Meta(*s, new_e, new_m))
                }
            }
            Expr::ArgTuple(s, xs) => {
                let new_xs: Vec<_> = xs.iter().map(f).collect();
                let all_same = xs.iter().zip(new_xs.iter()).all(|(a, b)| a.ptr_eq(b));
                if all_same {
                    self.clone()
                } else {
                    RcExpr::from(Expr::ArgTuple(*s, new_xs))
                }
            }
            Expr::App(s, g, xs) => {
                let new_g = f(g);
                let new_xs: Vec<_> = xs.iter().map(f).collect();
                let g_same = new_g.ptr_eq(g);
                let xs_same = xs.iter().zip(new_xs.iter()).all(|(a, b)| a.ptr_eq(b));
                if g_same && xs_same {
                    self.clone()
                } else {
                    RcExpr::from(Expr::App(*s, new_g, new_xs))
                }
            }
            Expr::Soup(s, xs, bk) => {
                let new_xs: Vec<_> = xs.iter().map(f).collect();
                let all_same = xs.iter().zip(new_xs.iter()).all(|(a, b)| a.ptr_eq(b));
                if all_same {
                    self.clone()
                } else {
                    RcExpr::from(Expr::Soup(*s, new_xs, *bk))
                }
            }
            Expr::Operator(s, fx, p, e) => {
                let new_e = f(e);
                if new_e.ptr_eq(e) {
                    self.clone()
                } else {
                    RcExpr::from(Expr::Operator(*s, *fx, *p, new_e))
                }
            }
            Expr::Let(s, scope, t) => {
                let new_bindings: Vec<_> = scope
                    .pattern
                    .iter()
                    .map(|(n, value)| (n, f(value)))
                    .collect();
                let new_body = f(&scope.body);

                let bindings_same = scope
                    .pattern
                    .iter()
                    .zip(new_bindings.iter())
                    .all(|((_, v), (_, new_v))| v.ptr_eq(new_v));
                let body_same = scope.body.ptr_eq(&new_body);

                if bindings_same && body_same {
                    self.clone()
                } else {
                    RcExpr::from(Expr::Let(
                        *s,
                        Scope {
                            pattern: new_bindings
                                .into_iter()
                                .map(|(n, v)| (n.clone(), v))
                                .collect(),
                            body: new_body,
                        },
                        *t,
                    ))
                }
            }
            Expr::Lam(s, inl, scope) => {
                let new_body = f(&scope.body);
                if scope.body.ptr_eq(&new_body) {
                    self.clone()
                } else {
                    RcExpr::from(Expr::Lam(
                        *s,
                        *inl,
                        Scope {
                            pattern: scope.pattern.clone(),
                            body: new_body,
                        },
                    ))
                }
            }
            _ => self.clone(),
        }
    }

    /// Optimized fallible walk that only rebuilds when children actually change.
    /// Uses pointer equality to detect unchanged subtrees, avoiding
    /// unnecessary allocations during tree transformations.
    pub fn try_walk_safe<E, F: FnMut(&RcExpr) -> Result<RcExpr, E>>(
        &self,
        f: &mut F,
    ) -> Result<RcExpr, E> {
        Ok(match &*self.inner {
            Expr::Lookup(s, e, n, fb) => {
                let new_e = f(e)?;
                let new_fb = match fb {
                    Some(expr) => Some(f(expr)?),
                    None => None,
                };
                let e_same = new_e.ptr_eq(e);
                let fb_same = match (&new_fb, fb) {
                    (Some(a), Some(b)) => a.ptr_eq(b),
                    (None, None) => true,
                    _ => false,
                };
                if e_same && fb_same {
                    self.clone()
                } else {
                    RcExpr::from(Expr::Lookup(*s, new_e, n.clone(), new_fb))
                }
            }
            Expr::List(s, xs) => {
                let new_xs: Vec<RcExpr> = xs.iter().map(f).collect::<Result<_, E>>()?;
                let all_same = xs.iter().zip(new_xs.iter()).all(|(a, b)| a.ptr_eq(b));
                if all_same {
                    self.clone()
                } else {
                    RcExpr::from(Expr::List(*s, new_xs))
                }
            }
            Expr::Block(s, block_map) => {
                let new_items: Vec<_> = block_map
                    .iter()
                    .map(|(k, v)| f(v).map(|new_v| (k, v, new_v)))
                    .collect::<Result<_, E>>()?;
                let all_same = new_items.iter().all(|(_, v, new_v)| v.ptr_eq(new_v));
                if all_same {
                    self.clone()
                } else {
                    RcExpr::from(Expr::Block(
                        *s,
                        new_items
                            .into_iter()
                            .map(|(k, _, v)| (k.clone(), v))
                            .collect(),
                    ))
                }
            }
            Expr::Meta(s, e, m) => {
                let new_e = f(e)?;
                let new_m = f(m)?;
                if new_e.ptr_eq(e) && new_m.ptr_eq(m) {
                    self.clone()
                } else {
                    RcExpr::from(Expr::Meta(*s, new_e, new_m))
                }
            }
            Expr::ArgTuple(s, xs) => {
                let new_xs: Vec<RcExpr> = xs.iter().map(f).collect::<Result<_, E>>()?;
                let all_same = xs.iter().zip(new_xs.iter()).all(|(a, b)| a.ptr_eq(b));
                if all_same {
                    self.clone()
                } else {
                    RcExpr::from(Expr::ArgTuple(*s, new_xs))
                }
            }
            Expr::App(s, g, xs) => {
                let new_g = f(g)?;
                let new_xs: Vec<RcExpr> = xs.iter().map(f).collect::<Result<_, E>>()?;
                let g_same = new_g.ptr_eq(g);
                let xs_same = xs.iter().zip(new_xs.iter()).all(|(a, b)| a.ptr_eq(b));
                if g_same && xs_same {
                    self.clone()
                } else {
                    RcExpr::from(Expr::App(*s, new_g, new_xs))
                }
            }
            Expr::Soup(s, xs, bk) => {
                let new_xs: Vec<RcExpr> = xs.iter().map(f).collect::<Result<_, E>>()?;
                let all_same = xs.iter().zip(new_xs.iter()).all(|(a, b)| a.ptr_eq(b));
                if all_same {
                    self.clone()
                } else {
                    RcExpr::from(Expr::Soup(*s, new_xs, *bk))
                }
            }
            Expr::Operator(s, fx, p, e) => {
                let new_e = f(e)?;
                if new_e.ptr_eq(e) {
                    self.clone()
                } else {
                    RcExpr::from(Expr::Operator(*s, *fx, *p, new_e))
                }
            }
            Expr::Let(s, scope, t) => {
                let new_bindings: Vec<_> = scope
                    .pattern
                    .iter()
                    .map(|(n, value)| f(value).map(|new_v| (n, value, new_v)))
                    .collect::<Result<_, E>>()?;
                let new_body = f(&scope.body)?;

                let bindings_same = new_bindings.iter().all(|(_, v, new_v)| v.ptr_eq(new_v));
                let body_same = scope.body.ptr_eq(&new_body);

                if bindings_same && body_same {
                    self.clone()
                } else {
                    RcExpr::from(Expr::Let(
                        *s,
                        Scope {
                            pattern: new_bindings
                                .into_iter()
                                .map(|(n, _, v)| (n.clone(), v))
                                .collect(),
                            body: new_body,
                        },
                        *t,
                    ))
                }
            }
            Expr::Lam(s, inl, scope) => {
                let new_body = f(&scope.body)?;
                if scope.body.ptr_eq(&new_body) {
                    self.clone()
                } else {
                    RcExpr::from(Expr::Lam(
                        *s,
                        *inl,
                        Scope {
                            pattern: scope.pattern.clone(),
                            body: new_body,
                        },
                    ))
                }
            }
            _ => self.clone(),
        })
    }

    /// Substitute expression for free variables as specified by mappings.
    /// Uses optimized try_walk to avoid unnecessary allocations.
    pub fn substs(&self, mappings: &[(String, RcExpr)]) -> RcExpr {
        match &*self.inner {
            Expr::Var(_, Var::Free(name)) => match mappings.iter().find(|(n, _)| n == name) {
                Some((_, replacement)) => replacement.clone(),
                None => self.clone(),
            },
            _ => self.try_walk(&|e: &RcExpr| e.substs(mappings)),
        }
    }

    /// Substitute expressions for free variables based on name only
    /// (allowing free vars minted in other core translations to be
    /// bound by binders in different core translations).
    /// Uses optimized try_walk to avoid unnecessary allocations.
    pub fn substs_free<F: Fn(&str) -> Option<RcExpr>>(&self, substitute: &F) -> RcExpr {
        match &*self.inner {
            Expr::Var(original_smid, Var::Free(name)) => {
                if let Some(replacement) = substitute(name) {
                    // If the replacement was built with Smid::default() but the
                    // original Var carried a meaningful call-site Smid, re-stamp
                    // the Smid so that source locations are not lost during merge.
                    if original_smid.is_valid() {
                        if let Expr::Var(rep_smid, rep_var) = &*replacement.inner {
                            if !rep_smid.is_valid() {
                                return RcExpr::from(Expr::Var(*original_smid, rep_var.clone()));
                            }
                        }
                    }
                    replacement
                } else {
                    self.clone()
                }
            }
            _ => self.try_walk(&|e: &RcExpr| e.substs_free(substitute)),
        }
    }

    /// Discards metadata to retrieve top-level let expression in
    /// self.
    fn top_let(&self) -> Option<(&Smid, &LetScope<RcExpr>, &LetType)> {
        match &*self.inner {
            Expr::Let(s, scope, lt) => Some((s, scope, lt)),
            Expr::Meta(_, expr, _) => expr.top_let(),
            _ => None,
        }
    }

    /// Instantiate top-level let bindings in body
    pub fn instantiate_lets(self) -> RcExpr {
        if let Some((_, scope, _)) = self.top_let() {
            let (open_bindings, open_body) = open_let_scope_full(scope);
            open_body.substs(&open_bindings)
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
#[derive(Debug, Clone)]
pub struct RcFatExpr<T>
where
    T: Clone + Default,
{
    pub inner: Rc<Expr<Self>>,
    pub data: T,
}

impl<T> From<Expr<RcFatExpr<T>>> for RcFatExpr<T>
where
    T: Clone + Default,
{
    fn from(expr: Expr<RcFatExpr<T>>) -> Self {
        RcFatExpr {
            inner: Rc::new(expr),
            data: T::default(),
        }
    }
}

/// Extract data from a core expression.
///
/// Implementations are used to read processing-time metadata from
/// core representations and therefore using only basic inline /
/// substitution techniques such as are available during core
/// processing and transformation.
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

pub fn free(n: &str) -> String {
    n.to_string()
}

/// Close a let scope: convert free variables matching binding names to bound variables.
/// Existing bound variable scope indices are incremented to account for the new scope.
pub fn close_let_scope(bindings: Vec<(String, RcExpr)>, body: RcExpr) -> LetScope<RcExpr> {
    let names: Vec<String> = bindings.iter().map(|(n, _)| n.clone()).collect();
    let name_refs: Vec<&str> = names.iter().map(|n| n.as_str()).collect();
    let closed_bindings: Vec<(String, RcExpr)> = bindings
        .into_iter()
        .map(|(n, v)| (n, close_expr_vars(&name_refs, 0, &v)))
        .collect();
    let closed_body = close_expr_vars(&name_refs, 0, &body);
    Scope {
        pattern: closed_bindings,
        body: closed_body,
    }
}

/// Close a lam scope: convert free variables matching parameter names to bound variables.
pub fn close_lam_scope(params: Vec<String>, body: RcExpr) -> LamScope<RcExpr> {
    let names: Vec<&str> = params.iter().map(|n| n.as_str()).collect();
    let closed_body = close_expr_vars(&names, 0, &body);
    Scope {
        pattern: params,
        body: closed_body,
    }
}

/// Open a let scope: convert bound variables (scope==0) back to free variables.
/// Existing bound variable scope indices > 0 are decremented.
pub fn open_let_scope(scope: &LetScope<RcExpr>) -> RcExpr {
    let names: Vec<Option<&str>> = scope
        .pattern
        .iter()
        .map(|(n, _)| Some(n.as_str()))
        .collect();
    open_expr_vars(&names, 0, &scope.body)
}

/// Open a let scope fully — returns both the opened binding values AND the opened body.
///
/// Equivalent to `scope.unbind()` in the old moniker API.
/// Converts `Var::Bound(scope=0, ...)` in ALL positions (bindings + body) back to
/// `Var::Free(name)`, and decrements scope indices of outer references by 1.
///
/// Used when you need to substitute free variables INTO the binding values,
/// e.g. in `distribute` (inline pass) which inlines lambdas into other bindings.
pub fn open_let_scope_full(scope: &LetScope<RcExpr>) -> (Vec<(String, RcExpr)>, RcExpr) {
    let names: Vec<Option<&str>> = scope
        .pattern
        .iter()
        .map(|(n, _)| Some(n.as_str()))
        .collect();
    let open_bindings = scope
        .pattern
        .iter()
        .map(|(n, v)| (n.clone(), open_expr_vars(&names, 0, v)))
        .collect();
    let open_body = open_expr_vars(&names, 0, &scope.body);
    (open_bindings, open_body)
}

/// Open a lam scope: same but for lambda.
pub fn open_lam_scope(scope: &LamScope<RcExpr>) -> RcExpr {
    let names: Vec<Option<&str>> = scope.pattern.iter().map(|n| Some(n.as_str())).collect();
    open_expr_vars(&names, 0, &scope.body)
}

/// Walk an expression, converting `Var::Free(name)` where `name` matches any of `names`
/// into `Var::Bound(BoundVar { scope: depth, binder: i, name: Some(name) })`,
/// WITHOUT shifting existing `Var::Bound` scope indices.
///
/// Used in `rebody_int` when the expression already has correct de Bruijn indices
/// from a previous scope-building step, and we only need to bind additional free
/// variables without disturbing the existing structure.
pub fn bind_free_vars(names: &[&str], depth: u32, expr: &RcExpr) -> RcExpr {
    match &*expr.inner {
        Expr::Var(s, Var::Free(name)) => {
            if let Some(i) = names.iter().position(|&n| n == name.as_str()) {
                RcExpr::from(Expr::Var(
                    *s,
                    Var::Bound(BoundVar {
                        scope: depth,
                        binder: i as u32,
                        name: Some(name.clone()),
                    }),
                ))
            } else {
                expr.clone()
            }
        }
        Expr::Let(s, scope, t) => {
            let new_bindings = scope
                .pattern
                .iter()
                .map(|(n, v)| (n.clone(), bind_free_vars(names, depth + 1, v)))
                .collect();
            let new_body = bind_free_vars(names, depth + 1, &scope.body);
            RcExpr::from(Expr::Let(
                *s,
                Scope {
                    pattern: new_bindings,
                    body: new_body,
                },
                *t,
            ))
        }
        Expr::Lam(s, inl, scope) => {
            let new_body = bind_free_vars(names, depth + 1, &scope.body);
            RcExpr::from(Expr::Lam(
                *s,
                *inl,
                Scope {
                    pattern: scope.pattern.clone(),
                    body: new_body,
                },
            ))
        }
        _ => expr.walk(&|e| bind_free_vars(names, depth, &e)),
    }
}

/// Walk an expression, converting `Var::Free(name)` where `name` matches any of `names`
/// into `Var::Bound(BoundVar { scope: depth, binder: i, name: Some(name) })`.
/// Also increments `scope` of any existing `Var::Bound` with `scope >= depth`.
pub fn close_expr_vars(names: &[&str], depth: u32, expr: &RcExpr) -> RcExpr {
    match &*expr.inner {
        Expr::Var(s, Var::Free(name)) => {
            if let Some(i) = names.iter().position(|&n| n == name.as_str()) {
                RcExpr::from(Expr::Var(
                    *s,
                    Var::Bound(BoundVar {
                        scope: depth,
                        binder: i as u32,
                        name: Some(name.clone()),
                    }),
                ))
            } else {
                expr.clone()
            }
        }
        Expr::Var(s, Var::Bound(bv)) if bv.scope >= depth => RcExpr::from(Expr::Var(
            *s,
            Var::Bound(BoundVar {
                scope: bv.scope + 1,
                binder: bv.binder,
                name: bv.name.clone(),
            }),
        )),
        Expr::Let(s, scope, t) => {
            let new_bindings = scope
                .pattern
                .iter()
                .map(|(n, v)| (n.clone(), close_expr_vars(names, depth + 1, v)))
                .collect();
            let new_body = close_expr_vars(names, depth + 1, &scope.body);
            RcExpr::from(Expr::Let(
                *s,
                Scope {
                    pattern: new_bindings,
                    body: new_body,
                },
                *t,
            ))
        }
        Expr::Lam(s, inl, scope) => {
            let new_body = close_expr_vars(names, depth + 1, &scope.body);
            RcExpr::from(Expr::Lam(
                *s,
                *inl,
                Scope {
                    pattern: scope.pattern.clone(),
                    body: new_body,
                },
            ))
        }
        _ => expr.walk(&|e| close_expr_vars(names, depth, &e)),
    }
}

/// Walk an expression, converting `Var::Bound` with `scope == depth` back to `Var::Free`.
/// Decrements `scope` of any `Var::Bound` with `scope > depth`.
pub fn open_expr_vars(names: &[Option<&str>], depth: u32, expr: &RcExpr) -> RcExpr {
    match &*expr.inner {
        Expr::Var(s, Var::Bound(bv)) => {
            if bv.scope == depth {
                let name = names
                    .get(bv.binder as usize)
                    .and_then(|n| *n)
                    .or(bv.name.as_deref())
                    .unwrap_or("_")
                    .to_string();
                RcExpr::from(Expr::Var(*s, Var::Free(name)))
            } else if bv.scope > depth {
                RcExpr::from(Expr::Var(
                    *s,
                    Var::Bound(BoundVar {
                        scope: bv.scope - 1,
                        binder: bv.binder,
                        name: bv.name.clone(),
                    }),
                ))
            } else {
                expr.clone()
            }
        }
        Expr::Let(s, scope, t) => {
            let new_bindings = scope
                .pattern
                .iter()
                .map(|(n, v)| (n.clone(), open_expr_vars(names, depth + 1, v)))
                .collect();
            let new_body = open_expr_vars(names, depth + 1, &scope.body);
            RcExpr::from(Expr::Let(
                *s,
                Scope {
                    pattern: new_bindings,
                    body: new_body,
                },
                *t,
            ))
        }
        Expr::Lam(s, inl, scope) => {
            let new_body = open_expr_vars(names, depth + 1, &scope.body);
            RcExpr::from(Expr::Lam(
                *s,
                *inl,
                Scope {
                    pattern: scope.pattern.clone(),
                    body: new_body,
                },
            ))
        }
        _ => expr.walk(&|e| open_expr_vars(names, depth, &e)),
    }
}

/// Collect all free variable names in an expression.
pub fn free_vars(expr: &RcExpr) -> std::collections::HashSet<String> {
    let mut result = std::collections::HashSet::new();
    collect_free_vars(expr, &mut result);
    result
}

fn collect_free_vars(expr: &RcExpr, result: &mut std::collections::HashSet<String>) {
    match &*expr.inner {
        Expr::Var(_, Var::Free(name)) => {
            result.insert(name.clone());
        }
        Expr::Var(_, Var::Bound(_)) => {}
        Expr::Let(_, scope, _) => {
            for (_, v) in &scope.pattern {
                collect_free_vars(v, result);
            }
            collect_free_vars(&scope.body, result);
        }
        Expr::Lam(_, _, scope) => {
            collect_free_vars(&scope.body, result);
        }
        Expr::Lookup(_, e, _, fb) => {
            collect_free_vars(e, result);
            if let Some(f) = fb {
                collect_free_vars(f, result);
            }
        }
        Expr::List(_, xs) | Expr::ArgTuple(_, xs) => {
            for x in xs {
                collect_free_vars(x, result);
            }
        }
        Expr::Block(_, bm) => {
            for (_, v) in bm.iter() {
                collect_free_vars(v, result);
            }
        }
        Expr::Meta(_, e, m) => {
            collect_free_vars(e, result);
            collect_free_vars(m, result);
        }
        Expr::App(_, f, xs) => {
            collect_free_vars(f, result);
            for x in xs {
                collect_free_vars(x, result);
            }
        }
        Expr::Soup(_, xs, _) => {
            for x in xs {
                collect_free_vars(x, result);
            }
        }
        Expr::Operator(_, _, _, e) => collect_free_vars(e, result),
        _ => {}
    }
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
    pub fn var(smid: Smid, n: String) -> RcExpr {
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
    pub fn lam(smid: Smid, binders: Vec<String>, body: RcExpr) -> RcExpr {
        RcExpr::from(Expr::Lam(smid, false, close_lam_scope(binders, body)))
    }

    /// Create an inlineable lambda expression
    pub fn inline(smid: Smid, binders: Vec<String>, body: RcExpr) -> RcExpr {
        RcExpr::from(Expr::Lam(smid, true, close_lam_scope(binders, body)))
    }

    /// Create operator soup
    pub fn soup(smid: Smid, exprs: Vec<RcExpr>) -> RcExpr {
        RcExpr::from(Expr::Soup(smid, exprs, false))
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
    pub fn default_let(smid: Smid, bindings: Vec<(String, RcExpr)>) -> RcExpr {
        let block_map = bindings.iter().map(|(name, _)| {
            (
                name.clone(),
                RcExpr::from(Expr::Var(Smid::default(), Var::Free(name.clone()))),
            )
        });
        let body_block = block(smid, block_map);
        RcExpr::from(Expr::Let(
            smid,
            close_let_scope(bindings, body_block),
            LetType::DefaultBlockLet,
        ))
    }

    /// Create a let
    pub fn let_(smid: Smid, bindings: Vec<(String, RcExpr)>, body: RcExpr) -> RcExpr {
        RcExpr::from(Expr::Let(
            smid,
            close_let_scope(bindings, body),
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

    /// Create a nullary operator
    pub fn nullary(smid: Smid, prec: Precedence, def: RcExpr) -> RcExpr {
        RcExpr::from(Expr::Operator(smid, Fixity::Nullary, prec, def))
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
            let body = var(smid, base.clone());
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
    pub fn var(n: String) -> RcExpr {
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

    /// Create a lambda
    pub fn lam(binders: Vec<String>, body: RcExpr) -> RcExpr {
        core::lam(Smid::default(), binders, body)
    }

    /// Create an inlineable lambda
    pub fn inline(binders: Vec<String>, body: RcExpr) -> RcExpr {
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
    pub fn default_let(bindings: Vec<(String, RcExpr)>) -> RcExpr {
        core::default_let(Smid::default(), bindings)
    }

    /// Create a let
    pub fn let_(bindings: Vec<(String, RcExpr)>, body: RcExpr) -> RcExpr {
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

    /// Create a nullary operator
    pub fn nullary(prec: Precedence, def: RcExpr) -> RcExpr {
        core::nullary(Smid::default(), prec, def)
    }

    /// Target path to a lookup expression
    pub fn path(path: &[String]) -> Option<RcExpr> {
        core::path(Smid::default(), path)
    }
}

/// Bind all free vars of an expression to test alpha equivalence
pub fn bound(expr: RcExpr) -> RcExpr {
    let fvs = free_vars(&expr);
    if fvs.is_empty() {
        expr
    } else {
        let mut binders: Vec<String> = fvs.into_iter().collect();
        binders.sort();
        RcExpr::from(Expr::Lam(
            Smid::default(),
            false,
            close_lam_scope(binders, expr),
        ))
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    /// Normalise an expression for structural comparison in tests.
    ///
    /// Replaces all `LetType` variants with `OtherLet`, all `Smid` fields with
    /// `Smid::default()`, and renames lambda parameters to canonical `_p0`, `_p1`,
    /// … names (updating corresponding bound variables).  This restores the
    /// alpha-equivalence semantics of the old `moniker::assert_term_eq!`.
    pub fn alpha_norm(expr: RcExpr) -> RcExpr {
        alpha_norm_inner(&expr, &mut 0)
    }

    fn alpha_norm_inner(expr: &RcExpr, counter: &mut u32) -> RcExpr {
        match &*expr.inner {
            Expr::Var(_, v) => RcExpr::from(Expr::Var(Smid::default(), v.clone())),
            Expr::Name(_, n) => RcExpr::from(Expr::Name(Smid::default(), n.clone())),
            Expr::Literal(_, p) => RcExpr::from(Expr::Literal(Smid::default(), p.clone())),
            Expr::Intrinsic(_, n) => RcExpr::from(Expr::Intrinsic(Smid::default(), n.clone())),
            Expr::Let(_, scope, _) => {
                let new_pattern = scope
                    .pattern
                    .iter()
                    .map(|(k, v)| (k.clone(), alpha_norm_inner(v, counter)))
                    .collect();
                let new_body = alpha_norm_inner(&scope.body, counter);
                RcExpr::from(Expr::Let(
                    Smid::default(),
                    Scope {
                        pattern: new_pattern,
                        body: new_body,
                    },
                    LetType::OtherLet,
                ))
            }
            Expr::Lam(_, ann, scope) => {
                // Rename parameters to canonical _p0, _p1, ...
                let new_params: Vec<String> = scope
                    .pattern
                    .iter()
                    .map(|_| {
                        let n = format!("_p{}", *counter);
                        *counter += 1;
                        n
                    })
                    .collect();
                // Rename bound vars referencing scope=0 in the body
                let renamed_body = rename_lam_params(&scope.body, &scope.pattern, &new_params, 0);
                let new_body = alpha_norm_inner(&renamed_body, counter);
                RcExpr::from(Expr::Lam(
                    Smid::default(),
                    *ann,
                    Scope {
                        pattern: new_params,
                        body: new_body,
                    },
                ))
            }
            Expr::App(_, f, args) => RcExpr::from(Expr::App(
                Smid::default(),
                alpha_norm_inner(f, counter),
                args.iter().map(|a| alpha_norm_inner(a, counter)).collect(),
            )),
            Expr::Meta(_, e, m) => RcExpr::from(Expr::Meta(
                Smid::default(),
                alpha_norm_inner(e, counter),
                alpha_norm_inner(m, counter),
            )),
            Expr::Lookup(_, obj, key, fb) => RcExpr::from(Expr::Lookup(
                Smid::default(),
                alpha_norm_inner(obj, counter),
                key.clone(),
                fb.as_ref().map(|f| alpha_norm_inner(f, counter)),
            )),
            Expr::List(_, items) => RcExpr::from(Expr::List(
                Smid::default(),
                items.iter().map(|i| alpha_norm_inner(i, counter)).collect(),
            )),
            Expr::Block(_, bm) => RcExpr::from(Expr::Block(
                Smid::default(),
                bm.iter()
                    .map(|(k, v)| (k.clone(), alpha_norm_inner(v, counter)))
                    .collect(),
            )),
            Expr::Soup(_, items, bk) => RcExpr::from(Expr::Soup(
                Smid::default(),
                items.iter().map(|i| alpha_norm_inner(i, counter)).collect(),
                *bk,
            )),
            Expr::ArgTuple(_, args) => RcExpr::from(Expr::ArgTuple(
                Smid::default(),
                args.iter().map(|a| alpha_norm_inner(a, counter)).collect(),
            )),
            Expr::Operator(_, fix, prec, e) => RcExpr::from(Expr::Operator(
                Smid::default(),
                *fix,
                *prec,
                alpha_norm_inner(e, counter),
            )),
            Expr::ExprAnaphor(_, ana) => {
                let norm_ana = normalise_anaphor(ana);
                RcExpr::from(Expr::ExprAnaphor(Smid::default(), norm_ana))
            }
            Expr::BlockAnaphor(_, ana) => {
                let norm_ana = normalise_anaphor(ana);
                RcExpr::from(Expr::BlockAnaphor(Smid::default(), norm_ana))
            }
            Expr::ErrUnresolved(_, n) => {
                RcExpr::from(Expr::ErrUnresolved(Smid::default(), n.clone()))
            }
            Expr::ErrRedeclaration(_, n) => {
                RcExpr::from(Expr::ErrRedeclaration(Smid::default(), n.clone()))
            }
            _ => expr.clone(),
        }
    }

    fn normalise_anaphor(ana: &Anaphor<Smid, i32>) -> Anaphor<Smid, i32> {
        match ana {
            // Normalise: strip smid and unify Left/Right (tests use term_eq which ignores
            // implicit anaphor side)
            Anaphor::Implicit(_, _) => {
                Anaphor::Implicit(Smid::default(), ImplicitAnaphorSide::Left)
            }
            Anaphor::ExplicitAnonymous(_) => Anaphor::ExplicitAnonymous(Smid::default()),
            Anaphor::ExplicitNumbered(n) => Anaphor::ExplicitNumbered(*n),
        }
    }

    /// Rename lambda parameter names in an expression body from `old_names` to `new_names`,
    /// updating `Var::Bound` with `scope == depth` and also `Var::Bound.name` hints.
    fn rename_lam_params(
        expr: &RcExpr,
        old_names: &[String],
        new_names: &[String],
        depth: u32,
    ) -> RcExpr {
        match &*expr.inner {
            Expr::Var(s, Var::Bound(bv)) => {
                let mut new_bv = bv.clone();
                if bv.scope == depth {
                    if let Some(ref old_name) = bv.name {
                        if let Some(pos) = old_names.iter().position(|n| n == old_name) {
                            new_bv.name = Some(new_names[pos].clone());
                        }
                    }
                }
                RcExpr::from(Expr::Var(*s, Var::Bound(new_bv)))
            }
            Expr::Let(s, scope, t) => {
                let new_bindings = scope
                    .pattern
                    .iter()
                    .map(|(k, v)| {
                        (
                            k.clone(),
                            rename_lam_params(v, old_names, new_names, depth + 1),
                        )
                    })
                    .collect();
                let new_body = rename_lam_params(&scope.body, old_names, new_names, depth + 1);
                RcExpr::from(Expr::Let(
                    *s,
                    Scope {
                        pattern: new_bindings,
                        body: new_body,
                    },
                    *t,
                ))
            }
            Expr::Lam(s, ann, scope) => {
                let new_body = rename_lam_params(&scope.body, old_names, new_names, depth + 1);
                RcExpr::from(Expr::Lam(
                    *s,
                    *ann,
                    Scope {
                        pattern: scope.pattern.clone(),
                        body: new_body,
                    },
                ))
            }
            _ => expr.walk(&|e| rename_lam_params(&e, old_names, new_names, depth)),
        }
    }

    // Keep the simpler norm() for tests that only need LetType normalisation
    fn norm(expr: RcExpr) -> RcExpr {
        alpha_norm(expr)
    }

    /// Strip Smids without changing LetType or lambda param names.
    /// Used in import tests where LetType must be preserved.
    pub fn smid_strip(expr: RcExpr) -> RcExpr {
        smid_strip_inner(&expr)
    }

    fn smid_strip_inner(expr: &RcExpr) -> RcExpr {
        match &*expr.inner {
            Expr::Var(_, v) => RcExpr::from(Expr::Var(Smid::default(), v.clone())),
            Expr::Name(_, n) => RcExpr::from(Expr::Name(Smid::default(), n.clone())),
            Expr::Literal(_, p) => RcExpr::from(Expr::Literal(Smid::default(), p.clone())),
            Expr::Intrinsic(_, n) => RcExpr::from(Expr::Intrinsic(Smid::default(), n.clone())),
            Expr::Let(_, scope, lt) => {
                let new_pattern = scope
                    .pattern
                    .iter()
                    .map(|(k, v)| (k.clone(), smid_strip_inner(v)))
                    .collect();
                let new_body = smid_strip_inner(&scope.body);
                RcExpr::from(Expr::Let(
                    Smid::default(),
                    Scope {
                        pattern: new_pattern,
                        body: new_body,
                    },
                    *lt,
                ))
            }
            Expr::Lam(_, ann, scope) => {
                let new_body = smid_strip_inner(&scope.body);
                RcExpr::from(Expr::Lam(
                    Smid::default(),
                    *ann,
                    Scope {
                        pattern: scope.pattern.clone(),
                        body: new_body,
                    },
                ))
            }
            Expr::App(_, f, args) => RcExpr::from(Expr::App(
                Smid::default(),
                smid_strip_inner(f),
                args.iter().map(smid_strip_inner).collect(),
            )),
            Expr::Meta(_, e, m) => RcExpr::from(Expr::Meta(
                Smid::default(),
                smid_strip_inner(e),
                smid_strip_inner(m),
            )),
            Expr::Lookup(_, obj, key, fb) => RcExpr::from(Expr::Lookup(
                Smid::default(),
                smid_strip_inner(obj),
                key.clone(),
                fb.as_ref().map(smid_strip_inner),
            )),
            Expr::List(_, items) => RcExpr::from(Expr::List(
                Smid::default(),
                items.iter().map(smid_strip_inner).collect(),
            )),
            Expr::Block(_, bm) => RcExpr::from(Expr::Block(
                Smid::default(),
                bm.iter()
                    .map(|(k, v)| (k.clone(), smid_strip_inner(v)))
                    .collect(),
            )),
            _ => expr.walk(&|e| smid_strip_inner(&e)),
        }
    }

    #[test]
    pub fn test_substs() {
        use super::acore::*;
        let x = free("x");
        let y = free("y");
        assert_eq!(
            bound(var(x.clone()).substs(&[(x, var(y.clone()))])),
            bound(var(y))
        );

        let a = free("a");
        let b = free("b");
        let c = free("c");
        let d = free("d");
        let e = free("e");

        assert_eq!(
            bound(
                lam(vec![a.clone(), b.clone(), c.clone()], var(d.clone()))
                    .substs(&[(d, var(e.clone()))])
            ),
            bound(lam(vec![a, b, c], var(e))),
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

        assert_eq!(
            bound(lam(vec![a.clone(), b.clone(), c.clone()], var(d)).substs_free(&sub)),
            bound(lam(vec![a, b, c], var(e))),
        );
    }

    #[test]
    pub fn test_simple_rebody() {
        use super::acore::*;

        let x = free("x");
        let y = free("y");

        let original = default_let(vec![(x.clone(), num(22)), (y.clone(), num(23))]);
        let expected = let_(vec![(x, num(22)), (y.clone(), num(23))], var(y.clone()));

        assert_eq!(norm(original.rebody(var(y))), norm(expected));
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
            vec![(x.clone(), num(22)), (y, num(23))],
            let_(vec![(z, num(24))], var(x.clone())),
        );

        assert_eq!(norm(original.rebody(var(x))), norm(expected));
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

        assert_eq!(original.rebody(var(y)), expected);
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

        assert_eq!(original.rebody(var(y_other)), expected);
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
                (c.clone(), var(y_other)),
            ],
            var(x_other),
        );

        let unit_c = unit_a.merge_in(unit_b);

        let expected = let_(
            vec![(x.clone(), num(22)), (y.clone(), var(x.clone()))],
            let_(
                vec![(z, num(24))],
                let_(vec![(a.clone(), num(1)), (b, var(a)), (c, var(y))], var(x)),
            ),
        );

        assert_eq!(unit_c.unwrap(), expected);
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
                (c.clone(), var(y_other)),
            ],
            var(x_other),
        );

        let unit_c = unit_a.merge_in(unit_b);

        let expected = let_(
            vec![(x.clone(), num(22)), (y.clone(), var(a_other))],
            let_(
                vec![(z, num(24))],
                let_(vec![(a.clone(), num(1)), (b, var(a)), (c, var(y))], var(x)),
            ),
        );

        assert_eq!(unit_c.unwrap(), expected);
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
            vec![(a.clone(), num(2)), (b.clone(), var(x_other))],
            var(a.clone()),
        );

        let unit_c = let_(
            vec![(m.clone(), num(3)), (n.clone(), var(b_other))],
            var(y_other),
        );

        let expected = let_(
            vec![(x.clone(), num(1)), (y.clone(), var(x.clone()))],
            let_(
                vec![(a, num(2)), (b.clone(), var(x))],
                let_(vec![(m, num(3)), (n, var(b))], var(y)),
            ),
        );

        assert_eq!(
            RcExpr::merge(vec![unit_a, unit_b, unit_c]).unwrap(),
            expected
        );
    }
}
