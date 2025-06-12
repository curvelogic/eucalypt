//! Dead code elimination by a mark and sweep process tracing
//! references around the expression.
use crate::core::expr::*;
use moniker::Rec;
use moniker::Scope;
use moniker::{BoundTerm, BoundVar, Embed, OnBoundFn, OnFreeFn, ScopeState, Var};
use std::cell::Cell;
use std::collections::VecDeque;
use std::rc::Rc;

/// Prune definitions from all let bindings that are unused.
pub fn prune(expr: &RcExpr) -> RcExpr {
    let markable_expression = RcMarkExpr::from(expr);
    ScopeTracker::default().mark_reachable(&markable_expression);
    ScopeTracker::blank_unseen(&markable_expression)
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
pub enum SeenState {
    /// A node not yet seen or explored
    #[default]
    Unseen,
    /// A node in the expression that has been traced to from a marked node
    Seen,
    /// A node which has not been seen directly but contains seen
    /// nodes (and therefore not a candidate for removal)
    SoftSeen,
    /// A node that is seen and considered fully explored so all
    /// internal propagations have been done
    Traversed,
}

/// Tracking data to keep track of which bindings are used and which
/// vars have been followed.
#[derive(Default, Debug)]
pub struct Tracker {
    seen: Cell<SeenState>,
}

impl_bound_term_ignore!(Tracker);

impl Clone for Tracker {
    fn clone(&self) -> Self {
        Tracker {
            seen: self.seen.clone(),
        }
    }
}

impl Tracker {
    pub fn seen(&self) -> SeenState {
        self.seen.get()
    }
}

/// A fat pointer that stores a mark to record whether an expression
/// is referenced.
pub type RcMarkExpr = RcFatExpr<Tracker>;

/// The Expression type linked together with mark pointers.
pub type MarkExpr = Expr<RcMarkExpr>;

impl From<&RcExpr> for RcMarkExpr {
    fn from(expr: &RcExpr) -> RcMarkExpr {
        RcMarkExpr {
            inner: Rc::new(fmap(&expr.inner)),
            data: Tracker::default(),
        }
    }
}

impl From<&RcMarkExpr> for RcExpr {
    fn from(marked_expr: &RcMarkExpr) -> RcExpr {
        RcExpr {
            inner: Rc::new(fmap(&marked_expr.inner)),
        }
    }
}

impl RcMarkExpr {
    pub fn mark(&self) -> bool {
        let old = self.data.seen.get();
        if old == SeenState::Unseen {
            self.data.seen.set(SeenState::Seen);
            true
        } else {
            false
        }
    }

    pub fn mark_soft(&self) -> bool {
        let old = self.data.seen.get();
        if old == SeenState::Unseen {
            self.data.seen.set(SeenState::SoftSeen);
            true
        } else {
            false
        }
    }

    pub fn state(&self) -> SeenState {
        self.data.seen()
    }

    pub fn set_traversed(&self) {
        self.data.seen.set(SeenState::Traversed);
    }

    pub fn unseen(&self) -> bool {
        self.data.seen() == SeenState::Unseen
    }
}

#[derive(Debug, Default)]
pub struct ScopeTracker<'expr> {
    scopes: VecDeque<&'expr RcMarkExpr>,
    reachable: bool,
    marked_count: usize,
}

impl<'expr> ScopeTracker<'expr> {
    pub fn mark_reachable(&mut self, expr: &'expr RcMarkExpr) {
        Self::mark_body(expr);
        while self.pass(expr) > 0 {}
    }

    pub fn pass(&mut self, expr: &'expr RcMarkExpr) -> usize {
        self.marked_count = 0;
        self.traverse(expr);
        self.marked_count
    }

    fn enter(&mut self, expr: &'expr RcMarkExpr) {
        self.scopes.push_front(expr);
    }

    fn exit(&mut self) {
        self.scopes.pop_front();
    }

    /// Mark the innermost let body as Seen so we traverse from there
    fn mark_body(expr: &'expr RcMarkExpr) {
        match &*expr.inner {
            Expr::Let(_, scope, _) => Self::mark_body(&scope.unsafe_body),
            Expr::Meta(_, e, _) => Self::mark_body(e),
            _ => {
                expr.mark();
            }
        }
    }

    /// Encounter a variable.
    ///
    /// If we are in a reachable region and the corresponding scope
    /// binding has not yet been marked as seen or traversed, mark it
    /// seen and return true.
    fn encounter(&mut self, bound_var: &BoundVar<String>) {
        if self.reachable {
            let expr = self.scopes[bound_var.scope.0 as usize];
            if let Expr::Let(_, scope, _) = &*expr.inner {
                let (_, Embed(rc)) =
                    &scope.unsafe_pattern.unsafe_pattern[bound_var.binder.to_usize()];
                if rc.mark() {
                    self.marked_count += 1
                }
            }
        }
    }

    /// When usage of one expression implies usage of another higher
    /// up the tree (e.g. its metadata), we
    /// propagate seen flags using this.
    ///
    /// Warning: ensure the dependancy has not already been traversed!
    fn depend(&mut self, dependant: &RcMarkExpr, dependancy: &RcMarkExpr) {
        if dependant.state() == SeenState::Seen && dependancy.mark() {
            self.marked_count += 1
        }
    }

    /// When usage of one expression implies usage of another higher
    /// up the tree (e.g. a containing let), we mark the dependency
    /// so that the higher node will not be blanked but only softly so
    /// the seen state will not be propagated down into all sub expressions.
    ///
    /// Warning: ensure the dependancy has not already been traversed!
    fn soft_depend(&mut self, dependant: &RcMarkExpr, dependancy: &RcMarkExpr) {
        if (dependant.state() == SeenState::Seen || dependant.state() == SeenState::SoftSeen)
            && dependancy.mark_soft()
        {
            self.marked_count += 1
        }
    }

    fn traverse(&mut self, expr: &'expr RcMarkExpr) {
        if expr.state() == SeenState::Traversed {
            return;
        }

        // check and record whether we're entering a reachable region
        let at_reachable_boundary = !self.reachable && expr.state() == SeenState::Seen;
        if at_reachable_boundary {
            self.reachable = true;
        }

        match &*expr.inner {
            Expr::Var(_, Var::Bound(bound_var)) => {
                self.encounter(bound_var);
            }
            Expr::Let(_, scope, _) => {
                self.enter(expr);
                for (_, Embed(ref rc)) in &scope.unsafe_pattern.unsafe_pattern {
                    self.soft_depend(rc, expr);
                    self.traverse(rc);
                }
                self.traverse(&scope.unsafe_body);
                self.exit();
            }
            Expr::Lam(_, _, scope) => {
                self.enter(expr);
                self.traverse(&scope.unsafe_body);
                self.exit();
            }
            Expr::Lookup(_, e, _, fb) => {
                self.traverse(e);
                if let Some(fallback) = fb {
                    self.traverse(fallback);
                }
            }
            Expr::List(_, xs) => {
                for x in xs {
                    self.traverse(x);
                }
            }
            Expr::Block(_, block_map) => {
                for (_, v) in block_map.iter() {
                    self.traverse(v);
                }
            }
            Expr::Meta(_, e, m) => {
                self.depend(e, m);
                self.traverse(e);
                self.traverse(m);
            }
            Expr::ArgTuple(_, xs) => {
                for x in xs {
                    self.traverse(x);
                }
            }
            Expr::App(_, g, xs) => {
                self.traverse(g);
                for x in xs {
                    self.traverse(x);
                }
            }
            Expr::Soup(_, xs) => {
                for x in xs {
                    self.traverse(x);
                }
            }
            Expr::Operator(_, _, _, e) => {
                self.traverse(e);
            }
            _ => {}
        }

        if at_reachable_boundary {
            expr.set_traversed();
            self.reachable = false;
        }
    }

    pub fn blank_unseen(expr: &RcMarkExpr) -> RcExpr {
        RcExpr::from(match &*expr.inner {
            Expr::Let(s, scope, t) => Expr::Let(
                *s,
                Scope {
                    unsafe_pattern: Rec {
                        unsafe_pattern: scope
                            .unsafe_pattern
                            .unsafe_pattern
                            .iter()
                            .map(|&(ref n, Embed(ref value))| {
                                if value.unseen() {
                                    (n.clone(), Embed(RcExpr::from(Expr::ErrEliminated)))
                                } else {
                                    (n.clone(), Embed(Self::blank_unseen(value)))
                                }
                            })
                            .collect(),
                    },
                    unsafe_body: Self::blank_unseen(&scope.unsafe_body),
                },
                *t,
            ),
            Expr::Lam(s, inl, scope) => Expr::Lam(
                *s,
                *inl,
                Scope {
                    unsafe_pattern: scope.unsafe_pattern.clone(),
                    unsafe_body: Self::blank_unseen(&scope.unsafe_body),
                },
            ),
            Expr::Lookup(s, e, n, fb) => Expr::Lookup(
                *s,
                Self::blank_unseen(e),
                n.clone(),
                fb.as_ref().map(Self::blank_unseen),
            ),
            Expr::List(s, xs) => Expr::List(*s, xs.iter().map(Self::blank_unseen).collect()),
            Expr::Block(s, block_map) => Expr::Block(
                *s,
                block_map
                    .iter()
                    .map(|(k, v)| (k.clone(), Self::blank_unseen(v)))
                    .collect(),
            ),
            Expr::Meta(s, e, m) => Expr::Meta(*s, Self::blank_unseen(e), Self::blank_unseen(m)),
            Expr::ArgTuple(s, xs) => {
                Expr::ArgTuple(*s, xs.iter().map(Self::blank_unseen).collect())
            }
            Expr::App(s, f, xs) => Expr::App(
                *s,
                Self::blank_unseen(f),
                xs.iter().map(Self::blank_unseen).collect(),
            ),
            Expr::Soup(s, xs) => Expr::Soup(*s, xs.iter().map(Self::blank_unseen).collect()),
            Expr::Operator(s, fx, p, e) => Expr::Operator(*s, *fx, *p, Self::blank_unseen(e)),
            _ => fmap(&*expr.inner),
        })
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::core::expr::acore::*;
    use crate::core::expr::RcExpr;
    use moniker::assert_term_eq;

    #[test]
    pub fn test_simple() {
        let x = free("x");
        let y = free("y");

        let simple = let_(
            vec![(x.clone(), num(1)), (y.clone(), num(2))],
            var(x.clone()),
        );

        let expected = let_(
            vec![(x.clone(), num(1)), (y, RcExpr::from(Expr::ErrEliminated))],
            var(x),
        );

        assert_term_eq!(prune(&simple), expected)
    }

    #[test]
    pub fn test_nested() {
        let x = free("x");
        let y = free("y");
        let z = free("z");
        let v = free("v");

        let nested = let_(
            vec![(x.clone(), num(1)), (y.clone(), num(2))],
            let_(
                vec![(z.clone(), num(3)), (v.clone(), num(4))],
                var(x.clone()),
            ),
        );

        let expected = let_(
            vec![(x.clone(), num(1)), (y, RcExpr::from(Expr::ErrEliminated))],
            let_(
                vec![
                    (z, RcExpr::from(Expr::ErrEliminated)),
                    (v, RcExpr::from(Expr::ErrEliminated)),
                ],
                var(x),
            ),
        );

        assert_term_eq!(prune(&nested), expected)
    }

    #[test]
    pub fn test_use_in_expr() {
        let a = free("a");
        let b = free("b");
        let c = free("c");

        let expr = let_(
            vec![
                (a.clone(), num(1)),
                (b.clone(), num(2)),
                (c.clone(), sym("BOMB")),
            ],
            app(bif("BLAH"), vec![var(a.clone()), var(b.clone())]),
        );

        let expected = let_(
            vec![
                (a.clone(), num(1)),
                (b.clone(), num(2)),
                (c, RcExpr::from(Expr::ErrEliminated)),
            ],
            app(bif("BLAH"), vec![var(a), var(b)]),
        );

        assert_term_eq!(prune(&expr), expected)
    }

    #[test]
    pub fn test_prune_in_nested() {
        let a = free("a");
        let b = free("b");
        let d = free("d");
        let e = free("e");

        let expr = let_(
            vec![
                (
                    a.clone(),
                    let_(
                        vec![(d.clone(), sym("BOMB")), (e.clone(), num(2))],
                        var(e.clone()),
                    ),
                ),
                (b.clone(), sym("BOMB")),
            ],
            var(a.clone()),
        );

        let expected = let_(
            vec![
                (
                    a.clone(),
                    let_(
                        vec![(d, RcExpr::from(Expr::ErrEliminated)), (e.clone(), num(2))],
                        var(e),
                    ),
                ),
                (b, RcExpr::from(Expr::ErrEliminated)),
            ],
            var(a),
        );

        assert_term_eq!(prune(&expr), expected)
    }
}
