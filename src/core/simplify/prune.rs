//! Dead code elimination by a mark and sweep process tracing
//! references around the expression.
//!
//! Includes block-level DCE: for `DefaultBlockLet` bindings that are
//! only accessed via static `Lookup` patterns (`ns.member`), inner
//! members that are never looked up are eliminated from the Block body.
use crate::core::expr::*;
use moniker::Rec;
use moniker::Scope;
use moniker::{BoundTerm, BoundVar, Embed, OnBoundFn, OnFreeFn, ScopeState, Var};
use std::cell::Cell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

/// Prune definitions from all let bindings that are unused.
///
/// Also performs block-level DCE: for `DefaultBlockLet` bindings that
/// are only accessed via static `Lookup` patterns, unreferenced
/// members are eliminated from the Block body.
pub fn prune(expr: &RcExpr) -> RcExpr {
    let markable_expression = RcMarkExpr::from(expr);
    let mut tracker = ScopeTracker::default();
    tracker.mark_reachable(&markable_expression);
    tracker.blank_unseen(&markable_expression)
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

/// Identity key for a binding's value node, used to track block
/// access patterns across the mark phase. Uses raw pointer from
/// `Rc::as_ptr` — valid for the lifetime of the `RcMarkExpr` tree.
type BindingId = *const Expr<RcMarkExpr>;

/// Tracks static-only access patterns for `DefaultBlockLet` bindings.
///
/// During the mark phase, identifies bindings whose value is a
/// `DefaultBlockLet` and records whether they are accessed only via
/// static `Lookup` patterns. For static-only bindings, records which
/// member names are accessed so that `blank_unseen` can filter the
/// Block body.
#[derive(Debug, Default)]
struct BlockAccessTracker {
    /// Binding values that are `DefaultBlockLet` and (so far) have
    /// only static (Lookup) access. Disqualified on bare Var reference.
    candidates: HashSet<BindingId>,
    /// Bindings that have been disqualified — prevents re-registration
    /// on subsequent traversal passes.
    disqualified: HashSet<BindingId>,
    /// For candidate bindings: which member names are accessed.
    accessed: HashMap<BindingId, HashSet<String>>,
}

impl BlockAccessTracker {
    /// Register a binding as a `DefaultBlockLet` candidate for block-level DCE.
    /// Does nothing if the binding was previously disqualified.
    fn register(&mut self, id: BindingId) {
        if !self.disqualified.contains(&id) {
            self.candidates.insert(id);
        }
    }

    /// Record a static member access (`ns.member`) for a candidate binding.
    fn record_access(&mut self, id: BindingId, member: String) {
        self.accessed.entry(id).or_default().insert(member);
    }

    /// Disqualify a binding from block-level DCE (dynamic/escape access).
    fn disqualify(&mut self, id: BindingId) {
        self.candidates.remove(&id);
        self.disqualified.insert(id);
    }

    /// Check whether a binding is eligible for block-level DCE.
    fn is_candidate(&self, id: BindingId) -> bool {
        self.candidates.contains(&id)
    }

    /// Get the set of accessed members for a candidate binding.
    fn accessed_members(&self, id: BindingId) -> Option<&HashSet<String>> {
        if self.candidates.contains(&id) {
            self.accessed.get(&id)
        } else {
            None
        }
    }
}

#[derive(Debug, Default)]
pub struct ScopeTracker<'expr> {
    scopes: VecDeque<&'expr RcMarkExpr>,
    reachable: bool,
    marked_count: usize,
    block_tracker: BlockAccessTracker,
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

    /// Encounter a variable via a bare reference (not through Lookup).
    ///
    /// If we are in a reachable region and the corresponding scope
    /// binding has not yet been marked as seen or traversed, mark it
    /// seen and return true. Also disqualifies the binding from
    /// block-level DCE since a bare reference means the entire block
    /// value escapes.
    fn encounter(&mut self, bound_var: &BoundVar<String>) {
        if self.reachable {
            let expr = self.scopes[bound_var.scope.0 as usize];
            if let Expr::Let(_, scope, _) = &*expr.inner {
                let (_, Embed(rc)) =
                    &scope.unsafe_pattern.unsafe_pattern[bound_var.binder.to_usize()];
                // Disqualify from block-level DCE on bare access
                let id: BindingId = Rc::as_ptr(&rc.inner);
                self.block_tracker.disqualify(id);
                if rc.mark() {
                    self.marked_count += 1
                }
            }
        }
    }

    /// Encounter a variable via a static Lookup (`ns.member`).
    ///
    /// Marks the binding as reachable (like `encounter`) but records
    /// the member access for block-level DCE instead of disqualifying.
    fn encounter_lookup(&mut self, bound_var: &BoundVar<String>, member: &str) {
        if self.reachable {
            let expr = self.scopes[bound_var.scope.0 as usize];
            if let Expr::Let(_, scope, _) = &*expr.inner {
                let (_, Embed(rc)) =
                    &scope.unsafe_pattern.unsafe_pattern[bound_var.binder.to_usize()];
                let id: BindingId = Rc::as_ptr(&rc.inner);
                if self.block_tracker.is_candidate(id) {
                    self.block_tracker.record_access(id, member.to_owned());
                }
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
                // Register DefaultBlockLet bindings as candidates
                // for block-level DCE
                for (_, Embed(ref rc)) in &scope.unsafe_pattern.unsafe_pattern {
                    if rc.inner.is_default_let() {
                        let id: BindingId = Rc::as_ptr(&rc.inner);
                        self.block_tracker.register(id);
                    }
                }
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
            Expr::Lookup(_, e, member, fb) => {
                // Check for static access pattern: Lookup(Var(Bound(bv)), member, ...)
                if let Expr::Var(_, Var::Bound(bound_var)) = &*e.inner {
                    self.encounter_lookup(bound_var, member);
                } else {
                    self.traverse(e);
                }
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

    pub fn blank_unseen(&self, expr: &RcMarkExpr) -> RcExpr {
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
                                    let id: BindingId = Rc::as_ptr(&value.inner);
                                    if let Some(members) = self.block_tracker.accessed_members(id) {
                                        (
                                            n.clone(),
                                            Embed(self.blank_default_block_let(value, members)),
                                        )
                                    } else {
                                        (n.clone(), Embed(self.blank_unseen(value)))
                                    }
                                }
                            })
                            .collect(),
                    },
                    unsafe_body: self.blank_unseen(&scope.unsafe_body),
                },
                *t,
            ),
            Expr::Lam(s, inl, scope) => Expr::Lam(
                *s,
                *inl,
                Scope {
                    unsafe_pattern: scope.unsafe_pattern.clone(),
                    unsafe_body: self.blank_unseen(&scope.unsafe_body),
                },
            ),
            Expr::Lookup(s, e, n, fb) => Expr::Lookup(
                *s,
                self.blank_unseen(e),
                n.clone(),
                fb.as_ref().map(|fb| self.blank_unseen(fb)),
            ),
            Expr::List(s, xs) => Expr::List(*s, xs.iter().map(|x| self.blank_unseen(x)).collect()),
            Expr::Block(s, block_map) => Expr::Block(
                *s,
                block_map
                    .iter()
                    .map(|(k, v)| (k.clone(), self.blank_unseen(v)))
                    .collect(),
            ),
            Expr::Meta(s, e, m) => Expr::Meta(*s, self.blank_unseen(e), self.blank_unseen(m)),
            Expr::ArgTuple(s, xs) => {
                Expr::ArgTuple(*s, xs.iter().map(|x| self.blank_unseen(x)).collect())
            }
            Expr::App(s, f, xs) => Expr::App(
                *s,
                self.blank_unseen(f),
                xs.iter().map(|x| self.blank_unseen(x)).collect(),
            ),
            Expr::Soup(s, xs) => Expr::Soup(*s, xs.iter().map(|x| self.blank_unseen(x)).collect()),
            Expr::Operator(s, fx, p, e) => Expr::Operator(*s, *fx, *p, self.blank_unseen(e)),
            _ => fmap(&*expr.inner),
        })
    }

    /// Blank a `DefaultBlockLet` binding, filtering its Block body to
    /// retain only the members that were accessed via static Lookup.
    ///
    /// Inner bindings are left intact — they may include values for
    /// the eliminated members but this is harmless (they become dead
    /// code that the compiler can skip or a future prune pass can
    /// eliminate).
    fn blank_default_block_let(
        &self,
        expr: &RcMarkExpr,
        accessed_members: &HashSet<String>,
    ) -> RcExpr {
        match &*expr.inner {
            Expr::Let(s, scope, LetType::DefaultBlockLet) => {
                // Convert inner bindings normally
                let new_bindings: Vec<_> = scope
                    .unsafe_pattern
                    .unsafe_pattern
                    .iter()
                    .map(|&(ref n, Embed(ref value))| {
                        if value.unseen() {
                            (n.clone(), Embed(RcExpr::from(Expr::ErrEliminated)))
                        } else {
                            (n.clone(), Embed(self.blank_unseen(value)))
                        }
                    })
                    .collect();

                // Filter the Block body to retain only accessed members
                let new_body = match &*scope.unsafe_body.inner {
                    Expr::Block(bs, block_map) => {
                        let filtered: BlockMap<RcExpr> = block_map
                            .iter()
                            .filter(|(k, _)| accessed_members.contains(k.as_str()))
                            .map(|(k, v)| (k.clone(), self.blank_unseen(v)))
                            .collect();
                        RcExpr::from(Expr::Block(*bs, filtered))
                    }
                    _ => self.blank_unseen(&scope.unsafe_body),
                };

                RcExpr::from(Expr::Let(
                    *s,
                    Scope {
                        unsafe_pattern: Rec {
                            unsafe_pattern: new_bindings,
                        },
                        unsafe_body: new_body,
                    },
                    LetType::DefaultBlockLet,
                ))
            }
            // If wrapped in Meta, look through it
            Expr::Meta(s, e, m) => {
                let new_e = self.blank_default_block_let(e, accessed_members);
                let new_m = self.blank_unseen(m);
                RcExpr::from(Expr::Meta(*s, new_e, new_m))
            }
            _ => self.blank_unseen(expr),
        }
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

    // Block-level DCE tests

    #[test]
    pub fn test_static_only_access_filters_block_body() {
        // ns is only accessed via ns.used — block body should be
        // filtered to remove "unused" entry. Inner bindings are left
        // intact (a subsequent compress pass handles cleanup).
        let used = free("used");
        let unused = free("unused");
        let ns = free("ns");
        let result = free("result");

        let expr = let_(
            vec![
                (
                    ns.clone(),
                    default_let(vec![(used.clone(), num(42)), (unused.clone(), num(99))]),
                ),
                (result.clone(), lookup(var(ns.clone()), "used", None)),
            ],
            var(result.clone()),
        );

        let pruned = prune(&expr);

        // Verify the block body is filtered (only "used" member)
        match &*pruned.inner {
            Expr::Let(_, scope, _) => {
                let (_, Embed(ref ns_val)) = scope.unsafe_pattern.unsafe_pattern[0];
                match &*ns_val.inner {
                    Expr::Let(_, inner_scope, LetType::DefaultBlockLet) => {
                        // Block body should only contain "used"
                        match &*inner_scope.unsafe_body.inner {
                            Expr::Block(_, block_map) => {
                                assert_eq!(block_map.len(), 1);
                                assert!(block_map.get("used").is_some());
                                assert!(block_map.get("unused").is_none());
                            }
                            other => panic!("expected Block body, got: {other:?}"),
                        }
                    }
                    other => panic!("expected DefaultBlockLet, got: {other:?}"),
                }
            }
            other => panic!("expected outer Let, got: {other:?}"),
        }
    }

    #[test]
    pub fn test_dynamic_access_preserves_all_members() {
        // ns escapes via application — all members must be preserved
        let a = free("a");
        let b = free("b");
        let ns = free("ns");
        let result = free("result");

        let expr = let_(
            vec![
                (
                    ns.clone(),
                    default_let(vec![(a.clone(), num(1)), (b.clone(), num(2))]),
                ),
                (result.clone(), app(bif("F"), vec![var(ns.clone())])),
            ],
            var(result.clone()),
        );

        let pruned = prune(&expr);
        // All members preserved (dynamic access), result used
        assert_term_eq!(pruned, expr);
    }

    #[test]
    pub fn test_mixed_access_preserves_all_members() {
        // ns is accessed both via lookup AND bare var — must preserve all
        let a = free("a");
        let b = free("b");
        let ns = free("ns");
        let x = free("x");
        let y = free("y");

        let expr = let_(
            vec![
                (
                    ns.clone(),
                    default_let(vec![(a.clone(), num(1)), (b.clone(), num(2))]),
                ),
                (x.clone(), lookup(var(ns.clone()), "a", None)),
                (y.clone(), var(ns.clone())),
            ],
            app(bif("ADD"), vec![var(x.clone()), var(y.clone())]),
        );

        let pruned = prune(&expr);
        assert_term_eq!(pruned, expr);
    }

    #[test]
    pub fn test_multiple_lookups_keep_accessed_members() {
        // Only "a" and "c" are looked up — "b" entry removed from
        // Block body. Inner bindings are left intact.
        let a = free("a");
        let b = free("b");
        let c = free("c");
        let ns = free("ns");
        let x = free("x");
        let y = free("y");

        let expr = let_(
            vec![
                (
                    ns.clone(),
                    default_let(vec![
                        (a.clone(), num(1)),
                        (b.clone(), num(2)),
                        (c.clone(), num(3)),
                    ]),
                ),
                (x.clone(), lookup(var(ns.clone()), "a", None)),
                (y.clone(), lookup(var(ns.clone()), "c", None)),
            ],
            app(bif("ADD"), vec![var(x.clone()), var(y.clone())]),
        );

        let pruned = prune(&expr);

        // Verify block body has only "a" and "c"
        match &*pruned.inner {
            Expr::Let(_, scope, _) => {
                let (_, Embed(ref ns_val)) = scope.unsafe_pattern.unsafe_pattern[0];
                match &*ns_val.inner {
                    Expr::Let(_, inner_scope, LetType::DefaultBlockLet) => {
                        // Block body should contain "a" and "c" only
                        match &*inner_scope.unsafe_body.inner {
                            Expr::Block(_, block_map) => {
                                assert_eq!(block_map.len(), 2);
                                assert!(block_map.get("a").is_some());
                                assert!(block_map.get("c").is_some());
                                assert!(block_map.get("b").is_none());
                            }
                            other => panic!("expected Block body, got: {other:?}"),
                        }
                    }
                    other => panic!("expected DefaultBlockLet, got: {other:?}"),
                }
            }
            other => panic!("expected outer Let, got: {other:?}"),
        }
    }

    #[test]
    pub fn test_dynamic_access_via_body_preserves_all() {
        // ns escapes via bare Var reference in body block —
        // all members must be preserved
        let used = free("used");
        let unused = free("unused");
        let ns = free("ns");
        let result = free("result");

        let expr = let_(
            vec![
                (
                    ns.clone(),
                    default_let(vec![(used.clone(), num(42)), (unused.clone(), num(99))]),
                ),
                (result.clone(), lookup(var(ns.clone()), "used", None)),
            ],
            block(vec![
                ("ns".to_string(), var(ns.clone())),
                ("result".to_string(), var(result.clone())),
            ]),
        );

        let pruned = prune(&expr);
        // ns escapes bare in body block, so all members preserved
        assert_term_eq!(pruned, expr);
    }
}
