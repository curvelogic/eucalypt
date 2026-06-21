//! Conservative forward demand analysis over core expressions.
//!
//! Unlike the backward reference-counting pass in `analyse_demand.rs`,
//! this pass walks the expression tree forward and only promotes
//! bindings to `AtMostOnce` when a provably safe structural pattern is
//! identified.  Everything else stays at `Unknown` (conservative —
//! triggers Thunk/Update).
//!
//! ## Safe patterns (AtMostOnce)
//!
//! 1. **Intrinsic arguments** — handled by the intrinsic demand
//!    signature table (unchanged from the backward analysis).
//! 2. **Single-use, non-rendered bindings** — a binding that appears
//!    exactly once in the scope body AND zero times in any sibling RHS,
//!    AND the scope body is NOT a `Block` constructor (which would be
//!    rendered by the runtime, adding invisible extra uses).
//! 3. **WHNF bindings** — bindings whose RHS is already in WHNF
//!    (literals, lambdas) can safely skip update regardless.
//!
//! ## Soundness
//!
//! AtMostOnce is only assigned to bindings that provably cannot be
//! re-entered.  No fixups are needed — the analysis is sound by
//! construction.  The trade-off is conservatism: some bindings that
//! the backward analysis could prove single-use will remain Unknown
//! here, producing unnecessary Update frames.

use std::collections::HashMap;
use std::rc::Rc;

use crate::common::sourcemap::Smid;
use crate::core::binding::{CoreBinding, Scope, Var};
use crate::core::demand::{Cardinality, Demand};
use crate::core::expr::*;

// Re-export types for compatibility with the existing interface.
pub use crate::core::analyse_demand::{
    strict_indices_for, DemandSignature, NamedSignatureTable, SignatureTable,
};

/// Count syntactic occurrences of each binding (scope=0) in an expression.
///
/// Returns a vector of counts indexed by binder position.
fn count_refs(expr: &RcExpr, n_bindings: usize) -> Vec<u32> {
    let mut counts = vec![0u32; n_bindings];
    count_refs_inner(expr, 0, &mut counts);
    counts
}

fn count_refs_inner(expr: &RcExpr, depth: u32, counts: &mut [u32]) {
    match &*expr.inner {
        Expr::Var(_, Var::Bound(bv)) if bv.scope == depth => {
            if let Some(c) = counts.get_mut(bv.binder as usize) {
                *c = c.saturating_add(1);
            }
        }

        Expr::Let(_, scope, _) => {
            // Binding RHSs are at the SAME scope depth (LetRec).
            for b in &scope.pattern {
                count_refs_inner(&b.expr, depth, counts);
            }
            // Body is also at the same scope depth (LetRec semantics
            // in eucalypt — all Let scopes are recursive).
            count_refs_inner(&scope.body, depth, counts);
        }

        Expr::Lam(_, _, scope) => {
            // Lambda introduces a new scope — increase depth.
            count_refs_inner(&scope.body, depth + 1, counts);
        }

        Expr::App(_, f, args) => {
            count_refs_inner(f, depth, counts);
            for a in args {
                count_refs_inner(a, depth, counts);
            }
        }

        Expr::Lookup(_, obj, _, fb) => {
            count_refs_inner(obj, depth, counts);
            if let Some(f) = fb {
                count_refs_inner(f, depth, counts);
            }
        }

        Expr::List(_, xs) | Expr::ArgTuple(_, xs) => {
            for x in xs {
                count_refs_inner(x, depth, counts);
            }
        }

        Expr::Block(_, bm) => {
            for (_, v) in bm.iter() {
                count_refs_inner(v, depth, counts);
            }
        }

        Expr::Meta(_, e, m) => {
            count_refs_inner(e, depth, counts);
            count_refs_inner(m, depth, counts);
        }

        _ => {}
    }
}

/// Check whether a binding's reference is ONLY in the body (not in
/// any sibling RHS).  Returns true if the binding has zero references
/// in sibling RHSs.
fn not_in_sibling_rhs(rhs_exprs: &[RcExpr], binder: usize) -> bool {
    for (i, rhs) in rhs_exprs.iter().enumerate() {
        if i == binder {
            continue; // skip own RHS
        }
        let mut count = 0u32;
        count_single_ref(rhs, 0, binder as u32, &mut count);
        if count > 0 {
            return false;
        }
    }
    true
}

fn count_single_ref(expr: &RcExpr, depth: u32, target_binder: u32, count: &mut u32) {
    match &*expr.inner {
        Expr::Var(_, Var::Bound(bv)) if bv.scope == depth && bv.binder == target_binder => {
            *count = count.saturating_add(1);
        }

        Expr::Let(_, scope, _) => {
            for b in &scope.pattern {
                count_single_ref(&b.expr, depth, target_binder, count);
            }
            count_single_ref(&scope.body, depth, target_binder, count);
        }

        Expr::Lam(_, _, scope) => {
            count_single_ref(&scope.body, depth + 1, target_binder, count);
        }

        Expr::App(_, f, args) => {
            count_single_ref(f, depth, target_binder, count);
            for a in args {
                count_single_ref(a, depth, target_binder, count);
            }
        }

        Expr::Lookup(_, obj, _, fb) => {
            count_single_ref(obj, depth, target_binder, count);
            if let Some(f) = fb {
                count_single_ref(f, depth, target_binder, count);
            }
        }

        Expr::List(_, xs) | Expr::ArgTuple(_, xs) => {
            for x in xs {
                count_single_ref(x, depth, target_binder, count);
            }
        }

        Expr::Block(_, bm) => {
            for (_, v) in bm.iter() {
                count_single_ref(v, depth, target_binder, count);
            }
        }

        Expr::Meta(_, e, m) => {
            count_single_ref(e, depth, target_binder, count);
            count_single_ref(m, depth, target_binder, count);
        }

        _ => {}
    }
}

/// Check whether an expression is a WHNF value (literal, lambda, block,
/// list literal, intrinsic).
fn is_whnf(expr: &RcExpr) -> bool {
    matches!(
        &*expr.inner,
        Expr::Literal(_, _)
            | Expr::Lam(_, _, _)
            | Expr::Block(_, _)
            | Expr::List(_, _)
            | Expr::Intrinsic(_, _)
    )
}

/// The forward demand analyser.
pub struct ForwardDemandAnalyser {
    /// Signatures discovered during analysis (keyed by Lam body pointer).
    signatures: SignatureTable,
    /// Name-keyed signatures for user function calls.
    named_signatures: NamedSignatureTable,
}

impl Default for ForwardDemandAnalyser {
    fn default() -> Self {
        Self::new()
    }
}

impl ForwardDemandAnalyser {
    pub fn new() -> Self {
        ForwardDemandAnalyser {
            signatures: HashMap::new(),
            named_signatures: HashMap::new(),
        }
    }

    /// Run the forward demand analysis on the given expression.
    pub fn analyse(mut self, expr: &RcExpr) -> (RcExpr, SignatureTable, NamedSignatureTable) {
        let new_expr = self.walk(expr);
        (new_expr, self.signatures, self.named_signatures)
    }

    /// Walk the expression tree, annotating Let bindings with demand.
    fn walk(&mut self, expr: &RcExpr) -> RcExpr {
        match &*expr.inner {
            Expr::Let(s, scope, let_type) => self.analyse_let(*s, scope, *let_type),
            Expr::Lam(s, inl, scope) => self.analyse_lam(*s, *inl, scope),
            Expr::App(s, f, args) => {
                let new_f = self.walk(f);
                let new_args: Vec<RcExpr> = args.iter().map(|a| self.walk(a)).collect();
                RcExpr::from(Expr::App(*s, new_f, new_args))
            }
            Expr::Lookup(s, obj, key, fb) => {
                let new_obj = self.walk(obj);
                let new_fb = fb.as_ref().map(|f| self.walk(f));
                RcExpr::from(Expr::Lookup(*s, new_obj, key.clone(), new_fb))
            }
            Expr::List(s, xs) => {
                let new_xs: Vec<RcExpr> = xs.iter().map(|x| self.walk(x)).collect();
                RcExpr::from(Expr::List(*s, new_xs))
            }
            Expr::Block(s, bm) => {
                let new_bm = bm.map_values(|v| Ok::<_, ()>(self.walk(&v))).unwrap();
                RcExpr::from(Expr::Block(*s, new_bm))
            }
            Expr::Meta(s, e, m) => {
                let new_e = self.walk(e);
                let new_m = self.walk(m);
                RcExpr::from(Expr::Meta(*s, new_e, new_m))
            }
            Expr::ArgTuple(s, xs) => {
                let new_xs: Vec<RcExpr> = xs.iter().map(|x| self.walk(x)).collect();
                RcExpr::from(Expr::ArgTuple(*s, new_xs))
            }
            _ => expr.clone(),
        }
    }

    /// Analyse a Let scope.
    ///
    /// Sound rules:
    /// - If body is a Block → rendered scope → all bindings stay Unknown
    ///   (renderer adds invisible uses)
    /// - Otherwise: single-use bindings (1 use in body, 0 in siblings)
    ///   get AtMostOnce; WHNF bindings get WHNF flag; everything else
    ///   stays Unknown.
    fn analyse_let(&mut self, s: Smid, scope: &LetScope<RcExpr>, let_type: LetType) -> RcExpr {
        let n = scope.pattern.len();

        // Walk RHS expressions
        let new_rhs_exprs: Vec<RcExpr> = scope.pattern.iter().map(|b| self.walk(&b.expr)).collect();

        // Walk body
        let new_body = self.walk(&scope.body);

        // Check if this is a rendered scope (body is Block).
        // Rendered scopes have invisible uses from the render traversal,
        // so NO binding can safely be AtMostOnce.
        let is_rendered = matches!(&*new_body.inner, Expr::Block(_, _));

        // Count references per binding in the body only
        let body_counts = count_refs(&new_body, n);

        // Build demand annotations
        let mut new_bindings = Vec::with_capacity(n);
        for (i, binding) in scope.pattern.iter().enumerate() {
            let mut demand = Demand::default(); // Unknown — conservative

            if body_counts[i] == 0 {
                // Not referenced in body — check if used by siblings
                let used_by_sibling = scope.pattern.iter().enumerate().any(|(j, _)| {
                    if j == i {
                        return false;
                    }
                    let mut c = 0u32;
                    count_single_ref(&new_rhs_exprs[j], 0, i as u32, &mut c);
                    c > 0
                });
                if !used_by_sibling {
                    demand.cardinality = Cardinality::Absent;
                }
            } else if !is_rendered && body_counts[i] == 1 && not_in_sibling_rhs(&new_rhs_exprs, i) {
                // Single use in body, not used by siblings, not rendered
                // → safe to skip update.
                demand = Demand::strict_once();
            }

            // WHNF check — safe to skip update regardless.
            if is_whnf(&new_rhs_exprs[i]) {
                demand.whnf = true;
            }

            // Record Lam signatures for user function calls.
            if let Expr::Lam(_, _, ref lam_scope) = &*new_rhs_exprs[i].inner {
                let key = Rc::as_ptr(&lam_scope.body.inner) as usize;
                if let Some(sig) = self.signatures.get(&key) {
                    self.named_signatures
                        .insert(binding.name.clone(), sig.clone());
                }
            }

            new_bindings.push(CoreBinding::with_demand(
                binding.name.clone(),
                new_rhs_exprs[i].clone(),
                demand,
            ));
        }

        let new_scope = Scope {
            pattern: new_bindings,
            body: new_body,
        };

        RcExpr::from(Expr::Let(s, new_scope, let_type))
    }

    /// Analyse a Lam expression — record its signature.
    fn analyse_lam(&mut self, s: Smid, inl: bool, scope: &LamScope<RcExpr>) -> RcExpr {
        let num_params = scope.pattern.len();
        let new_body = self.walk(&scope.body);

        // Compute a simple signature: each parameter used in the body
        // gets strict+once, unused get absent.
        let body_counts = count_refs(&new_body, num_params);
        let mut sig = Vec::with_capacity(num_params);
        for &c in body_counts.iter().take(num_params) {
            if c == 0 {
                sig.push(Demand::absent());
            } else if c == 1 {
                sig.push(Demand::strict_once());
            } else {
                sig.push(Demand::lazy_multi());
            }
        }

        let sig_key = Rc::as_ptr(&new_body.inner) as usize;
        self.signatures.insert(sig_key, sig);

        let new_scope = Scope {
            pattern: scope.pattern.clone(),
            body: new_body,
        };

        RcExpr::from(Expr::Lam(s, inl, new_scope))
    }
}

/// Run the forward demand analysis pass on a core expression.
///
/// Returns the annotated expression and the signature side-table.
pub fn analyse_demands_forward(expr: &RcExpr) -> (RcExpr, SignatureTable, NamedSignatureTable) {
    let analyser = ForwardDemandAnalyser::new();
    analyser.analyse(expr)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::expr::acore::*;

    #[test]
    fn single_use_nonrecursive_binding_gets_at_most_once() {
        let x = free("x");
        let expr = let_(vec![(x.clone(), num(1))], var(x));
        let (result, _, _) = analyse_demands_forward(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(scope.pattern[0].demand.cardinality, Cardinality::AtMostOnce);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn multi_use_binding_stays_unknown() {
        let x = free("x");
        let expr = let_(
            vec![(x.clone(), num(1))],
            app(bif("ADD"), vec![var(x.clone()), var(x)]),
        );
        let (result, _, _) = analyse_demands_forward(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                // Multi use → stays at Unknown (conservative)
                assert_eq!(scope.pattern[0].demand.cardinality, Cardinality::Unknown);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn unused_binding_gets_absent() {
        let x = free("x");
        let y = free("y");
        let expr = let_(vec![(x.clone(), num(1)), (y.clone(), num(2))], var(x));
        let (result, _, _) = analyse_demands_forward(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(scope.pattern[1].demand.cardinality, Cardinality::Absent);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn rendered_scope_stays_unknown() {
        // let x = 1 in { x: x }  — body is Block → rendered
        let x = free("x");
        let expr = let_(
            vec![(x.clone(), num(1))],
            block(vec![("x".to_string(), var(x))]),
        );
        let (result, _, _) = analyse_demands_forward(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                // Even single-use: stays Unknown because scope is rendered
                assert_eq!(scope.pattern[0].demand.cardinality, Cardinality::Unknown);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn whnf_binding_gets_whnf_flag() {
        let x = free("x");
        let expr = let_(vec![(x.clone(), num(1))], var(x));
        let (result, _, _) = analyse_demands_forward(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                // Literal is WHNF
                assert!(scope.pattern[0].demand.whnf);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn binding_used_by_sibling_stays_unknown() {
        // let x = 1; y = ADD(x, 2) in y
        // x used once in body=0 times, used once in sibling RHS
        let x = free("x");
        let y = free("y");
        let expr = let_(
            vec![
                (x.clone(), num(1)),
                (y.clone(), app(bif("ADD"), vec![var(x), num(2)])),
            ],
            var(y),
        );
        let (result, _, _) = analyse_demands_forward(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                // x is not in body (body is just `y`)
                // x IS in sibling y's RHS → not Absent, stays Unknown
                // Actually: x has 0 refs in body, but is used by sibling
                // So it shouldn't be Absent. Let me check...
                // The current logic: body_counts[0] == 0 (x not in body `y`)
                // But x IS used by sibling y's RHS → used_by_sibling = true
                // So demand stays Unknown
                assert_eq!(scope.pattern[0].demand.cardinality, Cardinality::Unknown);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }
}
