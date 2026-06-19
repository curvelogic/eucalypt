//! Backward demand analysis over core expressions.
//!
//! Walks the core expression tree bottom-up, computing a demand
//! environment (map from bound variable identities to their aggregate
//! demand) and populating `CoreBinding.demand` on every binding.
//!
//! Also produces a signature side-table mapping lambda body pointers
//! to per-argument demand vectors for use by the STG compiler.

use std::collections::HashMap;
use std::rc::Rc;

use crate::common::sourcemap::Smid;
use crate::core::binding::{CoreBinding, Scope, Var};
use crate::core::demand::{Cardinality, Demand};
use crate::core::expr::*;
use crate::eval::intrinsics;

/// A demand environment: maps `(scope, binder)` pairs to their
/// aggregate demand within a given expression.
///
/// The keys are `(scope, binder)` de Bruijn coordinates as seen from
/// the expression that produced this environment.
type DemandEnv = HashMap<(u32, u32), Demand>;

/// Key for the signature side-table: raw pointer to the `Rc` data of
/// a `Lam` scope body's inner `CoreExpr`.
type SigKey = usize;

/// Per-function demand signature: one `Demand` per parameter.
pub type DemandSignature = Vec<Demand>;

/// The demand signature side-table produced by the analysis.
///
/// Keyed by the raw pointer of the `Lam` body's `Rc<CoreExpr>` inner
/// data, mapping to per-argument demand vectors.
pub type SignatureTable = HashMap<SigKey, DemandSignature>;

/// Build intrinsic seed signatures from the INTRINSICS catalogue.
///
/// For each intrinsic, strict arguments get `(Strict, AtMostOnce)` per
/// the blanket rule. Non-strict arguments get `(Lazy, Multi)`.
/// Special refinements are applied for IF, AND, OR, LOOKUPOR, IO_BIND.
///
/// This function is also called by the STG compiler to populate its
/// per-intrinsic demand table, replacing the `strict_args` consultation.
pub fn build_intrinsic_signatures() -> HashMap<String, DemandSignature> {
    let mut sigs = HashMap::new();
    for info in intrinsics::catalogue() {
        let arity = info.arity();
        if arity == 0 {
            continue;
        }
        let strict_set = info.strict_args();
        let mut sig = Vec::with_capacity(arity);
        for i in 0..arity {
            if strict_set.contains(&i) {
                sig.push(Demand::strict_once());
            } else {
                sig.push(Demand::lazy_multi());
            }
        }
        sigs.insert(info.name().to_string(), sig);
    }

    // Special refinements per spec
    // IF: condition strict+once, then/else lazy+once
    sigs.insert(
        "IF".to_string(),
        vec![
            Demand::strict_once(),
            Demand::lazy_once(),
            Demand::lazy_once(),
        ],
    );

    // AND/OR: first strict+once, second lazy+once (short-circuit)
    sigs.insert(
        "AND".to_string(),
        vec![Demand::strict_once(), Demand::lazy_once()],
    );
    sigs.insert(
        "OR".to_string(),
        vec![Demand::strict_once(), Demand::lazy_once()],
    );

    // LOOKUPOR: obj strict+once, key strict+once, fallback lazy+once, (extra arg)
    if let Some(sig) = sigs.get_mut("LOOKUPOR") {
        // LOOKUPOR has 4 args: object(0), key(1), fallback(2), extra(3)
        // object and key are strict; fallback is lazy+once
        if sig.len() >= 3 {
            sig[2] = Demand::lazy_once();
        }
    }
    if let Some(sig) = sigs.get_mut("LOOKUPOR#") {
        if sig.len() >= 3 {
            sig[2] = Demand::lazy_once();
        }
    }

    // IO_BIND: action strict+once, continuation lazy+once
    sigs.insert(
        "IO_BIND".to_string(),
        vec![
            Demand::strict_once(),
            Demand::lazy_once(),
            Demand::lazy_multi(),
            Demand::lazy_multi(),
        ],
    );

    sigs
}

/// Remove entries for scope 0 (current scope) and decrement all
/// remaining scope indices by 1. Used when leaving a scope.
fn unshift_env(env: &DemandEnv) -> DemandEnv {
    env.iter()
        .filter(|&(&(scope, _), _)| scope > 0)
        .map(|(&(scope, binder), &demand)| ((scope - 1, binder), demand))
        .collect()
}

/// Merge two demand environments, combining demands for the same
/// variable using `plus` (sequential composition).
fn merge_envs(a: &DemandEnv, b: &DemandEnv) -> DemandEnv {
    let mut result = a.clone();
    for (&key, &demand) in b {
        let entry = result.entry(key).or_insert(Demand::absent());
        *entry = entry.plus(demand);
    }
    result
}

/// The demand analyser.
pub struct DemandAnalyser {
    /// Intrinsic seed signatures.
    intrinsic_sigs: HashMap<String, DemandSignature>,
    /// Signatures discovered during analysis (keyed by Lam body pointer).
    signatures: SignatureTable,
}

impl Default for DemandAnalyser {
    fn default() -> Self {
        Self::new()
    }
}

impl DemandAnalyser {
    pub fn new() -> Self {
        DemandAnalyser {
            intrinsic_sigs: build_intrinsic_signatures(),
            signatures: HashMap::new(),
        }
    }

    /// Run the demand analysis on the given expression, returning the
    /// annotated expression and the signature side-table.
    pub fn analyse(mut self, expr: &RcExpr) -> (RcExpr, SignatureTable) {
        let (new_expr, _env) = self.analyse_expr(expr);
        (new_expr, self.signatures)
    }

    /// Analyse an expression, returning the annotated expression and
    /// the demand environment it places on its free variables.
    fn analyse_expr(&mut self, expr: &RcExpr) -> (RcExpr, DemandEnv) {
        match &*expr.inner {
            Expr::Var(_s, Var::Bound(bv)) => {
                let mut env = DemandEnv::new();
                // A variable reference places strict+once demand
                env.insert((bv.scope, bv.binder), Demand::strict_once());
                (expr.clone(), env)
            }

            Expr::Var(_, _) | Expr::Intrinsic(_, _) | Expr::Literal(_, _) => {
                (expr.clone(), DemandEnv::new())
            }

            Expr::Let(s, scope, let_type) => self.analyse_let(*s, scope, *let_type),

            Expr::Lam(s, inl, scope) => self.analyse_lam(*s, *inl, scope),

            Expr::App(s, f, args) => self.analyse_app(*s, f, args),

            Expr::Lookup(s, obj, key, fallback) => {
                // Object is strict+once: analyse it and collect its env.
                let (new_obj, mut env) = self.analyse_expr(obj);

                let new_fb = if let Some(fb) = fallback {
                    let (new_fb, fb_env) = self.analyse_expr(fb);
                    // Fallback is lazy+once: merge with main env.
                    env = merge_envs(&env, &fb_env);
                    Some(new_fb)
                } else {
                    None
                };

                let new_expr = RcExpr::from(Expr::Lookup(*s, new_obj, key.clone(), new_fb));
                (new_expr, env)
            }

            Expr::List(s, xs) => {
                let mut env = DemandEnv::new();
                let mut new_xs = Vec::with_capacity(xs.len());
                for x in xs {
                    let (new_x, x_env) = self.analyse_expr(x);
                    env = merge_envs(&env, &x_env);
                    new_xs.push(new_x);
                }
                (RcExpr::from(Expr::List(*s, new_xs)), env)
            }

            Expr::Block(s, block_map) => {
                let mut env = DemandEnv::new();
                let mut new_entries = Vec::new();
                for (k, v) in block_map.iter() {
                    let (new_v, v_env) = self.analyse_expr(v);
                    env = merge_envs(&env, &v_env);
                    new_entries.push((k.clone(), new_v));
                }
                (
                    RcExpr::from(Expr::Block(*s, new_entries.into_iter().collect())),
                    env,
                )
            }

            Expr::Meta(s, e, m) => {
                let (new_e, e_env) = self.analyse_expr(e);
                let (new_m, m_env) = self.analyse_expr(m);
                let env = merge_envs(&e_env, &m_env);
                (RcExpr::from(Expr::Meta(*s, new_e, new_m)), env)
            }

            Expr::ArgTuple(s, xs) => {
                let mut env = DemandEnv::new();
                let mut new_xs = Vec::with_capacity(xs.len());
                for x in xs {
                    let (new_x, x_env) = self.analyse_expr(x);
                    env = merge_envs(&env, &x_env);
                    new_xs.push(new_x);
                }
                (RcExpr::from(Expr::ArgTuple(*s, new_xs)), env)
            }

            // Remaining variants: pass through unchanged
            _ => (expr.clone(), DemandEnv::new()),
        }
    }

    /// Analyse a `Let` expression.
    fn analyse_let(
        &mut self,
        s: Smid,
        scope: &LetScope<RcExpr>,
        let_type: LetType,
    ) -> (RcExpr, DemandEnv) {
        let num_bindings = scope.pattern.len();

        // 1. Analyse the body to find what demands it places on bindings
        let (new_body, body_env) = self.analyse_expr(&scope.body);

        // 2. Analyse each binding's RHS and annotate with the demand
        //    from the body (or absent if not referenced).
        let mut new_bindings = Vec::with_capacity(num_bindings);
        let mut outer_env = unshift_env(&body_env);

        for (i, binding) in scope.pattern.iter().enumerate() {
            // The demand on this binding from the body
            let mut binding_demand = body_env
                .get(&(0, i as u32))
                .copied()
                .unwrap_or(Demand::absent());

            // All core Let scopes are recursive.  A binding referenced
            // once syntactically may be evaluated many times through
            // recursion (e.g. self-referential lookup fallbacks).
            // AtMostOnce would skip the Update frame, disabling the
            // blackhole detector that catches infinite loops.
            // Promote AtMostOnce → Multi for safety.
            if binding_demand.cardinality == Cardinality::AtMostOnce {
                binding_demand.cardinality = Cardinality::Multi;
            }

            // Analyse the binding's RHS
            let (new_rhs, rhs_env) = self.analyse_expr(&binding.expr);

            // If the binding is used, its RHS's demands propagate outward,
            // but only for non-dead bindings
            if binding_demand.cardinality != Cardinality::Absent
                && binding_demand.cardinality != Cardinality::Unknown
            {
                let shifted_rhs_env = unshift_env(&rhs_env);
                outer_env = merge_envs(&outer_env, &shifted_rhs_env);
            }

            new_bindings.push(CoreBinding::with_demand(
                binding.name.clone(),
                new_rhs,
                binding_demand,
            ));
        }

        let new_scope = Scope {
            pattern: new_bindings,
            body: new_body,
        };

        (RcExpr::from(Expr::Let(s, new_scope, let_type)), outer_env)
    }

    /// Analyse a `Lam` expression.
    fn analyse_lam(&mut self, s: Smid, inl: bool, scope: &LamScope<RcExpr>) -> (RcExpr, DemandEnv) {
        let num_params = scope.pattern.len();

        // Analyse the body
        let (new_body, body_env) = self.analyse_expr(&scope.body);

        // Extract the demand signature: one Demand per parameter
        let mut sig = Vec::with_capacity(num_params);
        for i in 0..num_params {
            let param_demand = body_env
                .get(&(0, i as u32))
                .copied()
                .unwrap_or(Demand::absent());
            sig.push(param_demand);
        }

        // Store the signature keyed by the body's Rc pointer
        let sig_key = Rc::as_ptr(&new_body.inner) as usize;
        self.signatures.insert(sig_key, sig);

        // The outer env is the body env with scope-0 entries removed
        let outer_env = unshift_env(&body_env);

        let new_scope = Scope {
            pattern: scope.pattern.clone(),
            body: new_body,
        };

        (RcExpr::from(Expr::Lam(s, inl, new_scope)), outer_env)
    }

    /// Analyse an `App` expression.
    fn analyse_app(&mut self, s: Smid, f: &RcExpr, args: &[RcExpr]) -> (RcExpr, DemandEnv) {
        // Analyse the function
        let (new_f, f_env) = self.analyse_expr(f);
        let mut env = f_env;

        // Try to find the function's signature
        let sig = self.lookup_signature(f);

        // Analyse each argument with the demand from the signature
        let mut new_args = Vec::with_capacity(args.len());
        for (i, arg) in args.iter().enumerate() {
            let (new_arg, arg_env) = self.analyse_expr(arg);

            // Scale the arg's env by the demand the function places on this arg
            let arg_demand = sig
                .as_ref()
                .and_then(|s| s.get(i))
                .copied()
                .unwrap_or(Demand::lazy_multi());

            // If the argument is used (not absent), merge its demands
            if arg_demand.cardinality != Cardinality::Absent {
                env = merge_envs(&env, &arg_env);
            }

            new_args.push(new_arg);
        }

        (RcExpr::from(Expr::App(s, new_f, new_args)), env)
    }

    /// Look up the demand signature for a function expression.
    fn lookup_signature(&self, f: &RcExpr) -> Option<DemandSignature> {
        match &*f.inner {
            // Intrinsic function: use seed signature
            Expr::Intrinsic(_, name) => self.intrinsic_sigs.get(name).cloned(),

            // Lambda: look in the signatures table
            Expr::Lam(_, _, scope) => {
                let key = Rc::as_ptr(&scope.body.inner) as usize;
                self.signatures.get(&key).cloned()
            }

            // Bound variable pointing to a known lambda: we'd need to
            // trace through the binding. For now, conservative.
            _ => None,
        }
    }
}

/// Run the demand analysis pass on a core expression.
///
/// Returns the annotated expression and the signature side-table.
pub fn analyse_demands(expr: &RcExpr) -> (RcExpr, SignatureTable) {
    let analyser = DemandAnalyser::new();
    analyser.analyse(expr)
}

/// Walk the annotated core expression and print demand annotations for
/// every let-bound name. Used by `eu dump demands`.
pub fn print_demands(expr: &RcExpr) {
    print_demands_inner(expr, 0);
}

fn print_demands_inner(expr: &RcExpr, depth: usize) {
    use crate::core::demand::{Cardinality, Strictness};

    match &*expr.inner {
        Expr::Let(_, scope, _) => {
            for b in &scope.pattern {
                let s = match b.demand.strictness {
                    Strictness::Strict => "Strict",
                    Strictness::Lazy => "Lazy",
                    Strictness::Unknown => "Unknown",
                };
                let c = match b.demand.cardinality {
                    Cardinality::AtMostOnce => "AtMostOnce",
                    Cardinality::Multi => "Multi",
                    Cardinality::Absent => "Absent",
                    Cardinality::Unknown => "Unknown",
                };
                println!("{}{}: ({}, {})", "  ".repeat(depth), b.name, s, c);
                print_demands_inner(&b.expr, depth + 1);
            }
            print_demands_inner(&scope.body, depth);
        }
        Expr::Lam(_, _, scope) => {
            print_demands_inner(&scope.body, depth);
        }
        Expr::App(_, f, args) => {
            print_demands_inner(f, depth);
            for a in args {
                print_demands_inner(a, depth);
            }
        }
        Expr::Lookup(_, obj, _, fb) => {
            print_demands_inner(obj, depth);
            if let Some(f) = fb {
                print_demands_inner(f, depth);
            }
        }
        Expr::List(_, xs) => {
            for x in xs {
                print_demands_inner(x, depth);
            }
        }
        Expr::Block(_, bm) => {
            for (_, v) in bm.iter() {
                print_demands_inner(v, depth);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::demand::Strictness;
    use crate::core::expr::acore::*;

    #[test]
    fn single_use_let_binding_gets_multi() {
        // All core Let scopes are recursive, so even a single-use
        // binding must be Multi (not AtMostOnce) to preserve the
        // blackhole detection for self-referential bindings.
        let x = free("x");
        let expr = let_(vec![(x.clone(), num(1))], var(x));
        let (result, _sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                let x_binding = &scope.pattern[0];
                assert_eq!(x_binding.demand.cardinality, Cardinality::Multi);
                assert_eq!(x_binding.demand.strictness, Strictness::Strict);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn unused_binding_gets_absent() {
        let x = free("x");
        let y = free("y");
        let expr = let_(vec![(x.clone(), num(1)), (y.clone(), num(2))], var(x));
        let (result, _sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(scope.pattern[1].demand.cardinality, Cardinality::Absent);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn multi_use_binding_gets_multi() {
        let x = free("x");
        let expr = let_(
            vec![(x.clone(), num(1))],
            app(bif("ADD"), vec![var(x.clone()), var(x)]),
        );
        let (result, _sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(scope.pattern[0].demand.cardinality, Cardinality::Multi);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn if_branches_get_lazy_once_from_intrinsic_sig() {
        let analyser = DemandAnalyser::new();
        let if_sig = analyser.intrinsic_sigs.get("IF").unwrap();
        assert_eq!(if_sig.len(), 3);
        assert_eq!(if_sig[0], Demand::strict_once());
        assert_eq!(if_sig[1], Demand::lazy_once());
        assert_eq!(if_sig[2], Demand::lazy_once());
    }

    #[test]
    fn lam_signature_recorded() {
        let x = free("x");
        // \x y -> x (y is unused)
        let lam_expr = lam(vec!["x".to_string(), "y".to_string()], var(x.clone()));
        let (result, sigs) = analyse_demands(&lam_expr);

        assert!(!sigs.is_empty(), "signature table should not be empty");

        if let Expr::Lam(_, _, scope) = &*result.inner {
            let key = Rc::as_ptr(&scope.body.inner) as usize;
            let sig = sigs.get(&key).expect("signature should exist for lambda");
            assert_eq!(sig.len(), 2);
            assert_eq!(sig[0].cardinality, Cardinality::AtMostOnce); // x used once
            assert_eq!(sig[1].cardinality, Cardinality::Absent); // y unused
        } else {
            panic!("expected Lam");
        }
    }

    #[test]
    fn absent_demand_skips_update() {
        assert!(Demand::absent().skip_update());
    }

    #[test]
    fn demand_lub_combines_branches() {
        let strict_once = Demand::strict_once();
        let lazy_once = Demand::lazy_once();
        let combined = strict_once.lub(lazy_once);
        assert_eq!(combined.strictness, Strictness::Lazy);
        assert_eq!(combined.cardinality, Cardinality::AtMostOnce);
    }

    #[test]
    fn demand_plus_combines_sequentially() {
        let once = Demand::strict_once();
        let combined = once.plus(once);
        assert_eq!(combined.cardinality, Cardinality::Multi);
        assert_eq!(combined.strictness, Strictness::Strict);
    }
}
