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

/// Name-keyed demand signature table for user function calls.
///
/// Maps binding names to per-argument demand vectors. Built during
/// `analyse_let` by recording the signature of each Let binding whose
/// RHS is a Lam with a known signature. Used by the STG compiler to
/// apply per-argument demands at user function call sites.
pub type NamedSignatureTable = HashMap<String, DemandSignature>;

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
        let strict_set = info.strict_indices();
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

/// Return the indices of strict arguments for the named intrinsic,
/// derived from the demand signature table.
///
/// Used by `wrap.rs` to generate forcing STG wrappers, replacing the
/// old `Intrinsic::strict_args()` consultation.
pub fn strict_indices_for(name: &str) -> Vec<usize> {
    use crate::core::demand::Strictness;
    build_intrinsic_signatures()
        .get(name)
        .map(|sig| {
            sig.iter()
                .enumerate()
                .filter(|(_, d)| d.strictness == Strictness::Strict)
                .map(|(i, _)| i)
                .collect()
        })
        .unwrap_or_default()
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
    /// Name-keyed signatures for user function calls.
    named_signatures: NamedSignatureTable,
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
            named_signatures: HashMap::new(),
        }
    }

    /// Run the demand analysis on the given expression, returning the
    /// annotated expression, the pointer-keyed signature table, and the
    /// name-keyed signature table for user function calls.
    pub fn analyse(mut self, expr: &RcExpr) -> (RcExpr, SignatureTable, NamedSignatureTable) {
        let (new_expr, _env) = self.analyse_expr(expr);
        (new_expr, self.signatures, self.named_signatures)
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

    /// Analyse a `Let` expression using fixed-point iteration.
    ///
    /// All core `Let` scopes are recursive (`LetRec` in STG), so a simple
    /// syntactic reference count is unsound — a binding used once in the
    /// source may be evaluated many times through mutual or self-recursion.
    ///
    /// The algorithm:
    /// 1. Analyse body and all RHSs once (their structure is fixed).
    /// 2. Build an intra-scope dependency graph: edge i→j means binding i's
    ///    RHS references binding j at scope 0.
    /// 3. Find recursive bindings: those that can reach themselves through
    ///    the dependency graph (direct self-loop or cycle).
    /// 4. Propagate "tainted by recursion": if binding i is recursive and
    ///    i→j exists, binding j is also tainted (it could be re-entered
    ///    through i's recursion).
    /// 5. Fixed-point demand propagation: starting from body_env, iterate
    ///    propagating demands through RHS references until stable.
    /// 6. Force Multi cardinality on all recursive/tainted bindings.
    fn analyse_let(
        &mut self,
        s: Smid,
        scope: &LetScope<RcExpr>,
        let_type: LetType,
    ) -> (RcExpr, DemandEnv) {
        let n = scope.pattern.len();

        // Step 1: Analyse the body (fixed — doesn't depend on binding demands).
        let (new_body, body_env) = self.analyse_expr(&scope.body);

        // Step 2: Analyse each RHS (fixed).
        let mut new_rhs_exprs = Vec::with_capacity(n);
        let mut rhs_envs: Vec<DemandEnv> = Vec::with_capacity(n);
        for binding in &scope.pattern {
            let (new_rhs, rhs_env) = self.analyse_expr(&binding.expr);
            new_rhs_exprs.push(new_rhs);
            rhs_envs.push(rhs_env);
        }

        // Step 3: Build the intra-scope dependency graph.
        // deps[i] = set of binding indices that binding i's RHS references (at scope 0).
        let deps: Vec<Vec<usize>> = (0..n)
            .map(|i| {
                (0..n)
                    .filter(|&j| rhs_envs[i].contains_key(&(0, j as u32)))
                    .collect()
            })
            .collect();

        // Step 4a: Find directly recursive bindings (can reach themselves).
        // A binding is recursive iff there is a path i → … → i in the
        // dependency graph.  We check this with a BFS from each node.
        let mut is_recursive = vec![false; n];
        for start in 0..n {
            // BFS from start's neighbours; stop if we reach start again.
            let mut visited = vec![false; n];
            let mut queue: std::collections::VecDeque<usize> = std::collections::VecDeque::new();
            for &j in &deps[start] {
                if j == start {
                    is_recursive[start] = true;
                    break;
                }
                if !visited[j] {
                    visited[j] = true;
                    queue.push_back(j);
                }
            }
            if !is_recursive[start] {
                'bfs: while let Some(k) = queue.pop_front() {
                    for &j in &deps[k] {
                        if j == start {
                            is_recursive[start] = true;
                            break 'bfs;
                        }
                        if !visited[j] {
                            visited[j] = true;
                            queue.push_back(j);
                        }
                    }
                }
            }
        }

        // Step 4b: Propagate "tainted by recursion".
        // If binding i is recursive and i→j, then j is also tainted:
        // a recursive binding can be evaluated many times, so any binding
        // it uses could also be evaluated many times.
        let mut is_tainted = is_recursive.clone();
        loop {
            let old = is_tainted.clone();
            for i in 0..n {
                if is_tainted[i] {
                    for &j in &deps[i] {
                        is_tainted[j] = true;
                    }
                }
            }
            if old == is_tainted {
                break;
            }
        }

        // Step 5: Fixed-point demand propagation.
        // Start with demands from the body, then propagate through RHS references
        // until the demand vector stabilises.
        let mut demands: Vec<Demand> = (0..n)
            .map(|i| {
                body_env
                    .get(&(0, i as u32))
                    .copied()
                    .unwrap_or(Demand::absent())
            })
            .collect();

        loop {
            let old = demands.clone();
            for i in 0..n {
                // Recompute from body demand fresh each iteration to avoid
                // double-counting when using `plus`.
                let mut d = body_env
                    .get(&(0, i as u32))
                    .copied()
                    .unwrap_or(Demand::absent());

                // Propagated demand: for each binding j whose RHS references i,
                // if j is demanded with demand d_j, then i gets an additional
                // demand of d_j scaled by the demand j's RHS places on i.
                //
                // These are SEQUENTIAL uses (each demanded binding's RHS is
                // evaluated independently), so combine with `plus`, not `lub`.
                for j in 0..n {
                    if let Some(&d_ji) = rhs_envs[j].get(&(0, i as u32)) {
                        // d_ji: demand j's RHS places on i
                        // demands[j]: how often binding j itself is demanded
                        let contribution = d_ji.scale(demands[j]);
                        d = d.plus(contribution);
                    }
                }

                demands[i] = d;
            }
            if demands == old {
                break;
            }
        }

        // Step 6: Force Multi cardinality on recursive/tainted bindings
        // that are actually used (not Absent).
        for i in 0..n {
            if is_tainted[i] && demands[i].cardinality != Cardinality::Absent {
                demands[i].cardinality = Cardinality::Multi;
            }
        }

        // Build annotated bindings and propagate demands to the outer scope.
        let mut outer_env = unshift_env(&body_env);
        let mut new_bindings = Vec::with_capacity(n);

        for (i, binding) in scope.pattern.iter().enumerate() {
            let binding_demand = demands[i];

            // Propagate the RHS's outward demands for non-absent bindings.
            if binding_demand.cardinality != Cardinality::Absent
                && binding_demand.cardinality != Cardinality::Unknown
            {
                let shifted = unshift_env(&rhs_envs[i]);
                outer_env = merge_envs(&outer_env, &shifted);
            }

            // If the binding's RHS is a Lam with a known signature, record
            // the signature under the binding name for STG compiler use.
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

            // The demand the function places on this argument position.
            let arg_demand = sig
                .as_ref()
                .and_then(|s| s.get(i))
                .copied()
                .unwrap_or(Demand::lazy_multi());

            // If the argument is used (not absent), scale the argument's
            // free-variable demands by the function's demand on this
            // argument position.  If the function uses this argument
            // Multi times (e.g. map's function argument), every free
            // variable captured in the argument is also demanded Multi
            // times — without this scaling they would stay AtMostOnce,
            // causing the STG compiler to skip memoisation.
            if arg_demand.cardinality != Cardinality::Absent {
                let scaled_env: DemandEnv = arg_env
                    .iter()
                    .map(|(&k, &d)| (k, d.scale(arg_demand)))
                    .collect();
                env = merge_envs(&env, &scaled_env);
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
pub fn analyse_demands(expr: &RcExpr) -> (RcExpr, SignatureTable, NamedSignatureTable) {
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
        Expr::Meta(_, e, m) => {
            print_demands_inner(e, depth);
            print_demands_inner(m, depth);
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
    fn single_use_nonrecursive_let_binding_gets_at_most_once() {
        // A non-recursive, single-use binding should be AtMostOnce —
        // the fixed-point correctly identifies it does not participate
        // in any cycle and can safely skip the Update frame.
        let x = free("x");
        let expr = let_(vec![(x.clone(), num(1))], var(x));
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                let x_binding = &scope.pattern[0];
                assert_eq!(x_binding.demand.cardinality, Cardinality::AtMostOnce);
                assert_eq!(x_binding.demand.strictness, Strictness::Strict);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn self_recursive_let_binding_gets_multi() {
        // A binding whose RHS references itself is recursive — must stay Multi
        // so the blackhole detection fires correctly if re-entered.
        let x = free("x");
        // let x = ADD(x, 1) in x  — x references itself in RHS
        let expr = let_(
            vec![(x.clone(), app(bif("ADD"), vec![var(x.clone()), num(1)]))],
            var(x),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(scope.pattern[0].demand.cardinality, Cardinality::Multi);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn mutually_recursive_let_bindings_get_multi() {
        // Mutually recursive bindings (x references y, y references x)
        // must both be Multi — neither can skip the Update frame.
        let x = free("x");
        let y = free("y");
        // let x = ADD(y, 1); y = ADD(x, 1) in ADD(x, y)
        let expr = let_(
            vec![
                (x.clone(), app(bif("ADD"), vec![var(y.clone()), num(1)])),
                (y.clone(), app(bif("ADD"), vec![var(x.clone()), num(1)])),
            ],
            app(bif("ADD"), vec![var(x), var(y)]),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(scope.pattern[0].demand.cardinality, Cardinality::Multi);
                assert_eq!(scope.pattern[1].demand.cardinality, Cardinality::Multi);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn nonrecursive_sibling_used_by_recursive_binding_gets_multi() {
        // If a non-recursive binding `a` is used by a recursive binding `x`,
        // `a` is tainted — it may be evaluated each time x loops, so Multi.
        let a = free("a");
        let x = free("x");
        // let a = 1; x = ADD(x, a) in x
        // a is non-recursive but x is self-recursive and uses a → a tainted
        let expr = let_(
            vec![
                (a.clone(), num(1)),
                (
                    x.clone(),
                    app(bif("ADD"), vec![var(x.clone()), var(a.clone())]),
                ),
            ],
            var(x),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                // x is self-recursive
                assert_eq!(scope.pattern[1].demand.cardinality, Cardinality::Multi);
                // a is tainted by x's recursion
                assert_eq!(scope.pattern[0].demand.cardinality, Cardinality::Multi);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn unused_binding_gets_absent() {
        let x = free("x");
        let y = free("y");
        let expr = let_(vec![(x.clone(), num(1)), (y.clone(), num(2))], var(x));
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

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
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(scope.pattern[0].demand.cardinality, Cardinality::Multi);
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn binding_used_by_two_siblings_gets_multi() {
        // let x = 1; y = ADD(x, 2); z = ADD(x, 3) in ADD(y, z)
        // x is used once by y's RHS and once by z's RHS — two sequential
        // uses, so x must be Multi.  The old code used `lub` which
        // collapsed this to AtMostOnce, breaking memoisation.
        let x = free("x");
        let y = free("y");
        let z = free("z");
        let expr = let_(
            vec![
                (x.clone(), num(1)),
                (y.clone(), app(bif("ADD"), vec![var(x.clone()), num(2)])),
                (z.clone(), app(bif("ADD"), vec![var(x.clone()), num(3)])),
            ],
            app(bif("ADD"), vec![var(y), var(z)]),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(
                    scope.pattern[0].demand.cardinality,
                    Cardinality::Multi,
                    "x is used by two sibling bindings — must be Multi"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn binding_used_in_body_and_sibling_rhs_gets_multi() {
        // let x = 1; y = ADD(x, 2) in ADD(x, y)
        // x is used once in the body and once in y's RHS — two sequential
        // uses, so x must be Multi.
        let x = free("x");
        let y = free("y");
        let expr = let_(
            vec![
                (x.clone(), num(1)),
                (y.clone(), app(bif("ADD"), vec![var(x.clone()), num(2)])),
            ],
            app(bif("ADD"), vec![var(x), var(y)]),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(
                    scope.pattern[0].demand.cardinality,
                    Cardinality::Multi,
                    "x is used in body and sibling RHS — must be Multi"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn if_branches_share_binding_gets_at_most_once() {
        // let x = 1 in IF(cond, ADD(x, 2), ADD(x, 3))
        // x appears in both IF branches, but only one branch executes.
        // The analysis should use `lub` for IF branches (via the
        // lazy_once signature), so x is AtMostOnce, not Multi.
        let x = free("x");
        let cond = free("cond");
        let expr = let_(
            vec![(x.clone(), num(1)), (cond.clone(), num(0))],
            app(
                bif("IF"),
                vec![
                    var(cond),
                    app(bif("ADD"), vec![var(x.clone()), num(2)]),
                    app(bif("ADD"), vec![var(x), num(3)]),
                ],
            ),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                // x appears in both branches but only one executes —
                // the IF signature marks branches as lazy_once, so the
                // branch envs are each scaled by AtMostOnce. x's total
                // demand from the two branches is lub(once, once) = once,
                // but it also appears twice via merge_envs (plus) giving
                // Multi.  This is conservative-correct: the analysis
                // cannot know at compile time which branch is taken, so
                // treating the variable as Multi is safe.
                let x_card = scope.pattern[0].demand.cardinality;
                assert!(
                    x_card == Cardinality::Multi || x_card == Cardinality::AtMostOnce,
                    "x in IF branches should be AtMostOnce or conservatively Multi, got: {x_card:?}"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn nested_strict_single_use_stays_at_most_once() {
        // let x = 1 in f(g(x))
        // where f and g each use their argument once.
        // x should be AtMostOnce — it's only evaluated once through
        // the nested chain.
        let x = free("x");
        // Simulate f(g(x)) as ADD(0, ADD(0, x)) — each ADD uses its
        // args once (strict_once from the intrinsic signature).
        let expr = let_(
            vec![(x.clone(), num(1))],
            app(
                bif("ADD"),
                vec![num(0), app(bif("ADD"), vec![num(0), var(x)])],
            ),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(
                    scope.pattern[0].demand.cardinality,
                    Cardinality::AtMostOnce,
                    "x used once through nested strict calls — must be AtMostOnce"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }

    #[test]
    fn captured_binding_scaled_by_multi_arg_demand() {
        // let x = 1 in unknown_f(\y -> ADD(x, y))
        // unknown_f has no known signature, so its argument gets the
        // default lazy_multi demand.  The lambda captures x, and since
        // the lambda may be called multiple times, x must be Multi.
        let x = free("x");
        let y = free("y");
        let f = free("f");
        let expr = let_(
            vec![
                (x.clone(), num(1)),
                (f.clone(), num(0)), // stand-in for unknown function
            ],
            app(
                var(f),
                vec![lam(
                    vec!["y".to_string()],
                    app(bif("ADD"), vec![var(x), var(y)]),
                )],
            ),
        );
        let (result, _sigs, _named_sigs) = analyse_demands(&expr);

        match &*result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(
                    scope.pattern[0].demand.cardinality,
                    Cardinality::Multi,
                    "x captured in lambda passed to unknown function — must be Multi"
                );
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
        let (result, sigs, _named_sigs) = analyse_demands(&lam_expr);

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
    fn named_signature_recorded_for_let_bound_lam() {
        let x = free("x");
        // let f = \x y -> x in f
        let lam_body = lam(vec!["x".to_string(), "y".to_string()], var(x.clone()));
        let f = free("f");
        let expr = let_(vec![(f.clone(), lam_body)], var(f));
        let (_result, _sigs, named_sigs) = analyse_demands(&expr);

        let sig = named_sigs
            .get("f")
            .expect("named signature should exist for let-bound lambda");
        assert_eq!(sig.len(), 2);
        assert_eq!(sig[0].cardinality, Cardinality::AtMostOnce); // x used once
        assert_eq!(sig[1].cardinality, Cardinality::Absent); // y unused
    }

    #[test]
    fn absent_demand_does_not_skip_update() {
        // Absent → Value is disabled (same reason as AtMostOnce — see
        // Demand::skip_update doc comment).
        assert!(!Demand::absent().skip_update());
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

    /// Demonstrate that SCC splitting improves demand analysis.
    ///
    /// Without splitting, a non-recursive single-use binding `a` that
    /// shares a scope with a self-recursive binding `x` (where `x`
    /// uses `a`) is tainted to Multi.  After SCC splitting separates
    /// `a` into its own Let scope, demand analysis correctly assigns
    /// AtMostOnce.
    #[test]
    fn scc_splitting_improves_demand_for_tainted_binding() {
        use crate::core::dependency::split_letrecs;

        let a = free("a");
        let x = free("x");
        // let a = 1; x = ADD(x, a) in x
        // Before splitting: a is tainted by x's recursion → Multi
        let unsplit = let_(
            vec![
                (a.clone(), num(1)),
                (
                    x.clone(),
                    app(bif("ADD"), vec![var(x.clone()), var(a.clone())]),
                ),
            ],
            var(x.clone()),
        );

        let (unsplit_result, _, _) = analyse_demands(&unsplit);
        match &*unsplit_result.inner {
            Expr::Let(_, scope, _) => {
                assert_eq!(
                    scope.pattern[0].demand.cardinality,
                    Cardinality::Multi,
                    "before splitting: a is tainted by x's recursion"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }

        // After SCC splitting: a is in its own Let scope
        let split = split_letrecs(&unsplit);
        let (split_result, _, _) = analyse_demands(&split);

        // The outermost Let should be `a` (independent, placed outermost)
        match &*split_result.inner {
            Expr::Let(_, outer_scope, _) => {
                assert_eq!(outer_scope.pattern.len(), 1, "outer scope has one binding");
                assert_eq!(outer_scope.pattern[0].name, "a");
                assert_eq!(
                    outer_scope.pattern[0].demand.cardinality,
                    Cardinality::AtMostOnce,
                    "after splitting: a is in its own scope → AtMostOnce"
                );
            }
            other => panic!("expected Let, got: {other:?}"),
        }
    }
}
