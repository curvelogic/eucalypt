//! SCC-based LetRec splitting pass.
//!
//! Eucalypt desugars all scopes as recursive `Let` (LetRec), but most
//! bindings do not participate in recursion.  This pass builds a
//! dependency graph for each non-block Let scope, computes strongly
//! connected components (SCCs) via Tarjan's algorithm, topologically
//! sorts them, and re-emits the scope as a chain of minimal nested
//! `Let` scopes — one per SCC.
//!
//! Single-binding, non-self-recursive SCCs become single-binding `Let`
//! scopes that the STG compiler can compile as non-recursive `Let`
//! (rather than `LetRec`), enabling better demand analysis and inlining.
//!
//! **`DefaultBlockLet` scopes are never split** — all bindings must
//! stay together for the block map.

use crate::core::binding::CoreBinding;
use crate::core::binding::Scope;
use crate::core::expr::{close_let_scope, open_let_scope_full, Expr, LetType, RcExpr};

/// Run the SCC splitting pass over an entire expression tree.
pub fn split_letrecs(expr: &RcExpr) -> RcExpr {
    walk(expr)
}

/// Recursive walk: process children first (bottom-up), then split the
/// current node if it is a splittable Let scope.
fn walk(expr: &RcExpr) -> RcExpr {
    match &*expr.inner {
        Expr::Let(s, scope, let_type) => {
            // Recurse into binding RHSs and body first.
            let new_bindings: Vec<CoreBinding<RcExpr>> = scope
                .pattern
                .iter()
                .map(|b| CoreBinding::with_demand(b.name.clone(), walk(&b.expr), b.demand))
                .collect();
            let new_body = walk(&scope.body);
            let new_scope = Scope {
                pattern: new_bindings,
                body: new_body,
            };

            // Only split OtherLet scopes with more than one binding.
            // DefaultBlockLet, DestructureBlockLet, DestructureListLet
            // must not be split.
            if *let_type != LetType::OtherLet || new_scope.pattern.len() <= 1 {
                return RcExpr::from(Expr::Let(*s, new_scope, *let_type));
            }

            split_scope(*s, new_scope)
        }
        Expr::Lam(s, inl, scope) => {
            let new_body = walk(&scope.body);
            RcExpr::from(Expr::Lam(
                *s,
                *inl,
                Scope {
                    pattern: scope.pattern.clone(),
                    body: new_body,
                },
            ))
        }
        Expr::App(s, f, args) => {
            let new_f = walk(f);
            let new_args: Vec<RcExpr> = args.iter().map(walk).collect();
            RcExpr::from(Expr::App(*s, new_f, new_args))
        }
        Expr::Lookup(s, obj, key, fb) => {
            let new_obj = walk(obj);
            let new_fb = fb.as_ref().map(walk);
            RcExpr::from(Expr::Lookup(*s, new_obj, key.clone(), new_fb))
        }
        Expr::List(s, xs) => {
            let new_xs: Vec<RcExpr> = xs.iter().map(walk).collect();
            RcExpr::from(Expr::List(*s, new_xs))
        }
        Expr::Block(s, bm) => {
            let new_bm = bm.map_values(|v| Ok::<_, ()>(walk(&v))).unwrap();
            RcExpr::from(Expr::Block(*s, new_bm))
        }
        Expr::Meta(s, e, m) => {
            let new_e = walk(e);
            let new_m = walk(m);
            RcExpr::from(Expr::Meta(*s, new_e, new_m))
        }
        Expr::ArgTuple(s, xs) => {
            let new_xs: Vec<RcExpr> = xs.iter().map(walk).collect();
            RcExpr::from(Expr::ArgTuple(*s, new_xs))
        }
        Expr::Soup(s, xs, b) => {
            let new_xs: Vec<RcExpr> = xs.iter().map(walk).collect();
            RcExpr::from(Expr::Soup(*s, new_xs, *b))
        }
        Expr::Operator(s, f, p, e) => {
            let new_e = walk(e);
            RcExpr::from(Expr::Operator(*s, *f, *p, new_e))
        }
        // Leaf nodes — no recursion needed.
        _ => expr.clone(),
    }
}

/// Split a single `OtherLet` scope into a chain of nested `Let` scopes,
/// one per SCC, in dependency order (innermost = first needed).
fn split_scope(
    smid: crate::common::sourcemap::Smid,
    scope: Scope<Vec<CoreBinding<RcExpr>>, RcExpr>,
) -> RcExpr {
    let n = scope.pattern.len();

    // Open the scope: convert Bound(scope=0) back to Free so we can
    // inspect inter-binding references.
    let (open_bindings, open_body) = open_let_scope_full(&scope);

    // Build name → index map.
    let name_to_idx: std::collections::HashMap<&str, usize> = open_bindings
        .iter()
        .enumerate()
        .map(|(i, (name, _))| (name.as_str(), i))
        .collect();

    // Build dependency graph: deps[i] = set of sibling indices that
    // binding i's RHS references as free variables.
    let mut deps: Vec<Vec<usize>> = Vec::with_capacity(n);
    for (_, rhs) in &open_bindings {
        let fvs = crate::core::expr::free_vars(rhs);
        let mut binding_deps: Vec<usize> = Vec::new();
        for fv in &fvs {
            if let Some(&j) = name_to_idx.get(fv.as_str()) {
                binding_deps.push(j);
            }
        }
        deps.push(binding_deps);
    }

    // Compute SCCs via Tarjan's algorithm.
    let sccs = tarjan_scc(n, &deps);
    // `sccs` is in reverse topological order (Tarjan's produces them
    // bottom-up): sinks first (no dependencies), sources last
    // (depended upon but don't depend on others in the SCC DAG).

    // If there's only one SCC covering all bindings, no splitting needed.
    if sccs.len() <= 1 {
        return RcExpr::from(Expr::Let(smid, scope, LetType::OtherLet));
    }

    // Build the nested chain of Let scopes (inside-out construction).
    //
    // Nesting rule: a binding's scope must ENCLOSE (be outer to) any
    // scope that references it, because inner scopes can see outer
    // bindings but not vice versa.
    //
    // - Sinks (no dependencies) should be OUTERMOST — they provide
    //   definitions that others need.
    // - Sources (nothing depends on them) should be INNERMOST — they
    //   consume from outer scopes and wrap the body.
    //
    // Tarjan gives [sinks, ..., sources].  We build inside-out (first
    // processed = innermost), so we iterate in REVERSE to place sinks
    // outermost and sources innermost.

    let mut result = open_body;

    for scc in sccs.iter().rev() {
        let scc_bindings: Vec<(String, RcExpr)> =
            scc.iter().map(|&i| open_bindings[i].clone()).collect();

        let new_scope = close_let_scope(scc_bindings, result);
        result = RcExpr::from(Expr::Let(smid, new_scope, LetType::OtherLet));
    }

    result
}

// --- Tarjan's SCC algorithm ---

struct TarjanState {
    index_counter: usize,
    stack: Vec<usize>,
    on_stack: Vec<bool>,
    index: Vec<Option<usize>>,
    lowlink: Vec<usize>,
    sccs: Vec<Vec<usize>>,
}

/// Compute SCCs using Tarjan's algorithm.  Returns SCCs in reverse
/// topological order (sinks first).
fn tarjan_scc(n: usize, deps: &[Vec<usize>]) -> Vec<Vec<usize>> {
    let mut state = TarjanState {
        index_counter: 0,
        stack: Vec::new(),
        on_stack: vec![false; n],
        index: vec![None; n],
        lowlink: vec![0; n],
        sccs: Vec::new(),
    };

    for v in 0..n {
        if state.index[v].is_none() {
            strongconnect(v, deps, &mut state);
        }
    }

    state.sccs
}

fn strongconnect(v: usize, deps: &[Vec<usize>], state: &mut TarjanState) {
    state.index[v] = Some(state.index_counter);
    state.lowlink[v] = state.index_counter;
    state.index_counter += 1;
    state.stack.push(v);
    state.on_stack[v] = true;

    for &w in &deps[v] {
        if state.index[w].is_none() {
            strongconnect(w, deps, state);
            state.lowlink[v] = state.lowlink[v].min(state.lowlink[w]);
        } else if state.on_stack[w] {
            state.lowlink[v] = state.lowlink[v].min(state.index[w].unwrap());
        }
    }

    if state.lowlink[v] == state.index[v].unwrap() {
        let mut scc = Vec::new();
        loop {
            let w = state.stack.pop().unwrap();
            state.on_stack[w] = false;
            scc.push(w);
            if w == v {
                break;
            }
        }
        state.sccs.push(scc);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::sourcemap::Smid;
    use crate::core::expr::{close_let_scope, Expr, LetType, Primitive, RcExpr};

    /// Helper: build a Let scope with the given bindings (as name-expr pairs)
    /// and body, and run the SCC splitter.
    fn make_let(bindings: Vec<(&str, RcExpr)>, body: RcExpr) -> RcExpr {
        let pairs: Vec<(String, RcExpr)> = bindings
            .into_iter()
            .map(|(n, e)| (n.to_string(), e))
            .collect();
        let scope = close_let_scope(pairs, body);
        RcExpr::from(Expr::Let(Smid::default(), scope, LetType::OtherLet))
    }

    fn var(name: &str) -> RcExpr {
        RcExpr::from(Expr::Var(
            Smid::default(),
            crate::core::binding::Var::Free(name.to_string()),
        ))
    }

    fn lit_num(n: i64) -> RcExpr {
        RcExpr::from(Expr::Literal(
            Smid::default(),
            Primitive::Num(serde_json::Number::from(n)),
        ))
    }

    fn app(f: RcExpr, args: Vec<RcExpr>) -> RcExpr {
        RcExpr::from(Expr::App(Smid::default(), f, args))
    }

    /// Count the number of nested Let scopes (depth of Let nesting).
    fn count_lets(expr: &RcExpr) -> usize {
        match &*expr.inner {
            Expr::Let(_, scope, _) => 1 + count_lets(&scope.body),
            _ => 0,
        }
    }

    /// Count bindings in the outermost Let scope.
    fn outer_let_bindings(expr: &RcExpr) -> usize {
        match &*expr.inner {
            Expr::Let(_, scope, _) => scope.pattern.len(),
            _ => 0,
        }
    }

    #[test]
    fn test_independent_bindings_split() {
        // let a = 1; b = 2 in a
        // a and b are independent → should become 2 nested single-binding lets
        let expr = make_let(vec![("a", lit_num(1)), ("b", lit_num(2))], var("a"));
        let result = split_letrecs(&expr);
        assert_eq!(count_lets(&result), 2);
        assert_eq!(outer_let_bindings(&result), 1);
    }

    #[test]
    fn test_mutually_recursive_not_split() {
        // let f = g; g = f in f
        // f and g are mutually recursive → should stay as one scope
        let expr = make_let(vec![("f", var("g")), ("g", var("f"))], var("f"));
        let result = split_letrecs(&expr);
        assert_eq!(count_lets(&result), 1);
        assert_eq!(outer_let_bindings(&result), 2);
    }

    #[test]
    fn test_chain_dependency() {
        // let a = 1; b = a; c = b in c
        // Linear chain: c depends on b depends on a
        // Should become 3 nested single-binding lets
        let expr = make_let(
            vec![("a", lit_num(1)), ("b", var("a")), ("c", var("b"))],
            var("c"),
        );
        let result = split_letrecs(&expr);
        assert_eq!(count_lets(&result), 3);
    }

    #[test]
    fn test_self_recursive_stays_single() {
        // let f = f(1) in f
        // f is self-recursive → single-binding Let that the STG compiler
        // will still compile as LetRec
        let expr = make_let(vec![("f", app(var("f"), vec![lit_num(1)]))], var("f"));
        let result = split_letrecs(&expr);
        // Single binding → no splitting possible, stays as 1 Let
        assert_eq!(count_lets(&result), 1);
    }

    #[test]
    fn test_mixed_recursive_and_independent() {
        // let a = 1; f = g(a); g = f(a) in f
        // f and g are mutually recursive, a is independent
        // Should become 2 lets: one for a, one for {f,g}
        let expr = make_let(
            vec![
                ("a", lit_num(1)),
                ("f", app(var("g"), vec![var("a")])),
                ("g", app(var("f"), vec![var("a")])),
            ],
            var("f"),
        );
        let result = split_letrecs(&expr);
        assert_eq!(count_lets(&result), 2);
    }

    #[test]
    fn test_default_block_let_not_split() {
        // DefaultBlockLet scopes must never be split
        let pairs: Vec<(String, RcExpr)> =
            vec![("a".to_string(), lit_num(1)), ("b".to_string(), lit_num(2))];
        let scope = close_let_scope(pairs, var("a"));
        let expr = RcExpr::from(Expr::Let(Smid::default(), scope, LetType::DefaultBlockLet));
        let result = split_letrecs(&expr);
        assert_eq!(count_lets(&result), 1);
        assert_eq!(outer_let_bindings(&result), 2);
    }

    #[test]
    fn test_tarjan_simple() {
        // 0 → 1, 1 → 0, 2 → 1
        // SCCs: {0,1} and {2}
        let deps = vec![vec![1], vec![0], vec![1]];
        let sccs = tarjan_scc(3, &deps);
        assert_eq!(sccs.len(), 2);
    }

    #[test]
    fn test_tarjan_no_edges() {
        // 3 independent nodes → 3 SCCs
        let deps = vec![vec![], vec![], vec![]];
        let sccs = tarjan_scc(3, &deps);
        assert_eq!(sccs.len(), 3);
    }
}
