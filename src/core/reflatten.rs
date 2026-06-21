//! Re-flatten nested Let/LetRec scopes after demand analysis.
//!
//! SCC splitting decomposes large `LetRec` scopes into chains of
//! nested `Let` + `LetRec` scopes for precise demand analysis.  Each
//! nested scope becomes a separate `EnvFrame` at runtime, causing
//! O(depth) variable lookup regressions.
//!
//! This pass runs **after** demand analysis and **before** STG
//! compilation.  It merges chains of directly nested `Let`/`LetRec`
//! scopes back into a single scope while preserving the per-binding
//! `Demand` annotations that the analysis pass populated.
//!
//! `DefaultBlockLet` scopes are never merged — they represent block
//! values, not function bindings.

use crate::core::binding::{CoreBinding, Scope};
use crate::core::demand::Demand;
use crate::core::expr::{close_expr_vars, open_let_scope_full, Expr, LetType, RcExpr};

/// Run the re-flatten pass over an entire expression tree.
pub fn reflatten(expr: &RcExpr) -> RcExpr {
    walk(expr)
}

/// Recursive walk: process children first (bottom-up), then flatten
/// the current node if it heads a chain of mergeable Let scopes.
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
            let rebuilt = RcExpr::from(Expr::Let(*s, new_scope, *let_type));

            // Only flatten OtherLet scopes.
            if *let_type != LetType::OtherLet {
                return rebuilt;
            }

            // Check if this heads a chain of mergeable scopes.
            flatten_chain(&rebuilt)
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

/// A binding with its demand annotation, in opened (free-variable) form.
struct OpenBinding {
    name: String,
    expr: RcExpr,
    demand: Demand,
}

/// Check if `expr` heads a chain of mergeable Let/LetRec scopes.
/// If so, collect all bindings and merge into a single scope.
/// Otherwise return the expression unchanged.
fn flatten_chain(expr: &RcExpr) -> RcExpr {
    let (smid, scope, let_type) = match &*expr.inner {
        Expr::Let(s, scope, lt) => (*s, scope, *lt),
        _ => return expr.clone(),
    };

    // Only flatten OtherLet scopes.
    if let_type != LetType::OtherLet {
        return expr.clone();
    }

    // Check if body is also a mergeable Let scope.
    if !is_mergeable_let(&scope.body) {
        return expr.clone();
    }

    // Collect the chain of nested scopes.
    // We open each scope to convert bound vars to free, collecting
    // bindings with their demand annotations.
    let mut all_bindings: Vec<OpenBinding> = Vec::new();
    let innermost_body = collect_chain(scope, &mut all_bindings);

    // If only one scope was collected, no flattening needed.
    if all_bindings.len() == scope.pattern.len() {
        return expr.clone();
    }

    // Close all bindings into a single scope, preserving demand.
    // All merged scopes use OtherLet — the STG compiler determines
    // recursiveness from the binding graph, not the LetType.
    close_let_scope_with_demand(smid, all_bindings, innermost_body, LetType::OtherLet)
}

/// Check whether an expression is a Let/LetRec scope that can be
/// merged (i.e. OtherLet, not DefaultBlockLet or destructure lets).
fn is_mergeable_let(expr: &RcExpr) -> bool {
    matches!(&*expr.inner, Expr::Let(_, _, LetType::OtherLet))
}

/// Recursively collect bindings from a chain of nested Let/LetRec scopes.
///
/// Opens each scope, appending bindings to `all_bindings` with their
/// demand annotations preserved.  Returns the innermost non-Let body.
fn collect_chain(
    scope: &Scope<Vec<CoreBinding<RcExpr>>, RcExpr>,
    all_bindings: &mut Vec<OpenBinding>,
) -> RcExpr {
    // Open this scope: Bound(scope=0) → Free(name)
    let (open_bindings, open_body) = open_let_scope_full(scope);

    // Preserve demand from the original CoreBindings.
    let demands: Vec<Demand> = scope.pattern.iter().map(|b| b.demand).collect();

    for ((name, expr), demand) in open_bindings.into_iter().zip(demands) {
        all_bindings.push(OpenBinding { name, expr, demand });
    }

    // If the opened body is another mergeable Let, recurse.
    if let Expr::Let(_, inner_scope, inner_lt) = &*open_body.inner {
        if *inner_lt == LetType::OtherLet {
            return collect_chain(inner_scope, all_bindings);
        }
    }

    open_body
}

/// Close a set of opened bindings back into a single Let scope,
/// preserving the demand annotation on each binding.
fn close_let_scope_with_demand(
    smid: crate::common::sourcemap::Smid,
    bindings: Vec<OpenBinding>,
    body: RcExpr,
    let_type: LetType,
) -> RcExpr {
    let names: Vec<String> = bindings.iter().map(|b| b.name.clone()).collect();
    let name_refs: Vec<&str> = names.iter().map(|n| n.as_str()).collect();

    let closed_bindings: Vec<CoreBinding<RcExpr>> = bindings
        .into_iter()
        .map(|b| {
            CoreBinding::with_demand(b.name, close_expr_vars(&name_refs, 0, &b.expr), b.demand)
        })
        .collect();
    let closed_body = close_expr_vars(&name_refs, 0, &body);

    let scope = Scope {
        pattern: closed_bindings,
        body: closed_body,
    };

    RcExpr::from(Expr::Let(smid, scope, let_type))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::sourcemap::Smid;
    use crate::core::binding::Var;
    use crate::core::demand::{Cardinality, Demand, Strictness};
    use crate::core::expr::{close_let_scope, Expr, LetType, Primitive, RcExpr};

    fn var(name: &str) -> RcExpr {
        RcExpr::from(Expr::Var(Smid::default(), Var::Free(name.to_string())))
    }

    fn lit_num(n: i64) -> RcExpr {
        RcExpr::from(Expr::Literal(
            Smid::default(),
            Primitive::Num(serde_json::Number::from(n)),
        ))
    }

    fn make_let_with_demand(bindings: Vec<(&str, RcExpr, Demand)>, body: RcExpr) -> RcExpr {
        let demands: Vec<Demand> = bindings.iter().map(|(_, _, d)| *d).collect();
        let pairs: Vec<(String, RcExpr)> = bindings
            .into_iter()
            .map(|(n, e, _)| (n.to_string(), e))
            .collect();
        let mut scope = close_let_scope(pairs, body);
        // Set demands on the bindings.
        for (b, d) in scope.pattern.iter_mut().zip(demands) {
            b.demand = d;
        }
        RcExpr::from(Expr::Let(Smid::default(), scope, LetType::OtherLet))
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

    /// Get demand of the nth binding in the outermost Let scope.
    fn binding_demand(expr: &RcExpr, n: usize) -> Demand {
        match &*expr.inner {
            Expr::Let(_, scope, _) => scope.pattern[n].demand,
            _ => panic!("not a Let"),
        }
    }

    #[test]
    fn test_nested_lets_flattened() {
        // Let { a = 1 } in Let { b = 2 } in body
        // Should become Let { a = 1, b = 2 } in body
        let inner = make_let_with_demand(vec![("b", lit_num(2), Demand::default())], var("x"));
        let outer = make_let_with_demand(vec![("a", lit_num(1), Demand::default())], inner);

        let result = reflatten(&outer);
        assert_eq!(count_lets(&result), 1);
        assert_eq!(outer_let_bindings(&result), 2);
    }

    #[test]
    fn test_demand_preserved() {
        let strict_once = Demand {
            strictness: Strictness::Strict,
            cardinality: Cardinality::AtMostOnce,
            whnf: false,
            recursive: false,
        };
        let lazy_multi = Demand {
            strictness: Strictness::Lazy,
            cardinality: Cardinality::Multi,
            whnf: false,
            recursive: false,
        };

        let inner = make_let_with_demand(vec![("b", lit_num(2), lazy_multi)], var("x"));
        let outer = make_let_with_demand(vec![("a", lit_num(1), strict_once)], inner);

        let result = reflatten(&outer);
        assert_eq!(count_lets(&result), 1);
        assert_eq!(binding_demand(&result, 0), strict_once);
        assert_eq!(binding_demand(&result, 1), lazy_multi);
    }

    #[test]
    fn test_default_block_let_not_flattened() {
        // DefaultBlockLet should not be merged.
        let pairs: Vec<(String, RcExpr)> = vec![("b".to_string(), lit_num(2))];
        let inner_scope = close_let_scope(pairs, var("x"));
        let inner = RcExpr::from(Expr::Let(
            Smid::default(),
            inner_scope,
            LetType::DefaultBlockLet,
        ));

        let outer = make_let_with_demand(vec![("a", lit_num(1), Demand::default())], inner);

        let result = reflatten(&outer);
        // Should be 2 lets — the inner DefaultBlockLet is NOT merged.
        assert_eq!(count_lets(&result), 2);
    }

    #[test]
    fn test_single_let_unchanged() {
        let expr = make_let_with_demand(vec![("a", lit_num(1), Demand::default())], var("x"));

        let result = reflatten(&expr);
        assert_eq!(count_lets(&result), 1);
        assert_eq!(outer_let_bindings(&result), 1);
    }

    #[test]
    fn test_three_deep_chain_flattened() {
        // Let { a = 1 } in Let { b = a } in Let { c = b } in body
        let c = make_let_with_demand(vec![("c", var("b"), Demand::default())], var("x"));
        let b = make_let_with_demand(vec![("b", var("a"), Demand::default())], c);
        let a = make_let_with_demand(vec![("a", lit_num(1), Demand::default())], b);

        let result = reflatten(&a);
        assert_eq!(count_lets(&result), 1);
        assert_eq!(outer_let_bindings(&result), 3);
    }

    #[test]
    fn test_cross_scope_references_correct() {
        // Let { a = 1 } in Let { b = a + 1 } in b
        // After flattening: Let { a = 1, b = a + 1 } in b
        // The reference from b to a should be Bound(scope=0, binder=0)
        let inner = make_let_with_demand(vec![("b", var("a"), Demand::default())], var("b"));
        let outer = make_let_with_demand(vec![("a", lit_num(1), Demand::default())], inner);

        let result = reflatten(&outer);
        assert_eq!(count_lets(&result), 1);
        assert_eq!(outer_let_bindings(&result), 2);

        // Verify b's RHS references a as Bound(scope=0, binder=0)
        if let Expr::Let(_, scope, _) = &*result.inner {
            let b_rhs = &scope.pattern[1].expr;
            if let Expr::Var(_, Var::Bound(bv)) = &*b_rhs.inner {
                assert_eq!(bv.scope, 0);
                assert_eq!(bv.binder, 0);
            } else {
                panic!("expected bound var in b's RHS, got: {:?}", b_rhs.inner);
            }

            // Verify body references b as Bound(scope=0, binder=1)
            if let Expr::Var(_, Var::Bound(bv)) = &*scope.body.inner {
                assert_eq!(bv.scope, 0);
                assert_eq!(bv.binder, 1);
            } else {
                panic!("expected bound var in body, got: {:?}", scope.body.inner);
            }
        } else {
            panic!("expected Let");
        }
    }
}
