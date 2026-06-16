//! Tag selected lambdas as inlinable for inline pass
use crate::core::{
    binding::Var,
    expr::{Expr, LamScope, LetType, RcExpr},
};
use std::collections::HashSet;

/// Walk the expression tagging combinators and destructuring lambdas.
///
/// A lambda is tagged inlinable if:
/// - Its body is a closed expression tree (every node is a lambda parameter
///   reference, literal, intrinsic, or application of closed sub-expressions);
///   or
/// - It has exactly one parameter and its body is a destructuring let (a
///   `DestructureBlockLet` or `DestructureListLet`). This allows the inline
///   pass to distribute destructuring functions to their call sites so that
///   the subsequent fusion pass can simplify the resulting static lookups.
pub fn tag_combinators(expr: &RcExpr) -> Result<RcExpr, crate::core::error::CoreError> {
    match &*expr.inner {
        Expr::Lam(s, false, scope) => {
            if closed_body(&scope.body) || destructuring(scope) {
                Ok(RcExpr::from(Expr::Lam(*s, true, scope.clone())))
            } else {
                // Recurse into the lambda body even if not itself inlinable
                expr.walk_safe(&mut |e| tag_combinators(&e))
            }
        }
        _ => expr.walk_safe(&mut |e| tag_combinators(&e)),
    }
}

/// A lambda body is a closed expression if every node is one of:
/// - `Var::Bound(scope=0)` — a reference to a parameter of the immediately
///   enclosing lambda
/// - `Literal`
/// - `Intrinsic`
/// - `App(closed_f, [closed_arg]*)` — application of closed sub-expressions
///
/// No free variables, no `Let` bindings, and no nested lambdas are permitted.
/// This generalises the old flat `combinator` check to arbitrary-depth
/// compositions, capturing functions such as:
/// - `abs(n)`   → `__IF(__LT(n, 0), __SUB(0, n), n)`
/// - `max(l,r)` → `__IF(__GT(l, r), l, r)`
/// - `!=(l,r)`  → `__NOT(__EQ(l, r))`
fn closed_body(expr: &RcExpr) -> bool {
    match &*expr.inner {
        Expr::Var(_, Var::Bound(bv)) => bv.scope == 0,
        Expr::Literal(_, _) => true,
        Expr::Intrinsic(_, _) => true,
        Expr::App(_, f, xs) => closed_body(f) && xs.iter().all(closed_body),
        _ => false,
    }
}

/// A lambda is a destructuring lambda if it takes exactly one parameter and
/// its body is a `DestructureBlockLet` or `DestructureListLet`. Such lambdas
/// are safe to inline because destructuring is deterministic and the fusion
/// pass will subsequently simplify the resulting static lookups.
fn destructuring(lam_scope: &LamScope<RcExpr>) -> bool {
    if lam_scope.pattern.len() != 1 {
        return false;
    }
    let body = &lam_scope.body;
    matches!(
        &*body.inner,
        Expr::Let(
            _,
            _,
            LetType::DestructureBlockLet | LetType::DestructureListLet
        )
    )
}

/// Check that every `Var::Free` in `expr` is a member of `set`.
///
/// Returns `true` if the expression's free variables are all known
/// (i.e. will be resolvable when injected alongside the set members).
/// Used by xtask's fixed-point collection to determine which prelude
/// bindings are safe to include as inline cores.
pub fn all_free_vars_in_set(expr: &RcExpr, set: &HashSet<String>) -> bool {
    match &*expr.inner {
        Expr::Var(_, Var::Free(name)) => set.contains(name.as_str()),
        Expr::Var(_, Var::Bound(_)) => true,
        Expr::Literal(_, _) => true,
        Expr::Intrinsic(_, _) => true,
        Expr::App(_, f, xs) => {
            all_free_vars_in_set(f, set) && xs.iter().all(|x| all_free_vars_in_set(x, set))
        }
        Expr::Meta(_, inner, _) => all_free_vars_in_set(inner, set),
        _ => false,
    }
}

#[cfg(test)]
pub mod tests {

    use super::*;
    use crate::core::expr::acore::*;
    use crate::core::expr::*;

    // ── closed_body tests ─────────────────────────────────────────────────────

    #[test]
    pub fn test_closed_body_bound_scope0() {
        // Var::Bound(scope=0) — a direct lambda parameter reference → closed
        let expr = lam(vec![free("x")], var(free("x")));
        // The body `var(x)` where x is Bound(scope=0) should be recognised as closed
        assert!(tag_combinators(&expr).is_ok());
        // Verify it is tagged inlinable
        if let Expr::Lam(_, inl, _) = &*tag_combinators(&expr).unwrap().inner {
            assert!(*inl, "Bound(scope=0) body should be inlinable");
        }
    }

    #[test]
    pub fn test_closed_body_literal() {
        // A literal is a closed body
        let x = free("x");
        let expr = lam(vec![x.clone()], num(42));
        if let Expr::Lam(_, inl, _) = &*tag_combinators(&expr).unwrap().inner {
            assert!(*inl, "Literal body should be inlinable");
        }
        let _ = x;
    }

    #[test]
    pub fn test_closed_body_intrinsic() {
        // A bare intrinsic is a closed body
        let x = free("x");
        let expr = lam(vec![x.clone()], bif("ADD"));
        if let Expr::Lam(_, inl, _) = &*tag_combinators(&expr).unwrap().inner {
            assert!(*inl, "Intrinsic body should be inlinable");
        }
        let _ = x;
    }

    #[test]
    pub fn test_closed_body_app_flat_combinator() {
        // App(Intrinsic, [Bound, Literal]) — flat combinator pattern → closed
        let x = free("x");
        let y = free("y");
        let body = app(bif("ADD"), vec![var(x.clone()), var(y.clone())]);
        let expr = lam(vec![x.clone(), y.clone()], body);
        if let Expr::Lam(_, inl, _) = &*tag_combinators(&expr).unwrap().inner {
            assert!(*inl, "Flat combinator App should be inlinable");
        }
        let _ = (x, y);
    }

    #[test]
    pub fn test_closed_body_app_deep() {
        // App(Intrinsic, [App(Intrinsic, [Bound]), Bound]) — deep composition → closed
        let x = free("x");
        let y = free("y");
        let inner = app(bif("NEG"), vec![var(x.clone())]);
        let body = app(bif("ADD"), vec![inner, var(y.clone())]);
        let expr = lam(vec![x.clone(), y.clone()], body);
        if let Expr::Lam(_, inl, _) = &*tag_combinators(&expr).unwrap().inner {
            assert!(*inl, "Deep combinator App should be inlinable");
        }
        let _ = (x, y);
    }

    #[test]
    pub fn test_closed_body_with_free_var_not_inlinable() {
        // App(Var::Free, [Bound]) — free variable reference → NOT closed
        let x = free("x");
        let f = free("external");
        let body = app(var(f.clone()), vec![var(x.clone())]);
        let expr = lam(vec![x.clone()], body);
        if let Expr::Lam(_, inl, _) = &*tag_combinators(&expr).unwrap().inner {
            assert!(
                !*inl,
                "Free var in body should NOT be inlinable as closed body"
            );
        }
        let _ = (x, f);
    }

    // ── all_free_vars_in_set tests ────────────────────────────────────────────

    #[test]
    pub fn test_all_free_vars_no_free_vars() {
        // Literals and intrinsics have no Free vars → true regardless of set
        use std::collections::HashSet;
        let set: HashSet<String> = HashSet::new();
        assert!(all_free_vars_in_set(&num(1), &set));
        assert!(all_free_vars_in_set(&bif("ADD"), &set));
        // Lam nodes are not traversed by all_free_vars_in_set (falls through
        // to the `_ => false` arm) — this is intentional: the function is
        // used on lambda bodies, not on the Lam wrappers themselves.
        let x = free("x");
        let lam_expr = lam(vec![x.clone()], num(42));
        assert!(
            !all_free_vars_in_set(&lam_expr, &set),
            "Lam node returns false (checked on body separately, not the wrapper)"
        );
        let _ = x;
    }

    #[test]
    pub fn test_all_free_vars_in_set_present() {
        // Free("if") with set containing "if" → true
        use std::collections::HashSet;
        let mut set: HashSet<String> = HashSet::new();
        set.insert("if".to_string());
        let expr = var(free("if"));
        assert!(all_free_vars_in_set(&expr, &set));
    }

    #[test]
    pub fn test_all_free_vars_not_in_set() {
        // Free("map") with set NOT containing "map" → false
        use std::collections::HashSet;
        let set: HashSet<String> = HashSet::new();
        let expr = var(free("map"));
        assert!(!all_free_vars_in_set(&expr, &set));
    }

    #[test]
    pub fn test_all_free_vars_nested_app_mixed() {
        // App(Free("known"), [Free("unknown")]) → false
        use std::collections::HashSet;
        let mut set: HashSet<String> = HashSet::new();
        set.insert("known".to_string());
        let expr = app(var(free("known")), vec![var(free("unknown"))]);
        assert!(!all_free_vars_in_set(&expr, &set));
    }

    #[test]
    pub fn test_all_free_vars_meta_transparent() {
        // Meta wrapping is transparent
        use std::collections::HashSet;
        let mut set: HashSet<String> = HashSet::new();
        set.insert("if".to_string());
        // Wrap a Free("if") in Meta
        use crate::common::sourcemap::Smid;
        use crate::core::expr::Expr;
        let inner = var(free("if"));
        let meta_expr = RcExpr::from(Expr::Meta(Smid::default(), inner, num(42)));
        assert!(all_free_vars_in_set(&meta_expr, &set));
    }

    #[test]
    pub fn test_simple() {
        let f = free("f");
        let y = free("y");
        let z = free("z");

        let original = let_(
            vec![(
                f.clone(),
                lam(
                    vec![y.clone(), z.clone()],
                    app(bif("F"), vec![var(y.clone()), var(z.clone())]),
                ),
            )],
            app(var(f.clone()), vec![num(22), num(23)]),
        );

        let expected = let_(
            vec![(
                f.clone(),
                inline(
                    vec![y.clone(), z.clone()],
                    app(bif("F"), vec![var(y), var(z)]),
                ),
            )],
            app(var(f), vec![num(22), num(23)]),
        );

        assert_eq!(tag_combinators(&original).unwrap(), expected);
    }
}
