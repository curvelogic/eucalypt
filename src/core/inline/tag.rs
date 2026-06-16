//! Tag selected lambdas as inlinable for inline pass
use crate::core::{
    binding::Var,
    expr::{Expr, LamScope, LetType, RcExpr},
};

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

#[cfg(test)]
pub mod tests {

    use super::*;
    use crate::core::expr::acore::*;
    use crate::core::expr::*;

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
