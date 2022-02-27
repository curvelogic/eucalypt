//! Tag selected lambdas as inlinable for inline pass
use crate::core::error::CoreError;
use crate::core::expr::{Expr, RcExpr};
use moniker::*;

/// Walk the expression tagging combinators.
///
/// Lambdas are combinator if they are simple types of combinators.
pub fn tag_combinators(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    match &*expr.inner {
        Expr::Lam(s, false, scope) => {
            if combinator(scope) {
                Ok(RcExpr::from(Expr::Lam(*s, true, scope.clone())))
            } else {
                Ok(expr.clone())
            }
        }
        _ => expr.walk_safe(&mut |e| tag_combinators(&e)),
    }
}

fn combinator(lam_scope: &Scope<Vec<Binder<String>>, RcExpr>) -> bool {
    let body = &lam_scope.unsafe_body;

    match &*body.inner {
        Expr::Var(_, _) => true,
        Expr::App(_, f, xs) => {
            let transposition = xs
                .iter()
                .all(|e| matches!(&*e.inner, Expr::Var(_, _) | Expr::Literal(_, _)));
            let simple = matches!(&*f.inner, Expr::Intrinsic(_, _) | Expr::Var(_, _));
            transposition && simple
        }
        _ => false,
    }
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

        assert_term_eq!(tag_combinators(&original).unwrap(), expected);
    }
}
