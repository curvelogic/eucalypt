//! Distribute fixity metadata from definition site to call site.
use crate::common::environment::SimpleEnvironment;
use crate::common::sourcemap::Smid;
use crate::core::error::CoreError;
use crate::core::expr::*;
use moniker::FreeVar;
use moniker::Rec;
use moniker::Scope;
use moniker::Var::Free;
use moniker::{Binder, Embed};
use std::collections::HashMap;

pub fn distribute(expr: RcExpr) -> Result<RcExpr, CoreError> {
    Distributor::default().dist(expr)
}

type Binding = (Binder<String>, Embed<RcExpr>);
type OpMeta = (Smid, Fixity, Precedence);
type Frame = HashMap<FreeVar<String>, OpMeta>;
type Env = SimpleEnvironment<FreeVar<String>, OpMeta>;

/// Distribute maintains state as we traverse through the tree
/// accumulating correspondence between names and operator metadata
pub struct Distributor {
    env: Env,
}

impl Default for Distributor {
    fn default() -> Self {
        Distributor {
            env: Env::default(),
        }
    }
}

impl Distributor {
    /// If an expression defines an operator, strip off (and return)
    /// the operator metadata and expose the operator callable.
    fn expose_definition(expr: RcExpr) -> (RcExpr, Option<OpMeta>) {
        match &*expr.inner {
            Expr::Meta(_, e, _) => {
                let (e, m) = Distributor::expose_definition(e.clone());
                if m.is_some() {
                    (e, m)
                } else {
                    (expr, None)
                }
            }
            Expr::Operator(s, f, p, e) => (e.clone(), Some((*s, *f, *p))),
            _ => (expr, None),
        }
    }

    /// Strip the operator metadata from bindings and construct an
    /// environment frame to record them.
    ///
    /// This is shallow and does not recurse into bindings by itself.
    fn process_bindings(bindings: &[Binding]) -> (Vec<Binding>, Frame) {
        let mut frame = Frame::new();
        let mut rebound = Vec::new();

        for (Binder(fv), Embed(value)) in bindings {
            let (expr, op_meta) = Self::expose_definition(value.clone());
            rebound.push((Binder(fv.clone()), Embed(expr.clone())));
            if let Some(op_data) = op_meta {
                frame.insert(fv.clone(), op_data);
            }
        }

        (rebound, frame)
    }

    /// Distribute fixity and precedence data from definitions to call sites.
    pub fn dist(&mut self, expr: RcExpr) -> Result<RcExpr, CoreError> {
        match &*expr.inner {
            Expr::Let(s, scope, t) => {
                // - Clone...
                let (rec_bindings, body) = scope.clone().unbind();

                let (mut new_bindings, ops) = Self::process_bindings(&rec_bindings.unrec());

                self.env.push(ops);

                for (_, Embed(value)) in &mut new_bindings {
                    *value = self.dist(value.clone())?;
                }

                let ret = RcExpr::from(Expr::Let(
                    *s,
                    Scope::new(Rec::new(new_bindings), self.dist(body)?),
                    *t,
                ));

                self.env.pop();

                Ok(ret)
            }
            Expr::Var(_, Free(fv)) => {
                if let Some((smid, fixity, precedence)) = self.env.get(fv) {
                    Ok(RcExpr::from(Expr::Operator(
                        *smid,
                        *fixity,
                        *precedence,
                        expr,
                    )))
                } else {
                    Ok(expr)
                }
            }
            _ => expr.walk_safe(&mut |x| self.dist(x)),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::acore::*;
    use super::*;
    use moniker::assert_term_eq;
    use moniker::FreeVar;

    #[test]
    pub fn test_sample_1() {
        let plus = FreeVar::fresh_named("+");
        let minus = FreeVar::fresh_named("-");

        let expr = let_(
            vec![
                (plus.clone(), infixl(50, bif("FOO"))),
                (minus.clone(), infixl(50, bif("BAR"))),
            ],
            soup(vec![num(1), var(plus.clone()), num(2)]),
        );

        let expected = let_(
            vec![(plus.clone(), bif("FOO")), (minus.clone(), bif("BAR"))],
            soup(vec![num(1), infixl(50, var(plus.clone())), num(2)]),
        );

        assert_term_eq!(distribute(expr).unwrap(), expected);
    }

    #[test]
    pub fn test_sample_2() {
        let plus = FreeVar::fresh_named("+");
        let minus = FreeVar::fresh_named("-");

        let expr = let_(
            vec![(plus.clone(), infixl(50, bif("FOO")))],
            let_(
                vec![(minus.clone(), infixl(50, bif("BAR")))],
                soup(vec![num(1), var(plus.clone()), num(2)]),
            ),
        );

        let expected = let_(
            vec![(plus.clone(), bif("FOO"))],
            let_(
                vec![(minus.clone(), bif("BAR"))],
                soup(vec![num(1), infixl(50, var(plus.clone())), num(2)]),
            ),
        );

        assert_term_eq!(distribute(expr).unwrap(), expected);
    }

    #[test]
    pub fn test_sample_3() {
        let plus = FreeVar::fresh_named("+");
        let minus = FreeVar::fresh_named("-");

        let expr = let_(
            vec![
                (plus.clone(), infixl(50, bif("FOO"))),
                (minus.clone(), infixr(90, bif("BAR"))),
            ],
            soup(vec![num(1), var(minus.clone()), num(2)]),
        );

        let expected = let_(
            vec![(plus.clone(), bif("FOO")), (minus.clone(), bif("BAR"))],
            soup(vec![num(1), infixr(90, var(minus.clone())), num(2)]),
        );

        assert_term_eq!(distribute(expr).unwrap(), expected);
    }
}
