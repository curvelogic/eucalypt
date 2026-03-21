//! Distribute fixity metadata from definition site to call site.
use crate::common::environment::SimpleEnvironment;
use crate::common::sourcemap::Smid;
use crate::core::binding::Var;
use crate::core::error::CoreError;
use crate::core::expr::*;
use std::collections::HashMap;

pub fn distribute(expr: RcExpr) -> Result<RcExpr, CoreError> {
    Distributor::default().dist(expr)
}

type Binding = (String, RcExpr);
type OpMeta = (Smid, Fixity, Precedence);
type Frame = HashMap<String, OpMeta>;
type Env = SimpleEnvironment<String, OpMeta>;

/// Distribute maintains state as we traverse through the tree
/// accumulating correspondence between names and operator metadata
#[derive(Default)]
pub struct Distributor {
    env: Env,
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
            Expr::Operator(s, f, p, _e) => (_e.clone(), Some((*s, *f, *p))),
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

        for (name, value) in bindings {
            let (expr, op_meta) = Self::expose_definition(value.clone());
            rebound.push((name.clone(), expr));
            if let Some(op_data) = op_meta {
                frame.insert(name.clone(), op_data);
            }
        }

        (rebound, frame)
    }

    /// Distribute fixity and precedence data from definitions to call sites.
    pub fn dist(&mut self, expr: RcExpr) -> Result<RcExpr, CoreError> {
        match &*expr.inner {
            Expr::Let(s, scope, t) => {
                // Open the scope fully (both binding values and body), mirroring
                // the original `scope.clone().unbind()` from the moniker API.
                // This is necessary so that `dist` can find operator free
                // variables inside binding values as well as in the body.
                let (open_bindings, body) = open_let_scope_full(scope);

                let (mut new_bindings, ops) = Self::process_bindings(&open_bindings);

                self.env.push(ops);

                for (_, value) in &mut new_bindings {
                    *value = self.dist(value.clone())?;
                }

                // Re-close both the binding values and the body with
                // `close_let_scope` (equivalent to moniker's `Scope::new`).
                let ret = RcExpr::from(Expr::Let(
                    *s,
                    close_let_scope(new_bindings, self.dist(body)?),
                    *t,
                ));

                self.env.pop();

                Ok(ret)
            }
            Expr::Var(call_smid, Var::Free(name)) => {
                if let Some((def_smid, fixity, precedence)) = self.env.get(name) {
                    // Prefer the call-site Smid (from the user's source) over the
                    // definition-site Smid (from the prelude) so that infix operator
                    // applications carry a source location pointing at the actual
                    // usage, not the operator's definition.
                    let smid = if call_smid.is_valid() {
                        *call_smid
                    } else {
                        *def_smid
                    };
                    Ok(RcExpr::from(Expr::Operator(
                        smid,
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

    #[test]
    pub fn test_sample_1() {
        let plus = free("+");
        let minus = free("-");

        let expr = let_(
            vec![
                (plus.clone(), infixl(50, bif("FOO"))),
                (minus.clone(), infixl(50, bif("BAR"))),
            ],
            soup(vec![num(1), var(plus.clone()), num(2)]),
        );

        let expected = let_(
            vec![(plus.clone(), bif("FOO")), (minus, bif("BAR"))],
            soup(vec![num(1), infixl(50, var(plus)), num(2)]),
        );

        assert_eq!(distribute(expr).unwrap(), expected);
    }

    #[test]
    pub fn test_sample_2() {
        let plus = free("+");
        let minus = free("-");

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
                vec![(minus, bif("BAR"))],
                soup(vec![num(1), infixl(50, var(plus)), num(2)]),
            ),
        );

        assert_eq!(distribute(expr).unwrap(), expected);
    }

    #[test]
    pub fn test_sample_3() {
        let plus = free("+");
        let minus = free("-");

        let expr = let_(
            vec![
                (plus.clone(), infixl(50, bif("FOO"))),
                (minus.clone(), infixr(90, bif("BAR"))),
            ],
            soup(vec![num(1), var(minus.clone()), num(2)]),
        );

        let expected = let_(
            vec![(plus, bif("FOO")), (minus.clone(), bif("BAR"))],
            soup(vec![num(1), infixr(90, var(minus)), num(2)]),
        );

        assert_eq!(distribute(expr).unwrap(), expected);
    }
}
