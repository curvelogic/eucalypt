//! Turn an expression into a genearalised lookup expression
use crate::common::sourcemap::{HasSmid, Smid};
use crate::core::error::CoreError;
use crate::core::expr::*;
use crate::core::transform::succ;
use crate::syntax::rowan::lex as lexer;
use moniker::*;

/// Transform an expression into a dynamic generalised lookup
///
/// All free variables become (fallible) lookups against an implicit
/// parameter with fallbacks to possibly in scope vars (which will be
/// pruned later) if they are not in scope
pub fn dynamise(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    Dynamiser::new(expr.smid()).dynamise(expr.smid(), expr)
}

pub struct Dynamiser {
    depth: ScopeOffset,
    param: FreeVar<String>,
    wrap_lambda: bool,
}

impl Dynamiser {
    fn new(smid: Smid) -> Self {
        Dynamiser {
            depth: ScopeOffset(0),
            param: free(format!("__g{}", smid).as_ref()),
            wrap_lambda: false,
        }
    }
}

fn is_normal(n: &str) -> bool {
    let mut i = n.chars();

    i.next().is_some_and(lexer::is_normal_start) && i.all(lexer::is_normal_continuation)
}

impl Dynamiser {
    /// Enter a let or lambda scope increment depth
    fn enter(&mut self) {
        self.depth = self.depth.succ();
    }

    /// Decrement depth
    fn exit(&mut self) {
        self.depth = self.depth.pred().unwrap();
    }

    /// Provide a possibly lookup-wrapped variation of the variable
    /// suitable for use in a lambda.
    fn wrap(&mut self, s: &Smid, var: &Var<String>) -> RcExpr {
        match var {
            Var::Free(fv) => {
                // ignore operator names
                if is_normal(fv.pretty_name.as_ref().unwrap()) {
                    self.wrap_lambda = true;
                    RcExpr::from(Expr::Lookup(
                        *s,
                        core::var(*s, self.param.clone()),
                        fv.pretty_name.clone().unwrap(),
                        Some(core::var(*s, fv.clone())),
                    ))
                } else {
                    core::var(*s, fv.clone())
                }
            }
            Var::Bound(bv) => {
                // if the binder is outside the scope of the dynamic
                // expression then we insert a lookup
                // TODO: ignore operators!
                if bv.scope >= self.depth {
                    self.wrap_lambda = true;
                    RcExpr::from(Expr::Lookup(
                        *s,
                        core::var(*s, self.param.clone()),
                        bv.pretty_name.clone().unwrap(),
                        Some(RcExpr::from(Expr::Var(*s, Var::Bound(bv.clone())))),
                    ))
                } else {
                    RcExpr::from(Expr::Var(*s, Var::Bound(bv.clone())))
                }
            }
        }
    }

    pub fn dynamise(&mut self, smid: Smid, expr: &RcExpr) -> Result<RcExpr, CoreError> {
        let expr = self.process(expr)?;

        if self.wrap_lambda {
            Ok(RcExpr::from(Expr::Lam(
                smid,
                false, // might be arbitrarily complex - not inlinable
                Scope::new(vec![Binder(self.param.clone())], succ::succ(&expr)?),
            )))
        } else {
            Ok(expr)
        }
    }

    pub fn process(&mut self, expr: &RcExpr) -> Result<RcExpr, CoreError> {
        match &*expr.inner {
            Expr::Let(_, _, _) => {
                self.enter();
                let ret = expr.walk_safe(&mut |e| self.process(&e));
                self.exit();
                ret
            }
            Expr::Lam(_, _, _) => {
                self.enter();
                let ret = expr.walk_safe(&mut |e| self.process(&e));
                self.exit();
                ret
            }
            Expr::Var(s, v) => Ok(self.wrap(s, v)),
            _ => expr.walk_safe(&mut |e| self.process(&e)),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::core::expr::acore::*;

    #[test]
    pub fn test_simple_dynamise() {
        let x = free("x");
        let implicit = free("implicit");

        let original = var(x.clone());
        let expected = lam(
            vec![implicit.clone()],
            lookup(var(implicit), "x", Some(var(x))),
        );

        assert_term_eq!(dynamise(&original).unwrap(), expected);
    }

    #[test]
    pub fn test_leaves_internal_bindings_intact() {
        let x = free("x");
        let y = free("y");
        let z = free("z");
        let implicit = free("implicit");

        let original = let_(
            vec![(x.clone(), num(22)), (y, num(23))],
            let_(vec![(z.clone(), num(24))], var(x)),
        );

        assert_term_eq!(dynamise(&original).unwrap(), original);

        if let Expr::Let(_, scope, _) = &*original.inner {
            let sublet = scope.unsafe_body.clone();
            let expected = lam(
                vec![implicit.clone()],
                let_(
                    vec![(z, num(24))],
                    lookup(
                        var(implicit),
                        "x",
                        Some(RcExpr::from(Expr::Var(
                            Smid::default(),
                            Var::Bound(BoundVar {
                                scope: ScopeOffset(2),
                                binder: BinderIndex(0),
                                pretty_name: Some("x".to_string()),
                            }),
                        ))),
                    ),
                ),
            );
            assert_term_eq!(dynamise(&sublet).unwrap(), expected);
        }
    }
}
