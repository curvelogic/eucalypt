//! Check we haven't disturbed de bruijn indexes during processing
use crate::core::error::CoreError;
use crate::core::expr::*;
use moniker::*;
use std::collections::VecDeque;

pub fn verify(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    ScopeTracker::default().verify(expr)
}

#[derive(Debug, Default)]
pub struct ScopeTracker {
    scopes: VecDeque<RcExpr>,
}

impl ScopeTracker {
    fn enter(&mut self, expr: &RcExpr) {
        self.scopes.push_front(expr.clone());
    }

    fn exit(&mut self) {
        self.scopes.pop_front();
    }

    fn encounter(&mut self, bound_var: &BoundVar<String>) {
        let expr = self.scopes[bound_var.scope.0 as usize].clone();
        match &*expr.inner {
            Expr::Let(_, scope, _) => {
                if let Some((Binder(fv), Embed(_))) = &scope
                    .unsafe_pattern
                    .unsafe_pattern
                    .get(bound_var.binder.to_usize())
                {
                    assert_eq!(fv.pretty_name, bound_var.pretty_name);
                } else {
                    panic!("missing binding");
                }
            }
            Expr::Lam(_, _, scope) => {
                if let Some(Binder(fv)) = &scope.unsafe_pattern.get(bound_var.binder.to_usize()) {
                    assert_eq!(fv.pretty_name, bound_var.pretty_name);
                } else {
                    panic!("missing lambda binding")
                }
            }
            _ => {}
        }
    }

    pub fn verify(&mut self, expr: &RcExpr) -> Result<RcExpr, CoreError> {
        match &*expr.inner {
            Expr::Let(_, _, _) => {
                self.enter(&expr);
                let ret = expr.walk_safe(&mut |e| self.verify(&e));
                self.exit();
                ret
            }
            Expr::Lam(_, _, _) => {
                self.enter(&expr);
                let ret = expr.walk_safe(&mut |e| self.verify(&e));
                self.exit();
                ret
            }
            Expr::Var(_, Var::Bound(bv)) => {
                self.encounter(&bv);
                Ok(expr.clone())
            }
            _ => expr.walk_safe(&mut |e| self.verify(&e)),
        }
    }
}
