//! Check we haven't disturbed de bruijn indexes during processing
use crate::core::binding::{BoundVar, Var};
use crate::core::error::CoreError;
use crate::core::expr::*;
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

    fn encounter(&mut self, bound_var: &BoundVar) {
        if bound_var.scope as usize >= self.scopes.len() {
            panic!(
                "VERIFIER: scope OOB: scope={} binder={} name={:?} scopes.len()={}",
                bound_var.scope,
                bound_var.binder,
                bound_var.name,
                self.scopes.len()
            );
        }
        let expr = self.scopes[bound_var.scope as usize].clone();
        match &*expr.inner {
            Expr::Let(_, scope, _) => {
                if let Some((name, _)) = scope.pattern.get(bound_var.binder as usize) {
                    assert_eq!(
                        Some(name.as_str()),
                        bound_var.name.as_deref(),
                        "name mismatch for BV scope={} binder={}",
                        bound_var.scope,
                        bound_var.binder
                    );
                } else {
                    panic!("VERIFIER: missing Let binding: scope={} binder={} name={:?} pattern.len()={}", bound_var.scope, bound_var.binder, bound_var.name, scope.pattern.len());
                }
            }
            Expr::Lam(_, _, scope) => {
                if let Some(name) = scope.pattern.get(bound_var.binder as usize) {
                    assert_eq!(Some(name.as_str()), bound_var.name.as_deref());
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
                self.enter(expr);
                let ret = expr.walk_safe(&mut |e| self.verify(&e));
                self.exit();
                ret
            }
            Expr::Lam(_, _, _) => {
                self.enter(expr);
                let ret = expr.walk_safe(&mut |e| self.verify(&e));
                self.exit();
                ret
            }
            Expr::Var(_, Var::Bound(bv)) => {
                self.encounter(bv);
                Ok(expr.clone())
            }
            _ => expr.walk_safe(&mut |e| self.verify(&e)),
        }
    }
}
