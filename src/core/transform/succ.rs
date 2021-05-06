//! Compress a pruned tree to remove all eliminated bindings
use crate::core::error::CoreError;
use crate::core::expr::*;
use moniker::{BoundVar, ScopeOffset, Var};

/// Occasionally we need to push an expression with bound vars
/// inside an extra scope, this increments de bruijn indices to
/// compensate.
pub fn succ(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    BoundVarNudger::default().process(expr, encounter_succ)
}

pub fn pred(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    BoundVarNudger::default().process(expr, encounter_pred)
}

/// Update a bound var according to increase the scope offset
fn encounter_succ(bound_var: &BoundVar<String>) -> BoundVar<String> {
    BoundVar {
        scope: bound_var.scope.succ(),
        binder: bound_var.binder,
        pretty_name: bound_var.pretty_name.clone(),
    }
}

/// Update a bound var according to decrease the scope offset
fn encounter_pred(bound_var: &BoundVar<String>) -> BoundVar<String> {
    BoundVar {
        scope: bound_var.scope.pred().unwrap(),
        binder: bound_var.binder,
        pretty_name: bound_var.pretty_name.clone(),
    }
}

pub struct BoundVarNudger {
    depth: ScopeOffset,
}

impl Default for BoundVarNudger {
    fn default() -> Self {
        BoundVarNudger {
            depth: ScopeOffset(0),
        }
    }
}

impl BoundVarNudger {
    /// Enter a let or lambda scope increment depth
    fn enter(&mut self) {
        self.depth = self.depth.succ();
    }

    /// Decrement depth
    fn exit(&mut self) {
        self.depth = self.depth.pred().unwrap();
    }

    pub fn process(
        &mut self,
        expr: &RcExpr,
        handle: fn(&BoundVar<String>) -> BoundVar<String>,
    ) -> Result<RcExpr, CoreError> {
        match &*expr.inner {
            Expr::Let(_, _, _) => {
                self.enter();
                let ret = expr.walk_safe(&mut |e| self.process(&e, handle));
                self.exit();
                ret
            }
            Expr::Lam(_, _, _) => {
                self.enter();
                let ret = expr.walk_safe(&mut |e| self.process(&e, handle));
                self.exit();
                ret
            }
            Expr::Var(s, v) => {
                if let Var::Bound(bound_var) = v {
                    if bound_var.scope >= self.depth {
                        Ok(RcExpr::from(Expr::Var(*s, Var::Bound(handle(bound_var)))))
                    } else {
                        Ok(expr.clone())
                    }
                } else {
                    Ok(expr.clone())
                }
            }
            _ => expr.walk_safe(&mut |e| self.process(&e, handle)),
        }
    }
}
