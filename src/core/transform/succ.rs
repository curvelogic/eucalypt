//! Compress a pruned tree to remove all eliminated bindings
use crate::core::binding::{BoundVar, Var};
use crate::core::error::CoreError;
use crate::core::expr::*;

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
fn encounter_succ(bound_var: &BoundVar) -> BoundVar {
    BoundVar {
        scope: bound_var.scope + 1,
        binder: bound_var.binder,
        name: bound_var.name.clone(),
    }
}

/// Update a bound var according to decrease the scope offset
fn encounter_pred(bound_var: &BoundVar) -> BoundVar {
    BoundVar {
        scope: bound_var.scope - 1,
        binder: bound_var.binder,
        name: bound_var.name.clone(),
    }
}

#[derive(Default)]
pub struct BoundVarNudger {
    depth: u32,
}

impl BoundVarNudger {
    /// Enter a let or lambda scope increment depth
    fn enter(&mut self) {
        self.depth += 1;
    }

    /// Decrement depth
    fn exit(&mut self) {
        self.depth -= 1;
    }

    pub fn process(
        &mut self,
        expr: &RcExpr,
        handle: fn(&BoundVar) -> BoundVar,
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
