//! Compress a pruned tree to remove all eliminated bindings
use crate::core::binding::{BoundVar, Scope, Var};
use crate::core::error::CoreError;
use crate::core::expr::*;
use crate::core::transform::succ;
use std::collections::VecDeque;

pub fn compress(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    ScopeCompressor::default().compress(expr)
}

pub type Permutation = Vec<Option<usize>>;

#[derive(Default)]
pub struct ScopeCompressor {
    /// How to permute bindings for nested scopes
    perms: VecDeque<Permutation>,
}

impl ScopeCompressor {
    /// Enter a let or lambda scope and store a permutation of binder indexes
    fn enter(&mut self, perm: Permutation) {
        self.perms.push_front(perm);
    }

    /// Pop a permutation
    fn exit(&mut self) {
        self.perms.pop_front();
    }

    /// Update a bound var according to the relevant permutation
    fn encounter(&mut self, bound_var: &BoundVar) -> Option<BoundVar> {
        let perm = &self.perms[bound_var.scope as usize];
        let reindex = perm.get(bound_var.binder as usize).cloned().flatten();
        reindex.map(|i| BoundVar {
            scope: bound_var.scope,
            binder: i as u32,
            name: bound_var.name.clone(),
        })
    }

    pub fn compress(&mut self, expr: &RcExpr) -> Result<RcExpr, CoreError> {
        match &*expr.inner {
            Expr::Let(s, scope, t) => {
                self.enter(permutation_from_let_scope(scope));

                let new_bindings: Result<Vec<_>, CoreError> = scope
                    .pattern
                    .iter()
                    .filter(|(_, value)| !matches!(*value.inner, Expr::ErrEliminated))
                    .map(|(n, value)| self.compress(value).map(|val| (n.clone(), val)))
                    .collect();

                let new_bindings = match new_bindings {
                    Ok(b) => b,
                    Err(e) => {
                        self.exit();
                        return Err(e);
                    }
                };

                let new_body = self.compress(&scope.body)?;

                let ret = if !new_bindings.is_empty() {
                    RcExpr::from(Expr::Let(
                        *s,
                        Scope {
                            pattern: new_bindings,
                            body: new_body,
                        },
                        *t,
                    ))
                } else {
                    // optimise away the let entirely
                    succ::pred(&new_body)?
                };

                self.exit();
                Ok(ret)
            }
            Expr::Lam(s, inl, scope) => {
                self.enter(permutation_from_lam_scope(scope));
                let ret = RcExpr::from(Expr::Lam(
                    *s,
                    *inl,
                    Scope {
                        pattern: scope.pattern.clone(),
                        body: self.compress(&scope.body)?,
                    },
                ));
                self.exit();
                Ok(ret)
            }
            Expr::Var(s, v) => {
                if let Var::Bound(bound_var) = v {
                    let bv = self.encounter(bound_var).expect("eliminating used var");
                    Ok(RcExpr::from(Expr::Var(*s, Var::Bound(bv))))
                } else {
                    Ok(expr.clone())
                }
            }
            _ => expr.walk_safe(&mut |e| self.compress(&e)),
        }
    }
}

#[allow(clippy::type_complexity)]
fn permutation_from_let_scope(scope: &LetScope<RcExpr>) -> Permutation {
    let mut perm = Vec::new();
    let mut i = 0;
    for (_, value) in &scope.pattern {
        if matches!(*value.inner, Expr::ErrEliminated) {
            perm.push(None);
        } else {
            perm.push(Some(i));
            i += 1;
        }
    }

    perm
}

fn permutation_from_lam_scope(scope: &LamScope<RcExpr>) -> Permutation {
    let mut perm = Vec::new();
    for (i, _) in scope.pattern.iter().enumerate() {
        perm.push(Some(i));
    }

    perm
}

#[cfg(test)]
pub mod tests {

    use super::*;
    use crate::core::expr::acore::*;

    #[test]
    pub fn test_simple() {
        let a = free("a");
        let b = free("b");
        let d = free("d");
        let e = free("e");

        let sample = let_(
            vec![
                (
                    a.clone(),
                    let_(
                        vec![(d, RcExpr::from(Expr::ErrEliminated)), (e.clone(), num(2))],
                        var(e.clone()),
                    ),
                ),
                (b, RcExpr::from(Expr::ErrEliminated)),
            ],
            var(a.clone()),
        );

        let expected = let_(
            vec![(a.clone(), let_(vec![(e.clone(), num(2))], var(e)))],
            var(a),
        );

        assert_eq!(compress(&sample).unwrap(), expected);
    }
}
