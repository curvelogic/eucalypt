//! Verify integrity of core expression before eval / compiles
use crate::common::sourcemap::HasSmid;
use crate::core::binding::{BoundVar, Var};
use crate::core::error::CoreError;
use crate::core::expr::*;

/// Scan the core expression for errors
pub fn verify(expr: RcExpr) -> Result<Vec<CoreError>, CoreError> {
    let mut verifier = Verifier::default();
    verifier.verify(expr)?;
    Ok(verifier.errors)
}

/// Strip any `Meta` wrappers from an expression to reach its inner form.
fn strip_meta(expr: &RcExpr) -> &RcExpr {
    let mut e = expr;
    loop {
        match &*e.inner {
            Expr::Meta(_, inner, _) => e = inner,
            _ => return e,
        }
    }
}

/// Return `true` if `expr` (after stripping `Meta` wrappers) is exactly
/// `BoundVar { scope: 0, binder }`.
fn is_self_bv(expr: &RcExpr, binder: u32) -> bool {
    matches!(
        &*strip_meta(expr).inner,
        Expr::Var(
            _,
            Var::Bound(BoundVar {
                scope: 0,
                binder: b,
                ..
            }),
        ) if *b == binder
    )
}

/// Return `true` if `value` is an always-divergent self-reference for binder
/// `binder` at scope 0.  We flag:
///
/// - **direct**: the value IS `BoundVar(scope=0, binder)`
/// - **function-head position**: the value is `App(head, _)` and `head`
///   (after stripping `Meta`) IS `BoundVar(scope=0, binder)`
///
/// Argument-position self-reference (`cons(1, ones)`) is NOT flagged.
fn is_trivially_self_referential(value: &RcExpr, binder: u32) -> bool {
    if is_self_bv(value, binder) {
        return true;
    }
    if let Expr::App(_, head, _) = &*strip_meta(value).inner {
        if is_self_bv(head, binder) {
            return true;
        }
    }
    false
}

#[derive(Default)]
pub struct Verifier {
    errors: Vec<CoreError>,
}

impl Verifier {
    fn verify(&mut self, expr: RcExpr) -> Result<RcExpr, CoreError> {
        match &*expr.inner {
            Expr::ErrPseudoCall | Expr::ErrPseudoCat | Expr::ErrPseudoDot => {
                self.errors.push(CoreError::UneliminatedPseudoOperators);
                Ok(expr)
            }
            Expr::ErrEliminated => {
                // self.errors.push(CoreError::UnprunedEliminations);
                Ok(expr)
            }
            Expr::ErrUnresolved(s, n) => {
                self.errors
                    .push(CoreError::UnresolvedVariable(*s, n.clone()));
                Ok(expr)
            }
            Expr::ErrRedeclaration(s, n) => {
                self.errors
                    .push(CoreError::RedeclaredVariable(*s, n.clone()));
                Ok(expr)
            }
            Expr::Soup(s, _, _) => {
                self.errors.push(CoreError::UneliminatedSoup(*s));
                Ok(expr)
            }
            Expr::Let(_, scope, _) => {
                // Check each binding value for trivial self-assignment before
                // walking into the scope body.
                for (binder, (name, value)) in scope.pattern.iter().enumerate() {
                    if is_trivially_self_referential(value, binder as u32) {
                        self.errors.push(CoreError::TrivialSelfAssignment(
                            value.smid(),
                            name.clone(),
                        ));
                    }
                }
                expr.walk_safe(&mut |x| self.verify(x))
            }
            _ => expr.walk_safe(&mut |x| self.verify(x)),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::{
        common::sourcemap::Smid,
        core::expr::acore::*,
    };

    #[test]
    pub fn test_verify_pseudo_ops() {
        assert_eq!(
            verify(soup(vec![var(free("x")), dot(), name("a")])).unwrap(),
            vec![CoreError::UneliminatedSoup(Smid::default())]
        );
        assert_eq!(
            verify(app(
                var(free("f")),
                vec![app(cat(), vec![var(free("x")), var(free("g"))])]
            ))
            .unwrap(),
            vec![CoreError::UneliminatedPseudoOperators]
        );
        assert_eq!(
            verify(app(
                var(free("f")),
                vec![app(
                    call(),
                    vec![var(free("f")), arg_tuple(vec![var(free("g"))])]
                )]
            ))
            .unwrap(),
            vec![CoreError::UneliminatedPseudoOperators]
        );
    }

    #[test]
    pub fn test_verify_self_assignment_direct() {
        // Simulates desugaring `{ x: x }`: after close_let_scope, the FreeVar("x")
        // in the binding value becomes BoundVar(scope=0, binder=0).
        let expr = let_(
            vec![("x".to_string(), var(free("x")))],
            block(vec![]),
        );
        let errors = verify(expr).unwrap();
        assert_eq!(errors.len(), 1, "expected 1 error, got {errors:?}");
        assert!(
            matches!(&errors[0], CoreError::TrivialSelfAssignment(_, n) if n == "x"),
            "expected TrivialSelfAssignment for x, got {errors:?}"
        );
    }

    #[test]
    pub fn test_verify_self_assignment_function_position() {
        // Simulates desugaring `{ f: f(1) }`: after close_let_scope the head
        // FreeVar("f") becomes BoundVar(scope=0, binder=0).
        let expr = let_(
            vec![(
                "f".to_string(),
                app(var(free("f")), vec![num(1)]),
            )],
            block(vec![]),
        );
        let errors = verify(expr).unwrap();
        assert_eq!(errors.len(), 1, "expected 1 error, got {errors:?}");
        assert!(
            matches!(&errors[0], CoreError::TrivialSelfAssignment(_, n) if n == "f"),
            "expected TrivialSelfAssignment for f, got {errors:?}"
        );
    }

    #[test]
    pub fn test_verify_self_assignment_meta_wrapped() {
        // A Meta wrapper around a self-referential value must still be detected.
        // After close_let_scope the FreeVar("x") → BoundVar(scope=0, binder=0),
        // wrapped in Meta(_, BoundVar, _).
        let expr = let_(
            vec![("x".to_string(), meta(var(free("x")), sym("doc")))],
            block(vec![]),
        );
        let errors = verify(expr).unwrap();
        assert_eq!(errors.len(), 1, "expected 1 error, got {errors:?}");
        assert!(
            matches!(&errors[0], CoreError::TrivialSelfAssignment(_, n) if n == "x"),
            "expected TrivialSelfAssignment for meta-wrapped x, got {errors:?}"
        );
    }

    #[test]
    pub fn test_verify_no_flag_argument_position() {
        // Simulates desugaring `{ ones: cons(1, ones) }`: self-reference is in
        // argument position → must NOT be flagged (legitimate lazy fixpoint).
        let expr = let_(
            vec![(
                "ones".to_string(),
                app(var(free("cons")), vec![num(1), var(free("ones"))]),
            )],
            block(vec![]),
        );
        let errors = verify(expr).unwrap();
        assert!(
            errors
                .iter()
                .all(|e| !matches!(e, CoreError::TrivialSelfAssignment(..))),
            "argument-position self-reference must NOT be flagged, got {errors:?}"
        );
    }
}
