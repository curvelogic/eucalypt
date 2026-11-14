//! Verify integrity of core expression before eval / compiles
use crate::common::sourcemap::HasSmid;
use crate::core::binding::{BoundVar, Var};
use crate::core::error::CoreError;
use crate::core::expr::*;

/// Return `true` if `name` looks like a user-written identifier rather than
/// an internal compiler-generated name (e.g. `_e_a0`, `__build`) or a
/// well-known prelude constant (e.g. `null`, `true`, `false`).
///
/// Used to suppress false-positive assignment-style hints for mangled names
/// and prelude names that commonly appear in equality expressions like
/// `(null = null)` or `(true = x)`.
fn is_user_identifier(name: &str) -> bool {
    const PRELUDE_CONSTANTS: &[&str] = &["null", "nil", "true", "false", "identity", "const"];
    name.starts_with(|c: char| c.is_alphabetic()) && !PRELUDE_CONSTANTS.contains(&name)
}

/// Scan the core expression for errors
pub fn verify(expr: RcExpr) -> Result<Vec<CoreError>, CoreError> {
    verify_with_globals(expr, &|_| false)
}

/// Scan the core expression for errors, treating any free variable for
/// which `is_global` returns `true` as resolved.
///
/// With the prelude blob active the prelude is not loaded as source, so a
/// reference to a prelude name (`head`, `map`, ...) is still a
/// `Var::Free` when the verifier runs; it resolves later to a global slot.
/// The caller passes the blob's global names here so that such a name is
/// not mistaken for an undefined, user-written one — exactly as it is not
/// in source-prelude mode, where it is a bound variable (eu-oaz72.134).
pub fn verify_with_globals(
    expr: RcExpr,
    is_global: &dyn Fn(&str) -> bool,
) -> Result<Vec<CoreError>, CoreError> {
    let mut verifier = Verifier {
        errors: vec![],
        is_global,
    };
    verifier.verify(expr)?;
    Ok(verifier.errors)
}

/// Return `true` if `f` is a reference to the `=` operator, whether it is
/// still free (blob path, before the prelude's inlinable cores are
/// injected) or bound to the prelude's own `=` (source-prelude path).
fn is_eq_operator(f: &RcExpr) -> bool {
    match &*strip_meta(f).inner {
        Expr::Var(_, Var::Free(n)) => n == "=",
        Expr::Var(_, Var::Bound(BoundVar { name: Some(n), .. })) => n == "=",
        _ => false,
    }
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

pub struct Verifier<'g> {
    errors: Vec<CoreError>,
    /// Free names that resolve to globals outside the expression (the
    /// prelude blob's bindings), and so are not undefined.
    is_global: &'g dyn Fn(&str) -> bool,
}

impl Verifier<'_> {
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
                for (binder, b) in scope.pattern.iter().enumerate() {
                    if is_trivially_self_referential(&b.expr, binder as u32) {
                        self.errors.push(CoreError::TrivialSelfAssignment(
                            b.expr.smid(),
                            b.name.clone(),
                        ));
                    }
                }
                expr.walk_safe(&mut |x| self.verify(x))
            }
            Expr::App(s, f, args) => {
                // Detect assignment-style declarations: `name = value`.
                //
                // After cook, `name = value` produces `App(=, [Var::Free("name"),
                // ...])`.  The `=` is free on the blob path (the prelude is not
                // loaded) and bound to the prelude's `=` on the source-prelude
                // path; `is_eq_operator` accepts both.  The first argument being
                // an *undefined* identifier is strong evidence of a
                // Python/JS-style declaration mistake.  A prelude name is not
                // undefined: on the source path it is bound, and on the blob
                // path `is_global` recognises it, so the two modes agree
                // (eu-oaz72.134).
                //
                // We only fire when the identifier looks user-written (starts with
                // a letter) to avoid false positives on compiler-generated names
                // like `_e_a0` or `__build`.
                if is_eq_operator(f) {
                    if let Some(first_arg) = args.first() {
                        if let Expr::Var(_, Var::Free(name)) = &*first_arg.inner {
                            if is_user_identifier(name) && !(self.is_global)(name) {
                                self.errors
                                    .push(CoreError::AssignmentStyleDeclaration(*s, name.clone()));
                            }
                        }
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
    use crate::{common::sourcemap::Smid, core::expr::acore::*};

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
        let expr = let_(vec![("x".to_string(), var(free("x")))], block(vec![]));
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
            vec![("f".to_string(), app(var(free("f")), vec![num(1)]))],
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
