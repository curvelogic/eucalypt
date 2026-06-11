//! Deprecation reference checker.
//!
//! Walks a Core expression tree and emits a [`TypeWarning`] for every
//! variable reference whose original name matches a deprecated declaration.
use crate::common::sourcemap::Smid;
use crate::core::binding::Var;
use crate::core::expr::{Expr, RcExpr};
use crate::core::metadata::DeprecationSpec;
use crate::core::typecheck::error::TypeWarning;
use std::collections::HashMap;

/// Walk `expr` and return a warning for every reference to a declaration
/// that appears in `deprecations`.
///
/// `deprecations` maps declaration names (as they appear in source) to their
/// deprecation spec.  The check matches against the `name` field of
/// `Var::Bound`, which the desugarer preserves for exactly this purpose.
pub fn check_deprecated_references(
    expr: &RcExpr,
    deprecations: &HashMap<String, DeprecationSpec>,
) -> Vec<TypeWarning> {
    if deprecations.is_empty() {
        return vec![];
    }
    let mut warnings = Vec::new();
    check_expr(expr, deprecations, &mut warnings);
    warnings
}

fn check_expr(
    expr: &RcExpr,
    deprecations: &HashMap<String, DeprecationSpec>,
    warnings: &mut Vec<TypeWarning>,
) {
    match &*expr.inner {
        Expr::Var(smid, Var::Bound(bv)) => {
            if let Some(name) = &bv.name {
                if let Some(spec) = deprecations.get(name.as_str()) {
                    warnings.push(make_warning(name, spec, *smid));
                }
            }
        }
        Expr::Var(smid, Var::Free(name)) => {
            if let Some(spec) = deprecations.get(name.as_str()) {
                warnings.push(make_warning(name, spec, *smid));
            }
        }
        Expr::Let(_, scope, _) => {
            for (_, v) in &scope.pattern {
                check_expr(v, deprecations, warnings);
            }
            check_expr(&scope.body, deprecations, warnings);
        }
        Expr::Lam(_, _, scope) => {
            check_expr(&scope.body, deprecations, warnings);
        }
        Expr::App(_, f, args) => {
            check_expr(f, deprecations, warnings);
            for a in args {
                check_expr(a, deprecations, warnings);
            }
        }
        Expr::List(_, xs) => {
            for x in xs {
                check_expr(x, deprecations, warnings);
            }
        }
        Expr::Block(_, bm) => {
            for (key, v) in bm.iter() {
                // Skip synthetic re-export entries of the form `{name: name}`.
                // The unit desugarer emits these to make each top-level binding
                // available in the output block; they are not user call sites and
                // would produce spurious deprecation warnings.
                let is_reexport = match &*v.inner {
                    Expr::Var(_, Var::Bound(bv)) => bv.name.as_deref() == Some(key.as_str()),
                    Expr::Var(_, Var::Free(n)) => n == key,
                    _ => false,
                };
                if !is_reexport {
                    check_expr(v, deprecations, warnings);
                }
            }
        }
        Expr::Meta(_, e, m) => {
            check_expr(e, deprecations, warnings);
            check_expr(m, deprecations, warnings);
        }
        Expr::Lookup(_, e, _, fb) => {
            check_expr(e, deprecations, warnings);
            if let Some(fallback) = fb {
                check_expr(fallback, deprecations, warnings);
            }
        }
        Expr::ArgTuple(_, xs) | Expr::Soup(_, xs, _) => {
            for x in xs {
                check_expr(x, deprecations, warnings);
            }
        }
        Expr::Operator(_, _, _, e) => {
            check_expr(e, deprecations, warnings);
        }
        // Literals, intrinsics, names, unit, error markers — no sub-expressions
        _ => {}
    }
}

fn make_warning(name: &str, spec: &DeprecationSpec, smid: Smid) -> TypeWarning {
    let message = match &spec.message {
        Some(msg) => format!("'{name}' is deprecated: {msg}"),
        None => format!("'{name}' is deprecated"),
    };
    let warning = TypeWarning::new(message).at(smid);
    match &spec.replacement {
        Some(r) => warning.with_note(format!("use '{r}' instead")),
        None => warning,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::sourcemap::Smid;
    use crate::core::binding::BoundVar;
    use crate::core::expr::{acore, Expr};

    #[test]
    fn no_warnings_when_no_deprecations() {
        let expr = acore::var("x".to_string());
        let warnings = check_deprecated_references(&expr, &HashMap::new());
        assert!(warnings.is_empty());
    }

    #[test]
    fn warns_on_bound_var_with_deprecated_name() {
        let deprecated_name = "old-fn".to_string();
        let bv = BoundVar {
            scope: 0,
            binder: 0,
            name: Some(deprecated_name.clone()),
        };
        let expr = RcExpr::from(Expr::Var(Smid::default(), Var::Bound(bv)));
        let mut deprecations = HashMap::new();
        deprecations.insert(
            deprecated_name,
            DeprecationSpec {
                message: Some("use new-fn instead".to_string()),
                replacement: Some("new-fn".to_string()),
            },
        );
        let warnings = check_deprecated_references(&expr, &deprecations);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("old-fn"));
        assert!(warnings[0].message.contains("use new-fn instead"));
        assert_eq!(warnings[0].notes, vec!["use 'new-fn' instead"]);
    }

    #[test]
    fn warns_on_free_var_with_deprecated_name() {
        let expr = acore::var("old-fn".to_string());
        let mut deprecations = HashMap::new();
        deprecations.insert(
            "old-fn".to_string(),
            DeprecationSpec {
                message: None,
                replacement: None,
            },
        );
        let warnings = check_deprecated_references(&expr, &deprecations);
        assert_eq!(warnings.len(), 1);
        assert_eq!(warnings[0].message, "'old-fn' is deprecated");
        assert!(warnings[0].notes.is_empty());
    }

    #[test]
    fn no_warning_for_unrelated_var() {
        let expr = acore::var("new-fn".to_string());
        let mut deprecations = HashMap::new();
        deprecations.insert("old-fn".to_string(), DeprecationSpec::default());
        let warnings = check_deprecated_references(&expr, &deprecations);
        assert!(warnings.is_empty());
    }
}
