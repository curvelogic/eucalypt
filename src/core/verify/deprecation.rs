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
            for b in &scope.pattern {
                check_expr(&b.expr, deprecations, warnings);
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
        Expr::Lookup(smid, e, key, fb) => {
            // Namespace-member deprecation: `state.exec` where the `exec`
            // member of the `state` namespace carries deprecation metadata,
            // recorded under the dotted key `state.exec`.  Only a lookup whose
            // base is a chain of directly-named variables qualifies, so an
            // unrelated block's `.exec` member (or the non-deprecated
            // `io.exec`) never false-fires.
            if let Some(base) = dotted_path(e) {
                let qualified = format!("{base}.{key}");
                if let Some(spec) = deprecations.get(qualified.as_str()) {
                    warnings.push(make_warning(&qualified, spec, *smid));
                }
            }
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

/// The dotted source path of a lookup base, if it is a variable or a chain of
/// lookups over one — used to qualify a namespace-member lookup (`state.exec`,
/// or `a.b.c` for a declaration nested two blocks deep) for matching.
///
/// Matching is **name-based, not resolution-based**: this is the path written
/// at the source site, not the binding it resolves to.  A user block that
/// shadows a deprecated namespace — their own `state` with an `exec` member —
/// therefore matches the library's key and warns with its message.  Doing
/// better needs the resolved binder identity of the base, which Core does not
/// carry through to this pass.
fn dotted_path(expr: &RcExpr) -> Option<String> {
    match &*expr.inner {
        Expr::Var(_, Var::Bound(bv)) => bv.name.clone(),
        Expr::Var(_, Var::Free(name)) => Some(name.clone()),
        Expr::Lookup(_, base, key, _) => dotted_path(base).map(|path| format!("{path}.{key}")),
        _ => None,
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

    #[test]
    fn warns_on_deprecated_namespace_member_lookup() {
        // `state.exec` — a lookup whose base is a directly-named variable
        // and whose dotted path is a deprecated key.
        let expr = acore::lookup(acore::var("state".to_string()), "exec", None);
        let mut deprecations = HashMap::new();
        deprecations.insert(
            "state.exec".to_string(),
            DeprecationSpec {
                message: Some("use state.run(a, i).state".to_string()),
                replacement: Some("run".to_string()),
            },
        );
        let warnings = check_deprecated_references(&expr, &deprecations);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("state.exec"));
        assert_eq!(warnings[0].notes, vec!["use 'run' instead"]);
    }

    #[test]
    fn no_warning_for_bare_leaf_of_deprecated_namespace_member() {
        // A user's own top-level `exec` must not warn when only the nested
        // `state.exec` is deprecated: a nested declaration is keyed by its
        // dotted path alone, never by its bare leaf name.
        let expr = acore::var("exec".to_string());
        let mut deprecations = HashMap::new();
        deprecations.insert("state.exec".to_string(), DeprecationSpec::default());
        let warnings = check_deprecated_references(&expr, &deprecations);
        assert!(warnings.is_empty());
    }

    #[test]
    fn warns_on_deeply_nested_deprecated_member_lookup() {
        // `a.b.c` — the base is itself a lookup, so the dotted path is built
        // through the whole chain, matching the key the desugarer records.
        let expr = acore::lookup(
            acore::lookup(acore::var("a".to_string()), "b", None),
            "c",
            None,
        );
        let mut deprecations = HashMap::new();
        deprecations.insert("a.b.c".to_string(), DeprecationSpec::default());
        let warnings = check_deprecated_references(&expr, &deprecations);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("a.b.c"));
    }

    #[test]
    fn no_warning_for_undeprecated_member_of_same_leaf_name() {
        // `io.exec` must not warn when only `state.exec` is deprecated —
        // the qualified path, not the bare leaf, is what matches.
        let expr = acore::lookup(acore::var("io".to_string()), "exec", None);
        let mut deprecations = HashMap::new();
        deprecations.insert("state.exec".to_string(), DeprecationSpec::default());
        let warnings = check_deprecated_references(&expr, &deprecations);
        assert!(warnings.is_empty());
    }
}
