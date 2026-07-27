//! Deprecation reference checker.
//!
//! Walks a Core expression tree and emits a [`TypeWarning`] for every
//! reference to a deprecated declaration — a bare reference to a deprecated
//! top-level declaration (`old-fn`), or a namespace-member lookup whose
//! dotted path is deprecated (`state.exec`).
//!
//! # Which references count
//!
//! A deprecation is declared at the top level of some translation unit, so
//! only a reference that *resolves* to a unit top-level declaration can be a
//! reference to it.  The walk therefore tracks the binder scopes it descends
//! through and resolves each variable's de Bruijn `(scope, binder)` back to
//! the frame that binds it (see [`binds_unit_top_level`]).  Without that, a
//! lambda parameter or a locally-introduced block that merely *shares a name*
//! with a deprecated namespace inherits its deprecation — so
//!
//! ```text
//! apply-all(random): random.exec(10)
//! ```
//!
//! would warn about the random monad's deprecated runner, which it has
//! nothing to do with.
use crate::common::sourcemap::Smid;
use crate::core::binding::Var;
use crate::core::expr::{Expr, LetType, RcExpr};
use crate::core::metadata::DeprecationSpec;
use crate::core::typecheck::error::TypeWarning;
use std::collections::HashMap;

/// A binder scope the walk has descended through, outermost first.
///
/// Only the *kind* is needed: resolution asks whether the frame binding a
/// variable is a unit/namespace scope or something user code introduced.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Frame {
    /// A `let` scope, carrying the classification the desugarer gave it.
    Let(LetType),
    /// A lambda parameter list.
    Lam,
}

/// Walk `expr` and return a warning for every reference to a declaration
/// that appears in `deprecations`.
///
/// `deprecations` maps the path a caller must write — a bare name for a
/// top-level declaration, a dotted path for a namespace member — to its
/// deprecation spec, exactly as `Desugarer::record_deprecation` records it.
pub fn check_deprecated_references(
    expr: &RcExpr,
    deprecations: &HashMap<String, DeprecationSpec>,
) -> Vec<TypeWarning> {
    if deprecations.is_empty() {
        return vec![];
    }
    let mut warnings = Vec::new();
    let mut frames = Vec::new();
    check_expr(expr, deprecations, &mut warnings, &mut frames);
    warnings
}

fn check_expr(
    expr: &RcExpr,
    deprecations: &HashMap<String, DeprecationSpec>,
    warnings: &mut Vec<TypeWarning>,
    frames: &mut Vec<Frame>,
) {
    match &*expr.inner {
        Expr::Var(smid, Var::Bound(bv)) => {
            if let Some(name) = &bv.name {
                if binds_unit_top_level(frames, bv.scope) {
                    if let Some(spec) = deprecations.get(name.as_str()) {
                        warnings.push(make_warning(name, spec, *smid));
                    }
                }
            }
        }
        Expr::Var(smid, Var::Free(name)) => {
            if let Some(spec) = deprecations.get(name.as_str()) {
                warnings.push(make_warning(name, spec, *smid));
            }
        }
        Expr::Let(_, scope, let_type) => {
            frames.push(Frame::Let(*let_type));
            for b in &scope.pattern {
                check_expr(&b.expr, deprecations, warnings, frames);
            }
            check_expr(&scope.body, deprecations, warnings, frames);
            frames.pop();
        }
        Expr::Lam(_, _, scope) => {
            frames.push(Frame::Lam);
            check_expr(&scope.body, deprecations, warnings, frames);
            frames.pop();
        }
        Expr::App(_, f, args) => {
            check_expr(f, deprecations, warnings, frames);
            for a in args {
                check_expr(a, deprecations, warnings, frames);
            }
        }
        Expr::List(_, xs) => {
            for x in xs {
                check_expr(x, deprecations, warnings, frames);
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
                    check_expr(v, deprecations, warnings, frames);
                }
            }
        }
        Expr::Meta(_, e, m) => {
            check_expr(e, deprecations, warnings, frames);
            check_expr(m, deprecations, warnings, frames);
        }
        Expr::Lookup(smid, e, key, fb) => {
            // Namespace-member deprecation: `state.exec` where the `exec`
            // member of the `state` namespace carries deprecation metadata,
            // recorded under the dotted key `state.exec`.  Only a lookup whose
            // base resolves to a unit top-level declaration qualifies, so an
            // unrelated block's `.exec` member, a lambda parameter that
            // happens to be called `state`, and the non-deprecated `io.exec`
            // all stay silent.
            if let Some(base) = dotted_path(e, frames) {
                let qualified = format!("{base}.{key}");
                if let Some(spec) = deprecations.get(qualified.as_str()) {
                    warnings.push(make_warning(&qualified, spec, *smid));
                }
            }
            check_expr(e, deprecations, warnings, frames);
            if let Some(fallback) = fb {
                check_expr(fallback, deprecations, warnings, frames);
            }
        }
        Expr::ArgTuple(_, xs) | Expr::Soup(_, xs, _) => {
            for x in xs {
                check_expr(x, deprecations, warnings, frames);
            }
        }
        Expr::Operator(_, _, _, e) => {
            check_expr(e, deprecations, warnings, frames);
        }
        // Literals, intrinsics, names, unit, error markers — no sub-expressions
        _ => {}
    }
}

/// Does a `Var::Bound` at de Bruijn `scope` resolve to a declaration at the
/// top level of a translation unit, rather than to something user code
/// introduced along the way?
///
/// `scope` counts binder scopes outward from the innermost, so the frame that
/// binds the variable is `frames[frames.len() - 1 - scope]`.  Unit and
/// namespace scopes are `DefaultBlockLet` — a `let` whose body is the block it
/// binds.  Everything a caller can introduce below one is not: a lambda
/// parameter list is a `Lam` frame, a destructuring pattern is a
/// `Destructure*Let`, and a block used as an expression (`{ … }.( … )`) is an
/// `OtherLet` because its body is the projected expression rather than the
/// block.  Requiring the *whole* chain from the outside down to the binding
/// frame to be `DefaultBlockLet` therefore admits exactly the unit/namespace
/// nesting and rejects the rest.
///
/// Residual limitation, deliberate rather than forced: a declaration that
/// genuinely is at a unit's top level and merely shares a name with a
/// deprecated library namespace — a user's own top-level `state` block with an
/// `exec` member — still matches, because distinguishing it needs the binder
/// identity of the *declaration site* the deprecation came from, which
/// `record_deprecation` does not record (it records a source path). Fixing
/// that means keying deprecations by binder rather than by path, through the
/// loader's unit merge; it is a bigger change than this pass.
fn binds_unit_top_level(frames: &[Frame], scope: u32) -> bool {
    let Some(idx) = frames.len().checked_sub(1 + scope as usize) else {
        // Resolves outside anything we walked through — treat as top level.
        return true;
    };
    frames[..=idx]
        .iter()
        .all(|f| matches!(f, Frame::Let(LetType::DefaultBlockLet)))
}

/// The dotted source path of a lookup base, if it is a unit top-level variable
/// or a chain of lookups over one — used to qualify a namespace-member lookup
/// (`state.exec`, or `a.b.c` for a declaration nested two blocks deep).
fn dotted_path(expr: &RcExpr, frames: &[Frame]) -> Option<String> {
    match &*expr.inner {
        Expr::Var(_, Var::Bound(bv)) if binds_unit_top_level(frames, bv.scope) => bv.name.clone(),
        Expr::Var(_, Var::Free(name)) => Some(name.clone()),
        Expr::Lookup(_, base, key, _) => {
            dotted_path(base, frames).map(|path| format!("{path}.{key}"))
        }
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

    /// `random.exec` is deprecated.  `deprecations` for the scope tests.
    fn random_exec_deprecated() -> HashMap<String, DeprecationSpec> {
        let mut deprecations = HashMap::new();
        deprecations.insert("random.exec".to_string(), DeprecationSpec::default());
        deprecations
    }

    #[test]
    fn no_warning_when_lookup_base_is_a_lambda_parameter() {
        // `apply-all(random): random.exec(10)` — the base is a parameter, not
        // the library namespace, so it must not inherit its deprecation.
        let expr = acore::default_let(vec![(
            "apply-all".to_string(),
            acore::lam(
                vec!["random".to_string()],
                acore::lookup(acore::var("random".to_string()), "exec", None),
            ),
        )]);
        let warnings = check_deprecated_references(&expr, &random_exec_deprecated());
        assert!(warnings.is_empty());
    }

    #[test]
    fn warns_when_lookup_base_is_top_level_even_from_inside_a_lambda() {
        // The complement of the test above: a genuine reference to the
        // top-level `random`, written inside a lambda, must still warn.
        let expr = acore::default_let(vec![
            ("random".to_string(), acore::block(vec![])),
            (
                "f".to_string(),
                acore::lam(
                    vec!["n".to_string()],
                    acore::lookup(acore::var("random".to_string()), "exec", None),
                ),
            ),
        ]);
        let warnings = check_deprecated_references(&expr, &random_exec_deprecated());
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("random.exec"));
    }

    #[test]
    fn no_warning_when_lookup_base_is_bound_by_an_expression_let() {
        // `{ random: … }.(random.exec(7))` — a block used as an expression is
        // an `OtherLet`, not a unit/namespace scope.
        let expr = acore::let_(
            vec![("random".to_string(), acore::block(vec![]))],
            acore::lookup(acore::var("random".to_string()), "exec", None),
        );
        let warnings = check_deprecated_references(&expr, &random_exec_deprecated());
        assert!(warnings.is_empty());
    }

    #[test]
    fn no_warning_for_bare_var_shadowed_by_a_lambda_parameter() {
        // A parameter that shadows a deprecated top-level declaration is not
        // a reference to it.
        let expr = acore::default_let(vec![(
            "g".to_string(),
            acore::lam(vec!["old-fn".to_string()], acore::var("old-fn".to_string())),
        )]);
        let mut deprecations = HashMap::new();
        deprecations.insert("old-fn".to_string(), DeprecationSpec::default());
        let warnings = check_deprecated_references(&expr, &deprecations);
        assert!(warnings.is_empty());
    }

    #[test]
    fn warns_for_bare_var_referring_to_the_deprecated_top_level_declaration() {
        // The complement: the same name, resolving to the top-level binding.
        let expr = acore::default_let(vec![
            ("old-fn".to_string(), acore::block(vec![])),
            ("caller".to_string(), acore::var("old-fn".to_string())),
        ]);
        let mut deprecations = HashMap::new();
        deprecations.insert("old-fn".to_string(), DeprecationSpec::default());
        let warnings = check_deprecated_references(&expr, &deprecations);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("old-fn"));
    }
}
