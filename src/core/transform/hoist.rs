//! Namespace lambda hoisting pass.
//!
//! Many namespaces (e.g. `str`, `cal`, `vec`) are desugared as nested
//! `DefaultBlockLet` bindings whose members are inlinable lambdas (`Lam(_, true,
//! _)`) or intrinsics.  Before this pass the inliner must distribute `str` as a
//! whole block to every call site before individual lookups like `str.upper` can
//! be simplified.  This is expensive.
//!
//! This pass hoists the individual members to top-level `Let` bindings named
//! `__<namespace>_<member>` (e.g. `__str_upper`) and rewrites
//! `Lookup(Var(Free("str")), "upper", _)` to `Var(Free("__str_upper"))`.  The
//! inliner can then work directly with the individual functions without ever
//! distributing the namespace block.
//!
//! # Algorithm
//!
//! 1. Walk the expression tree.  For every `Let` binding whose value is a
//!    `DefaultBlockLet`, treat the binding name as a namespace candidate.
//!
//! 2. Open the inner `DefaultBlockLet` scope.  For each member that is a
//!    `Lam(_, true, _)` or `Intrinsic(_, _)` **and** whose free variables do
//!    not intersect with the set of sibling member names (i.e. it is
//!    self-contained), record it as hoistable with the generated name
//!    `__<namespace>_<member>`.
//!
//! 3. Add the hoistable bindings as an `OtherLet` scope wrapping the entire
//!    expression.
//!
//! 4. Rewrite all `Lookup(Var(Free(ns)), member, _)` nodes where
//!    `__<ns>_<member>` was hoisted to `Var(Free("__<ns>_<member>"))`.  The
//!    original namespace blocks are preserved so they can still be used as
//!    values (e.g. passed to a higher-order function).

use std::collections::{HashMap, HashSet};

use crate::common::sourcemap::Smid;
use crate::core::binding::{CoreBinding, Var};
use crate::core::error::CoreError;
use crate::core::expr::{close_let_scope, free_vars, open_let_scope_full, Expr, LetType, RcExpr};

/// A hoistable member: namespace name, member name, and the open (free-var)
/// expression to bind at the top level.
struct Hoistable {
    ns: String,
    member: String,
    expr: RcExpr,
}

/// Strip `Meta` wrappers to get the inner expression.
///
/// Namespace members in the prelude are often wrapped in a `Meta` node that
/// carries documentation and type annotations.  We strip those to examine the
/// underlying expression kind when deciding what is hoistable.
fn strip_meta(expr: &RcExpr) -> &RcExpr {
    match &*expr.inner {
        Expr::Meta(_, inner, _) => strip_meta(inner),
        _ => expr,
    }
}

/// True iff `expr` (after stripping metadata) is an inlinable lambda or
/// an intrinsic reference — the kinds of expressions that the inline pass
/// can beta-reduce once hoisted to a top-level binding.
fn is_hoistable_kind(expr: &RcExpr) -> bool {
    matches!(
        &*strip_meta(expr).inner,
        Expr::Lam(_, true, _) | Expr::Intrinsic(_, _)
    )
}

/// Collect hoistable members from a single `DefaultBlockLet` binding value.
///
/// `ns_name` is the name of the binding (e.g. `"str"`).  `ns_expr` is the
/// binding's value, which may be a `DefaultBlockLet` optionally wrapped in
/// one or more `Meta` nodes (the desugarer adds metadata wrappers for doc
/// and type annotations).
///
/// Returns `None` if `ns_expr` (after stripping metadata) is not a
/// `DefaultBlockLet`.  Returns `Some(vec)` with the members safe to hoist.
fn collect_hoistable_from_ns(ns_name: &str, ns_expr: &RcExpr) -> Option<Vec<Hoistable>> {
    // Strip any metadata wrapper added by the desugarer before checking the
    // expression kind — namespace bindings in the prelude are often
    // `Meta(smid, Let(..., DefaultBlockLet), doc_block)`.
    let inner = strip_meta(ns_expr);
    let Expr::Let(_, inner_scope, LetType::DefaultBlockLet) = &*inner.inner else {
        return None;
    };

    // Open the inner scope so members are in free-variable form.
    let (open_bindings, _body) = open_let_scope_full(inner_scope);

    // The sibling names are all binding names in this scope.
    let sibling_names: HashSet<&str> = open_bindings.iter().map(|(n, _)| n.as_str()).collect();

    let mut hoistable = Vec::new();

    for (member_name, member_expr) in &open_bindings {
        // Only hoist inlinable lambdas and intrinsics (stripping any metadata
        // wrappers that the desugarer adds for doc/type annotations).
        if !is_hoistable_kind(member_expr) {
            continue;
        }

        // Skip members whose free variables include any sibling name —
        // these are self-referential and cannot be lifted without also
        // rewriting their bodies.
        let fvs = free_vars(member_expr);
        let self_referential = fvs.iter().any(|fv| sibling_names.contains(fv.as_str()));
        if self_referential {
            continue;
        }

        // Strip metadata wrappers from the hoisted expression: the hoisted
        // binding should be the raw lambda or intrinsic so that the inliner's
        // `inlinable()` check recognises it and distributes it to call sites.
        hoistable.push(Hoistable {
            ns: ns_name.to_string(),
            member: member_name.clone(),
            expr: strip_meta(member_expr).clone(),
        });
    }

    Some(hoistable)
}

/// Walk `expr` and collect all hoistable namespace members.
///
/// The result maps `(ns_name, member_name)` → generated hoisted name.
fn collect_all_hoistable(expr: &RcExpr) -> Vec<Hoistable> {
    let mut result = Vec::new();
    collect_all_hoistable_rec(expr, &mut result);
    result
}

fn collect_all_hoistable_rec(expr: &RcExpr, result: &mut Vec<Hoistable>) {
    match &*expr.inner {
        Expr::Let(_, scope, LetType::DefaultBlockLet) => {
            // Open the outer scope to inspect binding values.
            let (open_bindings, _body) = open_let_scope_full(scope);

            for (ns_name, ns_expr) in &open_bindings {
                if let Some(members) = collect_hoistable_from_ns(ns_name, ns_expr) {
                    result.extend(members);
                }
                // Recurse into the binding value (handles nesting).
                collect_all_hoistable_rec(ns_expr, result);
            }

            // Recurse into the body too.
            collect_all_hoistable_rec(&scope.body, result);
        }
        Expr::Let(_, scope, _) => {
            for b in &scope.pattern {
                collect_all_hoistable_rec(&b.expr, result);
            }
            collect_all_hoistable_rec(&scope.body, result);
        }
        Expr::Lam(_, _, scope) => {
            collect_all_hoistable_rec(&scope.body, result);
        }
        Expr::Lookup(_, target, _, fallback) => {
            collect_all_hoistable_rec(target, result);
            if let Some(fb) = fallback {
                collect_all_hoistable_rec(fb, result);
            }
        }
        Expr::App(_, f, xs) => {
            collect_all_hoistable_rec(f, result);
            for x in xs {
                collect_all_hoistable_rec(x, result);
            }
        }
        Expr::List(_, xs) | Expr::ArgTuple(_, xs) => {
            for x in xs {
                collect_all_hoistable_rec(x, result);
            }
        }
        Expr::Block(_, bm) => {
            for (_, v) in bm.iter() {
                collect_all_hoistable_rec(v, result);
            }
        }
        Expr::Meta(_, e, m) => {
            // Walk through metadata wrappers — namespace bindings in the
            // prelude are wrapped as `Meta(smid, DefaultBlockLet, doc)`.
            collect_all_hoistable_rec(e, result);
            collect_all_hoistable_rec(m, result);
        }
        Expr::Soup(_, xs, _) => {
            for x in xs {
                collect_all_hoistable_rec(x, result);
            }
        }
        // Leaves — nothing to recurse into.
        _ => {}
    }
}

/// The generated name for a hoisted namespace member.
fn hoisted_name(ns: &str, member: &str) -> String {
    format!("__{ns}_{member}")
}

/// Rewrite `Lookup(Var(Free(ns)), member, fallback)` to
/// `Var(Free("__<ns>_<member>"))` where the hoisted name is known.
///
/// Also rewrites `Var(Free(ns))` in positions where the namespace is used
/// purely for its member lookups — but the dispatch says to preserve
/// the original block so we only rewrite the Lookup nodes.
fn rewrite_lookups(
    expr: &RcExpr,
    hoisted: &HashMap<(String, String), String>,
) -> Result<RcExpr, CoreError> {
    match &*expr.inner {
        Expr::Lookup(s, target, member, _fallback) => {
            // Check if target is Var(Free(ns)) and (ns, member) is hoisted.
            if let Expr::Var(_, Var::Free(ns_name)) = &*target.inner {
                let key = (ns_name.clone(), member.clone());
                if let Some(generated) = hoisted.get(&key) {
                    return Ok(RcExpr::from(Expr::Var(*s, Var::Free(generated.clone()))));
                }
            }
            // Recurse — target may itself be a complex expression.
            expr.try_walk_safe(&mut |e| rewrite_lookups(e, hoisted))
        }
        _ => expr.try_walk_safe(&mut |e| rewrite_lookups(e, hoisted)),
    }
}

/// Run the namespace lambda hoisting pass.
///
/// Returns the expression with hoisted namespace members added as top-level
/// `OtherLet` bindings and all eligible `Lookup` nodes rewritten to
/// direct `Var` references.
pub fn hoist(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    let hoistable = collect_all_hoistable(expr);

    if hoistable.is_empty() {
        return Ok(expr.clone());
    }

    // Build lookup map: (ns, member) → generated name.
    let mut hoisted_map: HashMap<(String, String), String> = HashMap::new();
    let mut bindings: Vec<CoreBinding<RcExpr>> = Vec::new();

    for h in hoistable {
        let gen_name = hoisted_name(&h.ns, &h.member);
        hoisted_map.entry((h.ns, h.member)).or_insert_with(|| {
            bindings.push(CoreBinding::new(gen_name.clone(), h.expr));
            gen_name.clone()
        });
    }

    // Rewrite Lookup nodes in the expression.
    let rewritten = rewrite_lookups(expr, &hoisted_map)?;

    // Wrap with an OtherLet binding all hoisted members.
    //
    // We use OtherLet (not DefaultBlockLet) so the prune pass treats these
    // as regular definitions and the inliner distributes them normally.
    //
    // IMPORTANT: We must close the scope via `close_let_scope` so that
    // `Var::Free("__str_to-upper")` references in the body become
    // `Var::Bound(scope=0, binder=k)`.  The prune pass only follows
    // `Var::Bound` references, so without closing the free references
    // would be invisible and the bindings would be pruned as dead code.
    let binding_pairs: Vec<(String, RcExpr)> =
        bindings.into_iter().map(|b| (b.name, b.expr)).collect();
    Ok(RcExpr::from(Expr::Let(
        Smid::default(),
        close_let_scope(binding_pairs, rewritten),
        LetType::OtherLet,
    )))
}

/// Run the namespace lambda hoisting pass with awareness of prelude blob globals.
///
/// Extends `hoist` for the blob-prelude path: when the blob is active the
/// user-code core expression contains no namespace `DefaultBlockLet` blocks
/// (they live in the blob as precompiled globals).  However user code may
/// still contain `Lookup(Var(Free("str")), "upper")` nodes that need to be
/// rewritten to `Var(Free("__str_upper"))` so the compiler's
/// `resolve_free_var` can emit `Ref::G` for them.
///
/// This function:
/// 1. Collects hoistable members from the user expression (may be empty).
/// 2. Scans `prelude_globals` for names matching `__<ns>_<member>` and seeds
///    the rewrite map from them — without adding new bindings (the blob already
///    has them as globals).
/// 3. Rewrites matching `Lookup` nodes to direct `Var(Free(...))` references.
/// 4. Wraps any locally-hoisted members (from user code) in an `OtherLet`,
///    as `hoist` does.
///
/// When `prelude_globals` is empty this behaves identically to `hoist`.
pub fn hoist_with_blob_globals(
    expr: &RcExpr,
    prelude_globals: &HashMap<String, usize>,
) -> Result<RcExpr, CoreError> {
    let hoistable = collect_all_hoistable(expr);

    // Build lookup map: (ns, member) → generated name.
    let mut hoisted_map: HashMap<(String, String), String> = HashMap::new();
    let mut bindings: Vec<CoreBinding<RcExpr>> = Vec::new();

    // Register locally hoistable members (from user code, e.g. when EU_SOURCE_PRELUDE=1).
    for h in hoistable {
        let gen_name = hoisted_name(&h.ns, &h.member);
        hoisted_map.entry((h.ns, h.member)).or_insert_with(|| {
            bindings.push(CoreBinding::new(gen_name.clone(), h.expr));
            gen_name.clone()
        });
    }

    // Seed the rewrite map from blob globals that match `__<ns>_<member>`.
    // These already exist as prelude globals, so no new bindings are needed.
    // Splitting on the first `_` after `__` extracts (ns, member).  Eucalypt
    // namespace names and member names use hyphens, not underscores, so the
    // first `_` in the remainder is reliably the separator.
    for name in prelude_globals.keys() {
        if let Some(rest) = name.strip_prefix("__") {
            if let Some(sep) = rest.find('_') {
                let ns = &rest[..sep];
                let member = &rest[sep + 1..];
                if !ns.is_empty() && !member.is_empty() {
                    let key = (ns.to_string(), member.to_string());
                    hoisted_map.entry(key).or_insert_with(|| name.clone());
                }
            }
        }
    }

    if hoisted_map.is_empty() {
        return Ok(expr.clone());
    }

    // Rewrite Lookup nodes in the expression.
    let rewritten = rewrite_lookups(expr, &hoisted_map)?;

    if bindings.is_empty() {
        // All rewrites point to blob globals; no new local bindings needed.
        return Ok(rewritten);
    }

    // Wrap locally hoisted members in an OtherLet scope (same as `hoist`).
    let binding_pairs: Vec<(String, RcExpr)> =
        bindings.into_iter().map(|b| (b.name, b.expr)).collect();
    Ok(RcExpr::from(Expr::Let(
        Smid::default(),
        close_let_scope(binding_pairs, rewritten),
        LetType::OtherLet,
    )))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::expr::acore;
    use crate::core::expr::{close_lam_scope, Primitive};

    fn make_inline_lam(params: Vec<String>, body: RcExpr) -> RcExpr {
        RcExpr::from(Expr::Lam(
            Smid::default(),
            true, // inlinable
            close_lam_scope(params, body),
        ))
    }

    fn make_identity_lam() -> RcExpr {
        make_inline_lam(
            vec!["x".to_string()],
            RcExpr::from(Expr::Var(Smid::default(), Var::Free("x".to_string()))),
        )
    }

    #[test]
    fn test_hoist_simple_namespace() {
        // Build: let str = DefaultBlockLet { upper: Lam } in Block { str: str }
        let ns_expr = acore::default_let(vec![("upper".to_string(), make_identity_lam())]);
        let outer = acore::default_let(vec![("str".to_string(), ns_expr)]);

        let result = hoist(&outer).expect("hoist failed");

        // The result should be an OtherLet with a binding named __str_upper.
        match &*result.inner {
            Expr::Let(_, scope, LetType::OtherLet) => {
                let has_hoisted = scope.pattern.iter().any(|b| b.name == "__str_upper");
                assert!(has_hoisted, "expected __str_upper binding");
            }
            other => panic!("expected OtherLet, got: {other:?}"),
        }
    }

    #[test]
    fn test_rewrite_lookup_to_var() {
        // Build a Lookup node: str.upper
        let lookup = RcExpr::from(Expr::Lookup(
            Smid::default(),
            RcExpr::from(Expr::Var(Smid::default(), Var::Free("str".to_string()))),
            "upper".to_string(),
            None,
        ));

        let mut hoisted_map = HashMap::new();
        hoisted_map.insert(
            ("str".to_string(), "upper".to_string()),
            "__str_upper".to_string(),
        );

        let result = rewrite_lookups(&lookup, &hoisted_map).expect("rewrite failed");
        match &*result.inner {
            Expr::Var(_, Var::Free(name)) => {
                assert_eq!(name, "__str_upper");
            }
            other => panic!("expected Var(Free(__str_upper)), got: {other:?}"),
        }
    }

    #[test]
    fn test_skip_self_referential_member() {
        // A member that references a sibling should not be hoisted.
        // "upper" body contains Var(Free("lower")) — a sibling ref.
        let upper_body = RcExpr::from(Expr::Var(Smid::default(), Var::Free("lower".to_string())));
        let upper_lam = make_inline_lam(vec![], upper_body);
        // "lower" is a plain identity lambda — no sibling refs.
        let lower_lam = make_identity_lam();

        let inner = acore::default_let(vec![
            ("upper".to_string(), upper_lam),
            ("lower".to_string(), lower_lam),
        ]);
        let ns_members = collect_hoistable_from_ns("str", &inner).expect("expected Some");
        // "upper" references "lower" (sibling) → skip.
        // "lower" references only "x" (its own param) → hoist.
        let names: Vec<&str> = ns_members.iter().map(|h| h.member.as_str()).collect();
        assert!(
            !names.contains(&"upper"),
            "self-referential upper should be skipped"
        );
        assert!(names.contains(&"lower"), "plain lower should be hoisted");
    }

    #[test]
    fn test_idempotent_on_no_namespace() {
        // An expression with no DefaultBlockLet namespaces should be returned unchanged.
        let expr = RcExpr::from(Expr::Literal(Smid::default(), Primitive::Num(42.into())));
        let result = hoist(&expr).expect("hoist failed");
        // With no hoistable members, the result is a clone of the input.
        assert_eq!(result, expr);
    }

    #[test]
    fn test_no_hoist_non_inlinable_member() {
        // A member that is not Lam(_, true, _) or Intrinsic should not be hoisted.
        let non_inline = RcExpr::from(Expr::Lam(
            Smid::default(),
            false, // NOT inlinable
            close_lam_scope(
                vec!["x".to_string()],
                RcExpr::from(Expr::Var(Smid::default(), Var::Free("x".to_string()))),
            ),
        ));
        let inner = acore::default_let(vec![("apply".to_string(), non_inline)]);
        let ns_members = collect_hoistable_from_ns("ns", &inner).expect("expected Some");
        assert!(
            ns_members.is_empty(),
            "non-inlinable member should not be hoisted"
        );
    }
}
