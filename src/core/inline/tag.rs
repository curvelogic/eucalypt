//! Tag selected lambdas as inlinable for inline pass
use crate::core::{
    binding::{CoreBinding, Scope, Var},
    error::CoreError,
    expr::{Expr, LamScope, LetType, RcExpr},
};
use std::collections::HashSet;

/// Maximum core-expr node count for a recursive higher-order combinator body to
/// qualify for copy-specialisation. Guards against a single pathologically large
/// recursive combinator bloating user code at each call site. See design §1.2
/// (`docs/superpowers/specs/2026-07-13-recursive-combinator-copy-specialisation-design.md`).
const MAX_COPY_BODY_NODES: usize = 48;

/// Walk the expression tagging combinators and destructuring lambdas.
///
/// A lambda is tagged inlinable if:
/// - Its body is a closed expression tree (every node is a lambda parameter
///   reference, literal, intrinsic, or application of closed sub-expressions);
///   or
/// - It has exactly one parameter and its body is a destructuring let (a
///   `DestructureBlockLet` or `DestructureListLet`). This allows the inline
///   pass to distribute destructuring functions to their call sites so that
///   the subsequent fusion pass can simplify the resulting static lookups.
pub fn tag_combinators(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    tag_combinators_named(expr, None)
}

/// As [`tag_combinators`], but carrying the name of the binding whose value
/// `expr` is (when known), so a self-recursive higher-order combinator can be
/// recognised by the [`recursive_combinator`] criterion (design §1.1). The
/// source path supplies each binding's name through the `Let` arm below;
/// xtask's blob-gen fixed-point passes the binding name directly for the bare
/// lambda it holds.
pub fn tag_combinators_named(expr: &RcExpr, self_name: Option<&str>) -> Result<RcExpr, CoreError> {
    match &*expr.inner {
        Expr::Lam(s, false, scope) => {
            if closed_body(&scope.body) || destructuring(scope) {
                Ok(RcExpr::from(Expr::Lam(*s, true, scope.clone())))
            } else if self_name.is_some_and(|n| recursive_combinator(n, scope)) {
                // Tag inlinable and still process the body: unlike a closed
                // body, a recursive combinator body may hold nested inlinable
                // lambdas that should be tagged too.
                let body = tag_combinators_named(&scope.body, None)?;
                Ok(RcExpr::from(Expr::Lam(
                    *s,
                    true,
                    Scope {
                        pattern: scope.pattern.clone(),
                        body,
                    },
                )))
            } else {
                // Recurse into the lambda body even if not itself inlinable
                expr.walk_safe(&mut |e| tag_combinators_named(&e, None))
            }
        }
        Expr::Meta(s, inner, m) => {
            // Metadata (e.g. a docstring) wraps a binding value without changing
            // its identity — thread the self-name through to the inner lambda so
            // a documented recursive combinator is still recognised.
            let new_inner = tag_combinators_named(inner, self_name)?;
            let new_meta = tag_combinators_named(m, None)?;
            Ok(RcExpr::from(Expr::Meta(*s, new_inner, new_meta)))
        }
        Expr::Let(s, scope, lt) => {
            // Each binding value is tagged with its own name as self-context, so
            // a self-recursive higher-order combinator bound here is recognised.
            let bindings = scope
                .pattern
                .iter()
                .map(|b| {
                    tag_combinators_named(&b.expr, Some(&b.name))
                        .map(|e| CoreBinding::with_demand(b.name.clone(), e, b.demand))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let body = tag_combinators_named(&scope.body, None)?;
            Ok(RcExpr::from(Expr::Let(
                *s,
                Scope {
                    pattern: bindings,
                    body,
                },
                *lt,
            )))
        }
        _ => expr.walk_safe(&mut |e| tag_combinators_named(&e, None)),
    }
}

/// A self-recursive higher-order combinator qualifies for copy-specialisation
/// when **all** hold (design §1.1):
/// 1. it applies one of its own parameters (higher-order), and
/// 2. it recurses on its own name, and
/// 3. its body is within [`MAX_COPY_BODY_NODES`].
fn recursive_combinator(self_name: &str, scope: &LamScope<RcExpr>) -> bool {
    applies_own_param(&scope.body, 0)
        && references_name(&scope.body, self_name)
        && node_count(&scope.body) <= MAX_COPY_BODY_NODES
}

/// Direct sub-expressions of a core node, for read-only structural traversal.
fn child_exprs(expr: &RcExpr) -> Vec<&RcExpr> {
    match &*expr.inner {
        Expr::Let(_, scope, _) => {
            let mut v: Vec<&RcExpr> = scope.pattern.iter().map(|b| &b.expr).collect();
            v.push(&scope.body);
            v
        }
        Expr::Lam(_, _, scope) => vec![&scope.body],
        Expr::App(_, f, xs) => {
            let mut v = vec![f];
            v.extend(xs.iter());
            v
        }
        Expr::Lookup(_, t, _, fallback) => {
            let mut v = vec![t];
            if let Some(f) = fallback {
                v.push(f);
            }
            v
        }
        Expr::Meta(_, e, m) => vec![e, m],
        Expr::List(_, xs) | Expr::ArgTuple(_, xs) | Expr::Soup(_, xs, _) => xs.iter().collect(),
        Expr::Block(_, m) => m.values().collect(),
        Expr::Operator(_, _, _, e) => vec![e],
        _ => vec![],
    }
}

/// True if `expr` applies, in function-head position, a parameter of the lambda
/// whose body sits `depth` binding-scopes above (0 = that lambda's own body). A
/// parameter reference of the target lambda is `Var::Bound { scope == depth }`;
/// `Lam`/`Let` each add one scope as we descend.
fn applies_own_param(expr: &RcExpr, depth: u32) -> bool {
    match &*expr.inner {
        Expr::Lam(_, _, scope) => applies_own_param(&scope.body, depth + 1),
        Expr::Let(_, scope, _) => {
            scope
                .pattern
                .iter()
                .any(|b| applies_own_param(&b.expr, depth + 1))
                || applies_own_param(&scope.body, depth + 1)
        }
        Expr::App(_, f, xs) => {
            head_is_param_at(f, depth)
                || applies_own_param(f, depth)
                || xs.iter().any(|x| applies_own_param(x, depth))
        }
        // Non-binding nodes: children are at the same depth.
        _ => child_exprs(expr)
            .iter()
            .any(|c| applies_own_param(c, depth)),
    }
}

/// True if the application head `f` resolves to a bound variable of the scope at
/// `depth` (curried applications peel the head).
fn head_is_param_at(f: &RcExpr, depth: u32) -> bool {
    match &*f.inner {
        Expr::Var(_, Var::Bound(bv)) => bv.scope == depth,
        Expr::App(_, g, _) => head_is_param_at(g, depth),
        _ => false,
    }
}

/// True if `expr` references the binding `name` anywhere — as a free variable
/// or as a bound variable that preserves the name (the self-reference is
/// `Var::Free(name)` after blob-gen peeling, `Var::Bound { name }` on the
/// scoped source path).
fn references_name(expr: &RcExpr, name: &str) -> bool {
    match &*expr.inner {
        Expr::Var(_, Var::Free(n)) => n == name,
        Expr::Var(_, Var::Bound(bv)) => bv.name.as_deref() == Some(name),
        _ => child_exprs(expr).iter().any(|c| references_name(c, name)),
    }
}

/// Total core-expr node count (this node plus all descendants).
fn node_count(expr: &RcExpr) -> usize {
    1 + child_exprs(expr)
        .iter()
        .map(|c| node_count(c))
        .sum::<usize>()
}

/// A lambda body is a closed expression if every node is one of:
/// - `Var::Bound(scope=0)` — a reference to a parameter of the immediately
///   enclosing lambda
/// - `Literal`
/// - `Intrinsic`
/// - `App(closed_f, [closed_arg]*)` — application of closed sub-expressions
///
/// No free variables, no `Let` bindings, and no nested lambdas are permitted.
/// This generalises the old flat `combinator` check to arbitrary-depth
/// compositions, capturing functions such as:
/// - `abs(n)`   → `__IF(__LT(n, 0), __SUB(0, n), n)`
/// - `max(l,r)` → `__IF(__GT(l, r), l, r)`
/// - `!=(l,r)`  → `__NOT(__EQ(l, r))`
fn closed_body(expr: &RcExpr) -> bool {
    match &*expr.inner {
        Expr::Var(_, Var::Bound(bv)) => bv.scope == 0,
        Expr::Literal(_, _) => true,
        Expr::Intrinsic(_, _) => true,
        Expr::App(_, f, xs) => closed_body(f) && xs.iter().all(closed_body),
        _ => false,
    }
}

/// A lambda is a destructuring lambda if it takes exactly one parameter and
/// its body is a `DestructureBlockLet` or `DestructureListLet`. Such lambdas
/// are safe to inline because destructuring is deterministic and the fusion
/// pass will subsequently simplify the resulting static lookups.
fn destructuring(lam_scope: &LamScope<RcExpr>) -> bool {
    if lam_scope.pattern.len() != 1 {
        return false;
    }
    let body = &lam_scope.body;
    matches!(
        &*body.inner,
        Expr::Let(
            _,
            _,
            LetType::DestructureBlockLet | LetType::DestructureListLet
        )
    )
}

/// Check that every `Var::Free` in `expr` is a member of `set`.
///
/// Returns `true` if the expression's free variables are all known
/// (i.e. will be resolvable when injected alongside the set members).
/// Used by xtask's fixed-point collection to determine which prelude
/// bindings are safe to include as inline cores.
pub fn all_free_vars_in_set(expr: &RcExpr, set: &HashSet<String>) -> bool {
    all_free_vars_resolvable(expr, set, None)
}

/// As [`all_free_vars_in_set`], but the binding's **own name** counts as
/// resolvable (a self-reference binds to the binding's own injected `Let` slot
/// at runtime). Required for the blob-gen leg of the copy-specialisation
/// criterion so a self-recursive combinator is not rejected on its own
/// recursive reference. See design §3.2.
pub fn all_free_vars_in_set_with_self(
    expr: &RcExpr,
    set: &HashSet<String>,
    self_name: &str,
) -> bool {
    all_free_vars_resolvable(expr, set, Some(self_name))
}

fn all_free_vars_resolvable(expr: &RcExpr, set: &HashSet<String>, self_name: Option<&str>) -> bool {
    match &*expr.inner {
        Expr::Var(_, Var::Free(name)) => {
            self_name == Some(name.as_str()) || set.contains(name.as_str())
        }
        Expr::Var(_, Var::Bound(_)) => true,
        Expr::Literal(_, _) => true,
        Expr::Intrinsic(_, _) => true,
        Expr::App(_, f, xs) => {
            all_free_vars_resolvable(f, set, self_name)
                && xs
                    .iter()
                    .all(|x| all_free_vars_resolvable(x, set, self_name))
        }
        Expr::Meta(_, inner, _) => all_free_vars_resolvable(inner, set, self_name),
        _ => false,
    }
}

#[cfg(test)]
pub mod tests {

    use super::*;
    use crate::core::expr::acore::*;
    use crate::core::expr::*;

    // ── closed_body tests ─────────────────────────────────────────────────────

    #[test]
    pub fn test_closed_body_bound_scope0() {
        // Var::Bound(scope=0) — a direct lambda parameter reference → closed
        let expr = lam(vec![free("x")], var(free("x")));
        // The body `var(x)` where x is Bound(scope=0) should be recognised as closed
        assert!(tag_combinators(&expr).is_ok());
        // Verify it is tagged inlinable
        if let Expr::Lam(_, inl, _) = &*tag_combinators(&expr).unwrap().inner {
            assert!(*inl, "Bound(scope=0) body should be inlinable");
        }
    }

    #[test]
    pub fn test_closed_body_literal() {
        // A literal is a closed body
        let x = free("x");
        let expr = lam(vec![x.clone()], num(42));
        if let Expr::Lam(_, inl, _) = &*tag_combinators(&expr).unwrap().inner {
            assert!(*inl, "Literal body should be inlinable");
        }
        let _ = x;
    }

    #[test]
    pub fn test_closed_body_intrinsic() {
        // A bare intrinsic is a closed body
        let x = free("x");
        let expr = lam(vec![x.clone()], bif("ADD"));
        if let Expr::Lam(_, inl, _) = &*tag_combinators(&expr).unwrap().inner {
            assert!(*inl, "Intrinsic body should be inlinable");
        }
        let _ = x;
    }

    #[test]
    pub fn test_closed_body_app_flat_combinator() {
        // App(Intrinsic, [Bound, Literal]) — flat combinator pattern → closed
        let x = free("x");
        let y = free("y");
        let body = app(bif("ADD"), vec![var(x.clone()), var(y.clone())]);
        let expr = lam(vec![x.clone(), y.clone()], body);
        if let Expr::Lam(_, inl, _) = &*tag_combinators(&expr).unwrap().inner {
            assert!(*inl, "Flat combinator App should be inlinable");
        }
        let _ = (x, y);
    }

    #[test]
    pub fn test_closed_body_app_deep() {
        // App(Intrinsic, [App(Intrinsic, [Bound]), Bound]) — deep composition → closed
        let x = free("x");
        let y = free("y");
        let inner = app(bif("NEG"), vec![var(x.clone())]);
        let body = app(bif("ADD"), vec![inner, var(y.clone())]);
        let expr = lam(vec![x.clone(), y.clone()], body);
        if let Expr::Lam(_, inl, _) = &*tag_combinators(&expr).unwrap().inner {
            assert!(*inl, "Deep combinator App should be inlinable");
        }
        let _ = (x, y);
    }

    #[test]
    pub fn test_closed_body_with_free_var_not_inlinable() {
        // App(Var::Free, [Bound]) — free variable reference → NOT closed
        let x = free("x");
        let f = free("external");
        let body = app(var(f.clone()), vec![var(x.clone())]);
        let expr = lam(vec![x.clone()], body);
        if let Expr::Lam(_, inl, _) = &*tag_combinators(&expr).unwrap().inner {
            assert!(
                !*inl,
                "Free var in body should NOT be inlinable as closed body"
            );
        }
        let _ = (x, f);
    }

    // ── all_free_vars_in_set tests ────────────────────────────────────────────

    #[test]
    pub fn test_all_free_vars_no_free_vars() {
        // Literals and intrinsics have no Free vars → true regardless of set
        use std::collections::HashSet;
        let set: HashSet<String> = HashSet::new();
        assert!(all_free_vars_in_set(&num(1), &set));
        assert!(all_free_vars_in_set(&bif("ADD"), &set));
        // Lam nodes are not traversed by all_free_vars_in_set (falls through
        // to the `_ => false` arm) — this is intentional: the function is
        // used on lambda bodies, not on the Lam wrappers themselves.
        let x = free("x");
        let lam_expr = lam(vec![x.clone()], num(42));
        assert!(
            !all_free_vars_in_set(&lam_expr, &set),
            "Lam node returns false (checked on body separately, not the wrapper)"
        );
        let _ = x;
    }

    #[test]
    pub fn test_all_free_vars_in_set_present() {
        // Free("if") with set containing "if" → true
        use std::collections::HashSet;
        let mut set: HashSet<String> = HashSet::new();
        set.insert("if".to_string());
        let expr = var(free("if"));
        assert!(all_free_vars_in_set(&expr, &set));
    }

    #[test]
    pub fn test_all_free_vars_not_in_set() {
        // Free("map") with set NOT containing "map" → false
        use std::collections::HashSet;
        let set: HashSet<String> = HashSet::new();
        let expr = var(free("map"));
        assert!(!all_free_vars_in_set(&expr, &set));
    }

    #[test]
    pub fn test_all_free_vars_nested_app_mixed() {
        // App(Free("known"), [Free("unknown")]) → false
        use std::collections::HashSet;
        let mut set: HashSet<String> = HashSet::new();
        set.insert("known".to_string());
        let expr = app(var(free("known")), vec![var(free("unknown"))]);
        assert!(!all_free_vars_in_set(&expr, &set));
    }

    #[test]
    pub fn test_all_free_vars_meta_transparent() {
        // Meta wrapping is transparent
        use std::collections::HashSet;
        let mut set: HashSet<String> = HashSet::new();
        set.insert("if".to_string());
        // Wrap a Free("if") in Meta
        use crate::common::sourcemap::Smid;
        use crate::core::expr::Expr;
        let inner = var(free("if"));
        let meta_expr = RcExpr::from(Expr::Meta(Smid::default(), inner, num(42)));
        assert!(all_free_vars_in_set(&meta_expr, &set));
    }

    #[test]
    pub fn test_simple() {
        let f = free("f");
        let y = free("y");
        let z = free("z");

        let original = let_(
            vec![(
                f.clone(),
                lam(
                    vec![y.clone(), z.clone()],
                    app(bif("F"), vec![var(y.clone()), var(z.clone())]),
                ),
            )],
            app(var(f.clone()), vec![num(22), num(23)]),
        );

        let expected = let_(
            vec![(
                f.clone(),
                inline(
                    vec![y.clone(), z.clone()],
                    app(bif("F"), vec![var(y), var(z)]),
                ),
            )],
            app(var(f), vec![num(22), num(23)]),
        );

        assert_eq!(tag_combinators(&original).unwrap(), expected);
    }

    // ── recursive-combinator criterion tests (design §1.1) ────────────────────

    /// Is the `Lam` bound to `name` in a top-level `Let` tagged inlinable?
    fn binding_inlinable(expr: &RcExpr, name: &str) -> Option<bool> {
        if let Expr::Let(_, scope, _) = &*expr.inner {
            for b in &scope.pattern {
                if b.name == name {
                    if let Expr::Lam(_, inl, _) = &*b.expr.inner {
                        return Some(*inl);
                    }
                }
            }
        }
        None
    }

    /// `myfold(op, acc, xs)` — self-recursive AND applies its own parameter
    /// `op`, within the size cap → tagged inlinable.
    #[test]
    fn test_recursive_combinator_tagged() {
        let body = app(
            bif("IF"),
            vec![
                app(bif("NILP"), vec![var(free("xs"))]),
                var(free("acc")),
                app(
                    var(free("myfold")),
                    vec![
                        var(free("op")),
                        app(
                            var(free("op")),
                            vec![var(free("acc")), app(bif("HEAD"), vec![var(free("xs"))])],
                        ),
                        app(bif("TAIL"), vec![var(free("xs"))]),
                    ],
                ),
            ],
        );
        let f = lam(vec![free("op"), free("acc"), free("xs")], body);
        let expr = let_(vec![(free("myfold"), f)], var(free("myfold")));
        let tagged = tag_combinators(&expr).unwrap();
        assert_eq!(binding_inlinable(&tagged, "myfold"), Some(true));
    }

    /// `fib(n)` — self-recursive but first-order (applies no parameter) → the
    /// null control; NOT tagged.
    #[test]
    fn test_first_order_recursion_not_tagged() {
        let body = app(
            bif("IF"),
            vec![
                app(bif("LT"), vec![var(free("n")), num(2)]),
                var(free("n")),
                app(
                    bif("ADD"),
                    vec![
                        app(
                            var(free("fib")),
                            vec![app(bif("SUB"), vec![var(free("n")), num(1)])],
                        ),
                        app(
                            var(free("fib")),
                            vec![app(bif("SUB"), vec![var(free("n")), num(2)])],
                        ),
                    ],
                ),
            ],
        );
        let f = lam(vec![free("n")], body);
        let expr = let_(vec![(free("fib"), f)], var(free("fib")));
        let tagged = tag_combinators(&expr).unwrap();
        assert_eq!(binding_inlinable(&tagged, "fib"), Some(false));
    }

    /// `hof(f, x) = external(f(x))` — applies its parameter `f` and is not
    /// closed (references the free var `external`), but is NOT self-recursive →
    /// NOT tagged (the recursive leg fails; closed_body also fails).
    #[test]
    fn test_higher_order_non_recursive_not_tagged() {
        let body = app(
            var(free("external")),
            vec![app(var(free("f")), vec![var(free("x"))])],
        );
        let g = lam(vec![free("f"), free("x")], body);
        let expr = let_(vec![(free("hof"), g)], var(free("hof")));
        let tagged = tag_combinators(&expr).unwrap();
        assert_eq!(binding_inlinable(&tagged, "hof"), Some(false));
    }

    /// A self-recursive higher-order combinator whose body exceeds
    /// `MAX_COPY_BODY_NODES` is NOT tagged (the size-cap leg fails).
    #[test]
    fn test_size_cap_excludes_oversized() {
        let mut deep = var(free("xs"));
        for _ in 0..(MAX_COPY_BODY_NODES + 4) {
            deep = app(bif("ID"), vec![deep]);
        }
        let body = app(
            bif("IF"),
            vec![
                app(bif("NILP"), vec![var(free("xs"))]),
                var(free("acc")),
                app(
                    var(free("bigfold")),
                    vec![
                        var(free("op")),
                        app(var(free("op")), vec![var(free("acc")), deep]),
                        app(bif("TAIL"), vec![var(free("xs"))]),
                    ],
                ),
            ],
        );
        let f = lam(vec![free("op"), free("acc"), free("xs")], body);
        let expr = let_(vec![(free("bigfold"), f)], var(free("bigfold")));
        let tagged = tag_combinators(&expr).unwrap();
        assert_eq!(binding_inlinable(&tagged, "bigfold"), Some(false));
    }

    /// A bare lambda passed without a self-name (xtask style) is only tagged when
    /// the name is supplied via [`tag_combinators_named`].
    #[test]
    fn test_named_entry_point_required_for_recursion() {
        let body = app(
            bif("IF"),
            vec![
                app(bif("NILP"), vec![var(free("xs"))]),
                var(free("acc")),
                app(
                    var(free("g")),
                    vec![
                        var(free("op")),
                        app(var(free("op")), vec![var(free("acc")), var(free("xs"))]),
                    ],
                ),
            ],
        );
        // Close the self-reference `g` over a let so it is a named Bound var,
        // then extract the lambda to feed the bare-lambda entry points.
        let closed = let_(
            vec![(
                free("g"),
                lam(vec![free("op"), free("acc"), free("xs")], body),
            )],
            var(free("g")),
        );
        let lam_expr = if let Expr::Let(_, scope, _) = &*closed.inner {
            scope.pattern[0].expr.clone()
        } else {
            unreachable!()
        };
        // Without the name, the bare lambda is not tagged.
        if let Expr::Lam(_, inl, _) = &*tag_combinators(&lam_expr).unwrap().inner {
            assert!(!*inl, "bare lambda without self-name must not be tagged");
        }
        // With the name (xtask path), it is.
        if let Expr::Lam(_, inl, _) = &*tag_combinators_named(&lam_expr, Some("g")).unwrap().inner {
            assert!(*inl, "named entry must tag the recursive combinator");
        }
    }
}
