//! Distribute and beta reduce inline functions
use crate::core::error::CoreError;
use crate::core::expr::*;
use crate::core::transform::succ;
use moniker::*;

/// Depth-aware substitution for beta reduction.
///
/// Unlike `RcExpr::substs`, this function correctly handles the case
/// where a replacement expression contains scope-relative bound
/// variables (`BV(scope=k, binder=j)`) that are placed inside nested
/// scope boundaries (Let or Lam).  Each time we descend into a new
/// scope boundary, we increment the scope indices in the replacement
/// by one to keep them pointing at the same binding in the outer
/// context.
///
/// This is necessary when beta-reducing a destructuring lambda such as
/// `([a, b]) -> body` where the body contains a `DestructureListLet`.
/// The Embed values in that Let are inside one additional scope
/// boundary relative to the call site, so any BV substituted in there
/// must have its scope incremented.
fn substs_depth<N: PartialEq<Var<String>>>(
    expr: &RcExpr,
    mappings: &[(N, RcExpr)],
    depth: u32,
) -> Result<RcExpr, CoreError> {
    match &*expr.inner {
        Expr::Var(_, v) => match mappings.iter().find(|&(n, _)| n == v) {
            Some((_, replacement)) => {
                // Replacement is from the call site (depth == 0).
                // If we are now inside `depth` additional scope
                // boundaries, we must increment all outer BVs in the
                // replacement by `depth`.
                let mut adjusted = replacement.clone();
                for _ in 0..depth {
                    adjusted = succ::succ(&adjusted)?;
                }
                Ok(adjusted)
            }
            None => Ok(expr.clone()),
        },
        // Descend into Let: the Embed values are inside the new scope
        // boundary, so depth increases by 1.
        Expr::Let(s, scope, t) => {
            let new_bindings: Result<Vec<_>, CoreError> = scope
                .unsafe_pattern
                .unsafe_pattern
                .iter()
                .map(|(n, Embed(value))| {
                    substs_depth(value, mappings, depth + 1).map(|v| (n.clone(), Embed(v)))
                })
                .collect();
            let new_body = substs_depth(&scope.unsafe_body, mappings, depth + 1)?;
            Ok(RcExpr::from(Expr::Let(
                *s,
                Scope {
                    unsafe_pattern: Rec {
                        unsafe_pattern: new_bindings?,
                    },
                    unsafe_body: new_body,
                },
                *t,
            )))
        }
        // Descend into Lam body: also a new scope boundary.
        Expr::Lam(s, inl, scope) => {
            let new_body = substs_depth(&scope.unsafe_body, mappings, depth + 1)?;
            Ok(RcExpr::from(Expr::Lam(
                *s,
                *inl,
                Scope {
                    unsafe_pattern: scope.unsafe_pattern.clone(),
                    unsafe_body: new_body,
                },
            )))
        }
        _ => {
            // Non-binding nodes: walk children at the same depth.
            let children_result: Result<RcExpr, CoreError> =
                expr.try_walk_safe(&mut |e| substs_depth(e, mappings, depth));
            children_result
        }
    }
}

#[allow(clippy::redundant_closure)]
pub fn inline_pass(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    distribute(expr).and_then(|ref e| beta_reduce(e))
}

/// True iff `expr` is an inlinable lambda
fn inlinable(expr: &RcExpr) -> bool {
    matches!(&*expr.inner, Expr::Lam(_, true, _) | Expr::Intrinsic(_, _))
}

/// Distribute inline lambdas to call site
fn distribute(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    match &*expr.inner {
        Expr::Let(s, scope, _) => {
            let (binders, body) = scope.clone().unbind();

            let open_binders = binders.unrec();

            let inlines: Vec<_> = open_binders
                .iter()
                .filter(|&(_, Embed(v))| inlinable(v))
                .cloned()
                .map(|(Binder(k), Embed(v))| (k, v))
                .collect();

            if inlines.is_empty() {
                Ok(expr.clone())
                // expr.walk_safe(&mut |e| distribute(&e))
            } else {
                let bindings = open_binders
                    .iter()
                    .map(|(b, Embed(v))| {
                        let substituted = v.substs(&inlines);
                        match distribute(&substituted) {
                            Ok(e) => Ok((b.clone(), Embed(e))),
                            Err(e) => Err(e),
                        }
                    })
                    .collect::<Result<Vec<(_, _)>, CoreError>>()?;

                let body = distribute(&body.substs(&inlines))?;

                Ok(RcExpr::from(Expr::Let(
                    *s,
                    Scope::new(Rec::new(bindings), body),
                    LetType::OtherLet,
                )))
            }
        }
        // Use optimized try_walk_safe to avoid unnecessary allocations
        _ => expr.try_walk_safe(&mut |e| distribute(e)),
    }
}

/// Apply lambdas which have been distribute to function positions
fn beta_reduce(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    match &*expr.inner {
        Expr::App(call_smid, f, xs) => {
            match &*f.inner {
                // as substs doesn't succ, we can only handle
                // inlinable lambdas here
                Expr::Lam(_, true, scope) => {
                    let (binders, body) = scope.clone().unbind();

                    // body may not be fully open so handle any bound
                    // variables
                    let body = succ::pred(&body)?;

                    if binders.len() != xs.len() {
                        // cannot inline partial application or extra
                        // args for now
                        // Use optimized try_walk_safe
                        expr.try_walk_safe(&mut |e| beta_reduce(e))
                    } else {
                        let args = xs
                            .iter()
                            .map(beta_reduce)
                            .collect::<Result<Vec<RcExpr>, CoreError>>()?;

                        let mappings = <_>::zip(binders.into_iter(), args).collect::<Vec<_>>();

                        let reduced = substs_depth(&body, &mappings, 0)?;

                        // Preserve the call-site source location on the reduced
                        // expression. After beta reduction, the top-level
                        // expression carries the callee's (prelude) Smid rather
                        // than the user's call site. If the outer App had a
                        // valid Smid, re-tag the result so that subsequent
                        // compiler passes (e.g. STG compiler) can annotate the
                        // code with the correct source location.
                        if call_smid.is_valid() {
                            if let Expr::App(_, rf, rargs) = &*reduced.inner {
                                return Ok(RcExpr::from(Expr::App(
                                    *call_smid,
                                    rf.clone(),
                                    rargs.clone(),
                                )));
                            }
                        }

                        Ok(reduced)
                    }
                }
                // Use optimized try_walk_safe
                _ => expr.try_walk_safe(&mut |e| beta_reduce(e)),
            }
        }
        // Use optimized try_walk_safe
        _ => expr.try_walk_safe(&mut |e| beta_reduce(e)),
    }
}

#[cfg(test)]
pub mod tests {

    use super::*;
    use crate::common::sourcemap::Smid;
    use crate::core::expr::acore::*;
    use crate::core::verify::binding;
    use std::iter;

    #[test]
    pub fn test_simple() {
        let f = free("f");
        let x = free("x");
        let y = free("y");

        let original = let_(
            vec![(
                f.clone(),
                inline(vec![x.clone(), y.clone()], var(y.clone())),
            )],
            app(var(f.clone()), vec![num(22), num(23)]),
        );

        let expected = let_(vec![(f, inline(vec![x, y.clone()], var(y)))], num(23));

        assert_term_eq!(inline_pass(&original).unwrap(), expected);
    }

    #[test]
    pub fn test_with_partially_closed_term() {
        let z = free("z");
        let r = free("∘");
        let f = free("f");
        let g = free("g");
        let j = free("j");
        let k = free("k");
        let x = free("x");
        let n = free("n");
        let m = free("m");
        let compose = free("compose");

        let original = let_(
            vec![
                (
                    z.clone(),
                    app(var(r.clone()), vec![var(n.clone()), var(m.clone())]),
                ),
                (
                    compose.clone(),
                    inline(
                        vec![f.clone(), g.clone(), x.clone()],
                        app(
                            var(f.clone()),
                            vec![app(var(g.clone()), vec![var(x.clone())])],
                        ),
                    ),
                ),
                (
                    r.clone(),
                    inline(
                        vec![f.clone(), g.clone()],
                        app(var(compose.clone()), vec![var(f.clone()), var(g.clone())]),
                    ),
                ),
            ],
            var(z.clone()),
        );
        let expected = let_(
            vec![
                (z.clone(), app(var(compose.clone()), vec![var(n), var(m)])),
                (
                    compose,
                    inline(
                        vec![f.clone(), g.clone(), x.clone()],
                        app(
                            var(f.clone()),
                            vec![app(var(g.clone()), vec![var(x.clone())])],
                        ),
                    ),
                ),
                (
                    r,
                    inline(
                        vec![j.clone(), k.clone()],
                        app(
                            inline(
                                vec![f.clone(), g.clone(), x.clone()],
                                app(var(f), vec![app(var(g), vec![var(x)])]),
                            ),
                            vec![var(j), var(k)],
                        ),
                    ),
                ),
            ],
            var(z),
        );

        assert_term_eq!(inline_pass(&original).unwrap(), expected);
    }

    #[test]
    pub fn test_with_partially_closed_term_deep() {
        let a = free("a");
        let b = free("b");
        let c = free("c");
        let d = free("c");
        let r = free("∘");
        let f = free("f");
        let g = free("g");
        let j = free("j");
        let k = free("k");
        let x = free("x");
        let n = free("n");
        let m = free("m");
        let compose = free("compose");

        let original = let_(
            vec![
                (
                    a.clone(),
                    let_(
                        vec![(
                            b.clone(),
                            let_(
                                vec![(
                                    c.clone(),
                                    let_(
                                        vec![(
                                            d.clone(),
                                            app(
                                                var(r.clone()),
                                                vec![var(n.clone()), var(m.clone())],
                                            ),
                                        )],
                                        block(iter::once(("d".to_string(), var(d.clone())))),
                                    ),
                                )],
                                block(iter::once(("c".to_string(), var(c.clone())))),
                            ),
                        )],
                        block(iter::once(("b".to_string(), var(b.clone())))),
                    ),
                ),
                (
                    compose.clone(),
                    inline(
                        vec![f.clone(), g.clone(), x.clone()],
                        app(
                            var(f.clone()),
                            vec![app(var(g.clone()), vec![var(x.clone())])],
                        ),
                    ),
                ),
                (
                    r.clone(),
                    inline(
                        vec![f.clone(), g.clone()],
                        app(var(compose.clone()), vec![var(f.clone()), var(g.clone())]),
                    ),
                ),
            ],
            var(a.clone()),
        );

        let expected = let_(
            vec![
                (
                    a.clone(),
                    let_(
                        vec![(
                            b.clone(),
                            let_(
                                vec![(
                                    c.clone(),
                                    let_(
                                        vec![(
                                            d.clone(),
                                            app(var(compose.clone()), vec![var(n), var(m)]),
                                        )],
                                        block(iter::once(("d".to_string(), var(d)))),
                                    ),
                                )],
                                block(iter::once(("c".to_string(), var(c)))),
                            ),
                        )],
                        block(iter::once(("b".to_string(), var(b)))),
                    ),
                ),
                (
                    compose,
                    inline(
                        vec![f.clone(), g.clone(), x.clone()],
                        app(
                            var(f.clone()),
                            vec![app(var(g.clone()), vec![var(x.clone())])],
                        ),
                    ),
                ),
                (
                    r,
                    inline(
                        vec![j.clone(), k.clone()],
                        app(
                            inline(
                                vec![f.clone(), g.clone(), x.clone()],
                                app(var(f), vec![app(var(g), vec![var(x)])]),
                            ),
                            vec![var(j), var(k)],
                        ),
                    ),
                ),
            ],
            var(a),
        );

        let inlined = inline_pass(&original).unwrap();
        binding::verify(&inlined).unwrap();
        assert_term_eq!(inlined, expected);
    }

    /// Regression test for eu-5pe9: beta-reducing a destructuring
    /// lambda (one whose body is a `DestructureListLet`) must
    /// correctly adjust scope indices in the replacement when it is
    /// placed inside the nested scope boundary of the Let.
    ///
    /// Previously `substs` was scope-unaware and would leave
    /// `BV(scope=k, binder=j)` unchanged inside the Let's Embed
    /// values, causing `compress::compress` to panic with "eliminating
    /// used var" because prune had marked the originally-referenced
    /// binder as eliminated.
    #[test]
    pub fn test_beta_reduce_destructuring_lambda_with_bound_arg() {
        // Build:
        //   let outer =
        //     let
        //       f = inline([__p0]) ->
        //             DestructureListLet (a = HEAD(__p0), b = HEAD(TAIL(__p0))) in
        //             a + b
        //       x = 99
        //     in f([x, 42])
        //   in outer
        //
        // After inline_pass, f is beta-reduced at the call site and
        // `x` (a BV in the argument) must be correctly placed inside
        // the DestructureListLet with an incremented scope index.

        let f = free("f");
        let p0 = free("__p0");
        let a = free("a");
        let b = free("b");
        let x = free("x");

        // Destructuring let body: a + b  (simplified as app for test purposes)
        let add_a_b = app(bif("ADD"), vec![var(a.clone()), var(b.clone())]);

        // DestructureListLet: a = HEAD(__p0), b = HEAD(TAIL(__p0))
        let destr_let = RcExpr::from(Expr::Let(
            Smid::default(),
            Scope::new(
                Rec::new(vec![
                    (
                        Binder(a.clone()),
                        Embed(app(bif("HEAD"), vec![var(p0.clone())])),
                    ),
                    (
                        Binder(b.clone()),
                        Embed(app(
                            bif("HEAD"),
                            vec![app(bif("TAIL"), vec![var(p0.clone())])],
                        )),
                    ),
                ]),
                add_a_b,
            ),
            LetType::DestructureListLet,
        ));

        // f is an inlinable lambda with single param __p0
        let f_def = inline(vec![p0.clone()], destr_let);

        // x = 99
        let x_val = num(99);

        // f([x, 42]) — note x is a free var that will become a BV
        // after the outer let is closed
        let call = app(var(f.clone()), vec![list(vec![var(x.clone()), num(42)])]);

        // Inner let: let f = ..., x = 99 in f([x, 42])
        let inner = let_(vec![(f.clone(), f_def), (x.clone(), x_val)], call);

        // Verify inline_pass does not panic and produces a
        // binding-valid result
        let result = inline_pass(&inner).expect("inline_pass should not panic");
        binding::verify(&result).expect("result should have valid bindings");
    }
}
