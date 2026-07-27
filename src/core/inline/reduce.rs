//! Distribute and beta reduce inline functions
use crate::common::sourcemap::{HasSmid, Smid};
use crate::core::binding::{Scope, Var};
use crate::core::error::CoreError;
use crate::core::expr::*;
use crate::core::transform::succ;
use std::collections::{HashMap, HashSet};

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
fn substs_depth(
    expr: &RcExpr,
    mappings: &[(String, RcExpr)],
    depth: u32,
) -> Result<RcExpr, CoreError> {
    match &*expr.inner {
        Expr::Var(_, Var::Free(name)) => match mappings.iter().find(|(n, _)| n == name) {
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
        Expr::Var(_, Var::Bound(_)) => Ok(expr.clone()),
        // Descend into Let: the binding values are inside the new scope
        // boundary, so depth increases by 1.
        Expr::Let(s, scope, t) => {
            let new_bindings: Result<Vec<CoreBinding<RcExpr>>, CoreError> = scope
                .pattern
                .iter()
                .map(|b| {
                    substs_depth(&b.expr, mappings, depth + 1)
                        .map(|v| CoreBinding::with_demand(b.name.clone(), v, b.demand))
                })
                .collect();
            let new_body = substs_depth(&scope.body, mappings, depth + 1)?;
            Ok(RcExpr::from(Expr::Let(
                *s,
                Scope {
                    pattern: new_bindings?,
                    body: new_body,
                },
                *t,
            )))
        }
        // Descend into Lam body: also a new scope boundary.
        Expr::Lam(s, inl, scope) => {
            let new_body = substs_depth(&scope.body, mappings, depth + 1)?;
            Ok(RcExpr::from(Expr::Lam(
                *s,
                *inl,
                Scope {
                    pattern: scope.pattern.clone(),
                    body: new_body,
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

/// Peel `Meta` wrappers from an expression, returning the innermost non-Meta
/// node (cloned — cheap, `RcExpr` is `Rc`-backed).
///
/// Source-path prelude combinators are `Meta(Lam(_, true, _), {doc, type})` —
/// `tag_combinators_named` already tags the inner `Lam` inlinable through the
/// `Meta` wrapper (it threads `self_name` through `Expr::Meta`), but
/// `inlinable()` above only recognises a *bare* `Lam(_, true, _)` /
/// `Intrinsic`, so a documented/annotated combinator's `Meta` wrapper hides it
/// from `distribute`'s inline-collection step entirely (eu-rb5n sub-problem
/// 1 — this is the actual mechanism of the source-vs-blob fusion divergence:
/// the blob path never hits this because `xtask` peels `Meta` before storing
/// `inlinable_bindings`). Peeling here is safe: the pre-inline type check
/// (eu-rb5n Q1, `prepare.rs`) has already run and captured any
/// type-annotation warnings on the *unpeeled* core before this pass ever
/// runs, so peeling here affects only the inline/fusion machinery, never
/// diagnostics — harness 104 (`104_suppress_type_warnings_ok.eu`) is the
/// type-safety tripwire for this claim.
fn peel_meta(expr: &RcExpr) -> RcExpr {
    match &*expr.inner {
        Expr::Meta(_, inner, _) => peel_meta(inner),
        _ => expr.clone(),
    }
}

/// Pre-expand a Let scope's inlinable bindings against each other, to a
/// fixed point, before they are substituted into call sites.
///
/// Without this, a self-recursive combinator's *recursive* branch stays
/// under-resolved even after localisation: `distribute`'s single-pass
/// substitution replaces a combinator's own self-reference with its
/// **pre-substitution** value (substitution never re-enters the value it
/// just inserted — see `RcExpr::substs`), so the embedded recursive
/// continuation still calls through wrapper-function siblings (e.g.
/// `foldl`'s `if(nil?(...))`/`head(...)`/`tail(...)`) instead of their
/// resolved (here, raw-intrinsic) bodies. Demand analysis can then only
/// see an opaque, un-specialised recursive body, and the source path stays
/// quadratic even though the copy is correctly localised (eu-rb5n
/// sub-problem 2 — pre-expansion parity with the blob path's xtask-baked
/// `inlinable_bindings`, which are pre-expanded the same way, just once at
/// blob-gen time instead of here on every compile).
///
/// Each entry is repeatedly substituted against its **siblings**
/// (excluding itself, so a binding's own self-reference is never touched —
/// recursion must still resolve to itself, not unroll) until a round makes
/// no further change. `RcExpr::substs`'s `try_walk` short-circuits via
/// `ptr_eq` when nothing changed, so convergence is checked cheaply.
/// Bounded to `inlines.len()` rounds: monotonic (each round can only
/// resolve more references, never fewer), so this is enough for any DAG of
/// dependencies among the scope's own inlinable members.
fn pre_expand_inlines(mut inlines: Vec<(String, RcExpr)>) -> Vec<(String, RcExpr)> {
    for _ in 0..inlines.len() {
        let mut changed = false;
        let next: Vec<(String, RcExpr)> = inlines
            .iter()
            .map(|(name, v)| {
                let siblings: Vec<(String, RcExpr)> =
                    inlines.iter().filter(|(n, _)| n != name).cloned().collect();
                let expanded = v.substs(&siblings);
                if !expanded.ptr_eq(v) {
                    changed = true;
                }
                (name.clone(), expanded)
            })
            .collect();
        inlines = next;
        if !changed {
            break;
        }
    }
    inlines
}

/// Distribute inline lambdas to call site
fn distribute(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    match &*expr.inner {
        Expr::Let(s, scope, _) => {
            let (open_bindings, body) = open_let_scope_full(scope);

            // Peel `Meta` wrappers before the `inlinable` check so a
            // documented/annotated combinator (the source path's shape for
            // every prelude binding) is recognised the same way the blob
            // path's pre-peeled `inlinable_bindings` already are. The
            // *peeled* value is what gets substituted into call sites below
            // — substituting the original `Meta`-wrapped value would leave
            // `beta_reduce`'s `Expr::Lam(_, true, scope)` match unable to see
            // through the wrapper at the call site.
            let inlines: Vec<(String, RcExpr)> = open_bindings
                .iter()
                .filter_map(|(name, v)| {
                    let peeled = peel_meta(v);
                    inlinable(&peeled).then(|| (name.clone(), peeled))
                })
                .collect();

            let inlines = pre_expand_inlines(inlines);

            if inlines.is_empty() {
                Ok(expr.clone())
            } else {
                let bindings = open_bindings
                    .iter()
                    .map(|(name, v)| {
                        let substituted = v.substs(&inlines);
                        match distribute(&substituted) {
                            Ok(e) => Ok((name.clone(), e)),
                            Err(e) => Err(e),
                        }
                    })
                    .collect::<Result<Vec<(String, RcExpr)>, CoreError>>()?;

                let new_body = distribute(&body.substs(&inlines))?;

                Ok(RcExpr::from(Expr::Let(
                    *s,
                    close_let_scope(bindings, new_body),
                    LetType::OtherLet,
                )))
            }
        }
        // Use optimized try_walk_safe to avoid unnecessary allocations
        _ => expr.try_walk_safe(&mut |e| distribute(e)),
    }
}

/// True iff copying `expr` to every occurrence of a binder duplicates no
/// work.
///
/// A variable already names a single shared thunk, and a literal or an
/// intrinsic reference costs nothing to re-evaluate, so any number of
/// copies is free.  Everything else — an application in particular — is a
/// fresh thunk per copy, sharing nothing with its siblings, and so must be
/// let-bound instead (see `beta_reduce`).
fn duplicable(expr: &RcExpr) -> bool {
    matches!(
        &*expr.inner,
        Expr::Var(_, _) | Expr::Literal(_, _) | Expr::Intrinsic(_, _)
    )
}

/// Count how many times each free variable occurs in `expr`.
fn free_var_occurrences(expr: &RcExpr) -> HashMap<String, usize> {
    let mut counts: HashMap<String, usize> = HashMap::new();
    visit_free_vars(expr, &mut |name| {
        *counts.entry(name.to_string()).or_insert(0) += 1;
    });
    counts
}

/// Derive a binding name from `base` that collides with nothing in `avoid`.
///
/// The chosen name is added to `avoid`, so repeated calls stay distinct
/// from each other as well as from the names already in play.
fn fresh_name(base: &str, avoid: &mut HashSet<String>) -> String {
    let mut candidate = format!("__shared_{base}");
    let mut n = 0;
    while avoid.contains(&candidate) {
        n += 1;
        candidate = format!("__shared{n}_{base}");
    }
    avoid.insert(candidate.clone());
    candidate
}

/// Re-tag a beta-reduced application with the call site's Smid.
///
/// After beta reduction the outermost expression carries the *callee's*
/// Smid (typically a prelude location) rather than the user's call site,
/// which is what a diagnostic should blame.  Re-tag so that later passes
/// (the STG compiler in particular) annotate the code with the user's
/// location.
fn retag_call_site(reduced: RcExpr, call_smid: Smid) -> RcExpr {
    if call_smid.is_valid() {
        if let Expr::App(_, rf, rargs) = &*reduced.inner {
            return RcExpr::from(Expr::App(call_smid, rf.clone(), rargs.clone()));
        }
    }
    reduced
}

/// Apply lambdas which have been distribute to function positions
fn beta_reduce(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    match &*expr.inner {
        Expr::App(call_smid, f, xs) => {
            match &*f.inner {
                // as substs doesn't succ, we can only handle
                // inlinable lambdas here
                Expr::Lam(_, true, scope) => {
                    let binders = scope.pattern.clone();
                    let body = open_lam_scope(scope);

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

                        // Substituting an argument at every occurrence of
                        // its binder copies the argument *expression*, and
                        // separate copies become separate thunks that share
                        // nothing.  For a callee that mentions a parameter
                        // more than once that turns one evaluation into
                        // several, and when the argument is the caller's own
                        // recursive call it turns linear work into 2^n
                        // (eu-gua64).  So an argument is substituted directly
                        // only when that is free — when it is duplicable, or
                        // when its binder is used at most once — and is
                        // otherwise bound once in a Let wrapped round the
                        // reduced body, leaving the occurrences to share that
                        // single thunk.
                        let occurrences = free_var_occurrences(&body);
                        // The occurrence map's keys are exactly the body's
                        // free names (same traversal), so reuse them rather
                        // than walking the body a second time.
                        let mut avoid: HashSet<String> = occurrences.keys().cloned().collect();
                        for arg in &args {
                            avoid.extend(free_vars(arg));
                        }

                        let mut mappings: Vec<(String, RcExpr)> = Vec::with_capacity(binders.len());
                        let mut shared: Vec<(String, RcExpr)> = Vec::new();

                        for (binder, arg) in binders.into_iter().zip(args) {
                            let uses = occurrences.get(&binder).copied().unwrap_or(0);
                            if uses > 1 && !duplicable(&arg) {
                                let name = fresh_name(&binder, &mut avoid);
                                let reference =
                                    RcExpr::from(Expr::Var(arg.smid(), Var::Free(name.clone())));
                                mappings.push((binder, reference));
                                shared.push((name, arg));
                            } else {
                                mappings.push((binder, arg));
                            }
                        }

                        let reduced =
                            retag_call_site(substs_depth(&body, &mappings, 0)?, *call_smid);

                        if shared.is_empty() {
                            Ok(reduced)
                        } else {
                            // `close_let_scope` binds the fresh names in the
                            // body and increments the de Bruijn scope indices
                            // of both the body and the bound argument
                            // expressions to account for the scope boundary
                            // they have just moved inside.
                            Ok(RcExpr::from(Expr::Let(
                                *call_smid,
                                close_let_scope(shared, reduced),
                                LetType::OtherLet,
                            )))
                        }
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
    use crate::core::expr::tests::alpha_norm;
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

        assert_eq!(inline_pass(&original).unwrap(), expected);
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
        // `z`'s value is now `compose`'s own Lam value applied directly to
        // `[n, m]` — a valid partial application (`compose` takes 3
        // parameters, `∘` only forwards 2) rather than a bare call to
        // `compose` by name. `pre_expand_inlines` (eu-rb5n sub-problem 2)
        // pre-expands `∘`'s own stored `inlines` entry against its sibling
        // `compose` *before* `∘` is substituted into `z`'s call site, so the
        // embedded `compose` Lam arrives already-expanded, not a reference.
        // `beta_reduce` deliberately never reduces an under-saturated
        // application (`binders.len() != xs.len()`, see its own comment) —
        // exactly the same shape as a genuine curried call (e.g.
        // `range(...) foldl(op, i)`), which the compiler already handles
        // correctly downstream (proven by the 022 benchmark). `∘`'s own
        // stored binding is unaffected: its reconstruction substitutes the
        // *original* (not pre-expanded) `∘` value, unchanged from before.
        let expected = let_(
            vec![
                (
                    z.clone(),
                    app(
                        inline(
                            vec![f.clone(), g.clone(), x.clone()],
                            app(
                                var(f.clone()),
                                vec![app(var(g.clone()), vec![var(x.clone())])],
                            ),
                        ),
                        vec![var(n), var(m)],
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
            var(z),
        );

        assert_eq!(
            alpha_norm(inline_pass(&original).unwrap()),
            alpha_norm(expected)
        );
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

        // See `test_with_partially_closed_term`'s comment: `d`'s value is
        // now `compose`'s Lam value applied directly to `[n, m]` (a valid
        // partial application), not a bare call to `compose` by name.
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
                                            app(
                                                inline(
                                                    vec![f.clone(), g.clone(), x.clone()],
                                                    app(
                                                        var(f.clone()),
                                                        vec![app(
                                                            var(g.clone()),
                                                            vec![var(x.clone())],
                                                        )],
                                                    ),
                                                ),
                                                vec![var(n), var(m)],
                                            ),
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
        assert_eq!(alpha_norm(inlined), alpha_norm(expected));
    }

    /// Regression test for eu-5pe9: beta-reducing a destructuring
    /// lambda (one whose body is a `DestructureListLet`) must
    /// correctly adjust scope indices in the replacement when it is
    /// placed inside the nested scope boundary of the Let.
    #[test]
    pub fn test_beta_reduce_destructuring_lambda_with_bound_arg() {
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
            close_let_scope(
                vec![
                    (a.clone(), app(bif("HEAD"), vec![var(p0.clone())])),
                    (
                        b.clone(),
                        app(bif("HEAD"), vec![app(bif("TAIL"), vec![var(p0.clone())])]),
                    ),
                ],
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
        let _ = (p0, a, b, f, x);
    }

    /// Count `App` nodes anywhere in `expr`.
    fn app_count(expr: &RcExpr) -> usize {
        let mut n = 0;
        count_apps(expr, &mut n);
        n
    }

    fn count_apps(expr: &RcExpr, n: &mut usize) {
        if matches!(&*expr.inner, Expr::App(_, _, _)) {
            *n += 1;
        }
        match &*expr.inner {
            Expr::Let(_, scope, _) => {
                for b in &scope.pattern {
                    count_apps(&b.expr, n);
                }
                count_apps(&scope.body, n);
            }
            Expr::Lam(_, _, scope) => count_apps(&scope.body, n),
            Expr::App(_, f, xs) => {
                count_apps(f, n);
                for x in xs {
                    count_apps(x, n);
                }
            }
            Expr::List(_, xs) | Expr::ArgTuple(_, xs) => {
                for x in xs {
                    count_apps(x, n);
                }
            }
            Expr::Meta(_, e, m) => {
                count_apps(e, n);
                count_apps(m, n);
            }
            _ => {}
        }
    }

    /// Regression test for eu-gua64: an argument that is not trivially
    /// duplicable must be bound once, not copied to every occurrence of
    /// its binder.
    ///
    /// `pick = λ(a, b). GT(a, b)` uses both binders twice over; calling it
    /// with two applications used to substitute each application twice,
    /// so a recursive caller passing its own recursive call cost 2^n.
    /// After the fix the reduced body holds exactly one copy of each
    /// argument, in a `Let` that the two uses share.
    #[test]
    pub fn test_beta_reduce_shares_duplicated_non_atomic_argument() {
        let pick = free("pick");
        let a = free("a");
        let b = free("b");
        let xs = free("xs");

        // λ(a, b). ADD(GT(a, b), GT(b, a)) — each binder used twice.
        let body = app(
            bif("ADD"),
            vec![
                app(bif("GT"), vec![var(a.clone()), var(b.clone())]),
                app(bif("GT"), vec![var(b.clone()), var(a.clone())]),
            ],
        );
        let pick_def = inline(vec![a.clone(), b.clone()], body);

        // pick(HEAD(xs), TAIL(xs)) — two non-atomic arguments.
        let call = app(
            var(pick.clone()),
            vec![
                app(bif("HEAD"), vec![var(xs.clone())]),
                app(bif("TAIL"), vec![var(xs.clone())]),
            ],
        );

        let original = let_(
            vec![(pick.clone(), pick_def), (xs.clone(), list(vec![num(1)]))],
            call,
        );

        let result = inline_pass(&original).expect("inline_pass should not panic");
        binding::verify(&result).expect("result should have valid bindings");

        // Locate the reduced call site: the body of the outer Let.
        let (_, reduced) = open_let_scope_full(match &*result.inner {
            Expr::Let(_, scope, _) => scope,
            other => panic!("expected a Let, got {other:?}"),
        });

        // It must be a Let sharing the arguments, not a bare App.
        let (bindings, inner_body) = match &*reduced.inner {
            Expr::Let(_, scope, _) => open_let_scope_full(scope),
            other => panic!(
                "expected the reduced call site to be wrapped in a sharing Let, got {other:?}"
            ),
        };
        assert_eq!(
            bindings.len(),
            2,
            "both duplicated arguments should be bound once each"
        );

        // The shared bindings hold the two argument applications (HEAD,
        // TAIL); the body holds ADD plus the two GTs. Five `App` nodes in
        // total — seven would mean both arguments were copied.
        assert_eq!(
            app_count(&reduced),
            5,
            "arguments were copied rather than shared: {reduced:?}"
        );
        assert_eq!(app_count(&inner_body), 3);
    }

    /// eu-gua64 boundary: an argument that *is* trivially duplicable, or
    /// whose binder is used at most once, must still be substituted
    /// directly — the sharing `Let` is a cost, not a default.
    #[test]
    pub fn test_beta_reduce_does_not_share_duplicable_or_single_use_arguments() {
        let f = free("f");
        let g = free("g");
        let a = free("a");
        let b = free("b");
        let xs = free("xs");

        // λ(a). ADD(a, a) applied to a *variable* — duplicable, no Let.
        let dup_atomic = let_(
            vec![
                (
                    f.clone(),
                    inline(
                        vec![a.clone()],
                        app(bif("ADD"), vec![var(a.clone()), var(a.clone())]),
                    ),
                ),
                (xs.clone(), num(3)),
            ],
            app(var(f.clone()), vec![var(xs.clone())]),
        );
        let reduced = inline_pass(&dup_atomic).expect("inline_pass should not panic");
        binding::verify(&reduced).expect("result should have valid bindings");
        let (_, site) = open_let_scope_full(match &*reduced.inner {
            Expr::Let(_, scope, _) => scope,
            other => panic!("expected a Let, got {other:?}"),
        });
        assert!(
            matches!(&*site.inner, Expr::App(_, _, _)),
            "a duplicable (variable) argument must not be let-bound: {site:?}"
        );

        // λ(b). NOT(b) applied to an application — used once, no Let.
        let single_use = let_(
            vec![
                (
                    g.clone(),
                    inline(vec![b.clone()], app(bif("NOT"), vec![var(b.clone())])),
                ),
                (xs.clone(), list(vec![num(1)])),
            ],
            app(
                var(g.clone()),
                vec![app(bif("HEAD"), vec![var(xs.clone())])],
            ),
        );
        let reduced = inline_pass(&single_use).expect("inline_pass should not panic");
        binding::verify(&reduced).expect("result should have valid bindings");
        let (_, site) = open_let_scope_full(match &*reduced.inner {
            Expr::Let(_, scope, _) => scope,
            other => panic!("expected a Let, got {other:?}"),
        });
        assert!(
            matches!(&*site.inner, Expr::App(_, _, _)),
            "a single-use argument must not be let-bound: {site:?}"
        );
    }

    /// eu-gua64: the sharing `Let` must preserve the call-site Smid on the
    /// reduced application, so a diagnostic still blames the user's call
    /// site rather than the callee's body (eu-1tkk.7 / eu-og3u6).
    #[test]
    pub fn test_beta_reduce_sharing_let_keeps_call_site_smid() {
        let h = free("h");
        let a = free("a");
        let xs = free("xs");

        let call_smid = Smid::fake(4242);

        let h_def = inline(
            vec![a.clone()],
            app(bif("ADD"), vec![var(a.clone()), var(a.clone())]),
        );

        let call = RcExpr::from(Expr::App(
            call_smid,
            var(h.clone()),
            vec![app(bif("HEAD"), vec![var(xs.clone())])],
        ));

        let original = let_(
            vec![(h.clone(), h_def), (xs.clone(), list(vec![num(1)]))],
            call,
        );

        let result = inline_pass(&original).expect("inline_pass should not panic");
        binding::verify(&result).expect("result should have valid bindings");

        let (_, site) = open_let_scope_full(match &*result.inner {
            Expr::Let(_, scope, _) => scope,
            other => panic!("expected a Let, got {other:?}"),
        });

        // The sharing Let carries the call site...
        let (_, inner_body) = match &*site.inner {
            Expr::Let(s, scope, _) => {
                assert_eq!(*s, call_smid, "sharing Let lost the call-site Smid");
                open_let_scope_full(scope)
            }
            other => panic!("expected a sharing Let, got {other:?}"),
        };

        // ...and so does the application it wraps, which is what the STG
        // compiler turns into the `Ann` a diagnostic reads.
        match &*inner_body.inner {
            Expr::App(s, _, _) => {
                assert_eq!(*s, call_smid, "reduced application lost the call-site Smid")
            }
            other => panic!("expected an App under the sharing Let, got {other:?}"),
        }
    }
}
