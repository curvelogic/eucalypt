//! Distribute and beta reduce inline functions
use crate::core::error::CoreError;
use crate::core::expr::*;
use crate::core::transform::succ;
use moniker::*;

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
                .cloned()
                .filter(|(_, Embed(v))| inlinable(v))
                .map(|(Binder(k), Embed(v))| (k, v))
                .collect();

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
        _ => expr.walk_safe(&mut |e| distribute(&e)),
    }
}

/// Apply lambdas which have been distribute to function positions
fn beta_reduce(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    match &*expr.inner {
        Expr::App(_, f, xs) => {
            match &*f.inner {
                Expr::Lam(_, _, scope) => {
                    let (binders, body) = scope.clone().unbind();

                    // body may not be fully open so handle any bound
                    // variables
                    let body = succ::pred(&body)?;

                    if binders.len() != xs.len() {
                        // cannot inline partial application or extra
                        // args for now
                        expr.walk_safe(&mut |e| beta_reduce(&e))
                    } else {
                        let args =
                            xs.iter()
                                .map(|arg| beta_reduce(&arg))
                                .collect::<Result<Vec<RcExpr>, CoreError>>()?;

                        let mappings = <_>::zip(binders.into_iter(), args).collect::<Vec<_>>();

                        Ok(body.substs(&mappings))
                    }
                }
                _ => expr.walk_safe(&mut |e| beta_reduce(&e)),
            }
        }
        _ => expr.walk_safe(&mut |e| beta_reduce(&e)),
    }
}

#[cfg(test)]
pub mod tests {

    use super::*;
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

        let expected = let_(
            vec![(
                f.clone(),
                inline(vec![x.clone(), y.clone()], var(y.clone())),
            )],
            num(23),
        );

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
                (
                    z.clone(),
                    app(var(compose.clone()), vec![var(n.clone()), var(m.clone())]),
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
                        vec![j.clone(), k.clone()],
                        app(
                            inline(
                                vec![f.clone(), g.clone(), x.clone()],
                                app(
                                    var(f.clone()),
                                    vec![app(var(g.clone()), vec![var(x.clone())])],
                                ),
                            ),
                            vec![var(j.clone()), var(k.clone())],
                        ),
                    ),
                ),
            ],
            var(z.clone()),
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
                                            app(
                                                var(compose.clone()),
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
                        vec![j.clone(), k.clone()],
                        app(
                            inline(
                                vec![f.clone(), g.clone(), x.clone()],
                                app(
                                    var(f.clone()),
                                    vec![app(var(g.clone()), vec![var(x.clone())])],
                                ),
                            ),
                            vec![var(j.clone()), var(k.clone())],
                        ),
                    ),
                ),
            ],
            var(a.clone()),
        );

        let inlined = inline_pass(&original).unwrap();
        binding::verify(&inlined).unwrap();
        assert_term_eq!(inlined, expected);
    }
}
