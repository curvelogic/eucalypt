//! Cooking is the process of resolving operator fixities and
//! precedence to turn operator soup into hierarchical expressions.
use crate::common::sourcemap::{HasSmid, Smid};
use crate::core::anaphora;
use crate::core::binding::{Scope, Var};
use crate::core::error::CoreError;
use crate::core::expr::*;
use std::collections::{HashMap, HashSet};

pub mod fill;
pub mod fixity;
pub mod shunt;

pub fn cook(expr: RcExpr) -> Result<RcExpr, CoreError> {
    Cooker::default().cook(expr)
}

/// Cook state
///
/// Needs to track whether we are within the scope of an
/// expression-anaphoric lambda.
///
/// ## Anaphor Scoping Model
///
/// Three rules govern expression anaphor scope:
///
/// 1. **Parens are opaque by default** — anaphors inside parens create
///    a lambda scoped to that paren group.
///
/// 2. **Subsumption** — if the enclosing scope already has direct
///    anaphors, inner paren groups become transparent (their anaphors
///    join the outer scope).
///
/// 3. **ArgTuples follow the same rules** — opaque unless subsumed.
///
/// Anonymous `_` and numbered `_0`/`_1` follow identical scoping
/// rules. Both are opaque at parens, both participate in subsumption.
/// The only difference is that each `_` introduces a new parameter
/// while `_0`/`_1` can reference the same parameter.
///
/// Subsumption works naturally in the implementation: when `wrap_lambda`
/// is true for an outer soup, `in_expr_anaphor_scope` is set to `true`.
/// Inner soups then see this flag and do NOT create their own lambda,
/// allowing their anaphors to propagate to the outer scope.
#[derive(Default)]
pub struct Cooker {
    /// True when we have traversed into the scope of expression anaphora
    in_expr_anaphor_scope: bool,
    /// While in the scope of anaphoric lambda, collect all the
    /// anaphora for processing at the boundary of the scope
    pending_expr_anaphora: HashMap<Anaphor<Smid, i32>, String>,
    /// While in the scope of anaphoric lambda, collect all the
    /// anaphora for processing at the boundary of the scope
    pending_block_anaphora: HashMap<Anaphor<Smid, i32>, String>,
}

impl Cooker {
    /// Cook the expression `expr` resolving all operators and
    /// eliminating Expr::Soup in favour of hierarchical expressions.
    ///
    /// This involves handling expression anaphora (`_`, `_0`, `_1`
    /// etc.) and inferring the existence of invisible ones, as well
    /// as using the shunting yard algo to properly handle the
    /// resolved precedences.
    pub fn cook(&mut self, expr: RcExpr) -> Result<RcExpr, CoreError> {
        let prepped = self.distribute_fixities(expr)?;
        self.cook_(prepped)
    }

    /// A preparatory pass to move operator fixity information from
    /// definition site to call site so the shunting yard algo can do
    /// its work.
    fn distribute_fixities(&mut self, expr: RcExpr) -> Result<RcExpr, CoreError> {
        fixity::distribute(expr)
    }

    /// Infer anaphora in gaps and insert them explicitly.
    ///
    /// e.g.
    /// `(+ 10) => (_ + 10)`
    /// `(2+*5) => (2+_*5)`
    ///
    /// Also return the expression anaphora which have been added.
    fn insert_anaphora(
        &mut self,
        soup: &[RcExpr],
    ) -> Result<(Vec<RcExpr>, fill::AnaphorSet), CoreError> {
        fill::fill_gaps(soup)
    }

    /// Internal walk function for recursing down.
    /// Uses optimized try_walk_safe to avoid unnecessary allocations.
    fn cook_(&mut self, expr: RcExpr) -> Result<RcExpr, CoreError> {
        match &*expr.inner {
            Expr::Let(_, _, _) => self.cook_let(&expr),
            Expr::Soup(_, ref xs) => self.cook_soup(xs),
            Expr::BlockAnaphor(s, anaphor) => Ok(self.cook_block_anaphor(*s, anaphor)),
            Expr::ExprAnaphor(s, anaphor) => Ok(self.cook_expr_anaphor(*s, anaphor)),
            _ => expr.try_walk_safe(&mut |e| self.cook_(e.clone())),
        }
    }

    /// Record the anaphor and substitute a Var
    fn cook_expr_anaphor(&mut self, s: Smid, anaphor: &Anaphor<Smid, i32>) -> RcExpr {
        let var = self
            .pending_expr_anaphora
            .entry(*anaphor)
            .or_insert_with(|| free(&format!("_e{anaphor}")));
        core::var(s, var.clone())
    }

    /// Record the anaphor and substitute a Var
    fn cook_block_anaphor(&mut self, s: Smid, anaphor: &Anaphor<Smid, i32>) -> RcExpr {
        let var = self
            .pending_block_anaphora
            .entry(*anaphor)
            .or_insert_with(|| free(&format!("_b{anaphor}")));
        core::var(s, var.clone())
    }

    /// Resolve precedence and handle expression anaphora.
    ///
    /// Parens are opaque by default: a soup that contains direct
    /// (non-nested) anaphors wraps itself in a lambda. Inner soups
    /// that find `in_expr_anaphor_scope` already true do NOT create
    /// their own lambda — their anaphors propagate to the outer scope
    /// (subsumption).
    fn cook_soup(&mut self, exprs: &[RcExpr]) -> Result<RcExpr, CoreError> {
        let (filled, naked_anaphora) = self.insert_anaphora(exprs)?;

        let wrap_lambda = !self.in_expr_anaphor_scope && !naked_anaphora.is_empty();

        if wrap_lambda {
            self.in_expr_anaphor_scope = true;
        }

        let atoms: Vec<_> = {
            let old_in_expr_anaphor_scope = self.in_expr_anaphor_scope;
            let subcooked = filled
                .iter()
                .map(|e| self.cook_(e.clone()))
                .collect::<Result<Vec<RcExpr>, CoreError>>();
            self.in_expr_anaphor_scope = old_in_expr_anaphor_scope;
            subcooked?
        };

        let cooked = shunt::shunt(atoms)?;

        if wrap_lambda {
            self.in_expr_anaphor_scope = false;

            self.process_expr_anaphora(cooked)
        } else {
            Ok(cooked)
        }
    }

    /// Handle block anaphora.
    /// Uses optimized try_walk_safe to avoid unnecessary allocations.
    fn cook_let(&mut self, expr: &RcExpr) -> Result<RcExpr, CoreError> {
        let inside_anaphoric_block = !self.pending_block_anaphora.is_empty();

        let let_ = expr.try_walk_safe(&mut |e| self.cook_(e.clone()))?;

        let is_anaphoric_block = !self.pending_block_anaphora.is_empty();

        if is_anaphoric_block && !inside_anaphoric_block {
            self.process_block_anaphora(let_)
        } else {
            Ok(let_)
        }
    }

    /// Wrap a lambda around an expr-anaphoric expression
    fn process_expr_anaphora(&mut self, expr: RcExpr) -> Result<RcExpr, CoreError> {
        let binders = anaphora::to_binding_pattern(&self.pending_expr_anaphora)?;
        self.pending_expr_anaphora.clear();
        // Do NOT call succ: close_lam_scope (inside core::lam) uses close_expr_vars
        // which already increments outer BV scope indices to account for the new lambda
        // scope. Calling succ first would double-increment them.
        Ok(core::lam(expr.smid(), binders, expr))
    }

    /// Wrap a lambda around a block-anaphoric expression, collapsing
    /// redundant inner lets where possible.
    ///
    /// When a single block anaphor like `{k: •}` is used with a dot
    /// lookup, desugaring produces `let k = • in body`. After cooking,
    /// this becomes `let k = _b_a in body`. Wrapping naively gives
    /// `lam(_b_a, succ(let k = _b_a in body))` which has a redundant
    /// let scope that confuses STG compilation of outer-scope variable
    /// references. We collapse to `lam(_b_a, body)` when the single
    /// let binding is a simple alias of the anaphor var.
    fn process_block_anaphora(&mut self, expr: RcExpr) -> Result<RcExpr, CoreError> {
        let binders = anaphora::to_binding_pattern(&self.pending_block_anaphora)?;
        let anaphora_vars: HashSet<String> =
            self.pending_block_anaphora.values().cloned().collect();
        self.pending_block_anaphora.clear();

        // Try to collapse: if the expression is a Let with a single
        // binding whose value is our anaphor var, eliminate the let
        // and bind directly as a lambda parameter.
        if let Expr::Let(_, scope, _) = &*expr.inner {
            let bindings = &scope.pattern;
            let body = &scope.body;

            let all_alias = bindings.iter().all(|(_, val)| {
                if let Expr::Var(_, Var::Free(name)) = &*val.inner {
                    anaphora_vars.contains(name)
                } else {
                    false
                }
            });

            if all_alias && bindings.len() == 1 && binders.len() == 1 {
                // Single binding: the lambda directly replaces the let
                // scope, so the body's BV(0,0) already points at the right
                // slot. We do NOT go via core::lam / close_lam_scope because
                // close_expr_vars would increment BV(0,0) → BV(1,0).
                //
                // Use the let binding's name (e.g. "it") rather than the
                // anaphor var name (e.g. "_b_a[…]") so that the BoundVar
                // name field in the body agrees with the lambda pattern,
                // keeping the binding verifier happy.
                let let_name = bindings[0].0.clone();
                return Ok(RcExpr::from(Expr::Lam(
                    expr.smid(),
                    false,
                    Scope {
                        pattern: vec![let_name],
                        body: body.clone(),
                    },
                )));
            }
        }

        // Do NOT call succ::succ before core::lam: close_lam_scope (inside
        // core::lam) uses close_expr_vars which already increments outer BV
        // scope indices to account for the new lambda scope. Calling succ
        // first would double-increment them.
        Ok(core::lam(expr.smid(), binders, expr))
    }
}

#[cfg(test)]
pub mod tests {

    use super::*;
    use crate::core::expr::acore::*;
    use crate::core::expr::core;
    use crate::core::expr::tests::alpha_norm;
    use crate::core::expr::RcExpr;

    fn cook_soup(exprs: &[RcExpr]) -> RcExpr {
        alpha_norm(cook(soup(exprs.to_vec())).unwrap())
    }

    fn expected(expr: RcExpr) -> RcExpr {
        alpha_norm(expr)
    }

    #[test]
    pub fn test_cook() {
        let l50 = core::infixl(Smid::fake(1), 50, bif("L50"));
        let r50 = core::infixr(Smid::fake(2), 50, bif("R50"));
        let l60 = core::infixl(Smid::fake(3), 60, bif("L60"));
        let r60 = core::infixr(Smid::fake(4), 60, bif("R60"));
        let l40 = core::infixl(Smid::fake(5), 40, bif("L40"));
        let r40 = core::infixr(Smid::fake(6), 40, bif("R40"));
        let pre100 = core::prefix(Smid::fake(7), 100, bif("PRE100"));
        let post100 = core::postfix(Smid::fake(8), 100, bif("POST100"));
        let pre10 = core::prefix(Smid::fake(9), 10, bif("PRE10"));
        let post10 = core::postfix(Smid::fake(10), 10, bif("POST10"));
        let ana0 = free("_0");
        let ana1 = free("_1");

        assert_eq!(
            cook_soup(&[num(5), l50.clone(), num(7)]),
            app(bif("L50"), vec![num(5), num(7)])
        );

        let x = free("x");
        let f = free("f");

        assert_eq!(
            cook_soup(&[var(x.clone()), var(f.clone())]),
            app(var(f), vec![var(x)])
        );

        // associates left
        assert_eq!(
            cook_soup(&[num(1), l50.clone(), num(2), l50.clone(), num(3)]),
            app(
                bif("L50"),
                vec![app(bif("L50"), vec![num(1), num(2)]), num(3)]
            )
        );

        // associates right
        assert_eq!(
            cook_soup(&[num(1), r50.clone(), num(2), r50.clone(), num(3)]),
            app(
                bif("R50"),
                vec![num(1), app(bif("R50"), vec![num(2), num(3)])]
            )
        );

        // respects precedence in left
        assert_eq!(
            cook_soup(&[num(1), l40, num(2), l50.clone(), num(3), l60, num(4)]),
            app(
                bif("L40"),
                vec![
                    num(1),
                    app(
                        bif("L50"),
                        vec![num(2), app(bif("L60"), vec![num(3), num(4)])]
                    )
                ]
            )
        );

        // respects precedence in right
        assert_eq!(
            cook_soup(&[num(1), r60, num(2), r50, num(3), r40, num(4)]),
            app(
                bif("R40"),
                vec![
                    app(
                        bif("R50"),
                        vec![app(bif("R60"), vec![num(1), num(2)]), num(3)]
                    ),
                    num(4)
                ]
            )
        );

        // handles unary prefix
        assert_eq!(
            cook_soup(&[pre100.clone(), num(10)]),
            app(bif("PRE100"), vec![num(10)])
        );

        // handles unary postfix
        assert_eq!(
            cook_soup(&[num(10), post100.clone()]),
            app(bif("POST100"), vec![num(10)])
        );

        // handles mixed high precedence unary & binary
        assert_eq!(
            cook_soup(&[
                pre100.clone(),
                num(20),
                l50.clone(),
                num(30),
                post100.clone()
            ]),
            app(
                bif("L50"),
                vec![
                    app(bif("PRE100"), vec![num(20)]),
                    app(bif("POST100"), vec![num(30)])
                ]
            )
        );

        // handles mixed high precedence unary & binary
        assert_eq!(
            cook_soup(&[
                num(30),
                post100.clone(),
                l50.clone(),
                pre100.clone(),
                num(20)
            ]),
            app(
                bif("L50"),
                vec![
                    app(bif("POST100"), vec![num(30)]),
                    app(bif("PRE100"), vec![num(20)])
                ]
            )
        );

        // handles mixed low precedence unary & binary
        assert_eq!(
            cook_soup(&[pre10.clone(), num(20), l50.clone(), num(30), post10.clone()]),
            app(
                bif("PRE10"),
                vec![app(
                    bif("POST10"),
                    vec![app(bif("L50"), vec![num(20), num(30)])]
                )]
            )
        );

        // fills section (`l50` 20) with anaphoric var and abstracts
        assert_eq!(
            cook_soup(&[l50.clone(), num(20)]),
            expected(lam(
                vec![ana0.clone()],
                app(bif("L50"), vec![var(ana0.clone()), num(20)])
            ))
        );

        // fills section (20 `l50`) with anaphoric var and abstracts
        assert_eq!(
            cook_soup(&[num(20), l50.clone()]),
            expected(lam(
                vec![ana0.clone()],
                app(bif("L50"), vec![num(20), var(ana0.clone())])
            ))
        );

        // fills ... (unary pre) (binary)... and abstracts
        assert_eq!(
            cook_soup(&[pre10.clone(), l50.clone(), num(30), post10.clone()]),
            expected(lam(
                vec![ana0.clone()],
                app(
                    bif("PRE10"),
                    vec![app(
                        bif("POST10"),
                        vec![app(bif("L50"), vec![var(ana0.clone()), num(30)])]
                    )]
                )
            ))
        );

        // fills ... (binary) (unary post) ... with anaphor and
        // abstracts
        assert_eq!(
            cook_soup(&[num(30), l50.clone(), post100, pre100, num(20)]),
            expected(lam(
                vec![ana0.clone()],
                app(
                    app(bif("PRE100"), vec![num(20)]),
                    vec![app(
                        bif("L50"),
                        vec![num(30), app(bif("POST100"), vec![var(ana0.clone())])]
                    )]
                )
            ))
        );

        // corrects pre10 pre10 pre10 pre10 with anaphor and abstracts
        assert_eq!(
            cook_soup(&[pre10.clone(), pre10.clone(), pre10.clone(), pre10.clone()]),
            expected(lam(
                vec![ana0.clone()],
                app(
                    bif("PRE10"),
                    vec![app(
                        bif("PRE10"),
                        vec![app(
                            bif("PRE10"),
                            vec![app(bif("PRE10"), vec![var(ana0.clone())])]
                        )]
                    )]
                )
            ))
        );

        // fills pre10 l50 post10
        assert_eq!(
            cook_soup(&[pre10, l50, post10]),
            expected(lam(
                vec![ana0.clone(), ana1.clone()],
                app(
                    bif("PRE10"),
                    vec![app(
                        bif("POST10"),
                        vec![app(
                            bif("L50"),
                            vec![
                                core::var(Smid::fake(11), ana0),
                                core::var(Smid::fake(12), ana1)
                            ]
                        )]
                    )]
                )
            ))
        );
    }

    #[test]
    pub fn test_nested_sample() {
        assert_eq!(
            cook_soup(&[
                bif("HEAD"),
                call(),
                arg_tuple(vec![soup(vec![
                    bif("CONS"),
                    call(),
                    arg_tuple(vec![
                        soup(vec![list(vec![num(1), num(2), num(3)]), bif("HEAD")]),
                        soup(vec![list(vec![num(1), num(2), num(3)]), bif("TAIL")])
                    ])
                ])])
            ]),
            app(
                bif("HEAD"),
                vec![app(
                    bif("CONS"),
                    vec![
                        app(bif("HEAD"), vec![list(vec![num(1), num(2), num(3)])]),
                        app(bif("TAIL"), vec![list(vec![num(1), num(2), num(3)])])
                    ]
                )]
            )
        );
    }

    #[test]
    pub fn test_anaphoric_operation() {
        let l50 = core::infixl(Smid::fake(1), 50, bif("MUL"));
        let ana0 = free("_e_n0");

        assert_eq!(
            cook_soup(&[
                core::expr_anaphor(Smid::fake(2), Some(0)),
                l50,
                core::expr_anaphor(Smid::fake(2), Some(0))
            ]),
            expected(lam(
                vec![ana0.clone()],
                app(bif("MUL"), vec![var(ana0.clone()), var(ana0)])
            ))
        );
    }

    #[test]
    pub fn test_anon_anaphora_opaque_in_parens() {
        // Simulates (_ + _) / 2 — anonymous anaphora are opaque in their
        // paren group. The paren group `(_ + _)` forms its own 2-arg
        // lambda, and the outer soup sees `lam / 2` (applying DIV to a
        // function, not a lambda spanning the whole expression).
        //
        // This preserves patterns like `(_ = :quux) ∘ tag` where the paren
        // group is a complete predicate being composed with another function.
        let l50 = core::infixl(Smid::fake(1), 50, bif("ADD"));
        let l60 = core::infixl(Smid::fake(2), 60, bif("DIV"));

        // Inner soup: _ + _ (two anonymous anaphora)
        let inner = soup(vec![
            core::expr_anaphor(Smid::fake(3), None),
            l50.clone(),
            core::expr_anaphor(Smid::fake(4), None),
        ]);

        // Outer soup: (inner) / 2 — no outer anaphora, so inner stays opaque
        let outer = soup(vec![inner, l60, num(2)]);

        // The outer expression should NOT be a lambda — the inner paren group
        // is opaque (no subsumption).
        let result = cook(outer).unwrap();
        assert!(
            !matches!(&*result.inner, Expr::Lam(_, _, _)),
            "expected NOT a lambda (anon anaphora are opaque in parens without subsumption), got: {result:?}"
        );
    }

    #[test]
    pub fn test_numbered_anaphora_opaque_in_parens() {
        // Simulates (_0 + _1) / 2 — numbered anaphora are also opaque in
        // their paren group under the new model. Parens are opaque by
        // default for BOTH anonymous and numbered anaphora.
        //
        // The paren group `(_0 + _1)` forms its own 2-arg lambda; the
        // outer expression DIV(lambda, 2) is NOT itself a lambda.
        let l50 = core::infixl(Smid::fake(1), 50, bif("ADD"));
        let l60 = core::infixl(Smid::fake(2), 60, bif("DIV"));

        let inner = soup(vec![
            core::expr_anaphor(Smid::fake(3), Some(0)),
            l50.clone(),
            core::expr_anaphor(Smid::fake(4), Some(1)),
        ]);

        let outer = soup(vec![inner, l60, num(2)]);

        // Outer should NOT be a lambda — numbered anaphora in parens are
        // opaque just like anonymous ones.
        let result = cook(outer).unwrap();
        assert!(
            !matches!(&*result.inner, Expr::Lam(_, _, _)),
            "expected NOT a lambda (numbered anaphora opaque in parens), got: {result:?}"
        );
    }

    #[test]
    pub fn test_section_contained_by_parens() {
        // Simulates (+ 1) / 2 — section should stay inside parens
        let l50 = core::infixl(Smid::fake(1), 50, bif("ADD"));
        let l60 = core::infixl(Smid::fake(2), 60, bif("DIV"));
        let ana0 = free("_e_i_l0");

        // Inner soup: + 1 (section — fill_gaps adds implicit anaphor)
        let inner = soup(vec![l50, num(1)]);

        // Outer soup: (inner) / 2
        let outer = soup(vec![inner, l60, num(2)]);

        // Should produce: DIV(lam([_0], ADD(_0, 1)), 2)
        // The section lambda stays inside — NOT lam([_0], DIV(ADD(_0, 1), 2))
        let result = alpha_norm(cook(outer).unwrap());
        assert_eq!(
            result,
            expected(app(
                bif("DIV"),
                vec![
                    lam(vec![ana0.clone()], app(bif("ADD"), vec![var(ana0), num(1)])),
                    num(2)
                ]
            ))
        );
    }
}
