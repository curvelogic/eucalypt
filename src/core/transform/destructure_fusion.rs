//! Destructure fusion pass: elide block/list construction on direct pattern match.
//!
//! When a function with a destructuring parameter is called with a literal
//! block or list, this pass fuses the construction and deconstruction into
//! direct bindings, avoiding the intermediate allocation.
//!
//! For block destructuring:
//!
//! ```text
//! App(Lam([__p0], Let(DestructureBlockLet, [x = Lookup(__p0, "x"), y = Lookup(__p0, "y")], body)),
//!     [Block({x: e_x, y: e_y})])
//! ```
//!
//! becomes:
//!
//! ```text
//! Let(OtherLet, [x = e_x, y = e_y], body)
//! ```
//!
//! For fixed-length list destructuring:
//!
//! ```text
//! App(Lam([__p0], Let(DestructureListLet, [a = HEAD(__p0), b = HEAD(TAIL(__p0))], body)),
//!     [List([e_a, e_b])])
//! ```
//!
//! becomes:
//!
//! ```text
//! Let(OtherLet, [a = e_a, b = e_b], body)
//! ```
//!
//! Head/tail patterns (where the tail binding is `xs = TAIL^n(__p0)`) are NOT
//! fused because fusing the tail would require constructing a sub-list.
//!
//! This pass runs before the inline pass. It is conservative: it only fuses
//! when the argument is a literal block or list (not a variable or computed
//! value), and only when ALL bindings in the destructure let can be directly
//! resolved from the literal.

use crate::common::sourcemap::Smid;
use crate::core::error::CoreError;
use crate::core::expr::*;
use moniker::*;

/// Type alias for the let scope used in destructuring lets.
type LetScope = Scope<Rec<Vec<(Binder<String>, Embed<RcExpr>)>>, RcExpr>;

/// Run the destructure fusion pass over an expression.
///
/// Recursively walks the expression tree, fusing any directly applicable
/// pattern.
pub fn destructure_fusion(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    match &*expr.inner {
        Expr::App(s, f, args) => {
            // Recurse into the function first (in case it contains fuseable apps)
            let f_reduced = destructure_fusion(f)?;
            let args_reduced = args
                .iter()
                .map(destructure_fusion)
                .collect::<Result<Vec<_>, _>>()?;

            // Attempt to fuse
            let fused = try_fuse_app(*s, &f_reduced, &args_reduced)?;
            match fused {
                Some(result) => {
                    // Recurse into the fused result (may expose more fusions)
                    destructure_fusion(&result)
                }
                None => Ok(RcExpr::from(Expr::App(*s, f_reduced, args_reduced))),
            }
        }
        _ => expr.try_walk_safe(&mut |e| destructure_fusion(e)),
    }
}

/// Attempt to fuse `App(f, args)` where `f` is a lambda with destructuring lets.
///
/// Returns `Some(fused)` if fusion was possible, `None` if not applicable.
fn try_fuse_app(s: Smid, f: &RcExpr, args: &[RcExpr]) -> Result<Option<RcExpr>, CoreError> {
    // Only handle single-param lambdas with a single arg for simplicity
    // (the common case for destructuring patterns)
    if args.len() != 1 {
        return Ok(None);
    }

    let arg = &args[0];

    // The function must be a non-inline lambda
    let lam_scope = match &*f.inner {
        Expr::Lam(_, false, scope) => scope,
        _ => return Ok(None),
    };

    // Unbind the lambda — this replaces the lambda param (now bound) with a fresh FreeVar
    let (binders, body) = lam_scope.clone().unbind();

    // Only handle exactly one param (the synthetic destructure param)
    if binders.len() != 1 {
        return Ok(None);
    }

    // The body (possibly wrapped in metadata) must be a DestructureBlockLet or
    // DestructureListLet at the top level.
    let (let_s, let_scope, let_type) = match top_destructure_let(&body) {
        Some(t) => t,
        None => return Ok(None),
    };

    // Extract the FreeVar for the synthetic param (after unbind, it's a fresh FreeVar)
    let p0_fv = match &binders[0] {
        Binder(fv) => fv.clone(),
    };

    let _ = s;

    match (let_type, &*arg.inner) {
        (LetType::DestructureBlockLet, Expr::Block(_, block_map)) => {
            // Try to fuse block destructuring with a block literal
            try_fuse_block(let_s, &p0_fv, let_scope, block_map)
        }
        (LetType::DestructureListLet, Expr::List(_, list_items)) => {
            // Try to fuse list destructuring with a list literal
            try_fuse_list(let_s, &p0_fv, let_scope, list_items)
        }
        _ => Ok(None),
    }
}

/// Find the top-level destructure let in an expression, looking through
/// metadata wrappers.
fn top_destructure_let(expr: &RcExpr) -> Option<(Smid, LetScope, LetType)> {
    match &*expr.inner {
        Expr::Let(s, scope, lt @ LetType::DestructureBlockLet)
        | Expr::Let(s, scope, lt @ LetType::DestructureListLet) => Some((*s, scope.clone(), *lt)),
        Expr::Meta(_, inner, _) => top_destructure_let(inner),
        _ => None,
    }
}

/// Fuse block destructuring: replace `Lookup(__p0, field)` bindings with
/// the corresponding block field values.
///
/// Only fuses if ALL bindings can be resolved directly. Returns `None`
/// if any binding cannot be fused (e.g., field not present in literal).
fn try_fuse_block(
    let_s: Smid,
    p0_fv: &FreeVar<String>,
    let_scope: LetScope,
    block_map: &BlockMap<RcExpr>,
) -> Result<Option<RcExpr>, CoreError> {
    let (binders, let_body) = let_scope.unbind();
    let open_binders = binders.unrec();

    // For each binding, check if it is `binding_var = Lookup(Var::Free(p0_fv), "field")`
    // and if so, replace it with `binding_var = block_map[field]`.
    let mut fused_bindings: Vec<(Binder<String>, Embed<RcExpr>)> = Vec::new();

    for (binder, Embed(value)) in &open_binders {
        match try_match_block_lookup(p0_fv, value, block_map) {
            Some(field_val) => {
                fused_bindings.push((binder.clone(), Embed(field_val)));
            }
            None => {
                // Some binding could not be fused — do not fuse partially
                return Ok(None);
            }
        }
    }

    // Check that __p0 is not referenced in the let body (outside the now-fused bindings).
    // For a pure block destructure, __p0 should only appear in the Lookup calls.
    if is_free_var_in(p0_fv, &let_body) {
        // __p0 still needed in body — cannot eliminate without substitution
        return Ok(None);
    }

    // Reconstruct as an OtherLet — the synthetic param is no longer needed
    let result = RcExpr::from(Expr::Let(
        let_s,
        Scope::new(Rec::new(fused_bindings), let_body),
        LetType::OtherLet,
    ));

    Ok(Some(result))
}

/// Match `Lookup(Var::Free(p0_fv), field_name, None)` and return the value
/// from `block_map` for that field, if present.
fn try_match_block_lookup(
    p0_fv: &FreeVar<String>,
    value: &RcExpr,
    block_map: &BlockMap<RcExpr>,
) -> Option<RcExpr> {
    match &*value.inner {
        Expr::Lookup(_, obj, field, None) => {
            if is_exactly_free_var(obj, p0_fv) {
                block_map.get(field).cloned()
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Fuse list destructuring: replace `HEAD(TAIL^n(__p0))` bindings with
/// the corresponding list element values.
///
/// Only fuses fixed-length element bindings (HEAD/TAIL chains). Tail
/// bindings (`xs = TAIL^n(__p0)`) are NOT fused because constructing a
/// sub-list at compile time would be more expensive than runtime access.
fn try_fuse_list(
    let_s: Smid,
    p0_fv: &FreeVar<String>,
    let_scope: LetScope,
    list_items: &[RcExpr],
) -> Result<Option<RcExpr>, CoreError> {
    let (binders, let_body) = let_scope.unbind();
    let open_binders = binders.unrec();

    let mut fused_bindings: Vec<(Binder<String>, Embed<RcExpr>)> = Vec::new();

    for (binder, Embed(value)) in &open_binders {
        match try_match_list_access(p0_fv, value, list_items) {
            Some(elem_val) => {
                fused_bindings.push((binder.clone(), Embed(elem_val)));
            }
            None => {
                // Cannot fuse this binding — do not fuse partially
                return Ok(None);
            }
        }
    }

    // Check __p0 is not referenced in let_body outside the fused bindings
    if is_free_var_in(p0_fv, &let_body) {
        return Ok(None);
    }

    let result = RcExpr::from(Expr::Let(
        let_s,
        Scope::new(Rec::new(fused_bindings), let_body),
        LetType::OtherLet,
    ));

    Ok(Some(result))
}

/// Match a list element access: `HEAD(TAIL^n(__p0))` for index `n`.
///
/// The pattern is: the value is `HEAD(tail_chain)` where `tail_chain` is
/// zero or more `TAIL(...)` applications wrapping `Var::Free(p0_fv)`.
///
/// Returns the corresponding list item if the pattern matches and the index
/// is within bounds.
fn try_match_list_access(
    p0_fv: &FreeVar<String>,
    value: &RcExpr,
    list_items: &[RcExpr],
) -> Option<RcExpr> {
    // The outer expression must be HEAD(...)
    match &*value.inner {
        Expr::App(_, head_f, head_args) if head_args.len() == 1 => {
            if let Expr::Intrinsic(_, name) = &*head_f.inner {
                if name != "HEAD" {
                    return None;
                }
            } else {
                return None;
            }

            // Count TAIL applications inside HEAD's argument
            let (inner, tail_count) = peel_tail_apps(&head_args[0]);

            // The innermost expression must be Var::Free(p0_fv)
            if is_exactly_free_var(inner, p0_fv) {
                // This is HEAD(TAIL^tail_count(__p0)) — element at index tail_count
                list_items.get(tail_count).cloned()
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Peel off TAIL applications: `TAIL(TAIL(TAIL(e)))` → `(e, 3)`.
fn peel_tail_apps(expr: &RcExpr) -> (&RcExpr, usize) {
    match &*expr.inner {
        Expr::App(_, f, args) if args.len() == 1 => {
            if let Expr::Intrinsic(_, name) = &*f.inner {
                if name == "TAIL" {
                    let (inner, count) = peel_tail_apps(&args[0]);
                    return (inner, count + 1);
                }
            }
            (expr, 0)
        }
        _ => (expr, 0),
    }
}

/// Check if `expr` is exactly `Var::Free(fv)` by unique identity.
fn is_exactly_free_var(expr: &RcExpr, fv: &FreeVar<String>) -> bool {
    match &*expr.inner {
        Expr::Var(_, Var::Free(expr_fv)) => expr_fv == fv,
        _ => false,
    }
}

/// Check if a free variable appears anywhere in `expr`.
fn is_free_var_in(fv: &FreeVar<String>, expr: &RcExpr) -> bool {
    let mut found = false;
    expr.visit_vars(&mut |var| {
        if let Var::Free(expr_fv) = var {
            if expr_fv == fv {
                found = true;
            }
        }
    });
    found
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::expr::acore::*;
    use crate::core::verify::binding;

    fn lookup_free(fv: FreeVar<String>, field: &str) -> RcExpr {
        RcExpr::from(Expr::Lookup(
            Smid::default(),
            var(fv),
            field.to_string(),
            None,
        ))
    }

    fn destruct_block_let(
        p0_fv: FreeVar<String>,
        fields: &[(&str, &str)], // (field_name, binding_name)
        body: RcExpr,
    ) -> RcExpr {
        let bindings: Vec<(Binder<String>, Embed<RcExpr>)> = fields
            .iter()
            .map(|(field, binding)| {
                let binding_fv = free(binding);
                (Binder(binding_fv), Embed(lookup_free(p0_fv.clone(), field)))
            })
            .collect();
        RcExpr::from(Expr::Let(
            Smid::default(),
            Scope::new(Rec::new(bindings), body),
            LetType::DestructureBlockLet,
        ))
    }

    fn head_of(fv: FreeVar<String>) -> RcExpr {
        app(bif("HEAD"), vec![var(fv)])
    }

    fn tail_of(fv: FreeVar<String>) -> RcExpr {
        app(bif("TAIL"), vec![var(fv)])
    }

    fn head_tail_of(fv: FreeVar<String>, tail_count: usize) -> RcExpr {
        let mut expr = var(fv);
        for _ in 0..tail_count {
            expr = app(bif("TAIL"), vec![expr]);
        }
        app(bif("HEAD"), vec![expr])
    }

    fn destruct_list_let(
        p0_fv: FreeVar<String>,
        elems: &[&str], // binding names for each position
        body: RcExpr,
    ) -> RcExpr {
        let bindings: Vec<(Binder<String>, Embed<RcExpr>)> = elems
            .iter()
            .enumerate()
            .map(|(i, name)| {
                let binding_fv = free(name);
                (Binder(binding_fv), Embed(head_tail_of(p0_fv.clone(), i)))
            })
            .collect();
        RcExpr::from(Expr::Let(
            Smid::default(),
            Scope::new(Rec::new(bindings), body),
            LetType::DestructureListLet,
        ))
    }

    #[test]
    fn test_no_fusion_without_block_arg() {
        // App(Lam([p0], Let(DestructureBlockLet, ...)), [non-block]) — should not fuse
        let p0 = free("__p0");
        let x = free("x");

        let lam_body = destruct_block_let(p0.clone(), &[("x", "x")], var(x));
        let input = app(lam(vec![p0], lam_body), vec![num(42)]);

        let result = destructure_fusion(&input).unwrap();
        // Should not fuse — 42 is not a Block
        assert!(matches!(&*result.inner, Expr::App(_, _, _)));
    }

    #[test]
    fn test_block_fusion_basic() {
        // sum-xy({x y}): x + y  applied to  {x: 3, y: 4}
        // → Let([x = 3, y = 4], x + y)
        let p0 = free("__p0");
        let x = free("x");
        let y = free("y");

        let body = app(bif("+"), vec![var(x.clone()), var(y.clone())]);
        let lam_body = destruct_block_let(p0.clone(), &[("x", "x"), ("y", "y")], body);
        let f = lam(vec![p0], lam_body);

        let block_arg = block(vec![("x".to_string(), num(3)), ("y".to_string(), num(4))]);

        let input = app(f, vec![block_arg]);
        let result = destructure_fusion(&input).unwrap();

        // Result should be a Let (not an App)
        assert!(
            matches!(&*result.inner, Expr::Let(_, _, LetType::OtherLet)),
            "Expected Let(OtherLet), got: {:?}",
            &*result.inner
        );

        // Binding check
        binding::verify(&result).expect("binding check failed after block fusion");
    }

    #[test]
    fn test_block_fusion_field_missing() {
        // Block literal is missing a field that the destructure expects — no fusion
        let p0 = free("__p0");
        let x = free("x");
        let z = free("z");

        // Destructures x and z, but block only has x
        let body = app(bif("+"), vec![var(x.clone()), var(z.clone())]);
        let lam_body = destruct_block_let(p0.clone(), &[("x", "x"), ("z", "z")], body);
        let f = lam(vec![p0], lam_body);

        let block_arg = block(vec![("x".to_string(), num(3))]); // no "z" field

        let input = app(f, vec![block_arg]);
        let result = destructure_fusion(&input).unwrap();

        // Should not fuse (z field missing)
        assert!(matches!(&*result.inner, Expr::App(_, _, _)));
    }

    #[test]
    fn test_list_fusion_basic() {
        // add-pair([a, b]): a + b  applied to  [10, 20]
        // → Let([a = 10, b = 20], a + b)
        let p0 = free("__p0");
        let a = free("a");
        let b = free("b");

        let body = app(bif("+"), vec![var(a.clone()), var(b.clone())]);
        let lam_body = destruct_list_let(p0.clone(), &["a", "b"], body);
        let f = lam(vec![p0], lam_body);

        let list_arg = list(vec![num(10), num(20)]);

        let input = app(f, vec![list_arg]);
        let result = destructure_fusion(&input).unwrap();

        // Result should be a Let (not an App)
        assert!(
            matches!(&*result.inner, Expr::Let(_, _, LetType::OtherLet)),
            "Expected Let(OtherLet), got: {:?}",
            &*result.inner
        );

        // Binding check
        binding::verify(&result).expect("binding check failed after list fusion");
    }

    #[test]
    fn test_list_fusion_out_of_bounds() {
        // List literal has fewer elements than destructure expects — no fusion
        let p0 = free("__p0");
        let a = free("a");
        let b = free("b");

        let body = app(bif("+"), vec![var(a.clone()), var(b.clone())]);
        // Destructures [a, b, c] but literal only has [10, 20]
        let lam_body = destruct_list_let(p0.clone(), &["a", "b", "c"], body);
        let f = lam(vec![p0], lam_body);

        let list_arg = list(vec![num(10), num(20)]);

        let input = app(f, vec![list_arg]);
        let result = destructure_fusion(&input).unwrap();

        // Should not fuse (c out of bounds)
        assert!(matches!(&*result.inner, Expr::App(_, _, _)));
    }

    #[test]
    fn test_no_fusion_for_variable_arg() {
        // App(Lam([p0], DestructureBlockLet), [Var(x)]) — arg is a variable, not literal
        let p0 = free("__p0");
        let x = free("x");
        let y = free("y");

        let body = var(y.clone());
        let lam_body = destruct_block_let(p0.clone(), &[("y", "y")], body);
        let f = lam(vec![p0], lam_body);

        // Arg is a variable, not a block literal
        let input = app(f, vec![var(x)]);
        let result = destructure_fusion(&input).unwrap();

        // Should not fuse
        assert!(matches!(&*result.inner, Expr::App(_, _, _)));
    }

    #[test]
    fn test_head_tail_not_fused() {
        // Head/tail pattern [a : xs] — tail binding cannot be fused (no sub-list construction)
        // Destructure: a = HEAD(__p0), xs = TAIL(__p0)
        // The TAIL binding does not match the HEAD(TAIL^n) pattern, so fusion fails
        let p0 = free("__p0");
        let a = free("a");
        let xs = free("xs");

        let body = var(a.clone());

        // Bindings: a = HEAD(__p0), xs = TAIL(__p0)
        let bindings = vec![
            (Binder(a.clone()), Embed(head_of(p0.clone()))),
            (Binder(xs.clone()), Embed(tail_of(p0.clone()))),
        ];
        let lam_body = RcExpr::from(Expr::Let(
            Smid::default(),
            Scope::new(Rec::new(bindings), body),
            LetType::DestructureListLet,
        ));

        let f = lam(vec![p0], lam_body);
        let list_arg = list(vec![num(1), num(2), num(3)]);

        let input = app(f, vec![list_arg]);
        let result = destructure_fusion(&input).unwrap();

        // Should not fuse (xs = TAIL(__p0) is not a HEAD pattern)
        assert!(matches!(&*result.inner, Expr::App(_, _, _)));
    }
}
