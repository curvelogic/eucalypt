//! Destructure fusion pass
//!
//! After the inline pass beta-reduces destructuring lambdas at their call
//! sites, the resulting expressions contain static patterns such as
//! `Lookup(Block{...}, "key")` or `App(HEAD, [List[...]])`. This pass folds
//! those patterns to their values, eliminating the intermediate block/list
//! construction entirely.
//!
//! The pass is idempotent and purely structural — it does not require
//! variable-scope tracking. It is safe to run after the inline pass.
use crate::core::error::CoreError;
use crate::core::expr::*;

/// Fold static `Lookup(Block{...}, key, fallback)` to the block value, the
/// fallback, or leave untouched.
///
/// Only folds when the target is a literal `Block` expression. If the key is
/// present the value is returned; if the key is absent and a fallback is
/// provided the fallback is returned; otherwise the original `Lookup` is left
/// in place.
fn fold_block_lookup(expr: &RcExpr) -> Option<RcExpr> {
    if let Expr::Lookup(_, target, key, fallback) = &*expr.inner {
        if let Expr::Block(_, block_map) = &*target.inner {
            if let Some(val) = block_map.get(key.as_str()) {
                return Some(val.clone());
            } else if let Some(fb) = fallback {
                return Some(fb.clone());
            }
        }
    }
    None
}

/// Fold `App(Intrinsic("HEAD"), [List[v0, v1, ...]])` → `v0`.
///
/// Only folds when the list is a non-empty literal `List` expression.
fn fold_head_call(expr: &RcExpr) -> Option<RcExpr> {
    if let Expr::App(_, f, xs) = &*expr.inner {
        if let Expr::Intrinsic(_, name) = &*f.inner {
            if name == "HEAD" && xs.len() == 1 {
                if let Expr::List(_, elements) = &*xs[0].inner {
                    if let Some(first) = elements.first() {
                        return Some(first.clone());
                    }
                }
            }
        }
    }
    None
}

/// Fold `App(Intrinsic("TAIL"), [List[v0, v1, ...]])` → `List[v1, ...]`.
///
/// Only folds when the argument is a literal `List` expression. The tail of an
/// empty list is left unreduced (that would be a runtime error anyway).
fn fold_tail_call(expr: &RcExpr) -> Option<RcExpr> {
    if let Expr::App(_, f, xs) = &*expr.inner {
        if let Expr::Intrinsic(_, name) = &*f.inner {
            if name == "TAIL" && xs.len() == 1 {
                if let Expr::List(s, elements) = &*xs[0].inner {
                    if !elements.is_empty() {
                        let tail: Vec<RcExpr> = elements[1..].to_vec();
                        return Some(RcExpr::from(Expr::List(*s, tail)));
                    }
                }
            }
        }
    }
    None
}

/// Recursively apply all fusion reductions to an expression.
///
/// The walk is bottom-up: children are reduced first, then the node itself.
pub fn fuse(expr: &RcExpr) -> Result<RcExpr, CoreError> {
    // Recursively reduce children first
    let reduced = expr.try_walk_safe(&mut fuse)?;

    // Apply static folding rules to the (already reduced) node
    if let Some(folded) = fold_block_lookup(&reduced)
        .or_else(|| fold_head_call(&reduced))
        .or_else(|| fold_tail_call(&reduced))
    {
        // The folded result may itself be reducible — apply once more
        fuse(&folded)
    } else {
        Ok(reduced)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::expr::acore::*;

    #[test]
    fn test_fold_block_lookup_present() {
        // Lookup(Block{x: 3, y: 4}, "x") → 3
        let b = block([("x".to_string(), num(3)), ("y".to_string(), num(4))]);
        let expr = lookup(b, "x", None);
        let result = fuse(&expr).unwrap();
        assert_eq!(result, num(3));
    }

    #[test]
    fn test_fold_block_lookup_missing_no_fallback() {
        // Lookup(Block{x: 3}, "z") — no fallback, leave untouched
        let b = block([("x".to_string(), num(3))]);
        let expr = lookup(b.clone(), "z", None);
        let result = fuse(&expr).unwrap();
        // Should be unchanged: key missing and no fallback
        assert_eq!(result, lookup(b, "z", None));
    }

    #[test]
    fn test_fold_block_lookup_missing_with_fallback() {
        // Lookup(Block{x: 3}, "z", Some(99)) → 99
        let b = block([("x".to_string(), num(3))]);
        let expr = lookup(b, "z", Some(num(99)));
        let result = fuse(&expr).unwrap();
        assert_eq!(result, num(99));
    }

    #[test]
    fn test_fold_head_list() {
        // HEAD([10, 20, 30]) → 10
        let l = list(vec![num(10), num(20), num(30)]);
        let expr = app(bif("HEAD"), vec![l]);
        let result = fuse(&expr).unwrap();
        assert_eq!(result, num(10));
    }

    #[test]
    fn test_fold_tail_list() {
        // TAIL([10, 20, 30]) → [20, 30]
        let l = list(vec![num(10), num(20), num(30)]);
        let expr = app(bif("TAIL"), vec![l]);
        let result = fuse(&expr).unwrap();
        assert_eq!(result, list(vec![num(20), num(30)]));
    }

    #[test]
    fn test_fold_tail_then_head() {
        // HEAD(TAIL([10, 20, 30])) → 20
        let l = list(vec![num(10), num(20), num(30)]);
        let tail_call = app(bif("TAIL"), vec![l]);
        let head_of_tail = app(bif("HEAD"), vec![tail_call]);
        let result = fuse(&head_of_tail).unwrap();
        assert_eq!(result, num(20));
    }

    #[test]
    fn test_fold_nested_in_let() {
        // let x = HEAD([1,2,3]) in x  →  let x = 1 in x
        let x = free("x");
        let l = list(vec![num(1), num(2), num(3)]);
        let head_call = app(bif("HEAD"), vec![l]);
        let expr = let_(vec![(x.clone(), head_call)], var(x.clone()));
        let result = fuse(&expr).unwrap();
        let expected = let_(vec![(x.clone(), num(1))], var(x));
        assert_eq!(result, expected);
    }
}
