//! Logic for filling out expressions with implicit anaphora
use crate::common::sourcemap::{HasSmid, Smid};
use crate::core::anaphora;
use crate::core::expr::*;
use std::collections::HashSet;

pub type AnaphorSet = HashSet<Anaphor<Smid, i32>>;

/// Fill implied gaps with expression anaphora and keep tabs on
/// whether there are any naked anaphora amongs the `xs`.
pub fn fill_gaps(xs: &[RcExpr]) -> (Vec<RcExpr>, AnaphorSet) {
    let mut out = Vec::new();
    let mut naked_anaphora = HashSet::new();

    if let Some(prefix) = filler(None, Some(&xs[0])) {
        naked_anaphora.extend(anaphora::naked_anaphora(&prefix));
        out.push(prefix);
    }

    for window in xs.windows(2) {
        if let [ref l, ref r] = window {
            naked_anaphora.extend(anaphora::naked_anaphora(&l));
            out.push(l.clone());
            if let Some(fill) = filler(Some(l), Some(r)) {
                naked_anaphora.extend(anaphora::naked_anaphora(&fill));
                out.push(fill.clone());
            }
        }
    }

    if let Some(end) = xs.last() {
        naked_anaphora.extend(anaphora::naked_anaphora(&end));
        out.push(end.clone());
        if let Some(suffix) = filler(Some(end), None) {
            naked_anaphora.extend(anaphora::naked_anaphora(&suffix));
            out.push(suffix);
        }
    }

    (out, naked_anaphora)
}

/// Whether a side of an atom is operator like or value like
enum BindSide {
    OpLike,
    ValueLike,
}

/// Return the bind behaviour of each side of an expression (or
/// the start or end of an expression, represented by None).
fn bind_sides(expr: Option<&RcExpr>) -> (BindSide, BindSide) {
    use BindSide::*;
    if let Some(expr) = expr {
        match &*expr.inner {
            Expr::Operator(_, Fixity::UnaryPrefix, _, _) => (ValueLike, OpLike),
            Expr::Operator(_, Fixity::UnaryPostfix, _, _) => (OpLike, ValueLike),
            Expr::Operator(_, _, _, _) => (OpLike, OpLike),
            _ => (ValueLike, ValueLike),
        }
    } else {
        (OpLike, OpLike)
    }
}

/// Return an appropriate filler expression to go between `left`
/// and `right`
pub fn filler(left: Option<&RcExpr>, right: Option<&RcExpr>) -> Option<RcExpr> {
    match (bind_sides(left).1, bind_sides(right).0) {
        (BindSide::ValueLike, BindSide::ValueLike) => Some(core::cat()),
        (BindSide::OpLike, BindSide::OpLike) => {
            let rsmid = match right {
                Some(r) => r.smid(),
                _ => Smid::default(),
            };
            let lsmid = match left {
                Some(l) => l.smid(),
                _ => Smid::default(),
            };
            if rsmid != Smid::default() {
                Some(core::section_anaphor_left(rsmid))
            } else if lsmid != Smid::default() {
                Some(core::section_anaphor_right(lsmid))
            } else {
                panic!("no SMID for implicit anaphor")
            }
        }
        _ => None,
    }
}

#[cfg(test)]
pub mod tests {

    use super::*;
    use crate::core::expr::acore::*;
    use crate::core::expr::core;
    use crate::core::expr::RcExpr;
    use moniker::assert_term_eq;

    fn fill(exprs: &[RcExpr]) -> Vec<RcExpr> {
        fill_gaps(exprs).0
    }

    #[test]
    pub fn test_cat() {
        let x = free("x");
        let f = free("f");
        assert_term_eq!(
            soup(fill(&[var(x.clone()), var(f.clone())])),
            soup(vec![var(x), cat(), var(f)])
        );
    }

    #[test]
    pub fn test_sections() {
        let pre10 = core::prefix(Smid::fake(1), 10, bif("PRE10"));
        let post10 = core::postfix(Smid::fake(2), 10, bif("POST10"));
        let l50 = core::infixl(Smid::fake(3), 50, bif("L50"));
        let x = free("x");

        // nb. term_eq does not verify the anaphora

        assert_term_eq!(
            soup(fill(&[l50.clone(), var(x.clone())])),
            soup(vec![
                core::section_anaphor_left(Smid::fake(1)),
                l50.clone(),
                var(x.clone())
            ])
        );

        assert_term_eq!(
            soup(fill(&[var(x.clone()), l50.clone()])),
            soup(vec![
                var(x),
                l50.clone(),
                core::section_anaphor_right(Smid::fake(3))
            ])
        );

        assert_term_eq!(
            soup(fill(&[pre10.clone(), l50.clone(), post10.clone()])),
            soup(vec![
                pre10,
                core::section_anaphor_right(Smid::fake(3)),
                l50,
                core::section_anaphor_right(Smid::fake(2)),
                post10
            ])
        );
    }

    #[test]
    pub fn test_anaphora() {
        let plus = infixl(50, bif("__ADD"));
        let x = free("x");

        let ana = core::expr_anaphor(Smid::fake(10), None);

        assert_term_eq!(
            soup(fill(&[ana.clone(), plus.clone(), var(x.clone())])),
            soup(vec![ana.clone(), plus.clone(), var(x.clone())])
        );
    }
}
