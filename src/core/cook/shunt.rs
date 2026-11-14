//! Logic for resolving operator precedences
use crate::common::sourcemap::{CatenationOperand, HasSmid, Smid, SourceMap};
use crate::core::error::CoreError;
use crate::core::expr::*;

/// Apply the shunting yard algorithm to exprs and return cooked
/// expression
///
/// `source_map` is the map the exprs' `Smid`s index into; it is used to
/// mint whole-call spans for the applications formed here
/// (see [`Shunter::form_apply`]).
///
/// `extents` holds, for each of `exprs`, the atom as it was written,
/// before it was cooked: a parenthesised group cooks to an application
/// stamped with its operator's `Smid`, but as written it is the whole
/// group. Extents are used only to record catenations whose
/// function operand is an unparenthesised operator expression
/// (eu-oaz72.29).
pub fn shunt(
    exprs: Vec<RcExpr>,
    extents: Vec<Written>,
    source_map: &mut SourceMap,
) -> Result<RcExpr, CoreError> {
    Shunter::new(exprs, extents, source_map).shunt()
}

/// An atom as it was written, before cooking (eu-oaz72.29).
#[derive(Clone, Copy, Debug)]
pub struct Written {
    /// The atom's location.
    pub smid: Smid,
    /// Whether the atom is an expression anaphor (`_`, `_0`, …).
    pub anaphor: bool,
}

/// The source extent of an expression on the shunter's output stack, as
/// atom `Smid`s (eu-oaz72.29).
#[derive(Clone, Copy)]
struct Extent {
    /// The first atom of the expression.
    first: Smid,
    /// The last atom of the expression.
    last: Smid,
    /// The last atom of the expression's leftmost operand: the expression
    /// itself unless it is an infix-operator application.
    head_end: Smid,
    /// Whether the expression is an infix-operator application formed by
    /// this shunt — that is, written without parentheses round it. A
    /// parenthesised group is a nested soup, cooked before it gets here,
    /// so it arrives as a single atom.
    infix: bool,
    /// Whether an expression anaphor of this soup is among its atoms.
    anaphor: bool,
}

impl Written {
    fn operator(smid: Smid) -> Self {
        Written {
            smid,
            anaphor: false,
        }
    }
}

/// An expression on the output stack with its extent.
type Entry = (RcExpr, Extent);

impl Extent {
    fn atom(written: Written) -> Self {
        Extent {
            first: written.smid,
            last: written.smid,
            head_end: written.smid,
            infix: false,
            anaphor: written.anaphor,
        }
    }

    /// A leaf spanning the extents `left` to `right`.
    fn span(left: &Extent, right: &Extent) -> Self {
        Extent {
            first: left.first,
            last: right.last,
            head_end: right.last,
            infix: false,
            anaphor: left.anaphor || right.anaphor,
        }
    }
}

/// Shunting algorithm state
///
/// Input stream, output stream, ops stack and error state.
struct Shunter<'smap> {
    input: Vec<(RcExpr, Written)>,
    output: Vec<(RcExpr, Extent)>,
    ops: Vec<RcExpr>,
    error: Option<CoreError>,
    source_map: &'smap mut SourceMap,
}

impl<'smap> Shunter<'smap> {
    pub fn new(
        exprs: Vec<RcExpr>,
        extents: Vec<Written>,
        source_map: &'smap mut SourceMap,
    ) -> Self {
        debug_assert_eq!(exprs.len(), extents.len());
        let mut input: Vec<(RcExpr, Written)> = exprs.into_iter().zip(extents).collect();
        input.reverse();
        Shunter {
            input,
            output: vec![],
            ops: vec![],
            error: None,
            source_map,
        }
    }

    pub fn is_complete(&self) -> bool {
        self.input.is_empty() && self.ops.is_empty()
    }

    pub fn shunt(mut self) -> Result<RcExpr, CoreError> {
        if !self.is_complete() {
            self.insert_initial_fill();
        }

        while !self.is_complete() {
            self.shunt1();
        }

        match self.error {
            Some(e) => Err(e),
            None => {
                if let Some((result, _)) = self.output.pop() {
                    Ok(result)
                } else {
                    panic!("shunting ended with empty output")
                }
            }
        }
    }

    fn pop_next(&mut self) -> Option<(RcExpr, Written)> {
        self.input.pop()
    }

    fn peek_next(&mut self) -> Option<&RcExpr> {
        self.input.last().map(|(e, _)| e)
    }

    /// Push back a filler synthesised by `fill`: it was not written, so
    /// its extent is its own `Smid`.
    fn push_back(&mut self, expr: RcExpr) {
        let written = Written {
            smid: expr.smid(),
            anaphor: false,
        };
        self.input.push((expr, written));
    }

    fn push_op(&mut self, op: RcExpr) {
        self.ops.push(op)
    }

    fn drop_op(&mut self) {
        self.ops.pop();
    }

    fn peek_op(&self) -> Option<&RcExpr> {
        self.ops.last()
    }

    fn pop_one_output(&mut self) -> Option<(RcExpr, Extent)> {
        self.output.pop()
    }

    fn pop_two_output(&mut self) -> Option<(Entry, Entry)> {
        if self.output.len() >= 2 {
            let a = self.pop_one_output().unwrap();
            let b = self.pop_one_output().unwrap();
            Some((a, b))
        } else {
            None
        }
    }

    fn push_output(&mut self, expr: RcExpr, extent: Extent) {
        self.output.push((expr, extent))
    }

    /// Seat the operator comfortably at the top of the operator
    /// stack, applying as many as need to be appiled to make it
    /// sit comfortably with regard to precedence.
    fn seat_op(&mut self, op: RcExpr) {
        if let Expr::Operator(_, this_fixity, this_prec, _) = &*op.inner {
            // Nullary operators apply immediately - they don't consume operands
            if *this_fixity == Fixity::Nullary {
                self.apply_op(op);
                return;
            }

            let top = self.peek_op().cloned();

            match top {
                None => self.push_op(op),
                Some(top) => match &*top.inner {
                    Expr::Operator(_, top_fixity, top_prec, _) => {
                        if *top_fixity == Fixity::InfixLeft {
                            if this_prec <= top_prec {
                                self.drop_op();
                                self.apply_op(top.clone());
                                self.seat_op(op);
                            } else {
                                self.push_op(op);
                            }
                        } else if *top_fixity == Fixity::InfixRight {
                            if this_prec < top_prec {
                                self.drop_op();
                                self.apply_op(top.clone());
                                self.seat_op(op);
                            } else {
                                self.push_op(op);
                            }
                        } else if *top_fixity == Fixity::UnaryPostfix {
                            self.drop_op();
                            self.apply_op(top.clone());
                            self.seat_op(op);
                        } else if *this_fixity == Fixity::UnaryPostfix {
                            if this_prec < top_prec {
                                self.drop_op();
                                self.apply_op(top.clone());
                                self.seat_op(op);
                            } else {
                                self.apply_op(op);
                            }
                        } else if this_prec < top_prec {
                            self.drop_op();
                            self.apply_op(top.clone());
                            self.seat_op(op);
                        } else {
                            self.push_op(op);
                        }
                    }
                    _ => panic!("non-operator on op stack in seat_op"),
                },
            }
        } else {
            panic!("non-operator passed to seat_op");
        }
    }

    /// Clear and apply remaining
    fn clear_ops(&mut self) {
        while let Some(top) = self.peek_op().cloned() {
            self.drop_op();
            self.apply_op(top.clone());
        }
    }

    fn apply_op(&mut self, op: RcExpr) {
        match &*op.inner {
            Expr::Operator(smid, f, _, expr) => match f.arity() {
                0 => self.apply_zero(expr.clone(), *smid),
                1 => self.apply_one(expr.clone(), *smid, *f),
                2 => self.apply_two(expr.clone()),
                _ => unreachable!(),
            },
            _ => panic!("apply_op applied to non-operator"),
        }
    }

    fn apply_zero(&mut self, expr: RcExpr, op_smid: Smid) {
        self.push_output(expr, Extent::atom(Written::operator(op_smid)))
    }

    fn apply_one(&mut self, expr: RcExpr, op_smid: Smid, fixity: Fixity) {
        if let Some((operand, ext)) = self.pop_one_output() {
            let op = Extent::atom(Written::operator(op_smid));
            let extent = if fixity == Fixity::UnaryPostfix {
                Extent::span(&ext, &op)
            } else {
                Extent::span(&op, &ext)
            };
            let applied = self.form_apply(expr, &[operand]);
            self.push_output(applied, extent)
        } else {
            self.error = Some(CoreError::TooFewOperands(expr.smid()))
        }
    }

    fn apply_two(&mut self, expr: RcExpr) {
        if let Some(((right, rext), (left, lext))) = self.pop_two_output() {
            let extent = if expr.is_pseudocall() || expr.is_pseudodot() {
                Extent::span(&lext, &rext)
            } else if expr.is_pseudocat() {
                if rext.infix {
                    // `x a op b`: the operator expression `a op b`, written
                    // without parentheses, became the pipeline step applied
                    // to `x` (eu-oaz72.29). The catenation's application is
                    // stamped with the operator application's `Smid`
                    // (`form_apply`), which is where a runtime error from
                    // calling its result, or from the operator receiving a
                    // function, is raised.
                    self.source_map.record_catenation_operand(
                        right.smid(),
                        CatenationOperand {
                            pipeline: (lext.first, lext.last),
                            step: (rext.first, rext.last),
                            step_head_end: rext.head_end,
                            pipeline_has_anaphor: lext.anaphor,
                        },
                    );
                }
                Extent::span(&lext, &rext)
            } else {
                Extent {
                    first: lext.first,
                    last: rext.last,
                    head_end: lext.head_end,
                    infix: true,
                    anaphor: lext.anaphor || rext.anaphor,
                }
            };
            let applied = self.form_apply(expr, &[left, right]);
            self.push_output(applied, extent)
        } else {
            self.error = Some(CoreError::TooFewOperands(expr.smid()))
        }
    }

    /// Build the expression an operator application denotes.
    ///
    /// For a juxtaposed call `f(a, b)` the resulting `App` is stamped with a
    /// `Smid` spanning the callee *and* the argument tuple, rather than the
    /// callee alone (eu-1tkk.7.38). The callee here is whatever precedence
    /// resolution decided it is — `a.b` in `a.b(x)`, `f(a)` in `f(a)(b)` —
    /// so the merged span is the whole call however it was written, and a
    /// diagnostic blaming an under-applied call underlines all of it.
    fn form_apply(&mut self, op: RcExpr, xs: &[RcExpr]) -> RcExpr {
        if op.is_pseudocall() {
            let f = xs[0].clone();
            match &*xs[1].inner {
                Expr::ArgTuple(tuple_smid, args) => {
                    let smid = self.source_map.merge(f.smid(), *tuple_smid);
                    RcExpr::from(Expr::App(smid, f, args.to_vec()))
                }
                _ => panic!("shunt: pseudocall op without arg tuple"),
            }
        } else if op.is_pseudodot() {
            let target = xs[0].clone();
            match &*xs[1].inner {
                Expr::Name(_, name) => {
                    // straightforward name lookup
                    RcExpr::from(Expr::Lookup(target.smid(), target, name.clone(), None))
                }
                Expr::Lam(_, _, _) => {
                    // dynamic generalised lookup
                    RcExpr::from(Expr::App(target.smid(), xs[1].clone(), vec![target]))
                }
                _ => xs[1].clone(),
            }
        } else if op.is_pseudocat() {
            assert!(xs.len() == 2);
            let x = xs[0].clone();
            let f = xs[1].clone();
            RcExpr::from(Expr::App(f.smid(), f, vec![x]))
        } else {
            RcExpr::from(Expr::App(op.smid(), op, xs.to_vec()))
        }
    }

    /// Insert an initial fill if the first atom is not legal
    fn insert_initial_fill(&mut self) {
        match super::fill::filler(None, self.peek_next()) {
            Ok(Some(fill)) => self.push_back(fill),
            Ok(None) => {}
            Err(e) => self.error = Some(e),
        }
    }

    /// Check the upcoming expr in case we need to insert an anaphor
    fn insert_fill(&mut self, lhs: &RcExpr) {
        match super::fill::filler(Some(lhs), self.peek_next()) {
            Ok(Some(fill)) => self.push_back(fill),
            Ok(None) => {}
            Err(e) => self.error = Some(e),
        }
    }

    /// One step of the shunting algo
    fn shunt1(&mut self) {
        if let Some((expr, extent)) = self.pop_next() {
            self.insert_fill(&expr);
            if expr.inner.is_operator() {
                self.seat_op(expr);
            } else {
                self.push_output(expr, Extent::atom(extent));
            }
        } else {
            self.clear_ops();
        }
    }
}
