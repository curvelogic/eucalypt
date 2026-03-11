//! Logic for resolving operator precedences
use crate::common::sourcemap::HasSmid;
use crate::core::error::CoreError;
use crate::core::expr::*;

/// Apply the shunting yard algorithm to exprs and return cooked
/// expression
pub fn shunt(exprs: Vec<RcExpr>) -> Result<RcExpr, CoreError> {
    Shunter::new(exprs).shunt()
}

/// Shunting algorithm state
///
/// Input stream, output stream, ops stack and error state.
struct Shunter {
    input: Vec<RcExpr>,
    output: Vec<RcExpr>,
    ops: Vec<RcExpr>,
    error: Option<CoreError>,
}

impl Shunter {
    pub fn new(mut exprs: Vec<RcExpr>) -> Self {
        exprs.reverse();
        Shunter {
            input: exprs,
            output: vec![],
            ops: vec![],
            error: None,
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
                if let Some(result) = self.output.pop() {
                    Ok(result)
                } else {
                    panic!("shunting ended with empty output")
                }
            }
        }
    }

    fn pop_next(&mut self) -> Option<RcExpr> {
        self.input.pop()
    }

    fn peek_next(&mut self) -> Option<&RcExpr> {
        self.input.last()
    }

    fn push_back(&mut self, expr: RcExpr) {
        self.input.push(expr);
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

    fn pop_one_output(&mut self) -> Option<RcExpr> {
        self.output.pop()
    }

    fn pop_two_output(&mut self) -> Option<(RcExpr, RcExpr)> {
        if self.output.len() >= 2 {
            let a = self.pop_one_output().unwrap();
            let b = self.pop_one_output().unwrap();
            Some((a, b))
        } else {
            None
        }
    }

    fn push_output(&mut self, expr: RcExpr) {
        self.output.push(expr)
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
            Expr::Operator(_, f, _, expr) => match f.arity() {
                0 => self.apply_zero(expr.clone()),
                1 => self.apply_one(expr.clone()),
                2 => self.apply_two(expr.clone()),
                _ => unreachable!(),
            },
            _ => panic!("apply_op applied to non-operator"),
        }
    }

    fn apply_zero(&mut self, expr: RcExpr) {
        self.push_output(expr)
    }

    fn apply_one(&mut self, expr: RcExpr) {
        if let Some(operand) = self.pop_one_output() {
            self.push_output(Self::form_apply(expr, &[operand]))
        } else {
            self.error = Some(CoreError::TooFewOperands(expr.smid()))
        }
    }

    fn apply_two(&mut self, expr: RcExpr) {
        if let Some((right, left)) = self.pop_two_output() {
            self.push_output(Self::form_apply(expr, &[left, right]))
        } else {
            self.error = Some(CoreError::TooFewOperands(expr.smid()))
        }
    }

    fn form_apply(op: RcExpr, xs: &[RcExpr]) -> RcExpr {
        if op.is_pseudocall() {
            let f = xs[0].clone();
            match &*xs[1].inner {
                Expr::ArgTuple(_, args) => RcExpr::from(Expr::App(f.smid(), f, args.to_vec())),
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
        if let Some(fill) = super::fill::filler(None, self.peek_next()) {
            self.push_back(fill);
        }
    }

    /// Check the upcoming expr in case we need to insert an anaphor
    fn insert_fill(&mut self, lhs: &RcExpr) {
        if let Some(fill) = super::fill::filler(Some(lhs), self.peek_next()) {
            self.push_back(fill);
        }
    }

    /// One step of the shunting algo
    fn shunt1(&mut self) {
        if let Some(expr) = self.pop_next() {
            self.insert_fill(&expr);
            if expr.inner.is_operator() {
                self.seat_op(expr);
            } else {
                self.push_output(expr);
            }
        } else {
            self.clear_ops();
        }
    }
}
