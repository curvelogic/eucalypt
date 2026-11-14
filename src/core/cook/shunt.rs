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
/// Each atom carries, with its cooked expression, the atom as it was
/// written, before it was cooked: a parenthesised group cooks to an
/// application stamped with its operator's `Smid`, but as written it is
/// the whole group. Extents are used only to record catenations whose
/// function operand is an unparenthesised operator expression
/// (eu-oaz72.29).
pub fn shunt(atoms: Vec<Atom>, source_map: &mut SourceMap) -> Result<RcExpr, CoreError> {
    Shunter::new(atoms, source_map).shunt()
}

/// A cooked atom with its extent as written.
pub type Atom = (RcExpr, Written);

/// What an atom, as written, can evaluate to (eu-oaz72.133).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Shape {
    /// A literal (number, string, symbol, list or block): never a function.
    Literal,
    /// A plain name: taken to be data.
    Name,
    /// Anything else, a call, an operator expression or an anaphor, which
    /// may be a function: `nth(0)` is a partial application.
    Other,
}

/// An atom as it was written, before cooking (eu-oaz72.29).
#[derive(Clone, Copy, Debug)]
pub struct Written {
    /// The atom's location.
    pub smid: Smid,
    /// Whether the atom is an expression anaphor (`_`, `_0`, …).
    pub anaphor: bool,
    /// What the atom can evaluate to.
    pub shape: Shape,
}

/// The source extent of an expression on the shunter's output stack, as
/// atom `Smid`s (eu-oaz72.29).
#[derive(Clone)]
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
    /// Whether an expression anaphor is among the atoms of the leftmost
    /// operand: the expression itself unless it is an infix application.
    head_anaphor: bool,
    /// The infix applications along the leftmost spine, outermost last:
    /// in `nth(0) + 1 = 2`, the `+` then the `=`. Each receives a value
    /// computed from the leftmost operand. Empty unless `infix`.
    ///
    /// The spine stops at the first application whose right operand is
    /// not a literal or a plain name (eu-oaz72.133): in `a + nth(0) = 1`
    /// the function `+` receives may be its right operand's, and then
    /// parenthesising the pipeline fixes nothing. Every application above
    /// that one may have received it too.
    spine: Vec<Smid>,
    /// Whether the spine may be extended: no application on it took a
    /// right operand that may be a function.
    spine_open: bool,
    /// The shape of the leftmost operand when that is a single atom;
    /// `Other` otherwise. Only `Literal` matters: a literal head cannot be
    /// the step's function, so no parenthesisation is a fix.
    head_shape: Shape,
    /// The shape of the expression when it is a single atom; `Other`
    /// otherwise.
    shape: Shape,
}

impl Written {
    fn operator(smid: Smid) -> Self {
        Written {
            smid,
            anaphor: false,
            shape: Shape::Other,
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
            head_anaphor: written.anaphor,
            spine: vec![],
            spine_open: true,
            head_shape: written.shape,
            shape: written.shape,
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
            head_anaphor: left.anaphor || right.anaphor,
            spine: vec![],
            spine_open: true,
            head_shape: Shape::Other,
            shape: Shape::Other,
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
    pub fn new(mut input: Vec<Atom>, source_map: &'smap mut SourceMap) -> Self {
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
        let written = Written::operator(expr.smid());
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
            let is_infix = !(expr.is_pseudocall() || expr.is_pseudodot() || expr.is_pseudocat());
            // A literal head cannot be the step's function (eu-oaz72.133):
            // `xs 1 + nth(0) = 1` would be no better parenthesised.
            if expr.is_pseudocat() && rext.infix && !lext.infix && rext.head_shape != Shape::Literal
            {
                // `x a op b`: the operator expression `a op b`, written
                // without parentheses, became the pipeline step applied to
                // `x` (eu-oaz72.29). The catenation's application is stamped
                // with the operator application's `Smid` (`form_apply`),
                // which is where a runtime error from calling its result is
                // raised; an error from an operator on the step's leftmost
                // spine receiving the step's function is raised at that
                // operator. A pipeline that is itself an operator expression
                // (`1 + x a op b`) is not recorded: the parse is
                // `(1 + x) (a op b)`, not a pipeline step on `x`.
                let operand = CatenationOperand {
                    pipeline: (lext.first, lext.last),
                    step: (rext.first, rext.last),
                    step_head_end: rext.head_end,
                    // Parentheses round the pipeline and the step's head
                    // would scope an anaphor in either to them.
                    pipeline_has_anaphor: lext.anaphor || rext.head_anaphor,
                };
                for op in &rext.spine {
                    self.source_map
                        .record_catenation_operand(*op, operand.clone());
                }
            }
            let applied = self.form_apply(expr, &[left, right]);
            let extent = if is_infix {
                let (mut spine, open) = if lext.infix {
                    (lext.spine, lext.spine_open)
                } else {
                    (vec![], true)
                };
                // The function may come from a right operand that is not
                // data (eu-oaz72.133): stop the spine here.
                let data = rext.shape != Shape::Other;
                let spine_open = open && data;
                if spine_open {
                    spine.push(applied.smid());
                }
                let head_shape = if lext.infix {
                    lext.head_shape
                } else {
                    lext.shape
                };
                Extent {
                    first: lext.first,
                    last: rext.last,
                    head_end: lext.head_end,
                    infix: true,
                    anaphor: lext.anaphor || rext.anaphor,
                    head_anaphor: lext.head_anaphor,
                    spine,
                    spine_open,
                    head_shape,
                    shape: Shape::Other,
                }
            } else {
                Extent::span(&lext, &rext)
            };
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
