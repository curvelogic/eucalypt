//! Tighten up compiled STG by removing superfluous allocations etc.

use std::{convert::TryInto, rc::Rc};

use super::{
    syntax::{dsl, LambdaForm, Ref, StgSyn},
    tags::DataConstructor,
};

pub trait IndexTransformation {
    fn apply(&self, r: &Ref, outer: &dyn Fn(&Ref) -> Ref) -> Ref;
}

/// A transformation of indexes to apply when eliminating a
/// superfluous let
pub struct LetIndexTransformation {
    mapping: Vec<usize>,
}

impl LetIndexTransformation {
    /// If the bindings are all local refs, return and equivalent
    /// index transformation
    pub fn from_let_bindings(bindings: &[LambdaForm]) -> Option<Self> {
        let mut mappings = vec![];

        for b in bindings {
            match b {
                LambdaForm::Thunk { body } | LambdaForm::Value { body } => {
                    if let StgSyn::Atom {
                        evaluand: Ref::L(n),
                    } = **body
                    {
                        mappings.push(n);
                    } else {
                        return None;
                    }
                }
                _ => {
                    return None;
                }
            }
        }

        Some(LetIndexTransformation { mapping: mappings })
    }
}

impl IndexTransformation for LetIndexTransformation {
    /// Apply the transformation to a Ref, updating locals and leaving
    /// globals and values intact
    fn apply(&self, r: &Ref, outer: &dyn Fn(&Ref) -> Ref) -> Ref {
        match r {
            Ref::L(n) => match self.mapping.get(*n) {
                Some(i) => Ref::L(*i),
                None => outer(&Ref::L(*n - self.mapping.len())),
            },
            x => x.clone(),
        }
    }
}

/// Unconditionally bump all local indices by a fixed offset.
///
/// Used when inserting a scope binding that the body does not
/// reference (e.g. converting a 0-arity branch to a force fallback).
pub struct BumpIndexTransformation {
    delta: usize,
}

impl BumpIndexTransformation {
    pub fn new(delta: usize) -> Self {
        Self { delta }
    }
}

impl IndexTransformation for BumpIndexTransformation {
    fn apply(&self, r: &Ref, outer: &dyn Fn(&Ref) -> Ref) -> Ref {
        if let Ref::L(n) = r {
            outer(&Ref::L(*n)).bump(self.delta)
        } else {
            r.clone()
        }
    }
}

/// Modify transformations when going inside case or lambda which
/// shunt indexes by one or the arity respectively
pub struct ShiftIndexTransformation {
    shift: usize,
}

impl ShiftIndexTransformation {
    pub fn new(shift: usize) -> Self {
        Self { shift }
    }
}

impl IndexTransformation for ShiftIndexTransformation {
    fn apply(&self, r: &Ref, outer: &dyn Fn(&Ref) -> Ref) -> Ref {
        if let Ref::L(n) = r {
            if *n < self.shift {
                r.clone()
            } else {
                outer(&Ref::L(*n - self.shift)).bump(self.shift)
            }
        } else {
            r.clone()
        }
    }
}

/// Tree walker to prune needless allocations
#[derive(Default)]
pub struct AllocationPruner {
    transform_stack: Vec<Box<dyn IndexTransformation>>,
}

impl AllocationPruner {
    pub fn transform(&self, r: &Ref) -> Ref {
        let f: Box<dyn Fn(&Ref) -> Ref> = self
            .transform_stack
            .iter()
            .fold(Box::new(|r: &Ref| -> Ref { r.clone() }), |e, t| {
                Box::new(move |r: &Ref| -> Ref { t.apply(r, e.as_ref()) })
            });

        f(r)
    }

    /// Apply transformations to let bindings
    fn transform_bindings(&mut self, bindings: &[LambdaForm]) -> Vec<LambdaForm> {
        let mut new_bindings = vec![];
        for b in bindings {
            let transformed = match b {
                LambdaForm::Lambda {
                    bound,
                    body,
                    annotation,
                } => {
                    self.transform_stack
                        .push(Box::new(ShiftIndexTransformation::new(*bound as usize)));
                    let body = self.apply(body.clone());
                    self.transform_stack.pop();
                    LambdaForm::Lambda {
                        bound: *bound,
                        body,
                        annotation: *annotation,
                    }
                }
                LambdaForm::Thunk { body } => LambdaForm::Thunk {
                    body: self.apply(body.clone()),
                },
                LambdaForm::Value { body } => LambdaForm::Value {
                    body: self.apply(body.clone()),
                },
            };

            new_bindings.push(transformed);
        }
        new_bindings
    }

    pub fn apply(&mut self, stg: Rc<StgSyn>) -> Rc<StgSyn> {
        match &*stg {
            StgSyn::Atom { evaluand } => dsl::atom(self.transform(evaluand)),
            StgSyn::Let { bindings, body } => {
                match LetIndexTransformation::from_let_bindings(bindings) {
                    Some(t) => {
                        // we can strip	the let
                        self.transform_stack.push(Box::new(t));
                        let body = self.apply(body.clone());
                        self.transform_stack.pop();
                        body
                    }
                    None => {
                        let bindings = self.transform_bindings(bindings);
                        self.transform_stack
                            .push(Box::new(ShiftIndexTransformation::new(bindings.len())));
                        let body = self.apply(body.clone());
                        self.transform_stack.pop();
                        Rc::new(StgSyn::Let { bindings, body })
                    }
                }
            }
            StgSyn::LetRec { bindings, body } => {
                match LetIndexTransformation::from_let_bindings(bindings) {
                    Some(t) => {
                        // we can strip	the let
                        self.transform_stack.push(Box::new(t));
                        self.transform_stack
                            .push(Box::new(ShiftIndexTransformation::new(bindings.len())));
                        let body = self.apply(body.clone());
                        self.transform_stack.pop();
                        self.transform_stack.pop();
                        body
                    }
                    None => {
                        self.transform_stack
                            .push(Box::new(ShiftIndexTransformation::new(bindings.len())));
                        let bindings = self.transform_bindings(bindings);
                        let body = self.apply(body.clone());
                        self.transform_stack.pop();
                        Rc::new(StgSyn::LetRec { bindings, body })
                    }
                }
            }
            StgSyn::Case {
                scrutinee,
                branches,
                fallback,
            } => {
                // Single-branch case elimination for 0-arity constructors:
                // case scrutinee of { Tag -> body } (no fallback, arity 0)
                // becomes force(scrutinee, body') where body' has locals
                // bumped by 1 to account for force's fallback binding.
                if branches.len() == 1 && fallback.is_none() {
                    let (tag, branch_body) = &branches[0];
                    if let Ok(cons) = DataConstructor::try_from(*tag) {
                        if cons.arity() == 0 {
                            let scrutinee = self.apply(scrutinee.clone());
                            // Body originally has 0 new bindings (0-arity branch).
                            // Force fallback adds 1 binding (whole value) at local 0,
                            // so bump all existing local refs by 1.
                            self.transform_stack
                                .push(Box::new(BumpIndexTransformation::new(1)));
                            let body = self.apply(branch_body.clone());
                            self.transform_stack.pop();
                            return dsl::force(scrutinee, body);
                        }
                    }
                }

                let scrutinee = self.apply(scrutinee.clone());

                // Case-of-known-constructor: if scrutinee reduced to a
                // literal Cons, fold the case away entirely
                if let StgSyn::Cons { tag, args } = &*scrutinee {
                    // Find the matching branch
                    if let Some((_, branch_body)) = branches.iter().find(|(t, _)| t == tag) {
                        // Wrap branch body in a Let binding the constructor args
                        let bindings: Vec<LambdaForm> = args
                            .iter()
                            .map(|r| LambdaForm::value(dsl::atom(r.clone())))
                            .collect();
                        let result = if bindings.is_empty() {
                            branch_body.clone()
                        } else {
                            dsl::let_(bindings, branch_body.clone())
                        };
                        return self.apply(result);
                    } else if let Some(fb) = fallback {
                        // Fallback: bind the whole constructor as local 0
                        let result =
                            dsl::let_(vec![LambdaForm::value(scrutinee.clone())], fb.clone());
                        return self.apply(result);
                    }
                    // No branch and no fallback — fall through to
                    // reconstruct the Case (will error at runtime)
                }

                let branches = branches
                    .iter()
                    .map(|(t, b)| {
                        let cons: DataConstructor = (*t).try_into().unwrap();
                        self.transform_stack
                            .push(Box::new(ShiftIndexTransformation::new(cons.arity())));
                        let branch = self.apply(b.clone());
                        self.transform_stack.pop();
                        (*t, branch)
                    })
                    .collect();

                self.transform_stack
                    .push(Box::new(ShiftIndexTransformation::new(1)));
                let fallback = fallback.as_ref().map(|s| self.apply(s.clone()));
                self.transform_stack.pop();

                Rc::new(StgSyn::Case {
                    scrutinee,
                    branches,
                    fallback,
                })
            }
            StgSyn::Cons { tag, args } => Rc::new(StgSyn::Cons {
                tag: *tag,
                args: args.iter().map(|s| self.transform(s)).collect(),
            }),
            StgSyn::App { callable, args } => Rc::new(StgSyn::App {
                callable: self.transform(callable),
                args: args.iter().map(|s| self.transform(s)).collect(),
            }),
            StgSyn::Bif { intrinsic, args } => Rc::new(StgSyn::Bif {
                intrinsic: *intrinsic,
                args: args.iter().map(|s| self.transform(s)).collect(),
            }),
            StgSyn::Ann { smid, body } => Rc::new(StgSyn::Ann {
                smid: *smid,
                body: self.apply(body.clone()),
            }),
            StgSyn::Meta { meta, body } => Rc::new(StgSyn::Meta {
                meta: self.transform(meta),
                body: self.transform(body),
            }),
            StgSyn::DeMeta {
                scrutinee,
                handler,
                or_else,
            } => {
                let scrutinee = self.apply(scrutinee.clone());

                self.transform_stack
                    .push(Box::new(ShiftIndexTransformation::new(2)));
                let handler = self.apply(handler.clone());
                self.transform_stack.pop();

                self.transform_stack
                    .push(Box::new(ShiftIndexTransformation::new(1)));
                let or_else = self.apply(or_else.clone());
                self.transform_stack.pop();

                Rc::new(StgSyn::DeMeta {
                    scrutinee,
                    handler,
                    or_else,
                })
            }
            _ => stg.clone(),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::eval::stg::syntax::dsl;
    use crate::eval::stg::tags::DataConstructor;

    #[test]
    pub fn test_simple() {
        let original = dsl::let_(
            vec![dsl::value(dsl::local(0)), dsl::value(dsl::local(1))],
            dsl::local(1),
        );

        let pruned = AllocationPruner::default().apply(original);

        let expected = dsl::local(1);

        assert_eq!(pruned, expected);
    }

    #[test]
    pub fn test_permutation_let() {
        let original = dsl::let_(
            vec![dsl::value(dsl::local(1)), dsl::value(dsl::local(0))],
            dsl::local(1),
        );

        let pruned = AllocationPruner::default().apply(original);

        let expected = dsl::local(0);

        assert_eq!(pruned, expected);
    }

    #[test]
    pub fn test_deeper_ref() {
        let original = dsl::let_(
            vec![dsl::value(dsl::local(0)), dsl::value(dsl::local(1))],
            dsl::local(2),
        );

        let pruned = AllocationPruner::default().apply(original);

        let expected = dsl::local(0);

        assert_eq!(pruned, expected);
    }

    #[test]
    pub fn test_nested_lets() {
        let original = dsl::let_(
            vec![dsl::value(dsl::local(0)), dsl::value(dsl::local(1))],
            dsl::let_(vec![dsl::value(dsl::local(1))], dsl::local(0)),
        );

        let pruned = AllocationPruner::default().apply(original);

        let expected = dsl::local(1);

        assert_eq!(pruned, expected);
    }

    #[test]
    pub fn test_refs_wrapped_in_app() {
        let original = dsl::let_(
            vec![dsl::value(dsl::local(0)), dsl::value(dsl::local(1))],
            dsl::app(dsl::lref(4), vec![dsl::lref(1), dsl::lref(0)]),
        );

        let pruned = AllocationPruner::default().apply(original);

        let expected = dsl::app(dsl::lref(2), vec![dsl::lref(1), dsl::lref(0)]);

        assert_eq!(pruned, expected);
    }

    #[test]
    pub fn test_case_of_known_ctor_matching_branch() {
        use crate::eval::stg::tags::DataConstructor;

        // case (BoolTrue) of { BoolTrue -> local(0); BoolFalse -> local(1) }
        // should fold to local(0) (0-arity, no let needed)
        let original = dsl::switch(
            dsl::t(),
            vec![
                (DataConstructor::BoolTrue.tag(), dsl::local(0)),
                (DataConstructor::BoolFalse.tag(), dsl::local(1)),
            ],
        );

        let pruned = AllocationPruner::default().apply(original);
        assert_eq!(pruned, dsl::local(0));
    }

    #[test]
    pub fn test_case_of_known_ctor_with_args() {
        use crate::eval::stg::tags::DataConstructor;

        // case (BoxedNumber(native_42)) of { BoxedNumber -> local(0) }
        // should fold to: let [value(atom(native_42))] in local(0)
        let original = dsl::switch(
            dsl::box_num(42),
            vec![(DataConstructor::BoxedNumber.tag(), dsl::local(0))],
        );

        let pruned = AllocationPruner::default().apply(original);
        let expected = dsl::let_(vec![dsl::value(dsl::atom(dsl::num(42)))], dsl::local(0));
        assert_eq!(pruned, expected);
    }

    #[test]
    pub fn test_case_of_known_ctor_with_local_args() {
        use crate::eval::stg::tags::DataConstructor;

        // case (ListCons(local(0), local(1))) of { ListCons -> local(1) }
        // should fold to: let [value(atom(local(0))), value(atom(local(1)))] in local(1)
        // which the let-pruner reduces to local(1)
        let original = dsl::switch(
            dsl::data(
                DataConstructor::ListCons.tag(),
                vec![dsl::lref(0), dsl::lref(1)],
            ),
            vec![(DataConstructor::ListCons.tag(), dsl::local(1))],
        );

        let pruned = AllocationPruner::default().apply(original);
        let expected = dsl::local(1);
        assert_eq!(pruned, expected);
    }

    #[test]
    pub fn test_case_of_known_ctor_fallback() {
        use crate::eval::stg::tags::DataConstructor;

        // case (BoolTrue) of { BoolFalse -> ...; _ -> local(0) }
        // BoolTrue has no matching branch, falls to fallback
        // which binds the whole scrutinee as local 0
        let original = dsl::case(
            dsl::t(),
            vec![(DataConstructor::BoolFalse.tag(), dsl::local(0))],
            dsl::local(0), // fallback: use the scrutinee
        );

        let pruned = AllocationPruner::default().apply(original);
        // Should become: let [value(t())] in local(0)
        // which the let-pruner cannot eliminate (t() is not a simple ref)
        let expected = dsl::let_(vec![dsl::value(dsl::t())], dsl::local(0));
        assert_eq!(pruned, expected);
    }

    #[test]
    pub fn test_single_branch_zero_arity_becomes_force() {
        // case local(0) of { BoolTrue -> local(1) }
        // becomes force(local(0), local(2))
        // (local(1) bumped to local(2) since force adds 1 binding)
        let original = dsl::switch(
            dsl::local(0),
            vec![(DataConstructor::BoolTrue.tag(), dsl::local(1))],
        );

        let pruned = AllocationPruner::default().apply(original);

        let expected = dsl::force(dsl::local(0), dsl::local(2));

        assert_eq!(pruned, expected);
    }

    #[test]
    pub fn test_single_branch_zero_arity_body_local_zero() {
        // case local(0) of { ListNil -> local(0) }
        // becomes force(local(0), local(1))
        let original = dsl::switch(
            dsl::local(0),
            vec![(DataConstructor::ListNil.tag(), dsl::local(0))],
        );

        let pruned = AllocationPruner::default().apply(original);

        let expected = dsl::force(dsl::local(0), dsl::local(1));

        assert_eq!(pruned, expected);
    }

    #[test]
    pub fn test_single_branch_nonzero_arity_unchanged() {
        // case local(0) of { BoxedNumber(x) -> local(0) }
        // should NOT be transformed (arity 1)
        let original = dsl::switch(
            dsl::local(0),
            vec![(DataConstructor::BoxedNumber.tag(), dsl::local(0))],
        );

        let pruned = AllocationPruner::default().apply(original.clone());

        // Should remain a switch (unchanged)
        assert_eq!(pruned, original);
    }

    #[test]
    pub fn test_single_branch_with_fallback_unchanged() {
        // case local(0) of { BoolTrue -> local(0) ; _ -> local(0) }
        // has a fallback so should NOT be transformed
        let original = dsl::case(
            dsl::local(0),
            vec![(DataConstructor::BoolTrue.tag(), dsl::local(0))],
            dsl::local(0),
        );

        let pruned = AllocationPruner::default().apply(original.clone());

        // Should remain a case (unchanged)
        assert_eq!(pruned, original);
    }

    #[test]
    pub fn test_single_branch_multiple_branches_unchanged() {
        // case local(0) of { BoolTrue -> local(0) ; BoolFalse -> local(0) }
        // multiple branches, should NOT be transformed
        let original = dsl::switch(
            dsl::local(0),
            vec![
                (DataConstructor::BoolTrue.tag(), dsl::local(0)),
                (DataConstructor::BoolFalse.tag(), dsl::local(0)),
            ],
        );

        let pruned = AllocationPruner::default().apply(original.clone());

        assert_eq!(pruned, original);
    }

    #[test]
    pub fn test_single_branch_zero_arity_with_app() {
        // case local(0) of { Unit -> app(local(1), [local(2)]) }
        // becomes force(local(0), app(local(2), [local(3)]))
        let original = dsl::switch(
            dsl::local(0),
            vec![(
                DataConstructor::Unit.tag(),
                dsl::app(dsl::lref(1), vec![dsl::lref(2)]),
            )],
        );

        let pruned = AllocationPruner::default().apply(original);

        let expected = dsl::force(dsl::local(0), dsl::app(dsl::lref(2), vec![dsl::lref(3)]));

        assert_eq!(pruned, expected);
    }
}
