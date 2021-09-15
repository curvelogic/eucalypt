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
                let scrutinee = self.apply(scrutinee.clone());
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
}
