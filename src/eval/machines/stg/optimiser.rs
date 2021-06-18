//! Tighten up compiled STG by removing superfluous allocations etc.

use std::rc::Rc;

use super::syntax::{dsl, LambdaForm, Ref, StgSyn};

pub trait IndexTransformation {
    fn apply(&self, r: &Ref) -> Ref;
}

/// A transformation of indexes
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
    fn apply(&self, r: &Ref) -> Ref {
        match r {
            Ref::L(n) => Ref::L(
                self.mapping
                    .get(*n)
                    .cloned()
                    .unwrap_or_else(|| *n - self.mapping.len()),
            ),
            x => x.clone(),
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
        let mut r = r.clone();
        for t in self.transform_stack.iter().rev() {
            r = t.apply(&r);
        }
        r
    }

    pub fn apply(&mut self, stg: Rc<StgSyn>) -> Rc<StgSyn> {
        match &*stg {
            StgSyn::Atom { evaluand } => dsl::atom(self.transform(evaluand)),
            StgSyn::LetRec { bindings, body } | StgSyn::Let { bindings, body } => {
                if let Some(t) = LetIndexTransformation::from_let_bindings(&bindings) {
                    self.transform_stack.push(Box::new(t));
                }
                self.apply(body.clone())
            }
            StgSyn::Case {
                scrutinee,
                branches,
                fallback,
            } => Rc::new(StgSyn::Case {
                scrutinee: self.apply(scrutinee.clone()),
                branches: branches
                    .iter()
                    .map(|(t, b)| (*t, self.apply(b.clone())))
                    .collect(),
                fallback: fallback.as_ref().map(|s| self.apply(s.clone())),
            }),
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
            } => Rc::new(StgSyn::DeMeta {
                scrutinee: self.apply(scrutinee.clone()),
                handler: self.apply(handler.clone()),
                or_else: self.apply(or_else.clone()),
            }),
            _ => stg.clone(),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::eval::machines::stg::syntax::dsl;

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
