//! Closure flattening pass (transitional).
//!
//! This pass walks the compiled STG tree and adds empty
//! `capture_recipe` fields to every `LambdaForm` and `Let`/`LetRec`
//! node.  In the current transitional phase, `Ref::L` refs are NOT
//! rewritten to `Local`/`Capture` — the runtime still uses the
//! parent-chain (cactus stack) model for all environment lookups.
//!
//! When the runtime supports fully flat closures (no parent chain),
//! this pass will be extended to:
//! 1. Track frame boundaries (Let/LetRec/Lambda frame sizes).
//! 2. Rewrite within-frame `L(i)` → `Local(i)`.
//! 3. Rewrite cross-frame `L(i)` → `Capture(j)` with appropriate
//!    `CaptureInstruction` entries in the recipe.
//!
//! The Case runtime frame (remapped shared-backing) must also be
//! accounted for before ref rewriting can be enabled.

use std::rc::Rc;

use super::syntax::{LambdaForm, StgSyn};

/// Flatten a compiled STG tree: ensure every frame boundary carries
/// a (currently empty) capture recipe.
pub fn flatten_closures(syn: &Rc<StgSyn>) -> Rc<StgSyn> {
    flatten_syn(syn)
}

fn flatten_syn(syn: &Rc<StgSyn>) -> Rc<StgSyn> {
    match &**syn {
        StgSyn::Atom { .. } => syn.clone(),

        StgSyn::Case {
            scrutinee,
            branches,
            fallback,
        } => {
            let scrutinee = flatten_syn(scrutinee);
            let branches = branches
                .iter()
                .map(|(tag, body)| (*tag, flatten_syn(body)))
                .collect();
            let fallback = fallback.as_ref().map(flatten_syn);
            Rc::new(StgSyn::Case {
                scrutinee,
                branches,
                fallback,
            })
        }

        StgSyn::Cons { .. } | StgSyn::App { .. } | StgSyn::Bif { .. } => syn.clone(),

        StgSyn::Let { bindings, body, .. } => {
            let flat_bindings: Vec<LambdaForm> = bindings.iter().map(flatten_lambda_form).collect();
            let flat_body = flatten_syn(body);
            Rc::new(StgSyn::Let {
                bindings: flat_bindings,
                body: flat_body,
                capture_recipe: vec![],
            })
        }

        StgSyn::LetRec { bindings, body, .. } => {
            let flat_bindings: Vec<LambdaForm> = bindings.iter().map(flatten_lambda_form).collect();
            let flat_body = flatten_syn(body);
            Rc::new(StgSyn::LetRec {
                bindings: flat_bindings,
                body: flat_body,
                capture_recipe: vec![],
            })
        }

        StgSyn::Ann { smid, body } => Rc::new(StgSyn::Ann {
            smid: *smid,
            body: flatten_syn(body),
        }),

        StgSyn::Meta { .. } => syn.clone(),

        StgSyn::DeMeta {
            scrutinee,
            handler,
            or_else,
        } => Rc::new(StgSyn::DeMeta {
            scrutinee: flatten_syn(scrutinee),
            handler: flatten_syn(handler),
            or_else: flatten_syn(or_else),
        }),

        StgSyn::Seq { scrutinee, body } => Rc::new(StgSyn::Seq {
            scrutinee: flatten_syn(scrutinee),
            body: flatten_syn(body),
        }),

        StgSyn::BlackHole => syn.clone(),
    }
}

fn flatten_lambda_form(lf: &LambdaForm) -> LambdaForm {
    match lf {
        LambdaForm::Lambda {
            bound,
            body,
            annotation,
            ..
        } => LambdaForm::Lambda {
            bound: *bound,
            body: flatten_syn(body),
            annotation: *annotation,
            capture_recipe: vec![],
        },
        LambdaForm::Thunk { body, .. } => LambdaForm::Thunk {
            body: flatten_syn(body),
            capture_recipe: vec![],
        },
        LambdaForm::Value { body, .. } => LambdaForm::Value {
            body: flatten_syn(body),
            capture_recipe: vec![],
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::stg::syntax::dsl;
    use crate::eval::stg::syntax::{Native, Ref};

    fn num(n: i64) -> Ref {
        Ref::V(Native::Num(n.into()))
    }

    #[test]
    fn test_refs_unchanged() {
        // In transitional mode, all L refs stay as L.
        let syn = Rc::new(StgSyn::Let {
            bindings: vec![dsl::value(dsl::atom(num(10)))],
            body: dsl::atom(Ref::L(0)),
            capture_recipe: vec![],
        });
        let flat = flatten_closures(&syn);
        if let StgSyn::Let {
            body,
            capture_recipe,
            ..
        } = &*flat
        {
            assert_eq!(
                **body,
                StgSyn::Atom {
                    evaluand: Ref::L(0)
                }
            );
            assert!(capture_recipe.is_empty());
        } else {
            panic!("expected Let");
        }
    }

    #[test]
    fn test_globals_unchanged() {
        let syn = dsl::atom(Ref::G(5));
        let flat = flatten_closures(&syn);
        assert_eq!(
            *flat,
            StgSyn::Atom {
                evaluand: Ref::G(5)
            }
        );
    }

    #[test]
    fn test_letrec_refs_unchanged() {
        let syn = Rc::new(StgSyn::LetRec {
            bindings: vec![dsl::thunk(dsl::atom(Ref::L(0)))],
            body: dsl::atom(Ref::L(0)),
            capture_recipe: vec![],
        });
        let flat = flatten_closures(&syn);
        if let StgSyn::LetRec {
            bindings,
            body,
            capture_recipe,
        } = &*flat
        {
            assert_eq!(
                **body,
                StgSyn::Atom {
                    evaluand: Ref::L(0)
                }
            );
            assert!(capture_recipe.is_empty());

            if let LambdaForm::Thunk {
                body: thunk_body,
                capture_recipe: thunk_recipe,
            } = &bindings[0]
            {
                assert_eq!(
                    **thunk_body,
                    StgSyn::Atom {
                        evaluand: Ref::L(0)
                    }
                );
                assert!(thunk_recipe.is_empty());
            }
        }
    }

    #[test]
    fn test_all_form_variants_get_empty_recipe() {
        let syn = Rc::new(StgSyn::Let {
            bindings: vec![
                dsl::value(dsl::atom(num(1))),
                dsl::thunk(dsl::atom(Ref::L(0))),
                LambdaForm::Lambda {
                    bound: 1,
                    body: dsl::atom(Ref::L(0)),
                    annotation: crate::common::sourcemap::Smid::default(),
                    capture_recipe: vec![],
                },
            ],
            body: dsl::atom(Ref::L(0)),
            capture_recipe: vec![],
        });
        let flat = flatten_closures(&syn);
        if let StgSyn::Let {
            bindings,
            capture_recipe,
            ..
        } = &*flat
        {
            assert!(capture_recipe.is_empty());
            for lf in bindings {
                match lf {
                    LambdaForm::Value { capture_recipe, .. }
                    | LambdaForm::Thunk { capture_recipe, .. }
                    | LambdaForm::Lambda { capture_recipe, .. } => {
                        assert!(capture_recipe.is_empty());
                    }
                }
            }
        }
    }
}
