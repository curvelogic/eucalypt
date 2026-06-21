//! Closure flattening pass.
//!
//! Rewrites `Ref::L(n)` (de Bruijn flat indices) to `Ref::Local(i)` /
//! `Ref::Capture(i)` and builds capture recipes on every `LambdaForm`
//! and `Let`/`LetRec` node.
//!
//! At each frame boundary (Let, LetRec, or LambdaForm), refs into the
//! current frame become `Local(i)`, and refs that cross the boundary
//! become `Capture(i)` with a `CaptureInstruction` in the frame's
//! recipe.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::syntax::{CaptureInstruction, LambdaForm, Ref, StgSyn};

/// Per-frame capture tracking state.
struct CaptureBuilder {
    /// Number of locals in this frame.
    frame_size: usize,
    /// Maps outer-relative de Bruijn index → capture slot.
    map: HashMap<usize, u16>,
    /// The recipe being built.
    recipe: Vec<CaptureInstruction>,
}

impl CaptureBuilder {
    fn new(frame_size: usize) -> Self {
        CaptureBuilder {
            frame_size,
            map: HashMap::new(),
            recipe: Vec::new(),
        }
    }
}

/// Stack of frame boundaries using interior mutability so we can
/// push/pop without borrow conflicts.
struct FrameStack {
    frames: Vec<RefCell<CaptureBuilder>>,
}

impl FrameStack {
    fn new() -> Self {
        FrameStack { frames: Vec::new() }
    }

    fn push(&mut self, builder: CaptureBuilder) {
        self.frames.push(RefCell::new(builder));
    }

    fn pop(&mut self) -> Vec<CaptureInstruction> {
        self.frames
            .pop()
            .expect("frame stack underflow")
            .into_inner()
            .recipe
    }

    /// Resolve a `Ref::L(n)` against the frame stack.
    ///
    /// The last element is the innermost frame.  Walk outward through
    /// frames, registering captures at each level.
    fn resolve(&self, n: usize) -> Ref {
        if self.frames.is_empty() {
            return Ref::L(n);
        }
        self.resolve_at(n, self.frames.len() - 1)
    }

    /// Resolve at a specific frame level (0 = outermost).
    fn resolve_at(&self, n: usize, level: usize) -> Ref {
        let mut frame = self.frames[level].borrow_mut();
        if n < frame.frame_size {
            return Ref::Local(n as u16);
        }

        let outer_idx = n - frame.frame_size;

        // Already captured?
        if let Some(&cap_idx) = frame.map.get(&outer_idx) {
            return Ref::Capture(cap_idx);
        }

        // Determine capture instruction by resolving in the
        // enclosing frame.
        let instruction = if level == 0 {
            // Outermost tracked frame — no further tracking.
            CaptureInstruction::CaptureLocal(outer_idx as u16)
        } else {
            // Temporarily drop the borrow so we can recurse.
            drop(frame);
            let enclosing_ref = self.resolve_at(outer_idx, level - 1);
            frame = self.frames[level].borrow_mut();
            match enclosing_ref {
                Ref::Local(i) => CaptureInstruction::CaptureLocal(i),
                Ref::Capture(i) => CaptureInstruction::CopyCapture(i),
                _ => CaptureInstruction::CaptureLocal(outer_idx as u16),
            }
        };

        let cap_idx = frame.recipe.len() as u16;
        frame.recipe.push(instruction);
        frame.map.insert(outer_idx, cap_idx);
        Ref::Capture(cap_idx)
    }

    fn flatten_ref(&self, r: &Ref) -> Ref {
        match r {
            Ref::L(n) => self.resolve(*n),
            _ => r.clone(),
        }
    }
}

/// Flatten a compiled STG tree: rewrite `Ref::L` to `Local`/`Capture`
/// and populate capture recipes.
pub fn flatten_closures(syn: &Rc<StgSyn>) -> Rc<StgSyn> {
    let mut stack = FrameStack::new();
    flatten_syn(syn, &mut stack)
}

fn flatten_syn(syn: &Rc<StgSyn>, stack: &mut FrameStack) -> Rc<StgSyn> {
    match &**syn {
        StgSyn::Atom { evaluand } => Rc::new(StgSyn::Atom {
            evaluand: stack.flatten_ref(evaluand),
        }),

        StgSyn::Case {
            scrutinee,
            branches,
            fallback,
        } => {
            let scrutinee = flatten_syn(scrutinee, stack);
            let branches = branches
                .iter()
                .map(|(tag, body)| (*tag, flatten_syn(body, stack)))
                .collect();
            let fallback = fallback.as_ref().map(|fb| flatten_syn(fb, stack));
            Rc::new(StgSyn::Case {
                scrutinee,
                branches,
                fallback,
            })
        }

        StgSyn::Cons { tag, args } => Rc::new(StgSyn::Cons {
            tag: *tag,
            args: args.iter().map(|r| stack.flatten_ref(r)).collect(),
        }),

        StgSyn::App { callable, args } => Rc::new(StgSyn::App {
            callable: stack.flatten_ref(callable),
            args: args.iter().map(|r| stack.flatten_ref(r)).collect(),
        }),

        StgSyn::Bif { intrinsic, args } => Rc::new(StgSyn::Bif {
            intrinsic: *intrinsic,
            args: args.iter().map(|r| stack.flatten_ref(r)).collect(),
        }),

        StgSyn::Let { bindings, body, .. } => {
            let n = bindings.len();

            // Non-recursive: bindings see the current scope.
            let flat_bindings: Vec<LambdaForm> = bindings
                .iter()
                .map(|lf| flatten_lambda_form(lf, stack))
                .collect();

            // Body sees a new frame with `n` locals.
            stack.push(CaptureBuilder::new(n));
            let flat_body = flatten_syn(body, stack);
            let recipe = stack.pop();

            Rc::new(StgSyn::Let {
                bindings: flat_bindings,
                body: flat_body,
                capture_recipe: recipe,
            })
        }

        StgSyn::LetRec { bindings, body, .. } => {
            let n = bindings.len();

            // Recursive: bindings AND body see the new frame.
            stack.push(CaptureBuilder::new(n));

            let flat_bindings: Vec<LambdaForm> = bindings
                .iter()
                .map(|lf| flatten_lambda_form(lf, stack))
                .collect();

            let flat_body = flatten_syn(body, stack);
            let recipe = stack.pop();

            Rc::new(StgSyn::LetRec {
                bindings: flat_bindings,
                body: flat_body,
                capture_recipe: recipe,
            })
        }

        StgSyn::Ann { smid, body } => Rc::new(StgSyn::Ann {
            smid: *smid,
            body: flatten_syn(body, stack),
        }),

        StgSyn::Meta { meta, body } => Rc::new(StgSyn::Meta {
            meta: stack.flatten_ref(meta),
            body: stack.flatten_ref(body),
        }),

        StgSyn::DeMeta {
            scrutinee,
            handler,
            or_else,
        } => Rc::new(StgSyn::DeMeta {
            scrutinee: flatten_syn(scrutinee, stack),
            handler: flatten_syn(handler, stack),
            or_else: flatten_syn(or_else, stack),
        }),

        StgSyn::Seq { scrutinee, body } => Rc::new(StgSyn::Seq {
            scrutinee: flatten_syn(scrutinee, stack),
            body: flatten_syn(body, stack),
        }),

        StgSyn::BlackHole => syn.clone(),
    }
}

fn flatten_lambda_form(lf: &LambdaForm, stack: &mut FrameStack) -> LambdaForm {
    match lf {
        LambdaForm::Lambda {
            bound,
            body,
            annotation,
            ..
        } => {
            stack.push(CaptureBuilder::new(*bound as usize));
            let flat_body = flatten_syn(body, stack);
            let recipe = stack.pop();
            LambdaForm::Lambda {
                bound: *bound,
                body: flat_body,
                annotation: *annotation,
                capture_recipe: recipe,
            }
        }
        LambdaForm::Thunk { body, .. } => {
            stack.push(CaptureBuilder::new(0));
            let flat_body = flatten_syn(body, stack);
            let recipe = stack.pop();
            LambdaForm::Thunk {
                body: flat_body,
                capture_recipe: recipe,
            }
        }
        LambdaForm::Value { body, .. } => {
            stack.push(CaptureBuilder::new(0));
            let flat_body = flatten_syn(body, stack);
            let recipe = stack.pop();
            LambdaForm::Value {
                body: flat_body,
                capture_recipe: recipe,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::stg::syntax::dsl;
    use crate::eval::stg::syntax::Native;

    fn num(n: i64) -> Ref {
        Ref::V(Native::Num(n.into()))
    }

    #[test]
    fn test_let_body_local_ref() {
        let syn = Rc::new(StgSyn::Let {
            bindings: vec![dsl::value(dsl::atom(num(10)))],
            body: dsl::atom(Ref::L(0)),
            capture_recipe: vec![],
        });
        let flat = flatten_closures(&syn);
        if let StgSyn::Let { body, .. } = &*flat {
            assert_eq!(
                **body,
                StgSyn::Atom {
                    evaluand: Ref::Local(0)
                }
            );
        } else {
            panic!("expected Let");
        }
    }

    #[test]
    fn test_nested_let_capture() {
        // Let([val(10), val(20)],
        //   Let([val(30)],
        //     Atom(L(1))))  -- crosses inner let → outer's local 0
        let inner = Rc::new(StgSyn::Let {
            bindings: vec![dsl::value(dsl::atom(num(30)))],
            body: dsl::atom(Ref::L(1)),
            capture_recipe: vec![],
        });
        let syn = Rc::new(StgSyn::Let {
            bindings: vec![
                dsl::value(dsl::atom(num(10))),
                dsl::value(dsl::atom(num(20))),
            ],
            body: inner,
            capture_recipe: vec![],
        });
        let flat = flatten_closures(&syn);
        if let StgSyn::Let {
            body: outer_body, ..
        } = &*flat
        {
            if let StgSyn::Let {
                body: inner_body,
                capture_recipe,
                ..
            } = &**outer_body
            {
                assert_eq!(
                    **inner_body,
                    StgSyn::Atom {
                        evaluand: Ref::Capture(0)
                    }
                );
                assert_eq!(capture_recipe.len(), 1);
                assert_eq!(capture_recipe[0], CaptureInstruction::CaptureLocal(0));
            } else {
                panic!("expected inner Let");
            }
        }
    }

    #[test]
    fn test_lambda_capture_recipe() {
        let lf = LambdaForm::Lambda {
            bound: 2,
            body: dsl::atom(Ref::L(2)),
            annotation: crate::common::sourcemap::Smid::default(),
            capture_recipe: vec![],
        };
        let mut stack = FrameStack::new();
        let flat = flatten_lambda_form(&lf, &mut stack);
        if let LambdaForm::Lambda {
            body,
            capture_recipe,
            ..
        } = &flat
        {
            assert_eq!(
                **body,
                StgSyn::Atom {
                    evaluand: Ref::Capture(0)
                }
            );
            assert_eq!(capture_recipe.len(), 1);
        } else {
            panic!("expected Lambda");
        }
    }

    #[test]
    fn test_thunk_all_captures() {
        let lf = LambdaForm::Thunk {
            body: dsl::atom(Ref::L(0)),
            capture_recipe: vec![],
        };
        let mut stack = FrameStack::new();
        let flat = flatten_lambda_form(&lf, &mut stack);
        if let LambdaForm::Thunk {
            body,
            capture_recipe,
        } = &flat
        {
            assert_eq!(
                **body,
                StgSyn::Atom {
                    evaluand: Ref::Capture(0)
                }
            );
            assert_eq!(capture_recipe.len(), 1);
        }
    }

    #[test]
    fn test_dedup_captures() {
        let lf = LambdaForm::Lambda {
            bound: 1,
            body: Rc::new(StgSyn::App {
                callable: Ref::L(1),
                args: vec![Ref::L(1)],
            }),
            annotation: crate::common::sourcemap::Smid::default(),
            capture_recipe: vec![],
        };
        let mut stack = FrameStack::new();
        let flat = flatten_lambda_form(&lf, &mut stack);
        if let LambdaForm::Lambda {
            body,
            capture_recipe,
            ..
        } = &flat
        {
            if let StgSyn::App { callable, args } = &**body {
                assert_eq!(*callable, Ref::Capture(0));
                assert_eq!(args[0], Ref::Capture(0));
            }
            assert_eq!(capture_recipe.len(), 1);
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
    fn test_lambda_with_inner_let() {
        // Lambda(bound=1):
        //   Let([val(10)]):
        //     Atom(L(1))  -- crosses let → lambda local 0
        let inner_let = Rc::new(StgSyn::Let {
            bindings: vec![dsl::value(dsl::atom(num(10)))],
            body: dsl::atom(Ref::L(1)),
            capture_recipe: vec![],
        });
        let lf = LambdaForm::Lambda {
            bound: 1,
            body: inner_let,
            annotation: crate::common::sourcemap::Smid::default(),
            capture_recipe: vec![],
        };
        let mut stack = FrameStack::new();
        let flat = flatten_lambda_form(&lf, &mut stack);
        if let LambdaForm::Lambda {
            body,
            capture_recipe: lambda_recipe,
            ..
        } = &flat
        {
            // Lambda has no captures (the let is inside it)
            assert!(lambda_recipe.is_empty());

            if let StgSyn::Let {
                body: let_body,
                capture_recipe: let_recipe,
                ..
            } = &**body
            {
                assert_eq!(
                    **let_body,
                    StgSyn::Atom {
                        evaluand: Ref::Capture(0)
                    }
                );
                assert_eq!(let_recipe.len(), 1);
                assert_eq!(let_recipe[0], CaptureInstruction::CaptureLocal(0));
            }
        }
    }

    #[test]
    fn test_three_level_copy_capture() {
        // L0: Let([val(99)]):
        //   L1: Let([val(88)]):
        //     L2: Let([val(77)]):
        //       Atom(L(2))  -- crosses L2(1), L1(1), reaches L0 local 0
        //
        // L2 captures from L1 via CopyCapture(0)
        // L1 captures from L0 via CaptureLocal(0)
        let l2 = Rc::new(StgSyn::Let {
            bindings: vec![dsl::value(dsl::atom(num(77)))],
            body: dsl::atom(Ref::L(2)),
            capture_recipe: vec![],
        });
        let l1 = Rc::new(StgSyn::Let {
            bindings: vec![dsl::value(dsl::atom(num(88)))],
            body: l2,
            capture_recipe: vec![],
        });
        let l0 = Rc::new(StgSyn::Let {
            bindings: vec![dsl::value(dsl::atom(num(99)))],
            body: l1,
            capture_recipe: vec![],
        });
        let flat = flatten_closures(&l0);

        if let StgSyn::Let {
            body: b0,
            capture_recipe: r0,
            ..
        } = &*flat
        {
            assert!(r0.is_empty());
            if let StgSyn::Let {
                body: b1,
                capture_recipe: r1,
                ..
            } = &**b0
            {
                assert_eq!(r1.len(), 1);
                assert_eq!(r1[0], CaptureInstruction::CaptureLocal(0));
                if let StgSyn::Let {
                    body: b2,
                    capture_recipe: r2,
                    ..
                } = &**b1
                {
                    assert_eq!(
                        **b2,
                        StgSyn::Atom {
                            evaluand: Ref::Capture(0)
                        }
                    );
                    assert_eq!(r2.len(), 1);
                    assert_eq!(r2[0], CaptureInstruction::CopyCapture(0));
                }
            }
        }
    }

    #[test]
    fn test_letrec_self_ref() {
        // LetRec([thunk(Atom(L(0)))], Atom(L(0)))
        // L(0) in both binding and body → Local(0)
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
            // Body: L(0) → Local(0)
            assert_eq!(
                **body,
                StgSyn::Atom {
                    evaluand: Ref::Local(0)
                }
            );
            assert!(capture_recipe.is_empty());

            // Binding thunk body: L(0) → Capture(0)
            // because the thunk's frame has 0 locals, so L(0) crosses
            // to the letrec frame's local 0
            if let LambdaForm::Thunk {
                body: thunk_body,
                capture_recipe: thunk_recipe,
            } = &bindings[0]
            {
                assert_eq!(
                    **thunk_body,
                    StgSyn::Atom {
                        evaluand: Ref::Capture(0)
                    }
                );
                assert_eq!(thunk_recipe.len(), 1);
                assert_eq!(thunk_recipe[0], CaptureInstruction::CaptureLocal(0));
            }
        }
    }
}
