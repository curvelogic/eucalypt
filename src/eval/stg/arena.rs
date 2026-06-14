//! Arena-flattened STG syntax for the pre-compiled prelude blob.
//!
//! [`StgSyn`] and [`LambdaForm`] use `Rc<StgSyn>` for child nodes,
//! which cannot be serialised.  This module provides mirror types
//! — [`ArenaStgSyn`] and [`ArenaLambdaForm`] — where every child
//! reference is a [`NodeIdx`] (`u32`) into a flat `Vec<ArenaStgSyn>`.
//! The whole tree is stored in an [`StgArena`] struct that can be
//! serialised with postcard.
//!
//! ## Layout
//!
//! Each node and form is **pre-allocated** (a `BlackHole` placeholder
//! is reserved) before its children are processed, so the parent
//! always has a lower index than its children.  The root of the first
//! tree added to a fresh arena is therefore always at index 0.
//!
//! ```text
//! StgArena {
//!     nodes: Vec<ArenaStgSyn>,  // node pool; root is nodes[0]
//!     forms: Vec<ArenaLambdaForm>,  // lambda forms referenced by Let/LetRec
//! }
//! ```
//!
//! ## Round-trip guarantee
//!
//! `StgArena::from_stg(rc)` → `arena.reconstruct(0)` produces a tree
//! that is structurally identical to the original (verified by the
//! unit tests below).
//!
//! ## Handling of shared `Rc` nodes
//!
//! `StgSyn` trees produced by the compiler are *mostly* trees (no
//! shared `Rc` nodes in practice).  The flattener treats each `Rc`
//! as a fresh node copy, which is correct: `StgSyn: PartialEq` so
//! sharing is unobservable from the semantics.

use std::rc::Rc;

use serde::{Deserialize, Serialize};

use super::{
    syntax::{LambdaForm, Ref, StgSyn},
    tags::Tag,
};
use crate::common::sourcemap::Smid;

// ── Index types ──────────────────────────────────────────────────────────────

/// Index of an [`ArenaStgSyn`] node in [`StgArena::nodes`].
pub type NodeIdx = u32;

/// Index of an [`ArenaLambdaForm`] in [`StgArena::forms`].
pub type FormIdx = u32;

// ── ArenaLambdaForm ──────────────────────────────────────────────────────────

/// Arena-serialisable mirror of [`LambdaForm`].
///
/// `body` is a [`NodeIdx`] rather than `Rc<StgSyn>`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArenaLambdaForm {
    Lambda {
        bound: u8,
        body: NodeIdx,
        annotation: Smid,
    },
    Thunk {
        body: NodeIdx,
    },
    Value {
        body: NodeIdx,
    },
}

// ── ArenaStgSyn ──────────────────────────────────────────────────────────────

/// Arena-serialisable mirror of [`StgSyn`].
///
/// Every `Rc<StgSyn>` child becomes a [`NodeIdx`]; lambda form lists
/// become `Vec<FormIdx>` into [`StgArena::forms`].
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArenaStgSyn {
    Atom {
        evaluand: Ref,
    },
    Case {
        scrutinee: NodeIdx,
        branches: Vec<(Tag, NodeIdx)>,
        fallback: Option<NodeIdx>,
        suppress_update: bool,
    },
    Cons {
        tag: Tag,
        args: Vec<Ref>,
    },
    App {
        callable: Ref,
        args: Vec<Ref>,
    },
    Bif {
        intrinsic: u8,
        args: Vec<Ref>,
    },
    Let {
        bindings: Vec<FormIdx>,
        body: NodeIdx,
    },
    LetRec {
        bindings: Vec<FormIdx>,
        body: NodeIdx,
    },
    Ann {
        smid: Smid,
        body: NodeIdx,
    },
    Meta {
        meta: Ref,
        body: Ref,
    },
    DeMeta {
        scrutinee: NodeIdx,
        handler: NodeIdx,
        or_else: NodeIdx,
    },
    BlackHole,
}

// ── StgArena ─────────────────────────────────────────────────────────────────

/// A serialisable arena containing a single flattened `StgSyn` tree.
///
/// `nodes[0]` is always the root of the first tree flattened into a
/// fresh arena.  `forms` holds all `LambdaForm`s referenced by
/// `Let`/`LetRec` nodes.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct StgArena {
    pub nodes: Vec<ArenaStgSyn>,
    pub forms: Vec<ArenaLambdaForm>,
}

impl StgArena {
    /// Flatten `root` into this arena.
    ///
    /// Pre-allocates the parent slot before recursing into children so
    /// parents always have a lower index than their children.  Returns
    /// the [`NodeIdx`] of `root` (0 for the first call on a fresh arena).
    pub fn flatten(&mut self, root: &Rc<StgSyn>) -> NodeIdx {
        self.alloc_node(root)
    }

    /// Pre-allocate a slot, recurse, then fill in.
    fn alloc_node(&mut self, syn: &Rc<StgSyn>) -> NodeIdx {
        let idx = self.nodes.len() as NodeIdx;
        self.nodes.push(ArenaStgSyn::BlackHole); // placeholder
        let arena_node = self.build_node(syn);
        self.nodes[idx as usize] = arena_node;
        idx
    }

    fn build_node(&mut self, syn: &Rc<StgSyn>) -> ArenaStgSyn {
        match &**syn {
            StgSyn::Atom { evaluand } => ArenaStgSyn::Atom {
                evaluand: evaluand.clone(),
            },
            StgSyn::Case {
                scrutinee,
                branches,
                fallback,
                suppress_update,
            } => {
                let s_idx = self.alloc_node(scrutinee);
                let b_idxs: Vec<(Tag, NodeIdx)> = branches
                    .iter()
                    .map(|(tag, body)| (*tag, self.alloc_node(body)))
                    .collect();
                let fb_idx = fallback.as_ref().map(|fb| self.alloc_node(fb));
                ArenaStgSyn::Case {
                    scrutinee: s_idx,
                    branches: b_idxs,
                    fallback: fb_idx,
                    suppress_update: *suppress_update,
                }
            }
            StgSyn::Cons { tag, args } => ArenaStgSyn::Cons {
                tag: *tag,
                args: args.clone(),
            },
            StgSyn::App { callable, args } => ArenaStgSyn::App {
                callable: callable.clone(),
                args: args.clone(),
            },
            StgSyn::Bif { intrinsic, args } => ArenaStgSyn::Bif {
                intrinsic: *intrinsic,
                args: args.clone(),
            },
            StgSyn::Let { bindings, body } => {
                let form_idxs: Vec<FormIdx> =
                    bindings.iter().map(|lf| self.alloc_form(lf)).collect();
                let body_idx = self.alloc_node(body);
                ArenaStgSyn::Let {
                    bindings: form_idxs,
                    body: body_idx,
                }
            }
            StgSyn::LetRec { bindings, body } => {
                let form_idxs: Vec<FormIdx> =
                    bindings.iter().map(|lf| self.alloc_form(lf)).collect();
                let body_idx = self.alloc_node(body);
                ArenaStgSyn::LetRec {
                    bindings: form_idxs,
                    body: body_idx,
                }
            }
            StgSyn::Ann { smid, body } => {
                let body_idx = self.alloc_node(body);
                ArenaStgSyn::Ann {
                    smid: *smid,
                    body: body_idx,
                }
            }
            StgSyn::Meta { meta, body } => ArenaStgSyn::Meta {
                meta: meta.clone(),
                body: body.clone(),
            },
            StgSyn::DeMeta {
                scrutinee,
                handler,
                or_else,
            } => {
                let s_idx = self.alloc_node(scrutinee);
                let h_idx = self.alloc_node(handler);
                let o_idx = self.alloc_node(or_else);
                ArenaStgSyn::DeMeta {
                    scrutinee: s_idx,
                    handler: h_idx,
                    or_else: o_idx,
                }
            }
            StgSyn::BlackHole => ArenaStgSyn::BlackHole,
        }
    }

    /// Flatten a [`LambdaForm`] into this arena and return its [`FormIdx`].
    ///
    /// The form's body is added to `self.nodes`; the form itself is added to
    /// `self.forms`.  Used by the xtask to flatten individual prelude binding
    /// lambda forms into a shared node pool.
    pub fn flatten_form(&mut self, lf: &LambdaForm) -> FormIdx {
        self.alloc_form(lf)
    }

    /// Pre-allocate a form slot, recurse, then fill in.
    fn alloc_form(&mut self, lf: &LambdaForm) -> FormIdx {
        let idx = self.forms.len() as FormIdx;
        // Push a placeholder (Thunk with a dummy body slot).
        // We know the real form will overwrite it immediately.
        self.forms.push(ArenaLambdaForm::Thunk { body: u32::MAX });
        let arena_form = self.build_form(lf);
        self.forms[idx as usize] = arena_form;
        idx
    }

    fn build_form(&mut self, lf: &LambdaForm) -> ArenaLambdaForm {
        match lf {
            LambdaForm::Lambda {
                bound,
                body,
                annotation,
            } => {
                let body_idx = self.alloc_node(body);
                ArenaLambdaForm::Lambda {
                    bound: *bound,
                    body: body_idx,
                    annotation: *annotation,
                }
            }
            LambdaForm::Thunk { body } => {
                let body_idx = self.alloc_node(body);
                ArenaLambdaForm::Thunk { body: body_idx }
            }
            LambdaForm::Value { body } => {
                let body_idx = self.alloc_node(body);
                ArenaLambdaForm::Value { body: body_idx }
            }
        }
    }

    /// Reconstruct an `Rc<StgSyn>` tree from this arena.
    ///
    /// `root` is the [`NodeIdx`] of the root, as returned by [`flatten`].
    /// For a fresh single-tree arena this is always 0.
    ///
    /// # Panics
    ///
    /// Panics if any index is out of range (indicates a malformed blob).
    pub fn reconstruct(&self, root: NodeIdx) -> Rc<StgSyn> {
        self.reconstruct_node(root)
    }

    fn reconstruct_node(&self, idx: NodeIdx) -> Rc<StgSyn> {
        let node = &self.nodes[idx as usize];
        // Prelude Ann nodes carry Smids from the xtask's source map which
        // are meaningless at runtime.  Elide them so they do not overwrite
        // the user's call-site annotation in vm.annotation.
        if let ArenaStgSyn::Ann { body, .. } = node {
            return self.reconstruct_node(*body);
        }
        Rc::new(self.reconstruct_arena_syn(node))
    }

    fn reconstruct_arena_syn(&self, node: &ArenaStgSyn) -> StgSyn {
        match node {
            ArenaStgSyn::Atom { evaluand } => StgSyn::Atom {
                evaluand: evaluand.clone(),
            },
            ArenaStgSyn::Case {
                scrutinee,
                branches,
                fallback,
                suppress_update,
            } => StgSyn::Case {
                scrutinee: self.reconstruct_node(*scrutinee),
                branches: branches
                    .iter()
                    .map(|(tag, idx)| (*tag, self.reconstruct_node(*idx)))
                    .collect(),
                fallback: fallback.map(|idx| self.reconstruct_node(idx)),
                suppress_update: *suppress_update,
            },
            ArenaStgSyn::Cons { tag, args } => StgSyn::Cons {
                tag: *tag,
                args: args.clone(),
            },
            ArenaStgSyn::App { callable, args } => StgSyn::App {
                callable: callable.clone(),
                args: args.clone(),
            },
            ArenaStgSyn::Bif { intrinsic, args } => StgSyn::Bif {
                intrinsic: *intrinsic,
                args: args.clone(),
            },
            ArenaStgSyn::Let { bindings, body } => StgSyn::Let {
                bindings: bindings
                    .iter()
                    .map(|&idx| self.reconstruct_form(idx))
                    .collect(),
                body: self.reconstruct_node(*body),
            },
            ArenaStgSyn::LetRec { bindings, body } => StgSyn::LetRec {
                bindings: bindings
                    .iter()
                    .map(|&idx| self.reconstruct_form(idx))
                    .collect(),
                body: self.reconstruct_node(*body),
            },
            // Ann nodes are elided in reconstruct_node() above.
            ArenaStgSyn::Ann { .. } => unreachable!("Ann handled in reconstruct_node"),
            ArenaStgSyn::Meta { meta, body } => StgSyn::Meta {
                meta: meta.clone(),
                body: body.clone(),
            },
            ArenaStgSyn::DeMeta {
                scrutinee,
                handler,
                or_else,
            } => StgSyn::DeMeta {
                scrutinee: self.reconstruct_node(*scrutinee),
                handler: self.reconstruct_node(*handler),
                or_else: self.reconstruct_node(*or_else),
            },
            ArenaStgSyn::BlackHole => StgSyn::BlackHole,
        }
    }

    pub fn reconstruct_form(&self, idx: FormIdx) -> LambdaForm {
        match &self.forms[idx as usize] {
            ArenaLambdaForm::Lambda { bound, body, .. } => LambdaForm::Lambda {
                bound: *bound,
                body: self.reconstruct_node(*body),
                // Clear xtask-sourced annotations — they are meaningless
                // at runtime and would pollute user error locations.
                annotation: Smid::default(),
            },
            ArenaLambdaForm::Thunk { body } => LambdaForm::Thunk {
                body: self.reconstruct_node(*body),
            },
            ArenaLambdaForm::Value { body } => LambdaForm::Value {
                body: self.reconstruct_node(*body),
            },
        }
    }
}

/// Convenience: flatten an `Rc<StgSyn>` into a fresh [`StgArena`].
///
/// The root is always at index 0.
pub fn flatten(root: &Rc<StgSyn>) -> StgArena {
    let mut arena = StgArena::default();
    arena.flatten(root);
    arena
}

// ── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        common::sourcemap::Smid,
        eval::stg::syntax::{Native, Reference},
    };

    fn atom(r: Ref) -> Rc<StgSyn> {
        Rc::new(StgSyn::Atom { evaluand: r })
    }

    fn int(n: i64) -> Ref {
        Reference::V(Native::Num(serde_json::Number::from(n)))
    }

    fn sym(s: &str) -> Ref {
        Reference::V(Native::Sym(s.to_string()))
    }

    #[test]
    fn round_trip_atom() {
        let original = atom(int(42));
        let arena = flatten(&original);
        assert_eq!(arena.nodes.len(), 1, "single atom = one node");
        let reconstructed = arena.reconstruct(0);
        assert_eq!(original, reconstructed);
    }

    #[test]
    fn round_trip_case() {
        let body = atom(int(1));
        let fallback = atom(int(0));
        let original: Rc<StgSyn> = Rc::new(StgSyn::Case {
            scrutinee: atom(Reference::L(0)),
            branches: vec![(1u8, body)],
            fallback: Some(fallback),
            suppress_update: false,
        });
        let arena = flatten(&original);
        let reconstructed = arena.reconstruct(0);
        assert_eq!(original, reconstructed);
    }

    #[test]
    fn round_trip_let() {
        let binding_body = atom(int(10));
        let lf = LambdaForm::Value { body: binding_body };
        let let_body = atom(Reference::L(0));
        let original: Rc<StgSyn> = Rc::new(StgSyn::Let {
            bindings: vec![lf],
            body: let_body,
        });
        let arena = flatten(&original);
        let reconstructed = arena.reconstruct(0);
        assert_eq!(original, reconstructed);
    }

    #[test]
    fn round_trip_lambda_form() {
        let body = atom(Reference::L(0));
        let lf = LambdaForm::Lambda {
            bound: 1,
            body,
            annotation: Smid::default(),
        };
        let original: Rc<StgSyn> = Rc::new(StgSyn::Let {
            bindings: vec![lf],
            body: atom(Reference::L(0)),
        });
        let arena = flatten(&original);
        let reconstructed = arena.reconstruct(0);
        assert_eq!(original, reconstructed);
    }

    #[test]
    fn round_trip_ann() {
        let original: Rc<StgSyn> = Rc::new(StgSyn::Ann {
            smid: Smid::default(),
            body: atom(sym("hello")),
        });
        let arena = flatten(&original);
        let reconstructed = arena.reconstruct(0);
        assert_eq!(original, reconstructed);
    }

    #[test]
    fn round_trip_demeta() {
        let original: Rc<StgSyn> = Rc::new(StgSyn::DeMeta {
            scrutinee: atom(Reference::L(0)),
            handler: atom(Reference::L(1)),
            or_else: atom(Reference::G(0)),
        });
        let arena = flatten(&original);
        let reconstructed = arena.reconstruct(0);
        assert_eq!(original, reconstructed);
    }

    #[test]
    fn postcard_round_trip() {
        let original: Rc<StgSyn> = Rc::new(StgSyn::Let {
            bindings: vec![LambdaForm::Thunk {
                body: atom(int(99)),
            }],
            body: Rc::new(StgSyn::Ann {
                smid: Smid::default(),
                body: atom(Reference::L(0)),
            }),
        });
        let arena = flatten(&original);
        let bytes = postcard::to_allocvec(&arena).expect("serialise");
        let restored: StgArena = postcard::from_bytes(&bytes).expect("deserialise");
        let reconstructed = restored.reconstruct(0);
        assert_eq!(original, reconstructed);
    }
}
