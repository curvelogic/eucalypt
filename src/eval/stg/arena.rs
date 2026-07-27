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

use std::fmt;
use std::rc::Rc;

use serde::{Deserialize, Serialize};

use super::{
    syntax::{LambdaForm, Ref, StgSyn},
    tags::Tag,
};
use crate::common::sourcemap::Smid;

/// Error from reconstructing a corrupt or incompatible prelude blob.
#[derive(Debug)]
pub enum BlobReconstructError {
    /// A node index is out of range.
    BadNodeIndex { idx: u32, len: usize },
    /// A form index is out of range.
    BadFormIndex { idx: u32, len: usize },
}

impl fmt::Display for BlobReconstructError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BadNodeIndex { idx, len } => {
                write!(f, "blob node index {idx} out of range (pool size {len})")
            }
            Self::BadFormIndex { idx, len } => {
                write!(f, "blob form index {idx} out of range (pool size {len})")
            }
        }
    }
}

impl std::error::Error for BlobReconstructError {}

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
    },
    Cons {
        tag: Tag,
        args: Vec<Ref>,
    },
    App {
        callable: Ref,
        args: Vec<Ref>,
        #[serde(default)]
        eager_args: bool,
    },
    DirectApp {
        smid: Smid,
        callable: Ref,
        args: Vec<Ref>,
        #[serde(default)]
        eager_args: bool,
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
    Seq {
        scrutinee: NodeIdx,
        body: NodeIdx,
    },
    LookupLit {
        smid: Smid,
        key: Ref,
        obj: Ref,
        default: Ref,
    },
    FusedPrimop {
        primop_id: u8,
        left: Ref,
        right: Ref,
        inner: NodeIdx,
    },
    BlackHole,
}

// ── Smid rewriting policy ────────────────────────────────────────────────────

/// How reconstruction rewrites the Smids in a form tree.
///
/// Reconstruction performs two *independent* Smid rewrites, and this type
/// exists so callers choose them independently:
///
/// 1. the `Lambda` **identity stamp**, which lets `SourceMap::classify_frame`
///    name a library boundary; and
/// 2. **neutralising `xtask`-baked `DirectApp`/`LookupLit` Smids**, which stops
///    a prelude-internal frame aliasing an unrelated user declaration.
///
/// They were previously both driven by one `Option<Smid>` parameter, so there
/// was no way to ask for (2) without (1). Narrowing (1) to the slots that
/// benefit from it therefore silently switched off (2) as well and re-admitted
/// the aliasing bug — the failure this type exists to make unrepresentable.
/// See [`StgArena::reconstruct_form_neutralised`] for the full account.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GlobalSmidPolicy {
    /// The form was flattened in *this* process (the pretty-printer, the
    /// freshly-compiled `__args`/`__io` overrides), so its Smids are genuine
    /// entries in this process's `SourceMap`. Leave them alone.
    Verbatim,
    /// A blob-reconstructed global whose name the blame table does not carry:
    /// neutralise baked Smids, stamp no identity.
    Neutralise,
    /// A blob-reconstructed global the blame table names: neutralise baked
    /// Smids by rebasing them onto this `Smid::global_slot` identity, and
    /// stamp the same identity on every reachable `Lambda`.
    Identity(Smid),
}

impl GlobalSmidPolicy {
    /// The annotation to stamp on a reconstructed `Lambda`.
    fn lambda_annotation(self) -> Smid {
        match self {
            GlobalSmidPolicy::Identity(smid) => smid,
            GlobalSmidPolicy::Verbatim | GlobalSmidPolicy::Neutralise => Smid::default(),
        }
    }
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
                }
            }
            StgSyn::Cons { tag, args } => ArenaStgSyn::Cons {
                tag: *tag,
                args: args.clone(),
            },
            StgSyn::App {
                callable,
                args,
                eager_args,
            } => ArenaStgSyn::App {
                callable: callable.clone(),
                args: args.clone(),
                eager_args: *eager_args,
            },
            StgSyn::DirectApp {
                smid,
                callable,
                args,
                eager_args,
            } => ArenaStgSyn::DirectApp {
                smid: *smid,
                callable: callable.clone(),
                args: args.clone(),
                eager_args: *eager_args,
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
            StgSyn::Seq { scrutinee, body } => {
                let s_idx = self.alloc_node(scrutinee);
                let b_idx = self.alloc_node(body);
                ArenaStgSyn::Seq {
                    scrutinee: s_idx,
                    body: b_idx,
                }
            }
            StgSyn::LookupLit {
                smid,
                key,
                obj,
                default,
            } => ArenaStgSyn::LookupLit {
                smid: *smid,
                key: key.clone(),
                obj: obj.clone(),
                default: default.clone(),
            },
            StgSyn::FusedPrimop {
                primop_id,
                left,
                right,
                inner,
            } => {
                let inner_idx = self.alloc_node(inner);
                ArenaStgSyn::FusedPrimop {
                    primop_id: *primop_id,
                    left: left.clone(),
                    right: right.clone(),
                    inner: inner_idx,
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
    pub fn reconstruct(&self, root: NodeIdx) -> Result<Rc<StgSyn>, BlobReconstructError> {
        self.reconstruct_node(root, GlobalSmidPolicy::Verbatim)
    }

    /// Reconstruct a lambda form, elided-Ann/`Smid::default()` behaviour
    /// unchanged from the historical `reconstruct_form`.
    pub fn reconstruct_form(&self, idx: FormIdx) -> Result<LambdaForm, BlobReconstructError> {
        self.reconstruct_form_impl(idx, GlobalSmidPolicy::Verbatim)
    }

    /// Reconstruct a lambda form, stamping every nested `Lambda` form's
    /// `annotation` with `global_annotation` instead of the usual
    /// `Smid::default()` (eu-1tkk.7.11).
    ///
    /// `global_annotation` is expected to be a [`Smid::global_slot`] value
    /// identifying which prelude global this form tree belongs to — not a
    /// real `SourceMap` index — so restoring it does not risk resolving
    /// against the wrong `SourceMap` (see [`Smid`]'s struct doc comment on
    /// why raw xtask-sourced Smids are normally elided). The same
    /// identity is stamped uniformly across every `Lambda` reachable from
    /// `idx` (including inner `Let`/`LetRec` bindings), mirroring how
    /// `Desugarer::new_smid` tags every Smid minted while desugaring
    /// inside a declaration with that declaration's name on the
    /// source-compiled-prelude path.
    ///
    /// Used only at the two blob-mode global-reconstruction chokepoints
    /// (`StandardRuntime::globals()` and the xtask bytecode pre-encode
    /// loop) — every other caller of `reconstruct`/`reconstruct_form`
    /// (the pretty-printer, `__args`/`__io` runtime overrides) is
    /// unaffected and keeps the historical `Smid::default()` behaviour.
    pub fn reconstruct_form_annotated(
        &self,
        idx: FormIdx,
        global_annotation: Smid,
    ) -> Result<LambdaForm, BlobReconstructError> {
        self.reconstruct_form_impl(idx, GlobalSmidPolicy::Identity(global_annotation))
    }

    /// Reconstruct a blob global that declares **no** blame classification.
    ///
    /// Same neutralisation of `xtask`-baked Smids as
    /// [`StgArena::reconstruct_form_annotated`] — the aliasing hazard it
    /// describes is a property of the *blob*, not of the blame table, so it
    /// applies to every reconstructed global — but without stamping a
    /// `Smid::global_slot` identity onto the lambda.
    ///
    /// The distinction matters because the two rewrites serve different ends
    /// and want different scopes (eu-1tkk.7.21 / eu-og3u6):
    ///
    /// * The **identity stamp** exists so `SourceMap::classify_frame` can name
    ///   a library boundary. It is worth having only for slots the blame table
    ///   names, because both machines treat any *valid* closure annotation as
    ///   "the location now in effect" — so stamping a global the blame table
    ///   does not name buys no classification while overwriting the caller's
    ///   genuine call site.
    /// * **Neutralising baked Smids** exists so a prelude-internal `DirectApp`
    ///   or `LookupLit` cannot alias an unrelated user declaration. That hazard
    ///   is unconditional: those raw indices are meaningless in the loading
    ///   process's `SourceMap` whether or not the enclosing global declares a
    ///   blame contract.
    ///
    /// Routing unstamped globals through the plain `reconstruct_form` path
    /// conflated the two and re-admitted the aliasing this neutralisation
    /// exists to prevent: the primary label of a failure inside `nth` named a
    /// `padNNNN` declaration the user never called. Neutralising to
    /// `Smid::default()` keeps the caller's call site (an invalid annotation
    /// does not satisfy the machines' `is_valid` guards, so nothing is
    /// overwritten) *and* removes the alias.
    pub fn reconstruct_form_neutralised(
        &self,
        idx: FormIdx,
    ) -> Result<LambdaForm, BlobReconstructError> {
        self.reconstruct_form_impl(idx, GlobalSmidPolicy::Neutralise)
    }

    /// Replace a Smid baked into the blob by `xtask` with the reconstructing
    /// global's own [`Smid::global_slot`] identity (eu-7x0r).
    ///
    /// `Ann` nodes are elided and `Lambda` annotations replaced outright
    /// (see [`StgArena::reconstruct_form_annotated`]), but `DirectApp` and
    /// `LookupLit` carry a Smid in a *data* field that the machine uses as a
    /// live annotation (`DirectApp` → the call-site annotation an `ApplyTo`
    /// continuation records; `LookupLit` → the location a failed lookup
    /// reports). Copied verbatim, those xtask-era indices are meaningless in
    /// the loading process's `SourceMap`: harmless-looking when the map is
    /// short (they simply fail to resolve and the frame is dropped), but in
    /// any program whose own `SourceMap` grows past them they silently
    /// *alias an unrelated user source position*, so a prelude-internal
    /// frame renders as — and can become the primary label of — a line the
    /// user never called.
    ///
    /// Rebasing onto the enclosing global's slot identity keeps the
    /// annotation valid (so the machine's `is_valid` guards behave as
    /// before) while making it mean the one thing that is true at runtime:
    /// which prelude global this code belongs to.
    ///
    /// `None` (the plain `reconstruct_form` path — the pretty-printer and
    /// the freshly-compiled `__args`/`__io` overrides) leaves the Smid
    /// untouched: those forms were flattened in *this* process, so their
    /// Smids are genuine entries in this process's `SourceMap`.
    fn rebase_baked_smid(&self, smid: Smid, policy: GlobalSmidPolicy) -> Smid {
        match policy {
            // Not from a blob: these Smids index *this* process's SourceMap.
            GlobalSmidPolicy::Verbatim => smid,
            // From a blob, no blame contract: strip the alias, add no identity.
            GlobalSmidPolicy::Neutralise => Smid::default(),
            // From a blob, blame contract: rebase onto the global's identity.
            GlobalSmidPolicy::Identity(global) => {
                if smid.is_valid() {
                    global
                } else {
                    smid
                }
            }
        }
    }

    fn reconstruct_node(
        &self,
        idx: NodeIdx,
        policy: GlobalSmidPolicy,
    ) -> Result<Rc<StgSyn>, BlobReconstructError> {
        let node = self
            .nodes
            .get(idx as usize)
            .ok_or(BlobReconstructError::BadNodeIndex {
                idx,
                len: self.nodes.len(),
            })?;
        // Prelude Ann nodes carry Smids from the xtask's source map which
        // are meaningless at runtime.  Elide them so they do not overwrite
        // the user's call-site annotation in vm.annotation.
        if let ArenaStgSyn::Ann { body, .. } = node {
            return self.reconstruct_node(*body, policy);
        }
        Ok(Rc::new(self.reconstruct_arena_syn(node, policy)?))
    }

    fn reconstruct_arena_syn(
        &self,
        node: &ArenaStgSyn,
        policy: GlobalSmidPolicy,
    ) -> Result<StgSyn, BlobReconstructError> {
        Ok(match node {
            ArenaStgSyn::Atom { evaluand } => StgSyn::Atom {
                evaluand: evaluand.clone(),
            },
            ArenaStgSyn::Case {
                scrutinee,
                branches,
                fallback,
            } => StgSyn::Case {
                scrutinee: self.reconstruct_node(*scrutinee, policy)?,
                branches: branches
                    .iter()
                    .map(|(tag, idx)| Ok((*tag, self.reconstruct_node(*idx, policy)?)))
                    .collect::<Result<_, BlobReconstructError>>()?,
                fallback: fallback
                    .map(|idx| self.reconstruct_node(idx, policy))
                    .transpose()?,
            },
            ArenaStgSyn::Cons { tag, args } => StgSyn::Cons {
                tag: *tag,
                args: args.clone(),
            },
            ArenaStgSyn::App {
                callable,
                args,
                eager_args,
            } => StgSyn::App {
                callable: callable.clone(),
                args: args.clone(),
                eager_args: *eager_args,
            },
            ArenaStgSyn::DirectApp {
                smid,
                callable,
                args,
                eager_args,
            } => StgSyn::DirectApp {
                smid: self.rebase_baked_smid(*smid, policy),
                callable: callable.clone(),
                args: args.clone(),
                eager_args: *eager_args,
            },
            ArenaStgSyn::Bif { intrinsic, args } => StgSyn::Bif {
                intrinsic: *intrinsic,
                args: args.clone(),
            },
            ArenaStgSyn::Let { bindings, body } => StgSyn::Let {
                bindings: bindings
                    .iter()
                    .map(|&idx| self.reconstruct_form_impl(idx, policy))
                    .collect::<Result<_, BlobReconstructError>>()?,
                body: self.reconstruct_node(*body, policy)?,
            },
            ArenaStgSyn::LetRec { bindings, body } => StgSyn::LetRec {
                bindings: bindings
                    .iter()
                    .map(|&idx| self.reconstruct_form_impl(idx, policy))
                    .collect::<Result<_, BlobReconstructError>>()?,
                body: self.reconstruct_node(*body, policy)?,
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
                scrutinee: self.reconstruct_node(*scrutinee, policy)?,
                handler: self.reconstruct_node(*handler, policy)?,
                or_else: self.reconstruct_node(*or_else, policy)?,
            },
            ArenaStgSyn::Seq { scrutinee, body } => StgSyn::Seq {
                scrutinee: self.reconstruct_node(*scrutinee, policy)?,
                body: self.reconstruct_node(*body, policy)?,
            },
            ArenaStgSyn::LookupLit {
                smid,
                key,
                obj,
                default,
            } => StgSyn::LookupLit {
                smid: self.rebase_baked_smid(*smid, policy),
                key: key.clone(),
                obj: obj.clone(),
                default: default.clone(),
            },
            ArenaStgSyn::FusedPrimop {
                primop_id,
                left,
                right,
                inner,
            } => StgSyn::FusedPrimop {
                primop_id: *primop_id,
                left: left.clone(),
                right: right.clone(),
                inner: self.reconstruct_node(*inner, policy)?,
            },
            ArenaStgSyn::BlackHole => StgSyn::BlackHole,
        })
    }

    fn reconstruct_form_impl(
        &self,
        idx: FormIdx,
        policy: GlobalSmidPolicy,
    ) -> Result<LambdaForm, BlobReconstructError> {
        let form = self
            .forms
            .get(idx as usize)
            .ok_or(BlobReconstructError::BadFormIndex {
                idx,
                len: self.forms.len(),
            })?;
        Ok(match form {
            ArenaLambdaForm::Lambda { bound, body, .. } => LambdaForm::Lambda {
                bound: *bound,
                body: self.reconstruct_node(*body, policy)?,
                // Clear xtask-sourced (real `SourceMap`) annotations —
                // they are meaningless at runtime and would pollute user
                // error locations. `global_annotation`, when supplied via
                // `reconstruct_form_annotated`, is a `Smid::global_slot`
                // value instead — a distinct, disjoint identity space, not
                // a raw source Smid — so restoring it is safe.
                annotation: policy.lambda_annotation(),
            },
            ArenaLambdaForm::Thunk { body } => LambdaForm::Thunk {
                body: self.reconstruct_node(*body, policy)?,
            },
            ArenaLambdaForm::Value { body } => LambdaForm::Value {
                body: self.reconstruct_node(*body, policy)?,
            },
        })
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
        let reconstructed = arena.reconstruct(0).unwrap();
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
        });
        let arena = flatten(&original);
        let reconstructed = arena.reconstruct(0).unwrap();
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
        let reconstructed = arena.reconstruct(0).unwrap();
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
        let reconstructed = arena.reconstruct(0).unwrap();
        assert_eq!(original, reconstructed);
    }

    #[test]
    fn round_trip_ann_elides() {
        // Ann nodes are elided during reconstruction (prelude Smids are
        // meaningless at runtime), so the reconstructed tree is the body.
        let original: Rc<StgSyn> = Rc::new(StgSyn::Ann {
            smid: Smid::default(),
            body: atom(sym("hello")),
        });
        let arena = flatten(&original);
        let reconstructed = arena.reconstruct(0).unwrap();
        assert_eq!(atom(sym("hello")), reconstructed);
    }

    #[test]
    fn round_trip_demeta() {
        let original: Rc<StgSyn> = Rc::new(StgSyn::DeMeta {
            scrutinee: atom(Reference::L(0)),
            handler: atom(Reference::L(1)),
            or_else: atom(Reference::G(0)),
        });
        let arena = flatten(&original);
        let reconstructed = arena.reconstruct(0).unwrap();
        assert_eq!(original, reconstructed);
    }

    #[test]
    fn postcard_round_trip() {
        // Ann nodes are elided during reconstruction, so the expected
        // output has the Ann stripped.
        let original: Rc<StgSyn> = Rc::new(StgSyn::Let {
            bindings: vec![LambdaForm::Thunk {
                body: atom(int(99)),
            }],
            body: Rc::new(StgSyn::Ann {
                smid: Smid::default(),
                body: atom(Reference::L(0)),
            }),
        });
        let expected: Rc<StgSyn> = Rc::new(StgSyn::Let {
            bindings: vec![LambdaForm::Thunk {
                body: atom(int(99)),
            }],
            body: atom(Reference::L(0)),
        });
        let arena = flatten(&original);
        let bytes = postcard::to_allocvec(&arena).expect("serialise");
        let restored: StgArena = postcard::from_bytes(&bytes).expect("deserialise");
        let reconstructed = restored.reconstruct(0).unwrap();
        assert_eq!(expected, reconstructed);
    }

    // ── baked-Smid rebasing (eu-7x0r) ───────────────────────────────────────

    /// A form carrying a `DirectApp` and a `LookupLit`, each with a Smid that
    /// would be a real `SourceMap` index in the process that flattened it.
    fn form_with_baked_smids(baked: Smid) -> LambdaForm {
        LambdaForm::Lambda {
            bound: 1,
            body: Rc::new(StgSyn::Seq {
                scrutinee: Rc::new(StgSyn::DirectApp {
                    smid: baked,
                    callable: Reference::G(3),
                    args: vec![Reference::L(0)],
                    eager_args: false,
                }),
                body: Rc::new(StgSyn::LookupLit {
                    smid: baked,
                    key: sym("k"),
                    obj: Reference::L(0),
                    default: Reference::L(0),
                }),
            }),
            annotation: baked,
        }
    }

    /// Collect `(DirectApp, LookupLit, Lambda.annotation)` Smids from a form
    /// shaped by [`form_with_baked_smids`].
    fn smids_of(form: &LambdaForm) -> (Smid, Smid, Smid) {
        let LambdaForm::Lambda {
            body, annotation, ..
        } = form
        else {
            panic!("expected a Lambda form");
        };
        let StgSyn::Seq { scrutinee, body } = &**body else {
            panic!("expected a Seq body");
        };
        let StgSyn::DirectApp { smid: app, .. } = &**scrutinee else {
            panic!("expected a DirectApp scrutinee");
        };
        let StgSyn::LookupLit { smid: lookup, .. } = &**body else {
            panic!("expected a LookupLit body");
        };
        (*app, *lookup, *annotation)
    }

    /// Blob-mode reconstruction must rebase every Smid baked by `xtask` onto
    /// the enclosing global's slot identity. `Ann` nodes are elided and
    /// `Lambda` annotations replaced, but `DirectApp` and `LookupLit` carry a
    /// Smid in a *data* field that the machine uses as a live annotation:
    /// copied verbatim, those xtask-era indices alias unrelated entries in
    /// the loading process's `SourceMap`.
    #[test]
    fn reconstruct_form_annotated_rebases_directapp_and_lookuplit_smids() {
        let baked = Smid::fake(1859);
        let mut arena = StgArena::default();
        let entry = arena.flatten_form(&form_with_baked_smids(baked));

        let slot = Smid::global_slot(42);
        let form = arena.reconstruct_form_annotated(entry, slot).unwrap();

        assert_eq!(
            smids_of(&form),
            (slot, slot, slot),
            "every Smid in a blob-reconstructed prelude form must be the global-slot identity"
        );
    }

    /// The plain `reconstruct_form` path (the pretty-printer, and the
    /// freshly-compiled `__args` / `__io` runtime overrides) must leave these
    /// Smids alone: those forms were flattened in *this* process, so their
    /// Smids are genuine entries in this process's `SourceMap`.
    #[test]
    fn reconstruct_form_preserves_locally_flattened_smids() {
        let local = Smid::fake(1859);
        let mut arena = StgArena::default();
        let entry = arena.flatten_form(&form_with_baked_smids(local));

        let form = arena.reconstruct_form(entry).unwrap();
        let (app, lookup, annotation) = smids_of(&form);

        assert_eq!((app, lookup), (local, local));
        // `Lambda.annotation` is cleared on this path, as it always has been.
        assert_eq!(annotation, Smid::default());
    }

    /// A blob global the blame table does **not** name must still have its
    /// `xtask`-baked `DirectApp`/`LookupLit` Smids neutralised, while its
    /// `Lambda` annotation stays `Smid::default()`.
    ///
    /// These are the two halves of the reconstruction rewrite, and they have
    /// different scopes on purpose (eu-1tkk.7.21 / eu-og3u6):
    ///
    /// * leaving the `Lambda` unannotated is what preserves the caller's call
    ///   site, because both machines overwrite the live annotation with any
    ///   *valid* closure annotation on entry; and
    /// * neutralising the data Smids is what stops a prelude-internal frame
    ///   aliasing an unrelated user declaration, which is a hazard of the blob
    ///   regardless of blame contract.
    ///
    /// The regression this gates is real and shipped-adjacent: routing
    /// unstamped globals through `reconstruct_form` (which does *neither*
    /// rewrite, being meant for locally-flattened forms) got the first half
    /// right and silently dropped the second, and the primary label of a
    /// failure inside `nth` went back to naming a `padNNNN` declaration the
    /// user never called.
    ///
    /// `Smid::default()` rather than the slot identity is deliberate: a
    /// `DirectApp`'s Smid is installed as the live annotation *unguarded*
    /// (`state.set_annotation(smid)` in both engines), so any valid value
    /// here would destroy the call site exactly as the `Lambda` stamp did.
    #[test]
    fn reconstruct_form_neutralised_clears_baked_smids_without_stamping_an_identity() {
        let baked = Smid::fake(1859);
        let mut arena = StgArena::default();
        let entry = arena.flatten_form(&form_with_baked_smids(baked));

        let form = arena.reconstruct_form_neutralised(entry).unwrap();
        let (app, lookup, annotation) = smids_of(&form);

        assert_eq!(
            (app, lookup),
            (Smid::default(), Smid::default()),
            "baked DirectApp/LookupLit Smids must be neutralised for every blob \
             global, blame contract or not — otherwise they alias user declarations"
        );
        assert!(
            !app.is_valid() && !lookup.is_valid(),
            "the neutralised Smid must be *invalid*, so the machines' is_valid \
             guards leave the caller's call-site annotation in place"
        );
        assert_eq!(
            annotation,
            Smid::default(),
            "an unstamped global must carry no lambda identity, or entering it \
             overwrites the caller's call site"
        );
    }
}
