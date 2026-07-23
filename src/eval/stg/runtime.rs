//! The runtime provides intrinsic implementations and corresponding
//! global wrapper STG forms to allow the machine to do real work.
use crate::{
    common::{
        prettify::{prettify, ToPretty},
        sourcemap::SourceMap,
    },
    eval::{
        emit::Emitter,
        machine::intrinsic::{IntrinsicMachine, StgIntrinsic},
        memory::{mutator::MutatorHeapView, syntax::Ref},
        stg::{
            arena::{ArenaLambdaForm, ArenaStgSyn, FormIdx, StgArena},
            support::call,
        },
    },
};

use pretty::{DocAllocator, DocBuilder};

use super::syntax::{
    dsl::{self},
    LambdaForm,
};
use crate::{
    common::sourcemap::Smid,
    eval::{error::ExecutionError, intrinsics},
};

/// The STG-machine can be run with different sets of intrinsics
/// available (mainly for testing). A runtime packages together the
/// intrinsics and their STG-syntax wrappers.
pub trait Runtime: Sync {
    /// Initialise a runtime, registering source map info for any
    /// annotations
    fn prepare(&mut self, source_map: &mut SourceMap);

    /// Provide an environment of globals wrappers
    fn globals(&self) -> Vec<LambdaForm>;

    /// Provide the (immutable) intrinsic implementations
    fn intrinsics(&self) -> Vec<&dyn StgIntrinsic>;
}

pub enum NativeVariant {
    Boxed,
    Unboxed,
}

pub struct StandardRuntime {
    /// Intrinsic implementations
    impls: Vec<Box<dyn StgIntrinsic>>,
    /// Annotation SMIDs to apply to globals
    annotations: Option<Vec<Smid>>,
    /// Pre-compiled prelude lambda forms stored in arena form.
    ///
    /// Arena-flattened forms are `Sync` (no `Rc<StgSyn>` pointers) and are
    /// reconstructed to `LambdaForm` on demand in `globals()`.
    ///
    /// Empty on the source-prelude path.  Populated from `PreludeBlob` when
    /// the blob path is active.
    prelude_nodes: Vec<ArenaStgSyn>,
    /// Complete pool of ALL lambda forms, including inner forms from
    /// `Let`/`LetRec` nodes referenced by `FormIdx` in `prelude_nodes`.
    prelude_forms_pool: Vec<ArenaLambdaForm>,
    /// Index into `prelude_forms_pool` for each prelude global binding
    /// (one entry per binding, in slot order).
    prelude_binding_entries: Vec<FormIdx>,
    /// Binding names in slot order (parallel to `prelude_binding_entries`).
    ///
    /// Empty string for any slot whose name is not recorded.  Used by
    /// `dump runtime` to label prelude global slots.
    prelude_names: Vec<String>,

    /// Per-slot overrides for pre-compiled prelude globals, stored in arena form.
    ///
    /// Each entry is `(slot, nodes, forms, entry_form_idx)` where:
    /// - `slot` is the zero-based prelude slot index (the index into
    ///   `prelude_binding_entries`, *not* the full `Ref::G` slot number)
    /// - `nodes` / `forms` / `entry_form_idx` are a self-contained arena for
    ///   the override `LambdaForm` (reconstructed on demand in `globals()`)
    ///
    /// Arena form (`ArenaStgSyn` / `ArenaLambdaForm`) does not contain `Rc`
    /// and is therefore `Sync`, satisfying the `Runtime: Sync` bound.
    ///
    /// Used to replace stale `__args` / `__io` blob globals with freshly-
    /// constructed runtime values.
    prelude_slot_overrides: Vec<(
        usize,
        Vec<crate::eval::stg::arena::ArenaStgSyn>,
        Vec<crate::eval::stg::arena::ArenaLambdaForm>,
        crate::eval::stg::arena::FormIdx,
    )>,
}

impl Default for StandardRuntime {
    fn default() -> Self {
        let impls = intrinsics::catalogue()
            .iter()
            .map(|i| -> Box<dyn StgIntrinsic> { Box::new(Unimplemented::new(i.name())) })
            .collect();
        StandardRuntime {
            impls,
            annotations: None,
            prelude_nodes: Vec::new(),
            prelude_forms_pool: Vec::new(),
            prelude_binding_entries: Vec::new(),
            prelude_names: Vec::new(),
            prelude_slot_overrides: Vec::new(),
        }
    }
}

impl StandardRuntime {
    /// Set pre-compiled prelude bindings from the blob's arena representation.
    ///
    /// Must be called before `globals()` / `prepare()` when using the blob path.
    /// Storing in arena form (no `Rc`) allows `StandardRuntime` to remain `Sync`.
    ///
    /// `forms_pool` is the COMPLETE set of lambda forms (entry thunks + all inner
    /// forms from `Let`/`LetRec` nodes).  `binding_entries` contains one `FormIdx`
    /// per prelude binding pointing into `forms_pool`.  `names` contains the
    /// binding name for each slot (parallel to `binding_entries`), used by
    /// `dump runtime` to label prelude global slots.
    pub fn set_prelude_bindings(
        &mut self,
        nodes: Vec<ArenaStgSyn>,
        forms_pool: Vec<ArenaLambdaForm>,
        binding_entries: Vec<FormIdx>,
        names: Vec<String>,
    ) {
        self.prelude_nodes = nodes;
        self.prelude_forms_pool = forms_pool;
        self.prelude_binding_entries = binding_entries;
        self.prelude_names = names;
    }
}

impl StandardRuntime {
    pub fn add(&mut self, imp: Box<dyn StgIntrinsic>) {
        let index = imp.index();
        self.impls[index] = imp;
    }

    /// Override a single pre-compiled prelude global with a freshly-compiled
    /// `LambdaForm`.
    ///
    /// `slot` is the zero-based prelude slot index (i.e. the value stored in
    /// `blob.name_to_slot`, *not* the full `Ref::G` index).  When `globals()`
    /// iterates `prelude_binding_entries`, it checks this list first and uses
    /// the override if present.
    ///
    /// The `LambdaForm` is immediately flattened into an arena so that the
    /// `StandardRuntime` remains `Sync` (arena types contain no `Rc`).
    ///
    /// This is used to replace stale `__args` / `__io` blob globals with
    /// forms that reflect the actual runtime argument list and environment.
    pub fn set_prelude_slot_override(&mut self, slot: usize, form: LambdaForm) {
        use crate::eval::stg::arena::StgArena;
        let mut arena = StgArena::default();
        let entry_idx = arena.flatten_form(&form);
        self.prelude_slot_overrides
            .push((slot, arena.nodes, arena.forms, entry_idx));
    }

    /// Reconstruct the per-invocation override globals as `(full global
    /// slot, LambdaForm)` pairs, for the bytecode blob loader.
    ///
    /// The pre-encoded prelude bytecode in the blob bakes stale `__args` /
    /// `__io` bodies; the loader re-encodes these fresh forms onto the
    /// embedded program (mirroring how `globals()` substitutes them into the
    /// STG path). The returned slot is the full `Ref::G` index
    /// (`INTRINSIC_COUNT + prelude slot`), matching the `global_forms` order.
    pub fn prelude_global_overrides(&self) -> Vec<(usize, LambdaForm)> {
        let intrinsic_count = self.impls.len();
        self.prelude_slot_overrides
            .iter()
            .filter_map(|(slot, nodes, forms, entry)| {
                let arena = StgArena {
                    nodes: nodes.clone(),
                    forms: forms.clone(),
                };
                arena
                    .reconstruct_form(*entry)
                    .ok()
                    .map(|form| (intrinsic_count + slot, form))
            })
            .collect()
    }
}

impl ToPretty for StandardRuntime {
    fn pretty<'b, D, A>(&'b self, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        let intrinsic_count = self.impls.len();

        let intrinsic_docs = self.lambdas().into_iter().enumerate().map(|(i, lam)| {
            let name = intrinsics::intrinsic(i).name();

            allocator
                .text(format!("(⊗{i}) "))
                .append(name)
                .append(":")
                .append(allocator.space())
                .append(allocator.line())
                .append(allocator.text(prettify(&lam)))
                .append(allocator.line())
        });

        if self.prelude_binding_entries.is_empty() {
            return allocator.intersperse(intrinsic_docs, allocator.line());
        }

        // In blob mode, also print prelude globals (slots INTRINSIC_COUNT..).
        let arena = StgArena {
            nodes: self.prelude_nodes.clone(),
            forms: self.prelude_forms_pool.clone(),
        };

        let prelude_docs =
            self.prelude_binding_entries
                .iter()
                .enumerate()
                .filter_map(|(i, &entry_idx)| {
                    let slot = intrinsic_count + i;
                    let name = self
                        .prelude_names
                        .get(i)
                        .map(|s| s.as_str())
                        .unwrap_or("<unnamed>");
                    match arena.reconstruct_form(entry_idx) {
                        Ok(lam) => Some(
                            allocator
                                .text(format!("(⊗{slot}) "))
                                .append(name)
                                .append(":")
                                .append(allocator.space())
                                .append(allocator.line())
                                .append(allocator.text(prettify(&lam)))
                                .append(allocator.line()),
                        ),
                        Err(_) => None,
                    }
                });

        allocator.intersperse(intrinsic_docs.chain(prelude_docs), allocator.line())
    }
}

impl StandardRuntime {
    /// Generate the wrappers to populate the global environment
    fn lambdas(&self) -> Vec<LambdaForm> {
        let smids = self.annotations.as_ref().expect("runtime not initialised");

        self.impls
            .iter()
            .zip(smids)
            .map(|(g, ann)| g.wrapper(*ann))
            .collect()
    }
}

impl Runtime for StandardRuntime {
    /// Generate the STG wrappers for all the intrinsics
    fn prepare(&mut self, source_map: &mut SourceMap) {
        self.annotations = Some(
            self.impls
                .iter()
                .map(|bif| source_map.add_synthetic(bif.name()))
                .collect(),
        )
    }

    /// Provide all global STG wrappers for the machine.
    ///
    /// Returns intrinsic wrappers (slots 0..INTRINSIC_COUNT) followed by
    /// any pre-compiled prelude bindings (slots INTRINSIC_COUNT..).
    ///
    /// Must not be called until `prepare()` has been called.
    fn globals(&self) -> Vec<LambdaForm> {
        let mut gs = self.lambdas();
        if !self.prelude_binding_entries.is_empty() {
            let arena = StgArena {
                nodes: self.prelude_nodes.clone(),
                forms: self.prelude_forms_pool.clone(),
            };
            for (i, &entry_idx) in self.prelude_binding_entries.iter().enumerate() {
                // Use the runtime override when present (e.g. for `__args` /
                // `__io` whose blob-baked values are stale at runtime).
                if let Some(override_form) = self
                    .prelude_slot_overrides
                    .iter()
                    .find(|(slot, _, _, _)| *slot == i)
                {
                    let (_, ref nodes, ref forms, entry) = *override_form;
                    let override_arena = StgArena {
                        nodes: nodes.clone(),
                        forms: forms.clone(),
                    };
                    match override_arena.reconstruct_form(entry) {
                        Ok(form) => {
                            gs.push(form);
                            continue;
                        }
                        Err(e) => {
                            eprintln!(
                                "warning: corrupt override for prelude slot {i} ({}), \
                                 using blob value",
                                e
                            );
                        }
                    }
                }
                // Stamp the global-slot identity (eu-1tkk.7.11) so blame
                // classification has something to key on for this prelude
                // combinator, without resurrecting a raw xtask-sourced Smid
                // (see `reconstruct_form_annotated`'s doc comment).
                match arena.reconstruct_form_annotated(entry_idx, Smid::global_slot(i as u32)) {
                    Ok(form) => gs.push(form),
                    Err(e) => {
                        eprintln!(
                            "warning: corrupt prelude blob ({}), falling back to source prelude",
                            e
                        );
                        return self.lambdas();
                    }
                }
            }
        }
        gs
    }

    /// Provide reference to intrinsic implementations for the machine
    fn intrinsics(&self) -> Vec<&dyn StgIntrinsic> {
        self.impls.iter().map(|b| b.as_ref()).collect()
    }
}

#[derive(Clone)]
pub struct Unimplemented {
    name: &'static str,
}

impl Unimplemented {
    fn new(name: &'static str) -> Self {
        Unimplemented { name }
    }
}

impl StgIntrinsic for Unimplemented {
    fn name(&self) -> &str {
        self.name
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        dsl::value(call::bif::panic(dsl::str(format!(
            "unimplemented intrinsic wrapper {}",
            self.name()
        ))))
    }

    fn execute(
        &self,
        _machine: &mut dyn IntrinsicMachine,
        _view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        panic!("executing unimplemented intrinsic {}", self.name())
    }
}
