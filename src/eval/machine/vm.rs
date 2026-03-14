//! A variation of the STG machine
//!
//! This is in the process of morphing from something that is clearly
//! an interpreter to something more like a VM

use std::{cmp::Ordering, convert::TryInto, num::NonZeroUsize};

use itertools::Itertools;
use lru::LruCache;
use regex::Regex;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::{Emitter, NullEmitter},
        error::ExecutionError,
        intrinsics,
        machine::env::ScopeAndClosure,
        memory::{
            alloc::{ScopedAllocator, ScopedPtr},
            array::Array,
            collect::{self, CollectorHeapView, CollectorScope, GcScannable, ScanPtr},
            heap::{Heap, HeapStats},
            infotable::InfoTable,
            mutator::{Mutator, MutatorHeapView},
            symbol::SymbolPool,
            syntax::{HeapSyn, Native, Ref, RefPtr, StgBuilder},
        },
        stg::tags::{DataConstructor, Tag},
    },
};

use super::{
    cont::{match_tag, Continuation},
    env::{EnvFrame, SynClosure},
    intrinsic::{IntrinsicMachine, StgIntrinsic},
    metrics::{Metrics, ThreadOccupation},
};
use super::{env_builder::EnvBuilder, metrics::Clock};

/// A utility for navigating chains of refs / pointers through the heap
pub struct HeapNavigator<'scope> {
    /// Local environment
    locals: ScopedPtr<'scope, EnvFrame>,
    /// Global environment
    globals: ScopedPtr<'scope, EnvFrame>,
    /// Mutator view of heap
    pub(crate) view: MutatorHeapView<'scope>,
}

impl HeapNavigator<'_> {
    /// Resolve a ref (creating atom closure if it resolves to native)
    pub fn resolve(&self, r: &Ref) -> Result<SynClosure, ExecutionError> {
        match r {
            Ref::L(index) => Ok(self.get(*index)?),
            Ref::G(index) => Ok(self.global(*index)?),
            Ref::V(_) => Ok(SynClosure::new(
                self.view
                    .alloc(HeapSyn::Atom {
                        evaluand: r.clone(),
                    })?
                    .as_ptr(),
                self.view.alloc(EnvFrame::default())?.as_ptr(),
            )),
        }
    }

    /// Index into current environment, retrieving pointer
    pub fn get(&self, index: usize) -> Result<SynClosure, ExecutionError> {
        (*self.locals)
            .get(&self.view, index)
            .ok_or(ExecutionError::BadEnvironmentIndex(index))
    }

    /// Index into globals
    pub fn global(&self, index: usize) -> Result<SynClosure, ExecutionError> {
        (*self.globals)
            .get(&self.view, index)
            .ok_or(ExecutionError::BadGlobalIndex(index))
    }

    /// Resolve a ref to a closure
    pub fn resolve_callable(&self, r: &Ref) -> Result<SynClosure, ExecutionError> {
        match r {
            Ref::L(index) => self.get(*index),
            Ref::G(index) => self.global(*index),
            Ref::V(n) => Err(ExecutionError::NotCallable(
                Smid::default(),
                n.type_description().to_string(),
            )),
        }
    }

    /// Repeat resolves until we terminate with a native (vref)
    ///
    /// Should never have to burrow. But our forcing mechanism (CASE)
    /// currently wraps a bound ref to the value so we need unwrap again.
    pub fn resolve_native(&self, r: &Ref) -> Result<Native, ExecutionError> {
        let mut closure = match r {
            Ref::L(index) => self.get(*index)?,
            Ref::G(index) => self.global(*index)?,
            Ref::V(n) => return Ok(n.clone()),
        };

        let mut scoped_code = self.view.scoped(closure.code());

        while let HeapSyn::Atom { evaluand: r } = &*scoped_code {
            closure = match r {
                Ref::L(index) => {
                    let env = self.view.scoped(closure.env());
                    (*env)
                        .get(&self.view, *index)
                        .ok_or(ExecutionError::BadEnvironmentIndex(*index))?
                }
                Ref::G(index) => self.global(*index)?,
                Ref::V(n) => return Ok(n.clone()),
            };

            scoped_code = self.view.scoped(closure.code());
        }

        let description = match &*scoped_code {
            HeapSyn::Cons { .. } => "a data constructor (e.g. block or list)",
            HeapSyn::App { .. } => "a function application",
            HeapSyn::Bif { .. } => "an intrinsic function call",
            HeapSyn::Case { .. } => "a case expression",
            HeapSyn::Let { .. } | HeapSyn::LetRec { .. } => "a let binding",
            HeapSyn::Meta { .. } | HeapSyn::DeMeta { .. } => "a metadata expression",
            HeapSyn::BlackHole => "an uninitialised value (possible cycle)",
            HeapSyn::Ann { .. } => "an annotated expression",
            HeapSyn::Atom { .. } => "an atom",
        };
        Err(ExecutionError::NotValue(
            Smid::default(),
            description.to_string(),
        ))
    }

    pub fn env_trace(&self) -> Vec<Smid> {
        (*self.locals).annotation_trace(&self.view)
    }

    /// Resolve a ref relative to a specific closure's environment, using
    /// the navigator's globals for global refs. Returns None for value
    /// refs (Ref::V) since forming a closure would require heap allocation.
    pub fn resolve_in_closure(&self, closure: &SynClosure, r: Ref) -> Option<SynClosure> {
        match r {
            Ref::L(i) => {
                let env = self.view.scoped(closure.env());
                (*env).get(&self.view, i)
            }
            Ref::G(i) => (*self.globals).get(&self.view, i),
            Ref::V(_) => None,
        }
    }
}

/// Classification of data constructor arg patterns for shared-env optimisation.
///
/// Used by `env_from_data_args` to determine whether the new environment frame
/// can share backing storage with the constructor's environment frame, preserving
/// thunk memoisation across multiple traversals of the same structure.
enum ArgPattern {
    /// All args map to physical slots `0, 1, ..., n-1` (after composing with the
    /// constructor env's remap if any) and `n == backing_len` — identity physical
    /// mapping, shares full backing with no remap overhead.
    SequentialFromZero(usize),
    /// All args are `L(_)` indices within the constructor's top frame, up to 4 args.
    /// Shares the full backing array and uses a physical remap table for
    /// logical→physical translation.
    Remapped { remap: [u8; 4], len: usize },
    /// Contains `Ref::V` or `Ref::G` refs, has more than 4 args, or a logical index
    /// exceeds the constructor's top frame — must copy into fresh storage.
    RequiresCopy,
}

/// Classify a data constructor's arg slice for the shared-env optimisation.
///
/// Computes the *physical* remap by composing each `L(i)` arg with the
/// constructor frame's own remap (if any).  Then checks:
///
/// - If the physical slots are `0, 1, ..., n-1` and `n == backing_len` → `SequentialFromZero`.
/// - If all args are `L(idx)` within the top frame and `len <= 4` → `Remapped`.
/// - Otherwise → `RequiresCopy`.
///
/// The `backing_len` parameter is the physical slot count of the constructor's top
/// frame.  `logical_len` is its exposed (logical) slot count.  They differ when the
/// constructor frame itself has a remap table.
fn classify_args(
    args: &[Ref],
    logical_len: usize,
    backing_len: usize,
    env_physical_index: impl Fn(usize) -> usize,
) -> ArgPattern {
    if args.len() > 4 {
        return ArgPattern::RequiresCopy;
    }

    let mut phys_remap = [0u8; 4];
    let mut is_identity = true;

    for (j, r) in args.iter().enumerate() {
        match r {
            Ref::L(idx) if *idx < logical_len => {
                let phys = env_physical_index(*idx);
                if phys > u8::MAX as usize {
                    // Physical index doesn't fit in the u8 remap table —
                    // fall back to copying for correctness.
                    return ArgPattern::RequiresCopy;
                }
                phys_remap[j] = phys as u8;
                if phys != j {
                    is_identity = false;
                }
            }
            _ => return ArgPattern::RequiresCopy,
        }
    }

    if is_identity && args.len() == backing_len {
        ArgPattern::SequentialFromZero(args.len())
    } else {
        ArgPattern::Remapped {
            remap: phys_remap,
            len: args.len(),
        }
    }
}

/// The state of the machine (stack / closure etc.)
pub struct MachineState {
    /// Root (empty) environment
    root_env: RefPtr<EnvFrame>,
    /// Current closure
    closure: SynClosure,
    /// Globals (primarily STG wrappers for intrinsics)
    globals: RefPtr<EnvFrame>,
    /// Stack of continuations (stored inline, not on the eucalypt heap)
    stack: Vec<Continuation>,
    /// Termination flag. Set when machine has terminated
    terminated: bool,
    /// Yield flag. Set (alongside `terminated`) when the machine has
    /// evaluated an IO constructor to WHNF with no pending Branch or
    /// ApplyTo continuations.  The `io-run` driver loop checks this
    /// flag to distinguish a normal termination from an IO yield and
    /// inspects `closure` to read the IO constructor tag and fields.
    yielded_io: bool,
    /// Annotation to paint on any environments we create
    annotation: Smid,
    /// Cache compiled regexes
    rcache: LruCache<String, Regex>,
    /// Interned symbol pool for fast symbol comparison
    symbol_pool: SymbolPool,
    /// When set, suppress the next Update continuation push.
    ///
    /// Set by `return_data` when processing a Branch with
    /// `suppress_update=true` and a branch body that is a bare local
    /// atom. Prevents O(N) Update accumulation in tail-recursive
    /// conditional loops such as countdown(n) = if(n=0, 0, countdown(n-1)).
    suppress_next_update: bool,
    /// Stash of closures kept alive across `machine.run()` calls.
    ///
    /// The io-run driver holds closures (e.g. `cont` and `world` from an
    /// `IoBind` handler) across multiple `machine.run()` calls.  Those
    /// closures are heap-allocated but are NOT part of `closure` or the
    /// continuation stack, so the GC would not mark them.  Pushing them
    /// into this stash ensures they are scanned as GC roots for the
    /// duration of the io-run loop.
    stash: Vec<SynClosure>,
    /// Temporary storage for BIF execution context.
    ///
    /// During BIF execution, `handle_instruction` stores raw pointers
    /// to the intrinsics slice, emitter, and metrics so that
    /// `force_to_whnf` can re-enter the step loop.  These pointers are
    /// valid only for the duration of the BIF call (they point into the
    /// enclosing `Machine::step()` call frame).  Cleared immediately
    /// after each BIF returns.
    bif_intrinsics: Option<*const &'static dyn StgIntrinsic>,
    bif_intrinsics_len: usize,
}

impl Default for MachineState {
    fn default() -> Self {
        Self {
            root_env: RefPtr::dangling(),
            closure: SynClosure::new(RefPtr::dangling(), RefPtr::dangling()),
            globals: RefPtr::dangling(),
            stack: Default::default(),
            terminated: Default::default(),
            yielded_io: Default::default(),
            annotation: Default::default(),
            rcache: LruCache::new(
                NonZeroUsize::new(100).expect("regex cache size must be non-zero"),
            ),
            symbol_pool: SymbolPool::new(),
            suppress_next_update: false,
            stash: Vec::new(),
            bif_intrinsics: None,
            bif_intrinsics_len: 0,
        }
    }
}

impl MachineState {
    /// Set global environment
    pub fn set_globals(&mut self, globals: RefPtr<EnvFrame>) -> Result<(), ExecutionError> {
        self.globals = globals;
        Ok(())
    }

    /// Has the machine terminated?
    pub fn terminated(&self) -> bool {
        self.terminated
    }

    /// Has the machine yielded on an IO constructor?
    ///
    /// When true, `terminated` is also true (so the run loop stops).
    /// The io-run driver checks this flag to distinguish an IO yield
    /// from a normal termination.
    pub fn yielded_io(&self) -> bool {
        self.yielded_io
    }

    /// Push a new continuation onto the stack
    fn push(&mut self, _view: MutatorHeapView, cont: Continuation) -> Result<(), ExecutionError> {
        self.stack.push(cont);
        Ok(())
    }

    /// Handle an instruction
    #[inline]
    fn handle_instruction<'guard>(
        &mut self,
        view: MutatorHeapView<'guard>,
        emitter: &mut dyn Emitter,
        intrinsics: &[&'guard dyn StgIntrinsic],
        metrics: &mut Metrics,
    ) -> Result<(), ExecutionError> {
        // Load "op code"
        let code = (*view.scoped(self.closure.code())).clone();
        let environment = self.closure.env();
        let remaining_arity = self.closure.arity();

        // Set annotation to stamp on any allocations.
        // Only update if the closure carries a valid annotation — value forms
        // and other synthetic closures use Smid::default(), and propagating
        // that would overwrite a meaningful call-site annotation set by an
        // enclosing Ann node.
        let closure_ann = self.closure.annotation();
        if closure_ann.is_valid() {
            self.annotation = closure_ann;
        }

        if remaining_arity > 0 {
            return self.return_fun(view);
        }

        match code {
            HeapSyn::Atom { evaluand } => {
                // Consume suppress_next_update flag set by return_data
                // when processing a suppress_update Branch continuation.
                let suppress_update = std::mem::replace(&mut self.suppress_next_update, false);
                match evaluand {
                    Ref::L(i) => {
                        self.closure = self.nav(view).get(i)?;
                        let is_thunk = self.closure.update();
                        let updateable = is_thunk && !suppress_update;
                        // When suppress_update is active but the loaded closure is
                        // not itself a thunk (e.g. a synthetic value-Atom closure
                        // created by create_arg_array), propagate the flag to the
                        // next Atom step so the full indirection chain is covered.
                        //
                        // This handles the two-level case that arises when IF args
                        // are passed via App (not inlined): the Branch body is
                        // Atom{L(i)} in IF's arg env → value closure → Atom{L(j)}
                        // in the caller's let env → actual thunk.  Without
                        // propagation, only the first hop is suppressed and the
                        // second hop pushes an unwanted Update continuation.
                        if suppress_update && !is_thunk {
                            self.suppress_next_update = true;
                        }
                        if updateable {
                            // Overwrite the env slot with a BlackHole
                            // closure to catch cyclic thunk re-entry.
                            // The Update continuation will replace it
                            // with the computed value on success.
                            let hole = view.alloc(HeapSyn::BlackHole)?;
                            let black_hole = SynClosure::new(hole.as_ptr(), environment);
                            let cont_env = view.scoped(environment);
                            cont_env.update(&view, i, black_hole)?;

                            self.push(
                                view,
                                Continuation::Update {
                                    environment,
                                    index: i,
                                },
                            )?;
                        }
                    }
                    Ref::G(i) => {
                        self.closure = self.nav(view).global(i)?;
                    }
                    Ref::V(v) => {
                        self.return_native(view, &v)?;
                    }
                }
            }
            HeapSyn::Case {
                scrutinee,
                min_tag,
                branch_table,
                fallback,
                suppress_update: case_suppress,
            } => {
                self.push(
                    view,
                    Continuation::Branch {
                        min_tag,
                        branch_table,
                        fallback,
                        environment,
                        annotation: self.annotation,
                        suppress_update: case_suppress,
                    },
                )?;
                self.closure = SynClosure::new(scrutinee, environment);
            }
            HeapSyn::Cons { tag, args } => {
                self.return_data(view, tag, args.as_slice())?;
            }
            HeapSyn::App { callable, args } => {
                let array = view.create_arg_array(args.as_slice(), environment)?;
                self.push(
                    view,
                    Continuation::ApplyTo {
                        args: array,
                        annotation: self.annotation,
                    },
                )?;
                self.closure = self.nav(view).resolve_callable(&callable)?;
            }
            HeapSyn::Bif { intrinsic, args } => {
                let bif = intrinsics[intrinsic as usize];
                // Store intrinsics pointer so force_to_whnf can
                // re-enter the step loop.  SAFETY: intrinsics is a
                // shared (&) slice that lives in the enclosing call
                // frame and outlives the bif.execute() call.
                self.bif_intrinsics = Some(unsafe {
                    std::mem::transmute::<*const &dyn StgIntrinsic, *const &'static dyn StgIntrinsic>(
                        intrinsics.as_ptr(),
                    )
                });
                self.bif_intrinsics_len = intrinsics.len();
                let result = bif
                    .execute(self, view, emitter, args.as_slice())
                    .inspect_err(|_| {
                        // Set annotation to the BIF's own annotation so
                        // errors report the correct intrinsic context
                        if let Ok(global_closure) = self.nav(view).global(intrinsic as usize) {
                            let bif_ann = global_closure.annotation();
                            if bif_ann.is_valid() {
                                self.annotation = bif_ann;
                            }
                        }
                    });
                self.bif_intrinsics = None;
                result?;
            }
            HeapSyn::Let { bindings, body } => {
                metrics.alloc(bindings.len());
                let new_env = view.from_let(bindings.as_slice(), environment, self.annotation)?;
                self.closure = SynClosure::new(body, new_env);
            }
            HeapSyn::LetRec { bindings, body } => {
                metrics.alloc(bindings.len());
                let new_env =
                    view.from_letrec(bindings.as_slice(), environment, self.annotation)?;
                self.closure = SynClosure::new(body, new_env);
            }
            HeapSyn::Ann { smid, body } => {
                self.annotation = smid;
                self.closure = SynClosure::new(body, environment);
            }
            HeapSyn::Meta { meta, body } => {
                self.return_meta(view, &meta, &body)?;
            }
            HeapSyn::DeMeta {
                scrutinee,
                handler,
                or_else,
            } => {
                self.push(
                    view,
                    Continuation::DeMeta {
                        handler,
                        or_else,
                        environment,
                    },
                )?;
                self.closure = SynClosure::new(scrutinee, environment);
            }
            HeapSyn::BlackHole => return Err(ExecutionError::BlackHole(self.annotation)),
        }

        Ok(())
    }

    /// Update environment index to point to current closure
    fn update(
        &mut self,
        view: MutatorHeapView,
        environment: RefPtr<EnvFrame>,
        index: usize,
    ) -> Result<(), ExecutionError> {
        let cont_env = view.scoped(environment);
        cont_env.update(&view, index, self.closure.clone())
    }

    /// Return meta into a demeta destructuring or just strip metadata
    /// and continue
    fn return_meta(
        &mut self,
        view: MutatorHeapView<'_>,
        meta: &Ref,
        body: &Ref,
    ) -> Result<(), ExecutionError> {
        if let Some(continuation) = self.stack.pop() {
            match continuation {
                Continuation::DeMeta {
                    handler,
                    environment,
                    ..
                } => {
                    self.closure = SynClosure::new(
                        handler,
                        view.from_closures(
                            [self.nav(view).resolve(meta)?, self.nav(view).resolve(body)?]
                                .iter()
                                .cloned(),
                            2,
                            environment,
                            self.annotation,
                        )?,
                    );
                }
                Continuation::Update { environment, index } => {
                    self.update(view, environment, index)?;
                }
                other => {
                    self.closure = self.nav(view).resolve(body)?;
                    self.stack.push(other);
                }
            }
        } else {
            self.closure = self.nav(view).resolve(body)?;
            // Don't terminate at metadata, carrying processing
        }

        Ok(())
    }

    /// Return a native value into continuation or terminate.
    ///
    /// The current closure already wraps the native in an Atom on the
    /// heap, so we reuse that closure when building the env frame for
    /// Branch/DeMeta fallbacks instead of allocating a fresh Atom.
    fn return_native(
        &mut self,
        view: MutatorHeapView<'_>,
        value: &Native,
    ) -> Result<(), ExecutionError> {
        if let Some(continuation) = self.stack.pop() {
            match continuation {
                Continuation::Branch {
                    fallback,
                    environment,
                    ..
                } => {
                    // case fallbacks can handle natives
                    if let Some(fb) = fallback {
                        // Reuse the existing closure (already an Atom
                        // wrapping the native value) instead of allocating
                        // a new HeapSyn::Atom via from_args.
                        self.closure = SynClosure::new(
                            fb,
                            view.from_closure(self.closure.clone(), environment, self.annotation)?,
                        );
                    } else {
                        return Err(ExecutionError::NoBranchForNative(
                            self.annotation,
                            value.type_description().to_string(),
                        ));
                    }
                }
                Continuation::Update { environment, index } => {
                    self.update(view, environment, index)?;
                }
                Continuation::ApplyTo { annotation, .. } => {
                    return Err(ExecutionError::NotCallable(
                        annotation,
                        value.type_description().to_string(),
                    ));
                }
                Continuation::DeMeta {
                    or_else,
                    environment,
                    ..
                } => {
                    // Reuse existing Atom closure as above
                    self.closure = SynClosure::new(
                        or_else,
                        view.from_closure(self.closure.clone(), environment, self.annotation)?,
                    );
                }
            }
        } else {
            self.terminated = true
        }

        Ok(())
    }

    /// Build an env frame from data constructor args, sharing backing
    /// storage where possible to preserve thunk memoisation.
    ///
    /// When all args are `Ref::L` references whose physical indices all
    /// fall within the top frame of the constructor's environment, the new
    /// frame shares that frame's backing `Array` rather than copying
    /// closures into fresh storage.  Thunk updates (via the `Update`
    /// continuation) write back to the shared storage and are visible
    /// through all frames that share it.
    ///
    /// **GC safety**: the shared array always has `length == backing_len`
    /// (the constructor frame's full physical binding count) so the GC
    /// scans all live slots regardless of which frame it encounters first.
    /// For identity-mapped frames the new frame exposes all slots directly.
    /// For remapped frames the remap table restricts *logical* access to
    /// the relevant indices without affecting the GC's view of the full
    /// physical backing.
    ///
    /// Falls back to `env_from_data_args_copy` when:
    /// - args contain `Ref::V` or `Ref::G` references
    /// - any arg's logical index reaches beyond the constructor's top frame
    ///   (it chains into a deeper frame — sharing across frames is unsupported)
    /// - more than 4 args (remap table capacity)
    #[inline]
    fn env_from_data_args(
        &self,
        view: MutatorHeapView<'_>,
        args: &[Ref],
        next: RefPtr<EnvFrame>,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError> {
        let constructor_env = view.scoped(self.closure.env());
        let logical_len = (*constructor_env).logical_len();
        let backing_len = (*constructor_env).backing_len();

        match classify_args(args, logical_len, backing_len, |i| {
            (*constructor_env).physical_index(i)
        }) {
            ArgPattern::SequentialFromZero(n) => {
                // Physical slots are 0, 1, ..., n-1 and n == backing_len — identity
                // mapping covering the full physical backing.  Share the full backing
                // so the GC traces every live slot regardless of which frame it sees
                // first.  Since args.len() == backing_len the new frame's logical_len
                // also equals backing_len, exposing all slots before chaining to next.
                let shared = (*constructor_env).shared_bindings_full();
                debug_assert_eq!(n, backing_len);
                Ok(view
                    .alloc(EnvFrame::new(shared, self.annotation, Some(next)))?
                    .as_ptr())
            }
            ArgPattern::Remapped { remap, len } => {
                // Non-sequential, offset, or fewer args than backing_len.  Share the
                // full physical backing for GC correctness and store the physical remap
                // so the branch body's L(i) refs resolve to the correct physical slots.
                //
                // The remap table holds *physical* slot indices (already composed with
                // any remap in the constructor env), so no further indirection is needed.
                //
                // GC safety: shared array length == backing_len so all live slots are
                // traced regardless of which frame the collector encounters first.
                let shared = (*constructor_env).shared_bindings_full();
                Ok(view
                    .alloc(EnvFrame::new_remapped(
                        shared,
                        &remap[..len],
                        self.annotation,
                        Some(next),
                    ))?
                    .as_ptr())
            }
            ArgPattern::RequiresCopy => self.env_from_data_args_copy(view, args, next),
        }
    }

    /// Fallback: build an env frame by copying closures from the constructor env.
    ///
    /// Used when the arg pattern cannot use shared backing: non-local refs
    /// (`Ref::V` or `Ref::G`), more than 4 args, or physical indices that reach
    /// beyond the constructor's top frame.  This is the original behaviour prior
    /// to the shared-env optimisation.
    fn env_from_data_args_copy(
        &self,
        view: MutatorHeapView<'_>,
        args: &[Ref],
        next: RefPtr<EnvFrame>,
    ) -> Result<RefPtr<EnvFrame>, ExecutionError> {
        let local_env = view.scoped(self.closure.env());
        let global_env = view.scoped(self.globals);

        let mut array = Array::with_capacity(&view, args.len());
        for r in args {
            let closure = match r {
                Ref::L(index) => (*local_env)
                    .get(&view, *index)
                    .ok_or(ExecutionError::BadEnvironmentIndex(*index))?,
                Ref::G(index) => (*global_env)
                    .get(&view, *index)
                    .ok_or(ExecutionError::BadGlobalIndex(*index))?,
                Ref::V(_) => SynClosure::new(
                    view.alloc(HeapSyn::Atom {
                        evaluand: r.clone(),
                    })?
                    .as_ptr(),
                    self.closure.env(),
                ),
            };
            array.push(&view, closure);
        }

        view.from_saturation(array, next, self.annotation)
    }

    /// Return data into an appropriate branch handler
    ///
    /// Data is destructured for tag handlers but not for the default
    /// handler which is used for natives and unknown data constructors.
    fn return_data(
        &mut self,
        view: MutatorHeapView<'_>,
        tag: Tag,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        if let Some(continuation) = self.stack.pop() {
            match continuation {
                Continuation::Branch {
                    min_tag,
                    branch_table,
                    fallback,
                    environment,
                    annotation,
                    suppress_update,
                } => {
                    if let Some(body) = match_tag(tag, min_tag, branch_table.as_slice()) {
                        let env = if args.is_empty() {
                            // 0-arity constructor: reuse parent env directly
                            environment
                        } else {
                            self.env_from_data_args(view, args, environment)?
                        };
                        // When suppress_update is set on this Branch and the
                        // branch body is a bare local atom, suppress the next
                        // Update push. This prevents O(N) Update accumulation
                        // in tail-recursive conditionals (e.g. IF branches).
                        if suppress_update {
                            if let HeapSyn::Atom {
                                evaluand: Ref::L(_),
                            } = &*view.scoped(body)
                            {
                                self.suppress_next_update = true;
                            }
                        }
                        self.closure = SynClosure::new(body, env);
                    } else if let Some(body) = fallback {
                        self.closure = SynClosure::new(
                            body,
                            view.from_closure(self.closure.clone(), environment, self.annotation)?,
                        );
                    } else {
                        // Collect the tags that the branch table can handle
                        let expected_tags: Vec<u8> = (0..branch_table.len())
                            .filter(|i| branch_table.get(*i).flatten().is_some())
                            .map(|i| min_tag + i as u8)
                            .collect();
                        // Use the Branch annotation if valid, otherwise search
                        // the remaining stack for the nearest call context
                        let ann = if annotation.is_valid() {
                            annotation
                        } else {
                            self.nearest_stack_annotation(view)
                        };
                        return Err(ExecutionError::NoBranchForDataTag(ann, tag, expected_tags));
                    }
                }
                Continuation::Update { environment, index } => {
                    self.update(view, environment, index)?;
                }
                Continuation::ApplyTo { args, annotation } => {
                    // Block application: blocks can be applied as
                    // functions, delegating to __MERGE. This is the
                    // only data type with callable semantics — lists,
                    // numbers, strings, etc. have no natural
                    // application behaviour.
                    if tag == DataConstructor::Block.tag() {
                        let mut args = Array::from_slice(&view, args.as_slice());
                        args.push(&view, self.closure.clone());
                        self.push(view, Continuation::ApplyTo { args, annotation })?;
                        // Restore the application-site annotation so that any
                        // type-mismatch errors raised inside the MERGE wrapper
                        // (e.g. NoBranchForDataTag when a non-block is merged)
                        // carry the user's source location rather than a
                        // synthetic intrinsic label.
                        self.annotation = annotation;
                        self.closure = SynClosure::new(
                            view.atom(Ref::G(
                                intrinsics::index("MERGE")
                                    .expect("MERGE intrinsic must be registered"),
                            ))?
                            .as_ptr(),
                            self.env(view),
                        );
                    } else {
                        let type_name = DataConstructor::try_from(tag)
                            .map(|dc| dc.to_string())
                            .unwrap_or_else(|()| format!("data (tag {tag})"));
                        return Err(ExecutionError::NotCallable(annotation, type_name));
                    }
                }
                Continuation::DeMeta {
                    or_else,
                    environment,
                    ..
                } => {
                    self.closure = SynClosure::new(
                        or_else,
                        view.from_closure(self.closure.clone(), environment, self.annotation)?,
                    );
                }
            }
        } else if DataConstructor::is_io_constructor(tag) {
            // IO constructor at the top level with nothing left to
            // consume it — yield to the io-run driver loop rather than
            // terminating normally.  Both flags are set so that the
            // run() loop exits; the driver checks `yielded_io` to
            // distinguish the two cases.
            self.terminated = true;
            self.yielded_io = true;
        } else {
            self.terminated = true
        }

        Ok(())
    }

    /// Return function to either apply to args or default case branch
    fn return_fun(&mut self, view: MutatorHeapView) -> Result<(), ExecutionError> {
        if let Some(continuation) = self.stack.pop() {
            match continuation {
                Continuation::ApplyTo { args, annotation } => {
                    let excess = args.len() as isize - self.closure.arity() as isize;

                    match excess.cmp(&0) {
                        Ordering::Equal => {
                            self.closure = view.saturate(&self.closure, args.as_slice())?;
                        }
                        Ordering::Less => {
                            self.closure = view.partially_apply(&self.closure, args.as_slice())?;
                        }
                        Ordering::Greater => {
                            let (quorum, surplus) =
                                args.as_slice().split_at(args.len() - excess as usize);
                            self.closure = view.saturate(&self.closure, quorum)?;
                            self.push(
                                view,
                                Continuation::ApplyTo {
                                    args: Array::from_slice(&view, surplus),
                                    annotation,
                                },
                            )?;
                        }
                    }
                }
                Continuation::Branch {
                    min_tag,
                    branch_table,
                    fallback,
                    environment,
                    annotation,
                    ..
                } => {
                    // In a statically typed STG machine, functions
                    // never enter case continuations. With dynamic
                    // typing, code may case a value to inspect its
                    // type and discover it is a lambda. When this
                    // happens, we fall through to the default branch
                    // with the evaluated scrutinee bound.
                    if let Some(body) = fallback {
                        self.closure = SynClosure::new(
                            body,
                            view.from_closure(self.closure.clone(), environment, self.annotation)?,
                        );
                    } else {
                        let expected_tags: Vec<u8> = (0..branch_table.len())
                            .filter(|i| branch_table.get(*i).flatten().is_some())
                            .map(|i| min_tag + i as u8)
                            .collect();
                        let ann = if annotation.is_valid() {
                            annotation
                        } else {
                            self.nearest_stack_annotation(view)
                        };
                        return Err(ExecutionError::CannotReturnFunToCase(ann, expected_tags));
                    }
                }
                Continuation::Update { environment, index } => {
                    self.update(view, environment, index)?;
                }
                Continuation::DeMeta {
                    or_else,
                    environment,
                    ..
                } => {
                    self.closure = SynClosure::new(
                        or_else,
                        view.from_closure(self.closure.clone(), environment, self.annotation)?,
                    );
                }
            }
        } else {
            self.terminated = true
        }

        Ok(())
    }

    /// Find the nearest valid annotation on the remaining stack.
    ///
    /// Used as a fallback when the immediate continuation has no
    /// annotation, to attribute errors to their enclosing call context.
    fn nearest_stack_annotation(&self, view: MutatorHeapView) -> Smid {
        for cont in self.stack.iter().rev() {
            let smid = match cont {
                Continuation::Branch { annotation, .. }
                | Continuation::ApplyTo { annotation, .. } => *annotation,
                Continuation::Update { environment, .. }
                | Continuation::DeMeta { environment, .. } => {
                    let cont_env = view.scoped(*environment);
                    cont_env.annotation()
                }
            };
            if smid.is_valid() {
                return smid;
            }
        }
        Smid::default()
    }

    /// Returns a lazy iterator over stack trace annotations.
    ///
    /// This avoids allocation during error handling - the caller can collect
    /// if and when needed. The iterator handles deduplication inline without
    /// intermediate structures.
    pub fn stack_trace_iter<'a>(
        &'a self,
        view: &'a MutatorHeapView,
    ) -> impl Iterator<Item = Smid> + 'a {
        let mut prev = Smid::default();
        self.stack.iter().rev().filter_map(move |cont| {
            let smid = match cont {
                Continuation::Branch { annotation, .. }
                | Continuation::ApplyTo { annotation, .. } => *annotation,
                Continuation::Update { environment, .. }
                | Continuation::DeMeta { environment, .. } => {
                    let cont_env = view.scoped(*environment);
                    cont_env.annotation()
                }
            };

            if smid != Smid::default() && smid != prev {
                prev = smid;
                Some(smid)
            } else {
                None
            }
        })
    }

    /// Trace of annotations in execution stack
    ///
    /// Collects the lazy iterator into a pre-allocated Vec to minimise
    /// reallocation during error handling.
    pub fn stack_trace(&self, view: &MutatorHeapView) -> Vec<Smid> {
        // Pre-allocate with a reasonable estimate: after dedup, trace
        // depth is typically much smaller than stack size but we cap at
        // 64 to avoid over-allocation for deep stacks.
        let capacity = self.stack.len().min(64);
        let mut trace = Vec::with_capacity(capacity);
        trace.extend(self.stack_trace_iter(view));
        trace
    }
}

impl IntrinsicMachine for MachineState {
    /// Return regex cache
    fn rcache(&mut self) -> &mut LruCache<String, Regex> {
        &mut self.rcache
    }

    /// Used by intrinsics to update the closure after execution
    fn set_closure(&mut self, closure: SynClosure) -> Result<(), ExecutionError> {
        self.closure = closure;
        Ok(())
    }

    /// Get a navigator for resolving references
    fn nav<'guard>(&'guard self, view: MutatorHeapView<'guard>) -> HeapNavigator<'guard> {
        HeapNavigator {
            locals: view.scoped(self.env(view)),
            globals: view.scoped(self.globals),
            view,
        }
    }

    /// The empty root environment
    fn root_env(&self) -> RefPtr<EnvFrame> {
        self.root_env
    }

    /// The current environment
    fn env(&self, _view: MutatorHeapView) -> RefPtr<EnvFrame> {
        self.closure.env()
    }

    /// Read-only access to the symbol pool
    fn symbol_pool(&self) -> &SymbolPool {
        &self.symbol_pool
    }

    /// Mutable access to the symbol pool for interning new symbols
    fn symbol_pool_mut(&mut self) -> &mut SymbolPool {
        &mut self.symbol_pool
    }

    /// Current source annotation for error reporting
    fn annotation(&self) -> Smid {
        self.annotation
    }

    /// Force a closure to WHNF by saving/restoring the machine state
    /// and running the step loop.
    ///
    /// Only valid during BIF execution — panics if the BIF context
    /// pointers are not set.
    fn force_to_whnf(
        &mut self,
        view: MutatorHeapView<'_>,
        closure: SynClosure,
    ) -> Result<SynClosure, ExecutionError> {
        // SAFETY: bif_intrinsics was set by handle_instruction just
        // before calling bif.execute() and points into the still-live
        // Machine::step() call frame.  Intrinsics are shared (&)
        // references so no aliasing concern.
        let intrinsics_ptr = self
            .bif_intrinsics
            .expect("force_to_whnf called outside BIF execution");
        let intrinsics_len = self.bif_intrinsics_len;
        let intrinsics: &[&dyn StgIntrinsic] =
            unsafe { std::slice::from_raw_parts(intrinsics_ptr, intrinsics_len) };

        // Use a private NullEmitter and Metrics to avoid aliasing the
        // caller's mutable references (which would be UB).  We are
        // only forcing a thunk to WHNF — emitter output is discarded
        // and metrics are unimportant.
        let mut null_emitter = crate::eval::emit::NullEmitter;
        let mut force_metrics = Metrics::default();

        // Save the current machine state.
        let saved_stack = std::mem::take(&mut self.stack);
        let saved_closure = std::mem::replace(&mut self.closure, closure);
        let saved_terminated = self.terminated;
        let saved_annotation = self.annotation;
        self.terminated = false;

        // Run the step loop until the thunk reaches WHNF.
        let step_limit = 100_000;
        let mut steps = 0;
        while !self.terminated && !self.yielded_io {
            steps += 1;
            if steps > step_limit {
                // Restore state before returning error.
                self.stack = saved_stack;
                self.closure = saved_closure;
                self.terminated = saved_terminated;
                self.annotation = saved_annotation;
                return Err(ExecutionError::Panic(
                    "force_to_whnf exceeded step limit".to_string(),
                ));
            }
            self.handle_instruction(view, &mut null_emitter, intrinsics, &mut force_metrics)?;
        }

        let result = self.closure.clone();

        // Restore the saved state.
        self.stack = saved_stack;
        self.closure = saved_closure;
        self.terminated = saved_terminated;
        self.annotation = saved_annotation;

        Ok(result)
    }
}

/// MachineState contains all the garbage collection roots
impl GcScannable for MachineState {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        if marker.mark(self.globals) {
            out.push(ScanPtr::from_non_null(scope, self.globals));
        }

        out.push(ScanPtr::new(scope, &self.closure));

        // Continuations are stored inline in the Vec (off the eucalypt heap).
        // Scan their internal heap pointers directly instead of marking the
        // continuations themselves as heap objects.
        for cont in &self.stack {
            cont.scan(scope, marker, out);
        }

        // Stashed closures must also be scanned so that the GC does not
        // collect heap objects that the io-run driver is holding between
        // machine.run() calls.
        for stashed in &self.stash {
            out.push(ScanPtr::new(scope, stashed));
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        if let Some(new) = heap.forwarded_to(self.root_env) {
            self.root_env = new;
        }
        if let Some(new) = heap.forwarded_to(self.globals) {
            self.globals = new;
        }
        self.closure.scan_and_update(heap);
        // Update forwarded pointers within each continuation's internal fields.
        for cont in &mut self.stack {
            cont.scan_and_update(heap);
        }
        // Update forwarded pointers in stashed closures.
        for stashed in &mut self.stash {
            stashed.scan_and_update(heap);
        }
    }
}

pub struct MachineSettings {
    pub trace_steps: bool,
    pub dump_heap: bool,
}

/// An STG machine variant using cactus environment
///
/// GC roots (outside heap, pointing in) are:
/// - closure
/// - globals
/// - stack
pub struct Machine<'a> {
    /// Main VM Memory - immutable ref and interior mutability
    heap: Heap,
    /// The current state of the machine - operated on as mutable ref
    state: MachineState,
    /// Intrinsics (actions with access into machine)
    intrinsics: Vec<&'a dyn StgIntrinsic>,
    /// Emitter to send output and error events to
    emitter: Box<dyn Emitter + 'a>,
    /// Whether to trace every step to stderr
    settings: MachineSettings,
    /// Metrics
    metrics: Metrics,
    /// Clock
    clock: Clock,
}

impl<'a> Machine<'a> {
    /// Construct a machine to evaluate `code`
    pub fn new(
        emitter: Box<dyn Emitter + 'a>,
        trace_steps: bool,
        heap_limit_mib: Option<usize>,
        dump_heap: bool,
    ) -> Self {
        Machine {
            heap: heap_limit_mib.map(Heap::with_limit).unwrap_or_default(),
            state: Default::default(),
            intrinsics: vec![],
            emitter,
            settings: MachineSettings {
                trace_steps,
                dump_heap,
            },
            metrics: Metrics::default(),
            clock: Clock::default(),
        }
    }

    /// Replace the symbol pool (used during initialisation)
    pub fn set_symbol_pool(&mut self, pool: SymbolPool) {
        self.state.symbol_pool = pool;
    }

    /// Read-only access to the symbol pool for resolving `SymbolId` to text.
    pub fn symbol_pool(&self) -> &SymbolPool {
        &self.state.symbol_pool
    }

    /// Intern a symbol string into the machine's symbol pool and return its ID.
    ///
    /// Used by the io-run driver to pre-register symbols (e.g. `"stdout"`,
    /// `"stderr"`, `"exit-code"`) before they are embedded in heap objects by
    /// a mutator.  Pre-interning ensures the IDs assigned during mutator heap
    /// construction are already present in the machine's pool, so that later
    /// LOOKUP and render operations (which use the machine pool) can resolve
    /// them.
    pub fn intern_symbol(&mut self, s: &str) -> crate::eval::memory::symbol::SymbolId {
        self.state.symbol_pool.intern(s)
    }

    /// Access the heap for allocation
    pub fn heap(&self) -> &Heap {
        &self.heap
    }

    /// The root (empty) environment — used by the io-run driver loop to
    /// root newly constructed closures that have no enclosing environment.
    pub fn root_env(&self) -> RefPtr<EnvFrame> {
        self.state.root_env
    }

    /// Return the globals environment frame.
    ///
    /// The globals frame holds the closures for all global bindings
    /// (intrinsics, prelude, etc.) indexed by their `G(i)` ref.
    pub fn globals_env(&self) -> RefPtr<EnvFrame> {
        self.state.globals
    }

    /// Access the metrics (ticks, allocs, etc.)
    pub fn metrics(&self) -> &Metrics {
        &self.metrics
    }

    /// Get heap statistics
    pub fn heap_stats(&self) -> HeapStats {
        self.heap.stats()
    }

    /// Return clock for access to GC timings
    pub fn clock(&self) -> &Clock {
        &self.clock
    }

    /// Current source annotation for error reporting
    pub fn annotation(&self) -> Smid {
        self.state.annotation
    }

    /// Create a mutator heap view for heap access
    fn view(&'a self) -> MutatorHeapView<'a> {
        MutatorHeapView::new(&self.heap)
    }

    /// Split reference into separate facilities (state, heap)
    #[inline]
    fn facilities(
        &mut self,
    ) -> (
        &mut MachineState,
        MutatorHeapView<'_>,
        &mut dyn Emitter,
        &mut Metrics,
        &MachineSettings,
        &[&dyn StgIntrinsic],
    ) {
        (
            &mut self.state,
            MutatorHeapView::new(&self.heap),
            self.emitter.as_mut(),
            &mut self.metrics,
            &self.settings,
            &self.intrinsics,
        )
    }

    /// Get a navigator for resolving references
    fn nav(&self) -> HeapNavigator<'_> {
        self.state.nav(MutatorHeapView::new(&self.heap))
    }

    /// Apply a mutation which needs heap access
    pub fn mutate<I, O, M>(&self, mutator: M, input: I) -> Result<O, ExecutionError>
    where
        I: Sized,
        O: Sized,
        M: Mutator<Input = I, Output = O>,
    {
        let view = MutatorHeapView::new(&self.heap);
        mutator.run(&view, input)
    }

    /// Initialise globals and starting closure
    pub fn initialise(
        &mut self,
        root_env: RefPtr<EnvFrame>,
        globals: RefPtr<EnvFrame>,
        closure: SynClosure,
        intrinsics: Vec<&'a dyn StgIntrinsic>,
    ) -> Result<(), ExecutionError> {
        self.clock.switch(ThreadOccupation::Initialisation);
        self.intrinsics = intrinsics.clone();
        self.state.root_env = root_env;
        self.state.set_globals(globals)?;
        self.state.set_closure(closure)
    }

    /// Execute one step
    #[inline]
    pub fn step(&mut self) -> Result<(), ExecutionError> {
        let (state, view, emitter, metrics, settings, intrinsics) = self.facilities();

        if settings.trace_steps {
            let stack = state.stack.iter().rev().map(|c| c.to_string()).format(":");
            eprintln!("M ⟪{}⟫ <{}>", ScopeAndClosure(&view, &state.closure), stack);
        }

        metrics.tick();
        let stack_len = state.stack.len();
        let prev_max = metrics.max_stack();
        metrics.stack(stack_len);
        // Diagnostic: dump stack composition when a new max is reached
        if std::env::var("EU_STACK_DIAG").is_ok() && stack_len > prev_max && stack_len > 5 {
            let counts = state.stack.iter().fold(
                (0usize, 0usize, 0usize, 0usize),
                |(branch, update, apply, demeta), cont| match cont {
                    super::cont::Continuation::Branch { .. } => (branch + 1, update, apply, demeta),
                    super::cont::Continuation::Update { .. } => (branch, update + 1, apply, demeta),
                    super::cont::Continuation::ApplyTo { .. } => {
                        (branch, update, apply + 1, demeta)
                    }
                    super::cont::Continuation::DeMeta { .. } => (branch, update, apply, demeta + 1),
                },
            );
            eprintln!(
                "STACK_MAX depth={} Branch={} Update={} ApplyTo={} DeMeta={}",
                stack_len, counts.0, counts.1, counts.2, counts.3
            );
        }

        state
            .handle_instruction(view, emitter, intrinsics, metrics)
            .map_err(|e| {
                ExecutionError::Traced(
                    Box::new(e),
                    state.nav(view).env_trace(),
                    state.stack_trace(&view),
                )
            })
    }

    /// Run the machine until termination or step limit
    pub fn run(&mut self, limit: Option<usize>) -> Result<Option<u8>, ExecutionError> {
        self.clock.switch(ThreadOccupation::Mutator);

        // Use a countdown counter instead of modulo check on every
        // tick. This replaces `ticks % 500 == 0` with a simple
        // decrement-and-compare, avoiding integer division on every
        // VM step.
        let gc_check_freq: u32 = 500;
        let mut gc_countdown: u32 = gc_check_freq;

        while !self.state.terminated {
            if let Some(limit) = limit {
                if self.metrics.ticks() as usize >= limit {
                    return Err(ExecutionError::DidntTerminate(limit));
                }
            }

            gc_countdown -= 1;
            if gc_countdown == 0 {
                gc_countdown = gc_check_freq;
                if self.heap().policy_requires_collection() {
                    collect::collect(
                        &mut self.state,
                        &mut self.heap,
                        &mut self.clock,
                        self.settings.dump_heap,
                    );
                    self.clock.switch(ThreadOccupation::Mutator);
                }
            }

            self.step()?;
        }

        collect::collect(
            &mut self.state,
            &mut self.heap,
            &mut self.clock,
            self.settings.dump_heap,
        );

        self.clock.stop();

        Ok(self.exit_code())
    }

    /// Determine an exit code if the machine is terminated
    ///
    /// Numbers in 0-255 are used as is
    /// Unit becomes zero
    /// Anything else is 1
    pub fn exit_code(&self) -> Option<u8> {
        // A view for mutable heap access
        let view = self.view();

        let code = view.scoped(self.state.closure.code());

        if self.state.terminated() {
            Some(match &*code {
                HeapSyn::Atom { evaluand: r } => match self.nav().resolve_native(r) {
                    Ok(Native::Num(n)) => {
                        if let Some(integer) = n.as_i64() {
                            let rc: Result<u8, _> = integer.try_into();
                            rc.unwrap_or(1)
                        } else {
                            1
                        }
                    }
                    _ => 1,
                },
                HeapSyn::Cons { tag, .. } if *tag == 0 => 0,
                _ => 1,
            })
        } else {
            None
        }
    }

    /// Recover the emitter after a run
    pub fn take_emitter(&mut self) -> Box<dyn Emitter + 'a> {
        let mut ret: Box<dyn Emitter + 'a> = Box::new(NullEmitter);
        std::mem::swap(&mut ret, &mut self.emitter);
        ret
    }

    /// Return a clone of the machine's current closure.
    ///
    /// Used by the headless-render fallback in `io_run.rs` to obtain the
    /// final evaluated value when the machine terminates without yielding
    /// an IO constructor.
    pub fn current_closure(&self) -> SynClosure {
        self.state.closure.clone()
    }

    /// Push a closure onto the GC stash.
    ///
    /// Used by the io-run driver to keep closures alive across `machine.run()`
    /// calls.  Each `stash_push` must be paired with a `stash_pop` once the
    /// closure is no longer needed.
    pub fn stash_push(&mut self, closure: SynClosure) {
        self.state.stash.push(closure);
    }

    /// Pop the most-recently stashed closure.
    ///
    /// Panics if the stash is empty — callers must ensure balanced push/pop.
    pub fn stash_pop(&mut self) -> SynClosure {
        self.state
            .stash
            .pop()
            .expect("io-run stash underflow: unbalanced stash_push/stash_pop")
    }

    /// Peek at a stashed closure by depth from the top (0 = top).
    ///
    /// Panics if the stash has fewer than `depth + 1` entries.
    pub fn stash_peek(&self, depth: usize) -> SynClosure {
        let len = self.state.stash.len();
        assert!(
            depth < len,
            "io-run stash peek out of bounds: depth={depth} len={len}"
        );
        self.state.stash[len - 1 - depth].clone()
    }

    /// Has the machine terminated
    pub fn terminated(&self) -> bool {
        self.state.terminated()
    }

    /// Has the machine yielded on an IO constructor?
    ///
    /// When true the machine has also set `terminated` so that
    /// `run()` exits.  The io-run driver checks this flag to
    /// distinguish IO yield from normal termination.
    pub fn io_yielded(&self) -> bool {
        self.state.yielded_io()
    }

    /// Return the IO constructor tag the machine yielded on.
    ///
    /// Returns `None` when the machine has not yielded on an IO
    /// constructor.  The caller is responsible for checking
    /// `io_yielded()` first.
    pub fn yielded_io_tag(&self) -> Option<Tag> {
        if !self.state.yielded_io() {
            return None;
        }
        let view = self.view();
        let code = view.scoped(self.state.closure.code());
        if let HeapSyn::Cons { tag, .. } = &*code {
            Some(*tag)
        } else {
            None
        }
    }

    /// Resolve the argument closures of the IO constructor the machine
    /// yielded on.
    ///
    /// Returns `None` when the machine has not yielded on an IO
    /// constructor.  The returned `Vec<SynClosure>` holds one closure
    /// per constructor field, in declaration order.
    pub fn yielded_io_args(&self) -> Option<Vec<SynClosure>> {
        if !self.state.yielded_io() {
            return None;
        }
        let view = self.view();
        let code = view.scoped(self.state.closure.code());
        if let HeapSyn::Cons { args, .. } = &*code {
            let env = view.scoped(self.state.closure.env());
            let globals = view.scoped(self.state.globals);
            let resolved: Result<Vec<_>, _> = args
                .iter()
                .map(|r| match r {
                    Ref::L(i) => (*env)
                        .get(&view, *i)
                        .ok_or(ExecutionError::BadEnvironmentIndex(*i)),
                    Ref::G(i) => (*globals)
                        .get(&view, *i)
                        .ok_or(ExecutionError::BadGlobalIndex(*i)),
                    Ref::V(_) => {
                        let ptr = view
                            .alloc(HeapSyn::Atom {
                                evaluand: r.clone(),
                            })
                            .map(|p| p.as_ptr());
                        ptr.map(|p| SynClosure::new(p, self.state.root_env))
                    }
                })
                .collect();
            resolved.ok()
        } else {
            None
        }
    }

    /// Resume execution with a new closure after an IO yield.
    ///
    /// Clears the `terminated` and `yielded_io` flags and sets the
    /// machine's current closure to `new_closure`.  Calling `run()`
    /// after this will continue execution from the new closure.
    ///
    /// # Panics
    ///
    /// Panics if the machine has not yielded on an IO constructor
    /// (i.e. `io_yielded()` is false).
    pub fn resume(&mut self, new_closure: SynClosure) {
        assert!(
            self.state.yielded_io,
            "resume() called on a machine that has not yielded on an IO constructor"
        );
        self.state.terminated = false;
        self.state.yielded_io = false;
        self.state.closure = new_closure;
    }

    /// Resume execution with a new closure after the io-run driver loop has
    /// completed and the final value needs to be rendered.
    ///
    /// Unlike `resume()`, this method does not require the machine to be in
    /// the IO yield state.  It is intended for use by the io-run driver after
    /// `io_run()` returns the final `IoReturn` value closure, to re-enter the
    /// machine for the render step.
    pub fn resume_for_render(&mut self, new_closure: SynClosure) {
        self.state.terminated = false;
        self.state.yielded_io = false;
        self.state.closure = new_closure;
    }

    /// Evaluate a closure to WHNF while the machine is in IO yield state.
    ///
    /// Temporarily suspends the IO yield state, evaluates `closure` to
    /// WHNF (normal termination), and restores the yielded IO state
    /// (including the original `state.closure`).  The continuation stack
    /// must be empty when called (which is always the case at an IO yield).
    ///
    /// Used by the io-run driver to force-evaluate spec block field
    /// closures that contain unevaluated thunks (e.g. `lookup-or` calls
    /// in `io.shell-with` / `io.exec-with` spec blocks).
    ///
    /// # Errors
    ///
    /// Returns `Err` if the sub-evaluation encounters a machine error or
    /// if it unexpectedly yields on an IO constructor (spec block fields
    /// must not produce IO).
    pub fn evaluate_to_whnf_for_io(
        &mut self,
        closure: SynClosure,
    ) -> Result<SynClosure, ExecutionError> {
        assert!(
            self.state.yielded_io,
            "evaluate_to_whnf_for_io called when machine is not in IO yield state"
        );
        assert!(
            self.state.stack.is_empty(),
            "evaluate_to_whnf_for_io called with non-empty continuation stack"
        );

        // Push the current IO yield closure onto the GC stash so that the
        // collector can trace and update its heap pointers if evacuation occurs
        // during self.run() below.  Without this, `saved_closure` is an
        // invisible Rust-stack root: the GC does not scan it, so any objects
        // it references that reside in a fragmented candidate block will be
        // evacuated and their forwarding pointers set — but `saved_closure`
        // still holds the old (now-dead) addresses, causing a SIGSEGV when
        // we later restore it as the machine closure.
        self.stash_push(self.state.closure.clone());

        // Temporarily evaluate the given closure to WHNF.
        self.state.terminated = false;
        self.state.yielded_io = false;
        self.state.closure = closure;

        let run_result = self.run(None);

        // Pop the (possibly-updated) saved closure from the stash.  The GC
        // may have rewritten its internal heap pointers during run(), so we
        // must use the stash copy rather than the Rust-stack copy.
        let saved_closure = self.stash_pop();

        // Propagate any run error only after restoring the GC-safe saved
        // closure, to leave the machine in a consistent state.
        run_result?;

        // Capture the result before restoring state.
        let sub_yielded = self.state.yielded_io;
        let result = self.state.closure.clone();

        // Restore the IO yield state unconditionally.
        self.state.terminated = true;
        self.state.yielded_io = true;
        self.state.closure = saved_closure;

        if sub_yielded {
            return Err(ExecutionError::Panic(
                "spec block field evaluation unexpectedly yielded an IO constructor".to_string(),
            ));
        }

        Ok(result)
    }

    /// Assertion helper for machine unit tests
    #[cfg(test)]
    pub fn native_return(&self) -> Option<Native> {
        let view = self.view();

        let code = view.scoped(self.state.closure.code());

        if self.state.terminated() {
            if let HeapSyn::Atom { evaluand: r } = &*code {
                self.nav().resolve_native(r).ok()
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Retrieve native string return value if it exists
    #[cfg(test)]
    pub fn string_return(&self) -> Option<String> {
        let view = self.view();

        let code = view.scoped(self.state.closure.code());

        if self.state.terminated() {
            if let HeapSyn::Atom { evaluand: r } = &*code {
                if let Ok(Native::Str(rp)) = self.nav().resolve_native(r) {
                    Some((*view.scoped(rp)).as_str().to_string())
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    #[cfg(test)]
    pub fn bool_return(&self) -> Option<bool> {
        let view = self.view();

        let code = view.scoped(self.state.closure.code());

        if self.state.terminated() {
            if let HeapSyn::Cons { tag, .. } = &*code {
                if *tag == DataConstructor::BoolTrue.tag() {
                    Some(true)
                } else if *tag == DataConstructor::BoolFalse.tag() {
                    Some(false)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    #[cfg(test)]
    pub fn unit_return(&self) -> bool {
        let view = self.view();

        let code = view.scoped(self.state.closure.code());

        if self.state.terminated() {
            if let HeapSyn::Cons { tag, .. } = &*code {
                *tag == DataConstructor::Unit.tag()
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Retrieve any events captured by the emitter
    #[cfg(test)]
    pub fn captures(&self) -> &[crate::eval::emit::Event] {
        self.emitter.captures()
    }
}

#[cfg(test)]
pub mod tests {

    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::eval::machine::env::{EnvFrame, SynClosure};
    use crate::eval::machine::intrinsic::StgIntrinsic;

    use crate::eval::memory::alloc::ScopedAllocator;
    use crate::eval::memory::loader::load;
    use crate::eval::memory::mutator::{Mutator, MutatorHeapView};
    use crate::eval::memory::symbol::SymbolPool;
    use crate::eval::memory::syntax::{Native, RefPtr};
    use crate::eval::stg::syntax::{ex::*, StgSyn};
    use crate::eval::{emit::DebugEmitter, stg::syntax::dsl::*};

    use super::Machine;

    lazy_static! {
        static ref EMPTY_INTRINSICS: Vec<Box<dyn StgIntrinsic>> = vec![];
    }

    pub struct Init;

    impl Mutator for Init {
        type Input = ();
        type Output = RefPtr<EnvFrame>;

        fn run(
            &self,
            view: &MutatorHeapView,
            _input: Self::Input,
        ) -> Result<Self::Output, crate::eval::error::ExecutionError> {
            Ok(view.alloc(EnvFrame::default())?.as_ptr())
        }
    }

    pub struct Load {
        syntax: Rc<StgSyn>,
        pool: RefCell<SymbolPool>,
    }

    impl Mutator for Load {
        type Input = RefPtr<EnvFrame>;
        type Output = SynClosure;

        fn run(
            &self,
            view: &MutatorHeapView,
            input: Self::Input,
        ) -> Result<Self::Output, crate::eval::error::ExecutionError> {
            let mut pool = self.pool.borrow_mut();
            Ok(SynClosure::new(
                load(view, &mut pool, self.syntax.clone())?,
                input,
            ))
        }
    }

    fn machine(syn: Rc<StgSyn>) -> Machine<'static> {
        let mut m = Machine::new(Box::new(DebugEmitter::default()), true, None, false);
        let blank = m.mutate(Init, ()).unwrap();
        let closure = m
            .mutate(
                Load {
                    syntax: syn,
                    pool: RefCell::new(SymbolPool::new()),
                },
                blank,
            )
            .unwrap();
        m.initialise(blank, blank, closure, vec![]).unwrap();
        m
    }

    #[test]
    pub fn test_terminate_with_number() {
        let mut machine = machine(atom(num(9)));
        machine.step().unwrap();
        assert!(machine.terminated());
    }

    #[test]
    pub fn test_terminate_with_true() {
        let mut machine = machine(t());
        machine.step().unwrap();
        assert!(machine.terminated());
    }

    #[test]
    pub fn test_terminate_with_unit() {
        let mut machine = machine(unit());
        machine.step().unwrap();
        assert!(machine.terminated());
    }

    #[test]
    pub fn test_identity() {
        let mut m = machine(let_(vec![i()], app(lref(0), vec![num(42)])));
        m.run(Some(20)).unwrap();
        assert_eq!(m.native_return(), Some(Native::Num(42.into())));
    }

    #[test]
    pub fn test_ski() {
        let syn = letrec_(
            vec![
                i(),
                s(),
                k(),
                lambda(0, app(lref(1), vec![lref(2), lref(2)])), //s(k,k)
            ],
            app(lref(3), vec![str("foo")]),
        );

        let mut m = machine(syn);
        m.run(Some(30)).unwrap();
        assert_eq!(m.string_return(), Some("foo".to_string()));
    }

    #[test]
    pub fn test_update() {
        let syn = letrec_(
            vec![i(), value(unit()), thunk(app(lref(0), vec![lref(1)]))],
            local(2),
        );

        let mut m = machine(syn);
        m.run(Some(20)).unwrap();
        assert!(m.unit_return());
    }

    #[test]
    pub fn test_case_bool_toggle() {
        let syn = letrec_(vec![not(), value(t())], app(lref(0), vec![lref(1)]));
        let mut m = machine(syn);
        m.run(Some(20)).unwrap();
        assert_eq!(m.bool_return(), Some(false));
    }

    #[test]
    pub fn test_demeta_meta() {
        let syn = letrec_(
            vec![value(t()), value(with_meta(lref(0), num(9))), meta()],
            app(lref(2), vec![lref(1)]),
        );

        let mut m = machine(syn);
        m.run(Some(20)).unwrap();
        assert_eq!(m.bool_return(), Some(true));
    }

    #[test]
    pub fn test_demeta_nometa() {
        let syn = letrec_(
            vec![value(atom(num(9))), meta()],
            app(lref(1), vec![lref(0)]),
        );

        let mut m = machine(syn);
        m.run(Some(20)).unwrap();
        assert!(m.unit_return());
    }

    /// Verify that a bare IoReturn constructor yields rather than terminates.
    #[test]
    pub fn test_io_return_yields() {
        use crate::eval::stg::tags::DataConstructor;

        // Construct IoReturn(unit, num(42)):
        //   letrec [world = Unit, value = 42] in IoReturn(lref(1), lref(0))
        let syn = letrec_(
            vec![value(unit()), value(atom(num(42i64)))],
            data(DataConstructor::IoReturn.tag(), vec![lref(1), lref(0)]),
        );

        let mut m = machine(syn);
        m.run(Some(20)).unwrap();

        // Machine should have terminated (so the run loop exited)…
        assert!(m.terminated(), "machine must have terminated");
        // …but specifically due to an IO yield, not normal termination
        assert!(
            m.io_yielded(),
            "machine must have yielded on IO constructor"
        );

        // The yielded tag must be IoReturn (12)
        assert_eq!(
            m.yielded_io_tag(),
            Some(DataConstructor::IoReturn.tag()),
            "yielded tag must be IoReturn"
        );

        // There must be two args: world (Unit) and value (42)
        let args = m.yielded_io_args().expect("must have args");
        assert_eq!(args.len(), 2, "IoReturn must have 2 args");
    }

    /// Verify that a bare IoBind constructor yields.
    #[test]
    pub fn test_io_bind_yields() {
        use crate::eval::stg::tags::DataConstructor;

        // IoBind(unit, unit, unit) — minimal: world, action, cont
        let syn = letrec_(
            vec![value(unit()), value(unit()), value(unit())],
            data(
                DataConstructor::IoBind.tag(),
                vec![lref(2), lref(1), lref(0)],
            ),
        );

        let mut m = machine(syn);
        m.run(Some(20)).unwrap();

        assert!(m.io_yielded(), "IoBind must yield");
        assert_eq!(m.yielded_io_tag(), Some(DataConstructor::IoBind.tag()));

        let args = m.yielded_io_args().expect("must have args");
        assert_eq!(args.len(), 3, "IoBind must have 3 args");
    }

    /// Verify that normal constructors still terminate (not yield).
    #[test]
    pub fn test_non_io_constructor_terminates_not_yields() {
        // A ListNil constructor should NOT trigger an IO yield
        let syn = nil();

        let mut m = machine(syn);
        m.run(Some(10)).unwrap();

        assert!(m.terminated(), "must be terminated");
        assert!(!m.io_yielded(), "ListNil must not trigger IO yield");
        assert_eq!(m.yielded_io_tag(), None);
    }

    /// Verify that resume() resets the machine and allows continuation.
    #[test]
    pub fn test_resume_after_io_yield() {
        use crate::eval::memory::loader::load;
        use crate::eval::memory::mutator::{Mutator, MutatorHeapView};
        use crate::eval::memory::symbol::SymbolPool;
        use crate::eval::stg::tags::DataConstructor;

        // First run: yield on IoReturn
        let syn = letrec_(
            vec![value(unit()), value(atom(num(99i64)))],
            data(DataConstructor::IoReturn.tag(), vec![lref(1), lref(0)]),
        );

        let mut m = machine(syn);
        m.run(Some(20)).unwrap();
        assert!(m.io_yielded(), "first run must yield on IO");

        // Resume with a plain number closure
        struct LoadNum;
        impl Mutator for LoadNum {
            type Input = RefPtr<EnvFrame>;
            type Output = SynClosure;
            fn run(
                &self,
                view: &MutatorHeapView,
                input: Self::Input,
            ) -> Result<Self::Output, crate::eval::error::ExecutionError> {
                let mut pool = SymbolPool::new();
                Ok(SynClosure::new(
                    load(view, &mut pool, atom(num(7i64)))?,
                    input,
                ))
            }
        }

        let blank = m.mutate(crate::eval::machine::vm::tests::Init, ()).unwrap();
        let new_closure = m.mutate(LoadNum, blank).unwrap();
        m.resume(new_closure);

        // After resume the machine should be ready to run again
        assert!(
            !m.terminated(),
            "after resume, machine must not be terminated"
        );
        assert!(!m.io_yielded(), "after resume, io_yielded must be cleared");

        m.run(Some(20)).unwrap();
        assert!(m.terminated());
        assert!(!m.io_yielded(), "second run must not yield on IO");
        assert_eq!(m.native_return(), Some(Native::Num(7.into())));
    }
}
