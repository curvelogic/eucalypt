//! Intrinsic traits

use std::rc::Rc;

use lru::LruCache;
use regex::Regex;

use serde_json::Number;

use crate::{
    common::sourcemap::Smid,
    eval::stg::wrap::wrap,
    eval::{
        bytecode::BcValue,
        emit::Emitter,
        error::ExecutionError,
        intrinsics,
        memory::{
            infotable::InfoTable,
            mutator::MutatorHeapView,
            symbol::SymbolPool,
            syntax::{Native, Ref},
        },
        stg::{
            syntax::{dsl, StgSyn},
            tags::Tag,
        },
    },
};

/// A closure handle passed across the intrinsic ABI (BV1 §5.5).
///
/// Historically code-type-erased over two coexisting engines — the deleted
/// HeapSyn tree-walk machine's `SynClosure` (`Heap`) and the bytecode
/// engine's `BcValue` (`Byte`) — with each engine downcasting to its own
/// variant in the `IntrinsicMachine` method impls. The Phase 4 collapse
/// (eu-oufc) deleted the `Heap` variant along with HeapSyn; `Byte` is now
/// the only one, kept as a named wrapper (rather than a bare type alias for
/// `BcValue`) since nothing outside this module and `bytecode::machine`
/// pattern-matches its variants directly.
#[derive(Clone)]
pub enum AbiClosure {
    /// A bytecode runtime value — a closure or a bare native (a resolved ref
    /// may be either, hence `BcValue` rather than a closure; BV1 REFINEMENT A).
    Byte(BcValue),
}

impl AbiClosure {
    /// Unwrap the bytecode value.
    pub fn expect_byte(self) -> BcValue {
        match self {
            AbiClosure::Byte(v) => v,
        }
    }

    /// The closure's arity (0 for a saturated/value closure).
    pub fn arity(&self) -> u8 {
        match self {
            AbiClosure::Byte(BcValue::Closure(c)) => c.arity(),
            AbiClosure::Byte(BcValue::Native(_)) => 0,
        }
    }
}

/// Machine interface exposed to intrinsic implementations
pub trait IntrinsicMachine {
    /// Access the regex cache
    fn rcache(&mut self) -> &mut LruCache<String, Regex>;

    /// Read-only access to the symbol pool
    fn symbol_pool(&self) -> &SymbolPool;

    /// Mutable access to the symbol pool for interning new symbols
    fn symbol_pool_mut(&mut self) -> &mut SymbolPool;

    /// Current source annotation for error reporting
    fn annotation(&self) -> Smid;

    // ── Code-type-neutral primitives (BV1 §5.5) ─────────────────────
    //
    // These express argument resolution and result construction without
    // naming the code type, so each engine implements the same intrinsic
    // layer over its own runtime value representation. Abstract (no
    // default body) since the bytecode engine is the sole implementor —
    // the HeapSyn defaults these methods used to carry were deleted by the
    // Phase 4 collapse (eu-oufc) along with `SynClosure`/`HeapSyn`.

    /// Resolve a ref to a native value, looking through atom indirections.
    fn resolve_native(
        &self,
        view: MutatorHeapView<'_>,
        arg: &Ref,
    ) -> Result<Native, ExecutionError>;

    /// Set the machine result to a native value (scalar return).
    fn return_native(
        &mut self,
        view: MutatorHeapView<'_>,
        native: Native,
    ) -> Result<(), ExecutionError>;

    /// Set the machine result to the unit value.
    fn return_unit(&mut self, view: MutatorHeapView<'_>) -> Result<(), ExecutionError>;

    /// Set the machine result to a `BoxedNumber` data value.
    fn return_boxed_num(
        &mut self,
        view: MutatorHeapView<'_>,
        n: Number,
    ) -> Result<(), ExecutionError>;

    /// Set the machine result to a boolean, reusing the global TRUE/FALSE
    /// closures rather than allocating a fresh data cell.
    fn return_bool(&mut self, view: MutatorHeapView<'_>, b: bool) -> Result<(), ExecutionError>;

    // ── Neutral closure operations (BV1 §5.5, Increment C) ──────────

    /// Resolve a ref to a closure handle (following the resolve rules:
    /// locals/globals looked up, `V` wrapped in an atom closure).
    fn resolve_closure(
        &self,
        view: MutatorHeapView<'_>,
        arg: &Ref,
    ) -> Result<AbiClosure, ExecutionError>;

    /// Resolve a ref to a closure that must be callable (errors on `V`).
    fn resolve_callable_closure(
        &self,
        view: MutatorHeapView<'_>,
        arg: &Ref,
    ) -> Result<AbiClosure, ExecutionError>;

    /// Set the machine result to a previously-resolved closure handle.
    fn set_result(&mut self, closure: AbiClosure) -> Result<(), ExecutionError>;

    /// Tail-call the global at `global_idx` applied to `arg_refs` (resolved in
    /// the current environment), replacing the machine's closure with that
    /// application. Used by intrinsics that delegate to another global (e.g.
    /// `RENDER_TO_STRING` → `RENDER_DOC`) without runtime code synthesis on the
    /// bytecode path.
    fn tail_apply_global(
        &mut self,
        view: MutatorHeapView<'_>,
        global_idx: usize,
        arg_refs: &[Ref],
    ) -> Result<(), ExecutionError>;

    /// Force a closure handle to WHNF.
    fn force(&mut self, closure: AbiClosure) -> Result<AbiClosure, ExecutionError>;

    /// The data-constructor tag of a (WHNF) closure handle, if it is a
    /// data constructor; `None` otherwise.
    fn data_tag(&self, view: MutatorHeapView<'_>, closure: &AbiClosure) -> Option<Tag>;

    /// The `idx`-th field of a data-constructor closure, resolved within
    /// the closure's environment; `None` if not a constructor / out of range.
    fn data_field(
        &self,
        view: MutatorHeapView<'_>,
        closure: &AbiClosure,
        idx: usize,
    ) -> Option<AbiClosure>;

    /// The `idx`-th field of a data-constructor closure read as a native
    /// value (used by native-list iteration).
    fn field_native(
        &self,
        view: MutatorHeapView<'_>,
        closure: &AbiClosure,
        idx: usize,
    ) -> Option<Native>;

    /// The native payload of a WHNF value: a bare native (an `Atom`) or a
    /// boxed scalar's field-0 payload (a `Cons`, e.g. `BoxedNumber`). `None`
    /// for a non-scalar (block/list) or a non-value. Used by native-list
    /// collectors and set construction.
    fn value_native(&self, view: MutatorHeapView<'_>, closure: &AbiClosure) -> Option<Native>;

    // ── Neutral value/data construction (spec §5.5) ─────────────────
    // These build values engine-agnostically, so list/data-returning
    // intrinsics need no runtime code synthesis on the bytecode path.

    /// Wrap a native as a (WHNF) value closure handle.
    fn native_value(
        &self,
        view: MutatorHeapView<'_>,
        native: Native,
    ) -> Result<AbiClosure, ExecutionError>;

    /// Build a data-constructor value of `tag` over `fields`, returned as a
    /// value handle (does not set the machine result).
    fn data_value(
        &self,
        view: MutatorHeapView<'_>,
        tag: Tag,
        fields: &[AbiClosure],
    ) -> Result<AbiClosure, ExecutionError>;

    /// Wrap a body value with metadata, returned as a (WHNF) `Meta` value
    /// handle (does not set the machine result).
    ///
    /// Used when rebuilding a metadata-annotated data value (e.g. a YAML
    /// `!tag`-carrying scalar produced by `parse-as`) directly through the
    /// neutral ABI.
    fn meta_value(
        &self,
        view: MutatorHeapView<'_>,
        meta: AbiClosure,
        body: AbiClosure,
    ) -> Result<AbiClosure, ExecutionError>;

    /// Set the machine result to a cons-list of the given value handles.
    fn return_closure_list(
        &mut self,
        view: MutatorHeapView<'_>,
        items: Vec<AbiClosure>,
    ) -> Result<(), ExecutionError>;

    // ── Fixed-shape thunk construction (spec §5.5, arena analysis) ──────
    // Two intrinsics build lazy application thunks as *stored values* (not
    // tail calls); the bytecode engine pre-encodes one template each and
    // allocates only a GC-heap env frame over that template.

    /// Build a lazy unary application `f(a)` as a stored (updatable) value
    /// handle. Used by the process-parallelism driver to build the per-element
    /// `f(xs[i])` thunk it then forces (engine-neutrally) in each worker or on
    /// the sequential-fallback path.
    fn apply1_thunk(
        &self,
        view: MutatorHeapView<'_>,
        f: AbiClosure,
        a: AbiClosure,
    ) -> Result<AbiClosure, ExecutionError>;

    /// Build a lazy binary application `f(a0, a1)` as a stored (updatable)
    /// value handle. Used by `MERGEWITH` to combine colliding block values.
    fn apply2_thunk(
        &self,
        view: MutatorHeapView<'_>,
        f: AbiClosure,
        a0: AbiClosure,
        a1: AbiClosure,
    ) -> Result<AbiClosure, ExecutionError>;

    /// Build an updatable tail thunk that re-enters the intrinsic `bif_index`
    /// applied to `handle` when forced. Used by `PRODUCER_NEXT` to make the
    /// lazy cons-cell tail; the `Update` continuation memoises it so the
    /// producer advances at most once per list position.
    fn bif_tail_thunk(
        &self,
        view: MutatorHeapView<'_>,
        bif_index: u8,
        handle: u64,
    ) -> Result<AbiClosure, ExecutionError>;

    /// Request an emitter capture for the given format.
    ///
    /// Sets a pending flag that `step()` reads to push a format-specific
    /// capture emitter. All subsequent emit BIF output goes to the capture
    /// buffer until `CaptureEnd` fires.
    fn start_capture(&mut self, format: &str) -> Result<(), ExecutionError>;

    /// Push a `CaptureEnd` continuation onto the STG stack.
    fn push_capture_end(&mut self, view: MutatorHeapView<'_>) -> Result<(), ExecutionError>;

    /// Take the latest capture result string (set by `step()` after a
    /// `CaptureEnd` continuation fires).
    fn take_capture_result(&mut self) -> Result<String, ExecutionError>;

    // ── GC root set for intrinsic-held handles (eu-u9xj.6) ──────────
    //
    // `force()` runs the machine, and the machine collects mid-run. Any
    // `AbiClosure` an intrinsic is holding on the Rust stack across that call
    // is invisible to the collector: it is neither the current closure nor on
    // the continuation stack, so evacuation moves or reclaims the object
    // behind it and leaves the handle dangling. `src/eval/stg/list.rs` states
    // the rule ("no handle is held across a force"); `src/driver/bytecode_io_run.rs`
    // is the place that cannot obey it and uses the machine's stash as a root
    // set instead.
    //
    // `stash_push` is inherent on the machine type, so an intrinsic written
    // against `&mut dyn IntrinsicMachine` could not reach it. These methods
    // expose the same root set neutrally, over `AbiClosure`, so a driver that
    // genuinely must accumulate handles across forces (the process-parallelism
    // driver and value serialiser) can be GC-safe.
    //
    // Protocol: push a handle, then **read it back** with `gc_root_get` after
    // every `force()` — the collector updates the entry in the root set, not
    // the copy on your stack. Frames are stack-disciplined: record
    // `gc_root_len()` on entry and `gc_root_truncate()` back to it on **every**
    // exit path, including errors. Nesting is safe: a `force()` pushes and pops
    // its own entries, so indices handed out before it remain valid.

    /// Number of handles currently in the intrinsic root set.
    fn gc_root_len(&self) -> usize;

    /// Push a handle into the root set, returning the index to read it back by.
    fn gc_root_push(&mut self, closure: AbiClosure) -> usize;

    /// Read back the handle at `idx`, with any relocation the collector
    /// performed since it was pushed already applied.
    fn gc_root_get(&self, idx: usize) -> AbiClosure;

    /// Replace the handle at `idx`.
    fn gc_root_set(&mut self, idx: usize, closure: AbiClosure);

    /// Drop every handle from `len` upwards, releasing the frame.
    fn gc_root_truncate(&mut self, len: usize);

    /// Metadata discarded by the most recent [`Self::force`], if any.
    ///
    /// A `Meta` value reaching the top of a sub-evaluation with nothing to
    /// consume it is *stripped*: the machine continues with the body and the
    /// metadata is gone (`return_meta`). That is right for evaluation —
    /// metadata is transparent — but it means an intrinsic that forces a
    /// value cannot see the metadata the value carried, and one that copies
    /// values (the PP serialiser) would silently drop it.
    ///
    /// The machine records the stripped metadata here and clears it at the
    /// start of each `force`, so an intrinsic can recover it immediately
    /// afterwards. Taking it clears it.
    fn take_stripped_meta(&mut self) -> Option<AbiClosure>;

    /// Whether the machine is running in test mode.
    ///
    /// In test mode, `__EXPECT` failures return `false` instead of
    /// panicking, allowing test harnesses to collect results.
    fn test_mode(&self) -> bool;

    /// Record a diagnostic message (e.g. an `EXPECT FAILED` report from
    /// `__EXPECT`) for the driver to flush to the active stderr sink once
    /// the run completes.
    ///
    /// Intrinsics must not `eprintln!` diagnostics directly: that bypasses
    /// the `Box<dyn Write>` stderr capture the test harness installs via
    /// `Executor::capture_output`, so the diagnostic never reaches
    /// `evidence.yaml` and can't gate a test's verdict (eu-ntwg.2).
    fn record_diagnostic(&mut self, msg: String);
}

/// All intrinsics have an STG syntax wrapper
pub trait StgIntrinsic: Sync {
    /// The name of the intrinsic
    fn name(&self) -> &str;

    /// The STG wrapper for calling the intrinsic
    fn wrapper(&self, annotation: Smid) -> crate::eval::stg::syntax::LambdaForm {
        wrap(self.index(), self.info(), annotation)
    }

    /// Whether the compiler should inline the wrapper
    fn inlinable(&self) -> bool {
        true
    }

    /// Argument indices that are single-use (entered at most once).
    /// Index of the intrinsic
    fn index(&self) -> usize {
        intrinsics::index(self.name()).unwrap()
    }

    /// Type and arity information for the intrinsic
    fn info(&self) -> &intrinsics::Intrinsic {
        intrinsics::intrinsic(self.index())
    }

    /// An intrinsic has mutable access to the machine
    ///
    /// A call to an intrinsic may assume that its strict arguments are
    /// already evaluated (by the corresponding global wrapper) but must
    /// take care of updating the machine's closure and stack as
    /// appropriate to constitute a return.
    fn execute(
        &self,
        _machine: &mut dyn IntrinsicMachine,
        _heap: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        panic!("{} is STG-only", self.name());
    }

    /// A Ref to this global
    fn gref(&self) -> crate::eval::stg::syntax::Ref {
        dsl::gref(self.index())
    }
}

pub trait Const: StgIntrinsic {
    fn global(&self) -> Rc<StgSyn> {
        dsl::global(self.index())
    }
}

pub trait CallGlobal0: StgIntrinsic {
    fn global(&self) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![])
    }
}

pub trait CallGlobal1: StgIntrinsic {
    fn global(&self, x: crate::eval::stg::syntax::Ref) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![x])
    }
}

pub trait CallGlobal2: StgIntrinsic {
    fn global(
        &self,
        x: crate::eval::stg::syntax::Ref,
        y: crate::eval::stg::syntax::Ref,
    ) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![x, y])
    }
}

pub trait CallGlobal3: StgIntrinsic {
    fn global(
        &self,
        x: crate::eval::stg::syntax::Ref,
        y: crate::eval::stg::syntax::Ref,
        z: crate::eval::stg::syntax::Ref,
    ) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![x, y, z])
    }
}

pub trait CallGlobal4: StgIntrinsic {
    fn global(
        &self,
        x: crate::eval::stg::syntax::Ref,
        y: crate::eval::stg::syntax::Ref,
        z: crate::eval::stg::syntax::Ref,
        w: crate::eval::stg::syntax::Ref,
    ) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![x, y, z, w])
    }
}

pub trait CallGlobal7: StgIntrinsic {
    #[allow(clippy::too_many_arguments)]
    fn global(
        &self,
        x0: crate::eval::stg::syntax::Ref,
        x1: crate::eval::stg::syntax::Ref,
        x2: crate::eval::stg::syntax::Ref,
        x3: crate::eval::stg::syntax::Ref,
        x4: crate::eval::stg::syntax::Ref,
        x5: crate::eval::stg::syntax::Ref,
        x6: crate::eval::stg::syntax::Ref,
    ) -> Rc<StgSyn> {
        dsl::app(self.gref(), vec![x0, x1, x2, x3, x4, x5, x6])
    }
}
