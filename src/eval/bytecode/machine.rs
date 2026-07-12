//! Bytecode dispatch machine (BV1 Phase 2). **Work in progress.**
//!
//! This module will grow the parallel `BytecodeMachine`: a `run`/`step`/
//! `handle_op` dispatch loop over the flat opcode stream, a neutral
//! `IntrinsicMachine` implementation producing `BcValue`s over the
//! constructor templates, and the re-entrant `evaluate_to_whnf` path.
//!
//! This first increment lands the foundational operand decoding and
//! reference resolution used by every dispatch arm.

use std::cmp::Ordering;
use std::num::NonZeroUsize;

use lru::LruCache;
use regex::Regex;
use serde_json::Number;
use smallvec::SmallVec;

use crate::common::sourcemap::Smid;
use crate::eval::emit::{Emitter, NullEmitter};
use crate::eval::error::ExecutionError;
use crate::eval::machine::env::{EnvFrame, SynClosure};
use crate::eval::machine::intrinsic::{AbiClosure, IntrinsicMachine, StgIntrinsic};
use crate::eval::machine::metrics::{Clock, Metrics, ThreadOccupation};
use crate::eval::machine::vm::{interrupted, HeapNavigator};
use crate::eval::memory::alloc::ScopedAllocator;
use crate::eval::memory::array::Array;
use crate::eval::memory::collect::{
    collect as gc_collect, CollectorHeapView, CollectorScope, GcScannable, ScanPtr,
};
use crate::eval::memory::heap::{Heap, HeapStats};
use crate::eval::memory::infotable::{InfoTable, InfoTagged};
use crate::eval::memory::mutator::MutatorHeapView;
use crate::eval::memory::string::HeapString;
use crate::eval::memory::symbol::{SymbolId, SymbolPool};
use crate::eval::memory::syntax::{Native, Ref, RefPtr};
use crate::eval::stg::render_to_string::OwnedCaptureEmitter;
use crate::eval::stg::tags::{DataConstructor, Tag};

use super::program::{read_u32, read_u8};
use super::{
    BcClosure, BcContinuation, BcEnvBuilder, BcEnvFrame, BcValue, BytecodeProgram, CodeRef,
    GlobalForm, Op, FORM_LAMBDA, FORM_THUNK, NO_BRANCH, REF_G, REF_L, REF_V,
};

/// Inline-capacity buffers for per-instruction operand decoding. On nearly
/// every reduction step the hot dispatch loop reads a short argument list
/// (the atom offsets of an App/Cons/Bif, and — for a `Bif` — the decoded arg
/// refs it defers); backing these with `SmallVec` keeps the common
/// small-arity case entirely on the stack, eliminating the system-heap
/// `malloc`/`free` churn a fresh `Vec` per step would incur. Application and
/// intrinsic arities are naturally small, so the inline capacity of 8 is
/// never exceeded on the hot path (a larger list would simply spill to the
/// heap, exactly as a `Vec` does). Operand lists whose length is unbounded
/// by arity — the densified branch table of a `Case` (up to the full
/// data-constructor tag range) and a let/letrec binding-header list — avoid the
/// system heap differently: the branch table and a non-recursive `Let`'s
/// headers are decoded straight into their final GC `Array` (see `Op::Case`,
/// `Op::Let`); a recursive `LetRec`, which must fill its array in a second pass
/// over the frame, buffers its headers in an inline `FormHeaders` `SmallVec`
/// (decoded once, spilling to the heap only for an unusually large group).
type ArgOffsets = SmallVec<[CodeRef; 8]>;
type ArgRefs = SmallVec<[DecodedRef; 8]>;
/// Inline buffer for a `LetRec`'s form headers. Recursive bindings must be
/// closed over the frame *after* it is allocated, so — unlike `Op::Let`, which
/// reads each header straight into the managed array in one pass — `Op::LetRec`
/// buffers the headers to fill the array in a second pass. Decoding once into
/// an inline `SmallVec` keeps the common small-arity case off the system heap
/// (no per-`LETREC` `malloc`) without re-decoding the byte stream (eu-cj1h.3).
type FormHeaders = SmallVec<[FormHeader; 8]>;

/// A decoded inline `Ref` operand from the byte stream.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DecodedRef {
    /// Local (de Bruijn) index into the environment.
    Local(u32),
    /// Global slot index.
    Global(u32),
    /// Index into the prepared constant pool.
    Value(u32),
}

/// Read an inline ref (tag byte + `u32` payload) at `*pc`, advancing it.
#[inline]
pub fn read_ref(code: &[u8], pc: &mut usize) -> Result<DecodedRef, ExecutionError> {
    let tag = read_u8(code, pc);
    let payload = read_u32(code, pc);
    match tag {
        REF_L => Ok(DecodedRef::Local(payload)),
        REF_G => Ok(DecodedRef::Global(payload)),
        REF_V => Ok(DecodedRef::Value(payload)),
        other => Err(ExecutionError::Panic(
            Default::default(),
            format!("bytecode: invalid ref tag {other:#x}"),
        )),
    }
}

/// Resolve a decoded ref to a runtime value.
///
/// - `Local(i)` → the closure's environment slot `i`.
/// - `Global(i)` → the globals frame slot `i`.
/// - `Value(k)` → the prepared constant `k` (always a native), as a
///   `BcValue::Native`.
///
/// `constants` is the prepared pool (`BytecodeProgram::prepare_constants`),
/// whose entries are all `Ref::V(Native)`.
pub fn resolve_ref(
    view: MutatorHeapView<'_>,
    constants: &[Ref],
    env: RefPtr<BcEnvFrame>,
    globals: RefPtr<BcEnvFrame>,
    dref: DecodedRef,
) -> Result<BcValue, ExecutionError> {
    match dref {
        DecodedRef::Local(i) => view
            .scoped(env)
            .get(&view, i as usize)
            .ok_or(ExecutionError::BadEnvironmentIndex(i as usize)),
        DecodedRef::Global(i) => view
            .scoped(globals)
            .get(&view, i as usize)
            .ok_or(ExecutionError::BadGlobalIndex(i as usize)),
        DecodedRef::Value(k) => match constants.get(k as usize) {
            Some(Ref::V(native)) => Ok(BcValue::Native(native.clone())),
            _ => Err(ExecutionError::Panic(
                Default::default(),
                format!("bytecode: constant {k} missing or not a native"),
            )),
        },
    }
}

/// Describe a non-native closure for a "not a value" error, mirroring the
/// HeapSyn `HeapNavigator::resolve_native` discrimination (vm.rs): the head
/// opcode names the shape, and a `Cons` opcode's tag distinguishes the data
/// constructors (a boolean, a list, a block, …). `pc` must sit just past the
/// opcode byte; for a `Cons` the tag byte that follows is read here. The
/// returned string is the `context` passed to `format_not_value` (error.rs).
fn not_value_description(op: Option<Op>, code: &[u8], pc: &mut usize) -> &'static str {
    match op {
        Some(Op::Cons) => match DataConstructor::try_from(read_u8(code, pc)) {
            Ok(DataConstructor::BoolTrue) => "a boolean (true)",
            Ok(DataConstructor::BoolFalse) => "a boolean (false)",
            Ok(DataConstructor::ListCons) | Ok(DataConstructor::ListNil) => "a list",
            Ok(DataConstructor::Block) => "a block",
            _ => "a data constructor",
        },
        Some(Op::App) | Some(Op::DirectApp) => "a function application",
        Some(Op::Bif) => "an intrinsic function call",
        Some(Op::Case) => "a case expression",
        Some(Op::Let) | Some(Op::LetRec) => "a let binding",
        Some(Op::Meta) | Some(Op::DeMeta) => "a metadata expression",
        Some(Op::Seq) => "a strict evaluation",
        Some(Op::LookupLit) => "a lookup expression",
        Some(Op::BlackHole) => "an uninitialised value (possible cycle)",
        Some(Op::Ann) => "an annotated expression",
        Some(Op::FusedPrimop) => "a fused primop dispatch",
        // `Atom` is followed transparently by the caller, so it should not
        // reach here; treat any residual/unknown opcode as a bare atom.
        Some(Op::Atom) | None => "an atom",
    }
}

/// Read an arg-offset list (`u8` count then that many `u32` offsets), as
/// emitted for App/DirectApp/Cons/Bif operands.
pub fn read_arg_offsets(code: &[u8], pc: &mut usize) -> ArgOffsets {
    let n = read_u8(code, pc) as usize;
    (0..n).map(|_| read_u32(code, pc)).collect()
}

/// A decoded lambda-form header (Let/LetRec binding, or a global form).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FormHeader {
    pub kind: u8,
    pub arity: u8,
    pub smid: Smid,
    pub body: CodeRef,
}

/// Read a lambda-form header (`kind`, `arity`, `smid`, `body` offset).
pub fn read_form_header(code: &[u8], pc: &mut usize) -> FormHeader {
    let kind = read_u8(code, pc);
    let arity = read_u8(code, pc);
    let smid = Smid::from(read_u32(code, pc));
    let body = read_u32(code, pc);
    FormHeader {
        kind,
        arity,
        smid,
        body,
    }
}

/// Read a densified branch table (`min_tag`, `len`, then `len` `u32`
/// entries, `NO_BRANCH` → `None`). `pc` must sit at the `min_tag` byte
/// (i.e. after the Case scrutinee offset).
pub fn read_branch_table(code: &[u8], pc: &mut usize) -> (Tag, Vec<Option<CodeRef>>) {
    let min_tag = read_u8(code, pc);
    let len = read_u8(code, pc) as usize;
    let entries = (0..len)
        .map(|_| {
            let e = read_u32(code, pc);
            if e == NO_BRANCH {
                None
            } else {
                Some(e)
            }
        })
        .collect();
    (min_tag, entries)
}

/// A single active BIF dispatch: the env the intrinsic's `Ref::L` operands
/// index into, plus its resolved argument refs. Held on the [`BcMachineState`]
/// `bif_frames` stack — and therefore scanned as GC roots — so that a
/// collection triggered mid-`execute` (an intrinsic force → `run_to_whnf` →
/// `gc_collect`) forwards the env frame and any `Ref::V` heap-backed constant
/// the still-running intrinsic dereferences afterwards (eu-zryh). Re-entrant:
/// one frame per nested dispatch, innermost on top. This is the bytecode
/// analogue of the HeapSyn engine reading env/args back through the rooted
/// `state.closure` Bif node (`vm.rs`), which never go stale because the node
/// itself is a heap root the collector updates.
pub struct BifFrame {
    /// The dispatching Bif closure's env frame.
    pub env: RefPtr<BcEnvFrame>,
    /// The intrinsic's resolved argument refs; `Ref::V` constants carry heap
    /// string/set/vec pointers the collector must forward across a force.
    ///
    /// This is a `Vec<Ref>` (not an inline `SmallVec`) *deliberately*: the
    /// dispatch tail captures a raw `*const [Ref]` into this buffer and holds
    /// it across `execute`, during which a nested BIF dispatch may push another
    /// `BifFrame` and reallocate the `bif_frames` `Vec` — moving this struct.
    /// A `Vec`'s backing buffer lives on the system heap independent of the
    /// struct, so the raw slice stays valid; inline storage would move with the
    /// struct and dangle. The per-dispatch `malloc`/`free` this would otherwise
    /// incur is avoided by recycling buffers through `BcMachineState`'s
    /// `bif_arg_pool` (eu-cj1h.3).
    pub args: Vec<Ref>,
}

/// The bytecode machine's runtime state (its GC roots point in from here).
///
/// The current value, the continuation stack, and the two environment
/// roots range over `BcValue`; the constant pool holds prepared native
/// values (`Ref::V`). The heap / intrinsics / emitter / metrics are held
/// alongside (added with the `run`/`step` loop in a later increment),
/// mirroring `MachineCore` on the HeapSyn side.
pub struct BcMachineState {
    /// The value currently being evaluated or returned.
    pub current: BcValue,
    /// Continuation stack (off-heap `Vec`, scanned as GC roots).
    pub stack: Vec<BcContinuation>,
    /// Globals frame (intrinsic wrappers then prelude bindings).
    pub globals: RefPtr<BcEnvFrame>,
    /// The empty root environment.
    pub root_env: RefPtr<BcEnvFrame>,
    /// Prepared constant pool (all `Ref::V(Native)`), a GC root set.
    pub constants: Vec<Ref>,
    /// Termination flag.
    pub terminated: bool,
    /// Annotation to stamp on allocations / attach to errors.
    pub annotation: Smid,
    /// Deferred BIF intrinsic index (set by the `Bif` arm, run in `step`).
    pub pending_bif: Option<u8>,
    /// The deferred BIF's decoded argument refs (spec §6.3): captured by the
    /// `Bif` arm and read by the deferred dispatch (there is no live node to
    /// re-read, unlike the HeapSyn path).
    pub pending_bif_args: ArgRefs,
    /// Set by a `CaptureEnd` continuation to signal `step` to finish an
    /// emitter capture.
    pub capture_end_pending: bool,
    /// Requested capture format (set by `start_capture`); the machine loop
    /// reads it to push a capture emitter (mirrors
    /// `MachineState::pending_capture_start`).
    pub pending_capture_start: Option<String>,
    /// Set (with `terminated`) when an IO constructor reaches the top with
    /// nothing to consume it — the io-run driver inspects `current`.
    pub yielded_io: bool,
    /// Offset of the shared `OP_BLACKHOLE` node (`BytecodeProgram::blackhole`),
    /// used to overwrite a thunk's slot while forcing it.
    pub blackhole: CodeRef,
    /// Closures/values stashed live across a nested `evaluate_to_whnf`
    /// sub-evaluation (and the io-run driver), kept as GC roots.
    pub stash: Vec<BcValue>,
    /// Continuation stacks suspended during nested `evaluate_to_whnf` calls;
    /// their heap pointers must stay live for the sub-evaluation (mirrors
    /// `MachineState::suspended_stacks`).
    pub suspended_stacks: Vec<Vec<BcContinuation>>,
    /// Active BIF dispatch frames (env + resolved arg refs), kept as GC roots
    /// so a collection triggered by an intrinsic force forwards the pointers
    /// the in-flight intrinsic still dereferences (eu-zryh). See [`BifFrame`].
    pub bif_frames: Vec<BifFrame>,
    /// Free-list of recycled `BifFrame::args` buffers. Each BIF dispatch draws
    /// a `Vec<Ref>` from here (or allocates one on a cold miss) and returns it,
    /// cleared, when the frame is popped — eliminating per-dispatch system-heap
    /// `malloc`/`free` after warmup while keeping the args buffer address-stable
    /// (see [`BifFrame::args`]). Pooled buffers are always empty, so they hold
    /// no live `Ref`s and need no GC scanning (eu-cj1h.3).
    pub bif_arg_pool: Vec<Vec<Ref>>,
    /// Closures allocated by `let`/`letrec` bindings, accumulated across every
    /// execution phase (main run, io-run drives, nested `evaluate_to_whnf`).
    /// Counted the same way as the HeapSyn machine's `Metrics::allocs`
    /// (`vm.rs` — one per `let`/`letrec` binding) so `-S` allocation counts
    /// are cross-engine comparable.
    pub allocs: u64,
}

impl BcMachineState {
    /// Create a fresh state with the given roots and constant pool. The
    /// initial `current` value is the program-root closure.
    pub fn new(
        current: BcValue,
        globals: RefPtr<BcEnvFrame>,
        root_env: RefPtr<BcEnvFrame>,
        constants: Vec<Ref>,
    ) -> Self {
        BcMachineState {
            current,
            stack: Vec::new(),
            globals,
            root_env,
            constants,
            terminated: false,
            annotation: Smid::default(),
            pending_bif: None,
            pending_bif_args: SmallVec::new(),
            capture_end_pending: false,
            pending_capture_start: None,
            yielded_io: false,
            blackhole: 0,
            stash: Vec::new(),
            suspended_stacks: Vec::new(),
            bif_frames: Vec::new(),
            bif_arg_pool: Vec::new(),
            allocs: 0,
        }
    }

    /// Collect the annotation trace of the environment chain currently in
    /// scope, for error diagnostics. Mirrors `MachineState::env_trace`
    /// (vm.rs), which walks the environment of the closure being evaluated.
    /// The current value is that closure while an op or BIF is executing; a
    /// native `current` has no environment and yields an empty trace.
    pub fn env_trace(&self, view: MutatorHeapView<'_>) -> Vec<Smid> {
        match self.current.as_closure() {
            Some(c) => view.scoped(c.env()).annotation_trace(&view),
            None => Vec::new(),
        }
    }

    /// Collect the annotation trace of the continuation stack (innermost
    /// first, de-duplicating adjacent repeats), for error diagnostics.
    /// Mirrors `MachineState::stack_trace` / `stack_trace_iter` (vm.rs) over
    /// the `BcContinuation` variants: the case/apply/seq/lookup continuations
    /// carry a source annotation directly; update/de-meta continuations
    /// borrow their captured environment's annotation.
    pub fn stack_trace(&self, view: MutatorHeapView<'_>) -> Vec<Smid> {
        let capacity = self.stack.len().min(64);
        let mut trace = Vec::with_capacity(capacity);
        let mut prev = Smid::default();
        for cont in self.stack.iter().rev() {
            let smid = match cont {
                BcContinuation::Branch { annotation, .. }
                | BcContinuation::ApplyTo { annotation, .. }
                | BcContinuation::SeqBind { annotation, .. }
                | BcContinuation::FusedPrimopLeft { annotation, .. }
                | BcContinuation::FusedPrimopRight { annotation, .. } => *annotation,
                BcContinuation::LookupLitForce { smid, .. } => *smid,
                BcContinuation::Update { environment, .. }
                | BcContinuation::DeMeta { environment, .. } => {
                    view.scoped(*environment).annotation()
                }
                BcContinuation::CaptureEnd => Smid::default(),
            };
            if smid != Smid::default() && smid != prev {
                prev = smid;
                trace.push(smid);
            }
        }
        trace
    }
}

/// Wrap a raised machine error with the environment and continuation-stack
/// annotation traces captured from the current state. Mirrors the HeapSyn
/// `handle_instruction` / BIF `map_err` sites (vm.rs): building the traces
/// here — before the Rust stack unwinds — captures the environment and
/// continuation stack live at the raise point, so `to_diagnostic` can walk
/// them to recover the user call site when the error's own Smid is synthetic
/// or points into the prelude. As on the HeapSyn side, the wrap is
/// unconditional (a nested sub-evaluation error may be wrapped twice; the
/// outer traces then win in `to_diagnostic`, matching HeapSyn).
fn attach_trace(
    state: &BcMachineState,
    view: MutatorHeapView<'_>,
    e: ExecutionError,
) -> ExecutionError {
    let env_trace = state.env_trace(view);
    let stack_trace = state.stack_trace(view);
    ExecutionError::Traced(Box::new(e), Box::new((env_trace, stack_trace)))
}

/// Mark the heap pointers embedded in a prepared constant (`Ref::V(native)`).
/// Mirrors `syntax::mark_ref_heap_pointers` for the bytecode constant pool,
/// which is an off-heap root set rather than part of the code graph.
fn scan_const<'a>(
    r: &'a Ref,
    scope: &'a dyn CollectorScope,
    marker: &mut CollectorHeapView<'a>,
    out: &mut Vec<ScanPtr<'a>>,
) {
    match r {
        Ref::V(Native::Str(ptr)) if marker.mark(*ptr) => {
            out.push(ScanPtr::from_non_null(scope, *ptr));
        }
        Ref::V(Native::Set(ptr)) => {
            marker.mark(*ptr);
        }
        Ref::V(Native::NdArray(ptr)) => {
            marker.mark(*ptr);
        }
        Ref::V(Native::Vec(ptr)) => {
            marker.mark(*ptr);
        }
        _ => {}
    }
}

/// Update forwarded heap pointers in a prepared constant (mirrors
/// `syntax::update_ref_heap_pointers`).
fn update_const(r: &mut Ref, heap: &CollectorHeapView<'_>) {
    match r {
        Ref::V(Native::Str(ptr)) => {
            if let Some(new) = heap.forwarded_to(*ptr) {
                *ptr = new;
            }
        }
        Ref::V(Native::Set(ptr)) => {
            if let Some(new) = heap.forwarded_to(*ptr) {
                *ptr = new;
            }
        }
        Ref::V(Native::NdArray(ptr)) => {
            if let Some(new) = heap.forwarded_to(*ptr) {
                *ptr = new;
            }
        }
        Ref::V(Native::Vec(ptr)) => {
            if let Some(new) = heap.forwarded_to(*ptr) {
                *ptr = new;
            }
        }
        _ => {}
    }
}

/// Materialise the deferred BIF's captured `DecodedRef`s into ABI `Ref`s. Local
/// / global refs become indices; a `Value(k)` clones the prepared constant
/// `state.constants[k]` (a `Ref::V` that may carry a heap pointer — rooted for
/// the dispatch via the `BifFrame`, see [`BifFrame`] and eu-zryh). Shared by
/// the top-level dispatch tail and the re-entrant sub-loop.
fn materialize_bif_args(state: &mut BcMachineState) -> Vec<Ref> {
    // Take the decoded refs into an owned local so the constant-pool access
    // below does not conflict with borrowing `state.pending_bif_args`.
    let pending = std::mem::take(&mut state.pending_bif_args);
    // Recycle a buffer from the pool (cold miss allocates) rather than
    // `malloc`ing a fresh `Vec` per dispatch (eu-cj1h.3).
    let mut args = state.bif_arg_pool.pop().unwrap_or_default();
    args.clear();
    args.reserve(pending.len());
    for d in &pending {
        args.push(match *d {
            DecodedRef::Local(i) => Ref::L(i as usize),
            DecodedRef::Global(i) => Ref::G(i as usize),
            DecodedRef::Value(k) => state.constants[k as usize].clone(),
        });
    }
    args
}

/// Return a popped `BifFrame`'s args buffer to the recycling pool, cleared so
/// it holds no live `Ref`s (see [`BcMachineState::bif_arg_pool`]). Paired with
/// every `bif_frames.pop()` that follows a `materialize_bif_args` dispatch.
fn recycle_bif_args(state: &mut BcMachineState, frame: BifFrame) {
    let mut args = frame.args;
    args.clear();
    state.bif_arg_pool.push(args);
}

/// One GC poll shared by every drive loop (`run`, `drive_whnf`,
/// `drive_to_whnf`), so the interrupt check and collection cadence cannot drift
/// between the three hand-written loops (eu-apn7). Returns `Err(Interrupted)`
/// if an interrupt is pending, otherwise collects when the heap policy asks.
fn gc_poll(
    state: &mut BcMachineState,
    heap: &mut Heap,
    clock: &mut Clock,
    dump_heap: bool,
) -> Result<(), ExecutionError> {
    if interrupted() {
        return Err(ExecutionError::Interrupted);
    }
    if heap.policy_requires_collection() {
        gc_collect(state, heap, clock, dump_heap);
        clock.switch(ThreadOccupation::Mutator);
    }
    Ok(())
}

/// Enter a WHNF sub-evaluation: suspend the caller's continuation stack and
/// stash its `current` value (both become GC roots for the sub-run), seat
/// `value` as the new `current`, and clear the termination / yield flags.
/// Returns the caller's `terminated` flag for [`exit_whnf_subrun`] to restore.
///
/// This is the single shared entry half of the stash/suspend/swap/restore
/// choreography used by both `run_to_whnf` (a BIF forcing a thunk) and
/// `evaluate_to_whnf_for_io` (the io-run driver forcing a spec field), so the
/// protocol can no longer drift between them (eu-apn7).
fn enter_whnf_subrun(state: &mut BcMachineState, value: BcValue) -> bool {
    let saved_stack = std::mem::take(&mut state.stack);
    state.suspended_stacks.push(saved_stack);
    let saved_current = std::mem::replace(&mut state.current, value);
    state.stash.push(saved_current);
    let saved_terminated = state.terminated;
    state.terminated = false;
    state.yielded_io = false;
    saved_terminated
}

/// Exit a WHNF sub-evaluation: pop the stashed `current` and suspended stack
/// and restore them, then the requested post-run flags.
///
/// INVARIANT (eu-apn7): the caller's stack / current / flags are restored
/// **before** `run_result` is propagated, so an intrinsic that swallows a
/// sub-run force error (e.g. `debug::boxed_native` `.ok()`,
/// `block::collect_block_keys` `Err(_) =>`) resumes stepping on the intact
/// caller continuation stack rather than the truncated sub-run stack. Mirrors
/// the ordering `evaluate_to_whnf_for_io` already relied on.
///
/// `post_terminated` / `post_yielded_io` are the flag values the caller wants
/// after the sub-run (the io path forces a yield; the thunk path restores the
/// saved flag). `yield_err_msg` is raised if the sub-run itself yielded on an
/// IO constructor (spec fields / forced thunks must be pure).
fn exit_whnf_subrun(
    state: &mut BcMachineState,
    run_result: Result<(), ExecutionError>,
    post_terminated: bool,
    post_yielded_io: bool,
    yield_err_msg: &'static str,
) -> Result<BcValue, ExecutionError> {
    let saved_current = state.stash.pop().expect("bytecode whnf stash underflow");
    let saved_stack = state
        .suspended_stacks
        .pop()
        .expect("bytecode whnf suspended-stack underflow");

    let sub_yielded = state.yielded_io;
    let result = std::mem::replace(&mut state.current, saved_current);
    state.terminated = post_terminated;
    state.yielded_io = post_yielded_io;
    state.stack = saved_stack;

    // Restore-before-propagate: state is now the caller's; only now unwind.
    run_result?;

    if sub_yielded {
        return Err(ExecutionError::Panic(
            state.annotation,
            yield_err_msg.to_string(),
        ));
    }
    Ok(result)
}

impl GcScannable for BcMachineState {
    fn scan<'a>(
        &'a self,
        scope: &'a dyn CollectorScope,
        marker: &mut CollectorHeapView<'a>,
        out: &mut Vec<ScanPtr<'a>>,
    ) {
        if marker.mark(self.globals) {
            out.push(ScanPtr::from_non_null(scope, self.globals));
        }
        if marker.mark(self.root_env) {
            out.push(ScanPtr::from_non_null(scope, self.root_env));
        }
        // The current value ranges over `BcValue` (a closure or native); its
        // `GcScannable` impl marks the env pointer / heap-backed native.
        out.push(ScanPtr::new(scope, &self.current));
        // Continuations live inline in the `Vec` (off the eucalypt heap); scan
        // their internal heap pointers directly (mirrors `MachineState::scan`).
        for cont in &self.stack {
            cont.scan(scope, marker, out);
        }
        // The prepared constant pool is a root set of heap-backed natives.
        for r in &self.constants {
            scan_const(r, scope, marker, out);
        }
        // Values stashed across a nested evaluate_to_whnf / io-run.
        for stashed in &self.stash {
            out.push(ScanPtr::new(scope, stashed));
        }
        // Continuation stacks suspended during nested evaluations.
        for suspended in &self.suspended_stacks {
            for cont in suspended {
                cont.scan(scope, marker, out);
            }
        }
        // Active BIF dispatch frames — env + resolved arg refs (eu-zryh).
        for frame in &self.bif_frames {
            if marker.mark(frame.env) {
                out.push(ScanPtr::from_non_null(scope, frame.env));
            }
            for r in &frame.args {
                scan_const(r, scope, marker, out);
            }
        }
    }

    fn scan_and_update(&mut self, heap: &CollectorHeapView<'_>) {
        if let Some(new) = heap.forwarded_to(self.root_env) {
            self.root_env = new;
        }
        if let Some(new) = heap.forwarded_to(self.globals) {
            self.globals = new;
        }
        self.current.scan_and_update(heap);
        for cont in &mut self.stack {
            cont.scan_and_update(heap);
        }
        for r in &mut self.constants {
            update_const(r, heap);
        }
        for stashed in &mut self.stash {
            stashed.scan_and_update(heap);
        }
        for suspended in &mut self.suspended_stacks {
            for cont in suspended {
                cont.scan_and_update(heap);
            }
        }
        for frame in &mut self.bif_frames {
            if let Some(new) = heap.forwarded_to(frame.env) {
                frame.env = new;
            }
            for r in &mut frame.args {
                update_const(r, heap);
            }
        }
    }
}

/// Physical-slot pattern for the shared-env optimisation (bytecode mirror of
/// `vm.rs` `ArgPattern`). Sharing the constructor's backing frame with the
/// case branch — rather than copying field values — lets a thunk update
/// (memoisation) in one traversal be visible to every other reference to the
/// same data value. This is required for the correctness of side-effecting
/// thunks such as `PRODUCER_NEXT`'s tail, and matches the HeapSyn engine.
enum BcArgPattern {
    /// Physical slots are `0, 1, ..., n-1` covering the full backing — share
    /// with an identity mapping (no remap).
    SequentialFromZero,
    /// All args are locals within the top frame (`len <= 4`) — share the full
    /// backing with a logical→physical remap table.
    Remapped { remap: [u8; 4], len: usize },
    /// Non-local refs, more than 4 args, or an index reaching a deeper frame —
    /// must copy into fresh storage.
    RequiresCopy,
}

/// Classify a constructor's arg slice for the shared-env optimisation
/// (bytecode mirror of `vm.rs` `classify_args`).
fn bc_classify_args(
    args: &[DecodedRef],
    logical_len: usize,
    backing_len: usize,
    env_physical_index: impl Fn(usize) -> usize,
) -> BcArgPattern {
    if args.len() > 4 {
        return BcArgPattern::RequiresCopy;
    }
    let mut remap = [0u8; 4];
    let mut is_identity = true;
    for (j, a) in args.iter().enumerate() {
        match a {
            DecodedRef::Local(i) if (*i as usize) < logical_len => {
                let phys = env_physical_index(*i as usize);
                if phys > u8::MAX as usize {
                    return BcArgPattern::RequiresCopy;
                }
                remap[j] = phys as u8;
                if phys != j {
                    is_identity = false;
                }
            }
            _ => return BcArgPattern::RequiresCopy,
        }
    }
    if is_identity && args.len() == backing_len {
        BcArgPattern::SequentialFromZero
    } else {
        BcArgPattern::Remapped {
            remap,
            len: args.len(),
        }
    }
}

/// Build a branch/handler env frame from the constructor's fields, sharing
/// the constructor's backing array where the arg pattern allows (mirror of
/// `vm.rs` `env_from_data_args`, `:923`). Sharing preserves thunk
/// memoisation across traversals; the `RequiresCopy` cases fall back to
/// `env_from_data_args_copy`.
fn env_from_data_args(
    state: &BcMachineState,
    view: MutatorHeapView<'_>,
    args: &[DecodedRef],
    next: RefPtr<BcEnvFrame>,
) -> Result<RefPtr<BcEnvFrame>, ExecutionError> {
    let cons_env = state
        .current
        .as_closure()
        .ok_or_else(|| {
            ExecutionError::Panic(
                state.annotation,
                "return_data on a native value".to_string(),
            )
        })?
        .env();
    let constructor_env = view.scoped(cons_env);
    let logical_len = (*constructor_env).logical_len();
    let backing_len = (*constructor_env).backing_len();

    match bc_classify_args(args, logical_len, backing_len, |i| {
        (*constructor_env).physical_index(i)
    }) {
        BcArgPattern::SequentialFromZero => {
            // Identity mapping over the full backing — share directly.
            let shared = (*constructor_env).shared_bindings_full();
            Ok(view
                .alloc(BcEnvFrame::new(shared, state.annotation, Some(next)))?
                .as_ptr())
        }
        BcArgPattern::Remapped { remap, len } => {
            // Share the full backing with a logical→physical remap table.
            let shared = (*constructor_env).shared_bindings_full();
            Ok(view
                .alloc(BcEnvFrame::new_remapped(
                    shared,
                    &remap[..len],
                    state.annotation,
                    Some(next),
                ))?
                .as_ptr())
        }
        BcArgPattern::RequiresCopy => env_from_data_args_copy(state, view, args, next),
    }
}

/// Build a branch/handler env frame by copying the constructor's field
/// values (the copy path of `vm.rs` `env_from_data_args_copy`, `:978`). Used
/// when the arg pattern cannot share backing (non-local refs, > 4 args, or an
/// index reaching a deeper frame).
fn env_from_data_args_copy(
    state: &BcMachineState,
    view: MutatorHeapView<'_>,
    args: &[DecodedRef],
    next: RefPtr<BcEnvFrame>,
) -> Result<RefPtr<BcEnvFrame>, ExecutionError> {
    let cons_env = state
        .current
        .as_closure()
        .ok_or_else(|| {
            ExecutionError::Panic(
                state.annotation,
                "return_data on a native value".to_string(),
            )
        })?
        .env();
    let local = view.scoped(cons_env);
    let globals = view.scoped(state.globals);

    let mut array = Array::with_capacity(&view, args.len());
    for a in args {
        let v = match a {
            DecodedRef::Local(i) => (*local)
                .get(&view, *i as usize)
                .ok_or(ExecutionError::BadEnvironmentIndex(*i as usize))?,
            DecodedRef::Global(i) => (*globals)
                .get(&view, *i as usize)
                .ok_or(ExecutionError::BadGlobalIndex(*i as usize))?,
            DecodedRef::Value(k) => match state.constants.get(*k as usize) {
                Some(Ref::V(n)) => BcValue::Native(n.clone()),
                _ => {
                    return Err(ExecutionError::Panic(
                        state.annotation,
                        format!("bytecode: constant {k} missing or not a native"),
                    ))
                }
            },
        };
        array.push(&view, v);
    }
    Ok(view
        .alloc(BcEnvFrame::new(array, state.annotation, Some(next)))?
        .as_ptr())
}

/// Return a data constructor (`current` is the constructor closure) into
/// the top continuation. Translated from `vm.rs` `return_data` (`:1014`).
///
/// `args` are the constructor's field refs (decoded from its `OP_CONS`).
/// The block-application (`ApplyTo` → `MERGE`) and `LookupLitForce`
/// block-lookup paths need the globals frame + a bytecode block navigator
/// and are wired with the full machine loop; they error explicitly here.
/// Decode a data-constructor closure's `OP_CONS` node into `(tag, arg
/// offsets)`; the fields live in `c.env()`. `None` if `c` is not a `Cons`.
fn decode_cons(prog: &BytecodeProgram, c: &BcClosure) -> Option<(Tag, ArgOffsets)> {
    let code = prog.code.as_slice();
    let mut pc = c.code() as usize;
    if Op::from_u8(read_u8(code, &mut pc)) != Some(Op::Cons) {
        return None;
    }
    let tag = read_u8(code, &mut pc);
    let offsets = read_arg_offsets(code, &mut pc);
    Some((tag, offsets))
}

/// Resolve a runtime value to an interned symbol id, following `OP_ATOM`
/// indirections and unwrapping a `BoxedSymbol` (the bytecode analogue of
/// `block::pair_key_symbol_id`'s key resolution).
fn value_symbol(
    prog: &BytecodeProgram,
    state: &BcMachineState,
    view: MutatorHeapView<'_>,
    value: BcValue,
) -> Option<SymbolId> {
    match value {
        BcValue::Native(Native::Sym(id)) => Some(id),
        BcValue::Native(_) => None,
        BcValue::Closure(c) => {
            let code = prog.code.as_slice();
            let mut pc = c.code() as usize;
            match Op::from_u8(read_u8(code, &mut pc)) {
                Some(Op::Atom) => {
                    let dref = read_ref(code, &mut pc).ok()?;
                    let inner =
                        resolve_ref(view, &state.constants, c.env(), state.globals, dref).ok()?;
                    value_symbol(prog, state, view, inner)
                }
                Some(Op::Cons) => {
                    let tag = read_u8(code, &mut pc);
                    if tag != DataConstructor::BoxedSymbol.tag() {
                        return None;
                    }
                    let offsets = read_arg_offsets(code, &mut pc);
                    let field_ref = arg_ref(code, *offsets.first()?).ok()?;
                    let field =
                        resolve_ref(view, &state.constants, c.env(), state.globals, field_ref)
                            .ok()?;
                    value_symbol(prog, state, view, field)
                }
                _ => None,
            }
        }
    }
}

/// The interned symbol key of a `BlockPair` value, if it is one.
fn pair_key_symbol(
    prog: &BytecodeProgram,
    state: &BcMachineState,
    view: MutatorHeapView<'_>,
    pair: &BcValue,
) -> Option<SymbolId> {
    let BcValue::Closure(c) = pair else {
        return None;
    };
    let (tag, offsets) = decode_cons(prog, c)?;
    if tag != DataConstructor::BlockPair.tag() {
        return None;
    }
    let key_ref = arg_ref(prog.code.as_slice(), *offsets.first()?).ok()?;
    let key = resolve_ref(view, &state.constants, c.env(), state.globals, key_ref).ok()?;
    value_symbol(prog, state, view, key)
}

/// Literal-key lookup on a forced block (`DataConstructor::Block`). Linear
/// scan of the entry list of `BlockPair`s (the perf index is skipped — a
/// linear scan yields the identical result). `None` on miss / non-block.
/// Translated from `block::lookup_lit_in_block` + `linear_scan_for_key`.
fn bc_lookup_in_block(
    prog: &BytecodeProgram,
    state: &BcMachineState,
    view: MutatorHeapView<'_>,
    block: &BcClosure,
    target: SymbolId,
) -> Option<BcValue> {
    let (tag, offsets) = decode_cons(prog, block)?;
    if tag != DataConstructor::Block.tag() {
        return None;
    }
    // Block args are [entry-list, index]; ignore the index and scan the list.
    let list_ref = arg_ref(prog.code.as_slice(), *offsets.first()?).ok()?;
    let mut current =
        resolve_ref(view, &state.constants, block.env(), state.globals, list_ref).ok()?;
    loop {
        let c = match &current {
            BcValue::Closure(c) => *c,
            BcValue::Native(_) => return None,
        };
        let (ltag, loffs) = decode_cons(prog, &c)?;
        if ltag != DataConstructor::ListCons.tag() {
            // ListNil or anything else: key not present.
            return None;
        }
        let head_ref = arg_ref(prog.code.as_slice(), *loffs.first()?).ok()?;
        let pair = resolve_ref(view, &state.constants, c.env(), state.globals, head_ref).ok()?;
        if pair_key_symbol(prog, state, view, &pair) == Some(target) {
            let BcValue::Closure(pc) = &pair else {
                return None;
            };
            let (_, poffs) = decode_cons(prog, pc)?;
            let val_ref = arg_ref(prog.code.as_slice(), *poffs.get(1)?).ok()?;
            return resolve_ref(view, &state.constants, pc.env(), state.globals, val_ref).ok();
        }
        let tail_ref = arg_ref(prog.code.as_slice(), *loffs.get(1)?).ok()?;
        current = resolve_ref(view, &state.constants, c.env(), state.globals, tail_ref).ok()?;
    }
}

pub fn return_data(
    state: &mut BcMachineState,
    view: MutatorHeapView<'_>,
    prog: &BytecodeProgram,
    tag: Tag,
    args: &[DecodedRef],
) -> Result<(), ExecutionError> {
    let Some(cont) = state.stack.pop() else {
        state.terminated = true;
        if DataConstructor::is_io_constructor(tag) {
            state.yielded_io = true;
        }
        return Ok(());
    };
    match cont {
        BcContinuation::Branch {
            min_tag,
            branch_table,
            fallback,
            environment,
            annotation,
        } => {
            let body = if tag >= min_tag {
                branch_table.get((tag - min_tag) as usize).flatten()
            } else {
                None
            };
            if let Some(body) = body {
                let env = if args.is_empty() {
                    environment
                } else {
                    env_from_data_args(state, view, args, environment)?
                };
                state.current = BcValue::Closure(BcClosure::new(body, env));
            } else if let Some(fb) = fallback {
                let cur = state.current.clone();
                let env = view.from_value(cur, environment, state.annotation)?;
                state.current = BcValue::Closure(BcClosure::new(fb, env));
            } else {
                let expected: Vec<u8> = (0..branch_table.len())
                    .filter(|i| branch_table.get(*i).flatten().is_some())
                    .map(|i| min_tag + i as u8)
                    .collect();
                let ann = if annotation.is_valid() {
                    annotation
                } else {
                    state.annotation
                };
                return Err(ExecutionError::NoBranchForDataTag(ann, tag, expected));
            }
        }
        BcContinuation::Update { environment, index } => {
            let cur = state.current.clone();
            view.scoped(environment).update(&view, index, cur)?;
        }
        BcContinuation::ApplyTo {
            args: apply_args,
            annotation,
        } => {
            // Block application: blocks are callable as functions, delegating
            // to the MERGE global. This is the only data type with callable
            // semantics. Append the block (the callable, held in
            // `state.current`) as MERGE's final argument, re-push the ApplyTo,
            // and enter MERGE — it is then applied to [applied-args…, block].
            // Mirrors `vm.rs` return_data.
            if tag == DataConstructor::Block.tag() {
                let block_val = state.current.clone();
                let mut merged: Vec<BcValue> = apply_args.as_slice().to_vec();
                merged.push(block_val);
                state.stack.push(BcContinuation::ApplyTo {
                    args: Array::from_slice(&view, &merged),
                    annotation,
                });
                // Restore the application-site annotation so any type-mismatch
                // raised inside the MERGE wrapper carries the user's location.
                state.annotation = annotation;
                let merge_idx = crate::eval::intrinsics::index("MERGE")
                    .expect("MERGE intrinsic must be registered");
                enter_callable(
                    state,
                    view,
                    state.root_env,
                    DecodedRef::Global(merge_idx as u32),
                )?;
            } else {
                let type_name = DataConstructor::try_from(tag)
                    .map(|dc| dc.to_string())
                    .unwrap_or_else(|()| format!("data (tag {tag})"));
                return Err(ExecutionError::NotCallable(annotation, type_name));
            }
        }
        BcContinuation::DeMeta {
            or_else,
            environment,
            ..
        } => {
            let cur = state.current.clone();
            let env = view.from_value(cur, environment, state.annotation)?;
            state.current = BcValue::Closure(BcClosure::new(or_else, env));
        }
        BcContinuation::SeqBind {
            body,
            environment,
            annotation,
        } => {
            state.current = BcValue::Closure(BcClosure::new(body, environment));
            // Only restore a meaningful annotation. A `force`/`Seq` in an outer
            // context (e.g. the render loop) captures `Smid::default()` and must
            // not wipe the live call-site annotation established by an inner
            // `Ann` before a deferred arg-check BIF runs — mirrors the same
            // invalid-annotation guard `handle_op` applies on closure entry.
            // (HeapSyn avoids this by evaluation ordering, raising before the
            // outer SeqBind is restored.)
            if annotation.is_valid() {
                state.annotation = annotation;
            }
        }
        BcContinuation::LookupLitForce {
            key,
            smid,
            default_closure,
        } => {
            // The forced value has arrived. If it is a block, perform the
            // deferred key lookup; otherwise it is a type error.
            if tag == DataConstructor::Block.tag() {
                let block = *state.current.as_closure().ok_or_else(|| {
                    ExecutionError::Panic(
                        state.annotation,
                        "bytecode: LookupLitForce block is not a closure".to_string(),
                    )
                })?;
                state.current = bc_lookup_in_block(prog, state, view, &block, key)
                    .unwrap_or(BcValue::Closure(default_closure));
            } else {
                let ann = if smid.is_valid() {
                    smid
                } else {
                    state.annotation
                };
                return Err(ExecutionError::NoBranchForDataTag(
                    ann,
                    tag,
                    vec![DataConstructor::Block.tag()],
                ));
            }
        }
        BcContinuation::CaptureEnd => {
            state.capture_end_pending = true;
        }
        BcContinuation::FusedPrimopLeft {
            primop_id,
            right,
            environment,
            annotation,
        } => {
            if let Some(field) = fused_boxed_field(state, view, tag, args)? {
                // Boxed native (e.g. `Zdt(field)`): the box has been forced to
                // WHNF, but its *field* may itself be an unevaluated thunk —
                // for `BoxedZdt` it is the `ZDT(..)` intrinsic call
                // (`arith.rs`), an `Op::Bif` node that `native_from_value` will
                // not force. The unfused `binary_wrapper` handles this by
                // `force(local(0), ..)` *after* `unbox_any` (`arith.rs`), so
                // the fused path must force the field too. Re-push this same
                // continuation and re-enter with the field as the value being
                // forced; when it reaches WHNF the `return_native`/`return_data`
                // arm captures the raw native (handling a nested box the same
                // way, if one ever occurs).
                state.stack.push(BcContinuation::FusedPrimopLeft {
                    primop_id,
                    right,
                    environment,
                    annotation,
                });
                state.current = field;
            } else {
                // Non-boxed data operand (bool/list/block/…): bind the whole
                // value; `execute()`'s `num_arg`/`ordered_cmp` classify and
                // error on it exactly as the unfused path does.
                let resolved = state.current.clone();
                state.stack.push(BcContinuation::FusedPrimopRight {
                    primop_id,
                    left: resolved,
                    environment,
                    annotation,
                });
                state.current = BcValue::Closure(BcClosure::new(right, environment));
            }
        }
        BcContinuation::FusedPrimopRight {
            primop_id,
            left,
            environment,
            annotation,
        } => {
            if let Some(field) = fused_boxed_field(state, view, tag, args)? {
                // As above (left operand): force the boxed field to WHNF before
                // capturing it, re-entering this continuation.
                state.stack.push(BcContinuation::FusedPrimopRight {
                    primop_id,
                    left,
                    environment,
                    annotation,
                });
                state.current = field;
            } else {
                let resolved = state.current.clone();
                finish_fused_primop(state, view, primop_id, left, resolved, environment)?;
            }
        }
    }
    Ok(())
}

/// A `FusedPrimop` operand has returned as a data value (`return_data`'s own
/// `tag`/`args`). If it is one of the four boxed-native constructors
/// (`BoxedNumber`/`BoxedString`/`BoxedSymbol`/`BoxedZdt`, all arity 1), return
/// `Some(field_value)` — the single field, resolved against the constructor's
/// own env exactly as `env_from_data_args` reads it — so the caller can force
/// that field to WHNF (mirroring `unbox_any` + `force` in `binary_wrapper`,
/// `arith.rs`). Return `None` for any other tag (bool/list/block/…), signalling
/// the caller to bind the whole value and let `execute()` classify/error on it.
fn fused_boxed_field(
    state: &BcMachineState,
    view: MutatorHeapView<'_>,
    tag: Tag,
    args: &[DecodedRef],
) -> Result<Option<BcValue>, ExecutionError> {
    match DataConstructor::try_from(tag) {
        Ok(DataConstructor::BoxedNumber)
        | Ok(DataConstructor::BoxedString)
        | Ok(DataConstructor::BoxedSymbol)
        | Ok(DataConstructor::BoxedZdt) => {
            let cons_env = state
                .current
                .as_closure()
                .ok_or_else(|| {
                    ExecutionError::Panic(
                        state.annotation,
                        "bytecode: fused-primop operand resolution on a native value".to_string(),
                    )
                })?
                .env();
            let field = args.first().ok_or_else(|| {
                ExecutionError::Panic(
                    state.annotation,
                    "bytecode: boxed-native constructor missing its field".to_string(),
                )
            })?;
            Ok(Some(resolve_ref(
                view,
                &state.constants,
                cons_env,
                state.globals,
                *field,
            )?))
        }
        _ => Ok(None),
    }
}

/// Complete a `FusedPrimop` dispatch once both operands are resolved: bind
/// them into a small scratch environment and defer to the *unmodified*
/// `Op::Bif` dispatch tail via `pending_bif`/`pending_bif_args` — the same
/// mechanism `Op::Bif` itself uses (design §5.6). This function's only job is
/// operand-gathering; it does not perform, or know anything about, the
/// arithmetic/comparison the deferred intrinsic will run.
fn finish_fused_primop(
    state: &mut BcMachineState,
    view: MutatorHeapView<'_>,
    primop_id: u8,
    left: BcValue,
    right: BcValue,
    environment: RefPtr<BcEnvFrame>,
) -> Result<(), ExecutionError> {
    let mut backing = Array::with_capacity(&view, 2);
    backing.push(&view, left);
    backing.push(&view, right);
    let scratch_env = view
        .alloc(BcEnvFrame::new(
            backing,
            state.annotation,
            Some(environment),
        ))?
        .as_ptr();

    state.pending_bif = Some(primop_id);
    let mut pending_args = ArgRefs::new();
    pending_args.push(DecodedRef::Local(0));
    pending_args.push(DecodedRef::Local(1));
    state.pending_bif_args = pending_args;
    // `dispatch()`'s tail reads `state.current.as_closure().map(|c| c.env())`
    // for the `BifFrame`'s rooted env (machine.rs `dispatch`) — this closure
    // is never entered (`pending_bif` short-circuits normal execution before
    // `state.current` is re-entered), so any harmless code offset works;
    // `state.blackhole` is the existing sentinel used for exactly this
    // "placeholder closure, only its env matters" role elsewhere (e.g.
    // `enter_local`'s black-holing).
    state.current = BcValue::Closure(BcClosure::new(state.blackhole, scratch_env));
    Ok(())
}

/// Return a native WHNF value into the top continuation (or terminate).
///
/// Translated from `vm.rs` `return_native` (`:816-897`) for the `BcValue`
/// model: the native is carried directly rather than via an `Atom` closure.
pub fn return_native(
    state: &mut BcMachineState,
    view: MutatorHeapView<'_>,
    value: Native,
) -> Result<(), ExecutionError> {
    let Some(cont) = state.stack.pop() else {
        // Nothing left to consume the value: it is the program result.
        state.current = BcValue::Native(value);
        state.terminated = true;
        return Ok(());
    };
    match cont {
        BcContinuation::Branch {
            fallback,
            environment,
            ..
        } => {
            // Case fallbacks handle natives; the native is bound as slot 0.
            if let Some(fb) = fallback {
                let env = view.from_value(BcValue::Native(value), environment, state.annotation)?;
                state.current = BcValue::Closure(BcClosure::new(fb, env));
            } else {
                return Err(ExecutionError::NoBranchForNative(
                    state.annotation,
                    value.type_description().to_string(),
                ));
            }
        }
        BcContinuation::Update { environment, index } => {
            // Memoise the value into the thunk's slot; leave `current` as the
            // native so it is re-processed against the next continuation.
            view.scoped(environment)
                .update(&view, index, BcValue::Native(value))?;
        }
        BcContinuation::ApplyTo { annotation, .. } => {
            return Err(ExecutionError::NotCallable(
                annotation,
                value.type_description().to_string(),
            ));
        }
        BcContinuation::DeMeta {
            or_else,
            environment,
            ..
        } => {
            let env = view.from_value(BcValue::Native(value), environment, state.annotation)?;
            state.current = BcValue::Closure(BcClosure::new(or_else, env));
        }
        BcContinuation::SeqBind {
            body,
            environment,
            annotation,
        } => {
            // Force-and-discard: enter the body without binding the result.
            state.current = BcValue::Closure(BcClosure::new(body, environment));
            // Only restore a meaningful annotation. A `force`/`Seq` in an outer
            // context (e.g. the render loop) captures `Smid::default()` and must
            // not wipe the live call-site annotation established by an inner
            // `Ann` before a deferred arg-check BIF runs — mirrors the same
            // invalid-annotation guard `handle_op` applies on closure entry.
            // (HeapSyn avoids this by evaluation ordering, raising before the
            // outer SeqBind is restored.)
            if annotation.is_valid() {
                state.annotation = annotation;
            }
        }
        BcContinuation::LookupLitForce { smid, .. } => {
            // A native is not a block — type error.
            let ann = if smid.is_valid() {
                smid
            } else {
                state.annotation
            };
            return Err(ExecutionError::NoBranchForNative(
                ann,
                value.type_description().to_string(),
            ));
        }
        BcContinuation::CaptureEnd => {
            state.capture_end_pending = true;
        }
        BcContinuation::FusedPrimopLeft {
            primop_id,
            right,
            environment,
            annotation,
        } => {
            state.stack.push(BcContinuation::FusedPrimopRight {
                primop_id,
                left: BcValue::Native(value),
                environment,
                annotation,
            });
            state.current = BcValue::Closure(BcClosure::new(right, environment));
        }
        BcContinuation::FusedPrimopRight {
            primop_id,
            left,
            environment,
            ..
        } => {
            finish_fused_primop(
                state,
                view,
                primop_id,
                left,
                BcValue::Native(value),
                environment,
            )?;
        }
    }
    Ok(())
}

/// Return a function value (`current` is a closure of arity > 0) into the
/// top continuation. Translated from `vm.rs` `return_fun` (`:1167`).
///
/// The partial-application (`ApplyTo` with too few args) case needs the
/// PAP trampoline templates and is stubbed until those land.
pub fn return_fun(
    state: &mut BcMachineState,
    view: MutatorHeapView<'_>,
    prog: &BytecodeProgram,
) -> Result<(), ExecutionError> {
    let Some(cont) = state.stack.pop() else {
        state.terminated = true;
        return Ok(());
    };
    let fun = *state.current.as_closure().ok_or_else(|| {
        ExecutionError::Panic(state.annotation, "return_fun on a native value".to_string())
    })?;

    match cont {
        BcContinuation::ApplyTo { args, annotation } => {
            let excess = args.len() as isize - fun.arity() as isize;
            match excess.cmp(&0) {
                Ordering::Equal => {
                    state.current = BcValue::Closure(view.saturate_with_array(&fun, args)?);
                }
                Ordering::Less => {
                    // Partial application: build a PAP trampoline closure. Its
                    // env is `[f, supplied…]` chaining onto `f`'s env; its code
                    // is the pre-encoded `App(L(pending), …)` template; its
                    // arity is the number still pending (spec §6.1).
                    state.current = BcValue::Closure(partially_apply(view, prog, &fun, args)?);
                }
                Ordering::Greater => {
                    let (quorum, surplus) = args.as_slice().split_at(args.len() - excess as usize);
                    state.current = BcValue::Closure(view.saturate(&fun, quorum)?);
                    state.stack.push(BcContinuation::ApplyTo {
                        args: Array::from_slice(&view, surplus),
                        annotation,
                    });
                }
            }
        }
        BcContinuation::Branch {
            min_tag,
            branch_table,
            fallback,
            environment,
            annotation,
        } => {
            // Dynamic typing: code may `case` a value and find it is a lambda.
            if let Some(body) = fallback {
                let env = view.from_value(BcValue::Closure(fun), environment, state.annotation)?;
                state.current = BcValue::Closure(BcClosure::new(body, env));
            } else {
                let expected: Vec<u8> = (0..branch_table.len())
                    .filter(|i| branch_table.get(*i).flatten().is_some())
                    .map(|i| min_tag + i as u8)
                    .collect();
                let ann = if annotation.is_valid() {
                    annotation
                } else {
                    state.annotation
                };
                return Err(ExecutionError::CannotReturnFunToCase(ann, expected));
            }
        }
        BcContinuation::Update { environment, index } => {
            view.scoped(environment)
                .update(&view, index, BcValue::Closure(fun))?;
        }
        BcContinuation::DeMeta {
            or_else,
            environment,
            ..
        } => {
            let env = view.from_value(BcValue::Closure(fun), environment, state.annotation)?;
            state.current = BcValue::Closure(BcClosure::new(or_else, env));
        }
        BcContinuation::SeqBind {
            body,
            environment,
            annotation,
        } => {
            state.current = BcValue::Closure(BcClosure::new(body, environment));
            // Only restore a meaningful annotation. A `force`/`Seq` in an outer
            // context (e.g. the render loop) captures `Smid::default()` and must
            // not wipe the live call-site annotation established by an inner
            // `Ann` before a deferred arg-check BIF runs — mirrors the same
            // invalid-annotation guard `handle_op` applies on closure entry.
            // (HeapSyn avoids this by evaluation ordering, raising before the
            // outer SeqBind is restored.)
            if annotation.is_valid() {
                state.annotation = annotation;
            }
        }
        BcContinuation::LookupLitForce { smid, .. } => {
            let ann = if smid.is_valid() {
                smid
            } else {
                state.annotation
            };
            return Err(ExecutionError::NotCallable(ann, "function".to_string()));
        }
        BcContinuation::CaptureEnd => {
            state.capture_end_pending = true;
        }
        BcContinuation::FusedPrimopLeft {
            primop_id,
            right,
            environment,
            annotation,
        } => {
            // A function value is not a valid primop operand, but this path
            // does not special-case that: bind it opaque, exactly like
            // `return_data`'s non-boxed-native fallback, and let `execute()`'s
            // `resolve_native`/`num_arg`/`ordered_cmp` raise the identical
            // type error the unfused path raises today.
            state.stack.push(BcContinuation::FusedPrimopRight {
                primop_id,
                left: BcValue::Closure(fun),
                environment,
                annotation,
            });
            state.current = BcValue::Closure(BcClosure::new(right, environment));
        }
        BcContinuation::FusedPrimopRight {
            primop_id,
            left,
            environment,
            ..
        } => {
            finish_fused_primop(
                state,
                view,
                primop_id,
                left,
                BcValue::Closure(fun),
                environment,
            )?;
        }
    }
    Ok(())
}

/// Handle a metadata-annotated value (`OP_META`) reaching the top of the
/// stack. Translated from `vm.rs` `return_meta`: a `DeMeta` continuation
/// binds `[meta, body]` into its handler; otherwise the metadata is stripped
/// and the body flows on. `meta_ref`/`body_ref` resolve in `env` (the Meta
/// node's environment).
fn return_meta(
    state: &mut BcMachineState,
    view: MutatorHeapView<'_>,
    env: RefPtr<BcEnvFrame>,
    meta_ref: DecodedRef,
    body_ref: DecodedRef,
) -> Result<(), ExecutionError> {
    let Some(cont) = state.stack.pop() else {
        // Nothing consumes the metadata: strip it and continue with the body.
        state.current = resolve_ref(view, &state.constants, env, state.globals, body_ref)?;
        return Ok(());
    };
    match cont {
        BcContinuation::DeMeta {
            handler,
            environment,
            ..
        } => {
            let meta_v = resolve_ref(view, &state.constants, env, state.globals, meta_ref)?;
            let body_v = resolve_ref(view, &state.constants, env, state.globals, body_ref)?;
            let frame = view.from_values(
                [meta_v, body_v].into_iter(),
                2,
                environment,
                state.annotation,
            )?;
            state.current = BcValue::Closure(BcClosure::new(handler, frame));
        }
        BcContinuation::Update { environment, index } => {
            // Memoise the metadata-annotated value itself (the current Meta
            // closure); it is re-processed against the next continuation on
            // the following step (mirrors the HeapSyn `update` path).
            let cur = state.current.clone();
            view.scoped(environment).update(&view, index, cur)?;
        }
        other => {
            // Any other continuation is metadata-transparent: strip and
            // re-offer the body, restoring the continuation.
            state.current = resolve_ref(view, &state.constants, env, state.globals, body_ref)?;
            state.stack.push(other);
        }
    }
    Ok(())
}

/// Build a partial-application (PAP) trampoline closure for a function
/// applied to too few args. The result closes `[f, supplied…]` over `f`'s
/// env and points at the pre-encoded `App(L(pending), …)` template
/// (`prog.pap`); its arity is the count still pending. Mirrors the HeapSyn
/// `partially_apply` (`env_builder.rs`).
fn partially_apply(
    view: MutatorHeapView<'_>,
    prog: &BytecodeProgram,
    fun: &BcClosure,
    args: Array<BcValue>,
) -> Result<BcClosure, ExecutionError> {
    let supplied = args.len();
    let pending = fun.arity() as usize - supplied;
    let tmpl = prog.pap_offset(supplied, pending).ok_or_else(|| {
        ExecutionError::Panic(
            fun.annotation(),
            format!(
                "bytecode: PAP arity out of range (supplied {supplied}, pending {pending}, max {})",
                super::PAP_MAX_ARITY
            ),
        )
    })?;
    let env = view.from_values(
        std::iter::once(BcValue::Closure(*fun)).chain(args.as_slice().iter().cloned()),
        supplied + 1,
        fun.env(),
        fun.annotation(),
    )?;
    Ok(BcClosure::new_annotated_lambda(
        tmpl,
        pending as u8,
        env,
        fun.annotation(),
    ))
}

/// Decode the field ref of an arg atom-offset (skip the `OP_ATOM` byte and
/// read the inline ref).
fn arg_ref(code: &[u8], atom_off: CodeRef) -> Result<DecodedRef, ExecutionError> {
    let mut pc = atom_off as usize + 1;
    read_ref(code, &mut pc)
}

/// Build an application's argument array. A lazy arg is a closure over its
/// pre-encoded `OP_ATOM`; an eager (`FLAG_EAGER`) `Local` arg is resolved
/// directly from the env (CG3 — avoids O(n) indirection chains).
fn make_arg_array(
    view: MutatorHeapView<'_>,
    code: &[u8],
    env: RefPtr<BcEnvFrame>,
    arg_offs: &[CodeRef],
    eager: bool,
) -> Result<Array<BcValue>, ExecutionError> {
    let mut array = Array::with_capacity(&view, arg_offs.len());
    for off in arg_offs {
        let v = if eager {
            match arg_ref(code, *off)? {
                DecodedRef::Local(i) => view
                    .scoped(env)
                    .get(&view, i as usize)
                    .ok_or(ExecutionError::BadEnvironmentIndex(i as usize))?,
                _ => BcValue::Closure(BcClosure::new(*off, env)),
            }
        } else {
            BcValue::Closure(BcClosure::new(*off, env))
        };
        array.push(&view, v);
    }
    Ok(array)
}

/// Build the static part of a closure from a form kind/arity/annotation
/// and its body offset (shared by let bindings and global forms).
fn form_info(kind: u8, arity: u8, smid: Smid, body: CodeRef) -> InfoTagged<CodeRef> {
    match kind {
        FORM_THUNK => InfoTagged::thunk(body),
        FORM_LAMBDA => InfoTagged::new(arity, body, smid),
        _ => InfoTagged::value(body), // FORM_VALUE
    }
}

/// Build the static part of a closure from a decoded form header.
fn bc_info(h: &FormHeader) -> InfoTagged<CodeRef> {
    form_info(h.kind, h.arity, h.smid, h.body)
}

/// Enter a local env slot, black-holing it and pushing an `Update`
/// continuation if it is a thunk (memoisation + cycle detection). Mirrors
/// the `Atom{L}` thunk dance in `vm.rs`.
fn enter_local(
    state: &mut BcMachineState,
    view: MutatorHeapView<'_>,
    env: RefPtr<BcEnvFrame>,
    i: usize,
) -> Result<(), ExecutionError> {
    let v = view
        .scoped(env)
        .get(&view, i)
        .ok_or(ExecutionError::BadEnvironmentIndex(i))?;
    if let BcValue::Closure(c) = v {
        if c.update() {
            let hole = BcValue::Closure(BcClosure::new(state.blackhole, env));
            view.scoped(env).update(&view, i, hole)?;
            state.stack.push(BcContinuation::Update {
                environment: env,
                index: i,
            });
        }
    }
    state.current = v;
    Ok(())
}

/// Enter a global slot, black-holing it and pushing an `Update` continuation
/// if it is a thunk. Mirrors [`enter_local`] but targets the shared, long-lived
/// globals frame, so a prelude global (stored in the blob as an arity-0
/// updatable thunk) is memoised — computed once and shared by every reference
/// rather than recomputed each time. Restoring this global-thunk sharing is the
/// BV5 blob perf fix (eu-fhoo); without it the blob path re-allocates every
/// prelude combinator/CAF for ~5.6× the allocation of the source pipeline.
fn enter_global(
    state: &mut BcMachineState,
    view: MutatorHeapView<'_>,
    i: usize,
) -> Result<(), ExecutionError> {
    let v = view
        .scoped(state.globals)
        .get(&view, i)
        .ok_or(ExecutionError::BadGlobalIndex(i))?;
    if let BcValue::Closure(c) = v {
        if c.update() {
            let hole = BcValue::Closure(BcClosure::new(state.blackhole, state.globals));
            view.scoped(state.globals).update(&view, i, hole)?;
            state.stack.push(BcContinuation::Update {
                environment: state.globals,
                index: i,
            });
        }
    }
    state.current = v;
    Ok(())
}

/// Resolve and enter an application's callable (mirrors `resolve_callable`
/// + the callable thunk dance).
fn enter_callable(
    state: &mut BcMachineState,
    view: MutatorHeapView<'_>,
    env: RefPtr<BcEnvFrame>,
    callable: DecodedRef,
) -> Result<(), ExecutionError> {
    match callable {
        DecodedRef::Local(i) => enter_local(state, view, env, i as usize),
        DecodedRef::Global(i) => enter_global(state, view, i as usize),
        DecodedRef::Value(_) => Err(ExecutionError::NotCallable(
            state.annotation,
            "native value".to_string(),
        )),
    }
}

/// Execute the code node of the current (arity-0) closure. Translated from
/// `vm.rs` `handle_instruction` (`:390`) for the opcode subset implemented
/// so far; the remaining opcodes error explicitly.
///
/// NOTE: thunk memoisation (blackhole + `Update` push on entering a local
/// thunk) is not yet wired — a local thunk is entered and re-evaluated on
/// each access (correct value, extra work). The blackhole template + Update
/// push land with the App/thunk increment.
pub fn handle_op(
    state: &mut BcMachineState,
    view: MutatorHeapView<'_>,
    prog: &BytecodeProgram,
) -> Result<(), ExecutionError> {
    let code = prog.code.as_slice();
    let closure = *state.current.as_closure().ok_or_else(|| {
        ExecutionError::Panic(state.annotation, "handle_op on a native value".to_string())
    })?;
    let env = closure.env();
    if closure.annotation().is_valid() {
        state.annotation = closure.annotation();
    }

    let mut pc = closure.code() as usize;
    let op = Op::from_u8(read_u8(code, &mut pc)).ok_or_else(|| {
        ExecutionError::Panic(state.annotation, "bytecode: invalid opcode".to_string())
    })?;

    match op {
        Op::Atom => {
            let dref = read_ref(code, &mut pc)?;
            match dref {
                DecodedRef::Local(i) => {
                    enter_local(state, view, env, i as usize)?;
                }
                DecodedRef::Global(i) => {
                    enter_global(state, view, i as usize)?;
                }
                DecodedRef::Value(k) => {
                    let native = match state.constants.get(k as usize) {
                        Some(Ref::V(n)) => n.clone(),
                        _ => {
                            return Err(ExecutionError::Panic(
                                state.annotation,
                                format!("bytecode: constant {k} missing or not a native"),
                            ))
                        }
                    };
                    return_native(state, view, native)?;
                }
            }
        }
        Op::Cons => {
            let tag = read_u8(code, &mut pc);
            let arg_offs = read_arg_offsets(code, &mut pc);
            let arg_refs: ArgRefs = arg_offs
                .iter()
                .map(|off| arg_ref(code, *off))
                .collect::<Result<_, _>>()?;
            return_data(state, view, prog, tag, &arg_refs)?;
        }
        Op::Case => {
            let scr_off = read_u32(code, &mut pc);
            // Decode the densified branch table straight into its final GC
            // `Array` (its home in the `Branch` continuation). The table can
            // span the full data-constructor tag range, so buffering it in a
            // transient `Vec`/`SmallVec` first would either malloc per `Case`
            // or overflow an inline buffer — reading directly avoids both.
            let min_tag = read_u8(code, &mut pc);
            let len = read_u8(code, &mut pc) as usize;
            let mut branch_table = Array::with_capacity(&view, len);
            for _ in 0..len {
                let e = read_u32(code, &mut pc);
                branch_table.push(&view, if e == NO_BRANCH { None } else { Some(e) });
            }
            let has_fb = read_u8(code, &mut pc);
            let fallback = if has_fb == 1 {
                Some(read_u32(code, &mut pc))
            } else {
                None
            };
            state.stack.push(BcContinuation::Branch {
                min_tag,
                branch_table,
                fallback,
                environment: env,
                annotation: state.annotation,
            });
            state.current = BcValue::Closure(BcClosure::new(scr_off, env));
        }
        Op::Seq => {
            let scr_off = read_u32(code, &mut pc);
            let body_off = read_u32(code, &mut pc);
            state.stack.push(BcContinuation::SeqBind {
                body: body_off,
                environment: env,
                annotation: state.annotation,
            });
            state.current = BcValue::Closure(BcClosure::new(scr_off, env));
        }
        Op::Ann => {
            let smid = Smid::from(read_u32(code, &mut pc));
            let body_off = read_u32(code, &mut pc);
            state.annotation = smid;
            state.current = BcValue::Closure(BcClosure::new(body_off, env));
        }
        Op::FusedPrimop => {
            // Force the first (source-order) operand to WHNF, mirroring
            // `Op::Seq`'s decode exactly (design §5.4/§6.2) — the second
            // operand is entered only once the first has resolved.
            let primop_id = read_u8(code, &mut pc);
            let left_off = read_u32(code, &mut pc);
            let right_off = read_u32(code, &mut pc);
            state.stack.push(BcContinuation::FusedPrimopLeft {
                primop_id,
                right: right_off,
                environment: env,
                annotation: state.annotation,
            });
            state.current = BcValue::Closure(BcClosure::new(left_off, env));
        }
        Op::BlackHole => {
            return Err(ExecutionError::BlackHole(state.annotation));
        }
        Op::App => {
            let flags = read_u8(code, &mut pc);
            let eager = flags & super::FLAG_EAGER != 0;
            let callable = read_ref(code, &mut pc)?;
            let arg_offs = read_arg_offsets(code, &mut pc);
            let args = make_arg_array(view, code, env, &arg_offs, eager)?;
            state.stack.push(BcContinuation::ApplyTo {
                args,
                annotation: state.annotation,
            });
            enter_callable(state, view, env, callable)?;
        }
        Op::Let => {
            // Non-recursive: bindings close over the enclosing env. Decode the
            // `u32` count (BV1 wire format v2, eu-2sa6.11 — a large imported
            // literal exceeds the old `u16` limit) then read each form header
            // straight into the managed env-frame `Array` — mirroring
            // `Op::Case`'s branch-table read — so no transient `Vec<FormHeader>`
            // is malloc'd on the system heap per `LET` (the dominant alloc-bound
            // per-op cost, eu-cj1h.3).
            let count = read_u32(code, &mut pc) as usize;
            state.allocs += count as u64;
            let mut array = Array::with_capacity(&view, count);
            for _ in 0..count {
                let h = read_form_header(code, &mut pc);
                array.push(&view, BcValue::Closure(BcClosure::close(&bc_info(&h), env)));
            }
            let body_off = read_u32(code, &mut pc);
            let new_env = view
                .alloc(BcEnvFrame::new(array, state.annotation, Some(env)))?
                .as_ptr();
            state.current = BcValue::Closure(BcClosure::new(body_off, new_env));
        }
        Op::LetRec => {
            // Recursive: bindings close over the new frame itself, so buffer the
            // form headers (decoded once into an inline `SmallVec` — no system
            // heap for the common small-arity case, no re-decode), pre-size the
            // array with dangling closures, allocate the frame, then fill each
            // slot with a closure over that frame (eu-cj1h.3).
            // `u32` binding count (BV1 wire format v2, eu-2sa6.11).
            let count = read_u32(code, &mut pc) as usize;
            let mut headers: FormHeaders = SmallVec::with_capacity(count);
            for _ in 0..count {
                headers.push(read_form_header(code, &mut pc));
            }
            let body_off = read_u32(code, &mut pc);
            state.allocs += count as u64;
            let mut array = Array::with_capacity(&view, count);
            for _ in 0..count {
                array.push(
                    &view,
                    BcValue::Closure(BcClosure::new(0, RefPtr::dangling())),
                );
            }
            let frame = view
                .alloc(BcEnvFrame::new(array.clone(), state.annotation, Some(env)))?
                .as_ptr();
            for (i, h) in headers.iter().enumerate() {
                // SAFETY: array was pre-sized to `count` and i < count.
                unsafe {
                    array.set_unchecked(i, BcValue::Closure(BcClosure::close(&bc_info(h), frame)));
                }
            }
            state.current = BcValue::Closure(BcClosure::new(body_off, frame));
        }
        Op::DirectApp => {
            let flags = read_u8(code, &mut pc);
            let eager = flags & super::FLAG_EAGER != 0;
            let smid = Smid::from(read_u32(code, &mut pc));
            let callable = read_ref(code, &mut pc)?;
            let arg_offs = read_arg_offsets(code, &mut pc);
            // Inline smid replaces a wrapping Ann.
            state.annotation = smid;
            let args = make_arg_array(view, code, env, &arg_offs, eager)?;

            let (callee_env, callee) = match callable {
                DecodedRef::Local(i) => (
                    env,
                    view.scoped(env)
                        .get(&view, i as usize)
                        .ok_or(ExecutionError::BadEnvironmentIndex(i as usize))?,
                ),
                DecodedRef::Global(i) => (
                    state.globals,
                    view.scoped(state.globals)
                        .get(&view, i as usize)
                        .ok_or(ExecutionError::BadGlobalIndex(i as usize))?,
                ),
                DecodedRef::Value(_) => {
                    return Err(ExecutionError::NotCallable(
                        state.annotation,
                        "native value".to_string(),
                    ))
                }
            };
            let BcValue::Closure(c) = callee else {
                return Err(ExecutionError::NotCallable(
                    state.annotation,
                    "native value".to_string(),
                ));
            };

            // Thunk callee: force it before applying the args. This is
            // reachable for a *global* callee too — every pre-compiled prelude
            // binding is stored as an arity-0 updatable thunk (see the xtask
            // `LambdaForm::thunk` wrapping), so e.g. `cons`/`if`/`nil?` are
            // thunks that yield their lambda when forced. Both a local and a
            // global thunk callee are memoised (blackhole + `Update`) below —
            // see the inner comment for why the shared globals frame is
            // updated too (eu-fhoo). Earlier this arm only handled the local
            // case, so a `DirectApp` to an unforced global thunk left
            // `state.current` unchanged and spun forever (eu-0iba).
            if c.update() {
                state.stack.push(BcContinuation::ApplyTo {
                    args,
                    annotation: state.annotation,
                });
                // Memoise the thunk callee. `callee_env` is the local frame for
                // a `Local` callable and the shared globals frame for a
                // `Global` one, so the same blackhole + `Update` dance memoises
                // both: a prelude global (an arity-0 updatable thunk) is
                // computed once and shared, rather than recomputed on every
                // reference. Restoring this global-thunk sharing is the BV5
                // blob perf fix (eu-fhoo); previously only `Local` was memoised.
                if let DecodedRef::Local(i) | DecodedRef::Global(i) = callable {
                    let hole = BcValue::Closure(BcClosure::new(state.blackhole, callee_env));
                    view.scoped(callee_env).update(&view, i as usize, hole)?;
                    state.stack.push(BcContinuation::Update {
                        environment: callee_env,
                        index: i as usize,
                    });
                }
                state.current = callee;
            } else if c.arity() as usize == args.len() {
                // Fast path: exact arity → saturate, skip the ApplyTo push.
                state.current = BcValue::Closure(view.saturate_with_array(&c, args)?);
            } else {
                // Arity mismatch: degrade to normal App semantics.
                state.stack.push(BcContinuation::ApplyTo {
                    args,
                    annotation: state.annotation,
                });
                state.current = callee;
            }
        }
        Op::Bif => {
            // Defer execution to the dispatch tail (needs the full machine /
            // intrinsic context). Capture the intrinsic index and its decoded
            // arg refs — there is no live node to re-read (spec §6.3).
            let intrinsic = read_u8(code, &mut pc);
            let arg_offs = read_arg_offsets(code, &mut pc);
            let arg_refs: ArgRefs = arg_offs
                .iter()
                .map(|off| arg_ref(code, *off))
                .collect::<Result<_, _>>()?;
            state.pending_bif = Some(intrinsic);
            state.pending_bif_args = arg_refs;
        }
        Op::Meta => {
            // Operands are atom offsets for the meta and body refs.
            let meta_off = read_u32(code, &mut pc);
            let body_off = read_u32(code, &mut pc);
            let meta_ref = arg_ref(code, meta_off)?;
            let body_ref = arg_ref(code, body_off)?;
            return_meta(state, view, env, meta_ref, body_ref)?;
        }
        Op::DeMeta => {
            let scr_off = read_u32(code, &mut pc);
            let handler_off = read_u32(code, &mut pc);
            let or_else_off = read_u32(code, &mut pc);
            state.stack.push(BcContinuation::DeMeta {
                handler: handler_off,
                or_else: or_else_off,
                environment: env,
            });
            state.current = BcValue::Closure(BcClosure::new(scr_off, env));
        }
        Op::LookupLit => {
            let smid = Smid::from(read_u32(code, &mut pc));
            let key = read_ref(code, &mut pc)?;
            let obj = read_ref(code, &mut pc)?;
            let default_off = read_u32(code, &mut pc);
            state.annotation = smid;

            // The key is always a `V`-const symbol.
            let sym_id = match key {
                DecodedRef::Value(k) => match state.constants.get(k as usize) {
                    Some(Ref::V(Native::Sym(id))) => *id,
                    _ => {
                        return Err(ExecutionError::NotValue(
                            smid,
                            "non-symbol key in LookupLit".to_string(),
                        ))
                    }
                },
                _ => {
                    return Err(ExecutionError::NotValue(
                        smid,
                        "non-symbol key in LookupLit".to_string(),
                    ))
                }
            };

            // The default is pre-encoded as an atom; wrap it as a closure.
            let default_closure = BcClosure::new(default_off, env);

            // Resolve the object (a local or global) to a closure.
            let (obj_slot, obj_value) = match obj {
                DecodedRef::Local(i) => (
                    Some(i as usize),
                    view.scoped(env)
                        .get(&view, i as usize)
                        .ok_or(ExecutionError::BadEnvironmentIndex(i as usize))?,
                ),
                DecodedRef::Global(i) => (
                    None,
                    view.scoped(state.globals)
                        .get(&view, i as usize)
                        .ok_or(ExecutionError::BadGlobalIndex(i as usize))?,
                ),
                DecodedRef::Value(_) => {
                    return Err(ExecutionError::NotCallable(
                        smid,
                        "native value".to_string(),
                    ))
                }
            };
            let BcValue::Closure(obj_closure) = obj_value else {
                return Err(ExecutionError::NotCallable(
                    smid,
                    "native value".to_string(),
                ));
            };

            // Fast path: the object is already a WHNF block — look up now.
            let is_block = decode_cons(prog, &obj_closure)
                .is_some_and(|(t, _)| t == DataConstructor::Block.tag());
            if is_block {
                state.current = bc_lookup_in_block(prog, state, view, &obj_closure, sym_id)
                    .unwrap_or(BcValue::Closure(default_closure));
            } else {
                // Slow path: force the object, then do the lookup in the
                // LookupLitForce continuation.
                state.stack.push(BcContinuation::LookupLitForce {
                    key: sym_id,
                    smid,
                    default_closure,
                });
                // Black-hole + Update if the object is a local thunk.
                if let (Some(i), true) = (obj_slot, obj_closure.update()) {
                    let hole = BcValue::Closure(BcClosure::new(state.blackhole, env));
                    view.scoped(env).update(&view, i, hole)?;
                    state.stack.push(BcContinuation::Update {
                        environment: env,
                        index: i,
                    });
                }
                state.current = BcValue::Closure(obj_closure);
            }
        }
    }
    Ok(())
}

/// One machine step: dispatch on the current value.
///
/// - `Native` → return it into the top continuation.
/// - `Closure` with arity > 0 → `return_fun` (apply / partial / default).
/// - `Closure` with arity 0 → execute its code node.
pub fn step(
    state: &mut BcMachineState,
    view: MutatorHeapView<'_>,
    prog: &BytecodeProgram,
) -> Result<(), ExecutionError> {
    enum Dispatch {
        Native(Native),
        Fun,
        Op,
    }
    let dispatch = match &state.current {
        BcValue::Native(n) => Dispatch::Native(n.clone()),
        BcValue::Closure(c) if c.arity() > 0 => Dispatch::Fun,
        BcValue::Closure(_) => Dispatch::Op,
    };
    match dispatch {
        Dispatch::Native(n) => return_native(state, view, n),
        Dispatch::Fun => return_fun(state, view, prog),
        Dispatch::Op => handle_op(state, view, prog),
    }
}

/// The bytecode execution machine: owns the heap, program and machine state,
/// and drives the `step` dispatch loop with periodic GC (spec §6). The
/// parallel-engine counterpart to the HeapSyn `Machine`.
///
/// Intrinsic (`Bif`) dispatch runs through a neutral [`BcBifContext`]. The
/// emitter/capture lifecycle and re-entrant `evaluate_to_whnf` are wired in
/// following increments; intrinsics that reach those (or the HeapSyn-typed
/// ABI methods) surface via the differential harness for migration.
pub struct BytecodeMachine<'a> {
    heap: Heap,
    program: BytecodeProgram,
    state: BcMachineState,
    metrics: Metrics,
    clock: Clock,
    dump_heap: bool,
    /// Symbol pool (interned symbols), shared with intrinsic dispatch.
    symbol_pool: SymbolPool,
    /// Regex cache used by string intrinsics.
    rcache: LruCache<String, Regex>,
    /// Whether `__EXPECT` failures return `false` rather than panicking.
    test_mode: bool,
    /// Intrinsic implementations, indexed by intrinsic index (spec §6.3).
    intrinsics: Vec<&'a dyn StgIntrinsic>,
    /// Output emitter.
    emitter: Box<dyn Emitter + 'a>,
    /// Active `render-as` capture emitters (a stack); emit BIFs route to the
    /// top one when non-empty, else to `emitter` (mirrors `Machine`).
    capture_emitters: Vec<OwnedCaptureEmitter>,
}

impl<'a> BytecodeMachine<'a> {
    /// Build a machine for an encoded program rooted at `root_off`. A
    /// `heap_limit_mib` of 0 means an unbounded managed heap. `intrinsics`
    /// must be indexed by intrinsic index (e.g. `runtime.intrinsics()`).
    pub fn new(
        program: BytecodeProgram,
        root_off: CodeRef,
        global_forms: &[GlobalForm],
        intrinsics: Vec<&'a dyn StgIntrinsic>,
        emitter: Box<dyn Emitter + 'a>,
        heap_limit_mib: usize,
        test_mode: bool,
    ) -> Result<Self, ExecutionError> {
        let heap = if heap_limit_mib == 0 {
            Heap::new()
        } else {
            Heap::with_limit(heap_limit_mib)
        };
        let mut pool = SymbolPool::new();
        let (constants, root_env, globals) = {
            let view = MutatorHeapView::new(&heap);
            let constants = program.prepare_constants(view, &mut pool);
            let root_env = view.alloc(BcEnvFrame::default())?.as_ptr();
            // The globals frame holds one closure per encoded global form.
            // Cross-global references compile to `Ref::G(i)` (resolved against
            // this frame), so the closures need only close over `root_env`.
            let globals = if global_forms.is_empty() {
                root_env
            } else {
                view.from_values(
                    global_forms.iter().map(|gf| {
                        BcValue::Closure(BcClosure::close(
                            &form_info(gf.kind, gf.arity, gf.smid, gf.entry),
                            root_env,
                        ))
                    }),
                    global_forms.len(),
                    root_env,
                    Smid::default(),
                )?
            };
            (constants, root_env, globals)
        };
        let mut state = BcMachineState::new(
            BcValue::Closure(BcClosure::new(root_off, root_env)),
            globals,
            root_env,
            constants,
        );
        state.blackhole = program.blackhole;
        Ok(BytecodeMachine {
            heap,
            program,
            state,
            metrics: Metrics::default(),
            clock: Clock::default(),
            dump_heap: false,
            symbol_pool: pool,
            rcache: LruCache::new(
                NonZeroUsize::new(100).expect("regex cache size must be non-zero"),
            ),
            test_mode,
            intrinsics,
            emitter,
            capture_emitters: Vec::new(),
        })
    }

    /// Run to termination (or `limit` ticks), returning the process exit
    /// code. Polls for GC every 500 ticks (mirrors `Machine::run`, spec §6).
    pub fn run(&mut self, limit: Option<usize>) -> Result<Option<u8>, ExecutionError> {
        self.clock.switch(ThreadOccupation::Mutator);
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
                gc_poll(
                    &mut self.state,
                    &mut self.heap,
                    &mut self.clock,
                    self.dump_heap,
                )?;
            }
            self.dispatch()?;
        }

        if self.heap.policy_requires_collection() {
            gc_collect(
                &mut self.state,
                &mut self.heap,
                &mut self.clock,
                self.dump_heap,
            );
        }
        self.clock.stop();
        Ok(self.exit_code())
    }

    /// Machine metrics (ticks, max stack) accumulated across every phase.
    /// Note: allocation counts live on the shared machine state (see
    /// [`Self::allocs`]) because they are recorded inside the free `handle_op`
    /// dispatch, which has no access to the metrics record.
    pub fn metrics(&self) -> &Metrics {
        &self.metrics
    }

    /// Closures allocated by `let`/`letrec` across every execution phase,
    /// counted equivalently to the HeapSyn `Metrics::allocs` so `-S`
    /// allocation figures are comparable across engines.
    pub fn allocs(&self) -> u64 {
        self.state.allocs
    }

    /// GC / occupation timings (mirrors `Machine::clock`).
    pub fn clock(&self) -> &Clock {
        &self.clock
    }

    /// Heap statistics (blocks, LOBs, collection counts) — the same
    /// `HeapStats` the HeapSyn machine reports.
    pub fn heap_stats(&self) -> HeapStats {
        self.heap.stats()
    }

    /// One dispatch step over the shared machine state (the free `step`),
    /// followed by the deferred `Bif` dispatch tail (spec §6.3): when the
    /// `Bif` arm captured an intrinsic, run it through a [`BcBifContext`].
    fn dispatch(&mut self) -> Result<(), ExecutionError> {
        self.metrics.tick();
        self.metrics.stack(self.state.stack.len());
        {
            let view = MutatorHeapView::new(&self.heap);
            // Mirror the HeapSyn `handle_instruction` map_err (vm.rs): attach
            // the env/stack traces at the raise point before unwinding.
            if let Err(e) = step(&mut self.state, view, &self.program) {
                return Err(attach_trace(&self.state, view, e));
            }
        }

        if let Some(idx) = self.state.pending_bif.take() {
            // Materialise the captured arg refs (there is no live code node to
            // re-read, unlike HeapSyn) and push a rooted `BifFrame` holding the
            // env + args, so a collection triggered by an intrinsic force
            // forwards both across the dispatch (eu-zryh). The Bif closure is
            // still `current`; its env holds the (strict, pre-evaluated) args
            // that `Ref::L` operands index into.
            let args = materialize_bif_args(&mut self.state);
            let env = self
                .state
                .current
                .as_closure()
                .map(|c| c.env())
                .unwrap_or(self.state.root_env);
            self.state.bif_frames.push(BifFrame { env, args });
            let bif = self.intrinsics[idx as usize];
            // SAFETY: the view borrows the heap only for the `execute` call and
            // is not retained; `ctx` also holds `&mut heap` for a possible
            // nested `evaluate_to_whnf`. This mirrors the HeapSyn `bif_view`
            // aliasing in `vm.rs`.
            let view = unsafe { MutatorHeapView::from_raw_heap(&self.heap as *const Heap) };
            // Borrow the rooted args by raw pointer so the `&mut state` in `ctx`
            // can coexist. A GC during `execute` rewrites the `BifFrame` in
            // place (the `Vec<Ref>` buffer address is stable — pushing a nested
            // frame may move the `BifFrame` struct but not its args buffer), so
            // the slice stays valid and its `Ref::V` pointers are forwarded.
            // Mirrors HeapSyn re-deriving args from the rooted Bif node (vm.rs).
            let args_ptr: *const [Ref] = self
                .state
                .bif_frames
                .last()
                .expect("bif frame just pushed")
                .args
                .as_slice();
            // Scope `ctx`/`emitter` so their `&mut self` borrows are released
            // before we build the error trace (which reborrows `self.state`).
            let bif_result = {
                let mut ctx = BcBifContext {
                    state: &mut self.state,
                    program: &self.program,
                    symbol_pool: &mut self.symbol_pool,
                    rcache: &mut self.rcache,
                    test_mode: self.test_mode,
                    heap: &mut self.heap,
                    intrinsics: &self.intrinsics,
                    metrics: &mut self.metrics,
                    clock: &mut self.clock,
                    dump_heap: self.dump_heap,
                };
                // Emit BIFs route to the active capture emitter, else the main one.
                let emitter: &mut dyn Emitter = match self.capture_emitters.last_mut() {
                    Some(capture) => capture,
                    None => self.emitter.as_mut(),
                };
                // SAFETY: see the `args_ptr` comment — the slice aliases the
                // rooted frame, which is only read (never resized) during
                // `execute`; GC updates it in place.
                bif.execute(&mut ctx, view, emitter, unsafe { &*args_ptr })
            };
            if let Some(frame) = self.state.bif_frames.pop() {
                recycle_bif_args(&mut self.state, frame);
            }
            // Mirror the HeapSyn BIF-result map_err (vm.rs): the Bif closure is
            // still `current`, so its env yields the trace at the raise point.
            if let Err(e) = bif_result {
                let view = MutatorHeapView::new(&self.heap);
                return Err(attach_trace(&self.state, view, e));
            }
        }

        // Capture lifecycle (mirrors `Machine::step`, vm.rs). A `render-as`
        // intrinsic sets `pending_capture_start`; `CaptureEnd` sets
        // `capture_end_pending`, whereupon the captured string becomes the
        // machine's current value.
        if let Some(format) = self.state.pending_capture_start.take() {
            let mut capture = OwnedCaptureEmitter::new(&format, self.state.annotation)?;
            capture.stream_start();
            self.capture_emitters.push(capture);
        }
        if self.state.capture_end_pending {
            self.state.capture_end_pending = false;
            let mut capture = self.capture_emitters.pop().ok_or_else(|| {
                ExecutionError::Panic(
                    self.state.annotation,
                    "bytecode: no active capture emitter".to_string(),
                )
            })?;
            capture.stream_end();
            let result = capture.into_string(self.state.annotation)?;
            let view = MutatorHeapView::new(&self.heap);
            let ptr = view.alloc(HeapString::from_str(&view, &result))?.as_ptr();
            self.state.current = BcValue::Native(Native::Str(ptr));
        }
        Ok(())
    }

    /// Recover the emitter after a run (so the caller can `stream_end` and
    /// read its output), replacing it with a `NullEmitter`.
    pub fn take_emitter(&mut self) -> Box<dyn Emitter + 'a> {
        std::mem::replace(&mut self.emitter, Box::new(NullEmitter))
    }

    /// The process exit code from a terminated machine's result value
    /// (mirrors `Machine::exit_code`: a numeric result is the code, a
    /// tag-0 success constructor exits 0, anything else exits 1).
    fn exit_code(&self) -> Option<u8> {
        if !self.state.terminated {
            return None;
        }
        Some(match &self.state.current {
            BcValue::Native(Native::Num(n)) => {
                n.as_i64().and_then(|i| u8::try_from(i).ok()).unwrap_or(1)
            }
            BcValue::Native(_) => 1,
            BcValue::Closure(c) => {
                let mut pc = c.code() as usize;
                match Op::from_u8(read_u8(&self.program.code, &mut pc)) {
                    Some(Op::Cons) if read_u8(&self.program.code, &mut pc) == 0 => 0,
                    _ => 1,
                }
            }
        })
    }

    /// Force a collection now (tests: prove the root set survives GC).
    #[cfg(test)]
    fn gc_now(&mut self) {
        gc_collect(
            &mut self.state,
            &mut self.heap,
            &mut self.clock,
            self.dump_heap,
        );
    }
}

// ── io-run driver support (spec §6; eu-lka7) ────────────────────────────────
//
// The public surface the bytecode io-run driver (`driver::bytecode_io_run`)
// needs to drive the IO monad: yield inspection, GC-safe stashing, re-entry,
// re-entrant WHNF sub-evaluation, and the data-synthesis / navigation helpers
// that build IO result values and read IO action spec blocks. This mirrors the
// HeapSyn `Machine` surface used by `driver::io_run` (`vm.rs`), one method per
// analogue, kept in a single additive block to localise the addition.
impl BytecodeMachine<'_> {
    /// Whether the machine has yielded on an IO constructor at the top.
    pub fn io_yielded(&self) -> bool {
        self.state.yielded_io
    }

    /// The current source annotation (stamped onto IO errors).
    pub fn annotation(&self) -> Smid {
        self.state.annotation
    }

    /// The current machine value (a clone; GC roots stay in the state).
    pub fn current(&self) -> BcValue {
        self.state.current.clone()
    }

    /// The empty root environment frame.
    pub fn root_env(&self) -> RefPtr<BcEnvFrame> {
        self.state.root_env
    }

    /// Intern a symbol into the machine's pool, returning its id.
    pub fn intern_symbol(&mut self, s: &str) -> SymbolId {
        self.symbol_pool.intern(s)
    }

    /// Push a value onto the GC-visible stash (a root across `run`/WHNF calls).
    pub fn stash_push(&mut self, value: BcValue) {
        self.state.stash.push(value);
    }

    /// Pop the top stashed value.
    pub fn stash_pop(&mut self) -> BcValue {
        self.state
            .stash
            .pop()
            .expect("bytecode io-run stash underflow")
    }

    /// Peek a stashed value `depth` slots from the top (0 = top).
    pub fn stash_peek(&self, depth: usize) -> BcValue {
        let len = self.state.stash.len();
        self.state.stash[len - 1 - depth].clone()
    }

    /// Resume execution with a new value after an IO yield: clear the
    /// terminated / yielded flags and re-seat `current` (mirrors `vm.rs`
    /// `resume`). A subsequent `run` drives from the new value.
    pub fn resume(&mut self, value: BcValue) {
        self.state.terminated = false;
        self.state.yielded_io = false;
        self.state.current = value;
    }

    /// The IO constructor tag the machine yielded on, or `None` if it has not
    /// yielded on an IO constructor.
    pub fn yielded_io_tag(&self) -> Option<Tag> {
        if !self.state.yielded_io {
            return None;
        }
        let c = self.state.current.as_closure()?;
        decode_cons(&self.program, c).map(|(tag, _)| tag)
    }

    /// Resolve the argument values of the IO constructor the machine yielded
    /// on, in declaration order. `None` if it has not yielded on one.
    pub fn yielded_io_args(&self) -> Option<Vec<BcValue>> {
        if !self.state.yielded_io {
            return None;
        }
        let view = MutatorHeapView::new(&self.heap);
        let c = *self.state.current.as_closure()?;
        let (_, offsets) = decode_cons(&self.program, &c)?;
        let code = self.program.code.as_slice();
        let mut out = Vec::with_capacity(offsets.len());
        for off in offsets {
            let dref = arg_ref(code, off).ok()?;
            let v = resolve_ref(
                view,
                &self.state.constants,
                c.env(),
                self.state.globals,
                dref,
            )
            .ok()?;
            out.push(v);
        }
        Some(out)
    }

    /// Evaluate `value` to WHNF on a fresh continuation stack, then restore the
    /// IO yield state so the driver sees a consistent post-yield machine
    /// (mirrors `vm.rs` `evaluate_to_whnf_for_io`). Errors if the
    /// sub-evaluation itself yields an IO constructor (spec fields are pure).
    pub fn evaluate_to_whnf_for_io(&mut self, value: BcValue) -> Result<BcValue, ExecutionError> {
        // Shared suspend/stash/swap choreography (eu-apn7). The io path forces
        // the yield state (`terminated`/`yielded_io` = true) on exit so the
        // driver sees a consistent post-yield machine, whether or not the
        // sub-run errored.
        let _ = enter_whnf_subrun(&mut self.state, value);
        let run_result = self.drive_whnf();
        exit_whnf_subrun(
            &mut self.state,
            run_result,
            true,
            true,
            "bytecode: IO spec field evaluation unexpectedly yielded an IO constructor",
        )
    }

    /// The re-entrant dispatch loop for `evaluate_to_whnf_for_io`: drives
    /// `dispatch` (which runs the deferred `Bif` tail + capture lifecycle)
    /// until the sub-evaluation terminates. Mirrors `run` — both the loop body
    /// and, crucially, its tail: the clock is **stopped** on return so the
    /// mutator segment does not keep ticking after the sub-run ends.
    ///
    /// `evaluate_to_whnf_for_io` is invoked directly by the io-run driver
    /// (`bytecode_io_run`), which then blocks on a subprocess wait (`run_spec`)
    /// before re-entering the machine. If the clock were left running in the
    /// mutator segment (as it was before eu-yhih), that blocking wait would be
    /// committed to VM-Mutator on the next `switch`, so an IO-dominated
    /// program's whole subprocess wait would be mis-attributed to the mutator —
    /// contradicting the `-S` top bar's "IO ~100%" and diverging from HeapSyn,
    /// whose `evaluate_to_whnf_for_io` delegates to `run` and so stops its clock
    /// on return. Mirroring `run`'s tail here keeps `-S` clock semantics
    /// identical across the two engines.
    fn drive_whnf(&mut self) -> Result<(), ExecutionError> {
        self.clock.switch(ThreadOccupation::Mutator);
        let gc_check_freq: u32 = 500;
        let mut gc_countdown: u32 = gc_check_freq;
        while !self.state.terminated {
            gc_countdown -= 1;
            if gc_countdown == 0 {
                gc_countdown = gc_check_freq;
                gc_poll(
                    &mut self.state,
                    &mut self.heap,
                    &mut self.clock,
                    self.dump_heap,
                )?;
            }
            self.dispatch()?;
        }

        if self.heap.policy_requires_collection() {
            gc_collect(
                &mut self.state,
                &mut self.heap,
                &mut self.clock,
                self.dump_heap,
            );
        }
        self.clock.stop();
        Ok(())
    }

    /// Statically peel an `IoAction` spec block, mirroring `io_run::peel_meta`.
    ///
    /// The spec block produced by `io.shell`/`io.exec`/`io.fail` is a
    /// `let`-bound `Meta(tag, block-thunk)` value whose field refs are relative
    /// to the **let frame** (the container), while the `Meta` closure's own env
    /// is the enclosing scope. Forcing the `Meta` naively resolves its body ref
    /// against the wrong (own) env — grabbing an enclosing binding rather than
    /// the block — so we follow `OP_ATOM` indirections tracking the container
    /// frame, and when we reach a `Meta` resolve its body ref against that
    /// container, yielding the block-thunk closure (whose own env is correct
    /// for its interior refs). For parameterised (App-thunk) spec blocks
    /// (`io.shell-with`, `io.exec-with`) no static `Meta` is visible; the spec
    /// is returned unchanged and forcing it evaluates the application to the
    /// block directly (the machine strips the `Meta` as nothing consumes it).
    pub fn peel_io_spec(&self, spec: BcValue) -> BcValue {
        let view = MutatorHeapView::new(&self.heap);
        let code = self.program.code.as_slice();
        let mut current = spec;
        let mut container: Option<RefPtr<BcEnvFrame>> = None;
        for _ in 0..64 {
            let c = match &current {
                BcValue::Closure(c) => *c,
                BcValue::Native(_) => break,
            };
            let mut pc = c.code() as usize;
            match Op::from_u8(read_u8(code, &mut pc)) {
                Some(Op::Atom) => {
                    let Ok(dref) = read_ref(code, &mut pc) else {
                        break;
                    };
                    match dref {
                        DecodedRef::Local(i) => {
                            let env = c.env();
                            match view.scoped(env).get(&view, i as usize) {
                                Some(inner) => {
                                    container = Some(env);
                                    current = inner;
                                }
                                None => break,
                            }
                        }
                        DecodedRef::Global(i) => {
                            match view.scoped(self.state.globals).get(&view, i as usize) {
                                Some(inner) => current = inner,
                                None => break,
                            }
                        }
                        DecodedRef::Value(_) => break,
                    }
                }
                Some(Op::Ann) => {
                    // Source annotation: transparent to spec navigation.
                    let _smid = read_u32(code, &mut pc);
                    let body_off = read_u32(code, &mut pc);
                    current = BcValue::Closure(BcClosure::new(body_off, c.env()));
                }
                Some(Op::Meta) => {
                    let _meta_off = read_u32(code, &mut pc);
                    let body_off = read_u32(code, &mut pc);
                    let Ok(body_ref) = arg_ref(code, body_off) else {
                        break;
                    };
                    // Resolve the body against the container frame (the let
                    // frame that holds this Meta binding), falling back to the
                    // Meta's own env when reached without an indirection.
                    let env = container.unwrap_or_else(|| c.env());
                    match resolve_ref(
                        view,
                        &self.state.constants,
                        env,
                        self.state.globals,
                        body_ref,
                    ) {
                        Ok(body) => return body,
                        Err(_) => break,
                    }
                }
                _ => break,
            }
        }
        current
    }

    /// Recover the static meta tag symbol (`io-shell` / `io-exec` / `io-fail`)
    /// of an `IoAction` spec block, if it is an inline `Meta`-tagged block.
    ///
    /// This mirrors the navigation of [`Self::peel_io_spec`] but resolves the
    /// `Meta` node's *meta* ref (rather than its body) to a symbol name. It is
    /// a purely additive, read-only accessor introduced for the cross-engine IO
    /// action dispatch parity fix (eu-xqab): forcing the spec block strips the
    /// `Meta` wrapper, so the tag must be read statically here — before forcing
    /// — for the bytecode driver to dispatch on the tag exactly as the HeapSyn
    /// driver's `peel_meta` does.
    ///
    /// Returns `None` for parameterised (App-thunk) spec blocks
    /// (`io.shell-with` / `io.exec-with`) whose `Meta` is not statically
    /// visible; the caller then falls back to field-based inference.
    pub fn peel_io_spec_tag(&self, spec: BcValue) -> Option<String> {
        let view = MutatorHeapView::new(&self.heap);
        let code = self.program.code.as_slice();
        let mut current = spec;
        let mut container: Option<RefPtr<BcEnvFrame>> = None;
        for _ in 0..64 {
            let c = match &current {
                BcValue::Closure(c) => *c,
                BcValue::Native(_) => return None,
            };
            let mut pc = c.code() as usize;
            match Op::from_u8(read_u8(code, &mut pc)) {
                Some(Op::Atom) => {
                    let dref = read_ref(code, &mut pc).ok()?;
                    match dref {
                        DecodedRef::Local(i) => {
                            let env = c.env();
                            let inner = view.scoped(env).get(&view, i as usize)?;
                            container = Some(env);
                            current = inner;
                        }
                        DecodedRef::Global(i) => {
                            current = view.scoped(self.state.globals).get(&view, i as usize)?;
                        }
                        DecodedRef::Value(_) => return None,
                    }
                }
                Some(Op::Ann) => {
                    let _smid = read_u32(code, &mut pc);
                    let body_off = read_u32(code, &mut pc);
                    current = BcValue::Closure(BcClosure::new(body_off, c.env()));
                }
                Some(Op::Meta) => {
                    let meta_off = read_u32(code, &mut pc);
                    let _body_off = read_u32(code, &mut pc);
                    let meta_ref = arg_ref(code, meta_off).ok()?;
                    // Resolve the meta ref against the container frame (the let
                    // frame that holds this Meta binding), falling back to the
                    // Meta's own env when reached without an indirection.
                    let env = container.unwrap_or_else(|| c.env());
                    let meta_v = resolve_ref(
                        view,
                        &self.state.constants,
                        env,
                        self.state.globals,
                        meta_ref,
                    )
                    .ok()?;
                    return value_symbol(&self.program, &self.state, view, meta_v)
                        .map(|id| self.symbol_pool.resolve(id).to_string());
                }
                _ => return None,
            }
        }
        None
    }

    // ── Data synthesis (pre-encoded templates; no per-call code alloc) ──────

    /// Build a data-constructor value of `tag` over `fields` using the
    /// pre-encoded constructor template (mirrors `BcBifContext::build_data`).
    pub fn build_data(&self, tag: Tag, fields: &[BcValue]) -> Result<BcValue, ExecutionError> {
        let view = MutatorHeapView::new(&self.heap);
        let code = self
            .program
            .templates
            .get(tag as usize)
            .copied()
            .ok_or_else(|| {
                ExecutionError::Panic(
                    self.state.annotation,
                    format!("bytecode: no constructor template for tag {tag}"),
                )
            })?;
        let env = if fields.is_empty() {
            self.state.root_env
        } else {
            view.from_values(
                fields.iter().cloned(),
                fields.len(),
                self.state.root_env,
                self.state.annotation,
            )?
        };
        Ok(BcValue::Closure(BcClosure::new(code, env)))
    }

    /// Build a lazy `f(arg)` application thunk via the `apply1` template.
    pub fn build_apply1(&self, f: BcValue, arg: BcValue) -> Result<BcValue, ExecutionError> {
        let view = MutatorHeapView::new(&self.heap);
        let env = view.from_values(
            [f, arg].into_iter(),
            2,
            self.state.root_env,
            self.state.annotation,
        )?;
        Ok(BcValue::Closure(BcClosure::new(
            self.program.apply1_template,
            env,
        )))
    }

    /// Build a lazy `f(a0, a1)` application thunk via the `apply2` template.
    pub fn build_apply2(
        &self,
        f: BcValue,
        a0: BcValue,
        a1: BcValue,
    ) -> Result<BcValue, ExecutionError> {
        let view = MutatorHeapView::new(&self.heap);
        let env = view.from_values(
            [f, a0, a1].into_iter(),
            3,
            self.state.root_env,
            self.state.annotation,
        )?;
        Ok(BcValue::Closure(BcClosure::new(
            self.program.apply2_template,
            env,
        )))
    }

    /// The zero-field `Unit` constructor value (the initial world token).
    pub fn build_unit(&self) -> Result<BcValue, ExecutionError> {
        self.build_data(DataConstructor::Unit.tag(), &[])
    }

    /// Resolve a global slot to its runtime value (e.g. the `RENDER_DOC`
    /// intrinsic wrapper closure) so it can be applied as an ordinary callable.
    pub fn global_value(&self, index: usize) -> Result<BcValue, ExecutionError> {
        let view = MutatorHeapView::new(&self.heap);
        view.scoped(self.state.globals)
            .get(&view, index)
            .ok_or(ExecutionError::BadGlobalIndex(index))
    }

    /// Build the shell result block `{stdout: Str, stderr: Str, exit-code: Num}`
    /// on the heap (mirrors `io_run::BuildResultBlock`). Strings are wrapped in
    /// `BoxedString` and the exit code in `BoxedNumber` so eucalypt intrinsics
    /// that case-match those tags receive them in the expected form.
    pub fn build_io_result_block(
        &mut self,
        stdout: &str,
        stderr: &str,
        exit_code: i64,
    ) -> Result<BcValue, ExecutionError> {
        let stdout_sym = self.symbol_pool.intern("stdout");
        let stderr_sym = self.symbol_pool.intern("stderr");
        let exitcode_sym = self.symbol_pool.intern("exit-code");

        let view = MutatorHeapView::new(&self.heap);
        let stdout_ptr = view.alloc(HeapString::from_str(&view, stdout))?.as_ptr();
        let stderr_ptr = view.alloc(HeapString::from_str(&view, stderr))?.as_ptr();

        let stdout_box = self.build_data(
            DataConstructor::BoxedString.tag(),
            &[BcValue::Native(Native::Str(stdout_ptr))],
        )?;
        let stderr_box = self.build_data(
            DataConstructor::BoxedString.tag(),
            &[BcValue::Native(Native::Str(stderr_ptr))],
        )?;
        let exit_box = self.build_data(
            DataConstructor::BoxedNumber.tag(),
            &[BcValue::Native(Native::Num(Number::from(exit_code)))],
        )?;

        // Three BlockPair(key-sym, value) entries.
        let pair0 = self.build_data(
            DataConstructor::BlockPair.tag(),
            &[BcValue::Native(Native::Sym(stdout_sym)), stdout_box],
        )?;
        let pair1 = self.build_data(
            DataConstructor::BlockPair.tag(),
            &[BcValue::Native(Native::Sym(stderr_sym)), stderr_box],
        )?;
        let pair2 = self.build_data(
            DataConstructor::BlockPair.tag(),
            &[BcValue::Native(Native::Sym(exitcode_sym)), exit_box],
        )?;

        // Cons list, tail-first: nil → [pair2] → [pair1] → [pair0].
        let nil = self.build_data(DataConstructor::ListNil.tag(), &[])?;
        let c2 = self.build_data(DataConstructor::ListCons.tag(), &[pair2, nil])?;
        let c1 = self.build_data(DataConstructor::ListCons.tag(), &[pair1, c2])?;
        let c0 = self.build_data(DataConstructor::ListCons.tag(), &[pair0, c1])?;

        // Block { entry-list, no-index } — the index arg is ignored on lookup.
        let no_index = BcValue::Native(Native::Num(Number::from(0)));
        self.build_data(DataConstructor::Block.tag(), &[c0, no_index])
    }

    /// Build `IoReturn(world, value)` to resume the io-run loop.
    pub fn build_io_return(
        &self,
        world: BcValue,
        value: BcValue,
    ) -> Result<BcValue, ExecutionError> {
        self.build_data(DataConstructor::IoReturn.tag(), &[world, value])
    }

    // ── Navigation of forced IO values (read-only) ─────────────────────────

    /// If `value` is a WHNF scalar (native, boxed scalar, unit, bool), extract
    /// its string form (mirrors `render_to_string::extract_scalar_string` over
    /// `BcValue`). `None` for complex values (blocks, lists) or non-scalars.
    pub fn whnf_scalar_string(&self, value: &BcValue) -> Option<String> {
        let view = MutatorHeapView::new(&self.heap);
        self.scalar_string_inner(view, value)
    }

    fn scalar_string_inner(&self, view: MutatorHeapView<'_>, value: &BcValue) -> Option<String> {
        match value {
            BcValue::Native(n) => scalar_from_native(&self.symbol_pool, view, n),
            BcValue::Closure(c) => {
                let code = self.program.code.as_slice();
                let mut pc = c.code() as usize;
                match Op::from_u8(read_u8(code, &mut pc)) {
                    Some(Op::Atom) => {
                        let dref = read_ref(code, &mut pc).ok()?;
                        let inner = resolve_ref(
                            view,
                            &self.state.constants,
                            c.env(),
                            self.state.globals,
                            dref,
                        )
                        .ok()?;
                        self.scalar_string_inner(view, &inner)
                    }
                    Some(Op::Cons) => {
                        let tag = read_u8(code, &mut pc);
                        let dc = DataConstructor::try_from(tag).ok()?;
                        match dc {
                            DataConstructor::Unit => Some(String::new()),
                            DataConstructor::BoolTrue => Some("true".to_string()),
                            DataConstructor::BoolFalse => Some("false".to_string()),
                            DataConstructor::BoxedNumber
                            | DataConstructor::BoxedString
                            | DataConstructor::BoxedSymbol
                            | DataConstructor::BoxedZdt => {
                                let offsets = read_arg_offsets(code, &mut pc);
                                let field_ref = arg_ref(code, *offsets.first()?).ok()?;
                                let inner = resolve_ref(
                                    view,
                                    &self.state.constants,
                                    c.env(),
                                    self.state.globals,
                                    field_ref,
                                )
                                .ok()?;
                                self.scalar_string_inner(view, &inner)
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                }
            }
        }
    }

    /// Whether `value` is a list (`ListCons`/`ListNil`) constructor.
    pub fn value_is_list(&self, value: &BcValue) -> bool {
        let BcValue::Closure(c) = value else {
            return false;
        };
        match decode_cons(&self.program, c) {
            Some((tag, _)) => {
                tag == DataConstructor::ListCons.tag() || tag == DataConstructor::ListNil.tag()
            }
            None => false,
        }
    }

    /// Collect the raw (unevaluated) element values of a `ListCons` chain.
    /// Mirrors `io_run::CollectListElements`.
    pub fn list_element_values(&self, value: &BcValue) -> Result<Vec<BcValue>, ExecutionError> {
        let view = MutatorHeapView::new(&self.heap);
        let code = self.program.code.as_slice();
        let mut out = Vec::new();
        let mut current = value.clone();
        while let BcValue::Closure(c) = current.clone() {
            let Some((tag, offsets)) = decode_cons(&self.program, &c) else {
                break;
            };
            if tag == DataConstructor::ListNil.tag() {
                break;
            }
            if tag != DataConstructor::ListCons.tag() {
                break;
            }
            let head_ref = arg_ref(
                code,
                *offsets.first().ok_or_else(|| {
                    ExecutionError::Panic(self.state.annotation, "malformed list cons".to_string())
                })?,
            )?;
            let head = resolve_ref(
                view,
                &self.state.constants,
                c.env(),
                self.state.globals,
                head_ref,
            )?;
            out.push(head);
            let tail_ref = arg_ref(
                code,
                *offsets.get(1).ok_or_else(|| {
                    ExecutionError::Panic(self.state.annotation, "malformed list cons".to_string())
                })?,
            )?;
            current = resolve_ref(
                view,
                &self.state.constants,
                c.env(),
                self.state.globals,
                tail_ref,
            )?;
        }
        Ok(out)
    }

    /// If `value` is a boxed scalar whose inner field is still an unevaluated
    /// closure (e.g. `BoxedString` wrapping a format-expr thunk), return the
    /// inner value so the driver can force it. `None` if already a native or
    /// not a box. Mirrors `io_run::peel_box_inner`.
    pub fn peel_box_inner(&self, value: &BcValue) -> Option<BcValue> {
        let view = MutatorHeapView::new(&self.heap);
        let BcValue::Closure(c) = value else {
            return None;
        };
        let (tag, offsets) = decode_cons(&self.program, c)?;
        let is_box = tag == DataConstructor::BoxedString.tag()
            || tag == DataConstructor::BoxedNumber.tag()
            || tag == DataConstructor::BoxedSymbol.tag()
            || tag == DataConstructor::BoxedZdt.tag();
        if !is_box {
            return None;
        }
        let field_ref = arg_ref(self.program.code.as_slice(), *offsets.first()?).ok()?;
        let inner = resolve_ref(
            view,
            &self.state.constants,
            c.env(),
            self.state.globals,
            field_ref,
        )
        .ok()?;
        // Only re-force closures; a native is already fully evaluated.
        match inner {
            BcValue::Closure(_) => Some(inner),
            BcValue::Native(_) => None,
        }
    }

    /// Read the raw (unevaluated) field values of an evaluated IO spec block,
    /// keyed by symbol-key name. Mirrors `io_run::collect_raw_block_fields`;
    /// keys that are not readable symbols are skipped.
    pub fn block_field_values(
        &self,
        block: &BcValue,
    ) -> Result<Vec<(String, BcValue)>, ExecutionError> {
        let view = MutatorHeapView::new(&self.heap);
        let code = self.program.code.as_slice();
        let BcValue::Closure(bc) = block else {
            return Err(ExecutionError::Panic(
                self.state.annotation,
                "bytecode: IO spec block is not a block value".to_string(),
            ));
        };
        let (btag, boffs) = decode_cons(&self.program, bc).ok_or_else(|| {
            ExecutionError::Panic(
                self.state.annotation,
                "bytecode: IO spec block did not evaluate to a Block constructor".to_string(),
            )
        })?;
        if btag != DataConstructor::Block.tag() {
            return Err(ExecutionError::Panic(
                self.state.annotation,
                "bytecode: IO spec block did not evaluate to a Block constructor".to_string(),
            ));
        }
        let list_ref = arg_ref(
            code,
            *boffs.first().ok_or_else(|| {
                ExecutionError::Panic(self.state.annotation, "malformed Block".to_string())
            })?,
        )?;
        let mut current = resolve_ref(
            view,
            &self.state.constants,
            bc.env(),
            self.state.globals,
            list_ref,
        )?;

        let mut fields = Vec::new();
        while let BcValue::Closure(c) = current.clone() {
            let Some((tag, offs)) = decode_cons(&self.program, &c) else {
                break;
            };
            if tag == DataConstructor::ListNil.tag() {
                break;
            }
            if tag != DataConstructor::ListCons.tag() {
                break;
            }
            let head_ref = arg_ref(
                code,
                *offs.first().ok_or_else(|| {
                    ExecutionError::Panic(self.state.annotation, "malformed list cons".to_string())
                })?,
            )?;
            let head = resolve_ref(
                view,
                &self.state.constants,
                c.env(),
                self.state.globals,
                head_ref,
            )?;
            if let BcValue::Closure(pc) = &head {
                if let Some((ptag, poffs)) = decode_cons(&self.program, pc) {
                    if ptag == DataConstructor::BlockPair.tag() && poffs.len() >= 2 {
                        let key_ref = arg_ref(code, poffs[0])?;
                        let key = resolve_ref(
                            view,
                            &self.state.constants,
                            pc.env(),
                            self.state.globals,
                            key_ref,
                        )?;
                        if let Some(sym) = value_symbol(&self.program, &self.state, view, key) {
                            let val_ref = arg_ref(code, poffs[1])?;
                            let val = resolve_ref(
                                view,
                                &self.state.constants,
                                pc.env(),
                                self.state.globals,
                                val_ref,
                            )?;
                            fields.push((self.symbol_pool.resolve(sym).to_string(), val));
                        }
                    }
                }
            }
            let tail_ref = arg_ref(
                code,
                *offs.get(1).ok_or_else(|| {
                    ExecutionError::Panic(self.state.annotation, "malformed list cons".to_string())
                })?,
            )?;
            current = resolve_ref(
                view,
                &self.state.constants,
                c.env(),
                self.state.globals,
                tail_ref,
            )?;
        }
        Ok(fields)
    }
}

/// Convert a native value to its string representation (bytecode analogue of
/// `render_to_string::scalar_from_native`).
fn scalar_from_native(
    pool: &SymbolPool,
    view: MutatorHeapView<'_>,
    native: &Native,
) -> Option<String> {
    match native {
        Native::Num(n) => Some(n.to_string()),
        Native::Str(s) => Some((*view.scoped(*s)).as_str().to_string()),
        Native::Sym(id) => Some(pool.resolve(*id).to_string()),
        Native::Zdt(dt) => Some(dt.to_string()),
        _ => None,
    }
}

/// The `IntrinsicMachine` implementation for the bytecode engine (spec §5.5).
///
/// It overrides the neutral, code-type-agnostic ABI methods to operate over
/// `BcValue`s (resolving refs against the Bif closure's env / the globals
/// frame / the constant pool, and returning results by mutating the machine
/// `current`). The HeapSyn-typed methods (`nav`/`set_closure`/`root_env`/
/// `env`/`evaluate_to_whnf`) are unreachable on this path and panic; an
/// intrinsic still using them (or the not-yet-wired capture lifecycle)
/// surfaces via the differential harness for migration.
struct BcBifContext<'ctx, 'a> {
    state: &'ctx mut BcMachineState,
    program: &'ctx BytecodeProgram,
    symbol_pool: &'ctx mut SymbolPool,
    rcache: &'ctx mut LruCache<String, Regex>,
    test_mode: bool,
    /// The machine heap (for the re-entrant `evaluate_to_whnf` sub-run).
    heap: &'ctx mut Heap,
    /// Intrinsics, indexed by intrinsic index (for nested `Bif` dispatch).
    intrinsics: &'ctx [&'a dyn StgIntrinsic],
    metrics: &'ctx mut Metrics,
    clock: &'ctx mut Clock,
    dump_heap: bool,
}

impl BcBifContext<'_, '_> {
    /// The env of the innermost active BIF dispatch — the frame this
    /// intrinsic's `Ref::L` operands index into. Read from the rooted
    /// `bif_frames` stack (not a cached copy) so a collection triggered by a
    /// force resolves to the forwarded frame (eu-zryh).
    fn bif_env(&self) -> RefPtr<BcEnvFrame> {
        self.state
            .bif_frames
            .last()
            .map(|f| f.env)
            .unwrap_or(self.state.root_env)
    }

    /// Resolve a runtime value to a native, following `OP_ATOM` indirections
    /// (the bytecode analogue of `HeapNavigator::resolve_native`).
    fn native_from_value(
        &self,
        view: MutatorHeapView<'_>,
        value: BcValue,
    ) -> Result<Native, ExecutionError> {
        match value {
            BcValue::Native(n) => Ok(n),
            BcValue::Closure(c) => {
                let code = self.program.code.as_slice();
                let mut pc = c.code() as usize;
                let op = Op::from_u8(read_u8(code, &mut pc));
                if op == Some(Op::Atom) {
                    let dref = read_ref(code, &mut pc)?;
                    let inner = resolve_ref(
                        view,
                        &self.state.constants,
                        c.env(),
                        self.state.globals,
                        dref,
                    )?;
                    self.native_from_value(view, inner)
                } else {
                    // Classify the non-native closure so the diagnostic names
                    // what was found (a boolean, a list, a block, …). Mirrors
                    // the HeapSyn `HeapNavigator::resolve_native` discrimination
                    // (vm.rs): decode the head opcode, reading a `Cons` tag to
                    // distinguish the data constructors. `pc` already sits just
                    // past the opcode byte.
                    let description = not_value_description(op, code, &mut pc);
                    Err(ExecutionError::NotValue(
                        self.state.annotation,
                        description.to_string(),
                    ))
                }
            }
        }
    }

    /// Resolve a ref to a runtime `BcValue` (the neutral resolve rule: locals
    /// and globals looked up, `V` returned as a native).
    fn resolve_value(
        &self,
        view: MutatorHeapView<'_>,
        arg: &Ref,
    ) -> Result<BcValue, ExecutionError> {
        match arg {
            Ref::V(n) => Ok(BcValue::Native(n.clone())),
            Ref::L(i) => view
                .scoped(self.bif_env())
                .get(&view, *i)
                .ok_or(ExecutionError::BadEnvironmentIndex(*i)),
            Ref::G(i) => view
                .scoped(self.state.globals)
                .get(&view, *i)
                .ok_or(ExecutionError::BadGlobalIndex(*i)),
        }
    }

    /// Build a data-constructor value of `tag` over `fields` using the
    /// pre-encoded constructor template (`BytecodeProgram::templates[tag]`).
    /// The fields become an env frame; the template's `OP_CONS tag [L0..]`
    /// reads them back. Zero-field constructors close over `root_env`.
    fn build_data(
        &self,
        view: MutatorHeapView<'_>,
        tag: Tag,
        fields: &[BcValue],
    ) -> Result<BcValue, ExecutionError> {
        let code = self
            .program
            .templates
            .get(tag as usize)
            .copied()
            .ok_or_else(|| {
                ExecutionError::Panic(
                    self.state.annotation,
                    format!("bytecode: no constructor template for tag {tag}"),
                )
            })?;
        let env = if fields.is_empty() {
            self.state.root_env
        } else {
            view.from_values(
                fields.iter().cloned(),
                fields.len(),
                self.state.root_env,
                self.state.annotation,
            )?
        };
        Ok(BcValue::Closure(BcClosure::new(code, env)))
    }

    /// If `value` is a data-constructor closure, return its `(tag, env, arg
    /// offsets)`; otherwise `None`. Decodes the `OP_CONS` node the closure
    /// points at (a template or an encoded `Cons`).
    fn cons_of(&self, value: &AbiClosure) -> Option<(Tag, RefPtr<BcEnvFrame>, ArgOffsets)> {
        let AbiClosure::Byte(BcValue::Closure(c)) = value else {
            return None;
        };
        let code = self.program.code.as_slice();
        let mut pc = c.code() as usize;
        if Op::from_u8(read_u8(code, &mut pc)) != Some(Op::Cons) {
            return None;
        }
        let tag = read_u8(code, &mut pc);
        let offsets = read_arg_offsets(code, &mut pc);
        Some((tag, c.env(), offsets))
    }

    /// Evaluate `value` to WHNF on a fresh continuation stack, restoring the
    /// caller's stack/current afterwards. The bytecode analogue of
    /// `Machine::evaluate_to_whnf` (`vm.rs`). Used by `force`.
    fn run_to_whnf(&mut self, value: BcValue) -> Result<BcValue, ExecutionError> {
        // Shared suspend/stash/swap choreography (eu-apn7). `exit_whnf_subrun`
        // restores the caller's stack/current/flags BEFORE propagating a
        // sub-run error, so an intrinsic that swallows the force error resumes
        // on the intact caller stack (was: the error was propagated first,
        // leaving the caller on the truncated sub-run stack).
        let saved_terminated = enter_whnf_subrun(self.state, value);
        let run_result = self.drive_to_whnf();
        exit_whnf_subrun(
            self.state,
            run_result,
            saved_terminated,
            false,
            "bytecode: thunk evaluation unexpectedly yielded an IO constructor",
        )
    }

    /// The re-entrant dispatch loop for `run_to_whnf`: drives `step` (with a
    /// NullEmitter and the `Bif` dispatch tail) until the sub-evaluation
    /// terminates. Mirrors `evaluate_to_whnf_impl` (`vm.rs`).
    fn drive_to_whnf(&mut self) -> Result<(), ExecutionError> {
        let mut null = NullEmitter;
        let gc_check_freq: u32 = 500;
        let mut gc_countdown: u32 = gc_check_freq;
        while !self.state.terminated {
            gc_countdown -= 1;
            if gc_countdown == 0 {
                gc_countdown = gc_check_freq;
                gc_poll(self.state, self.heap, self.clock, self.dump_heap)?;
            }
            self.metrics.tick();
            self.metrics.stack(self.state.stack.len());
            {
                // SAFETY: the view borrows the heap for this scope only and is
                // dropped before any `&mut self.heap` use; this mirrors the
                // HeapSyn `bif_view` aliasing (`vm.rs`).
                let view = unsafe { MutatorHeapView::from_raw_heap(self.heap as *const Heap) };
                // Mirror the sub-evaluation step map_err (vm.rs
                // `evaluate_to_whnf_impl`): wrap the step error with traces
                // here. The nested BIF error (below) is left raw, matching the
                // HeapSyn sub-evaluation loop — it is wrapped once at the outer
                // BIF site when it unwinds.
                if let Err(e) = step(self.state, view, self.program) {
                    return Err(attach_trace(self.state, view, e));
                }
            }
            if let Some(idx) = self.state.pending_bif.take() {
                self.dispatch_pending_bif(idx, &mut null)?;
            }
            // Capture-emitter lifecycle inside the force sub-run (eu-fgl0).
            // A `render-as` forced by an intrinsic sets `pending_capture_start`;
            // consume it here (previously the sub-loop ignored it, letting it
            // leak into the outer dispatch, which then pushed a spurious late
            // capture emitter — a divergence from HeapSyn). This mirrors
            // HeapSyn's `evaluate_to_whnf_impl` (vm.rs) exactly, keeping the two
            // engines byte-identical.
            self.run_subrun_capture_lifecycle()?;
        }
        Ok(())
    }

    /// Consume any `pending_capture_start` that fired during a force sub-run,
    /// mirroring HeapSyn's `evaluate_to_whnf_impl` (vm.rs) for byte-identical
    /// behaviour: BIF-internal forcing runs against a `NullEmitter`, so a nested
    /// `render-as` is created only to validate its format and then discarded.
    ///
    /// `capture_end_pending` is deliberately left untouched — HeapSyn also
    /// leaves it, so it surfaces in the outer dispatch identically on both
    /// engines. (Fully capturing a render-as forced inside a force sub-run is a
    /// shared limitation of both engines and out of scope for this bug, which
    /// concerns the bytecode/HeapSyn divergence, not the shared limitation.)
    fn run_subrun_capture_lifecycle(&mut self) -> Result<(), ExecutionError> {
        if let Some(format) = self.state.pending_capture_start.take() {
            let mut capture = OwnedCaptureEmitter::new(&format, self.state.annotation)?;
            capture.stream_start();
            drop(capture);
        }
        Ok(())
    }

    /// Run one captured pending BIF through `self` (re-entrant): materialise
    /// its args and push a rooted [`BifFrame`] pointing at the Bif closure's
    /// frame, so the env + `Ref::V` args survive a collection triggered inside
    /// `execute` (eu-zryh).
    fn dispatch_pending_bif(
        &mut self,
        idx: u8,
        emitter: &mut dyn Emitter,
    ) -> Result<(), ExecutionError> {
        let args = materialize_bif_args(self.state);
        let bif_env = self
            .state
            .current
            .as_closure()
            .map(|c| c.env())
            .unwrap_or(self.state.root_env);
        self.state.bif_frames.push(BifFrame { env: bif_env, args });
        let bif = self.intrinsics[idx as usize];
        // SAFETY: as above — the view is confined to the execute call.
        let view = unsafe { MutatorHeapView::from_raw_heap(self.heap as *const Heap) };
        // Borrow the rooted args by raw pointer (see the outer dispatch tail
        // for the aliasing rationale); GC forwards the frame in place.
        let args_ptr: *const [Ref] = self
            .state
            .bif_frames
            .last()
            .expect("bif frame just pushed")
            .args
            .as_slice();
        // SAFETY: the slice aliases the rooted frame, only read during execute.
        let result = bif.execute(self, view, emitter, unsafe { &*args_ptr });
        if let Some(frame) = self.state.bif_frames.pop() {
            recycle_bif_args(self.state, frame);
        }
        result
    }
}

impl IntrinsicMachine for BcBifContext<'_, '_> {
    fn rcache(&mut self) -> &mut LruCache<String, Regex> {
        self.rcache
    }

    fn symbol_pool(&self) -> &SymbolPool {
        self.symbol_pool
    }

    fn symbol_pool_mut(&mut self) -> &mut SymbolPool {
        self.symbol_pool
    }

    fn annotation(&self) -> Smid {
        self.state.annotation
    }

    fn test_mode(&self) -> bool {
        self.test_mode
    }

    fn block_index_enabled(&self) -> bool {
        // Bytecode blocks are template closures with no in-place mutation; use
        // the STG find-loop fallback for lookups (same result).
        false
    }

    // ── HeapSyn-typed methods: unreachable on the bytecode path ──────
    // These name `SynClosure`/`EnvFrame`, which the bytecode engine never
    // produces. An intrinsic reaching one has not been migrated to the
    // neutral ABI and is caught by the differential harness.

    fn set_closure(&mut self, _closure: SynClosure) -> Result<(), ExecutionError> {
        panic!("bytecode BifContext: set_closure(SynClosure) — intrinsic uses the HeapSyn ABI")
    }

    fn nav<'guard>(&'guard self, _view: MutatorHeapView<'guard>) -> HeapNavigator<'guard> {
        panic!("bytecode BifContext: nav — intrinsic uses the HeapSyn ABI")
    }

    fn root_env(&self) -> RefPtr<EnvFrame> {
        panic!("bytecode BifContext: root_env — intrinsic uses the HeapSyn ABI")
    }

    fn env(&self, _view: MutatorHeapView) -> RefPtr<EnvFrame> {
        panic!("bytecode BifContext: env — intrinsic uses the HeapSyn ABI")
    }

    // ── Emitter capture lifecycle (spec §6; render-as) ──────────────

    fn start_capture(&mut self, format: &str) -> Result<(), ExecutionError> {
        // The machine loop reads this and pushes a capture emitter.
        self.state.pending_capture_start = Some(format.to_string());
        Ok(())
    }

    fn push_capture_end(&mut self, _view: MutatorHeapView<'_>) -> Result<(), ExecutionError> {
        self.state.stack.push(BcContinuation::CaptureEnd);
        Ok(())
    }

    fn take_capture_result(&mut self) -> Result<String, ExecutionError> {
        // The captured string is installed as the machine value directly by
        // the capture-end handler, so this hook is unused on the bytecode
        // path (as on the HeapSyn path).
        Err(ExecutionError::Panic(
            self.state.annotation,
            "bytecode: take_capture_result is unused (result flows via current)".to_string(),
        ))
    }

    // ── Neutral ABI over `BcValue` (spec §5.5) ──────────────────────

    fn resolve_native(
        &self,
        view: MutatorHeapView<'_>,
        arg: &Ref,
    ) -> Result<Native, ExecutionError> {
        let value = match arg {
            Ref::V(n) => return Ok(n.clone()),
            Ref::L(i) => view
                .scoped(self.bif_env())
                .get(&view, *i)
                .ok_or(ExecutionError::BadEnvironmentIndex(*i))?,
            Ref::G(i) => view
                .scoped(self.state.globals)
                .get(&view, *i)
                .ok_or(ExecutionError::BadGlobalIndex(*i))?,
        };
        self.native_from_value(view, value)
    }

    fn return_native(
        &mut self,
        _view: MutatorHeapView<'_>,
        native: Native,
    ) -> Result<(), ExecutionError> {
        self.state.current = BcValue::Native(native);
        Ok(())
    }

    fn resolve_closure(
        &self,
        view: MutatorHeapView<'_>,
        arg: &Ref,
    ) -> Result<AbiClosure, ExecutionError> {
        Ok(AbiClosure::Byte(self.resolve_value(view, arg)?))
    }

    fn resolve_callable_closure(
        &self,
        view: MutatorHeapView<'_>,
        arg: &Ref,
    ) -> Result<AbiClosure, ExecutionError> {
        match self.resolve_value(view, arg)? {
            v @ BcValue::Closure(_) => Ok(AbiClosure::Byte(v)),
            BcValue::Native(n) => Err(ExecutionError::NotCallable(
                self.state.annotation,
                n.type_description().to_string(),
            )),
        }
    }

    fn set_result(&mut self, closure: AbiClosure) -> Result<(), ExecutionError> {
        match closure {
            AbiClosure::Byte(v) => {
                self.state.current = v;
                Ok(())
            }
            AbiClosure::Heap(_) => {
                panic!("bytecode BifContext: set_result(Heap) — intrinsic uses the HeapSyn ABI")
            }
        }
    }

    fn tail_apply_global(
        &mut self,
        view: MutatorHeapView<'_>,
        global_idx: usize,
        arg_refs: &[Ref],
    ) -> Result<(), ExecutionError> {
        // Resolve the args in the current (bif) env, push an `ApplyTo` for
        // them, then enter the global closure — the machine's `return_fun`
        // applies it, mirroring `Op::App`. No runtime code synthesis.
        let global = view
            .scoped(self.state.globals)
            .get(&view, global_idx)
            .ok_or(ExecutionError::BadGlobalIndex(global_idx))?;
        if !arg_refs.is_empty() {
            let args: Vec<BcValue> = arg_refs
                .iter()
                .map(|r| self.resolve_value(view, r))
                .collect::<Result<_, _>>()?;
            self.state.stack.push(BcContinuation::ApplyTo {
                args: Array::from_slice(&view, &args),
                annotation: self.state.annotation,
            });
        }
        self.state.current = global;
        Ok(())
    }

    fn force(&mut self, closure: AbiClosure) -> Result<AbiClosure, ExecutionError> {
        match closure {
            // A native is already WHNF.
            AbiClosure::Byte(v @ BcValue::Native(_)) => Ok(AbiClosure::Byte(v)),
            AbiClosure::Byte(v) => Ok(AbiClosure::Byte(self.run_to_whnf(v)?)),
            AbiClosure::Heap(_) => {
                panic!("bytecode BifContext: force(Heap) — intrinsic uses the HeapSyn ABI")
            }
        }
    }

    // ── Data construction over templates (spec §5.5) ────────────────

    fn return_unit(&mut self, view: MutatorHeapView<'_>) -> Result<(), ExecutionError> {
        self.state.current = self.build_data(view, DataConstructor::Unit.tag(), &[])?;
        Ok(())
    }

    fn return_boxed_num(
        &mut self,
        view: MutatorHeapView<'_>,
        n: Number,
    ) -> Result<(), ExecutionError> {
        self.state.current = self.build_data(
            view,
            DataConstructor::BoxedNumber.tag(),
            &[BcValue::Native(Native::Num(n))],
        )?;
        Ok(())
    }

    fn return_bool(&mut self, view: MutatorHeapView<'_>, b: bool) -> Result<(), ExecutionError> {
        let tag = if b {
            DataConstructor::BoolTrue.tag()
        } else {
            DataConstructor::BoolFalse.tag()
        };
        self.state.current = self.build_data(view, tag, &[])?;
        Ok(())
    }

    // ── Data inspection (spec §5.5) ─────────────────────────────────

    fn data_tag(&self, _view: MutatorHeapView<'_>, closure: &AbiClosure) -> Option<Tag> {
        self.cons_of(closure).map(|(tag, _, _)| tag)
    }

    fn data_field(
        &self,
        view: MutatorHeapView<'_>,
        closure: &AbiClosure,
        idx: usize,
    ) -> Option<AbiClosure> {
        let (_, env, offsets) = self.cons_of(closure)?;
        let off = *offsets.get(idx)?;
        let dref = arg_ref(self.program.code.as_slice(), off).ok()?;
        let value = resolve_ref(view, &self.state.constants, env, self.state.globals, dref).ok()?;
        Some(AbiClosure::Byte(value))
    }

    fn field_native(
        &self,
        view: MutatorHeapView<'_>,
        closure: &AbiClosure,
        idx: usize,
    ) -> Option<Native> {
        match self.data_field(view, closure, idx)? {
            AbiClosure::Byte(v) => self.native_from_value(view, v).ok(),
            AbiClosure::Heap(_) => None,
        }
    }

    fn value_native(&self, view: MutatorHeapView<'_>, closure: &AbiClosure) -> Option<Native> {
        let AbiClosure::Byte(v) = closure else {
            return None;
        };
        match v {
            BcValue::Native(n) => Some(n.clone()),
            // A bare-native Atom → follow it; a boxed scalar → its field 0.
            BcValue::Closure(_) => self
                .native_from_value(view, v.clone())
                .ok()
                .or_else(|| self.field_native(view, closure, 0)),
        }
    }

    fn native_value(
        &self,
        _view: MutatorHeapView<'_>,
        native: Native,
    ) -> Result<AbiClosure, ExecutionError> {
        Ok(AbiClosure::Byte(BcValue::Native(native)))
    }

    fn data_value(
        &self,
        view: MutatorHeapView<'_>,
        tag: Tag,
        fields: &[AbiClosure],
    ) -> Result<AbiClosure, ExecutionError> {
        let bc_fields: Vec<BcValue> = fields
            .iter()
            .map(|c| match c {
                AbiClosure::Byte(v) => v.clone(),
                AbiClosure::Heap(_) => {
                    panic!("bytecode BifContext: data_value with a HeapSyn field")
                }
            })
            .collect();
        Ok(AbiClosure::Byte(self.build_data(view, tag, &bc_fields)?))
    }

    fn meta_value(
        &self,
        view: MutatorHeapView<'_>,
        meta: AbiClosure,
        body: AbiClosure,
    ) -> Result<AbiClosure, ExecutionError> {
        let unwrap = |c: AbiClosure| match c {
            AbiClosure::Byte(v) => v,
            AbiClosure::Heap(_) => {
                panic!("bytecode BifContext: meta_value with a HeapSyn field")
            }
        };
        let env = view.from_values(
            [unwrap(meta), unwrap(body)].into_iter(),
            2,
            self.state.root_env,
            self.state.annotation,
        )?;
        Ok(AbiClosure::Byte(BcValue::Closure(BcClosure::new(
            self.program.meta_template,
            env,
        ))))
    }

    fn return_closure_list(
        &mut self,
        view: MutatorHeapView<'_>,
        items: Vec<AbiClosure>,
    ) -> Result<(), ExecutionError> {
        // Fold ListCons over the items from a ListNil tail, building each cell
        // from the constructor template (no runtime code synthesis).
        let mut acc = self.build_data(view, DataConstructor::ListNil.tag(), &[])?;
        for item in items.into_iter().rev() {
            let AbiClosure::Byte(head) = item else {
                panic!("bytecode BifContext: return_closure_list with a HeapSyn item")
            };
            acc = self.build_data(view, DataConstructor::ListCons.tag(), &[head, acc])?;
        }
        self.state.current = acc;
        Ok(())
    }

    // ── Fixed-shape thunk construction (spec §5.5, arena analysis) ──────

    fn apply2_thunk(
        &self,
        view: MutatorHeapView<'_>,
        f: AbiClosure,
        a0: AbiClosure,
        a1: AbiClosure,
    ) -> Result<AbiClosure, ExecutionError> {
        let (AbiClosure::Byte(fv), AbiClosure::Byte(a0v), AbiClosure::Byte(a1v)) = (f, a0, a1)
        else {
            panic!("bytecode BifContext: apply2_thunk with a HeapSyn field")
        };
        // A GC-heap env frame `[f, a0, a1]` over the fixed `App(L0,[L1,L2])`
        // template — zero arena growth.
        let env = view.from_values(
            [fv, a0v, a1v].into_iter(),
            3,
            self.state.root_env,
            self.state.annotation,
        )?;
        Ok(AbiClosure::Byte(BcValue::Closure(BcClosure::new(
            self.program.apply2_template,
            env,
        ))))
    }

    fn bif_tail_thunk(
        &self,
        view: MutatorHeapView<'_>,
        _bif_index: u8,
        handle: u64,
    ) -> Result<AbiClosure, ExecutionError> {
        // The template already targets PRODUCER_NEXT; the handle is slot 0.
        // `InfoTagged::thunk` marks it updatable, so entering it black-holes
        // and pushes `Update` (memoisation), matching the HeapSyn thunk.
        let env = view.from_value(
            BcValue::Native(Native::Num(handle.into())),
            self.state.root_env,
            self.state.annotation,
        )?;
        Ok(AbiClosure::Byte(BcValue::Closure(BcClosure::close(
            &InfoTagged::thunk(self.program.producer_tail_template),
            env,
        ))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::bytecode::{encode, BcClosure, BcEnvBuilder};
    use crate::eval::memory::alloc::ScopedAllocator;
    use crate::eval::memory::heap::Heap;
    use crate::eval::stg::syntax::dsl;

    /// A machine with no intrinsics and a null emitter (self-contained
    /// programs).
    fn bare_machine(
        program: BytecodeProgram,
        root_off: CodeRef,
        global_forms: &[GlobalForm],
    ) -> BytecodeMachine<'static> {
        BytecodeMachine::new(
            program,
            root_off,
            global_forms,
            Vec::new(),
            Box::new(NullEmitter),
            0,
            false,
        )
        .unwrap()
    }

    #[test]
    fn read_ref_decodes_tags() {
        // REF_L, payload 5 ; REF_V, payload 9
        let code = [REF_L, 5, 0, 0, 0, REF_V, 9, 0, 0, 0];
        let mut pc = 0usize;
        assert_eq!(read_ref(&code, &mut pc).unwrap(), DecodedRef::Local(5));
        assert_eq!(read_ref(&code, &mut pc).unwrap(), DecodedRef::Value(9));
    }

    #[test]
    fn resolve_value_const_yields_native() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let root = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
        let constants = vec![Ref::V(Native::Num(serde_json::Number::from(42)))];

        let v = resolve_ref(view, &constants, root, root, DecodedRef::Value(0)).unwrap();
        match v {
            BcValue::Native(Native::Num(n)) => assert_eq!(n.as_i64(), Some(42)),
            _ => panic!("expected native num"),
        }
    }

    #[test]
    fn resolve_local_reads_env_slot() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let root = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();

        // A frame with one closure slot (code offset 3) over root.
        let frame = view
            .from_saturation(
                view.array(&[BcValue::Closure(BcClosure::new(3, root))]),
                root,
                Default::default(),
            )
            .unwrap();
        let v = resolve_ref(view, &[], frame, root, DecodedRef::Local(0)).unwrap();
        assert_eq!(v.as_closure().unwrap().code(), 3);
    }

    #[test]
    fn read_arg_offsets_roundtrip() {
        let (prog, root, _) = encode(&dsl::app(dsl::gref(0), vec![dsl::num(7)]), &[]);
        let mut pc = root as usize + 1; // skip Op::App
        let _flags = read_u8(&prog.code, &mut pc);
        let _callable = read_ref(&prog.code, &mut pc).unwrap();
        let offs = read_arg_offsets(&prog.code, &mut pc);
        assert_eq!(offs.len(), 1);
        assert_eq!(
            prog.code[offs[0] as usize],
            crate::eval::bytecode::Op::Atom as u8
        );
    }

    #[test]
    fn read_branch_table_roundtrip() {
        let syn = dsl::case(
            dsl::local(0),
            vec![(2, dsl::atom(dsl::num(10))), (4, dsl::atom(dsl::num(20)))],
            dsl::atom(dsl::num(30)),
        );
        let (prog, root, _) = encode(&syn, &[]);
        let mut pc = root as usize + 1; // skip Op::Case
        let _scr = read_u32(&prog.code, &mut pc);
        let (min_tag, entries) = read_branch_table(&prog.code, &mut pc);
        assert_eq!(min_tag, 2);
        assert_eq!(entries.len(), 3);
        assert!(entries[0].is_some());
        assert!(entries[1].is_none()); // gap at tag 3
        assert!(entries[2].is_some());
    }

    #[test]
    fn read_form_header_roundtrip() {
        let syn = dsl::let_(vec![dsl::thunk(dsl::atom(dsl::num(5)))], dsl::local(0));
        let (prog, root, _) = encode(&syn, &[]);
        let mut pc = root as usize + 1; // skip Op::Let
        let count = read_u32(&prog.code, &mut pc);
        assert_eq!(count, 1);
        let hdr = read_form_header(&prog.code, &mut pc);
        assert_eq!(hdr.kind, crate::eval::bytecode::FORM_THUNK);
        assert_eq!(hdr.arity, 0);
        assert!((hdr.body as usize) < prog.code.len());
    }

    use crate::eval::memory::array::Array;

    fn num(n: i64) -> Native {
        Native::Num(serde_json::Number::from(n))
    }

    #[test]
    fn return_native_branch_fallback_binds_value() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let root = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
        let mut state = BcMachineState::new(BcValue::Native(num(0)), root, root, vec![]);

        let bt: Array<Option<CodeRef>> = view.array(&[]);
        state.stack.push(BcContinuation::Branch {
            min_tag: 0,
            branch_table: bt,
            fallback: Some(7),
            environment: root,
            annotation: Default::default(),
        });
        return_native(&mut state, view, num(99)).unwrap();

        let c = state.current.as_closure().unwrap();
        assert_eq!(c.code(), 7);
        let frame = view.scoped(c.env());
        match frame.get(&view, 0).unwrap() {
            BcValue::Native(Native::Num(n)) => assert_eq!(n.as_i64(), Some(99)),
            _ => panic!("expected native slot"),
        }
    }

    #[test]
    fn return_native_empty_stack_terminates() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let root = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
        let mut state = BcMachineState::new(BcValue::Native(num(0)), root, root, vec![]);
        return_native(&mut state, view, num(1)).unwrap();
        assert!(state.terminated);
    }

    #[test]
    fn return_native_branch_no_fallback_errors() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let root = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
        let mut state = BcMachineState::new(BcValue::Native(num(0)), root, root, vec![]);

        let bt: Array<Option<CodeRef>> = view.array(&[]);
        state.stack.push(BcContinuation::Branch {
            min_tag: 0,
            branch_table: bt,
            fallback: None,
            environment: root,
            annotation: Default::default(),
        });
        let err = return_native(&mut state, view, num(1));
        assert!(matches!(err, Err(ExecutionError::NoBranchForNative(..))));
    }

    #[test]
    fn return_data_branch_match_binds_field() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let root = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();

        // Constructor env holding one field: native 5.
        let cons_env = view
            .from_saturation(
                view.array(&[BcValue::Native(num(5))]),
                root,
                Default::default(),
            )
            .unwrap();
        let mut state = BcMachineState::new(
            BcValue::Closure(BcClosure::new(0, cons_env)),
            root,
            root,
            vec![],
        );

        // Branch matching tag 7 → body offset 42.
        let bt: Array<Option<CodeRef>> = view.array(&[Some(42u32)]);
        state.stack.push(BcContinuation::Branch {
            min_tag: 7,
            branch_table: bt,
            fallback: None,
            environment: root,
            annotation: Default::default(),
        });

        return_data(
            &mut state,
            view,
            &BytecodeProgram::default(),
            7,
            &[DecodedRef::Local(0)],
        )
        .unwrap();

        let c = state.current.as_closure().unwrap();
        assert_eq!(c.code(), 42);
        let frame = view.scoped(c.env());
        match frame.get(&view, 0).unwrap() {
            BcValue::Native(Native::Num(n)) => assert_eq!(n.as_i64(), Some(5)),
            _ => panic!("expected field bound in branch env"),
        }
    }

    #[test]
    fn return_data_no_branch_no_fallback_errors() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let root = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
        let mut state = BcMachineState::new(
            BcValue::Closure(BcClosure::new(0, root)),
            root,
            root,
            vec![],
        );

        let bt: Array<Option<CodeRef>> = view.array(&[Some(1u32)]);
        state.stack.push(BcContinuation::Branch {
            min_tag: 7,
            branch_table: bt,
            fallback: None,
            environment: root,
            annotation: Default::default(),
        });
        // Return tag 6 (no branch, no fallback).
        let err = return_data(&mut state, view, &BytecodeProgram::default(), 6, &[]);
        assert!(matches!(err, Err(ExecutionError::NoBranchForDataTag(..))));
    }

    #[test]
    fn return_fun_exact_application_saturates() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let root = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();

        let fun = BcClosure::new_annotated_lambda(10, 1, root, Default::default());
        let mut state = BcMachineState::new(BcValue::Closure(fun), root, root, vec![]);
        let arg = BcValue::Closure(BcClosure::new(55, root));
        state.stack.push(BcContinuation::ApplyTo {
            args: view.array(&[arg]),
            annotation: Default::default(),
        });

        return_fun(&mut state, view, &BytecodeProgram::default()).unwrap();

        let c = state.current.as_closure().unwrap();
        assert_eq!(c.code(), 10);
        let frame = view.scoped(c.env());
        assert_eq!(
            frame.get(&view, 0).unwrap().as_closure().unwrap().code(),
            55
        );
    }

    #[test]
    fn return_fun_over_application_pushes_surplus() {
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let root = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();

        // Arity-1 function applied to 2 args → saturate with 1, push ApplyTo[1].
        let fun = BcClosure::new_annotated_lambda(10, 1, root, Default::default());
        let mut state = BcMachineState::new(BcValue::Closure(fun), root, root, vec![]);
        let a = BcValue::Closure(BcClosure::new(55, root));
        let b = BcValue::Closure(BcClosure::new(66, root));
        state.stack.push(BcContinuation::ApplyTo {
            args: view.array(&[a, b]),
            annotation: Default::default(),
        });

        return_fun(&mut state, view, &BytecodeProgram::default()).unwrap();

        assert_eq!(state.current.as_closure().unwrap().code(), 10);
        assert_eq!(state.stack.len(), 1);
        match &state.stack[0] {
            BcContinuation::ApplyTo { args, .. } => assert_eq!(args.len(), 1),
            _ => panic!("expected surplus ApplyTo"),
        }
    }

    use crate::eval::memory::symbol::SymbolPool;
    use crate::eval::stg::tags::DataConstructor;

    /// Drive a freshly-encoded program to termination and return the final
    /// value.
    fn run_to_end(syn: &std::rc::Rc<crate::eval::stg::syntax::StgSyn>) -> Native {
        let (prog, root, _) = encode(syn, &[]);
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let mut pool = SymbolPool::new();
        let constants = prog.prepare_constants(view, &mut pool);
        let root_env = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
        let mut state = BcMachineState::new(
            BcValue::Closure(BcClosure::new(root, root_env)),
            root_env,
            root_env,
            constants,
        );
        state.blackhole = prog.blackhole;
        let mut guard = 0;
        while !state.terminated && guard < 1000 {
            step(&mut state, view, &prog).unwrap();
            guard += 1;
        }
        assert!(state.terminated, "machine did not terminate");
        match state.current {
            BcValue::Native(n) => n,
            BcValue::Closure(_) => panic!("expected a native result"),
        }
    }

    #[test]
    fn run_atom_returns_native() {
        match run_to_end(&dsl::atom(dsl::num(42))) {
            Native::Num(n) => assert_eq!(n.as_i64(), Some(42)),
            _ => panic!("expected num"),
        }
    }

    #[test]
    fn run_case_over_nil_matches_branch() {
        let nil_tag = DataConstructor::ListNil.tag();
        let syn = dsl::case(
            dsl::nil(),
            vec![(nil_tag, dsl::atom(dsl::num(7)))],
            dsl::atom(dsl::num(0)),
        );
        match run_to_end(&syn) {
            Native::Num(n) => assert_eq!(n.as_i64(), Some(7)),
            _ => panic!("expected num"),
        }
    }

    #[test]
    fn run_let_value() {
        // let x = 5 in x
        let syn = dsl::let_(vec![dsl::value(dsl::atom(dsl::num(5)))], dsl::local(0));
        match run_to_end(&syn) {
            Native::Num(n) => assert_eq!(n.as_i64(), Some(5)),
            _ => panic!("expected num"),
        }
    }

    #[test]
    fn run_apply_identity() {
        // let id = \x.x in id(5)
        let syn = dsl::let_(
            vec![dsl::lambda(1, dsl::local(0))],
            dsl::app(dsl::lref(0), vec![dsl::num(5)]),
        );
        match run_to_end(&syn) {
            Native::Num(n) => assert_eq!(n.as_i64(), Some(5)),
            _ => panic!("expected num"),
        }
    }

    #[test]
    fn bif_arm_captures_intrinsic_and_args() {
        let syn = dsl::app_bif(3, vec![dsl::num(1), dsl::num(2)]);
        let (prog, root, _) = encode(&syn, &[]);
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let mut pool = SymbolPool::new();
        let constants = prog.prepare_constants(view, &mut pool);
        let root_env = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
        let mut state = BcMachineState::new(
            BcValue::Closure(BcClosure::new(root, root_env)),
            root_env,
            root_env,
            constants,
        );
        handle_op(&mut state, view, &prog).unwrap();
        assert_eq!(state.pending_bif, Some(3));
        assert_eq!(state.pending_bif_args.len(), 2);
    }

    #[test]
    fn run_direct_apply_identity() {
        // let id = \x.x in id(5) via DirectApp (exact-arity fast path)
        let syn = dsl::let_(
            vec![dsl::lambda(1, dsl::local(0))],
            dsl::direct_app(Default::default(), dsl::lref(0), vec![dsl::num(5)]),
        );
        match run_to_end(&syn) {
            Native::Num(n) => assert_eq!(n.as_i64(), Some(5)),
            _ => panic!("expected num"),
        }
    }

    #[test]
    fn run_partial_application() {
        // letrec k = \x y. x        -- a binary const
        //        p = k(5)           -- thunk: too few args -> PAP of arity 1
        // in p(6)                   -- saturate the PAP -> k(5, 6) -> 5
        let syn = dsl::letrec_(
            vec![
                dsl::lambda(2, dsl::local(0)),
                dsl::thunk(dsl::app(dsl::lref(0), vec![dsl::num(5)])),
            ],
            dsl::app(dsl::lref(1), vec![dsl::num(6)]),
        );
        match run_to_end(&syn) {
            Native::Num(n) => assert_eq!(n.as_i64(), Some(5)),
            _ => panic!("expected num"),
        }
    }

    #[test]
    fn machine_runs_atom_to_exit_code() {
        // A numeric top-level result becomes the process exit code.
        let (prog, root, gforms) = encode(&dsl::atom(dsl::num(0)), &[]);
        let mut m = bare_machine(prog, root, &gforms);
        assert_eq!(m.run(Some(10_000)).unwrap(), Some(0));
    }

    #[test]
    fn machine_returns_numeric_exit_code() {
        let (prog, root, gforms) = encode(&dsl::atom(dsl::num(5)), &[]);
        let mut m = bare_machine(prog, root, &gforms);
        assert_eq!(m.run(Some(10_000)).unwrap(), Some(5));
    }

    #[test]
    fn machine_survives_forced_gc() {
        // let x = 5 in x, forcing a collection after the Let allocates its
        // env frame: the machine's root set must keep the frame live.
        let syn = dsl::let_(vec![dsl::value(dsl::atom(dsl::num(5)))], dsl::local(0));
        let (prog, root, gforms) = encode(&syn, &[]);
        let mut m = bare_machine(prog, root, &gforms);
        m.dispatch().unwrap(); // execute the Let (allocates the env frame)
        m.gc_now(); // roots must survive
        assert_eq!(m.run(Some(10_000)).unwrap(), Some(5));
    }

    #[test]
    fn bytecode_reports_machine_heap_gc_stats() {
        // The bytecode engine must expose the same Machine/Heap/GC statistics
        // the driver reports under `-S` (eu-s2oz): ticks and allocations
        // (Machine), heap blocks (Heap), and collection counts / timings (GC).
        // `let x = 5 in x` performs one heap allocation; a forced collection
        // advances the GC counters, exactly as a real run's periodic GC would.
        let syn = dsl::let_(vec![dsl::value(dsl::atom(dsl::num(5)))], dsl::local(0));
        let (prog, root, gforms) = encode(&syn, &[]);
        let mut m = bare_machine(prog, root, &gforms);

        m.dispatch().unwrap(); // execute the Let (allocates the env frame)
        m.gc_now(); // exercise the GC counters
        assert_eq!(m.run(Some(10_000)).unwrap(), Some(5));

        // Machine counters — non-zero and sensible.
        assert!(m.metrics().ticks() > 0, "ticks should be non-zero");
        assert!(
            m.allocs() > 0,
            "allocs should be non-zero (the `let` binding)"
        );

        // Heap counters.
        let heap_stats = m.heap_stats();
        assert!(
            heap_stats.blocks_allocated > 0,
            "heap blocks should have been allocated"
        );

        // GC counters — a collection ran, so counts and timings advance.
        assert!(
            heap_stats.collections_count > 0,
            "a collection should have been recorded"
        );
        let total_gc = m.clock().duration(ThreadOccupation::CollectorMark)
            + m.clock().duration(ThreadOccupation::CollectorSweep);
        assert!(
            total_gc > std::time::Duration::ZERO,
            "GC mark/sweep time should have been recorded"
        );
    }

    #[test]
    fn drive_whnf_stops_clock_so_io_wait_is_excluded_from_mutator() {
        // eu-yhih: the io-run driver (`bytecode_io_run`) forces IO spec fields
        // via `evaluate_to_whnf_for_io` (→ `drive_whnf`), then blocks on a
        // subprocess wait (`run_spec`) OUTSIDE the machine before re-entering.
        // If `drive_whnf` left the clock running in the mutator segment (the
        // regression), that blocking wait would be committed to VM-Mutator on
        // the next `switch`, so an IO-dominated program's whole subprocess wait
        // would be mis-attributed to the mutator — contradicting the `-S` top
        // bar's "IO ~100%" and diverging from HeapSyn, whose
        // `evaluate_to_whnf_for_io` delegates to `run` (which stops its clock).
        // This test asserts the invariant the bug violated: a blocking wait
        // that happens between two io-run sub-evaluations is NOT absorbed by
        // the mutator segment.
        let syn = dsl::let_(vec![dsl::value(dsl::atom(dsl::num(5)))], dsl::local(0));
        let (prog, root, gforms) = encode(&syn, &[]);
        let mut m = bare_machine(prog, root, &gforms);

        // Force to WHNF via the io-run entry point (drives `drive_whnf`).
        let v1 = m.current();
        m.evaluate_to_whnf_for_io(v1).unwrap();
        let mutator_after_subrun = m.clock().duration(ThreadOccupation::Mutator);

        // Simulate the blocking subprocess wait the driver performs between
        // `evaluate_to_whnf_for_io` and re-entering the machine.
        let wait = std::time::Duration::from_millis(200);
        std::thread::sleep(wait);

        // Re-enter the machine (as the driver does after `run_spec`). Its first
        // act is `clock.switch(Mutator)`, which commits whatever segment was
        // current: if `drive_whnf` stopped the clock the wait is excluded; if it
        // left the mutator segment running, the wait is absorbed here.
        let v2 = m.current();
        m.evaluate_to_whnf_for_io(v2).unwrap();
        let mutator_after_wait = m.clock().duration(ThreadOccupation::Mutator);

        let absorbed = mutator_after_wait - mutator_after_subrun;
        assert!(
            absorbed < wait / 2,
            "the mutator clock absorbed {absorbed:?} of the {wait:?} blocking \
             wait — drive_whnf must stop the clock so io-run subprocess waits \
             stay out of VM-Mutator (eu-yhih)"
        );
    }

    #[test]
    fn machine_resolves_global_ref() {
        // globals = [ id = \x. x ]; root = id(7) via a G-ref.
        let globals = vec![dsl::lambda(1, dsl::local(0))];
        let root = dsl::app(dsl::gref(0), vec![dsl::num(7)]);
        let (prog, root_off, gforms) = encode(&root, &globals);
        let mut m = bare_machine(prog, root_off, &gforms);
        assert_eq!(m.run(Some(10_000)).unwrap(), Some(7));
    }

    #[test]
    fn machine_dispatches_add_intrinsic() {
        // A bare `__ADD(2, 3)` Bif node dispatched through BcBifContext -> 5.
        use crate::common::sourcemap::SourceMap;
        use crate::eval::intrinsics;
        use crate::eval::stg::{make_standard_runtime, runtime::Runtime};

        let mut source_map = SourceMap::new();
        let runtime = make_standard_runtime(&mut source_map);
        let intrinsics = runtime.intrinsics();
        let add = intrinsics::index("ADD").expect("ADD intrinsic") as u8;

        let syn = dsl::app_bif(add, vec![dsl::num(2), dsl::num(3)]);
        let (prog, root, _gforms) = encode(&syn, &[]);
        let mut m =
            BytecodeMachine::new(prog, root, &[], intrinsics, Box::new(NullEmitter), 0, false)
                .unwrap();
        assert_eq!(m.run(Some(10_000)).unwrap(), Some(5));
    }

    #[test]
    fn machine_dispatches_add_with_local_arg() {
        // let x = 2 in __ADD(x, 3) — exercises resolve_native's L-ref path
        // (env slot -> OP_ATOM indirection -> native) in BcBifContext.
        use crate::common::sourcemap::SourceMap;
        use crate::eval::intrinsics;
        use crate::eval::stg::{make_standard_runtime, runtime::Runtime};

        let mut source_map = SourceMap::new();
        let runtime = make_standard_runtime(&mut source_map);
        let intrinsics = runtime.intrinsics();
        let add = intrinsics::index("ADD").expect("ADD intrinsic") as u8;

        let syn = dsl::let_(
            vec![dsl::value(dsl::atom(dsl::num(2)))],
            dsl::app_bif(add, vec![dsl::lref(0), dsl::num(3)]),
        );
        let (prog, root, _gforms) = encode(&syn, &[]);
        let mut m =
            BytecodeMachine::new(prog, root, &[], intrinsics, Box::new(NullEmitter), 0, false)
                .unwrap();
        assert_eq!(m.run(Some(10_000)).unwrap(), Some(5));
    }

    #[test]
    fn machine_forces_thunk_via_force_whnf() {
        // let t = __ADD(1, 2) in __FORCE_WHNF(t) -> 3.
        // Exercises the re-entrant evaluate_to_whnf: FORCE_WHNF resolves the
        // let-bound thunk (not pre-evaluated), forces it on a fresh stack, and
        // sets the result — all via BcBifContext's neutral closure ABI.
        use crate::common::sourcemap::SourceMap;
        use crate::eval::intrinsics;
        use crate::eval::stg::{
            arith::Add,
            force::ForceWhnf,
            runtime::{Runtime, StandardRuntime},
        };

        // A runtime with ADD + FORCE_WHNF registered at their catalogue
        // indices (the rest remain unimplemented stubs, unused here).
        let mut rt = StandardRuntime::default();
        rt.add(Box::new(Add) as Box<dyn StgIntrinsic>);
        rt.add(Box::new(ForceWhnf));
        rt.prepare(&mut SourceMap::default());
        let intrinsics = rt.intrinsics();
        let add = intrinsics::index_u8("ADD");
        let force = intrinsics::index_u8("__FORCE_WHNF");

        let syn = dsl::let_(
            vec![dsl::thunk(dsl::app_bif(
                add,
                vec![dsl::num(1), dsl::num(2)],
            ))],
            dsl::app_bif(force, vec![dsl::lref(0)]),
        );
        let (prog, root, _gforms) = encode(&syn, &[]);
        let mut m =
            BytecodeMachine::new(prog, root, &[], intrinsics, Box::new(NullEmitter), 0, false)
                .unwrap();
        assert_eq!(m.run(Some(10_000)).unwrap(), Some(3));
    }

    #[test]
    fn direct_app_forces_global_thunk() {
        // Regression (eu-0iba): every pre-compiled prelude global is stored as
        // an arity-0 updatable thunk (see the xtask `LambdaForm::thunk`
        // wrapping), so `cons`/`map`/`if`/… are thunks that yield their lambda
        // when forced. A `DirectApp` to such a global with args must FORCE the
        // thunk before applying. The previous handler only forced *local*
        // thunks; a `DirectApp` to an unforced *global* thunk left
        // `state.current` unchanged and spun forever (the whole bytecode+blob
        // path hung on any recursive prelude function). With a tick limit this
        // now surfaces as a clean failure rather than a hang.
        use crate::common::sourcemap::{Smid, SourceMap};
        use crate::eval::intrinsics;
        use crate::eval::stg::{make_standard_runtime, runtime::Runtime};

        let mut source_map = SourceMap::new();
        let runtime = make_standard_runtime(&mut source_map);
        let intrinsics = runtime.intrinsics();
        let add = intrinsics::index_u8("ADD");

        // Global 0: a thunk that, when forced, yields a 1-arg lambda `\x → x+1`.
        let inc = dsl::lambda(1, dsl::app_bif(add, vec![dsl::lref(0), dsl::num(1)]));
        let g_thunk = dsl::thunk(dsl::let_(vec![inc], dsl::local(0)));
        let globals = vec![g_thunk];

        // Root: `DirectApp G0(5)` — invoke the unforced global thunk with an arg.
        let root = dsl::direct_app(Smid::default(), dsl::gref(0), vec![dsl::num(5)]);
        let (prog, root_off, gforms) = encode(&root, &globals);
        let mut m = BytecodeMachine::new(
            prog,
            root_off,
            &gforms,
            intrinsics,
            Box::new(NullEmitter),
            0,
            false,
        )
        .unwrap();
        assert_eq!(m.run(Some(100_000)).unwrap(), Some(6));
    }

    #[test]
    fn global_thunk_memoised() {
        // Regression (eu-fhoo): every pre-compiled prelude global is stored in
        // the blob as an arity-0 updatable thunk in the SHARED globals frame.
        // Entering such a thunk must MEMOISE it (blackhole the slot + push an
        // `Update` targeting the globals frame) so its body runs once and the
        // result is shared by every reference — exactly as the source pipeline
        // shares prelude bindings compiled as `Ref::L` letrec-locals. Without
        // memoisation each reference recomputes the global, which on real
        // programs (day08) meant ~5.6× the allocation of the source prelude and
        // tipped HeapSyn into a GC death-spiral.
        //
        // Here a single global thunk (whose body performs a measurable ADD) is
        // forced `N` times via nested `seq` scrutinees (each an `Atom{G0}` that
        // routes through `enter_global`). With memoisation the body executes
        // once (~one ADD); without it, `N` times. We assert the total tick
        // count stays far below the non-memoised cost — revert-proven: removing
        // the `Global` arm from `enter_global`/`DirectApp` makes ticks blow past
        // the threshold.
        use crate::common::sourcemap::SourceMap;
        use crate::eval::intrinsics;
        use crate::eval::stg::{arith::Add, runtime::Runtime, runtime::StandardRuntime};

        let mut rt = StandardRuntime::default();
        rt.add(Box::new(Add) as Box<dyn StgIntrinsic>);
        rt.prepare(&mut SourceMap::default());
        let intrinsics = rt.intrinsics();
        let add = intrinsics::index_u8("ADD");

        // Global 0: a deliberately EXPENSIVE thunk — a chain of `K` forced
        // additions ending in `ADD(1, 2) = 3`. Re-running its body is therefore
        // clearly measurable in ticks, so the memoised/non-memoised gap is
        // dominated by body re-evaluation rather than per-reference overhead.
        const K: usize = 150;
        let mut g_body = dsl::app_bif(add, vec![dsl::num(1), dsl::num(2)]);
        for _ in 0..K {
            g_body = dsl::seq(dsl::app_bif(add, vec![dsl::num(1), dsl::num(1)]), g_body);
        }
        let g_thunk = dsl::thunk(g_body);
        let globals = vec![g_thunk];

        // Root: force G0 `N` times through nested `seq`, then return 0. With
        // memoisation G0's body runs ONCE; without it, `N` times.
        const N: usize = 4;
        let mut body = dsl::atom(dsl::num(0));
        for _ in 0..N {
            body = dsl::seq(dsl::atom(dsl::gref(0)), body);
        }
        let (prog, root_off, gforms) = encode(&body, &globals);
        let mut m = BytecodeMachine::new(
            prog,
            root_off,
            &gforms,
            intrinsics,
            Box::new(NullEmitter),
            0,
            false,
        )
        .unwrap();
        assert_eq!(m.run(Some(100_000)).unwrap(), Some(0));

        // With memoisation G0's ~450-tick body runs once (total ≈ 465 ticks);
        // without it, all N forces re-run the body (≈ N × 450 ≈ 1800 ticks for
        // N=4). The threshold sits well inside that gap, so removing the
        // `Global` arm from `enter_global` (or the `DirectApp` global-thunk
        // memoisation) makes this test fail.
        let ticks = m.metrics.ticks();
        assert!(
            ticks < 900,
            "global thunk not memoised: {ticks} ticks for {N} references \
             (expected the body to run once, not {N} times)"
        );
    }

    #[test]
    fn exit_whnf_subrun_restores_state_before_propagating_error() {
        // eu-apn7: on a sub-run error, the caller's continuation stack, current
        // value and termination flag must be restored BEFORE the error is
        // propagated, so an intrinsic that swallows the force error (e.g. debug
        // `boxed_native` `.ok()`) resumes on the intact caller stack. The buggy
        // ordering returned the error first, leaving the truncated sub-run
        // stack in place. Revert-proof: move the `run_result?` in
        // `exit_whnf_subrun` above the restores and this test fails.
        let heap = Heap::new();
        let view = MutatorHeapView::new(&heap);
        let root = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
        let mut state = BcMachineState::new(BcValue::Native(num(1)), root, root, vec![]);

        // The caller's continuation stack: a single marker continuation.
        state.stack.push(BcContinuation::Update {
            environment: root,
            index: 7,
        });
        state.terminated = true; // the caller's saved flag

        // Enter a sub-run for value `2`; the caller stack is suspended.
        let saved_terminated = enter_whnf_subrun(&mut state, BcValue::Native(num(2)));
        assert!(state.stack.is_empty(), "enter suspends the caller stack");
        assert!(!state.terminated, "enter clears the termination flag");

        // Simulate the sub-run leaving a truncated stack + leftover current.
        state.stack.push(BcContinuation::Update {
            environment: root,
            index: 99,
        });
        state.current = BcValue::Native(num(42));

        // Exit with a sub-run error.
        let err = ExecutionError::Panic(Smid::default(), "sub-run failed".to_string());
        let result = exit_whnf_subrun(&mut state, Err(err), saved_terminated, false, "unused");

        assert!(result.is_err(), "the sub-run error must still propagate");
        // …but only AFTER the caller's state was restored:
        assert_eq!(
            state.stack.len(),
            1,
            "caller stack restored, not the sub-run's"
        );
        match state.stack[0] {
            BcContinuation::Update { index, .. } => {
                assert_eq!(
                    index, 7,
                    "restored the caller's continuation, not the sub-run's"
                )
            }
            _ => panic!("wrong continuation restored"),
        }
        match &state.current {
            BcValue::Native(Native::Num(n)) => {
                assert_eq!(n.as_i64(), Some(1), "caller current restored")
            }
            _ => panic!("caller current not restored"),
        }
        assert_eq!(
            state.terminated, saved_terminated,
            "caller termination flag restored"
        );
    }

    #[test]
    fn bif_frame_roots_survive_gc_evacuation() {
        // eu-zryh: the env + `Ref::V` heap-string args of an in-flight BIF
        // dispatch live on `bif_frames`, which must be scanned + updated as GC
        // roots (using the same `scan_const`/`update_const` helpers as the
        // already-rooted `constants`/`stash`). Here an env frame and a string
        // are reachable ONLY through a bif frame; a real mark + evacuate
        // collection must keep them live and forward their pointers, and the
        // reads below must succeed — including under EU_GC_STRESS / POISON /
        // VERIFY=2. (The strong end-to-end guard against the stale-pointer
        // failure mode is the full harness run under those flags; a single
        // object cannot be reclaimed deterministically at the unit level — the
        // sibling `collect` tests skip-if-inconclusive for the same reason.)
        use crate::common::sourcemap::Smid;
        use crate::eval::machine::metrics::ThreadOccupation;
        use crate::eval::memory::collect::{collect, collect_with_evacuation};
        use crate::eval::memory::string::HeapString;
        use std::iter::repeat_with;

        let mut heap = Heap::new();
        let mut clock = Clock::default();
        clock.switch(ThreadOccupation::Mutator);

        let (root, env, str_ptr) = {
            let view = MutatorHeapView::new(&heap);
            let root = view.alloc(BcEnvFrame::default()).unwrap().as_ptr();
            // The canary string is allocated EARLY, then buried under many
            // unrooted fillers so its block fills up and becomes a `rest`
            // (evacuation candidate) block whose surrounding lines are dead.
            let str_ptr = view
                .alloc(HeapString::from_str(&view, "furnace-canary"))
                .unwrap()
                .as_ptr();
            let _fillers: Vec<_> = repeat_with(|| {
                view.alloc(HeapString::from_str(
                    &view,
                    "filler-0123456789abcdef0123456789",
                ))
                .unwrap()
                .as_ptr()
            })
            .take(8192)
            .collect();
            // The env frame is allocated last (in the bump area) with one
            // closure slot (code offset 3 over root).
            let env = view
                .from_saturation(
                    view.array(&[BcValue::Closure(BcClosure::new(3, root))]),
                    root,
                    Smid::default(),
                )
                .unwrap();
            (root, env, str_ptr)
        };

        let mut state = BcMachineState::new(BcValue::Native(num(0)), root, root, vec![]);
        // The ONLY live references to `env` and `str_ptr`: an active bif frame.
        state.bif_frames.push(BifFrame {
            env,
            args: vec![Ref::V(Native::Str(str_ptr))],
        });

        // Mark-in-place to settle live objects, then force evacuation of every
        // block (head, overflow and all rest blocks) so the canary — reachable
        // only via the bif frame — must be marked and forwarded to survive.
        collect(&mut state, &mut heap, &mut clock, false);
        heap.flush_unswept();
        let rest = heap.rest_block_count();
        let candidates: Vec<usize> = (0..2 + rest).collect();
        collect_with_evacuation(&mut state, &mut heap, &mut clock, &candidates, false);

        // The frame's env + string pointers must be forwarded and readable.
        let frame = state.bif_frames.last().expect("bif frame present");
        let view = MutatorHeapView::new(&heap);
        let slot = view
            .scoped(frame.env)
            .get(&view, 0)
            .expect("env slot 0 must survive GC");
        assert_eq!(
            slot.as_closure().unwrap().code(),
            3,
            "bif frame env not rooted/forwarded across evacuation"
        );
        match &frame.args[0] {
            Ref::V(Native::Str(p)) => assert_eq!(
                (*view.scoped(*p)).as_str(),
                "furnace-canary",
                "bif frame string arg not rooted across GC evacuation"
            ),
            _ => panic!("arg 0 is not a string"),
        }
    }

    #[test]
    fn drive_to_whnf_consumes_pending_capture_start() {
        // eu-fgl0: a `render-as` forced inside a force sub-run sets
        // `pending_capture_start`. The bytecode `drive_to_whnf` sub-loop must
        // consume it (via `run_subrun_capture_lifecycle`) exactly as HeapSyn's
        // `evaluate_to_whnf_impl` does — BIF-internal render-as runs against a
        // NullEmitter and its capture is discarded — so nothing leaks into the
        // outer dispatch to push a spurious late capture emitter. `pending_bif`
        // handling is unchanged.
        //
        // This locks in the helper's HeapSyn-parity semantics (consume start,
        // leave end): flip either behaviour and an assert below fails. The
        // call-site wiring in `drive_to_whnf` is covered by the full
        // both-engines harness parity run.
        let (prog, root, gforms) = encode(&dsl::atom(dsl::num(0)), &[]);
        let mut m = bare_machine(prog, root, &gforms);
        m.state.pending_capture_start = Some("json".to_string());
        m.state.capture_end_pending = true;
        {
            let mut ctx = BcBifContext {
                state: &mut m.state,
                program: &m.program,
                symbol_pool: &mut m.symbol_pool,
                rcache: &mut m.rcache,
                test_mode: m.test_mode,
                heap: &mut m.heap,
                intrinsics: &m.intrinsics,
                metrics: &mut m.metrics,
                clock: &mut m.clock,
                dump_heap: m.dump_heap,
            };
            ctx.run_subrun_capture_lifecycle().unwrap();
        }
        assert!(
            m.state.pending_capture_start.is_none(),
            "the sub-loop must consume pending_capture_start (was leaking to the outer dispatch)"
        );
        // `capture_end_pending` is deliberately left for the outer dispatch,
        // mirroring HeapSyn's `evaluate_to_whnf_impl` for byte-identical parity.
        assert!(
            m.state.capture_end_pending,
            "capture_end_pending must be left untouched (HeapSyn parity)"
        );
    }
}
