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
use crate::eval::memory::heap::Heap;
use crate::eval::memory::infotable::{InfoTable, InfoTagged};
use crate::eval::memory::mutator::MutatorHeapView;
use crate::eval::memory::string::HeapString;
use crate::eval::memory::symbol::{SymbolId, SymbolPool};
use crate::eval::memory::syntax::{Native, Ref, RefPtr};
use crate::eval::stg::render_to_string::OwnedCaptureEmitter;
use crate::eval::stg::tags::{DataConstructor, Tag};

use super::program::{read_u16, read_u32, read_u8};
use super::{
    BcClosure, BcContinuation, BcEnvBuilder, BcEnvFrame, BcValue, BytecodeProgram, CodeRef,
    GlobalForm, Op, FORM_LAMBDA, FORM_THUNK, NO_BRANCH, REF_G, REF_L, REF_V,
};

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

/// Resolve a native value directly (arg extractors), erroring if the ref
/// does not resolve to a `Native`. Locals/globals that hold a native slot
/// yield it; a `Value` const yields its native.
pub fn resolve_native(
    view: MutatorHeapView<'_>,
    constants: &[Ref],
    env: RefPtr<BcEnvFrame>,
    globals: RefPtr<BcEnvFrame>,
    dref: DecodedRef,
) -> Result<Native, ExecutionError> {
    match resolve_ref(view, constants, env, globals, dref)? {
        BcValue::Native(n) => Ok(n),
        BcValue::Closure(_) => Err(ExecutionError::NotValue(
            Default::default(),
            "expected a native value".to_string(),
        )),
    }
}

/// Read an arg-offset list (`u8` count then that many `u32` offsets), as
/// emitted for App/DirectApp/Cons/Bif operands.
pub fn read_arg_offsets(code: &[u8], pc: &mut usize) -> Vec<CodeRef> {
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
    pub pending_bif_args: Vec<DecodedRef>,
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
            pending_bif_args: Vec::new(),
            capture_end_pending: false,
            pending_capture_start: None,
            yielded_io: false,
            blackhole: 0,
            stash: Vec::new(),
            suspended_stacks: Vec::new(),
        }
    }
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
fn decode_cons(prog: &BytecodeProgram, c: &BcClosure) -> Option<(Tag, Vec<CodeRef>)> {
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
            state.annotation = annotation;
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
    }
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
            state.annotation = annotation;
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
            state.annotation = annotation;
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

/// Read a `Let`/`LetRec` binding-header list (`u16` count then that many
/// form headers) followed by the body offset.
fn read_let(code: &[u8], pc: &mut usize) -> (Vec<FormHeader>, CodeRef) {
    let count = read_u16(code, pc) as usize;
    let headers = (0..count).map(|_| read_form_header(code, pc)).collect();
    let body = read_u32(code, pc);
    (headers, body)
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
        DecodedRef::Global(i) => {
            state.current = view
                .scoped(state.globals)
                .get(&view, i as usize)
                .ok_or(ExecutionError::BadGlobalIndex(i as usize))?;
            Ok(())
        }
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
                    state.current = view
                        .scoped(state.globals)
                        .get(&view, i as usize)
                        .ok_or(ExecutionError::BadGlobalIndex(i as usize))?;
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
            let arg_refs: Vec<DecodedRef> = arg_offs
                .iter()
                .map(|off| arg_ref(code, *off))
                .collect::<Result<_, _>>()?;
            return_data(state, view, prog, tag, &arg_refs)?;
        }
        Op::Case => {
            let scr_off = read_u32(code, &mut pc);
            let (min_tag, entries) = read_branch_table(code, &mut pc);
            let has_fb = read_u8(code, &mut pc);
            let fallback = if has_fb == 1 {
                Some(read_u32(code, &mut pc))
            } else {
                None
            };
            let branch_table: Array<Option<CodeRef>> = view.array(&entries);
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
            let (headers, body_off) = read_let(code, &mut pc);
            // Non-recursive: bindings close over the enclosing env.
            let mut array = Array::with_capacity(&view, headers.len());
            for h in &headers {
                array.push(&view, BcValue::Closure(BcClosure::close(&bc_info(h), env)));
            }
            let new_env = view
                .alloc(BcEnvFrame::new(array, state.annotation, Some(env)))?
                .as_ptr();
            state.current = BcValue::Closure(BcClosure::new(body_off, new_env));
        }
        Op::LetRec => {
            let (headers, body_off) = read_let(code, &mut pc);
            // Recursive: bindings close over the new frame itself.
            let mut array = Array::with_capacity(&view, headers.len());
            for _ in &headers {
                array.push(
                    &view,
                    BcValue::Closure(BcClosure::new(0, RefPtr::dangling())),
                );
            }
            let frame = view
                .alloc(BcEnvFrame::new(array.clone(), state.annotation, Some(env)))?
                .as_ptr();
            for (i, h) in headers.iter().enumerate() {
                // SAFETY: array was pre-sized to headers.len() and i < len.
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

            // Thunk callee (only reachable for a local): blackhole + Update.
            if c.update() {
                if let DecodedRef::Local(i) = callable {
                    let hole = BcValue::Closure(BcClosure::new(state.blackhole, callee_env));
                    view.scoped(callee_env).update(&view, i as usize, hole)?;
                    state.stack.push(BcContinuation::ApplyTo {
                        args,
                        annotation: state.annotation,
                    });
                    state.stack.push(BcContinuation::Update {
                        environment: callee_env,
                        index: i as usize,
                    });
                    state.current = callee;
                }
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
            let arg_refs: Vec<DecodedRef> = arg_offs
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
                if interrupted() {
                    return Err(ExecutionError::Interrupted);
                }
                if self.heap.policy_requires_collection() {
                    gc_collect(
                        &mut self.state,
                        &mut self.heap,
                        &mut self.clock,
                        self.dump_heap,
                    );
                    self.clock.switch(ThreadOccupation::Mutator);
                }
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

    /// One dispatch step over the shared machine state (the free `step`),
    /// followed by the deferred `Bif` dispatch tail (spec §6.3): when the
    /// `Bif` arm captured an intrinsic, run it through a [`BcBifContext`].
    fn dispatch(&mut self) -> Result<(), ExecutionError> {
        self.metrics.tick();
        {
            let view = MutatorHeapView::new(&self.heap);
            step(&mut self.state, view, &self.program)?;
        }

        if let Some(idx) = self.state.pending_bif.take() {
            // Materialise the captured arg refs. There is no live code node to
            // re-read (unlike HeapSyn), so we translate the decoded refs back
            // into `Ref`s the intrinsic ABI understands.
            let args: Vec<Ref> = std::mem::take(&mut self.state.pending_bif_args)
                .iter()
                .map(|d| match *d {
                    DecodedRef::Local(i) => Ref::L(i as usize),
                    DecodedRef::Global(i) => Ref::G(i as usize),
                    DecodedRef::Value(k) => self.state.constants[k as usize].clone(),
                })
                .collect();
            // The Bif closure is still `current`; its env holds the (strict,
            // pre-evaluated) args that `Ref::L` operands index into.
            let env = self
                .state
                .current
                .as_closure()
                .map(|c| c.env())
                .unwrap_or(self.state.root_env);
            let bif = self.intrinsics[idx as usize];
            // SAFETY: the view borrows the heap only for the `execute` call and
            // is not retained; `ctx` also holds `&mut heap` for a possible
            // nested `evaluate_to_whnf`. This mirrors the HeapSyn `bif_view`
            // aliasing in `vm.rs`.
            let view = unsafe { MutatorHeapView::from_raw_heap(&self.heap as *const Heap) };
            let mut ctx = BcBifContext {
                state: &mut self.state,
                program: &self.program,
                symbol_pool: &mut self.symbol_pool,
                rcache: &mut self.rcache,
                test_mode: self.test_mode,
                env,
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
            bif.execute(&mut ctx, view, emitter, &args)?;
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
    /// Environment of the Bif closure (indexed by `Ref::L` arg operands).
    env: RefPtr<BcEnvFrame>,
    /// The machine heap (for the re-entrant `evaluate_to_whnf` sub-run).
    heap: &'ctx mut Heap,
    /// Intrinsics, indexed by intrinsic index (for nested `Bif` dispatch).
    intrinsics: &'ctx [&'a dyn StgIntrinsic],
    metrics: &'ctx mut Metrics,
    clock: &'ctx mut Clock,
    dump_heap: bool,
}

impl BcBifContext<'_, '_> {
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
                if Op::from_u8(read_u8(code, &mut pc)) == Some(Op::Atom) {
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
                    Err(ExecutionError::NotValue(
                        self.state.annotation,
                        "expected a native value".to_string(),
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
                .scoped(self.env)
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
    fn cons_of(&self, value: &AbiClosure) -> Option<(Tag, RefPtr<BcEnvFrame>, Vec<CodeRef>)> {
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
        // Suspend the caller's stack and stash its current value (GC roots).
        let saved_stack = std::mem::take(&mut self.state.stack);
        self.state.suspended_stacks.push(saved_stack);
        let saved_current = std::mem::replace(&mut self.state.current, value);
        self.state.stash.push(saved_current);
        let saved_terminated = self.state.terminated;
        self.state.terminated = false;
        self.state.yielded_io = false;

        let run_result = self.drive_to_whnf();

        let saved_current = self
            .state
            .stash
            .pop()
            .expect("bytecode whnf stash underflow");
        let saved_stack = self
            .state
            .suspended_stacks
            .pop()
            .expect("bytecode whnf suspended-stack underflow");

        run_result?;

        let sub_yielded = self.state.yielded_io;
        let result = std::mem::replace(&mut self.state.current, saved_current);
        self.state.terminated = saved_terminated;
        self.state.yielded_io = false;
        self.state.stack = saved_stack;

        if sub_yielded {
            return Err(ExecutionError::Panic(
                self.state.annotation,
                "bytecode: thunk evaluation unexpectedly yielded an IO constructor".to_string(),
            ));
        }
        Ok(result)
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
                if interrupted() {
                    return Err(ExecutionError::Interrupted);
                }
                if self.heap.policy_requires_collection() {
                    gc_collect(self.state, self.heap, self.clock, self.dump_heap);
                }
            }
            self.metrics.tick();
            {
                // SAFETY: the view borrows the heap for this scope only and is
                // dropped before any `&mut self.heap` use; this mirrors the
                // HeapSyn `bif_view` aliasing (`vm.rs`).
                let view = unsafe { MutatorHeapView::from_raw_heap(self.heap as *const Heap) };
                step(self.state, view, self.program)?;
            }
            if let Some(idx) = self.state.pending_bif.take() {
                self.dispatch_pending_bif(idx, &mut null)?;
            }
        }
        Ok(())
    }

    /// Run one captured pending BIF through `self` (re-entrant): materialise
    /// its args, point `env` at the Bif closure's frame, and execute.
    fn dispatch_pending_bif(
        &mut self,
        idx: u8,
        emitter: &mut dyn Emitter,
    ) -> Result<(), ExecutionError> {
        let args: Vec<Ref> = std::mem::take(&mut self.state.pending_bif_args)
            .iter()
            .map(|d| match *d {
                DecodedRef::Local(i) => Ref::L(i as usize),
                DecodedRef::Global(i) => Ref::G(i as usize),
                DecodedRef::Value(k) => self.state.constants[k as usize].clone(),
            })
            .collect();
        let bif_env = self
            .state
            .current
            .as_closure()
            .map(|c| c.env())
            .unwrap_or(self.state.root_env);
        let bif = self.intrinsics[idx as usize];
        // SAFETY: as above — the view is confined to the execute call.
        let view = unsafe { MutatorHeapView::from_raw_heap(self.heap as *const Heap) };
        let saved_env = std::mem::replace(&mut self.env, bif_env);
        let result = bif.execute(self, view, emitter, &args);
        self.env = saved_env;
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
                .scoped(self.env)
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
        let count = u16::from_le_bytes([prog.code[pc], prog.code[pc + 1]]);
        pc += 2;
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
}
