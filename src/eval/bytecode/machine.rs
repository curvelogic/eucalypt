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

use crate::common::sourcemap::Smid;
use crate::eval::error::ExecutionError;
use crate::eval::machine::metrics::{Clock, Metrics, ThreadOccupation};
use crate::eval::machine::vm::interrupted;
use crate::eval::memory::alloc::ScopedAllocator;
use crate::eval::memory::array::Array;
use crate::eval::memory::collect::{
    collect as gc_collect, CollectorHeapView, CollectorScope, GcScannable, ScanPtr,
};
use crate::eval::memory::heap::Heap;
use crate::eval::memory::infotable::{InfoTable, InfoTagged};
use crate::eval::memory::mutator::MutatorHeapView;
use crate::eval::memory::symbol::SymbolPool;
use crate::eval::memory::syntax::{Native, Ref, RefPtr};
use crate::eval::stg::tags::{DataConstructor, Tag};

use super::program::{read_u16, read_u32, read_u8};
use super::{
    BcClosure, BcContinuation, BcEnvBuilder, BcEnvFrame, BcValue, BytecodeProgram, CodeRef, Op,
    FORM_LAMBDA, FORM_THUNK, NO_BRANCH, REF_G, REF_L, REF_V,
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
    /// Set (with `terminated`) when an IO constructor reaches the top with
    /// nothing to consume it — the io-run driver inspects `current`.
    pub yielded_io: bool,
    /// Offset of the shared `OP_BLACKHOLE` node (`BytecodeProgram::blackhole`),
    /// used to overwrite a thunk's slot while forcing it.
    pub blackhole: CodeRef,
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
            yielded_io: false,
            blackhole: 0,
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
    }
}

/// Build a branch/handler env frame by copying the constructor's field
/// values (the copy path of `vm.rs` `env_from_data_args_copy`, `:978`).
///
/// Field closures are `Copy`/`Clone` over the same heap env, so thunk
/// memoisation is preserved; only the backing array is freshly allocated
/// (the shared-backing optimisation is a later perf refinement).
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
pub fn return_data(
    state: &mut BcMachineState,
    view: MutatorHeapView<'_>,
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
                    env_from_data_args_copy(state, view, args, environment)?
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
        BcContinuation::ApplyTo { annotation, .. } => {
            // Block application delegates to the MERGE global; needs the
            // globals frame + a G-ref entry (wired with the machine loop).
            let type_name = DataConstructor::try_from(tag)
                .map(|dc| dc.to_string())
                .unwrap_or_else(|()| format!("data (tag {tag})"));
            if tag == DataConstructor::Block.tag() {
                return Err(ExecutionError::Panic(
                    annotation,
                    "bytecode: block application (MERGE) not yet implemented".to_string(),
                ));
            }
            return Err(ExecutionError::NotCallable(annotation, type_name));
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
        BcContinuation::LookupLitForce { .. } => {
            return Err(ExecutionError::Panic(
                state.annotation,
                "bytecode: LookupLitForce block lookup not yet implemented".to_string(),
            ));
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

/// Build the static part of a closure from a decoded form header.
fn bc_info(h: &FormHeader) -> InfoTagged<CodeRef> {
    match h.kind {
        FORM_THUNK => InfoTagged::thunk(h.body),
        FORM_LAMBDA => InfoTagged::new(h.arity, h.body, h.smid),
        _ => InfoTagged::value(h.body), // FORM_VALUE
    }
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
            return_data(state, view, tag, &arg_refs)?;
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
        Op::Meta | Op::DeMeta | Op::LookupLit => {
            return Err(ExecutionError::Panic(
                state.annotation,
                format!("bytecode: opcode {op:?} not yet implemented"),
            ));
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
/// This increment runs self-contained programs (no globals/prelude, no `Bif`
/// dispatch); intrinsic dispatch, the globals frame and the emitter/capture
/// lifecycle are wired in the following increments.
pub struct BytecodeMachine {
    heap: Heap,
    program: BytecodeProgram,
    state: BcMachineState,
    metrics: Metrics,
    clock: Clock,
    dump_heap: bool,
}

impl BytecodeMachine {
    /// Build a machine for an encoded program rooted at `root_off`. A
    /// `heap_limit_mib` of 0 means an unbounded managed heap.
    pub fn new(
        program: BytecodeProgram,
        root_off: CodeRef,
        heap_limit_mib: usize,
    ) -> Result<Self, ExecutionError> {
        let heap = if heap_limit_mib == 0 {
            Heap::new()
        } else {
            Heap::with_limit(heap_limit_mib)
        };
        let mut pool = SymbolPool::new();
        let (constants, root_env) = {
            let view = MutatorHeapView::new(&heap);
            let constants = program.prepare_constants(view, &mut pool);
            let root_env = view.alloc(BcEnvFrame::default())?.as_ptr();
            (constants, root_env)
        };
        // No prelude yet: globals is the empty root frame (wired next increment).
        let globals = root_env;
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

    /// One dispatch step over the shared machine state (the free `step`).
    fn dispatch(&mut self) -> Result<(), ExecutionError> {
        self.metrics.tick();
        let view = MutatorHeapView::new(&self.heap);
        step(&mut self.state, view, &self.program)?;
        // Bif dispatch is wired in the next increment; until then a captured
        // pending BIF is an explicit error rather than a silent no-op.
        if self.state.pending_bif.take().is_some() {
            return Err(ExecutionError::Panic(
                self.state.annotation,
                "bytecode: Bif dispatch not yet wired (needs globals + intrinsics)".to_string(),
            ));
        }
        Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::bytecode::{encode, BcClosure, BcEnvBuilder};
    use crate::eval::memory::alloc::ScopedAllocator;
    use crate::eval::memory::heap::Heap;
    use crate::eval::stg::syntax::dsl;

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

        return_data(&mut state, view, 7, &[DecodedRef::Local(0)]).unwrap();

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
        let err = return_data(&mut state, view, 6, &[]);
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
        let (prog, root, _) = encode(&dsl::atom(dsl::num(0)), &[]);
        let mut m = BytecodeMachine::new(prog, root, 0).unwrap();
        assert_eq!(m.run(Some(10_000)).unwrap(), Some(0));
    }

    #[test]
    fn machine_returns_numeric_exit_code() {
        let (prog, root, _) = encode(&dsl::atom(dsl::num(5)), &[]);
        let mut m = BytecodeMachine::new(prog, root, 0).unwrap();
        assert_eq!(m.run(Some(10_000)).unwrap(), Some(5));
    }

    #[test]
    fn machine_survives_forced_gc() {
        // let x = 5 in x, forcing a collection after the Let allocates its
        // env frame: the machine's root set must keep the frame live.
        let syn = dsl::let_(vec![dsl::value(dsl::atom(dsl::num(5)))], dsl::local(0));
        let (prog, root, _) = encode(&syn, &[]);
        let mut m = BytecodeMachine::new(prog, root, 0).unwrap();
        m.dispatch().unwrap(); // execute the Let (allocates the env frame)
        m.gc_now(); // roots must survive
        assert_eq!(m.run(Some(10_000)).unwrap(), Some(5));
    }
}
