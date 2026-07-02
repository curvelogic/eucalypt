//! Bytecode dispatch machine (BV1 Phase 2). **Work in progress.**
//!
//! This module will grow the parallel `BytecodeMachine`: a `run`/`step`/
//! `handle_op` dispatch loop over the flat opcode stream, a neutral
//! `IntrinsicMachine` implementation producing `BcValue`s over the
//! constructor templates, and the re-entrant `evaluate_to_whnf` path.
//!
//! This first increment lands the foundational operand decoding and
//! reference resolution used by every dispatch arm.

use crate::common::sourcemap::Smid;
use crate::eval::error::ExecutionError;
use crate::eval::memory::mutator::MutatorHeapView;
use crate::eval::memory::syntax::{Native, Ref, RefPtr};
use crate::eval::stg::tags::Tag;

use super::program::{read_u32, read_u8};
use super::{
    BcClosure, BcContinuation, BcEnvBuilder, BcEnvFrame, BcValue, CodeRef, NO_BRANCH, REF_G, REF_L,
    REF_V,
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
    /// Set by a `CaptureEnd` continuation to signal `step` to finish an
    /// emitter capture.
    pub capture_end_pending: bool,
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
            capture_end_pending: false,
        }
    }
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
}
