//! RENDER_TO_STRING intrinsic — serialises a eucalypt value to a string
//!
//! Uses the emitter-capture approach: pushes a format-specific capture
//! emitter onto the machine's emitter stack, sets the closure to
//! `RENDER_DOC(value)`, and pushes a `CaptureEnd` continuation.  When
//! `RENDER_DOC` completes and the `CaptureEnd` fires, `Machine::step()`
//! pops the capture emitter and extracts the buffered output as a string.
//!
//! This avoids any heap-walk code or recursive machine re-entry.

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::{Emitter, Event},
        error::ExecutionError,
        intrinsics,
        machine::intrinsic::{CallGlobal2, IntrinsicMachine, StgIntrinsic},
        memory::{
            alloc::ScopedAllocator,
            array::Array,
            mutator::MutatorHeapView,
            syntax::{HeapSyn, Ref},
        },
    },
    export,
};

use super::{support::sym_arg, tags::DataConstructor};

// ─── OwnedCaptureEmitter ─────────────────────────────────────────────────────

/// An emitter that owns both its output buffer and the format-specific
/// emitter that writes to it.
///
/// # Safety invariant
///
/// The `emitter` field holds a lifetime-erased reference to `buffer`
/// via a raw pointer. Rust drops fields in declaration order, so
/// `emitter` is dropped before `buffer` — the borrow is released
/// before the buffer is freed.
///
/// The buffer is heap-allocated (`Box`) to provide a stable address
/// that remains valid even if the outer `OwnedCaptureEmitter` is moved
/// (e.g. when the `capture_emitters` `Vec` on the machine reallocates).
pub struct OwnedCaptureEmitter {
    // INVARIANT: emitter is dropped before buffer (field declaration order).
    // The emitter borrows from the buffer via a lifetime-erased pointer;
    // dropping emitter first ensures the borrow is released before the
    // buffer is freed.
    emitter: Option<Box<dyn Emitter + 'static>>,
    // The Box indirection is intentional: it provides a stable heap address
    // that the emitter's raw pointer can safely reference even if the outer
    // OwnedCaptureEmitter is moved (e.g. Vec reallocation).
    #[allow(clippy::box_collection)]
    buffer: Box<Vec<u8>>,
}

impl OwnedCaptureEmitter {
    /// Create a new capture emitter for the given format (e.g. "json",
    /// "yaml", "text").
    ///
    /// `annotation` is the source location from the machine at the point of
    /// the `render-as` call; it is used to produce a source-located diagnostic
    /// when the format name is not recognised.
    pub fn new(format: &str, annotation: Smid) -> Result<Self, ExecutionError> {
        #[allow(clippy::box_default)]
        let mut buffer: Box<Vec<u8>> = Box::new(Vec::new());
        // SAFETY: We take a raw pointer to the boxed buffer.  The Box
        // provides a stable heap address that does not move when the
        // OwnedCaptureEmitter is moved.  The emitter is always dropped
        // before the buffer (field declaration order), so the borrow is
        // valid for the emitter's entire lifetime.
        let buf_ptr: *mut Vec<u8> = &mut *buffer;
        let emitter = export::create_emitter(format, unsafe { &mut *buf_ptr })
            .ok_or_else(|| ExecutionError::UnknownRenderFormat(annotation, format.to_string()))?;
        // SAFETY: The lifetime erasure is sound because format-specific emitters
        // created by `create_emitter` only write through the `&mut dyn Write`
        // trait object and never capture the lifetime parameter. The `buffer`
        // field outlives `emitter` due to field declaration order.
        let emitter: Box<dyn Emitter + 'static> = unsafe { std::mem::transmute(emitter) };
        Ok(Self {
            emitter: Some(emitter),
            buffer,
        })
    }

    /// Consume the capture emitter and extract the buffered output as a
    /// UTF-8 string.
    pub fn into_string(mut self) -> Result<String, ExecutionError> {
        // Drop the emitter first to end its borrow on the buffer.
        drop(self.emitter.take());
        String::from_utf8(*self.buffer).map_err(|e| {
            ExecutionError::Panic(Smid::default(), format!("capture not valid UTF-8: {e}"))
        })
    }
}

impl Emitter for OwnedCaptureEmitter {
    fn emit(&mut self, event: Event) {
        self.emitter.as_mut().unwrap().emit(event);
    }
}

// ─── RenderToString intrinsic ─────────────────────────────────────────────────

/// RENDER_TO_STRING(value, format_sym)
///
/// Serialises `value` to a string using the specified format.
/// Recognised formats: `yaml`, `json`, `toml`, `text`, `edn`, `html`.
///
/// Both arguments are strict: the value must already be at WHNF and the
/// format symbol must be a resolved native symbol.
pub struct RenderToString;

impl StgIntrinsic for RenderToString {
    fn name(&self) -> &str {
        "RENDER_TO_STRING"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args[0] = value (strict — already at WHNF)
        // args[1] = format_sym (strict — already resolved to a native sym)
        let format_name = sym_arg(machine, view, &args[1])?;

        // Signal the machine to push a capture emitter for this format.
        machine.start_capture(&format_name)?;

        // Push CaptureEnd so that when RENDER_DOC returns, the machine
        // pops the capture emitter and produces the result string.
        machine.push_capture_end(view)?;

        // Set closure to RENDER_DOC(value).
        let render_doc_idx = intrinsics::index("RENDER_DOC").ok_or_else(|| {
            ExecutionError::Panic(machine.annotation(), "RENDER_DOC not found".to_string())
        })?;
        let app = view
            .alloc(HeapSyn::App {
                callable: Ref::G(render_doc_idx),
                args: Array::from_slice(&view, &[args[0].clone()]),
            })?
            .as_ptr();
        machine.set_closure(crate::eval::machine::env::SynClosure::new(
            app,
            machine.env(view),
        ))
    }
}

impl CallGlobal2 for RenderToString {}

// ─── Helper for io_run.rs ─────────────────────────────────────────────────────

/// Extract a string representation from a WHNF closure containing a
/// simple scalar value (string, number, symbol, boolean, null).
///
/// This is used by the io-run driver to read spec-block field values
/// without needing the full render machinery.  It handles the common
/// cases directly and returns `None` for complex values (blocks, lists).
pub fn extract_scalar_string(
    view: &MutatorHeapView<'_>,
    pool: &crate::eval::memory::symbol::SymbolPool,
    closure: &crate::eval::machine::env::SynClosure,
) -> Option<String> {
    use std::convert::TryInto;

    let code = view.scoped(closure.code());
    match &*code {
        HeapSyn::Atom { evaluand } => match evaluand {
            Ref::V(native) => scalar_from_native(view, pool, native),
            Ref::L(i) => {
                let env = view.scoped(closure.env());
                let inner = (*env).get(view, *i)?;
                extract_scalar_string(view, pool, &inner)
            }
            Ref::G(_) => None,
        },
        HeapSyn::Cons { tag, args } => {
            let dc: Result<DataConstructor, _> = (*tag).try_into();
            match dc {
                Ok(DataConstructor::Unit) => Some(String::new()),
                Ok(DataConstructor::BoolTrue) => Some("true".to_string()),
                Ok(DataConstructor::BoolFalse) => Some("false".to_string()),
                Ok(DataConstructor::BoxedNumber)
                | Ok(DataConstructor::BoxedString)
                | Ok(DataConstructor::BoxedSymbol)
                | Ok(DataConstructor::BoxedZdt) => {
                    let inner_ref = args.get(0)?;
                    let inner = match inner_ref {
                        Ref::V(native) => {
                            return scalar_from_native(view, pool, &native);
                        }
                        Ref::L(i) => {
                            let env = view.scoped(closure.env());
                            (*env).get(view, i)?
                        }
                        Ref::G(_) => return None,
                    };
                    extract_scalar_string(view, pool, &inner)
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// Convert a native value to its string representation.
fn scalar_from_native(
    view: &MutatorHeapView<'_>,
    pool: &crate::eval::memory::symbol::SymbolPool,
    native: &crate::eval::memory::syntax::Native,
) -> Option<String> {
    use crate::eval::memory::syntax::Native;
    match native {
        Native::Num(n) => Some(n.to_string()),
        Native::Str(s) => Some((*view.scoped(*s)).as_str().to_string()),
        Native::Sym(id) => Some(pool.resolve(*id).to_string()),
        Native::Zdt(dt) => Some(dt.to_string()),
        _ => None,
    }
}
