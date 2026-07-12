//! Materialise a pure-data compiled STG value onto the GC heap via the
//! neutral value-construction ABI (BV1 spec §5.5), engine-agnostically.
//!
//! `PARSE_STRING` and `TYPE_TO_DATA` turn parsed input / a type into a core
//! expression that is a pure DATA literal — blocks, lists, boxed scalars, and
//! (for parsed input) metadata-annotated scalars, with no runtime-varying
//! computation. The compiler lowers that to a (possibly nested) letrec of
//! value-form data cells (`Cons`/`Meta`/`Atom`) whose references only ever
//! point *backwards* within their own frame (the compiler builds each cell
//! bottom-up) plus the empty-list global.
//!
//! The HeapSyn engine can `load()` that tree and tree-walk it directly, but
//! the bytecode engine has no runtime tree-walker for `HeapSyn`. Rather than
//! synthesise per-call bytecode (which would grow the off-heap code arena
//! without bound), we walk the compiled STG here and rebuild the value
//! *directly* through the neutral `native_value`/`data_value`/`meta_value`/
//! `resolve_closure` methods. Each engine's implementation of those builds its
//! own value representation on the shared GC heap, so the result is
//! collectable and renders byte-identically on both engines with zero code
//! arena growth.
//!
//! Only the pure-data value subset is supported; any code-bearing form
//! (`App`/`Bif`/`Case`/lambda/…) or a forward/cross-frame reference returns an
//! `ExecutionError`, so a non-data input fails cleanly rather than producing a
//! wrong value.

use std::rc::Rc;

use crate::common::sourcemap::Smid;
use crate::eval::{
    error::ExecutionError,
    machine::intrinsic::{AbiClosure, IntrinsicMachine},
    memory::{
        mutator::MutatorHeapView,
        symbol::SymbolPool,
        syntax::{Native as HeapNative, Ref as HeapRef, StgBuilder},
    },
    stg::syntax::{LambdaForm, Native, Ref, StgSyn},
};

fn unsupported(smid: Smid, what: &str) -> ExecutionError {
    ExecutionError::Panic(
        smid,
        format!("materialise-data: unsupported compiled form ({what})"),
    )
}

/// Resolve a compiled-STG reference to a materialised value handle.
///
/// * `L(i)` indexes the values already built in the current letrec frame
///   (backward references only).
/// * `G(i)` is resolved against the machine's globals (the empty-list global
///   is the only global these data literals reference).
/// * `V(native)` is wrapped as a scalar value, interning symbols and
///   heap-allocating strings.
fn resolve_ref(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    pool: &mut SymbolPool,
    frame: &[AbiClosure],
    smid: Smid,
    r: &Ref,
) -> Result<AbiClosure, ExecutionError> {
    match r {
        Ref::L(i) => frame
            .get(*i)
            .cloned()
            .ok_or_else(|| unsupported(smid, "forward or out-of-range local reference")),
        Ref::G(i) => machine.resolve_closure(view, &HeapRef::G(*i)),
        Ref::V(Native::Num(n)) => machine.native_value(view, HeapNative::Num(n.clone())),
        Ref::V(Native::Str(s)) => {
            let ptr = view.str(s.as_str())?.as_ptr();
            machine.native_value(view, HeapNative::Str(ptr))
        }
        Ref::V(Native::Sym(s)) => {
            let id = pool.intern(s.as_str());
            machine.native_value(view, HeapNative::Sym(id))
        }
        Ref::V(Native::Zdt(d)) => machine.native_value(view, HeapNative::Zdt(*d)),
    }
}

/// Materialise a compiled-STG data value into a value handle on the heap.
///
/// `frame` holds the values already built for the enclosing letrec (empty at
/// the top level). Nested `Let`/`LetRec` structures are self-contained: each
/// opens a fresh frame that its own bindings and body reference.
pub fn materialise_data(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    pool: &mut SymbolPool,
    syn: &Rc<StgSyn>,
    frame: &[AbiClosure],
) -> Result<AbiClosure, ExecutionError> {
    let smid = machine.annotation();
    match &**syn {
        StgSyn::Atom { evaluand } => resolve_ref(machine, view, pool, frame, smid, evaluand),
        StgSyn::Cons { tag, args } => {
            let mut fields = Vec::with_capacity(args.len());
            for a in args {
                fields.push(resolve_ref(machine, view, pool, frame, smid, a)?);
            }
            machine.data_value(view, *tag, &fields)
        }
        StgSyn::Let { bindings, body } | StgSyn::LetRec { bindings, body } => {
            let mut new_frame: Vec<AbiClosure> = Vec::with_capacity(bindings.len());
            for b in bindings {
                let body = match b {
                    LambdaForm::Value { body } | LambdaForm::Thunk { body } => body,
                    LambdaForm::Lambda { .. } => return Err(unsupported(smid, "lambda binding")),
                };
                let value = materialise_data(machine, view, pool, body, &new_frame)?;
                new_frame.push(value);
            }
            materialise_data(machine, view, pool, body, &new_frame)
        }
        StgSyn::Ann { body, .. } => materialise_data(machine, view, pool, body, frame),
        StgSyn::Meta { meta, body } => {
            let meta = resolve_ref(machine, view, pool, frame, smid, meta)?;
            let body = resolve_ref(machine, view, pool, frame, smid, body)?;
            machine.meta_value(view, meta, body)
        }
        StgSyn::Case { .. } => Err(unsupported(smid, "case")),
        StgSyn::App { .. } | StgSyn::DirectApp { .. } => Err(unsupported(smid, "application")),
        StgSyn::Bif { .. } => Err(unsupported(smid, "intrinsic call")),
        StgSyn::DeMeta { .. } => Err(unsupported(smid, "demeta")),
        StgSyn::Seq { .. } => Err(unsupported(smid, "seq")),
        StgSyn::LookupLit { .. } => Err(unsupported(smid, "lookup")),
        StgSyn::FusedPrimop { .. } => Err(unsupported(smid, "fused-primop")),
        StgSyn::BlackHole => Err(unsupported(smid, "blackhole")),
    }
}
