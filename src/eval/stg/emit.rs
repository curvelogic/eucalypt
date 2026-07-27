//! Intrinsics for emitting data format events and debug / error
//! tracing

use crate::eval::{
    emit::{Emitter, Event, RenderMetadata},
    error::ExecutionError,
    machine::intrinsic::{CallGlobal0, CallGlobal1, CallGlobal2, IntrinsicMachine, StgIntrinsic},
    memory::{
        self,
        mutator::MutatorHeapView,
        ndarray::HeapNdArray,
        set::{HeapSet, Primitive as SetPrimitive},
        syntax::{Ref, RefPtr},
        vec::HeapVec,
    },
    primitive::Primitive,
};

use super::support::machine_return_unit;

/// Emit a scalar, first checking the active emitter can represent it.
///
/// Output formats differ in what they can carry — a YAML or TOML integer is
/// bounded by `i64`, while JSON is not — so a value that arrived intact can
/// still be unrenderable in the requested format. Asking the emitter here,
/// rather than discovering it inside the serialiser, means the failure
/// carries the source location of the value being rendered and can be
/// reported as an ordinary diagnostic (eu-1tkk.7.20).
fn emit_scalar(
    machine: &dyn IntrinsicMachine,
    emitter: &mut dyn Emitter,
    metadata: &RenderMetadata,
    primitive: &Primitive,
) -> Result<(), ExecutionError> {
    if let Some(reason) = emitter.unrepresentable(primitive) {
        return Err(ExecutionError::UnrepresentableValue(
            machine.annotation(),
            emitter.format_name().to_string(),
            Box::new(reason),
        ));
    }
    emit_event(
        machine,
        emitter,
        Event::OutputScalar(metadata.clone(), primitive.clone()),
    )
}

/// Emit an event, first checking the active emitter accepts it here.
///
/// A format may demand a particular document shape rather than merely
/// limiting individual values — html renders hiccup markup and nothing
/// else. Asking the emitter before the event is consumed turns a shape
/// mismatch into an `ExecutionError` with the source location of the value
/// being rendered (eu-1tkk.7.24).
fn emit_event(
    machine: &dyn IntrinsicMachine,
    emitter: &mut dyn Emitter,
    event: Event,
) -> Result<(), ExecutionError> {
    if let Some(reason) = emitter.unacceptable(&event) {
        return Err(ExecutionError::UnrenderableShape(
            machine.annotation(),
            emitter.format_name().to_string(),
            Box::new(reason),
        ));
    }
    emitter.emit(event);
    Ok(())
}

/// Convert a set primitive to a rendering primitive
fn set_primitive_to_render_primitive(
    prim: &SetPrimitive,
    machine: &dyn IntrinsicMachine,
) -> Primitive {
    match prim {
        SetPrimitive::Num(n) => {
            let num = serde_json::Number::from_f64(n.into_inner())
                .unwrap_or_else(|| serde_json::Number::from(0));
            Primitive::Num(num)
        }
        SetPrimitive::Str(s) => Primitive::Str(s.clone()),
        SetPrimitive::Sym(id) => Primitive::Sym(machine.symbol_pool().resolve(*id).to_string()),
    }
}

/// Emit a set as a sorted sequence of scalars
fn emit_set(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    emitter: &mut dyn Emitter,
    set_ref: RefPtr<HeapSet>,
    metadata: &RenderMetadata,
) -> Result<(), ExecutionError> {
    let set: crate::eval::memory::alloc::ScopedPtr<'_, HeapSet> = view.scoped(set_ref);
    emit_event(
        machine,
        emitter,
        Event::OutputSequenceStart(metadata.clone()),
    )?;
    for elem in set.sorted_elements() {
        let prim = set_primitive_to_render_primitive(elem, machine);
        emit_scalar(machine, emitter, &RenderMetadata::empty(), &prim)?;
    }
    emit_event(machine, emitter, Event::OutputSequenceEnd)
}

/// Emit an n-dimensional array as nested sequences.
///
/// A 1D array is emitted as a flat sequence of scalars.
/// A 2D array is emitted as a sequence of 1D row sequences.
/// In general, an N-D array is emitted as a sequence of (N-1)-D sub-arrays,
/// recursing down to scalar elements at rank 0.
fn emit_ndarray_data(
    machine: &dyn IntrinsicMachine,
    emitter: &mut dyn Emitter,
    arr: &HeapNdArray,
    metadata: &RenderMetadata,
) -> Result<(), ExecutionError> {
    let rank = arr.rank();
    if rank == 0 {
        // A scalar array: emit the single element value
        let val = arr.get(&[]).unwrap_or(0.0);
        let num = serde_json::Number::from_f64(val).unwrap_or_else(|| serde_json::Number::from(0));
        emit_scalar(machine, emitter, metadata, &Primitive::Num(num))?;
    } else if rank == 1 {
        emit_event(
            machine,
            emitter,
            Event::OutputSequenceStart(metadata.clone()),
        )?;
        let len = arr.shape()[0];
        for i in 0..len {
            let val = arr.get(&[i]).unwrap_or(0.0);
            let num =
                serde_json::Number::from_f64(val).unwrap_or_else(|| serde_json::Number::from(0));
            emit_scalar(
                machine,
                emitter,
                &RenderMetadata::empty(),
                &Primitive::Num(num),
            )?;
        }
        emit_event(machine, emitter, Event::OutputSequenceEnd)?;
    } else {
        emit_event(
            machine,
            emitter,
            Event::OutputSequenceStart(metadata.clone()),
        )?;
        let rows = arr.shape()[0];
        for i in 0..rows {
            if let Some(sub) = arr.slice_along(0, i) {
                emit_ndarray_data(machine, emitter, &sub, &RenderMetadata::empty())?;
            }
        }
        emit_event(machine, emitter, Event::OutputSequenceEnd)?;
    }
    Ok(())
}

/// Emit an NdArray from a heap pointer as nested sequences.
fn emit_ndarray(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    emitter: &mut dyn Emitter,
    arr_ref: RefPtr<HeapNdArray>,
    metadata: &RenderMetadata,
) -> Result<(), ExecutionError> {
    let arr: crate::eval::memory::alloc::ScopedPtr<'_, HeapNdArray> = view.scoped(arr_ref);
    emit_ndarray_data(machine, emitter, &arr, metadata)
}

/// Emit a vec as a sequence of scalars in element order.
fn emit_vec(
    machine: &dyn IntrinsicMachine,
    view: MutatorHeapView<'_>,
    emitter: &mut dyn Emitter,
    vec_ref: RefPtr<HeapVec>,
    metadata: &RenderMetadata,
) -> Result<(), ExecutionError> {
    let vec: crate::eval::memory::alloc::ScopedPtr<'_, HeapVec> = view.scoped(vec_ref);
    emit_event(
        machine,
        emitter,
        Event::OutputSequenceStart(metadata.clone()),
    )?;
    for elem in vec.elements() {
        let prim = set_primitive_to_render_primitive(elem, machine);
        emit_scalar(machine, emitter, &RenderMetadata::empty(), &prim)?;
    }
    emit_event(machine, emitter, Event::OutputSequenceEnd)
}

/// Interpret arg as tag if it exists otherwise None
fn tag_from_arg(
    arg: &Ref,
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
) -> Option<String> {
    let tag_nat = machine.resolve_native(view, arg).ok();
    match tag_nat {
        Some(memory::syntax::Native::Sym(id)) => {
            Some(machine.symbol_pool().resolve(id).to_string())
        }
        Some(memory::syntax::Native::Str(s)) => Some(view.scoped(s).as_str().to_string()),
        _ => None,
    }
}

/// EMIT0
///
/// Emit a "null"
pub struct Emit0;

impl StgIntrinsic for Emit0 {
    fn name(&self) -> &str {
        "EMIT0"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        emit_scalar(machine, emitter, &RenderMetadata::empty(), &Primitive::Null)?;
        machine_return_unit(machine, view)
    }
}

impl CallGlobal0 for Emit0 {}

/// EMITT
///
/// Emit a true
pub struct EmitT;

impl StgIntrinsic for EmitT {
    fn name(&self) -> &str {
        "EMITT"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        emit_scalar(
            machine,
            emitter,
            &RenderMetadata::empty(),
            &Primitive::Bool(true),
        )?;
        machine_return_unit(machine, view)
    }
}

impl CallGlobal0 for EmitT {}

/// EMITF
///
/// Emit a false
pub struct EmitF;

impl StgIntrinsic for EmitF {
    fn name(&self) -> &str {
        "EMITF"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        emit_scalar(
            machine,
            emitter,
            &RenderMetadata::empty(),
            &Primitive::Bool(false),
        )?;
        machine_return_unit(machine, view)
    }
}

impl CallGlobal0 for EmitF {}

/// EMITx
///
/// Emit a native
pub struct EmitNative;

impl StgIntrinsic for EmitNative {
    fn name(&self) -> &str {
        "EMITx"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let native = machine.resolve_native(view, &args[0])?;
        match native {
            memory::syntax::Native::Set(ptr) => {
                emit_set(machine, view, emitter, ptr, &RenderMetadata::empty())?;
            }
            memory::syntax::Native::NdArray(ptr) => {
                emit_ndarray(machine, view, emitter, ptr, &RenderMetadata::empty())?;
            }
            memory::syntax::Native::Vec(ptr) => {
                emit_vec(machine, view, emitter, ptr, &RenderMetadata::empty())?;
            }
            memory::syntax::Native::Sym(id) => {
                let primitive = Primitive::Sym(machine.symbol_pool().resolve(id).to_string());
                emit_scalar(machine, emitter, &RenderMetadata::empty(), &primitive)?;
            }
            memory::syntax::Native::Str(s) => {
                let primitive = Primitive::Str(view.scoped(s).as_str().to_string());
                emit_scalar(machine, emitter, &RenderMetadata::empty(), &primitive)?;
            }
            memory::syntax::Native::Num(n) => {
                emit_scalar(
                    machine,
                    emitter,
                    &RenderMetadata::empty(),
                    &Primitive::Num(n),
                )?;
            }
            memory::syntax::Native::Zdt(dt) => {
                emit_scalar(
                    machine,
                    emitter,
                    &RenderMetadata::empty(),
                    &Primitive::ZonedDateTime(dt),
                )?;
            }
            memory::syntax::Native::Index(_)
            | memory::syntax::Native::Prng(_)
            | memory::syntax::Native::Producer(_) => {
                return Err(ExecutionError::NotScalar(machine.annotation()));
            }
        }
        machine_return_unit(machine, view)
    }
}

impl CallGlobal1 for EmitNative {}

/// EMITTAGx(tag, native)
///
/// Emit a native
pub struct EmitTagNative;

impl StgIntrinsic for EmitTagNative {
    fn name(&self) -> &str {
        "EMITTAGx"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let tag = tag_from_arg(&args[0], machine, view);
        let native = machine.resolve_native(view, &args[1])?;
        match native {
            memory::syntax::Native::Set(ptr) => {
                emit_set(machine, view, emitter, ptr, &RenderMetadata::new(tag))?;
            }
            memory::syntax::Native::NdArray(ptr) => {
                emit_ndarray(machine, view, emitter, ptr, &RenderMetadata::new(tag))?;
            }
            memory::syntax::Native::Vec(ptr) => {
                emit_vec(machine, view, emitter, ptr, &RenderMetadata::new(tag))?;
            }
            memory::syntax::Native::Sym(id) => {
                let primitive = Primitive::Sym(machine.symbol_pool().resolve(id).to_string());
                emit_scalar(machine, emitter, &RenderMetadata::new(tag), &primitive)?;
            }
            memory::syntax::Native::Str(s) => {
                let primitive = Primitive::Str(view.scoped(s).as_str().to_string());
                emit_scalar(machine, emitter, &RenderMetadata::new(tag), &primitive)?;
            }
            memory::syntax::Native::Num(n) => {
                emit_scalar(
                    machine,
                    emitter,
                    &RenderMetadata::new(tag),
                    &Primitive::Num(n),
                )?;
            }
            memory::syntax::Native::Zdt(dt) => {
                emit_scalar(
                    machine,
                    emitter,
                    &RenderMetadata::new(tag),
                    &Primitive::ZonedDateTime(dt),
                )?;
            }
            memory::syntax::Native::Index(_)
            | memory::syntax::Native::Prng(_)
            | memory::syntax::Native::Producer(_) => {
                return Err(ExecutionError::NotScalar(machine.annotation()));
            }
        }
        machine_return_unit(machine, view)
    }
}

impl CallGlobal2 for EmitTagNative {}

/// EMIT[
///
/// Emit a sequence start
pub struct EmitSeqStart;

impl StgIntrinsic for EmitSeqStart {
    fn name(&self) -> &str {
        "EMIT["
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        emit_event(
            machine,
            emitter,
            Event::OutputSequenceStart(RenderMetadata::empty()),
        )?;
        machine_return_unit(machine, view)
    }
}

impl CallGlobal0 for EmitSeqStart {}

/// EMITTAG[
///
/// Emit a sequence start
pub struct EmitTagSeqStart;

impl StgIntrinsic for EmitTagSeqStart {
    fn name(&self) -> &str {
        "EMITTAG["
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let tag = tag_from_arg(&args[0], machine, view);
        emit_event(
            machine,
            emitter,
            Event::OutputSequenceStart(RenderMetadata::new(tag)),
        )?;
        machine_return_unit(machine, view)
    }
}

impl CallGlobal1 for EmitTagSeqStart {}

/// EMIT]
///
/// Emit a sequence end
pub struct EmitSeqEnd;

impl StgIntrinsic for EmitSeqEnd {
    fn name(&self) -> &str {
        "EMIT]"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        emit_event(machine, emitter, Event::OutputSequenceEnd)?;
        machine_return_unit(machine, view)
    }
}

impl CallGlobal0 for EmitSeqEnd {}

/// EMIT{
///
/// Emit a block start
pub struct EmitBlockStart;

impl StgIntrinsic for EmitBlockStart {
    fn name(&self) -> &str {
        "EMIT{"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        emit_event(
            machine,
            emitter,
            Event::OutputBlockStart(RenderMetadata::empty()),
        )?;
        machine_return_unit(machine, view)
    }
}

impl CallGlobal0 for EmitBlockStart {}

/// EMITTAG{
///
/// Emit a block start
pub struct EmitTagBlockStart;

impl StgIntrinsic for EmitTagBlockStart {
    fn name(&self) -> &str {
        "EMITTAG{"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let tag = tag_from_arg(&args[0], machine, view);
        emit_event(
            machine,
            emitter,
            Event::OutputBlockStart(RenderMetadata::new(tag)),
        )?;
        machine_return_unit(machine, view)
    }
}

impl CallGlobal1 for EmitTagBlockStart {}

/// EMIT}
///
/// Emit a block end
pub struct EmitBlockEnd;

impl StgIntrinsic for EmitBlockEnd {
    fn name(&self) -> &str {
        "EMIT}"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        emit_event(machine, emitter, Event::OutputBlockEnd)?;
        machine_return_unit(machine, view)
    }
}

impl CallGlobal0 for EmitBlockEnd {}

/// EMIT<
///
/// Emit a doc start
pub struct EmitDocStart;

impl StgIntrinsic for EmitDocStart {
    fn name(&self) -> &str {
        "EMIT<"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        emit_event(machine, emitter, Event::OutputDocumentStart)?;
        machine_return_unit(machine, view)
    }
}

impl CallGlobal0 for EmitDocStart {}

/// EMIT>
///
/// Emit a doc end
pub struct EmitDocEnd;

impl StgIntrinsic for EmitDocEnd {
    fn name(&self) -> &str {
        "EMIT>"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        emit_event(machine, emitter, Event::OutputDocumentEnd)?;
        machine_return_unit(machine, view)
    }
}

impl CallGlobal0 for EmitDocEnd {}
