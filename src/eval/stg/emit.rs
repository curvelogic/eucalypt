//! Intrinsics for emitting data format events and debug / error
//! tracing

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::{Emitter, RenderMetadata},
        error::ExecutionError,
        machine::intrinsic::{
            CallGlobal0, CallGlobal1, CallGlobal2, IntrinsicMachine, StgIntrinsic,
        },
        memory::{
            self,
            mutator::MutatorHeapView,
            set::{HeapSet, Primitive as SetPrimitive},
            syntax::{Ref, RefPtr},
        },
        primitive::Primitive,
    },
};

use super::support::machine_return_unit;

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
) {
    let set: crate::eval::memory::alloc::ScopedPtr<'_, HeapSet> = view.scoped(set_ref);
    emitter.sequence_start(metadata);
    for elem in set.sorted_elements() {
        let prim = set_primitive_to_render_primitive(elem, machine);
        emitter.scalar(&RenderMetadata::empty(), &prim);
    }
    emitter.sequence_end();
}

/// Interpret arg as tag if it exists otherwise None
fn tag_from_arg(
    arg: &Ref,
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
) -> Option<String> {
    let tag_nat = machine.nav(view).resolve_native(arg).ok();
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
        emitter.scalar(&RenderMetadata::empty(), &Primitive::Null);
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
        emitter.scalar(&RenderMetadata::empty(), &Primitive::Bool(true));
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
        emitter.scalar(&RenderMetadata::empty(), &Primitive::Bool(false));
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
        let native = machine.nav(view).resolve_native(&args[0])?;
        match native {
            memory::syntax::Native::Set(ptr) => {
                emit_set(machine, view, emitter, ptr, &RenderMetadata::empty());
            }
            _ => {
                let primitive = match native {
                    memory::syntax::Native::Sym(id) => {
                        Primitive::Sym(machine.symbol_pool().resolve(id).to_string())
                    }
                    memory::syntax::Native::Str(s) => {
                        Primitive::Str(view.scoped(s).as_str().to_string())
                    }
                    memory::syntax::Native::Num(n) => Primitive::Num(n),
                    memory::syntax::Native::Zdt(dt) => Primitive::ZonedDateTime(dt),
                    memory::syntax::Native::Index(_) | memory::syntax::Native::Set(_) => {
                        return Err(ExecutionError::NotScalar(Smid::default()))
                    }
                };
                emitter.scalar(&RenderMetadata::empty(), &primitive);
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
        let native = machine.nav(view).resolve_native(&args[1])?;
        match native {
            memory::syntax::Native::Set(ptr) => {
                emit_set(machine, view, emitter, ptr, &RenderMetadata::new(tag));
            }
            _ => {
                let primitive = match native {
                    memory::syntax::Native::Sym(id) => {
                        Primitive::Sym(machine.symbol_pool().resolve(id).to_string())
                    }
                    memory::syntax::Native::Str(s) => {
                        Primitive::Str(view.scoped(s).as_str().to_string())
                    }
                    memory::syntax::Native::Num(n) => Primitive::Num(n),
                    memory::syntax::Native::Zdt(dt) => Primitive::ZonedDateTime(dt),
                    memory::syntax::Native::Index(_) | memory::syntax::Native::Set(_) => {
                        return Err(ExecutionError::NotScalar(Smid::default()))
                    }
                };
                emitter.scalar(&RenderMetadata::new(tag), &primitive);
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
        emitter.sequence_start(&RenderMetadata::empty());
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
        emitter.sequence_start(&RenderMetadata::new(tag));
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
        emitter.sequence_end();
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
        emitter.block_start(&RenderMetadata::empty());
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
        emitter.block_start(&RenderMetadata::new(tag));
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
        emitter.block_end();
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
        emitter.doc_start();
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
        emitter.doc_end();
        machine_return_unit(machine, view)
    }
}

impl CallGlobal0 for EmitDocEnd {}
