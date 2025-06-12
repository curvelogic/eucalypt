//! Intrinsics for emitting data format events and debug / error
//! tracing

use crate::eval::{
    emit::{Emitter, RenderMetadata},
    error::ExecutionError,
    machine::intrinsic::{CallGlobal0, CallGlobal1, CallGlobal2, IntrinsicMachine, StgIntrinsic},
    memory::{self, mutator::MutatorHeapView, syntax::Ref},
    primitive::Primitive,
};

use super::support::machine_return_unit;

/// Interpret arg as tag if it exists otherwise None
fn tag_from_arg(
    arg: &Ref,
    machine: &mut dyn IntrinsicMachine,
    view: MutatorHeapView,
) -> Option<String> {
    let tag_nat = machine.nav(view).resolve_native(arg).ok();
    match tag_nat {
        Some(memory::syntax::Native::Sym(s)) | Some(memory::syntax::Native::Str(s)) => {
            Some(view.scoped(s).as_str().to_string())
        }
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
        let primitive = match native {
            memory::syntax::Native::Sym(s) => Primitive::Sym(view.scoped(s).as_str().to_string()),
            memory::syntax::Native::Str(s) => Primitive::Str(view.scoped(s).as_str().to_string()),
            memory::syntax::Native::Num(n) => Primitive::Num(n),
            memory::syntax::Native::Zdt(dt) => Primitive::ZonedDateTime(dt),
        };
        emitter.scalar(&RenderMetadata::empty(), &primitive);
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
        let primitive = match native {
            memory::syntax::Native::Sym(s) => Primitive::Sym(view.scoped(s).as_str().to_string()),
            memory::syntax::Native::Str(s) => Primitive::Str(view.scoped(s).as_str().to_string()),
            memory::syntax::Native::Num(n) => Primitive::Num(n),
            memory::syntax::Native::Zdt(dt) => Primitive::ZonedDateTime(dt),
        };
        emitter.scalar(&RenderMetadata::new(tag), &primitive);
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
