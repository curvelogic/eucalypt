//! Intrinsics for emitting data format events and debug / error
//! tracing

use crate::eval::{
    emit::{Emitter, RenderMetadata},
    error::ExecutionError,
    machine::intrinsic::{CallGlobal0, CallGlobal1, IntrinsicMachine, StgIntrinsic},
    memory::{self, mutator::MutatorHeapView, syntax::Ref},
    primitive::Primitive,
};

use super::support::machine_return_unit;

/// EMIT0
///
/// Emit a "null"
pub struct Emit0;

impl StgIntrinsic for Emit0 {
    fn name(&self) -> &str {
        "EMIT0"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
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

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
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

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
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

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let native = machine.nav(view).resolve_native(&args[0])?;
        let primitive = match native {
            memory::syntax::Native::Sym(s) => Primitive::Sym(s),
            memory::syntax::Native::Str(s) => Primitive::Str(s),
            memory::syntax::Native::Num(n) => Primitive::Num(n),
            memory::syntax::Native::Zdt(dt) => Primitive::ZonedDateTime(dt),
        };
        emitter.scalar(&RenderMetadata::empty(), &primitive);
        machine_return_unit(machine, view)
    }
}

impl CallGlobal1 for EmitNative {}

/// EMIT[
///
/// Emit a sequence start
pub struct EmitSeqStart;

impl StgIntrinsic for EmitSeqStart {
    fn name(&self) -> &str {
        "EMIT["
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        emitter.sequence_start(&RenderMetadata::empty());
        machine_return_unit(machine, view)
    }
}

impl CallGlobal0 for EmitSeqStart {}

/// EMIT]
///
/// Emit a sequence end
pub struct EmitSeqEnd;

impl StgIntrinsic for EmitSeqEnd {
    fn name(&self) -> &str {
        "EMIT]"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
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

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        emitter.block_start(&RenderMetadata::empty());
        machine_return_unit(machine, view)
    }
}

impl CallGlobal0 for EmitBlockStart {}

/// EMIT}
///
/// Emit a block end
pub struct EmitBlockEnd;

impl StgIntrinsic for EmitBlockEnd {
    fn name(&self) -> &str {
        "EMIT}"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
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

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
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

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        emitter: &mut dyn Emitter,
        _args: &[Ref],
    ) -> Result<(), ExecutionError> {
        emitter.doc_end();
        machine_return_unit(machine, view)
    }
}

impl CallGlobal0 for EmitDocEnd {}
