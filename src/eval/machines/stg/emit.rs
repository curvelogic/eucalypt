//! Intrinsics for emitting data format events and debug / error
//! tracing

use crate::eval::{emit::RenderMetadata, error::ExecutionError, primitive::Primitive};

use super::{intrinsic::StgIntrinsic, runtime::machine_return_unit};

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
        machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), ExecutionError> {
        machine
            .emitter()
            .scalar(&RenderMetadata::empty(), &Primitive::Null);
        machine_return_unit(machine)
    }
}

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
        machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), ExecutionError> {
        machine
            .emitter()
            .scalar(&RenderMetadata::empty(), &Primitive::Bool(true));
        machine_return_unit(machine)
    }
}

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
        machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), ExecutionError> {
        machine
            .emitter()
            .scalar(&RenderMetadata::empty(), &Primitive::Bool(false));
        machine_return_unit(machine)
    }
}

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
        machine: &mut super::machine::Machine,
        args: &[super::syntax::Ref],
    ) -> Result<(), ExecutionError> {
        let native = machine.resolve_native(&args[0])?;
        let primitive = match native {
            super::syntax::Native::Sym(s) => Primitive::Sym(s),
            super::syntax::Native::Str(s) => Primitive::Str(s),
            super::syntax::Native::Num(n) => Primitive::Num(n),
            super::syntax::Native::Zdt(dt) => Primitive::ZonedDateTime(dt),
        };
        machine
            .emitter()
            .scalar(&RenderMetadata::empty(), &primitive);
        machine_return_unit(machine)
    }
}

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
        machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), ExecutionError> {
        machine.emitter().sequence_start(&RenderMetadata::empty());
        machine_return_unit(machine)
    }
}

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
        machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), ExecutionError> {
        machine.emitter().sequence_end();
        machine_return_unit(machine)
    }
}

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
        machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), ExecutionError> {
        machine.emitter().block_start(&RenderMetadata::empty());
        machine_return_unit(machine)
    }
}

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
        machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), ExecutionError> {
        machine.emitter().block_end();
        machine_return_unit(machine)
    }
}

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
        machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), ExecutionError> {
        machine.emitter().doc_start();
        machine_return_unit(machine)
    }
}

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
        machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), ExecutionError> {
        machine.emitter().doc_end();
        machine_return_unit(machine)
    }
}
