//! Intrinsics for emitting data format events and debug / error
//! tracing

use crate::eval::{emit::RenderMetadata, error::ExecutionError, primitive::Primitive};

use super::{
    machine::StgIntrinsic,
    runtime::{machine_return_unit, StgWrapper},
};

/// EMIT0
///
/// Emit a "null"
pub struct Emit0;

impl StgWrapper for Emit0 {
    fn name(&self) -> &str {
        "EMIT0"
    }
}

impl StgIntrinsic for Emit0 {
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

impl StgWrapper for EmitT {
    fn name(&self) -> &str {
        "EMITT"
    }
}

impl StgIntrinsic for EmitT {
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

impl StgWrapper for EmitF {
    fn name(&self) -> &str {
        "EMITF"
    }
}

impl StgIntrinsic for EmitF {
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

impl StgWrapper for EmitNative {
    fn name(&self) -> &str {
        "EMITx"
    }
}

impl StgIntrinsic for EmitNative {
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

impl StgWrapper for EmitSeqStart {
    fn name(&self) -> &str {
        "EMIT["
    }
}

impl StgIntrinsic for EmitSeqStart {
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

impl StgWrapper for EmitSeqEnd {
    fn name(&self) -> &str {
        "EMIT]"
    }
}

impl StgIntrinsic for EmitSeqEnd {
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

impl StgWrapper for EmitBlockStart {
    fn name(&self) -> &str {
        "EMIT{"
    }
}

impl StgIntrinsic for EmitBlockStart {
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

impl StgWrapper for EmitBlockEnd {
    fn name(&self) -> &str {
        "EMIT}"
    }
}

impl StgIntrinsic for EmitBlockEnd {
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

impl StgWrapper for EmitDocStart {
    fn name(&self) -> &str {
        "EMIT<"
    }
}

impl StgIntrinsic for EmitDocStart {
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

impl StgWrapper for EmitDocEnd {
    fn name(&self) -> &str {
        "EMIT>"
    }
}

impl StgIntrinsic for EmitDocEnd {
    fn execute(
        &self,
        machine: &mut super::machine::Machine,
        _args: &[super::syntax::Ref],
    ) -> Result<(), ExecutionError> {
        machine.emitter().doc_end();
        machine_return_unit(machine)
    }
}
