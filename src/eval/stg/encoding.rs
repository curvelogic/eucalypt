//! Encoding and hashing intrinsics (base64, SHA-256, etc.)

use base64::{engine::general_purpose::STANDARD, Engine};
use sha2::{Digest, Sha256};

use crate::eval::{
    emit::Emitter,
    error::ExecutionError,
    machine::intrinsic::{CallGlobal1, IntrinsicMachine, StgIntrinsic},
    memory::{mutator::MutatorHeapView, syntax::Ref},
};

use super::support::{machine_return_str, str_arg};

/// BASE64_ENCODE(s) — encode a string as base64 (standard alphabet, with padding)
pub struct Base64Encode;

impl StgIntrinsic for Base64Encode {
    fn name(&self) -> &str {
        "BASE64_ENCODE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let input = str_arg(machine, view, &args[0])?;
        let encoded = STANDARD.encode(input.as_bytes());
        machine_return_str(machine, view, encoded)
    }
}

impl CallGlobal1 for Base64Encode {}

/// BASE64_DECODE(s) — decode a base64 string back to its original UTF-8 string
pub struct Base64Decode;

impl StgIntrinsic for Base64Decode {
    fn name(&self) -> &str {
        "BASE64_DECODE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let input = str_arg(machine, view, &args[0])?;
        let bytes = STANDARD
            .decode(&input)
            .map_err(|e| ExecutionError::Panic(format!("invalid base64 input: {e}")))?;
        let decoded = String::from_utf8(bytes).map_err(|e| {
            ExecutionError::Panic(format!("decoded base64 is not valid UTF-8: {e}"))
        })?;
        machine_return_str(machine, view, decoded)
    }
}

impl CallGlobal1 for Base64Decode {}

/// SHA256(s) — return the SHA-256 hash of a string as lowercase hex
pub struct Sha256Hash;

impl StgIntrinsic for Sha256Hash {
    fn name(&self) -> &str {
        "SHA256"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let input = str_arg(machine, view, &args[0])?;
        let hash = Sha256::digest(input.as_bytes());
        let hex = hash.iter().map(|b| format!("{b:02x}")).collect::<String>();
        machine_return_str(machine, view, hex)
    }
}

impl CallGlobal1 for Sha256Hash {}
