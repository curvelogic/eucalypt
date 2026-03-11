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
        let bytes = STANDARD.decode(&input).map_err(|e| {
            use base64::DecodeError;
            let detail = match e {
                DecodeError::InvalidByte(offset, byte) => {
                    let ch = char::from(byte);
                    if ch.is_ascii_graphic() {
                        format!(
                            "invalid character '{ch}' at position {offset}; \
                             base64 (standard) uses only A-Z, a-z, 0-9, +, /, and = for padding"
                        )
                    } else {
                        format!(
                            "invalid byte 0x{byte:02X} at position {offset}; \
                             base64 (standard) uses only A-Z, a-z, 0-9, +, /, and = for padding"
                        )
                    }
                }
                DecodeError::InvalidLength(len) => format!(
                    "invalid base64 length {len}; \
                     base64-encoded strings must have a length that is a multiple of 4"
                ),
                DecodeError::InvalidLastSymbol(offset, byte) => {
                    let ch = char::from(byte);
                    format!(
                        "invalid final character '{ch}' at position {offset}; \
                         the last group of a padded base64 string must end with '='"
                    )
                }
                DecodeError::InvalidPadding => "invalid padding in base64 string; \
                     standard base64 requires '=' padding to make the length a multiple of 4"
                    .to_string(),
            };
            ExecutionError::Panic(format!("str.base64-decode: {detail}"))
        })?;
        let decoded = String::from_utf8(bytes).map_err(|e| {
            ExecutionError::Panic(format!(
                "str.base64-decode: decoded bytes are not valid UTF-8: {e}\n\
                 note: str.base64-decode only works for base64-encoded UTF-8 text; \
                 binary data cannot be decoded to a string"
            ))
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
