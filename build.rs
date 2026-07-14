//! Build script for eucalypt.
//!
//! Responsibilities:
//! 1. Declare `rerun-if-changed` for embedded resource files so that cargo
//!    re-embeds them when they change.
//! 2. Verify the pre-compiled prelude blob (`lib/prelude.blob`) against the
//!    current `lib/prelude.eu` source:
//!    - If the blob is missing, emit a build warning and set
//!      `cfg(prelude_blob_stale)` so the driver falls back to source-prelude.
//!    - If the blob's `source_hash` field does not match SHA-256(`lib/prelude.eu`),
//!      emit a build warning and set `cfg(prelude_blob_stale)`.
//!    - If the blob is present and the hash matches, set `cfg(prelude_blob_ok)`.

use sha2::{Digest, Sha256};
use std::path::Path;

/// BV1 bytecode wire-format version, folded into the prelude-blob source hash
/// so that a change to the serialised code-stream layout invalidates a blob
/// that still carries the old encoding — even though `lib/prelude.eu` is
/// unchanged. Bump this whenever the encoder's byte layout changes.
///
/// - v1: original BV1 stream.
/// - v2: Let/LetRec binding count widened `u16` → `u32` (eu-2sa6.11).
///
/// MUST match `BYTECODE_WIRE_FORMAT_VERSION` in `xtask/src/main.rs`.
const BYTECODE_WIRE_FORMAT_VERSION: u32 = 3;

/// Compute the blob source hash: `SHA-256(prelude source ‖ wire-format version)`.
fn blob_source_hash(source_bytes: &[u8]) -> [u8; 32] {
    let mut hasher = Sha256::new();
    hasher.update(source_bytes);
    hasher.update(BYTECODE_WIRE_FORMAT_VERSION.to_le_bytes());
    hasher.finalize().into()
}

fn main() {
    // ── Rerun triggers ────────────────────────────────────────────────────────
    // Ensure cargo recompiles when baked-in resource files change.
    // These are embedded via include_bytes! in src/driver/resources.rs
    // but cargo's incremental compilation does not automatically track
    // non-Rust files referenced by include_bytes!.
    println!("cargo:rerun-if-changed=lib/prelude.eu");
    println!("cargo:rerun-if-changed=lib/prelude.blob");
    println!("cargo:rerun-if-changed=lib/test.eu");
    println!("cargo:rerun-if-changed=lib/lens.eu");
    println!("cargo:rerun-if-changed=lib/state.eu");
    println!("cargo:rerun-if-changed=build-meta.yaml");

    // ── Declare custom cfg keys ───────────────────────────────────────────────
    // Suppress the `unexpected_cfgs` lint for the two cfg flags we emit.
    println!("cargo::rustc-check-cfg=cfg(prelude_blob_ok)");
    println!("cargo::rustc-check-cfg=cfg(prelude_blob_stale)");

    // ── Prelude blob verification ─────────────────────────────────────────────
    verify_prelude_blob();
}

/// Check that `lib/prelude.blob` exists and its embedded source hash matches
/// `SHA-256(lib/prelude.eu ‖ BYTECODE_WIRE_FORMAT_VERSION)` — so a bytecode
/// wire-format change invalidates a stale-format blob as well as a source
/// change.  Emits `cfg(prelude_blob_ok)` or `cfg(prelude_blob_stale)`
/// accordingly.
fn verify_prelude_blob() {
    let prelude_src = Path::new("lib/prelude.eu");
    let blob_path = Path::new("lib/prelude.blob");

    // Compute SHA-256 of the current prelude source.
    let source_bytes = match std::fs::read(prelude_src) {
        Ok(b) => b,
        Err(_) => {
            // No prelude source — nothing to check.
            println!("cargo:rustc-cfg=prelude_blob_stale");
            return;
        }
    };
    let source_hash: [u8; 32] = blob_source_hash(&source_bytes);

    // Check the blob.
    match std::fs::read(blob_path) {
        Err(_) => {
            println!(
                "cargo:warning=precompiled prelude not found — compiling from source each run. \
                 Run `cargo xtask prelude-compile` to generate."
            );
            println!("cargo:rustc-cfg=prelude_blob_stale");
        }
        Ok(blob_bytes) => {
            // The first 32 bytes of the postcard blob are the `source_hash` field
            // (postcard serialises `[u8; N]` as N raw bytes with no length prefix).
            // We use the `postcard` crate's own deserialiser to avoid parsing
            // the full blob just for the hash.
            match read_blob_source_hash(&blob_bytes) {
                Some(blob_hash) if blob_hash == source_hash => {
                    println!("cargo:rustc-cfg=prelude_blob_ok");
                }
                Some(_) => {
                    println!(
                        "cargo:warning=prelude blob is stale — run `cargo xtask prelude-compile` \
                         to regenerate."
                    );
                    println!("cargo:rustc-cfg=prelude_blob_stale");
                }
                None => {
                    println!(
                        "cargo:warning=prelude blob could not be parsed — \
                         run `cargo xtask prelude-compile` to regenerate."
                    );
                    println!("cargo:rustc-cfg=prelude_blob_stale");
                }
            }
        }
    }
}

/// Read only the `source_hash` field from the beginning of a postcard blob.
///
/// `PreludeBlob` is serialised with `postcard`.  The first field is
/// `source_hash: [u8; 32]`, which postcard encodes as exactly 32 raw bytes
/// (postcard encodes fixed-size byte arrays without a length prefix).
///
/// Returns `None` if the blob is too short to contain the hash.
fn read_blob_source_hash(blob: &[u8]) -> Option<[u8; 32]> {
    if blob.len() < 32 {
        return None;
    }
    let mut hash = [0u8; 32];
    hash.copy_from_slice(&blob[..32]);
    Some(hash)
}
