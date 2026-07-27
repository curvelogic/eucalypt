//! Build script for eucalypt.
//!
//! Responsibilities:
//! 1. Declare `rerun-if-changed` for embedded resource files so that cargo
//!    re-embeds them when they change.
//! 2. Verify the pre-compiled prelude blob (`lib/prelude.blob`) against the
//!    inputs it was generated from — `lib/prelude.eu`, the intrinsic
//!    catalogue `src/eval/intrinsics.rs`, and the BV1 wire-format version:
//!    - If the blob is missing, unparseable, or was generated against
//!      different inputs, emit a build warning and set
//!      `cfg(prelude_blob_stale)` so the driver falls back to source-prelude.
//!    - Otherwise set `cfg(prelude_blob_ok)`.
//!
//! The version constant, the hash recipe and the staleness verdict all live
//! in `src/eval/stg/wire_format.rs`, `include!`d below, so that this script
//! and `cargo xtask prelude-compile` cannot drift apart (eu-3skeg).

/// The prelude-blob freshness machinery, shared verbatim with the library
/// module `eucalypt::eval::stg::wire_format` (a build script cannot depend on
/// its own crate, so the source is included rather than imported).
mod wire_format {
    include!("src/eval/stg/wire_format.rs");
}

use std::path::Path;
use wire_format::{
    blob_source_hash, classify_blob, BlobFreshness, BLOB_PATH, BUILD_RERUN_PATHS,
    INTRINSIC_TABLE_PATH, PRELUDE_SOURCE_PATH,
};

fn main() {
    // ── Rerun triggers ────────────────────────────────────────────────────────
    // The authoritative list lives beside the hash recipe it protects; see
    // `BUILD_RERUN_PATHS`.
    for path in BUILD_RERUN_PATHS {
        println!("cargo:rerun-if-changed={path}");
    }

    // ── Declare custom cfg keys ───────────────────────────────────────────────
    // Suppress the `unexpected_cfgs` lint for the two cfg flags we emit.
    println!("cargo::rustc-check-cfg=cfg(prelude_blob_ok)");
    println!("cargo::rustc-check-cfg=cfg(prelude_blob_stale)");

    // ── Prelude blob verification ─────────────────────────────────────────────
    verify_prelude_blob();
}

/// Check that `lib/prelude.blob` exists and its embedded source hash matches
/// the hash of the inputs it must have been generated from.  Emits
/// `cfg(prelude_blob_ok)` or `cfg(prelude_blob_stale)` accordingly.
fn verify_prelude_blob() {
    // A missing or unreadable input is treated as "cannot verify" — fall back
    // to the source prelude rather than trusting the blob.
    let (Ok(prelude_source), Ok(intrinsic_table)) = (
        std::fs::read(Path::new(PRELUDE_SOURCE_PATH)),
        std::fs::read(Path::new(INTRINSIC_TABLE_PATH)),
    ) else {
        println!("cargo:rustc-cfg=prelude_blob_stale");
        return;
    };

    let expected = blob_source_hash(&prelude_source, &intrinsic_table);
    let blob = std::fs::read(Path::new(BLOB_PATH)).ok();

    let freshness = classify_blob(blob.as_deref(), &expected);
    if let Some(warning) = freshness.warning() {
        println!("cargo:warning={warning}");
    }
    match freshness {
        BlobFreshness::Fresh => println!("cargo:rustc-cfg=prelude_blob_ok"),
        _ => println!("cargo:rustc-cfg=prelude_blob_stale"),
    }
}
