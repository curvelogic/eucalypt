// Single source of truth for the pre-compiled prelude blob's freshness hash.
//
// This file is used from three places and exists in exactly one copy:
//
//   * as a normal module of the `eucalypt` library
//     (`eucalypt::eval::stg::wire_format`), which is how `cargo xtask
//     prelude-compile` stamps a freshly generated `lib/prelude.blob`;
//   * `include!`d verbatim by the crate-root `build.rs`, which recomputes the
//     same hash to decide whether the blob already on disk is fresh; and
//   * from the enforcement tests in `tests/wire_format_enforcement_test.rs`.
//
// Having one copy is the point (eu-3skeg).  The wire-format version constant
// and the hash recipe used to be duplicated between `build.rs` and
// `xtask/src/main.rs`, kept in step only by a doc comment saying "MUST
// match" — and the recipe covered only `lib/prelude.eu`, so adding or
// removing an intrinsic silently invalidated every global slot baked into an
// existing blob without invalidating the blob.
//
// Because `build.rs` `include!`s this file, it must compile in a build-script
// context: no `//!` inner doc comments (illegal at an `include!` site), no
// references to other `crate::` items, and no dependencies beyond `sha2`,
// which is both a dependency and a build-dependency of the crate.

use sha2::{Digest, Sha256};

/// BV1 bytecode wire-format version, folded into the prelude-blob freshness
/// hash so that a change to the *serialised code-stream layout* invalidates a
/// blob that still carries the old encoding — even when neither
/// `lib/prelude.eu` nor the intrinsic table has changed.  Bump this whenever
/// the encoder's byte layout changes.
///
/// This constant deliberately does **not** need bumping when an intrinsic is
/// added, removed, renamed or reordered: the intrinsic table's contents are
/// hashed into [`blob_source_hash`] directly, so that class of staleness is
/// detected mechanically (eu-3skeg).
///
/// - v1: original BV1 stream.
/// - v2: Let/LetRec binding count widened `u16` → `u32` (eu-2sa6.11).
/// - v3: `desugared_unit_cores` field added (eu-rb5n Z).
/// - v4: `PreludeBlob::type_summary` (`PreludeSummary`) field removed — it
///   was write-only (sole writer `xtask`, zero readers; its only consumer
///   was deleted in PR #1012) (eu-2sa6.20).
/// - v5: `PreludeBlob::blame` field added (binding name → declared
///   `:transparent`/`:boundary` classification), and blob-mode global
///   reconstruction (`StandardRuntime::globals()`, xtask's bytecode
///   pre-encode loop) now stamps each prelude global's `LambdaForm::
///   Lambda.annotation` with a `Smid::global_slot(..)` identity instead of
///   always `Smid::default()` — not a serialised-shape change on its own,
///   but bundled into the same version bump as the `blame` field it feeds
///   (eu-1tkk.7.11).
/// - v6: `PreludeBlob::binding_spans` field added (per-binding declaration
///   span in `lib/prelude.eu`, so blob-mode trace frames can cite a real
///   `[prelude]:line:col`), and blob-mode reconstruction now rebases the
///   `DirectApp` / `LookupLit` Smids baked by `xtask` onto the enclosing
///   global's slot identity instead of copying them verbatim — the baked
///   values are indices into `xtask`'s own `SourceMap` and aliased unrelated
///   user source positions at runtime (eu-7x0r).
/// - v7: reserved for the SV3 structural-contract field (eu-u9xj.1). Note
///   that eu-3skeg changed the freshness *recipe* itself (domain-separated,
///   length-prefixed absorption of three labelled inputs, one of them the
///   intrinsic catalogue), so every blob predating this commit is invalidated
///   regardless of the version number — the bump is for the benefit of
///   readers, not the mechanism.
pub const BYTECODE_WIRE_FORMAT_VERSION: u32 = 7;

/// Path, relative to the workspace root, of the prelude source baked into the
/// blob.
pub const PRELUDE_SOURCE_PATH: &str = "lib/prelude.eu";

/// Path, relative to the workspace root, of the generated blob.
pub const BLOB_PATH: &str = "lib/prelude.blob";

/// Path, relative to the workspace root, of the intrinsic catalogue.
///
/// `INTRINSICS` in this file is the sole authority for the *number* and
/// *order* of intrinsic wrappers, and therefore for the global slot numbering
/// that the blob bakes in as `Ref::G(INTRINSIC_COUNT + prelude slot)`.  Adding
/// or removing an entry shifts every prelude global's slot, so the file's
/// contents are hashed into [`blob_source_hash`].
///
/// Hashing the whole file over-invalidates slightly — a comment-only edit
/// forces a blob regeneration — but that direction is the safe one, the file
/// changes rarely (14 of the 1168 commits between 2026-05-01 and 2026-07-26),
/// `lib/prelude.blob` is a git-ignored build artefact that CI regenerates
/// unconditionally, and a stale blob degrades to a build warning plus the
/// source-prelude fallback rather than to wrong answers.  Extracting just the
/// intrinsic names would need a Rust parser in `build.rs` that could itself
/// drift out of step with the file it parses.
pub const INTRINSIC_TABLE_PATH: &str = "src/eval/intrinsics.rs";

/// Path, relative to the workspace root, of this very file — which `build.rs`
/// `include!`s, and which cargo therefore does not track for it.
pub const WIRE_FORMAT_MODULE_PATH: &str = "src/eval/stg/wire_format.rs";

/// Every file `build.rs` must declare `cargo:rerun-if-changed` for.
///
/// Emitting any `rerun-if-changed` directive opts cargo out of its default
/// "re-run the build script when anything in the package changes" behaviour,
/// so each input has to be named explicitly.  Two of these are easy to
/// forget because nothing about the ordinary compile references them:
/// [`INTRINSIC_TABLE_PATH`] is read and hashed rather than imported, and
/// [`WIRE_FORMAT_MODULE_PATH`] reaches `build.rs` through `include!`.  Omit
/// either and the staleness check simply stops re-running — the failure this
/// module exists to prevent, wearing a different hat (eu-3skeg).
///
/// The list lives here, rather than as literals in `build.rs`, so that
/// `tests/wire_format_enforcement_test.rs` can assert on it.
pub const BUILD_RERUN_PATHS: &[&str] = &[
    PRELUDE_SOURCE_PATH,
    BLOB_PATH,
    INTRINSIC_TABLE_PATH,
    WIRE_FORMAT_MODULE_PATH,
    // Embedded via `include_bytes!` in `src/driver/resources.rs`; cargo does
    // not track non-Rust files referenced that way.
    "lib/test.eu",
    "lib/lens.eu",
    "lib/state.eu",
    "build-meta.yaml",
];

/// Absorb one labelled input into the hash, tag- and length-prefixed so that
/// no two distinct input tuples can produce the same byte stream.
fn absorb(hasher: &mut Sha256, tag: &[u8], bytes: &[u8]) {
    hasher.update((tag.len() as u64).to_le_bytes());
    hasher.update(tag);
    hasher.update((bytes.len() as u64).to_le_bytes());
    hasher.update(bytes);
}

/// Compute the blob freshness hash over everything that can invalidate a
/// pre-compiled prelude blob:
///
/// - the wire-format version ([`BYTECODE_WIRE_FORMAT_VERSION`]) — the
///   serialised code-stream layout, which nothing on disk reveals;
/// - `lib/prelude.eu` — the bindings themselves;
/// - `src/eval/intrinsics.rs` — the global slot numbering the blob bakes in.
///
/// `xtask prelude-compile` stamps the result into `PreludeBlob::source_hash`;
/// `build.rs` recomputes it and compares.
pub fn blob_source_hash(prelude_source: &[u8], intrinsic_table_source: &[u8]) -> [u8; 32] {
    let mut hasher = Sha256::new();
    absorb(
        &mut hasher,
        b"wire-format-version",
        &BYTECODE_WIRE_FORMAT_VERSION.to_le_bytes(),
    );
    absorb(&mut hasher, b"prelude-source", prelude_source);
    absorb(&mut hasher, b"intrinsic-table", intrinsic_table_source);
    hasher.finalize().into()
}

/// Read only the `source_hash` field from the beginning of a postcard blob.
///
/// `PreludeBlob` is serialised with `postcard`.  Its first field is
/// `source_hash: [u8; 32]`, which postcard encodes as exactly 32 raw bytes
/// (fixed-size byte arrays carry no length prefix), so the hash can be read
/// without deserialising the rest.
///
/// Returns `None` if the blob is too short to contain the hash.
pub fn read_blob_source_hash(blob: &[u8]) -> Option<[u8; 32]> {
    let prefix: &[u8; 32] = blob.get(..32)?.try_into().ok()?;
    Some(*prefix)
}

/// The verdict `build.rs` acts on: whether `lib/prelude.blob` may be embedded.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlobFreshness {
    /// Blob matches the current prelude source, intrinsic table and wire
    /// format — embed it and set `cfg(prelude_blob_ok)`.
    Fresh,
    /// No blob on disk — fall back to compiling the prelude from source.
    Missing,
    /// Blob present but too short to carry a `source_hash`.
    Unparseable,
    /// Blob present and readable, but built against different inputs.
    Stale,
}

impl BlobFreshness {
    /// The `cargo:warning=` text to emit, if any.  `Fresh` warns about
    /// nothing.
    pub fn warning(self) -> Option<&'static str> {
        match self {
            BlobFreshness::Fresh => None,
            BlobFreshness::Missing => Some(
                "precompiled prelude not found — compiling from source each run. \
                 Run `cargo xtask prelude-compile` to generate.",
            ),
            BlobFreshness::Unparseable => Some(
                "prelude blob could not be parsed — \
                 run `cargo xtask prelude-compile` to regenerate.",
            ),
            BlobFreshness::Stale => Some(
                "prelude blob is stale (prelude source, intrinsic table or bytecode \
                 wire format has changed) — run `cargo xtask prelude-compile` to regenerate.",
            ),
        }
    }
}

/// Decide whether `blob` (the raw bytes of `lib/prelude.blob`, or `None` when
/// the file is absent or unreadable) is usable against `expected_hash`.
pub fn classify_blob(blob: Option<&[u8]>, expected_hash: &[u8; 32]) -> BlobFreshness {
    match blob {
        None => BlobFreshness::Missing,
        Some(bytes) => match read_blob_source_hash(bytes) {
            None => BlobFreshness::Unparseable,
            Some(found) if found == *expected_hash => BlobFreshness::Fresh,
            Some(_) => BlobFreshness::Stale,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const PRELUDE: &[u8] = b"map(f, xs): ...";
    const TABLE: &[u8] = b"INTRINSICS: [LOOKUP, RENDER]";

    /// The guard this whole module exists for: a change to the intrinsic
    /// catalogue must change the blob freshness hash, with no human step.
    ///
    /// Fault injection: drop the `intrinsic-table` term from
    /// `blob_source_hash` and this fails.
    #[test]
    fn intrinsic_table_changes_change_the_hash() {
        let before = blob_source_hash(PRELUDE, TABLE);
        let after = blob_source_hash(PRELUDE, b"INTRINSICS: [LOOKUP, RENDER, CONTRACT_FAIL]");
        assert_ne!(
            before, after,
            "adding an intrinsic must invalidate the prelude blob"
        );
    }

    #[test]
    fn prelude_source_changes_change_the_hash() {
        let before = blob_source_hash(PRELUDE, TABLE);
        let after = blob_source_hash(b"map(f, xs): different", TABLE);
        assert_ne!(before, after);
    }

    #[test]
    fn the_hash_is_deterministic() {
        assert_eq!(
            blob_source_hash(PRELUDE, TABLE),
            blob_source_hash(PRELUDE, TABLE)
        );
    }

    /// Tag- and length-prefixing must stop a byte moving across the boundary
    /// between two inputs from going unnoticed.
    #[test]
    fn inputs_are_unambiguously_separated() {
        assert_ne!(blob_source_hash(b"ab", b"c"), blob_source_hash(b"a", b"bc"));
    }

    /// End-to-end shape of the decision `build.rs` makes: a blob generated
    /// before an intrinsic was added is classified `Stale`, not `Fresh`.
    ///
    /// Fault injection: drop the `intrinsic-table` term from
    /// `blob_source_hash` and this fails with `Fresh`.
    #[test]
    fn a_blob_predating_a_new_intrinsic_is_stale() {
        // A blob stamped before the new intrinsic landed.
        let mut blob = blob_source_hash(PRELUDE, TABLE).to_vec();
        blob.extend_from_slice(b"...rest of the postcard payload...");

        let expected_after = blob_source_hash(PRELUDE, b"INTRINSICS: [LOOKUP, RENDER, NEW]");
        assert_eq!(
            classify_blob(Some(&blob), &expected_after),
            BlobFreshness::Stale
        );

        // Sanity: against the table it was built from, the same blob is fresh.
        let expected_before = blob_source_hash(PRELUDE, TABLE);
        assert_eq!(
            classify_blob(Some(&blob), &expected_before),
            BlobFreshness::Fresh
        );
    }

    #[test]
    fn a_missing_or_truncated_blob_is_never_fresh() {
        let expected = blob_source_hash(PRELUDE, TABLE);
        assert_eq!(classify_blob(None, &expected), BlobFreshness::Missing);
        assert_eq!(
            classify_blob(Some(&[0u8; 31]), &expected),
            BlobFreshness::Unparseable
        );
    }

    #[test]
    fn only_a_fresh_blob_is_silent() {
        assert!(BlobFreshness::Fresh.warning().is_none());
        for status in [
            BlobFreshness::Missing,
            BlobFreshness::Unparseable,
            BlobFreshness::Stale,
        ] {
            assert!(status
                .warning()
                .is_some_and(|w| w.contains("cargo xtask prelude-compile")));
        }
    }
}
