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
/// - v7: no serialised-shape change. eu-3skeg changed the freshness *recipe*
///   itself (domain-separated, length-prefixed absorption of three labelled
///   inputs, one of them the intrinsic catalogue), so every blob predating
///   that commit is invalidated regardless of the version number — the bump
///   is for the benefit of readers, not the mechanism. (This entry originally
///   read "reserved for the SV3 structural-contract field"; SV3 (eu-u9xj.1)
///   in fact adds an *intrinsic*, `CONTRACT_FAIL`, and no blob field, so no
///   shape reservation was ever needed.)
/// - v8: `PreludeBlob::deprecations` field added (deprecation path →
///   `DeprecationSpec`, so a `deprecated` declaration in the prelude is not
///   inert on the blob-backed evaluate path) (eu-1tkk.2). The field is
///   inserted mid-struct, and postcard is positional and non-self-describing:
///   `#[serde(default)]` does **not** make that backward compatible, so a v7
///   blob decodes as a shifted field sequence and fails. `src/driver/eval.rs`
///   catches the decode error and falls back to the source prelude with a
///   warning, so the failure mode is a slow start rather than a wrong answer —
///   but the bump is what makes it a *detected* staleness rather than a
///   decode accident.
/// - v9: `BytecodeProgram::cell_template` added (the dict value-cell
///   template, eu-oaz72.21). Postcard is positional, so a v8 image would
///   decode as a shifted field sequence; the bump makes that staleness
///   detected rather than a decode accident.
/// - v10: `PreludeBlob::demand_signatures` field added (per-slot demand
///   signatures derived by the demand analysis at blob build, replacing the
///   hand-written prelude table; eu-oaz72.77). The freshness recipe also
///   absorbs the demand analysis sources ([`DEMAND_ANALYSIS_PATHS`]), so a
///   blob whose signatures came from an older analysis is stale.
///
/// - v11: the high bit of a `Let`/`LetRec` binding's form-header `kind` byte
///   is a *closed* flag (`FORM_CLOSED`; eu-oaz72.175): the binding is closed
///   over a slotless annotation spine instead of the enclosing frame. The byte stream
///   keeps its shape, but a v10 reader would mask the flag into an unknown
///   kind (treated as a value form) and mis-run every flagged binding, and
///   a v11 reader of a v10 image would simply never close anything — so the
///   bump is what makes either mismatch a *detected* staleness. The
///   bytecode encoder, the closedness analysis, the opcode flag definitions
///   and the constructor arities (`stg/tags.rs`) are now freshness inputs
///   ([`DEMAND_ANALYSIS_PATHS`], and since eu-ox7qz [`STG_SOURCE_ROOTS`] for
///   `tags.rs`), since they decide or encode the flag.
pub const BYTECODE_WIRE_FORMAT_VERSION: u32 = 11;

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

/// Paths, relative to the workspace root, of the demand analysis that derives
/// the blob's per-slot demand signatures (`PreludeBlob::demand_signatures`,
/// eu-oaz72.77), and of the bytecode encoder and closedness analysis (with
/// the opcode table they read) that decide the encoded form flags
/// (eu-oaz72.175). The STG compiler, which acts on the signatures, and the
/// constructor-arity table are covered by [`STG_SOURCE_ROOTS`].
///
/// A signature is a claim about which arguments a prelude function forces,
/// and the STG compiler acts on it by evaluating those arguments early. A
/// blob carrying signatures from an older analysis would keep them after
/// the analysis changed, so these files are hashed into
/// [`blob_source_hash`]. As with [`INTRINSIC_TABLE_PATH`], hashing whole
/// files over-invalidates (a comment edit forces a regeneration), which is
/// the safe direction.
pub const DEMAND_ANALYSIS_PATHS: &[&str] = &[
    "src/core/analyse_demand.rs",
    "src/core/demand.rs",
    "src/core/dependency.rs",
    "src/eval/bytecode/encode.rs",
    "src/eval/bytecode/closed.rs",
    "src/eval/bytecode/opcode.rs",
    // The generator that runs the analysis over each prelude body
    // (eu-oaz72.175): a change to what it compiles changes the blob's code.
    "xtask/src/main.rs",
    "src/core/reflatten.rs",
];

/// Roots, relative to the workspace root, of the STG sources the blob's code
/// is built from: every `.rs` file under a directory root, or the file
/// itself, is hashed into [`blob_source_hash`] (eu-ox7qz).
///
/// The blob bakes in the encoded intrinsic wrapper globals and, inlined into
/// prelude bodies, the wrapper bodies themselves, so a hand-written wrapper
/// (`StgIntrinsic::wrapper`, e.g. in `src/eval/stg/force.rs`) is as much a
/// blob input as the prelude source. Those wrappers live across the
/// intrinsic modules of `src/eval/stg/`, built with its `dsl` and passed
/// through its compiler (`compiler.rs`), optimiser and constructor tags
/// (`tags.rs`); `src/eval/machine/intrinsic.rs` holds the default wrapper.
/// Hashing the whole directory rather than a list of wrapper files means a
/// new intrinsic module, or a helper a wrapper starts to use, is covered
/// without anyone remembering to list it. It over-invalidates — an edit
/// to an intrinsic's Rust implementation alone forces a regeneration — which
/// is the safe direction, as for [`INTRINSIC_TABLE_PATH`].
pub const STG_SOURCE_ROOTS: &[&str] = &["src/eval/stg", "src/eval/machine/intrinsic.rs"];

/// Read every `.rs` file under [`STG_SOURCE_ROOTS`], relative to
/// `workspace_root`, as `(path, contents)` pairs sorted by path, the path
/// relative to the workspace root with `/` separators.
///
/// Only regular files are read, and entries whose names start with a dot
/// (editor lock and backup files, such as a dangling `.#force.rs`) are
/// skipped; no symlink is followed, a root included, so a symlinked
/// directory is not entered. Sorting makes the hash independent of
/// directory iteration order, and the path goes into the hash with the
/// contents, so moving code between files also invalidates the blob. A
/// missing root, or one that is not a real directory or regular file, is an
/// error: the caller cannot verify the blob without it.
///
/// `build.rs` declares `rerun-if-changed` on the same roots, and cargo's scan
/// of a directory does not skip what this walk skips: creating an editor
/// lock file there re-runs the build script, which then finds the blob
/// unchanged. That errs in the safe direction.
pub fn read_stg_sources(
    workspace_root: &std::path::Path,
) -> std::io::Result<Vec<(String, Vec<u8>)>> {
    fn push(
        root: &std::path::Path,
        path: &std::path::Path,
        out: &mut Vec<(String, Vec<u8>)>,
    ) -> std::io::Result<()> {
        let relative = path.strip_prefix(root).map_err(|_| {
            std::io::Error::other(format!(
                "STG source {} is outside the workspace {}",
                path.display(),
                root.display()
            ))
        })?;
        let relative = relative
            .components()
            .map(|c| c.as_os_str().to_string_lossy())
            .collect::<Vec<_>>()
            .join("/");
        out.push((relative, std::fs::read(path)?));
        Ok(())
    }

    // `DirEntry::file_type` does not follow symlinks, so a symlinked
    // directory is never entered (no cycles) and a dangling link is never
    // read.
    fn walk(
        root: &std::path::Path,
        dir: &std::path::Path,
        out: &mut Vec<(String, Vec<u8>)>,
    ) -> std::io::Result<()> {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            if entry.file_name().to_string_lossy().starts_with('.') {
                continue;
            }
            let path = entry.path();
            let file_type = entry.file_type()?;
            if file_type.is_dir() {
                walk(root, &path, out)?;
            } else if file_type.is_file() && path.extension().is_some_and(|e| e == "rs") {
                push(root, &path, out)?;
            }
        }
        Ok(())
    }

    let mut out = Vec::new();
    for root in STG_SOURCE_ROOTS {
        let path = workspace_root.join(root);
        let file_type = std::fs::symlink_metadata(&path).map(|m| m.file_type());
        if file_type.as_ref().is_ok_and(|t| t.is_dir()) {
            walk(workspace_root, &path, &mut out)?;
        } else if file_type.as_ref().is_ok_and(|t| t.is_file()) {
            push(workspace_root, &path, &mut out)?;
        } else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!(
                    "STG source root {} is missing, or is not a real directory or regular file",
                    path.display()
                ),
            ));
        }
    }
    out.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(out)
}

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
    "src/core/analyse_demand.rs",
    "src/core/demand.rs",
    "src/core/dependency.rs",
    "src/eval/bytecode/encode.rs",
    "src/eval/bytecode/closed.rs",
    "src/eval/bytecode/opcode.rs",
    "xtask/src/main.rs",
    "src/core/reflatten.rs",
    // A directory: cargo re-runs the build script when any file under it
    // changes. See `STG_SOURCE_ROOTS`.
    "src/eval/stg",
    "src/eval/machine/intrinsic.rs",
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
/// - `src/eval/intrinsics.rs` — the global slot numbering the blob bakes in;
/// - the demand analysis and bytecode encoder sources
///   ([`DEMAND_ANALYSIS_PATHS`], in that order) — what derived the blob's
///   signatures and encoded its code;
/// - the STG sources ([`STG_SOURCE_ROOTS`], as read by [`read_stg_sources`]),
///   each with its path — the compiler and the intrinsic wrappers baked in.
///
/// `xtask prelude-compile` stamps the result into `PreludeBlob::source_hash`;
/// `build.rs` recomputes it and compares.
pub fn blob_source_hash(
    prelude_source: &[u8],
    intrinsic_table_source: &[u8],
    demand_analysis_sources: &[Vec<u8>],
    stg_sources: &[(String, Vec<u8>)],
) -> [u8; 32] {
    let mut hasher = Sha256::new();
    absorb(
        &mut hasher,
        b"wire-format-version",
        &BYTECODE_WIRE_FORMAT_VERSION.to_le_bytes(),
    );
    absorb(&mut hasher, b"prelude-source", prelude_source);
    absorb(&mut hasher, b"intrinsic-table", intrinsic_table_source);
    for source in demand_analysis_sources {
        absorb(&mut hasher, b"demand-analysis", source);
    }
    for (path, source) in stg_sources {
        absorb(&mut hasher, b"stg-source-path", path.as_bytes());
        absorb(&mut hasher, b"stg-source", source);
    }
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
                "prelude blob is stale (prelude source, intrinsic table, demand analysis, STG \
                 sources or bytecode wire format has changed) — run `cargo xtask prelude-compile` to regenerate.",
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
        let before = blob_source_hash(PRELUDE, TABLE, &[], &[]);
        let after = blob_source_hash(
            PRELUDE,
            b"INTRINSICS: [LOOKUP, RENDER, CONTRACT_FAIL]",
            &[],
            &[],
        );
        assert_ne!(
            before, after,
            "adding an intrinsic must invalidate the prelude blob"
        );
    }

    #[test]
    fn prelude_source_changes_change_the_hash() {
        let before = blob_source_hash(PRELUDE, TABLE, &[], &[]);
        let after = blob_source_hash(b"map(f, xs): different", TABLE, &[], &[]);
        assert_ne!(before, after);
    }

    #[test]
    fn the_hash_is_deterministic() {
        assert_eq!(
            blob_source_hash(PRELUDE, TABLE, &[], &[]),
            blob_source_hash(PRELUDE, TABLE, &[], &[])
        );
    }

    /// A change to the demand analysis must change the hash, so a blob whose
    /// signatures an older analysis derived is stale (eu-oaz72.77).
    ///
    /// Fault injection: drop the `demand-analysis` term from
    /// `blob_source_hash` and this fails.
    #[test]
    fn demand_analysis_changes_change_the_hash() {
        let before = blob_source_hash(PRELUDE, TABLE, &[b"analyse v1".to_vec()], &[]);
        let after = blob_source_hash(PRELUDE, TABLE, &[b"analyse v2".to_vec()], &[]);
        assert_ne!(before, after);
    }

    /// A change to an STG source — a hand-written intrinsic wrapper, say —
    /// must change the hash, as must the same code under another path
    /// (eu-ox7qz).
    ///
    /// Fault injection: drop either `stg-source` term from
    /// `blob_source_hash` and this fails.
    #[test]
    fn stg_source_changes_change_the_hash() {
        let source = |path: &str, text: &[u8]| vec![(path.to_string(), text.to_vec())];
        let base = blob_source_hash(PRELUDE, TABLE, &[], &source("force.rs", b"lambda(2, ..)"));
        assert_ne!(
            base,
            blob_source_hash(
                PRELUDE,
                TABLE,
                &[],
                &source("force.rs", b"lambda(2, local(1))")
            ),
            "editing an intrinsic wrapper must invalidate the prelude blob"
        );
        assert_ne!(
            base,
            blob_source_hash(PRELUDE, TABLE, &[], &source("dict.rs", b"lambda(2, ..)")),
            "moving code to another STG source must invalidate the prelude blob"
        );
    }

    /// The walk reads regular `.rs` files only: a dangling editor lock
    /// symlink (`.#force.rs`), a dot-file, a non-Rust file and a symlinked
    /// directory are all passed over, and the result is sorted.
    ///
    /// Fault injection: follow symlinks or drop the dot-file skip in
    /// `read_stg_sources` and this fails.
    #[test]
    #[cfg(unix)]
    fn stg_sources_skip_links_dot_files_and_non_rust_files() {
        use std::os::unix::fs::symlink;

        let root = std::env::temp_dir().join(format!("eu-stg-sources-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        let stg = root.join("src/eval/stg");
        std::fs::create_dir_all(stg.join("sub")).unwrap();
        std::fs::create_dir_all(root.join("src/eval/machine")).unwrap();
        std::fs::write(root.join("src/eval/machine/intrinsic.rs"), b"m").unwrap();
        std::fs::write(stg.join("force.rs"), b"f").unwrap();
        std::fs::write(stg.join("sub/dict.rs"), b"d").unwrap();
        std::fs::write(stg.join("notes.txt"), b"n").unwrap();
        std::fs::write(stg.join(".hidden.rs"), b"h").unwrap();
        symlink("user@host.1234", stg.join(".#force.rs")).unwrap();
        symlink(stg.join("sub"), stg.join("linked")).unwrap();
        symlink(stg.join("force.rs"), stg.join("alias.rs")).unwrap();

        let result = read_stg_sources(&root);
        let _ = std::fs::remove_dir_all(&root);
        let paths: Vec<String> = result
            .expect("STG sources read despite links and dot-files")
            .into_iter()
            .map(|(p, _)| p)
            .collect();
        assert_eq!(
            paths,
            [
                "src/eval/machine/intrinsic.rs",
                "src/eval/stg/force.rs",
                "src/eval/stg/sub/dict.rs"
            ]
        );
    }

    /// Tag- and length-prefixing must stop a byte moving across the boundary
    /// between two inputs from going unnoticed.
    #[test]
    fn inputs_are_unambiguously_separated() {
        assert_ne!(
            blob_source_hash(b"ab", b"c", &[], &[]),
            blob_source_hash(b"a", b"bc", &[], &[])
        );
    }

    /// End-to-end shape of the decision `build.rs` makes: a blob generated
    /// before an intrinsic was added is classified `Stale`, not `Fresh`.
    ///
    /// Fault injection: drop the `intrinsic-table` term from
    /// `blob_source_hash` and this fails with `Fresh`.
    #[test]
    fn a_blob_predating_a_new_intrinsic_is_stale() {
        // A blob stamped before the new intrinsic landed.
        let mut blob = blob_source_hash(PRELUDE, TABLE, &[], &[]).to_vec();
        blob.extend_from_slice(b"...rest of the postcard payload...");

        let expected_after =
            blob_source_hash(PRELUDE, b"INTRINSICS: [LOOKUP, RENDER, NEW]", &[], &[]);
        assert_eq!(
            classify_blob(Some(&blob), &expected_after),
            BlobFreshness::Stale
        );

        // Sanity: against the table it was built from, the same blob is fresh.
        let expected_before = blob_source_hash(PRELUDE, TABLE, &[], &[]);
        assert_eq!(
            classify_blob(Some(&blob), &expected_before),
            BlobFreshness::Fresh
        );
    }

    #[test]
    fn a_missing_or_truncated_blob_is_never_fresh() {
        let expected = blob_source_hash(PRELUDE, TABLE, &[], &[]);
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
