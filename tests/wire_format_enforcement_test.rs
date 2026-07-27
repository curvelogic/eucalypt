//! Enforcement tests for the pre-compiled prelude blob's freshness machinery
//! (eu-3skeg).
//!
//! The defect these guard against: the blob bakes every prelude global in as
//! `Ref::G(INTRINSIC_COUNT + prelude slot)`, so adding or removing an
//! intrinsic shifts the entire global slot map.  The freshness hash used to
//! cover only `lib/prelude.eu`, which an intrinsic-adding change need not
//! touch, so a stale blob was silently accepted and the binary produced wrong
//! answers — `eu -e '[1,2,3] map(inc)'` returning `[0, 1, 2]` — with no
//! warning.  The mitigation was a doc comment asking future authors to
//! hand-bump `BYTECODE_WIRE_FORMAT_VERSION`, duplicated across two files.
//!
//! The fix is mechanical: `src/eval/intrinsics.rs` is hashed into the
//! freshness hash, and the constant plus recipe live in exactly one file.
//! These tests assert the *structural* preconditions for that to keep
//! working; `src/eval/stg/wire_format.rs`'s own unit tests cover the hash
//! and staleness logic.

use std::path::{Path, PathBuf};

use eucalypt::eval::stg::wire_format::{
    blob_source_hash, BLOB_PATH, BUILD_RERUN_PATHS, INTRINSIC_TABLE_PATH, PRELUDE_SOURCE_PATH,
    WIRE_FORMAT_MODULE_PATH,
};

/// Workspace root — `CARGO_MANIFEST_DIR` for the `eucalypt` package.
fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn read(relative: &str) -> String {
    let path = root().join(relative);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("reading {}: {e}", path.display()))
}

/// Every `.rs` file in the workspace that could plausibly hold a stray copy
/// of the constant.
fn all_rust_sources() -> Vec<(String, String)> {
    fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk(&path, out);
            } else if path.extension().is_some_and(|e| e == "rs") {
                out.push(path);
            }
        }
    }

    let root = root();
    let mut paths = vec![root.join("build.rs")];
    for dir in ["src", "xtask", "tests", "benches"] {
        walk(&root.join(dir), &mut paths);
    }

    paths
        .into_iter()
        .filter_map(|p| {
            let text = std::fs::read_to_string(&p).ok()?;
            let name = p
                .strip_prefix(&root)
                .unwrap_or(&p)
                .to_string_lossy()
                .replace('\\', "/");
            Some((name, text))
        })
        .collect()
}

/// The de-duplication guarantee: `BYTECODE_WIRE_FORMAT_VERSION` is *defined*
/// in one place only.
///
/// It was previously declared in both `build.rs` and `xtask/src/main.rs`,
/// coordinated by a comment saying "MUST match" — and PRs #1078 and #1079
/// then collided on bumping it within a single day.
#[test]
fn the_wire_format_version_is_defined_exactly_once() {
    // Assembled at runtime so that this scanner does not match itself.
    let needle = format!("const {}", "BYTECODE_WIRE_FORMAT_VERSION");
    let definers: Vec<String> = all_rust_sources()
        .into_iter()
        .filter(|(_, text)| text.contains(&needle))
        .map(|(name, _)| name)
        .collect();

    assert_eq!(
        definers,
        vec![WIRE_FORMAT_MODULE_PATH.to_string()],
        "BYTECODE_WIRE_FORMAT_VERSION must be defined only in {WIRE_FORMAT_MODULE_PATH}; \
         a second definition has to be hand-kept in step, which is the failure this \
         guards against (eu-3skeg)"
    );
}

/// `build.rs` must reach the shared definitions by `include!`ing them, not by
/// re-declaring them: a build script cannot depend on its own crate, and that
/// constraint is exactly what tempted the original duplication.
#[test]
fn build_rs_shares_the_one_definition() {
    let build_rs = read("build.rs");
    assert!(
        build_rs.contains(&format!("include!(\"{WIRE_FORMAT_MODULE_PATH}\")")),
        "build.rs must include! {WIRE_FORMAT_MODULE_PATH} rather than duplicate it"
    );
    assert!(
        build_rs.contains("blob_source_hash") && build_rs.contains("classify_blob"),
        "build.rs must use the shared hash recipe and staleness verdict"
    );
    assert!(
        !build_rs.contains("Sha256"),
        "build.rs must not compute the freshness hash itself"
    );
}

/// `xtask prelude-compile` stamps the hash the build script later checks, so
/// it too must use the shared recipe.
#[test]
fn xtask_shares_the_one_definition() {
    let xtask = read("xtask/src/main.rs");
    assert!(
        xtask.contains("wire_format::"),
        "xtask must import the shared wire_format module"
    );
    assert!(
        xtask.contains("blob_source_hash("),
        "xtask must stamp the blob with the shared hash recipe"
    );
    assert!(
        !xtask.contains("Sha256"),
        "xtask must not compute the freshness hash itself"
    );
}

/// The enforcement is inert unless cargo re-runs `build.rs` when the intrinsic
/// catalogue changes.  Emitting any `rerun-if-changed` directive turns off
/// cargo's default whole-package tracking, so the catalogue — which is hashed
/// rather than compiled in — has to be named explicitly, as does the
/// `include!`d shared module.
#[test]
fn the_build_script_reruns_on_the_inputs_it_hashes() {
    for required in [
        INTRINSIC_TABLE_PATH,
        WIRE_FORMAT_MODULE_PATH,
        PRELUDE_SOURCE_PATH,
        BLOB_PATH,
    ] {
        assert!(
            BUILD_RERUN_PATHS.contains(&required),
            "build.rs must declare rerun-if-changed for {required}, \
             or the blob staleness check silently stops re-running"
        );
    }

    let build_rs = read("build.rs");
    assert!(
        build_rs.contains("BUILD_RERUN_PATHS"),
        "build.rs must emit rerun-if-changed from the shared BUILD_RERUN_PATHS list"
    );
}

/// A path that never existed would make its `rerun-if-changed` entry a no-op
/// typo.  `BLOB_PATH` is exempt: `lib/prelude.blob` is a git-ignored build
/// artefact that is legitimately absent until `cargo xtask prelude-compile`
/// runs, and cargo handles a directive naming a not-yet-existing file.
#[test]
fn every_tracked_input_path_resolves() {
    for path in BUILD_RERUN_PATHS {
        if *path == BLOB_PATH {
            continue;
        }
        assert!(
            root().join(path).exists(),
            "tracked build input {path} does not exist"
        );
    }
}

/// The catalogue really is the file the slot numbering comes from: if
/// `INTRINSICS` ever moves elsewhere, hashing this path stops protecting
/// anything.
#[test]
fn the_hashed_file_is_the_intrinsic_catalogue() {
    let table = read(INTRINSIC_TABLE_PATH);
    assert!(
        table.contains("static ref INTRINSICS: Vec<Intrinsic>"),
        "{INTRINSIC_TABLE_PATH} no longer declares the INTRINSICS catalogue — \
         the freshness hash is hashing the wrong file (eu-3skeg)"
    );
    assert!(
        table.contains("pub fn catalogue()"),
        "{INTRINSIC_TABLE_PATH} no longer exposes catalogue()"
    );
}

/// End-to-end, and the one check that survives a *stale build script*: when
/// the binary was built with an embedded blob, recompute the freshness hash
/// from the files on disk right now and confirm the embedded blob carries it.
///
/// `build.rs` normally guarantees this, but only when cargo re-ran it.  This
/// test re-reads the inputs at test time, so it fires even if a cached
/// `cfg(prelude_blob_ok)` was carried over a change cargo failed to notice.
#[test]
#[cfg(prelude_blob_ok)]
fn the_embedded_blob_matches_the_inputs_on_disk() {
    use eucalypt::eval::stg::wire_format::read_blob_source_hash;

    let prelude = std::fs::read(root().join(PRELUDE_SOURCE_PATH)).expect("prelude source");
    let table = std::fs::read(root().join(INTRINSIC_TABLE_PATH)).expect("intrinsic catalogue");
    let expected = blob_source_hash(&prelude, &table);

    let embedded = read_blob_source_hash(eucalypt::driver::resources::PRELUDE_BLOB_BYTES)
        .expect("embedded blob carries a source hash");

    assert_eq!(
        embedded, expected,
        "the embedded prelude blob was generated against different inputs than the \
         working tree holds — run `cargo xtask prelude-compile` (eu-3skeg)"
    );
}

/// Keep `blob_source_hash` referenced in blob-less builds too, so this file
/// has teeth either way.
#[test]
fn the_freshness_hash_covers_the_real_inputs() {
    let prelude = std::fs::read(root().join(PRELUDE_SOURCE_PATH)).expect("prelude source");
    let table = std::fs::read(root().join(INTRINSIC_TABLE_PATH)).expect("intrinsic catalogue");

    let mut table_with_new_intrinsic = table.clone();
    table_with_new_intrinsic
        .extend_from_slice(b"\n// Intrinsic { name: \"CONTRACT_FAIL\", .. }: an added intrinsic\n");

    assert_ne!(
        blob_source_hash(&prelude, &table),
        blob_source_hash(&prelude, &table_with_new_intrinsic),
        "editing the intrinsic catalogue must invalidate the prelude blob without \
         anyone hand-bumping BYTECODE_WIRE_FORMAT_VERSION (eu-3skeg)"
    );
}
