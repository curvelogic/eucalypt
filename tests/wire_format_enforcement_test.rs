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
    blob_source_hash, read_front_end_sources, read_stg_sources, BLOB_PATH, BUILD_RERUN_PATHS,
    DEMAND_ANALYSIS_PATHS, FRONT_END_SOURCE_ROOTS, INTRINSIC_TABLE_PATH, PRELUDE_SOURCE_PATH,
    STG_SOURCE_ROOTS, WIRE_FORMAT_MODULE_PATH,
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
            // `DirEntry::file_type` does not follow symlinks, so a symlinked
            // directory is not entered, as `read_source_roots` does not.
            if entry.file_type().is_ok_and(|t| t.is_dir()) {
                walk(&path, out);
            } else if entry.file_type().is_ok_and(|t| t.is_file())
                && path.extension().is_some_and(|e| e == "rs")
            {
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
    ]
    .into_iter()
    .chain(DEMAND_ANALYSIS_PATHS.iter().copied())
    .chain(STG_SOURCE_ROOTS.iter().copied())
    .chain(FRONT_END_SOURCE_ROOTS.iter().copied())
    {
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
    let analysis: Vec<Vec<u8>> = DEMAND_ANALYSIS_PATHS
        .iter()
        .map(|p| std::fs::read(root().join(p)).expect("demand analysis source"))
        .collect();
    let stg = read_stg_sources(&root()).expect("STG sources");
    let front_end = read_front_end_sources(&root()).expect("front-end sources");
    let expected = blob_source_hash(&prelude, &table, &analysis, &stg, &front_end);

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
        blob_source_hash(&prelude, &table, &[], &[], &[]),
        blob_source_hash(&prelude, &table_with_new_intrinsic, &[], &[], &[]),
        "editing the intrinsic catalogue must invalidate the prelude blob without \
         anyone hand-bumping BYTECODE_WIRE_FORMAT_VERSION (eu-3skeg)"
    );
}

/// Every hand-written intrinsic wrapper is a blob input (eu-ox7qz): the blob
/// bakes in the encoded wrapper globals and the wrapper bodies inlined into
/// prelude code, so a file that defines `StgIntrinsic::wrapper` and is not
/// under a hashed STG root would let a wrapper edit leave a stale blob
/// reported fresh — blob mode silently running the old wrapper.
///
/// Fault injection: drop `src/eval/stg` (or `src/eval/machine/intrinsic.rs`)
/// from `STG_SOURCE_ROOTS` and this fails.
#[test]
fn every_intrinsic_wrapper_source_is_hashed() {
    let hashed: Vec<String> = read_stg_sources(&root())
        .expect("STG sources")
        .into_iter()
        .map(|(path, _)| path)
        .collect();
    let wrappers: Vec<String> = all_rust_sources()
        .into_iter()
        .filter(|(path, text)| path.starts_with("src/") && text.contains("fn wrapper("))
        .map(|(path, _)| path)
        .collect();
    assert!(
        wrappers.iter().any(|p| p == "src/eval/stg/force.rs"),
        "the scan found no wrapper in src/eval/stg/force.rs: {wrappers:?}"
    );
    for path in &wrappers {
        assert!(
            hashed.contains(path),
            "{path} defines an intrinsic wrapper but is not hashed into the prelude \
             blob's freshness key — add it under STG_SOURCE_ROOTS (eu-ox7qz)"
        );
    }
}

/// Files under `src/core` and `src/syntax` deliberately left out of the
/// blob's freshness key, each with the reason nothing in it can change a
/// generated blob. A directory entry ends with `/`. Every entry must exist
/// and must not also be hashed. See `FRONT_END_SOURCE_ROOTS` (eu-d3ydw).
const FRONT_END_UNHASHED: &[(&str, &str)] = &[
    ("src/core/mod.rs", "module declarations"),
    ("src/core/error.rs", "failure diagnostics"),
    ("src/core/analyse/", "test plans for eu test"),
    ("src/core/export/", "dump rendering"),
    (
        "src/core/simplify/",
        "prelude-compile runs no elimination pass",
    ),
    (
        "src/core/verify/",
        "checks that can fail generation, never alter it",
    ),
    // The type checker proper, file by file, so that a new file under
    // `src/core/typecheck/` (a renderer split out of `parse.rs`, say) must
    // be decided on rather than excluded by default. Its result is not
    // baked: prelude-compile type-checks only as a sanity pass.
    ("src/core/typecheck/mod.rs", "module declarations"),
    ("src/core/typecheck/check.rs", "checker; result not baked"),
    ("src/core/typecheck/env.rs", "checker; result not baked"),
    ("src/core/typecheck/error.rs", "checker diagnostics"),
    (
        "src/core/typecheck/resolve_typedata.rs",
        "checker; result not baked",
    ),
    ("src/core/typecheck/subtype.rs", "checker; result not baked"),
    ("src/core/typecheck/unify.rs", "checker; result not baked"),
    ("src/syntax/mod.rs", "module declarations"),
    ("src/syntax/error.rs", "failure diagnostics"),
    ("src/syntax/export/", "dump rendering"),
];

/// Every Rust file of the parser and core front end is either hashed into
/// the blob's freshness key — through `FRONT_END_SOURCE_ROOTS` or
/// `DEMAND_ANALYSIS_PATHS` — or on `FRONT_END_UNHASHED` with a reason, so a
/// new file under `src/core` or `src/syntax` cannot leave an edit to it
/// reported fresh without someone deciding so (eu-d3ydw).
///
/// Fault injection: drop `src/core/desugar` from `FRONT_END_SOURCE_ROOTS`,
/// or add a file under `src/core/typecheck/`, and this fails.
#[test]
fn every_front_end_source_is_hashed_or_excluded_with_a_reason() {
    let hashed: Vec<String> = read_front_end_sources(&root())
        .expect("front-end sources")
        .into_iter()
        .map(|(path, _)| path)
        .collect();
    let front_end: Vec<String> = all_rust_sources()
        .into_iter()
        .map(|(path, _)| path)
        .filter(|path| path.starts_with("src/core/") || path.starts_with("src/syntax/"))
        // Dot-files (editor lock and backup files) are skipped, as
        // `read_source_roots` skips them.
        .filter(|path| !path.split('/').any(|c| c.starts_with('.')))
        .collect();
    assert!(
        front_end
            .iter()
            .any(|p| p == "src/core/desugar/rowan_ast.rs"),
        "the scan found no desugarer: {front_end:?}"
    );
    for path in &front_end {
        let excluded = FRONT_END_UNHASHED.iter().any(|(entry, _)| {
            if entry.ends_with('/') {
                path.starts_with(entry)
            } else {
                path == entry
            }
        });
        let in_demand_analysis = DEMAND_ANALYSIS_PATHS.contains(&path.as_str());
        assert!(
            hashed.contains(path) || in_demand_analysis || excluded,
            "{path} is neither hashed into the prelude blob's freshness key nor \
             excluded with a reason — add it under FRONT_END_SOURCE_ROOTS, or to \
             FRONT_END_UNHASHED if nothing in it can change a generated blob (eu-d3ydw)"
        );
        assert!(
            !(excluded && (hashed.contains(path) || in_demand_analysis)),
            "{path} is both hashed and listed in FRONT_END_UNHASHED"
        );
    }
    // A stale exclusion (a file moved or deleted) would otherwise sit
    // unnoticed and could later exclude a new file of the same name.
    for (entry, _) in FRONT_END_UNHASHED {
        assert!(
            root().join(entry).exists(),
            "FRONT_END_UNHASHED names {entry}, which no longer exists"
        );
    }
}

/// The type-annotation renderer the desugarer runs is hashed even though the
/// checker beside it is not: the desugarer bakes `render_scheme`'s output
/// into the blob's desugared cores (eu-d3ydw).
///
/// Fault injection: drop `src/core/typecheck/parse.rs` from
/// `FRONT_END_SOURCE_ROOTS` and this fails.
#[test]
fn the_desugarer_s_type_renderer_is_hashed() {
    let hashed: Vec<String> = read_front_end_sources(&root())
        .expect("front-end sources")
        .into_iter()
        .map(|(path, _)| path)
        .collect();
    for path in [
        "src/core/typecheck/parse.rs",
        "src/core/typecheck/types.rs",
        "src/syntax/rowan/parse.rs",
        "src/core/expr.rs",
    ] {
        assert!(
            hashed.iter().any(|p| p == path),
            "{path} not hashed: {hashed:?}"
        );
    }
    assert!(
        !hashed.iter().any(|p| p == "src/core/typecheck/check.rs"),
        "the checker is not a blob input"
    );
}

/// Reading the STG sources is deterministic and keyed by workspace-relative
/// path, so `build.rs` (run from the package root) and `xtask` (given an
/// absolute workspace root) compute the same hash.
#[test]
fn stg_sources_are_read_relative_and_sorted() {
    let absolute = read_stg_sources(&root()).expect("STG sources");
    let paths: Vec<&str> = absolute.iter().map(|(p, _)| p.as_str()).collect();
    let mut sorted = paths.clone();
    sorted.sort();
    assert_eq!(paths, sorted);
    assert!(paths.contains(&"src/eval/stg/force.rs"), "{paths:?}");
    assert!(
        paths.contains(&"src/eval/machine/intrinsic.rs"),
        "{paths:?}"
    );
    assert!(paths.iter().all(|p| p.ends_with(".rs")));
}

/// The embedded blob carries the demand signatures the analysis derived for
/// the prelude (eu-oaz72.77): strict where the definition forces an argument
/// on every path, lazy where it may not. A blob carrying no signatures, or
/// signatures stricter than the definitions, fails here.
#[test]
#[cfg(prelude_blob_ok)]
fn the_embedded_blob_carries_derived_demand_signatures() {
    use eucalypt::core::demand::Strictness;
    use eucalypt::eval::stg::blob::PreludeBlob;

    let blob = PreludeBlob::from_bytes(eucalypt::driver::resources::PRELUDE_BLOB_BYTES)
        .expect("embedded blob decodes");
    let strictness = |name: &str| -> Vec<bool> {
        let slot = blob.name_to_slot[name];
        blob.demand_signatures[slot]
            .as_ref()
            .unwrap_or_else(|| panic!("`{name}` has a demand signature"))
            .iter()
            .map(|d| d.strictness == Strictness::Strict)
            .collect()
    };

    // `Some(true)`: the definition forces the argument on every path, and
    // the analysis must find it. `Some(false)`: some path never forces it,
    // so a strict claim would be unsound. `None`: not pinned. A pipeline
    // step (`xs nil? then(d, …)`) saturates `then`, an alias (`zip-with:
    // map2`) and a partial application (`zip: zip-with(pair)`) take the
    // signature of the function they denote (eu-oaz72.92).
    let (s, l) = (Some(true), Some(false));
    for (name, expected) in [
        ("max", vec![s, s]),
        ("take", vec![s, l]),
        ("then", vec![l, l, s]),
        ("head-or", vec![l, s]),
        ("tail-or", vec![l, s]),
        ("zip-with", vec![l, s, l]),
        ("zip", vec![s, l]),
        ("foldl", vec![l, l, s]),
        ("append", vec![s, l]),
        ("nth", vec![None, l]),
    ] {
        let actual = strictness(name);
        assert_eq!(actual.len(), expected.len(), "arity of `{name}`");
        for (i, (a, e)) in actual.iter().zip(&expected).enumerate() {
            if let Some(e) = e {
                assert_eq!(a, e, "derived strictness of `{name}` argument {i}");
            }
        }
    }
}

/// Every signature the embedded blob carries is the one the analysis
/// derives from the prelude source now, slot by slot, not just the
/// positions pinned above (eu-oaz72.92). The front-end steps are those
/// `cargo xtask prelude-compile` runs before step 5b; if they drift apart,
/// this fails rather than passing silently.
#[test]
#[cfg(prelude_blob_ok)]
fn every_blob_signature_matches_the_source_analysis() {
    use eucalypt::core::analyse_demand::derive_top_level_signatures;
    use eucalypt::driver::source::SourceLoader;
    use eucalypt::eval::stg::blob::PreludeBlob;
    use eucalypt::syntax::input::{Input, Locator};

    let inputs = vec![
        Input::new(
            Locator::Fs(root().join("build-meta.yaml")),
            Some("__build".to_string()),
            "yaml",
        ),
        Input::new(
            Locator::Pseudo("io".to_string()),
            Some("__io".to_string()),
            "core",
        ),
        Input::new(
            Locator::Pseudo("args".to_string()),
            Some("__args".to_string()),
            "core",
        ),
        Input::new(Locator::Fs(root().join(PRELUDE_SOURCE_PATH)), None, "eu"),
    ];
    let mut loader = SourceLoader::new(vec![]).with_deterministic_io(true);
    for input in &inputs {
        loader.load(input).expect("load");
    }
    for input in &inputs {
        loader.translate(input).expect("translate");
    }
    loader.merge_units(&inputs).expect("merge_units");
    loader.extract_operators();
    loader.extract_visibility();
    loader.cook().expect("cook");
    loader.hoist_namespaces().expect("hoist_namespaces");
    loader.fuse_destructure().expect("fuse_destructure");
    let derived = derive_top_level_signatures(&loader.core().expr);

    let blob = PreludeBlob::from_bytes(eucalypt::driver::resources::PRELUDE_BLOB_BYTES)
        .expect("embedded blob decodes");
    assert_eq!(blob.demand_signatures.len(), blob.name_to_slot.len());
    let mut mismatches = Vec::new();
    for (name, &slot) in &blob.name_to_slot {
        let expected = derived.get(name).cloned().flatten();
        if blob.demand_signatures[slot] != expected {
            mismatches.push(name.clone());
        }
    }
    mismatches.sort();
    assert!(
        mismatches.is_empty(),
        "blob signatures differ from the source analysis for: {mismatches:?}"
    );
    // The slots with no signature are the prelude's values, not functions
    // the analysis failed to see.
    let known = blob.demand_signatures.iter().flatten().count();
    assert!(
        known * 10 >= blob.demand_signatures.len() * 9,
        "only {known} of {} slots carry a signature",
        blob.demand_signatures.len()
    );
}
