//! The committed tree-sitter browser build must match the grammar sources
//! (eu-ioz21.5).
//!
//! `editors/tree-sitter-eucalypt/tree-sitter-eucalypt.wasm` is published by
//! committing it: a browser consumer fetches it, with `queries/highlights.scm`,
//! from the eucalypt tag it wants. Building it needs emscripten (or docker),
//! which `cargo test` does not have, so this test checks the record the build
//! leaves instead: `wasm-manifest.json` holds the digest of every C source the
//! WASM was compiled from and of the WASM itself. Changing `src/parser.c`,
//! `src/scanner.c` or a header without running `npm run build-wasm` fails
//! here, so the published WASM cannot silently fall behind the grammar.

use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

const REBUILD: &str = "run `npm ci && npm run build-wasm` in editors/tree-sitter-eucalypt \
                       and commit tree-sitter-eucalypt.wasm and wasm-manifest.json";

fn grammar_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("editors/tree-sitter-eucalypt")
}

fn manifest() -> serde_json::Value {
    let text = fs::read_to_string(grammar_dir().join("wasm-manifest.json"))
        .expect("editors/tree-sitter-eucalypt/wasm-manifest.json is missing");
    serde_json::from_str(&text).expect("wasm-manifest.json is not valid JSON")
}

fn sha256_hex(path: &Path) -> String {
    let bytes = fs::read(path).unwrap_or_else(|e| panic!("cannot read {}: {e}", path.display()));
    Sha256::digest(bytes)
        .iter()
        .map(|b| format!("{b:02x}"))
        .collect()
}

/// `src/*.c` and `src/tree_sitter/*.h`, as paths relative to the grammar
/// directory — the set `scripts/build-wasm.mjs` records.
fn current_sources() -> Vec<String> {
    let mut found = Vec::new();
    for (dir, ext) in [("src", "c"), ("src/tree_sitter", "h")] {
        for entry in fs::read_dir(grammar_dir().join(dir)).unwrap() {
            let path = entry.unwrap().path();
            if path.is_file() && path.extension().is_some_and(|e| e == ext) {
                let name = path.file_name().unwrap().to_str().unwrap();
                found.push(format!("{dir}/{name}"));
            }
        }
    }
    found.sort();
    found
}

#[test]
fn wasm_manifest_matches_grammar_sources() {
    let manifest = manifest();
    let recorded: BTreeMap<String, String> = manifest["sources"]
        .as_object()
        .expect("manifest has no sources")
        .iter()
        .map(|(k, v)| (k.clone(), v.as_str().unwrap().to_string()))
        .collect();

    let current = current_sources();
    let recorded_names: Vec<String> = recorded.keys().cloned().collect();
    assert_eq!(
        recorded_names, current,
        "the grammar's C sources are not the ones the WASM was built from; {REBUILD}"
    );

    let stale: Vec<&String> = recorded
        .iter()
        .filter(|(file, digest)| sha256_hex(&grammar_dir().join(file)) != **digest)
        .map(|(file, _)| file)
        .collect();
    assert!(
        stale.is_empty(),
        "changed since tree-sitter-eucalypt.wasm was built: {stale:?}; {REBUILD}"
    );
}

#[test]
fn wasm_matches_manifest() {
    let manifest = manifest();
    let wasm = grammar_dir().join(manifest["wasm"]["file"].as_str().unwrap());
    let bytes = fs::read(&wasm).expect("tree-sitter-eucalypt.wasm is missing");
    assert_eq!(
        sha256_hex(&wasm),
        manifest["wasm"]["sha256"].as_str().unwrap(),
        "tree-sitter-eucalypt.wasm is not the build wasm-manifest.json records; {REBUILD}"
    );
    assert_eq!(&bytes[..4], b"\0asm", "not a WebAssembly module");
    // web-tree-sitter loads grammars as emscripten side modules, which carry
    // a `dylink.0` custom section first.
    assert!(
        bytes.windows(8).take(64).any(|w| w == b"dylink.0"),
        "not an emscripten side module (no dylink.0 section)"
    );
}

#[test]
fn wasm_manifest_versions_are_current() {
    let manifest = manifest();
    let parser = fs::read_to_string(grammar_dir().join("src/parser.c")).unwrap();
    let abi = parser
        .lines()
        .find_map(|l| l.strip_prefix("#define LANGUAGE_VERSION "))
        .and_then(|v| v.trim().parse::<u64>().ok())
        .expect("no LANGUAGE_VERSION in parser.c");
    assert_eq!(manifest["abi_version"].as_u64(), Some(abi), "{REBUILD}");

    let lock: serde_json::Value =
        serde_json::from_str(&fs::read_to_string(grammar_dir().join("package-lock.json")).unwrap())
            .unwrap();
    for (key, package) in [
        ("tree_sitter_cli", "tree-sitter-cli"),
        ("web_tree_sitter", "web-tree-sitter"),
    ] {
        assert_eq!(
            manifest[key],
            lock["packages"][format!("node_modules/{package}")]["version"],
            "package-lock.json's {package} is not the one recorded; {REBUILD}"
        );
    }

    let package: serde_json::Value =
        serde_json::from_str(&fs::read_to_string(grammar_dir().join("package.json")).unwrap())
            .unwrap();
    assert_eq!(manifest["grammar_version"], package["version"], "{REBUILD}");
}
