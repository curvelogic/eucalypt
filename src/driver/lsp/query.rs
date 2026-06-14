//! Incremental query framework for the LSP front-end pipeline.
//!
//! Provides a demand-driven, memoised query graph over the front-end pipeline so
//! that an edit to one file re-runs only the queries whose inputs actually
//! changed.
//!
//! ## Design
//!
//! The framework is bespoke and single-threaded.  The core types (`RcExpr`,
//! `GreenNode`) are `Rc`-based and not `Send`; adopting Salsa would require a
//! migration to `Arc` that is out of scope for 0.9.  The query signatures are
//! designed to be compatible with a future Salsa migration.
//!
//! ### Key concepts
//!
//! - **Revision**: a global counter incremented on each input change (file edit).
//! - **ContentHash**: a `u64` hash of a query's output, used to detect whether
//!   downstream queries need to re-run.  Computed once on execution; compared on
//!   re-verification.
//! - **CachedEntry**: a stored query result, including its input hash, output
//!   hash, verified-at revision, value, and dependency list.
//! - **QueryStore**: the central table.  On access, stale entries are
//!   re-verified lazily: re-hash inputs, compare to stored input hash; if
//!   unchanged, bump `verified_at` and return the cached value.
//!
//! ### Re-verification protocol
//!
//! ```text
//! access query Q at revision R
//!   ├─ entry not found → execute, store, return
//!   ├─ entry.verified_at == R → return cached (already verified this revision)
//!   └─ entry.verified_at < R (stale)
//!        ├─ re-verify dependencies (recursive)
//!        ├─ recompute input_hash from current dependency output hashes
//!        ├─ input_hash == entry.input_hash → bump verified_at, return cached
//!        └─ input_hash ≠ entry.input_hash → re-execute, store new, return
//! ```
//!
//! ### Import graph and cross-file invalidation
//!
//! The store maintains a bidirectional import graph.  When file B is edited and
//! file A imports B, the store knows to re-verify A's pipeline query on the
//! next access.  This is tracked via:
//!
//! - `imports`: forward edges (file → files it imports)
//! - `importers`: reverse edges (file → files that import it)
//!
//! ### What is NOT cached
//!
//! - STG compilation and VM execution (per spec: front-end pipeline only)
//! - Prelude evaluation (W6 blob provides a pre-compiled static artefact)

use std::any::Any;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use lsp_types::Url;

use crate::syntax::rowan::ParseError;

// ── Primitive types ─────────────────────────────────────────────────────────

/// A content hash identifying a query result.
///
/// Computed from the query's outputs; used by downstream queries to detect
/// whether they need to re-run.
pub type ContentHash = u64;

/// Global revision counter.
///
/// Incremented on each input change (e.g. file edit via `didChange`).
/// Queries store the revision at which they were last verified; a query is
/// stale when `verified_at < store.revision`.
pub type Revision = u64;

// ── FileId ──────────────────────────────────────────────────────────────────

/// Identifies a file in the query store.
///
/// The primary key for all file-scoped queries.
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct FileId {
    /// Canonical file URI.
    pub uri: Url,
}

impl FileId {
    /// Create a `FileId` from a URI.
    pub fn new(uri: Url) -> Self {
        Self { uri }
    }

    /// Create a `FileId` from a URI reference (cloning).
    pub fn from_url(uri: &Url) -> Self {
        Self { uri: uri.clone() }
    }
}

// ── QueryKey ────────────────────────────────────────────────────────────────

/// Discriminates all query types in the store.
///
/// Every variant corresponds to one pipeline pass.  The key is used as the
/// primary index into the cache table.
///
/// Designed for future Salsa compatibility: each variant carries only the
/// *inputs* that identify the query uniquely (currently just `FileId`).
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum QueryKey {
    /// Input query: the raw source text of a file (set externally by the LSP).
    FileText(FileId),
    /// Parse the file text into a lossless syntax tree.
    Parse(FileId),
    /// Desugar the parse tree into a core expression.
    Desugar(FileId),
    /// Extract the unit interface (operators, monad specs, type summary).
    UnitInterface(FileId),
    /// Collect the unit interfaces of all transitive imports.
    ImportInterfaces(FileId),
    /// Merge the file's desugared expression with its import interfaces.
    Merge(FileId),
    /// Apply operator precedence (cook pass).
    Cook(FileId),
    /// Dead-code elimination.
    Eliminate(FileId),
    /// Inlining and simplification.
    Inline(FileId),
    /// Type-check the inlined expression.
    TypeCheck(FileId),
    /// Aggregate parse errors and type warnings into LSP diagnostics.
    Diagnostics(FileId),
}

impl QueryKey {
    /// Return the `FileId` this key is scoped to.
    pub fn file_id(&self) -> &FileId {
        match self {
            QueryKey::FileText(f)
            | QueryKey::Parse(f)
            | QueryKey::Desugar(f)
            | QueryKey::UnitInterface(f)
            | QueryKey::ImportInterfaces(f)
            | QueryKey::Merge(f)
            | QueryKey::Cook(f)
            | QueryKey::Eliminate(f)
            | QueryKey::Inline(f)
            | QueryKey::TypeCheck(f)
            | QueryKey::Diagnostics(f) => f,
        }
    }
}

// ── CachedEntry ─────────────────────────────────────────────────────────────

/// A single cached query result with validation metadata.
struct CachedEntry {
    /// Hash of all inputs at the time of computation.
    ///
    /// On re-verification, inputs are re-hashed and compared to this value.
    /// If equal, the cached result is still valid.
    input_hash: ContentHash,

    /// Hash of the output value.
    ///
    /// Stored so that downstream queries can check whether *their* inputs
    /// changed without re-executing the upstream query.
    output_hash: ContentHash,

    /// Revision at which this entry was last verified to be valid.
    verified_at: Revision,

    /// The cached value, type-erased.  Downcast on access.
    value: Box<dyn Any>,

    /// Query keys this entry depends on (direct dependencies only).
    dependencies: Vec<QueryKey>,
}

// ── Content hashing utilities ────────────────────────────────────────────────

/// Hash an arbitrary value that implements `Hash` to a `ContentHash`.
pub fn hash_value<T: Hash>(v: &T) -> ContentHash {
    let mut h = std::collections::hash_map::DefaultHasher::new();
    v.hash(&mut h);
    h.finish()
}

/// Hash a byte slice to a `ContentHash`.
pub fn hash_bytes(b: &[u8]) -> ContentHash {
    let mut h = std::collections::hash_map::DefaultHasher::new();
    b.hash(&mut h);
    h.finish()
}

/// Hash a string to a `ContentHash`.
pub fn hash_str(s: &str) -> ContentHash {
    hash_bytes(s.as_bytes())
}

/// Combine two hashes into one.
///
/// Used to build an input hash from multiple dependency output hashes.
pub fn hash_combine(a: ContentHash, b: ContentHash) -> ContentHash {
    // Inspired by boost::hash_combine
    let mut h = std::collections::hash_map::DefaultHasher::new();
    a.hash(&mut h);
    b.hash(&mut h);
    h.finish()
}

/// Combine a slice of hashes into a single `ContentHash`.
pub fn hash_many(hashes: &[ContentHash]) -> ContentHash {
    hashes.iter().fold(0u64, |acc, &h| hash_combine(acc, h))
}

/// Compute the content hash of a `rowan::GreenNode`.
///
/// Uses the text of the syntax tree (the full round-tripped source text) as a
/// stable, deterministic representation.  Two green nodes with identical
/// structure and token text will produce the same hash.
///
/// Note: rowan's green nodes include trivia (whitespace, comments) in the text,
/// so a whitespace-only edit will produce a different hash.  Stripping trivia
/// for a structure-only hash is a future optimisation.
pub fn hash_green_node(node: &rowan::GreenNode) -> ContentHash {
    use crate::syntax::rowan::kind::SyntaxNode;
    let syntax = SyntaxNode::new_root(node.clone());
    hash_str(&syntax.text().to_string())
}

// ── Cached parse result ──────────────────────────────────────────────────────

/// The result of the parse query for a single file.
#[derive(Clone)]
pub struct CachedParse {
    /// The lossless green tree produced by the parser.
    pub green: rowan::GreenNode,
    /// Parse errors (may be non-empty for error-recovering parses).
    pub errors: Vec<ParseError>,
    /// Content hash of the green tree (= `hash_green_node(&green)`).
    pub output_hash: ContentHash,
}

// ── QueryStore ───────────────────────────────────────────────────────────────

/// The central memoisation table for the LSP front-end pipeline.
///
/// Owned by `ServerState`.  Single-threaded; not `Send`.
///
/// ## Usage pattern
///
/// ```text
/// // On file change (didChange):
/// store.set_file_text(file_id, new_text);
/// // …spawn or run pipeline for file_id and all importers_of(file_id)…
///
/// // When a pipeline completes:
/// store.store_parse(file_id, green, errors);
/// store.store_pipeline_result(file_id, result, discovered_imports);
///
/// // On request (hover, diagnostics, etc.):
/// let cached = store.get_pipeline_result(&file_id);
/// ```
pub struct QueryStore {
    /// Global revision counter.  Incremented by `set_file_text`.
    revision: Revision,

    /// Per-file text with content hash.
    ///
    /// `Arc<String>` because text may be shared across the parse boundary
    /// without copying; it is always a leaf input, never embedded in an
    /// `Rc`-based expression type.
    file_texts: HashMap<FileId, (Arc<String>, ContentHash)>,

    /// General-purpose cache table.
    ///
    /// Values are type-erased; access requires the correct type parameter.
    entries: HashMap<QueryKey, CachedEntry>,

    /// Dedicated parse cache, kept separate for fast green-node access.
    parse_cache: HashMap<FileId, CachedParse>,

    /// Forward import edges: file → the files it imports.
    ///
    /// Updated when a pipeline run for a file completes and reveals its
    /// import set.
    imports: HashMap<FileId, Vec<FileId>>,

    /// Reverse import edges: file → files that import it.
    ///
    /// Derived from `imports`; maintained consistently.
    importers: HashMap<FileId, HashSet<FileId>>,
}

impl QueryStore {
    /// Create a new, empty query store.
    pub fn new() -> Self {
        Self {
            revision: 0,
            file_texts: HashMap::new(),
            entries: HashMap::new(),
            parse_cache: HashMap::new(),
            imports: HashMap::new(),
            importers: HashMap::new(),
        }
    }

    // ── Revision ─────────────────────────────────────────────────────────────

    /// Current global revision.
    pub fn revision(&self) -> Revision {
        self.revision
    }

    /// Increment the revision counter and return the new value.
    fn bump_revision(&mut self) -> Revision {
        self.revision += 1;
        self.revision
    }

    // ── File text (input queries) ─────────────────────────────────────────────

    /// Set the source text of a file.
    ///
    /// If the text is identical to the previously stored text (same content
    /// hash), the revision counter is NOT incremented and `false` is returned.
    ///
    /// If the text changed, the revision counter is incremented and `true` is
    /// returned.  Downstream cached entries are NOT eagerly removed — they
    /// become stale (their `verified_at` lags behind the current revision) and
    /// are detected as stale by `get_if_current`.  Callers that confirm a
    /// cached entry is still valid despite the revision bump can call
    /// `mark_verified` to bring it up to date.
    pub fn set_file_text(&mut self, file: FileId, text: Arc<String>) -> bool {
        let new_hash = hash_str(&text);
        let changed = match self.file_texts.get(&file) {
            Some((_, old_hash)) => *old_hash != new_hash,
            None => true,
        };
        if changed {
            self.file_texts.insert(file, (text, new_hash));
            self.bump_revision();
        }
        changed
    }

    /// Return the source text for `file`, or `None` if not loaded.
    pub fn file_text(&self, file: &FileId) -> Option<Arc<String>> {
        self.file_texts.get(file).map(|(t, _)| t.clone())
    }

    /// Return the content hash of the stored file text, or `None`.
    pub fn file_text_hash(&self, file: &FileId) -> Option<ContentHash> {
        self.file_texts.get(file).map(|(_, h)| *h)
    }

    // ── Parse cache ───────────────────────────────────────────────────────────

    /// Return the cached parse result for `file` if it is still current.
    ///
    /// A cached parse result is considered current when the stored content
    /// hash matches the current file text hash (i.e. the file has not been
    /// edited since the last parse).
    pub fn get_parse(&self, file: &FileId) -> Option<&CachedParse> {
        let cached = self.parse_cache.get(file)?;
        let current_hash = self.file_text_hash(file)?;
        // The parse content hash was computed from the file text at parse time.
        // Re-check by comparing the green node's text hash against the current
        // file text hash.  (They are equal for a successful parse of the same text.)
        if cached.output_hash == current_hash {
            Some(cached)
        } else {
            None
        }
    }

    /// Store a parse result for `file`.
    ///
    /// The `output_hash` is computed from the green node text, which
    /// allows downstream queries to detect whitespace-only edits: if the
    /// green-node text is unchanged, downstream queries need not re-run.
    pub fn store_parse(&mut self, file: FileId, green: rowan::GreenNode, errors: Vec<ParseError>) {
        let output_hash = hash_green_node(&green);
        self.parse_cache.insert(
            file,
            CachedParse {
                green,
                errors,
                output_hash,
            },
        );
    }

    /// Return the green node stored for `file`, regardless of staleness.
    ///
    /// Used for green-node change detection: the caller compares the stored
    /// node against a freshly parsed node to decide whether to re-run the
    /// pipeline.
    pub fn last_green(&self, file: &FileId) -> Option<&rowan::GreenNode> {
        self.parse_cache.get(file).map(|c| &c.green)
    }

    /// Return the cached parse errors for `file`, regardless of staleness.
    pub fn last_parse_errors(&self, file: &FileId) -> Option<&Vec<ParseError>> {
        self.parse_cache.get(file).map(|c| &c.errors)
    }

    // ── General typed cache ───────────────────────────────────────────────────

    /// Store a typed value under `key`.
    ///
    /// `input_hash`: hash of all inputs to this query (used for re-verification).
    /// `output_hash`: hash of the result (used by downstream queries).
    /// `deps`: direct dependency keys.
    pub fn store<T: Any + 'static>(
        &mut self,
        key: QueryKey,
        value: T,
        input_hash: ContentHash,
        output_hash: ContentHash,
        deps: Vec<QueryKey>,
    ) {
        let entry = CachedEntry {
            input_hash,
            output_hash,
            verified_at: self.revision,
            value: Box::new(value),
            dependencies: deps,
        };
        self.entries.insert(key, entry);
    }

    /// Retrieve a cached value for `key` if it was verified in the current revision.
    ///
    /// Returns `None` if the entry is absent or stale.  The caller must then
    /// re-execute the query and call `store` with the new result.
    pub fn get_if_current<T: Any + Clone>(&self, key: &QueryKey) -> Option<&T> {
        let entry = self.entries.get(key)?;
        if entry.verified_at == self.revision {
            entry.value.downcast_ref::<T>()
        } else {
            None
        }
    }

    /// Return the output hash of the cached entry for `key`, if any.
    pub fn output_hash_of(&self, key: &QueryKey) -> Option<ContentHash> {
        self.entries.get(key).map(|e| e.output_hash)
    }

    /// Return the input hash of the cached entry for `key`, if any.
    pub fn input_hash_of(&self, key: &QueryKey) -> Option<ContentHash> {
        self.entries.get(key).map(|e| e.input_hash)
    }

    /// Return the direct dependency keys of the cached entry for `key`, if any.
    ///
    /// Used by the re-verification protocol to walk the dependency graph: when
    /// verifying a query, each dependency is verified first; if any dependency's
    /// output hash changed, the query must be re-executed.
    pub fn dependencies_of(&self, key: &QueryKey) -> Option<&[QueryKey]> {
        self.entries.get(key).map(|e| e.dependencies.as_slice())
    }

    /// Mark an entry as verified at the current revision without re-executing.
    ///
    /// Called when re-verification confirms that the inputs are unchanged.
    pub fn mark_verified(&mut self, key: &QueryKey) {
        if let Some(entry) = self.entries.get_mut(key) {
            entry.verified_at = self.revision;
        }
    }

    // ── Import graph ──────────────────────────────────────────────────────────

    /// Record that `dependent` imports the files in `new_imports`.
    ///
    /// Replaces any previous import set for `dependent`.  Updates both the
    /// forward (`imports`) and reverse (`importers`) edges.
    pub fn set_imports(&mut self, dependent: &FileId, new_imports: Vec<FileId>) {
        // Remove old reverse edges.
        if let Some(old) = self.imports.remove(dependent) {
            for dep in &old {
                if let Some(set) = self.importers.get_mut(dep) {
                    set.remove(dependent);
                }
            }
        }
        // Insert new forward and reverse edges.
        for dep in &new_imports {
            self.importers
                .entry(dep.clone())
                .or_default()
                .insert(dependent.clone());
        }
        self.imports.insert(dependent.clone(), new_imports);
    }

    /// Return all files that directly import `file`.
    pub fn importers_of(&self, file: &FileId) -> impl Iterator<Item = &FileId> {
        self.importers
            .get(file)
            .into_iter()
            .flat_map(|set| set.iter())
    }

    /// Return all files that `file` imports (direct imports only).
    pub fn imports_of(&self, file: &FileId) -> impl Iterator<Item = &FileId> {
        self.imports.get(file).into_iter().flat_map(|v| v.iter())
    }

    /// Return all files known to the store (those whose text has been set).
    pub fn all_files(&self) -> impl Iterator<Item = &FileId> {
        self.file_texts.keys()
    }

    // ── Invalidation ──────────────────────────────────────────────────────────

    /// Invalidate all cached entries for `file`.
    ///
    /// Called when a file's text changes.  The entries are removed so that
    /// the next access will trigger re-execution.
    ///
    /// Note: the parse cache is NOT cleared here; it is checked for staleness
    /// via the content hash on access (`get_parse`).
    fn invalidate_file(&mut self, file: &FileId) {
        let keys_to_remove: Vec<QueryKey> = self
            .entries
            .keys()
            .filter(|k| k.file_id() == file)
            .cloned()
            .collect();
        for k in keys_to_remove {
            self.entries.remove(&k);
        }
    }

    /// Remove all state for a file (called on `didClose`).
    pub fn remove_file(&mut self, file: &FileId) {
        self.file_texts.remove(file);
        self.parse_cache.remove(file);
        self.invalidate_file(file);

        // Remove forward import edges and the corresponding reverse edges.
        if let Some(deps) = self.imports.remove(file) {
            for dep in &deps {
                if let Some(set) = self.importers.get_mut(dep) {
                    set.remove(file);
                }
            }
        }
        // Remove reverse import edges (file was being imported by others).
        self.importers.remove(file);
        // Also remove this file from others' forward import sets.
        for deps in self.imports.values_mut() {
            deps.retain(|f| f != file);
        }
    }
}

impl Default for QueryStore {
    fn default() -> Self {
        Self::new()
    }
}

// ── Re-verification helper ────────────────────────────────────────────────────

/// Re-verify a query against its stored input hash.
///
/// Returns `true` if the query is still valid (inputs unchanged); `false` if
/// the query must be re-executed.
///
/// This function checks whether the current `input_hash` (provided by the
/// caller, computed from the current dependency output hashes) matches the
/// stored input hash.  If it matches, the entry is marked as verified at the
/// current revision.
///
/// ## Usage
///
/// ```ignore
/// let current_input_hash = hash_combine(
///     store.file_text_hash(&file)?,
///     store.output_hash_of(&QueryKey::Parse(file.clone()))?,
/// );
/// if verify_entry(store, &QueryKey::Desugar(file.clone()), current_input_hash) {
///     // Use cached desugar result.
/// } else {
///     // Re-run desugar, store result.
/// }
/// ```
pub fn verify_entry(
    store: &mut QueryStore,
    key: &QueryKey,
    current_input_hash: ContentHash,
) -> bool {
    let stored = store.entries.get(key);
    match stored {
        Some(e) if e.input_hash == current_input_hash => {
            let rev = store.revision;
            store.entries.get_mut(key).unwrap().verified_at = rev;
            true
        }
        _ => false,
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn test_url(path: &str) -> Url {
        Url::parse(&format!("file://{path}")).unwrap()
    }

    fn file(path: &str) -> FileId {
        FileId::new(test_url(path))
    }

    // ── Content hashing ──────────────────────────────────────────────────────

    #[test]
    fn hash_str_deterministic() {
        let h1 = hash_str("hello world");
        let h2 = hash_str("hello world");
        assert_eq!(h1, h2);
    }

    #[test]
    fn hash_str_distinct() {
        assert_ne!(hash_str("foo"), hash_str("bar"));
    }

    #[test]
    fn hash_combine_deterministic() {
        let a = hash_str("a");
        let b = hash_str("b");
        assert_eq!(hash_combine(a, b), hash_combine(a, b));
    }

    #[test]
    fn hash_combine_order_sensitive() {
        let a = hash_str("a");
        let b = hash_str("b");
        // a XOR b is commutative — we intentionally make it non-commutative.
        // (The DefaultHasher feeds hash_combine in order, so a!=b implies different.)
        assert_ne!(hash_combine(a, b), hash_combine(b, a));
    }

    #[test]
    fn hash_many_empty() {
        // hash_many([]) should not panic.
        let _ = hash_many(&[]);
    }

    #[test]
    fn hash_many_single() {
        let h = hash_str("x");
        // hash_many([h]) == hash_combine(0, h) by the fold
        let expected = hash_combine(0, h);
        assert_eq!(hash_many(&[h]), expected);
    }

    // ── File text management ─────────────────────────────────────────────────

    #[test]
    fn set_file_text_new_returns_true() {
        let mut store = QueryStore::new();
        let f = file("/a.eu");
        let changed = store.set_file_text(f, Arc::new("x: 1".to_string()));
        assert!(changed);
    }

    #[test]
    fn set_file_text_identical_returns_false() {
        let mut store = QueryStore::new();
        let f = file("/a.eu");
        store.set_file_text(f.clone(), Arc::new("x: 1".to_string()));
        let changed = store.set_file_text(f, Arc::new("x: 1".to_string()));
        assert!(!changed);
    }

    #[test]
    fn set_file_text_different_returns_true() {
        let mut store = QueryStore::new();
        let f = file("/a.eu");
        store.set_file_text(f.clone(), Arc::new("x: 1".to_string()));
        let changed = store.set_file_text(f, Arc::new("x: 2".to_string()));
        assert!(changed);
    }

    #[test]
    fn revision_increments_on_change() {
        let mut store = QueryStore::new();
        let f = file("/a.eu");
        assert_eq!(store.revision(), 0);
        store.set_file_text(f.clone(), Arc::new("x: 1".to_string()));
        assert_eq!(store.revision(), 1);
        store.set_file_text(f.clone(), Arc::new("x: 2".to_string()));
        assert_eq!(store.revision(), 2);
        // Same text: no revision bump.
        store.set_file_text(f, Arc::new("x: 2".to_string()));
        assert_eq!(store.revision(), 2);
    }

    #[test]
    fn file_text_round_trips() {
        let mut store = QueryStore::new();
        let f = file("/b.eu");
        let text = Arc::new("y: 42".to_string());
        store.set_file_text(f.clone(), text.clone());
        let retrieved = store.file_text(&f).unwrap();
        assert_eq!(*retrieved, *text);
    }

    // ── Typed cache ──────────────────────────────────────────────────────────

    #[test]
    fn store_and_retrieve_typed_value() {
        let mut store = QueryStore::new();
        let f = file("/c.eu");
        store.set_file_text(f.clone(), Arc::new("z: 0".to_string()));
        let key = QueryKey::Desugar(f);
        store.store(key.clone(), 42u32, 0, 0, vec![]);
        let val: Option<&u32> = store.get_if_current(&key);
        assert_eq!(val, Some(&42u32));
    }

    #[test]
    fn cache_entry_stale_after_revision_bump() {
        let mut store = QueryStore::new();
        let f = file("/d.eu");
        store.set_file_text(f.clone(), Arc::new("a: 1".to_string()));
        let key = QueryKey::Desugar(f.clone());
        store.store(key.clone(), 99u32, 0, 0, vec![]);
        // Change the file → bump revision → entry is stale.
        store.set_file_text(f, Arc::new("a: 2".to_string()));
        let val: Option<&u32> = store.get_if_current(&key);
        assert!(val.is_none());
    }

    #[test]
    fn mark_verified_makes_entry_current() {
        let mut store = QueryStore::new();
        let f = file("/e.eu");
        store.set_file_text(f.clone(), Arc::new("b: 1".to_string()));
        let key = QueryKey::Desugar(f.clone());
        store.store(key.clone(), 7u32, 0, 0, vec![]);
        store.set_file_text(f, Arc::new("b: 2".to_string()));
        // Entry is now stale.
        assert!(store.get_if_current::<u32>(&key).is_none());
        // Mark verified at the new revision.
        store.mark_verified(&key);
        let val: Option<&u32> = store.get_if_current(&key);
        assert_eq!(val, Some(&7u32));
    }

    // ── Import graph ─────────────────────────────────────────────────────────

    #[test]
    fn importers_empty_initially() {
        let store = QueryStore::new();
        let f = file("/lib.eu");
        assert_eq!(store.importers_of(&f).count(), 0);
    }

    #[test]
    fn set_imports_records_reverse_edge() {
        let mut store = QueryStore::new();
        let a = file("/a.eu");
        let b = file("/b.eu");
        store.set_imports(&a, vec![b.clone()]);
        let importers: Vec<_> = store.importers_of(&b).cloned().collect();
        assert_eq!(importers, vec![a]);
    }

    #[test]
    fn set_imports_removes_old_edges() {
        let mut store = QueryStore::new();
        let a = file("/a.eu");
        let b = file("/b.eu");
        let c = file("/c.eu");
        store.set_imports(&a, vec![b.clone()]);
        // A now imports C instead of B.
        store.set_imports(&a, vec![c.clone()]);
        // B should no longer have A as an importer.
        assert_eq!(store.importers_of(&b).count(), 0);
        let importers_c: Vec<_> = store.importers_of(&c).collect();
        assert_eq!(importers_c.len(), 1);
    }

    #[test]
    fn multiple_importers() {
        let mut store = QueryStore::new();
        let lib = file("/lib.eu");
        let a = file("/a.eu");
        let b = file("/b.eu");
        store.set_imports(&a, vec![lib.clone()]);
        store.set_imports(&b, vec![lib.clone()]);
        let mut importers: Vec<_> = store.importers_of(&lib).cloned().collect();
        importers.sort_by_key(|f| f.uri.to_string());
        assert_eq!(importers.len(), 2);
    }

    #[test]
    fn remove_file_cleans_up() {
        let mut store = QueryStore::new();
        let a = file("/a.eu");
        let b = file("/b.eu");
        store.set_file_text(a.clone(), Arc::new("".to_string()));
        store.set_imports(&a, vec![b.clone()]);
        store.remove_file(&a);
        assert!(store.file_text(&a).is_none());
        assert_eq!(store.importers_of(&b).count(), 0);
    }

    // ── verify_entry ─────────────────────────────────────────────────────────

    #[test]
    fn verify_entry_matches_stored_hash() {
        let mut store = QueryStore::new();
        let f = file("/f.eu");
        store.set_file_text(f.clone(), Arc::new("".to_string()));
        let key = QueryKey::Cook(f);
        let input_h = hash_str("some-input");
        store.store(key.clone(), "result".to_string(), input_h, 0, vec![]);
        // Re-verify with the same hash — should succeed.
        assert!(verify_entry(&mut store, &key, input_h));
    }

    #[test]
    fn verify_entry_fails_on_different_hash() {
        let mut store = QueryStore::new();
        let f = file("/g.eu");
        store.set_file_text(f.clone(), Arc::new("".to_string()));
        let key = QueryKey::Cook(f);
        let input_h = hash_str("original");
        store.store(key.clone(), "result".to_string(), input_h, 0, vec![]);
        let new_h = hash_str("changed");
        assert!(!verify_entry(&mut store, &key, new_h));
    }

    // ── Diamond dependency ────────────────────────────────────────────────────

    #[test]
    fn diamond_import_graph() {
        // A and B both import Lib; C imports both A and B.
        // When Lib changes, all three files need re-checking.
        let mut store = QueryStore::new();
        let lib = file("/lib.eu");
        let a = file("/a.eu");
        let b = file("/b.eu");
        let c = file("/c.eu");

        store.set_imports(&a, vec![lib.clone()]);
        store.set_imports(&b, vec![lib.clone()]);
        store.set_imports(&c, vec![a.clone(), b.clone()]);

        let lib_importers: HashSet<FileId> = store.importers_of(&lib).cloned().collect();
        assert!(lib_importers.contains(&a));
        assert!(lib_importers.contains(&b));
        assert!(!lib_importers.contains(&c)); // C imports A/B, not lib directly

        let a_importers: HashSet<FileId> = store.importers_of(&a).cloned().collect();
        assert!(a_importers.contains(&c));
    }

    // ── Green node hashing ───────────────────────────────────────────────────

    #[test]
    fn green_node_hash_deterministic() {
        use crate::syntax::rowan::parse_unit;
        let p1 = parse_unit("x: 1");
        let p2 = parse_unit("x: 1");
        let h1 = hash_green_node(&p1.syntax_node().green().into_owned());
        let h2 = hash_green_node(&p2.syntax_node().green().into_owned());
        assert_eq!(h1, h2);
    }

    #[test]
    fn green_node_hash_differs_on_change() {
        use crate::syntax::rowan::parse_unit;
        let p1 = parse_unit("x: 1");
        let p2 = parse_unit("x: 2");
        let h1 = hash_green_node(&p1.syntax_node().green().into_owned());
        let h2 = hash_green_node(&p2.syntax_node().green().into_owned());
        assert_ne!(h1, h2);
    }
}
