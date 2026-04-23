//! Desugarer maintains state during desugar passes.
use super::desugarable::{Content, Desugarable};
use crate::{
    common::{
        environment::DefaultingEnvironment,
        sourcemap::{Smid, SourceMap},
    },
    core::{
        binding::Var, doc::DeclarationDocumentation, error::CoreError, expr::*, target::*,
        unit::TranslationUnit,
    },
    syntax::input::*,
};
use codespan::Span;
use std::collections::{HashMap, HashSet};

/// A monad specification registered for a bracket pair or used inline in block metadata.
///
/// Describes how monadic blocks are desugared:
/// - `Explicit` names the bind and return functions directly.
/// - `Namespace` refers to a block in scope that must have `bind` and `return` members.
#[derive(Debug, Clone)]
pub enum MonadSpec {
    /// Explicit bind/return function names (e.g. from `{ :monad bind: f return: r }`).
    Explicit {
        /// Name of the monadic bind function (e.g. `"list-bind"`)
        bind_name: String,
        /// Name of the monadic return function (e.g. `"list-return"`)
        return_name: String,
    },
    /// Namespace reference: the monad is a block in scope with `bind` and `return` members.
    /// Emits `name.bind(value, lambda)` and `name.return(expr)` calls.
    Namespace(String),
}

/// State kept during desugaring pass
pub struct Desugarer<'smap> {
    /// While in the scope of anaphoric string pattern, collect all the
    /// anaphora for processing at the boundary the pattern
    pending_string_anaphora: HashMap<Anaphor<Smid, i32>, String>,
    /// Targets discovered (including those from imported files)
    targets: HashSet<Target>,
    /// Targets that originated from imported files rather than the
    /// top-level unit being desugared.  Used to compute `own_targets`
    /// in [`Self::translate_unit`].
    imported_targets: HashSet<Target>,
    /// Doc strings discovered (path, doc)
    docs: Vec<DeclarationDocumentation>,
    /// Stack of names
    stack: Vec<String>,
    /// All parsed content for translation
    contents: &'smap HashMap<Input, Content<'smap>>,
    /// SourceMap
    source_map: &'smap mut SourceMap,
    /// Track variable names so we mint appropriate names
    env: DefaultingEnvironment<String, String>,
    /// Current usize
    file: Vec<usize>,
    /// Registry mapping bracket pair names (e.g. `"⟦⟧"`) to their monad spec.
    ///
    /// Populated when a bracket pair declaration with `bind` and `return` metadata
    /// is desugared.  Consulted when desugaring `⟦ { ... } ⟧` block expressions.
    monad_registry: HashMap<String, MonadSpec>,
    /// Registry mapping namespace names (e.g. `"io"`) to their monad spec.
    ///
    /// Populated when a top-level declaration carries `monad: true` metadata.
    /// Consulted when desugaring `{ :ns ... }` blocks without an explicit
    /// `.expr` return — only registered namespaces trigger implicit return.
    monad_namespace_registry: HashMap<String, MonadSpec>,
    /// Registry mapping monad names to their wrapper type string for type
    /// checking (e.g. `"for" → "[a]"`, `"io" → "IO(a)"`).
    ///
    /// Populated from `monad: "<type>"` metadata on namespace or bracket pair
    /// declarations.  Consulted during monadic block desugaring to inject
    /// `__type_hint` metadata on binding values.
    monad_type_registry: HashMap<String, String>,
    /// Import guard — direct imports (depth == 1, i.e. imported directly
    /// by the top-level unit being desugared).
    ///
    /// Files in this set are NOT deduplicated when re-imported directly again
    /// (because a file may intentionally be imported at multiple positions in the
    /// same unit to bring its bindings into different scopes).  They ARE
    /// suppressed if encountered again as a *nested* import (depth > 1), which
    /// is the classic diamond-dependency case.
    ///
    /// The key is `(file_id, import_name)`.
    import_seen_direct: HashSet<(usize, Option<String>)>,
    /// Import guard — nested (transitive) imports (depth > 1).
    ///
    /// Files in this set are suppressed unconditionally on any subsequent
    /// encounter, whether direct or nested.  This handles the case where a
    /// transitive dependency is pulled in first and a later direct import of
    /// the same file would create a duplicate.
    import_seen_nested: HashSet<(usize, Option<String>)>,
    /// Rule 3 — alias table for same-file-different-name direct imports.
    ///
    /// When a file is imported directly (depth == 1) under a name for the
    /// first time, its `(file_id → name)` is recorded here.  A subsequent
    /// direct import of the same file under a *different* name emits an
    /// alias binding `let { new_name = old_name }` instead of re-desugaring.
    import_first_direct: HashMap<usize, String>,
}

impl<'smap> Desugarer<'smap> {
    /// Construct a Desugarer from a SourceMap and the desugarable contents
    #[allow(clippy::redundant_closure)]
    pub fn new(
        contents: &'smap HashMap<Input, Content<'smap>>,
        source_map: &'smap mut SourceMap,
    ) -> Self {
        Desugarer {
            pending_string_anaphora: HashMap::new(),
            targets: HashSet::new(),
            imported_targets: HashSet::new(),
            docs: Vec::new(),
            stack: vec![],
            contents,
            source_map,
            env: DefaultingEnvironment::new(|k: &String| k.clone()),
            file: vec![],
            monad_registry: HashMap::new(),
            monad_namespace_registry: HashMap::new(),
            monad_type_registry: HashMap::new(),
            import_seen_direct: HashSet::new(),
            import_seen_nested: HashSet::new(),
            import_first_direct: HashMap::new(),
        }
    }

    /// Register a monad spec for a bracket pair.
    ///
    /// Called when a bracket pair declaration with `bind` and `return` metadata
    /// is processed so that subsequent monadic block usages can look up the spec.
    pub fn register_monad_spec(&mut self, pair_name: String, spec: MonadSpec) {
        self.monad_registry.insert(pair_name, spec);
    }

    /// Look up the monad spec for a bracket pair, if one has been registered.
    pub fn monad_spec(&self, pair_name: &str) -> Option<&MonadSpec> {
        self.monad_registry.get(pair_name)
    }

    /// Return `true` if the desugarer is currently at the top level of a unit
    /// (i.e., the name stack is empty — no outer declaration has been pushed).
    pub fn is_top_level(&self) -> bool {
        self.stack.is_empty()
    }

    /// Register a monad spec for a namespace name.
    ///
    /// Called when a top-level declaration carrying `monad: true` metadata is
    /// processed so that `{ :ns ... }` blocks without `.expr` can use implicit
    /// return semantics.
    pub fn register_monad_namespace(&mut self, name: String, spec: MonadSpec) {
        self.monad_namespace_registry.insert(name, spec);
    }

    /// Look up the monad spec for a namespace name, if one has been registered.
    pub fn monad_namespace_spec(&self, name: &str) -> Option<&MonadSpec> {
        self.monad_namespace_registry.get(name)
    }

    /// Seed the monad namespace registry with entries from a previous
    /// translation unit (e.g. the prelude).
    pub fn seed_monad_namespace_registry(&mut self, registry: &HashMap<String, MonadSpec>) {
        self.monad_namespace_registry.extend(registry.clone());
    }

    /// Drain the monad namespace registry for persistence across units.
    pub fn drain_monad_namespace_registry(&mut self) -> HashMap<String, MonadSpec> {
        std::mem::take(&mut self.monad_namespace_registry)
    }

    /// Register the monadic wrapper type for a monad name.
    pub fn register_monad_type(&mut self, name: String, type_str: String) {
        self.monad_type_registry.insert(name, type_str);
    }

    /// Look up the monadic wrapper type for a monad name.
    pub fn monad_type(&self, name: &str) -> Option<&str> {
        self.monad_type_registry.get(name).map(|s| s.as_str())
    }

    /// Seed the monad type registry from a previous unit.
    pub fn seed_monad_type_registry(&mut self, registry: &HashMap<String, String>) {
        self.monad_type_registry.extend(registry.clone());
    }

    /// Drain the monad type registry for persistence across units.
    pub fn drain_monad_type_registry(&mut self) -> HashMap<String, String> {
        std::mem::take(&mut self.monad_type_registry)
    }

    /// Desugar content at locator (and imports) to create a new
    /// translation unit.
    pub fn translate_unit(&mut self, input: &Input) -> Result<TranslationUnit, CoreError> {
        if let Some(source) = self.contents.get(input) {
            self.file.push(source.file_id());
            let mut expr = source.content().desugar(self)?;

            // A lone name at the top level won't have passed through
            // any desugaring context that varifies it, so handle it
            // explicitly here.
            if expr.inner.is_name() {
                expr = self.varify(expr);
            }

            if let Some(name) = input.name() {
                expr = expr.apply_name(Smid::default(), name);
            };

            // `own_targets` contains only the targets declared in this
            // file itself — targets from imported files are collected
            // separately in `self.imported_targets` by `translate_import`.
            let own_targets = self
                .targets
                .difference(&self.imported_targets)
                .cloned()
                .collect();

            let unit = TranslationUnit {
                expr,
                targets: self.targets.clone(),
                own_targets,
                docs: self.docs.clone(),
            };
            self.file.pop();
            Ok(unit)
        } else {
            Err(CoreError::NoParsedAstFor(Box::new(input.clone())))
        }
    }

    /// Used during translation to switch context and desugar an
    /// import.
    ///
    /// Any targets registered whilst desugaring the import are added to
    /// both `self.targets` (so that `-t target-name` can still invoke
    /// them) and `self.imported_targets` (so that [`Self::translate_unit`]
    /// can exclude them from `own_targets`, preventing the test runner
    /// from auto-discovering targets that belong to imported files).
    ///
    /// Returns `Ok(None)` when the import is suppressed by the import
    /// guard (diamond-deduplication semantics):
    ///
    /// - A file first seen **transitively** (depth > 1) is suppressed on
    ///   any subsequent encounter (direct or nested).
    /// - A file first seen **directly** (depth == 1) is suppressed only
    ///   when encountered again as a nested import.  Multiple direct
    ///   imports of the same file are allowed because each may serve a
    ///   different scope position in the unit.
    ///
    /// **Rule 3**: when a file is imported directly a second time under a
    /// *different* name, an alias binding is emitted instead of re-desugaring.
    /// For example, if `lens=lens.eu` was already imported, a subsequent
    /// `optics=lens.eu` emits `let { optics = lens }` referencing the
    /// already-bound name.
    pub fn translate_import(
        &mut self,
        smid: Smid,
        import: Input,
    ) -> Result<Option<RcExpr>, CoreError> {
        // Look up content by exact Input key, falling back to locator match.
        // The fallback is needed when the same file is imported under multiple
        // names (eu-v0t6): the ImportGraph deduplicates by locator, so only the
        // first-registered Input is in the contents map.
        let source_lookup = self.contents.get(&import).or_else(|| {
            self.contents
                .iter()
                .find(|(k, _)| k.locator() == import.locator())
                .map(|(_, v)| v)
        });
        if let Some(source) = source_lookup {
            let file_id = source.file_id();
            let import_name = import.name().clone();
            let guard_key = (file_id, import_name.clone());

            // `file.len()` is the current nesting depth:
            //   1 = direct import from the top-level unit
            //   2+ = nested/transitive import
            let is_nested = self.file.len() > 1;

            // Rule 3: if this file was already imported *directly* under a
            // different name, emit an alias binding rather than re-desugaring.
            // This avoids duplicating content and prevents nested imports from
            // being incorrectly suppressed on the second pass.
            if !is_nested {
                if let Some(first_name) = self.import_first_direct.get(&file_id).cloned() {
                    if let Some(new_name) = &import_name {
                        if &first_name != new_name {
                            let alias_rhs =
                                RcExpr::from(Expr::Var(smid, Var::Free(first_name.clone())));
                            return Ok(Some(alias_rhs.apply_name(smid, new_name)));
                        }
                    }
                }
            }

            // Suppress if already seen transitively, OR if already seen
            // directly and we're now inside a nested import.
            if self.import_seen_nested.contains(&guard_key)
                || (is_nested && self.import_seen_direct.contains(&guard_key))
            {
                return Ok(None);
            }

            // Record this import in the appropriate guard set.
            if is_nested {
                self.import_seen_nested.insert(guard_key);
            } else {
                self.import_seen_direct.insert(guard_key);
                // Record the first direct named import for Rule 3 alias resolution.
                if let Some(ref name) = import_name {
                    self.import_first_direct
                        .entry(file_id)
                        .or_insert_with(|| name.clone());
                }
            }

            self.file.push(file_id);

            // Snapshot targets before desugaring the import so we can
            // identify which targets were added by the imported file.
            let targets_before = self.targets.clone();
            let mut expr = source.content().desugar(self)?;

            // Any targets not present before this import are "imported" —
            // record them so `translate_unit` can exclude them from
            // `own_targets`.
            let newly_added = self.targets.difference(&targets_before).cloned();
            self.imported_targets.extend(newly_added);

            if let Some(name) = import.name() {
                expr = expr.apply_name(smid, name);
            }

            self.file.pop();
            Ok(Some(expr))
        } else {
            Err(CoreError::NoParsedAstFor(Box::new(import.clone())))
        }
    }

    /// Translate a chunk of parsed AST that has no imports
    pub fn translate_simple(
        &mut self,
        file_id: usize,
        ast: &impl Desugarable,
    ) -> Result<RcExpr, CoreError> {
        self.file.push(file_id);
        ast.desugar(self)
    }

    /// Reference to the name to variable name environment
    pub fn env(&self) -> &DefaultingEnvironment<String, String> {
        &self.env
    }

    /// Mutable reference to the name to variable name environment
    pub fn env_mut(&mut self) -> &mut DefaultingEnvironment<String, String> {
        &mut self.env
    }

    /// Record source position and mint a SMID for it.
    ///
    /// If we are inside a declaration (stack is non-empty), the SMID
    /// is automatically annotated with the current declaration name so
    /// that stack traces show function names instead of source snippets.
    pub fn new_smid(&mut self, span: Span) -> Smid {
        let file = *self.file.last().unwrap();
        if let Some(name) = self.stack.last() {
            self.source_map.add_annotated(file, span, name.clone())
        } else {
            self.source_map.add(file, span)
        }
    }

    /// Record an annotated source position and mint a SMID for it
    pub fn new_annotated_smid<T: AsRef<str>>(&mut self, span: Span, annotation: T) -> Smid {
        self.source_map
            .add_annotated(*self.file.last().unwrap(), span, annotation)
    }

    /// True iff we are in the main module, not an import
    pub fn in_base_file(&self) -> bool {
        self.file.len() == 1
    }

    /// True if there are string anaphora pending
    pub fn has_pending_string_anaphora(&self) -> bool {
        !self.pending_string_anaphora.is_empty()
    }

    /// Clear any pending string anaphora
    pub fn clear_pending_string_anaphora(&mut self) {
        self.pending_string_anaphora.clear();
    }

    /// Reference to the pending string anaphora for calculating binders
    pub fn pending_string_anaphora(&self) -> &HashMap<Anaphor<Smid, i32>, String> {
        &self.pending_string_anaphora
    }

    /// Add a pending string anaphor
    pub fn add_pending_string_anaphor(&mut self, anaphor: Anaphor<Smid, i32>) -> String {
        let name = free(&format!("{anaphor}"));
        self.pending_string_anaphora.insert(anaphor, name.clone());
        name
    }

    /// Return the appropriate var name to use for `name` in this scope
    pub fn var(&mut self, name: &str) -> String {
        self.env.encounter(name.to_string());
        self.env.get(&name.to_string()).unwrap().clone()
    }

    /// Used in contexts where we are desugaring something "initial"...
    /// i.e. something which is not somewhere in op soup but at the start
    /// of an expression, where names cannot be names but must be
    /// variables.
    ///
    /// The default desugaring leaves names as names to leave soup
    /// desugaring in control of lookup subtleties.
    pub fn varify(&mut self, expr: RcExpr) -> RcExpr {
        match &*expr.inner {
            Expr::Name(smid, name) => {
                let var_name = self.var(name);
                RcExpr::from(Expr::Var(*smid, Var::Free(var_name)))
            }
            _ => expr,
        }
    }

    /// Push a name component on entering a block, to keep track of
    /// locations for defining targets.
    pub fn push(&mut self, name: &str) {
        self.stack.push(String::from(name))
    }

    /// Pop a name component
    pub fn pop(&mut self) {
        self.stack.pop();
    }

    /// Record a target discovered in the desugaring process
    pub fn record_target(
        &mut self,
        target: String,
        doc: String,
        fmt: Option<String>,
        validations: Vec<String>,
    ) {
        let tgt = Target::new(
            target,
            doc,
            fmt,
            self.stack.iter().map(String::clone).collect(),
            validations,
        );
        self.targets.insert(tgt);
    }

    /// Record doc strings discovered during desugaring
    pub fn record_doc(&mut self, doc: String, name: &str, args: Vec<String>) {
        self.docs.push(DeclarationDocumentation {
            name: name.to_string(),
            args,
            path: self.stack.iter().map(String::clone).collect(),
            doc,
        });
    }
}
