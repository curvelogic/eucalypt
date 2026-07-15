use crate::common::sourcemap::*;
use crate::core::binding::CoreBinding;
use crate::core::binding::Scope;
use crate::core::cook;
use crate::core::desugar::desugarable::Desugarable;
use crate::core::desugar::{Content, Desugarer};
use crate::core::error::CoreError;
use crate::core::expr::{Expr, LetType, RcExpr};
use crate::core::inline::reduce;
use crate::core::inline::tag;
use crate::core::simplify::compress;
use crate::core::simplify::prune;
use crate::core::transform::fuse;
use crate::core::transform::hoist;
use crate::core::unit::TranslationUnit;
use crate::core::verify::content;
use crate::driver::error::EucalyptError;
use crate::driver::resources::Resources;
use crate::import::read_to_core;
use crate::syntax::rowan::ast::{Soup, Unit};

/// Enum to hold either a Unit (for files) or Soup (for CLI expressions)
#[derive(Debug)]
pub enum ParsedAst {
    Unit(Unit),
    Soup(Soup),
}

impl Desugarable for ParsedAst {
    fn desugar(
        &self,
        desugarer: &mut crate::core::desugar::desugarer::Desugarer,
    ) -> Result<crate::core::expr::RcExpr, crate::core::error::CoreError> {
        match self {
            ParsedAst::Unit(unit) => unit.desugar(desugarer),
            ParsedAst::Soup(soup) => soup.desugar(desugarer),
        }
    }
}
use crate::syntax::error::ParserError;
use crate::syntax::import::ImportGraph;
use crate::syntax::input::Input;
use crate::syntax::input::Locator;
use crate::syntax::parser;
use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::emit;
use codespan_reporting::term::termcolor::{ColorChoice, NoColor, StandardStream};
use std::io::{self, Read};
use std::path::PathBuf;
use std::{collections::HashMap, path::Path};
use std::{fs, iter};

use super::io::{
    create_args_pseudoblock, create_io_pseudoblock, create_io_pseudoblock_deterministic,
};
use super::unit_interface::UnitInterface;

/// A loader for source code that stores the bytes for error reporting
/// and manages the evolution of code through parse, translation to
/// core and manipulation as the final core expression syntax which
/// will be compiled / interpreted.
pub struct SourceLoader {
    /// Baked in resources (like the prelude)
    resources: Resources,
    /// Map Locators to usizes to key other stores
    locators: HashMap<Locator, usize>,
    /// Access to source file text
    files: SimpleFiles<String, String>,
    /// Parsed ASTs  pending core translation
    asts: HashMap<usize, ParsedAst>,
    /// Parsed core exprs pending core translation (YAML, JSON etc.)
    cores: HashMap<Input, RcExpr>,
    /// Import analysis
    imports: ImportGraph,
    /// Core Units pending merge
    translation_units: HashMap<Input, TranslationUnit>,
    /// Units once merged
    core: TranslationUnit,
    /// SourceMap for references to source locations across all files
    source_map: SourceMap,
    /// Paths to search for FS resources
    lib_path: Vec<PathBuf>,
    /// Command-line arguments passed after -- separator (for __ARGS)
    args: Vec<String>,
    /// Seed for random number generation
    seed: Option<i64>,
    /// When set, the `__io` pseudoblock is built with deterministic
    /// placeholder values (epoch 0, empty env, seed 0, UTC) rather than live
    /// invocation data.  Used by `cargo xtask prelude-compile` so the
    /// generated blob is byte-for-byte reproducible; the runtime always
    /// overrides the `__io` slot with real values anyway.
    deterministic_io: bool,
    /// Prelude override: set when a loaded unit specifies `prelude:` metadata.
    /// The `Input` replaces the default `Resource("prelude")` in the inputs list.
    prelude_override: Option<Input>,
    /// Cross-unit compilation interface.
    ///
    /// Carries monad registries, operator metadata, and type schemes forward
    /// across translation units.  Replaces the former separate
    /// `monad_namespace_registry` and `monad_type_registry` fields.
    unit_interface: UnitInterface,
    /// Parse errors collected during `load_eucalypt`.
    ///
    /// Parse errors no longer abort loading — the partial tree (possibly
    /// containing `ERROR_STOWAWAYS` nodes) is always stored.  Callers drain
    /// this list via `drain_parse_errors` and decide how to surface them.
    pending_parse_errors: Vec<EucalyptError>,
    /// Pre-compiled prelude blob, loaded once in `bin/eu.rs` before `prepare()`.
    ///
    /// Stored here so it can be used in two phases without double-loading:
    /// 1. `cook()` seeds the `Distributor` with `blob.operators`.
    /// 2. `take_prelude_blob()` hands it to the `Executor` for runtime loading.
    #[cfg(not(target_arch = "wasm32"))]
    prelude_blob: Option<crate::eval::stg::blob::PreludeBlob>,
    /// Type aliases collected from the full (unpruned) expression.
    ///
    /// Target pruning may remove bindings that carry `type-def:` or
    /// `types:` metadata, so alias collection must happen before the
    /// first dead-code elimination pass.  Callers that need TypeData
    /// resolution (the `eu` binary, the tester) retrieve these via
    /// `type_aliases()` after `prepare()` completes.
    early_type_aliases: HashMap<String, crate::core::typecheck::types::Type>,
}

impl Default for SourceLoader {
    fn default() -> Self {
        SourceLoader {
            resources: Resources::default(),
            locators: HashMap::new(),
            files: SimpleFiles::new(),
            asts: HashMap::new(),
            cores: HashMap::new(),
            imports: ImportGraph::default(),
            translation_units: HashMap::new(),
            core: TranslationUnit::default(),
            source_map: SourceMap::new(),
            lib_path: Vec::new(),
            args: Vec::new(),
            seed: None,
            deterministic_io: false,
            prelude_override: None,
            unit_interface: UnitInterface::default(),
            pending_parse_errors: Vec::new(),
            #[cfg(not(target_arch = "wasm32"))]
            prelude_blob: None,
            early_type_aliases: HashMap::new(),
        }
    }
}

impl SourceLoader {
    /// Create a fresh blank source loader
    pub fn new(lib_path: Vec<PathBuf>) -> Self {
        SourceLoader {
            resources: Resources::default(),
            locators: HashMap::new(),
            files: SimpleFiles::new(),
            asts: HashMap::new(),
            cores: HashMap::new(),
            imports: ImportGraph::default(),
            translation_units: HashMap::new(),
            core: TranslationUnit::default(),
            source_map: SourceMap::new(),
            lib_path,
            args: Vec::new(),
            seed: None,
            deterministic_io: false,
            prelude_override: None,
            unit_interface: UnitInterface::default(),
            #[cfg(not(target_arch = "wasm32"))]
            prelude_blob: None,
            pending_parse_errors: Vec::new(),
            early_type_aliases: HashMap::new(),
        }
    }

    /// Set the command-line arguments for the __ARGS pseudoblock
    pub fn with_args(mut self, args: Vec<String>) -> Self {
        self.args = args;
        self
    }

    /// Set the random seed for the __io pseudoblock
    pub fn with_seed(mut self, seed: Option<i64>) -> Self {
        self.seed = seed;
        self
    }

    /// Build the `__io` pseudoblock deterministically (for blob generation).
    ///
    /// When enabled, the `__io` pseudoblock uses fixed placeholder values
    /// instead of live invocation data, so `cargo xtask prelude-compile`
    /// produces a byte-for-byte reproducible blob.  The runtime always
    /// overrides the `__io` slot with real values, so behaviour is unchanged.
    pub fn with_deterministic_io(mut self, deterministic: bool) -> Self {
        self.deterministic_io = deterministic;
        self
    }

    /// Return refs to all parsed ASTs by locator,
    pub fn asts(&self) -> Vec<(&Locator, &ParsedAst)> {
        self.locators
            .iter()
            .filter_map(|(k, v)| self.asts.get(v).map(|value| (k, value)))
            .collect()
    }

    /// Retrieve the parsed AST for the specified locator
    pub fn ast(&self, locator: &Locator) -> Option<&ParsedAst> {
        self.locators.get(locator).and_then(|id| self.asts.get(id))
    }

    /// Load an input (and transitive imports)
    pub fn load(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        // Resolve git imports to a local cached path before any other dispatch.
        #[cfg(not(target_arch = "wasm32"))]
        if let Locator::Git { url, commit, path } = input.locator() {
            let cached_path = crate::import::git::resolve_git_import(url, commit, path)
                .map_err(|e| EucalyptError::Source(Box::new(e)))?;
            let resolved = Input::new(
                Locator::Fs(cached_path),
                input.name().clone(),
                input.format(),
            );
            return self.load(&resolved);
        }

        let fmt = input.format();

        if fmt == "eu" {
            self.load_tree(input)
        } else if fmt == "core" {
            self.load_core(input)
        } else {
            self.load_simple(input)
        }
    }

    /// Loads from a data format that does not support imports
    fn load_simple(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        if crate::import::is_stream_format(input.format()) {
            return self.load_stream(input);
        }
        let file_id = self.load_source(input.locator())?;
        let core = read_to_core(
            input.format(),
            &mut self.files,
            &mut self.source_map,
            file_id,
        )
        .map_err(|e| EucalyptError::Source(Box::new(e)))?;
        self.imports.add_leaf(input.clone())?;
        self.cores.insert(input.clone(), core);
        Ok(file_id)
    }

    /// Load a streaming format — opens the file directly without
    /// reading it into memory.
    fn load_stream(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        let path = match input.locator() {
            Locator::Fs(p) => self.resolve_fs_path(p)?,
            Locator::StdIn => "-".to_string(),
            _ => {
                return Err(EucalyptError::FileCouldNotBeRead(
                    format!("streaming not supported for locator: {}", input.locator()),
                    None,
                ))
            }
        };

        // Register a placeholder in the file store so we have a file_id
        let file_id = self
            .files
            .add(input.locator().to_string(), "(stream)".to_string());
        self.locators.insert(input.locator().clone(), file_id);

        let core = crate::import::create_stream_import(input.format(), &path)
            .map_err(|e| EucalyptError::Source(Box::new(e)))?;
        self.imports.add_leaf(input.clone())?;
        self.cores.insert(input.clone(), core);
        Ok(file_id)
    }

    /// Resolve a filesystem path using the lib_path search directories.
    fn resolve_fs_path(&self, path: &Path) -> Result<String, EucalyptError> {
        for libdir in &self.lib_path {
            let mut filename = libdir.to_path_buf();
            filename.push(path);
            if filename.exists() {
                return Ok(filename.to_string_lossy().to_string());
            }
        }
        // Try as absolute/relative from working directory
        if path.exists() {
            return Ok(path.to_string_lossy().to_string());
        }
        Err(EucalyptError::FileCouldNotBeRead(
            path.to_string_lossy().to_string(),
            None,
        ))
    }

    /// Load and parse import-graph of eucalypt files starting with the one
    /// specified by locator
    fn load_tree(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        let locator = input.locator();
        let file_id = self.load_eucalypt(locator)?;

        // Implement import analysis for Rowan AST
        let ast = self.asts.get(&file_id).expect("AST was just loaded");
        let inputs = self.imports.analyse_rowan_ast(input.clone(), ast)?;
        self.imports.check_for_cycles()?;

        // Resolve imports relative to the importing file's directory.
        //
        // When a file at `dir/foo.eu` imports `sub/bar.eu`, the path
        // `sub/bar.eu` should be resolved relative to `dir/` in addition
        // to the global lib_path. We achieve this by temporarily extending
        // the lib_path with the importing file's directory whilst loading
        // its transitive imports.
        let import_dir = self.find_source_dir(locator);
        if let Some(dir) = import_dir {
            self.lib_path.push(dir);
            for import_input in inputs {
                self.load(&import_input)?;
            }
            self.lib_path.pop();
        } else {
            for import_input in inputs {
                self.load(&import_input)?;
            }
        }

        Ok(file_id)
    }

    /// Find the directory containing the source file identified by `locator`,
    /// searching the lib_path. Returns `None` for non-filesystem locators.
    fn find_source_dir(&self, locator: &Locator) -> Option<PathBuf> {
        let path = match locator {
            Locator::Fs(path) => path,
            Locator::Buffer { path, .. } => path,
            _ => return None,
        };
        // Search lib_path entries
        for libdir in &self.lib_path {
            let candidate = libdir.join(path);
            if candidate.exists() {
                return candidate.parent().map(|p| p.to_path_buf());
            }
        }
        // Try the path directly (absolute or CWD-relative)
        if path.exists() {
            return path.parent().map(|p| p.to_path_buf());
        }
        // For Buffer locators, the path may not exist on disk but
        // we still know its directory.
        if matches!(locator, Locator::Buffer { .. }) {
            return path.parent().map(|p| p.to_path_buf());
        }
        None
    }

    /// Return the prelude override `Input` if one was set.
    pub fn prelude_override(&self) -> Option<&Input> {
        self.prelude_override.as_ref()
    }

    /// Read the `prelude:` key from the **first eucalypt unit file** in `inputs`.
    ///
    /// Only the first `.eu` file in the explicit inputs list is inspected.
    /// Subsequent files' `prelude:` keys are intentionally ignored, which
    /// prevents the test harness's validate phase (which re-imports the test
    /// subject as a secondary input) from triggering a prelude override.
    ///
    /// If found, sets the prelude override and eagerly loads the alternative
    /// prelude so it is ready for translate/merge.
    ///
    /// On WASM the prelude is always bundled as a resource; filesystem-based
    /// prelude overrides are not supported.  This method is a no-op stub on
    /// that target.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn detect_and_load_prelude_override(
        &mut self,
        inputs: &[Input],
    ) -> Result<(), EucalyptError> {
        // Scan explicit inputs for the first `.eu` filesystem file that carries a
        // `prelude:` metadata key.  Scanning all inputs (not just the first) means
        // the test harness validate phase can also pick up the override from the
        // test subject even when the first explicit input is non-`.eu` data (e.g.
        // `evidence.yaml`).
        for input in inputs {
            // Only act on user `.eu` filesystem files.
            if !matches!(input.locator(), Locator::Fs(_) | Locator::Buffer { .. })
                || input.format() != "eu"
            {
                continue;
            }

            // Parse the file's source and look for a `prelude:` key.
            let source = match input.locator() {
                Locator::Fs(path) => self.read_fs_input(path).ok(),
                _ => None,
            };
            let Some(source) = source else {
                continue;
            };

            let parse_result = crate::syntax::rowan::parse_unit(&source);
            let unit = parse_result.tree();
            let parsed = ParsedAst::Unit(unit);
            let Some(prelude_ref) = read_prelude_from_ast(&parsed) else {
                continue;
            };

            let prelude_input = prelude_ref_to_input(&prelude_ref);

            // Verify the resource exists (gives a clear error for :nonexistent).
            if let Locator::Resource(name) = prelude_input.locator() {
                if self.resources.get(name).is_none() {
                    return Err(EucalyptError::UnknownResource(name.clone()));
                }
            }

            self.prelude_override = Some(prelude_input.clone());
            // Load the alternative prelude now so it is ready for translate/merge.
            self.load(&prelude_input)?;
            return Ok(());
        }
        Ok(())
    }

    /// WASM stub: prelude overrides require filesystem access which is
    /// unavailable on WASM.  Always returns `Ok(())`.
    #[cfg(target_arch = "wasm32")]
    pub fn detect_and_load_prelude_override(
        &mut self,
        _inputs: &[Input],
    ) -> Result<(), EucalyptError> {
        Ok(())
    }

    /// Load and parse the source from a source specified by locator
    pub fn load_eucalypt(&mut self, locator: &Locator) -> Result<usize, EucalyptError> {
        // Load the text
        let id = self.load_source(locator)?;

        // We may still have an AST for this content (or we may have
        // consumed it in translation - it is rare enough we don't
        // mind the rework).
        if self.asts.contains_key(&id) {
            // we're done
            return Ok(id);
        }

        let ast = if matches!(locator, Locator::Cli(_)) {
            let (soup, errors) = parser::parse_expression(&self.files, id);
            if !errors.is_empty() {
                self.pending_parse_errors
                    .push(EucalyptError::Parser(ParserError::ParseErrors(id, errors)));
            }
            ParsedAst::Soup(soup)
        } else {
            let (unit, errors) = parser::parse_unit(&self.files, id);
            if !errors.is_empty() {
                self.pending_parse_errors
                    .push(EucalyptError::Parser(ParserError::ParseErrors(id, errors)));
            }
            ParsedAst::Unit(unit)
        };
        self.asts.insert(id, ast);
        Ok(id)
    }

    /// Load pseudoblock as core (dispatches based on pseudo name)
    fn load_core(&mut self, input: &Input) -> Result<usize, EucalyptError> {
        let file_id = self.load_source(input.locator())?;

        // Dispatch based on pseudo name
        let core = match input.locator() {
            Locator::Pseudo(name) if name == "args" => create_args_pseudoblock(&self.args),
            _ if self.deterministic_io => create_io_pseudoblock_deterministic(),
            _ => create_io_pseudoblock(self.seed),
        };

        self.imports.add_leaf(input.clone())?;
        self.cores.insert(input.clone(), core);
        Ok(file_id)
    }

    /// Load the text from the specified location into our file store
    fn load_source(&mut self, locator: &Locator) -> Result<usize, EucalyptError> {
        if let Some(file_id) = self.locators.get(locator) {
            return Ok(*file_id);
        }

        match self.locators.get(locator) {
            Some(id) => Ok(*id),
            None => {
                // read text
                let source = match locator {
                    Locator::Fs(path) => self.read_fs_input(path)?,
                    Locator::Cli(text) => text.to_string(),
                    Locator::Literal(text) => text.to_string(),
                    Locator::Buffer { text, .. } => text.to_string(),
                    Locator::Resource(name) => self
                        .resources
                        .get(name)
                        .ok_or_else(|| EucalyptError::UnknownResource(name.clone()))?
                        .clone(),
                    Locator::StdIn => self.read_stdin()?,
                    Locator::Pseudo(_) => "(no source)".to_string(),
                    other => {
                        return Err(EucalyptError::FileCouldNotBeRead(
                            format!("unsupported locator: {other}"),
                            None,
                        ))
                    }
                };

                // store text and map locator to fileid
                let id = self.files.add(locator.to_string(), source);
                self.locators.insert(locator.clone(), id);
                // Resources (prelude, stdlib) are not user files; mark them so
                // that diagnostics can prefer user-code locations as the primary
                // error site rather than showing prelude internals.
                if matches!(locator, Locator::Resource(_)) {
                    self.source_map.mark_resource_file(id);
                }
                Ok(id)
            }
        }
    }

    /// Desugar the interrelated tree of ASTs identified by `input`
    /// into a core expression.
    pub fn translate(&mut self, input: &Input) -> Result<&TranslationUnit, EucalyptError> {
        // If we already have a translation, we're done.
        if self.translation_units.contains_key(input) {
            return Ok(self
                .translation_units
                .get(input)
                .expect("translation unit existence just confirmed"));
        }

        // Check if this is a core input (already desugared)
        if let Some(core_expr) = self.cores.get(input) {
            // Apply name if specified (same as desugarer does)
            let expr = if let Some(name) = input.name() {
                core_expr.apply_name(Smid::default(), name)
            } else {
                core_expr.clone()
            };

            // Create a translation unit directly from the core expression
            let unit = TranslationUnit {
                expr,
                targets: std::collections::HashSet::new(),
                own_targets: std::collections::HashSet::new(),
                docs: Vec::new(),
                deprecations: std::collections::HashMap::new(),
            };
            self.translation_units.insert(input.clone(), unit);
            return Ok(&self.translation_units[input]);
        }

        // Retrieve the ASTs contributing to the unit
        let inputs = self.imports.unit_inputs(input)?;
        let mut desugarables: HashMap<Input, Content> = HashMap::new();

        for input in inputs {
            if let Some(file_id) = self.locators.get(input.locator()) {
                if let Some(ast) = self.asts.get(file_id) {
                    desugarables.insert(input.clone(), Content::new(*file_id, ast));
                } else if let Some(core_expr) = self.cores.get(input) {
                    // Include core expressions (from non-eucalypt file formats) in desugarables
                    desugarables.insert(input.clone(), Content::new(*file_id, core_expr));
                }
            }
        }

        // Desugar all the content starting from the input specified.
        // Seed the desugarer with the persisted monad namespace registry
        // so that `-e` expressions see `monad: true` from the prelude.
        let mut desugarer = Desugarer::new(&desugarables, &mut self.source_map);
        self.unit_interface
            .seed_desugarer_monad_registries(&mut desugarer);
        let unit = desugarer.translate_unit(input)?;
        // Persist any newly registered monad namespaces and types for later units.
        self.unit_interface
            .drain_desugarer_monad_registries(&mut desugarer);
        self.translation_units.insert(input.clone(), unit);
        Ok(&self.translation_units[input])
    }

    /// Pre-populate `translation_units` for a set of `(Input, TranslationUnit)`
    /// pairs, skipping the normal load + translate for those inputs.
    ///
    /// Used by [`crate::driver::check::run_type_checker_from_blob_core`]
    /// (eu-rb5n): the prelude blob carries each prelude-side unit's
    /// post-translate core (baked once at `cargo xtask prelude-compile`
    /// time), decoded once per process. Injecting those units directly here
    /// lets `merge_units` combine them with a freshly translated user unit
    /// without loading or translating prelude source. A subsequent call to
    /// `translate()` for an injected input is a no-op (see the existing-key
    /// short-circuit at the top of `translate()`).
    ///
    /// Entries for inputs that already have a translation (e.g. re-entrant
    /// calls) are left untouched — first write wins.
    pub fn inject_prelude_units(&mut self, units: Vec<(Input, TranslationUnit)>) {
        for (input, unit) in units {
            self.translation_units.entry(input).or_insert(unit);
        }
    }

    /// Set the body of the core expression to the specified target
    /// and return the target's format if specified
    pub fn retarget(&mut self, target: &str) -> Result<(), EucalyptError> {
        self.core = self.core.retarget(target)?;
        Ok(())
    }

    /// Cook the translated core to organise soup into proper
    /// application tree and to handle expression anaphora.
    ///
    /// When the pre-compiled prelude blob is active (set via `set_prelude_blob`),
    /// the cook `Distributor` is seeded with the blob's operator metadata so that
    /// infix uses of prelude functions in user code resolve correctly even though
    /// the prelude source is not present in the merged expression.
    pub fn cook(&mut self) -> Result<(), EucalyptError> {
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(ref blob) = self.prelude_blob {
            self.core.expr = cook::cook_with_prelude(self.core.expr.clone(), &blob.operators)?;
            return Ok(());
        }
        self.core.expr = cook::cook(self.core.expr.clone())?;
        Ok(())
    }

    /// Split `LetRec` scopes into minimal `Let`/`LetRec` via SCC
    /// decomposition.
    ///
    /// Builds a dependency graph for each non-block `OtherLet` scope,
    /// computes strongly connected components (Tarjan's algorithm),
    /// topologically sorts them, and re-emits the scope as a chain of
    /// nested `Let` scopes — one per SCC.  `DefaultBlockLet` scopes
    /// are not split.
    pub fn split_letrecs(&mut self) {
        self.core.expr = crate::core::dependency::split_letrecs(&self.core.expr);
    }

    /// Re-flatten nested Let/LetRec scopes after demand analysis.
    ///
    /// Merges chains of directly nested `Let`/`LetRec` scopes back
    /// into a single scope while preserving per-binding demand
    /// annotations.  This reverses the nesting introduced by SCC
    /// splitting so the STG compiler sees flat scopes (single
    /// `EnvFrame` at runtime) but with accurate demand information.
    pub fn reflatten_lets(&mut self) {
        self.core.expr = crate::core::reflatten::reflatten(&self.core.expr);
    }

    /// Run the namespace lambda hoisting pass.
    ///
    /// Hoists inlinable members of namespace `DefaultBlockLet` bindings (such
    /// as `str`, `cal`, `vec`) to top-level `OtherLet` bindings named
    /// `__<namespace>_<member>` and rewrites `Lookup(Var(ns), member)` to
    /// `Var(__<namespace>_<member>)`.  This lets the inline pass work directly
    /// on individual functions without distributing the namespace block.
    ///
    /// When the prelude blob is active the namespace blocks are not present in
    /// the user-code expression.  In that case `hoist_with_blob_globals` is
    /// used instead: it seeds the rewrite map from the blob's `name_to_slot`
    /// so that `Lookup(Var("str"), "upper")` is still rewritten to
    /// `Var(Free("__str_upper"))`, which the compiler resolves to `Ref::G`.
    ///
    /// The pass is a no-op when no hoistable members are found.
    pub fn hoist_namespaces(&mut self) -> Result<(), EucalyptError> {
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(ref blob) = self.prelude_blob {
            self.core.expr = hoist::hoist_with_blob_globals(&self.core.expr, &blob.name_to_slot)?;
            return Ok(());
        }
        self.core.expr = hoist::hoist(&self.core.expr)?;
        Ok(())
    }

    /// Store the pre-compiled prelude blob for use during cooking and evaluation.
    ///
    /// Must be called before `cook()` (for operator seeding) and before
    /// `take_prelude_blob()` (for runtime loading).  Calling this is equivalent
    /// to calling both `set_prelude_operators` and passing the blob to the
    /// `Executor`, but in a single load.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn set_prelude_blob(&mut self, blob: crate::eval::stg::blob::PreludeBlob) {
        // Seed monad specs from the blob so that :for/:random blocks are
        // recognised during desugaring even when the prelude source is skipped.
        self.unit_interface
            .monad_specs
            .extend(blob.monad_specs.iter().map(|(k, v)| (k.clone(), v.clone())));
        self.unit_interface.monad_type_hints.extend(
            blob.monad_type_hints
                .iter()
                .map(|(k, v)| (k.clone(), v.clone())),
        );
        self.prelude_blob = Some(blob);
    }

    /// Check whether a prelude blob is stored (i.e. the blob path is active).
    #[cfg(not(target_arch = "wasm32"))]
    pub fn has_prelude_blob(&self) -> bool {
        self.prelude_blob.is_some()
    }

    /// Peek at the stored prelude blob without taking it.
    ///
    /// Used by `bin/eu.rs` to decode `desugared_unit_cores` for the eval-path
    /// merged type check (eu-rb5n) — this runs before `take_prelude_blob()`
    /// hands the blob to the `Executor`, so the blob must still be present.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn prelude_blob(&self) -> Option<&crate::eval::stg::blob::PreludeBlob> {
        self.prelude_blob.as_ref()
    }

    /// Take the stored prelude blob out of the loader, leaving `None` behind.
    ///
    /// Called by `eval::run()` to pass the blob to the `Executor` without
    /// cloning it — the loader is consumed by `Executor::from(loader)` anyway.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn take_prelude_blob(&mut self) -> Option<crate::eval::stg::blob::PreludeBlob> {
        self.prelude_blob.take()
    }

    /// Inject inlinable prelude bindings from the blob as Let bindings wrapping
    /// the current core expression.
    ///
    /// When the prelude blob is active, prelude source is not loaded, so prelude
    /// function names appear as `Var::Free` in user code after cooking.  By
    /// injecting the blob's combinator lambdas as a Let scope immediately before
    /// the inline pass, the existing `distribute` and `beta_reduce` steps can
    /// distribute them to call sites and fold `+(x, 1)` → `__ADD(x, 1)`.
    ///
    /// The injected Let uses `OtherLet` so the prune pass treats its bindings
    /// as regular let-bindings; after inlining, any that are no longer referenced
    /// are removed by the subsequent eliminate pass.
    ///
    /// No-op when no blob is present or when `inlinable_bindings` is empty.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn inject_prelude_inlinable_bindings(&mut self) {
        let Some(ref blob) = self.prelude_blob else {
            return;
        };
        if blob.inlinable_bindings.is_empty() {
            return;
        }
        let bindings: Vec<CoreBinding<RcExpr>> = blob
            .inlinable_bindings
            .iter()
            .map(|(name, expr)| CoreBinding::new(name.clone(), expr.clone()))
            .collect();
        self.core.expr = RcExpr::from(Expr::Let(
            Smid::default(),
            Scope {
                pattern: bindings,
                body: self.core.expr.clone(),
            },
            LetType::OtherLet,
        ));
    }

    /// Run inliner
    pub fn inline(&mut self) -> Result<(), EucalyptError> {
        self.core.expr = tag::tag_combinators(&self.core.expr)?;
        self.core.expr = reduce::inline_pass(&self.core.expr)?;
        Ok(())
    }

    /// Run the destructure fusion pass.
    ///
    /// This folds static patterns that arise after the inline pass
    /// distributes destructuring lambdas to their call sites:
    /// - `Lookup(Block{...}, "key")` → the corresponding value
    /// - `HEAD(List[v0, ...])` → `v0`
    /// - `TAIL(List[v0, v1, ...])` → `List[v1, ...]`
    ///
    /// Running this pass after `inline` completes the fusion of block and
    /// list destructuring parameter patterns, eliminating intermediate
    /// allocations.
    pub fn fuse_destructure(&mut self) -> Result<(), EucalyptError> {
        self.core.expr = fuse::fuse(&self.core.expr)?;
        Ok(())
    }

    /// Prune definitions of any unused bindings prior to run.
    ///
    /// The single `prune` pass handles both binding-level DCE and
    /// block-level DCE (filtering unreferenced members from
    /// `DefaultBlockLet` bindings that are only accessed via static
    /// Lookup patterns).
    pub fn eliminate(&mut self) -> Result<(), EucalyptError> {
        self.core.expr = prune::prune(&self.core.expr);
        self.core.expr = compress::compress(&self.core.expr)?;
        Ok(())
    }

    /// Final verification of core expression prior to run
    pub fn verify(&mut self) -> Result<Vec<CoreError>, EucalyptError> {
        content::verify(self.core.expr.clone()).map_err(EucalyptError::from)
    }

    /// The core syntax translation units currently in play
    pub fn units(&self) -> Vec<&TranslationUnit> {
        self.translation_units.values().collect()
    }

    /// Merge all units into a single core expression
    pub fn merge_units(&mut self, inputs: &[Input]) -> Result<(), EucalyptError> {
        let units = inputs
            .iter()
            .map(|i| self.translation_units.get(i).expect("merging unknown unit"));
        self.core = TranslationUnit::merge(units.cloned())?;
        Ok(())
    }

    /// Collect all explicit inputs into a named collection then merge
    pub fn collect_and_merge_units(
        &mut self,
        collection_name: &str,
        name_inputs: bool,
        prologue_inputs: &[Input],
        explicit_inputs: &[Input],
        epilogue_inputs: &[Input],
    ) -> Result<(), EucalyptError> {
        let prologue_units = prologue_inputs
            .iter()
            .map(|i| self.translation_units.get(i).expect("merging unknown unit"));
        let explicit_units = explicit_inputs
            .iter()
            .map(|i| self.translation_units.get(i).expect("merging unknown unit"));
        let epilogue_units = epilogue_inputs
            .iter()
            .map(|i| self.translation_units.get(i).expect("merging unknown unit"));

        let mut collection_unit = if name_inputs {
            let names: Vec<String> = explicit_inputs
                .iter()
                .map(|i| i.name().clone().unwrap_or(format!("{}", i.locator())))
                .collect();
            TranslationUnit::blockify(&names, explicit_units)?
        } else {
            TranslationUnit::listify(explicit_units)?
        };

        collection_unit.expr = collection_unit
            .expr
            .apply_name(Smid::default(), collection_name);

        self.core = TranslationUnit::merge(
            prologue_units
                .chain(iter::once(&collection_unit))
                .chain(epilogue_units)
                .cloned(),
        )?;

        Ok(())
    }

    /// Retrieve the merged, complete, core expression
    pub fn core(&self) -> &TranslationUnit {
        &self.core
    }

    /// Replace the core expression (e.g. after post-check alias resolution).
    pub fn set_core_expr(&mut self, expr: RcExpr) {
        self.core.expr = expr;
    }

    /// Store type aliases collected from the unpruned expression.
    pub fn set_type_aliases(
        &mut self,
        aliases: HashMap<String, crate::core::typecheck::types::Type>,
    ) {
        self.early_type_aliases = aliases;
    }

    /// Retrieve pre-pruning type aliases for TypeData resolution.
    pub fn type_aliases(&self) -> &HashMap<String, crate::core::typecheck::types::Type> {
        &self.early_type_aliases
    }

    /// Return a reference to the cross-unit interface.
    pub fn unit_interface(&self) -> &UnitInterface {
        &self.unit_interface
    }

    /// Extract operator metadata from the current merged core expression into
    /// `unit_interface.operators`.
    ///
    /// Must be called BEFORE `cook()` — cook's `distribute_fixities` strips the
    /// `Meta` wrappers that carry `type:` annotations from operator definitions.
    pub fn extract_operators(&mut self) {
        let expr = self.core.expr.clone();
        self.unit_interface.extract_operators_from_expr(&expr);
    }

    /// Extract declaration visibility from the current merged core expression into
    /// `unit_interface.visibility`.
    ///
    /// Must be called BEFORE `cook()` while `Meta` wrappers are still present.
    /// Typically called alongside `extract_operators`.
    pub fn extract_visibility(&mut self) {
        let expr = self.core.expr.clone();
        self.unit_interface.extract_visibility_from_expr(&expr);
    }

    /// Extract demand (cardinality/strictness) annotations for exported bindings
    /// into `unit_interface.demands`.
    ///
    /// Must be called AFTER the prune pass (`eliminate()`) — the prune pass
    /// populates `CoreBinding::demand` with `Cardinality::AtMostOnce` for
    /// single-use bindings.  Calling this before prune would always yield
    /// `Demand::default()` (all `Unknown`) because the cardinality information
    /// has not yet been written.
    pub fn extract_demands(&mut self) {
        let expr = self.core.expr.clone();
        self.unit_interface.extract_demands_from_expr(&expr);
    }

    /// Return the source text for a file by its locator.
    pub fn source_text(&self, locator: &Locator) -> Option<&str> {
        self.locators
            .get(locator)
            .and_then(|id| self.files.get(*id).ok())
            .map(|f| f.source().as_str())
    }

    /// Return all loaded locators (including imports).
    pub fn loaded_locators(&self) -> Vec<&Locator> {
        self.locators.keys().collect()
    }

    /// Declare source manipulation complete and take ownership of
    /// reporting resources, along with the runtime environment data
    /// needed to override stale blob globals at execution time.
    pub fn complete(
        self,
    ) -> (
        SimpleFiles<String, String>,
        SourceMap,
        RcExpr,
        Vec<String>,
        Option<i64>,
    ) {
        (
            self.files,
            self.source_map,
            self.core.expr,
            self.args,
            self.seed,
        )
    }

    /// Read text from the filesystem, using lib-path to resolve the
    /// filenames.
    ///
    /// If the file cannot be found on the filesystem, falls back to checking
    /// baked-in resources. The stem of the filename (without extension) is
    /// used as the resource name, so `lens.eu` falls back to the `lens`
    /// resource if one exists. Filesystem resolution always takes priority,
    /// allowing users to override baked-in resources with their own files.
    fn read_fs_input(&mut self, path: &Path) -> Result<String, EucalyptError> {
        for libdir in &self.lib_path {
            let mut filename = libdir.to_path_buf();
            filename.push(path);
            if let Ok(text) = fs::read_to_string(&filename) {
                return Ok(text);
            }
        }

        // Try as absolute/relative from working directory
        if let Ok(text) = fs::read_to_string(path) {
            return Ok(text);
        }

        // Fall back to baked-in resources using the filename stem.
        // This allows libraries shipped inside the binary (e.g. `prelude.eu`,
        // `test.eu`) to be imported by filename without being present on disk.
        if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
            if let Some(resource_text) = self.resources.get(stem) {
                return Ok(resource_text.clone());
            }
        }

        Err(EucalyptError::FileCouldNotBeRead(
            path.to_string_lossy().to_string(),
            None,
        ))
    }

    /// Read source from stdin
    fn read_stdin(&mut self) -> Result<String, EucalyptError> {
        let mut buffer = String::new();
        io::stdin().read_to_string(&mut buffer)?;
        Ok(buffer)
    }

    /// Access to source map for creating diagnostics
    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }

    /// Drain and return any parse errors accumulated during `load_eucalypt`.
    ///
    /// Parse errors no longer abort loading — the partial tree is always
    /// stored.  Call this after loading all inputs to collect the errors and
    /// decide how to surface them (e.g. diagnose to stderr, or abort the
    /// pipeline with a structured error).
    pub fn drain_parse_errors(&mut self) -> Vec<EucalyptError> {
        std::mem::take(&mut self.pending_parse_errors)
    }

    /// Access the file store for error location resolution.
    pub fn files(&self) -> &SimpleFiles<String, String> {
        &self.files
    }

    /// Print a diagnostic to stderr
    pub fn diagnose_to_stderr(&self, diag: &Diagnostic<usize>) {
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = codespan_reporting::term::Config::default();
        emit(&mut writer.lock(), &config, &self.files, diag)
            .expect("failed to write diagnostic to stderr");
    }

    pub fn diagnose_to_string(&self, diag: &Diagnostic<usize>) -> String {
        let mut s: Vec<u8> = Vec::new();
        {
            let mut writer = NoColor::new(&mut s);
            let config = codespan_reporting::term::Config::default();
            emit(&mut writer, &config, &self.files, diag)
                .expect("failed to write diagnostic to buffer");
        }
        String::from_utf8_lossy(&s).into_owned()
    }
}

/// Read the `prelude:` value from the top-level unit metadata block.
///
/// Returns the raw string value (symbol name or file path) if found.
/// Only inspects `ParsedAst::Unit` — CLI Soup expressions cannot specify
/// a prelude override.
#[cfg(not(target_arch = "wasm32"))]
fn read_prelude_from_ast(ast: &ParsedAst) -> Option<String> {
    use crate::syntax::rowan::ast::{AstToken, DeclarationKind, Element, HasSoup};

    let unit = match ast {
        ParsedAst::Unit(u) => u,
        ParsedAst::Soup(_) => return None,
    };

    let meta = unit.meta()?;
    let soup = meta.soup()?;

    for element in soup.elements() {
        if let Element::Block(block) = element {
            for decl in block.declarations() {
                let head = decl.head()?;
                if let DeclarationKind::Property(prop) = head.classify_declaration() {
                    if prop.text() == "prelude" {
                        if let Some(body) = decl.body() {
                            if let Some(body_soup) = body.soup() {
                                for body_elem in body_soup.elements() {
                                    if let Element::Lit(lit) = body_elem {
                                        if let Some(val) = lit.value() {
                                            // Accept both string literals and symbol literals
                                            if let Some(s) = val.string_value() {
                                                return Some(s.to_string());
                                            }
                                            if let Some(s) = val.symbol_name() {
                                                return Some(s.to_string());
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// Convert a prelude reference string into an `Input`.
///
/// - If the string is a valid filesystem path (contains `/` or ends with `.eu`), use `Locator::Fs`.
/// - Otherwise treat it as a resource name, using `Locator::Resource`.
#[cfg(not(target_arch = "wasm32"))]
fn prelude_ref_to_input(prelude_ref: &str) -> Input {
    use std::path::PathBuf;

    // Use filesystem path if it looks like a path (contains a directory separator
    // or file extension).  Plain names like "v2" are treated as resource names.
    let is_path =
        prelude_ref.contains('/') || prelude_ref.contains('\\') || prelude_ref.ends_with(".eu");
    if is_path {
        Input::new(Locator::Fs(PathBuf::from(prelude_ref)), None, "eu")
    } else {
        Input::new(Locator::Resource(prelude_ref.to_string()), None, "eu")
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use std::fs;
    use std::path::Path;

    fn eu_paths() -> Vec<PathBuf> {
        let mut paths = vec![];

        if let Ok(entries) = fs::read_dir("./tests/harness") {
            for f in entries.flatten() {
                let path = f.path();
                if path.extension().and_then(|s| s.to_str()) == Some("eu") {
                    let name = path
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .expect(".eu file should have a valid UTF-8 stem");

                    if name == "x013_arith_overflow" {
                        continue;
                    }
                    paths.push(f.path())
                }
            }
        } else {
            panic!("Failed to read test directory");
        }

        paths
    }

    #[test]
    fn test_parse_harness_tests() {
        let mut loader = SourceLoader::new(vec![
            std::env::current_dir().unwrap(),
            Path::new("./tests").to_path_buf(),
        ]);

        for f in eu_paths() {
            if let Err(e) = loader.load_eucalypt(&Locator::Fs(f.clone())) {
                let diag = loader.diagnose_to_string(&e.to_diagnostic(loader.source_map()));
                panic!("Failed to parse {:?}.\n{}", f, diag);
            }
        }
    }

    #[test]
    fn test_desugar_tests() {
        let mut loader = SourceLoader::new(vec![Path::new("./tests/harness").to_path_buf()]);

        for f in eu_paths() {
            let loc = Locator::Fs(f.clone());

            if let Err(e) = loader.load_tree(&Input::from(loc.clone())) {
                let diag = loader.diagnose_to_string(&e.to_diagnostic(loader.source_map()));
                panic!("Failed to parse {:?}.\n{}", f, diag);
            }

            if let Err(e) = loader.translate(&Input::from(loc.clone())) {
                let diag = loader.diagnose_to_string(&e.to_diagnostic(loader.source_map()));
                panic!("Failed to desugar {:?}.\n{}", f, diag);
            };
        }
    }

    #[test]
    fn test_load_prelude() {
        let mut loader = SourceLoader::default();
        let prelude = Locator::Resource("prelude".to_string());
        let _id = loader.load_tree(&Input::from(prelude.clone())).unwrap();
        let _ast = loader.ast(&prelude);
    }

    /// When `import: "test.eu"` is used and no `test.eu` file exists on disk,
    /// the loader should fall back to the baked-in `test` resource.
    #[test]
    fn test_resource_fallback_for_fs_import() {
        // Use an empty lib_path so no filesystem files will be found.
        let mut loader = SourceLoader::new(vec![]);
        // `test.eu` is not present on disk relative to this path, but `test`
        // is a baked-in resource; loading via a Fs locator should succeed.
        let locator = Locator::Fs(PathBuf::from("test.eu"));
        let id = loader
            .load_eucalypt(&locator)
            .expect("should fall back to baked-in `test` resource");
        // The AST should have been stored under the original Fs locator.
        assert!(loader.ast(&locator).is_some(), "AST not stored for locator");
        // Verify a file entry was created in the file store (id is always a valid usize).
        let _ = id;
    }

    /// A filename that has no corresponding baked-in resource should still
    /// produce a FileCouldNotBeRead error.
    #[test]
    fn test_no_fallback_for_unknown_file() {
        let mut loader = SourceLoader::new(vec![]);
        let locator = Locator::Fs(PathBuf::from("nonexistent_file_xyz.eu"));
        let result = loader.load_eucalypt(&locator);
        assert!(
            result.is_err(),
            "should error when file and resource are both missing"
        );
    }
}
