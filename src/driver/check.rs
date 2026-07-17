//! Type annotation check command (`eu check`).
//!
//! Two phases:
//!
//! 1. **Annotation syntax check** — parse each source file's rowan AST,
//!    find all `type:` metadata annotations, and validate them against the
//!    type grammar.  This is fast and requires no pipeline processing.
//!
//! 2. **Bidirectional type check** — load files through the pipeline, then
//!    run the type checker.  The prelude is checked once per process and the
//!    result cached; user files are seeded with the cached prelude types,
//!    avoiding redundant prelude re-checking on every file.
//!
//! Type issues are always warnings unless `--strict` is passed. Genuine
//! parse errors and content-verifier errors (e.g. always-divergent
//! bindings) are exceptions to that rule: both are unconditional hard
//! errors — `eu check` exits non-zero for them with or without `--strict`
//! (eu-3621) — because they are more fundamental than a type-annotation
//! mismatch and the plain `eu` eval path already treats them as fatal.

use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::sync::Mutex;
use std::time::Instant;

use crate::core::error::CoreError;
use crate::core::expr::RcExpr;
use crate::core::typecheck::{
    check::{
        parse_operator_overloads, type_check, type_check_for_prelude, type_check_full,
        type_check_with_operator_overloads, type_check_with_seed,
    },
    parse,
};
use crate::core::unit::TranslationUnit;
use crate::core::verify::deprecation::check_deprecated_references;
use crate::driver::error::EucalyptError;
use crate::driver::options::EucalyptOptions;
use crate::driver::source::SourceLoader;
use crate::driver::unit_interface::UnitInterface;
use crate::syntax::input::{Input, Locator};
use crate::syntax::rowan::ast::{self, Block, Declaration, Element, HasSoup};
use crate::syntax::rowan::parse_unit;
use rowan::ast::AstNode;

// ── Prelude interface cache (§B7) ────────────────────────────────────────────

/// In-memory cache for compiled `UnitInterface`s, keyed by prelude identifier.
///
/// The key is the prelude resource name (e.g. `"prelude"`) or file path.
/// Multiple entries allow different preludes to coexist without cache corruption
/// when files in the same invocation select different preludes.
static PRELUDE_CACHE: Mutex<Option<HashMap<String, UnitInterface>>> = Mutex::new(None);

/// Return a cached (or freshly built) `UnitInterface` for the given prelude key.
///
/// Runs the prelude through load → translate → cook (no eliminate) once per key,
/// then calls `type_check_for_prelude` to capture the root scope.
/// Subsequent calls with the same key return the cached value immediately.
fn get_or_build_prelude_interface_for(prelude_key: &str) -> Option<UnitInterface> {
    {
        let guard = PRELUDE_CACHE.lock().ok()?;
        if let Some(ref map) = *guard {
            if let Some(ui) = map.get(prelude_key) {
                return Some(ui.clone());
            }
        }
    }

    let ui = build_prelude_interface_for(prelude_key).ok()?;
    let mut guard = PRELUDE_CACHE.lock().ok()?;
    let map = guard.get_or_insert_with(HashMap::new);
    map.entry(prelude_key.to_string())
        .or_insert(ui)
        .clone()
        .into()
}

/// Build the `UnitInterface` for a given prelude key.
///
/// `prelude_key` is a resource name (e.g. `"prelude"`) or a filesystem path.
/// Loads the prelude, translates, merges, extracts operators (once, before
/// cook), and cooks it (no eliminate step — every prelude binding must be
/// retained for caching), then runs `type_check_for_prelude` to capture
/// binding type schemes, aliases, and branch shapes.
fn build_prelude_interface_for(prelude_key: &str) -> Result<UnitInterface, EucalyptError> {
    use std::path::PathBuf;
    let locator = if prelude_key.contains('/') || prelude_key.ends_with(".eu") {
        Locator::Fs(PathBuf::from(prelude_key))
    } else {
        Locator::Resource(prelude_key.to_string())
    };
    let prelude = Input::new(locator, None, "eu");
    let inputs = vec![prelude];

    let mut loader = SourceLoader::new(vec![]);

    for input in &inputs {
        loader.load(input)?;
    }
    for input in &inputs {
        loader.translate(input)?;
    }
    loader.merge_units(&inputs)?;

    // Extract operator metadata and visibility BEFORE cook strips Meta wrappers.
    loader.extract_operators();
    loader.extract_visibility();

    loader.cook()?;
    // Deliberately NO eliminate — every prelude binding must be retained for
    // caching so that type information is not lost.

    let core_expr = loader.core().expr.clone();
    let (_, mut summary) = type_check_for_prelude(&core_expr);

    // Populate operator_overloads in the type summary from the pre-cook
    // operator info stored in unit_interface.
    //
    // Any operator-annotation parse-error warnings are discarded here, same
    // as the primary `type_check_for_prelude` warnings above — the prelude
    // is expected to stay warning-free (verified by `cargo xtask
    // prelude-compile` and the full test suite), so there is nothing for a
    // per-invocation cache build to usefully report.
    let operator_type_strings = loader.unit_interface().operator_type_strings();
    let (operator_overloads, _operator_annotation_warnings) =
        parse_operator_overloads(&operator_type_strings);
    summary.operator_overloads = operator_overloads;

    let mut ui = loader.unit_interface().clone();
    ui.type_summary = summary;
    Ok(ui)
}

// ── Phase 1: Annotation syntax check ────────────────────────────────────────

/// A single type annotation found in a source file.
struct Annotation {
    /// Byte offset of the annotation string within the source file.
    source_offset: usize,
    /// The annotation string value.
    value: String,
    /// Name of the declaration the annotation was attached to (for error messages).
    decl_name: String,
}

/// Run the `eu check` command.
pub fn check(opt: &EucalyptOptions) -> Result<i32, String> {
    let files: Vec<_> = opt.explicit_inputs().iter().collect();

    if files.is_empty() {
        eprintln!("eu check: no files specified");
        return Ok(1);
    }

    // ── Phase 1: annotation syntax validation ──────────────────────────────
    let t_phase1 = Instant::now();
    let mut syntax_errors = 0usize;
    let mut parse_errors = 0usize;

    for input in &files {
        let path_str = input.locator().to_string();
        let path = Path::new(&path_str);

        if !path.exists() {
            eprintln!("eu check: file not found: {path_str}");
            syntax_errors += 1;
            continue;
        }

        let source = fs::read_to_string(path)
            .map_err(|e| format!("Failed to read '{}': {}", path_str, e))?;

        let (ann_errors, file_parse_errors) =
            check_annotation_syntax(&path_str, &source, opt.check_strict());
        syntax_errors += ann_errors;
        parse_errors += file_parse_errors;
    }
    let phase1_elapsed = t_phase1.elapsed();

    // ── Phase 2: bidirectional type check + content verifier via pipeline ─────
    let t_phase2 = Instant::now();
    let (type_warning_count, content_error_count) = match run_type_checker(opt) {
        Ok(result) => {
            let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
                codespan_reporting::term::termcolor::ColorChoice::Auto,
            );
            let config = codespan_reporting::term::Config::default();

            // Emit type warnings.
            for w in &result.warnings {
                let diag = w.to_diagnostic(&result.source_map);
                // Ignore render errors — best effort
                let _ = codespan_reporting::term::emit(
                    &mut writer.lock(),
                    &config,
                    &result.files,
                    &diag,
                );
            }

            // Emit content verifier errors (e.g. trivial self-assignment).
            for e in &result.core_errors {
                let diag = e.to_diagnostic(&result.source_map);
                let _ = codespan_reporting::term::emit(
                    &mut writer.lock(),
                    &config,
                    &result.files,
                    &diag,
                );
            }

            (result.warnings.len(), result.core_errors.len())
        }
        Err(e) => {
            // Pipeline failure is not a type error — log at debug level and
            // continue with the annotation syntax results only.
            eprintln!("eu check: pipeline warning: {e}");
            (0, 0)
        }
    };
    let phase2_elapsed = t_phase2.elapsed();

    // Report timings when --statistics is active
    if opt.statistics() {
        eprintln!(
            "eu check: annotation syntax: {:.3}s, type check: {:.3}s",
            phase1_elapsed.as_secs_f64(),
            phase2_elapsed.as_secs_f64(),
        );
    }

    let total_issues = syntax_errors + type_warning_count + content_error_count + parse_errors;

    if total_issues == 0 {
        Ok(0)
    } else if opt.check_strict() || content_error_count > 0 || parse_errors > 0 {
        // Content verifier errors (e.g. always-divergent bindings) and
        // genuine rowan parse errors (eu-3621) are always hard errors,
        // regardless of --strict. A parse error is a more fundamental
        // failure than a type warning — the file never produced a syntax
        // tree at all — and the plain `eu` eval path (no subcommand)
        // already aborts unconditionally on one (`run` in
        // `src/driver/eval.rs`), so `eu check`, strict or not, must not
        // report success for a file that failed to parse.
        Ok(1)
    } else {
        // Type warnings never cause non-zero exit unless --strict.
        // Annotation *syntax* errors are always real errors, though.
        if syntax_errors > 0 {
            Ok(1)
        } else {
            Ok(0)
        }
    }
}

/// Run the type checker on a single eucalypt source file, loading the prelude
/// automatically.
///
/// Returns all `TypeWarning`s found.  Returns an empty `Vec` if the pipeline
/// fails (parse error, import error, etc.) — the caller is responsible for
/// deciding how to surface those errors separately.
///
/// This function is used by the LSP server to run the checker on document
/// open/change events.
pub fn type_check_path(path: &Path) -> Vec<crate::core::typecheck::error::TypeWarning> {
    type_check_path_full(path).warnings
}

/// Annotation syntax errors (Phase 1 only) — exposed for LSP use.
pub fn annotation_syntax_errors(source: &str) -> Vec<(usize, String, String)> {
    let parse_result = parse_unit(source);
    let unit = parse_result.tree();
    let annotations = collect_annotations_from_unit(&unit, source);
    let mut errors = vec![];
    for ann in annotations {
        if let Err(e) = parse::parse_scheme(&ann.value).map(|_| ()) {
            errors.push((ann.source_offset, ann.decl_name.clone(), e.to_string()));
        }
    }
    errors
}

/// Result of running the type checker via the pipeline: warnings, inferred
/// types, and the source map for resolving warning locations.
pub struct PathCheckResult {
    /// Type warnings (mismatches, missing fields, etc.).
    pub warnings: Vec<crate::core::typecheck::error::TypeWarning>,
    /// Flattened type environment mapping binding names to their inferred types.
    pub types: std::collections::HashMap<String, crate::core::typecheck::types::Type>,
    /// Source map for resolving Smids to source locations.
    pub source_map: crate::common::sourcemap::SourceMap,
}

/// Run the type checker on a single eucalypt source file, returning both
/// warnings and the inferred type environment.
///
/// Uses the prelude-cached pipeline: the prelude is checked once per
/// process and cached; user files are checked seeded with the cached
/// prelude types.  User-file imports are still resolved normally — only
/// the prelude is cached.  Falls back to the merged pipeline if the
/// prelude cache cannot be built.
///
/// If the pipeline fails at any stage, returns a single warning
/// explaining which stage failed rather than silently producing no
/// results.
pub fn type_check_path_full(path: &Path) -> PathCheckResult {
    use crate::core::typecheck::error::TypeWarning;

    let pipeline_error = |stage: &str, err: &dyn std::fmt::Display| PathCheckResult {
        warnings: vec![TypeWarning::new(format!(
            "type checking unavailable: {stage} failed: {err}"
        ))],
        types: std::collections::HashMap::new(),
        source_map: crate::common::sourcemap::SourceMap::new(),
    };

    // Detect any prelude: override declared in the file so that the correct
    // prelude interface is used for seeding — AC5.
    let prelude_key = detect_prelude_key(path).unwrap_or_else(|| "prelude".to_string());

    // ── Attempt prelude-cached check (fast path) ──────────────────────────
    if let Some(ui) = get_or_build_prelude_interface_for(&prelude_key) {
        return type_check_path_with_seed(path, &ui, pipeline_error);
    }

    // ── Fallback: merged pipeline (prelude cache could not be built) ───────
    type_check_path_merged(path, pipeline_error)
}

/// Read a eucalypt source file and return the value of its `prelude:` metadata
/// key, if present.  Returns `None` if the file cannot be read, has no unit
/// metadata, or carries no `prelude:` key.
fn detect_prelude_key(path: &Path) -> Option<String> {
    use crate::syntax::rowan::ast::{AstToken, DeclarationKind, HasSoup};

    let source = fs::read_to_string(path).ok()?;
    let parse_result = parse_unit(&source);
    let unit = parse_result.tree();

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

/// Check a single user file seeded with a cached prelude interface (§B7 fast path).
fn type_check_path_with_seed(
    path: &Path,
    ui: &UnitInterface,
    pipeline_error: impl Fn(&str, &dyn std::fmt::Display) -> PathCheckResult,
) -> PathCheckResult {
    let file = Input::new(Locator::Fs(path.to_path_buf()), None, "eu");
    let inputs = vec![file];

    let mut loader = SourceLoader::new(vec![]);

    if let Err(e) = loader.load(&inputs[0]) {
        return pipeline_error("load", &e);
    }
    if let Err(e) = loader.translate(&inputs[0]) {
        return pipeline_error("desugar", &e);
    }
    if let Err(e) = loader.merge_units(&inputs) {
        return pipeline_error("merge", &e);
    }
    if let Err(e) = loader.cook() {
        return pipeline_error("cook", &e);
    }
    if let Err(e) = loader.eliminate() {
        return pipeline_error("eliminate", &e);
    }

    let deprecation_warnings =
        check_deprecated_references(&loader.core().expr, &loader.core().deprecations);
    let core_expr = loader.core().expr.clone();
    let mut warnings = type_check_with_seed(&core_expr, &ui.type_summary);
    warnings.extend(deprecation_warnings);
    let (_, source_map, _, _, _) = loader.complete();
    PathCheckResult {
        warnings,
        types: std::collections::HashMap::new(),
        source_map,
    }
}

/// Check a file using the original merged pipeline (fallback when the
/// prelude cache is unavailable).
fn type_check_path_merged(
    path: &Path,
    pipeline_error: impl Fn(&str, &dyn std::fmt::Display) -> PathCheckResult,
) -> PathCheckResult {
    let prelude = Input::new(Locator::Resource("prelude".to_string()), None, "eu");
    let file = Input::new(Locator::Fs(path.to_path_buf()), None, "eu");
    let inputs = vec![prelude, file];

    let mut loader = SourceLoader::new(vec![]);

    for input in &inputs {
        if let Err(e) = loader.load(input) {
            return pipeline_error("load", &e);
        }
    }
    for input in &inputs {
        if let Err(e) = loader.translate(input) {
            return pipeline_error("desugar", &e);
        }
    }
    if let Err(e) = loader.merge_units(&inputs) {
        return pipeline_error("merge", &e);
    }
    if let Err(e) = loader.cook() {
        return pipeline_error("cook", &e);
    }
    if let Err(e) = loader.eliminate() {
        return pipeline_error("eliminate", &e);
    }

    let deprecation_warnings =
        check_deprecated_references(&loader.core().expr, &loader.core().deprecations);
    let core_expr = loader.core().expr.clone();
    let mut result = type_check_full(&core_expr);
    result.warnings.extend(deprecation_warnings);
    let (_, source_map, _, _, _) = loader.complete();
    PathCheckResult {
        warnings: result.warnings,
        types: result.types,
        source_map,
    }
}

/// Result of running the type checker through the full pipeline.
pub struct PipelineCheckResult {
    pub warnings: Vec<crate::core::typecheck::error::TypeWarning>,
    /// Static errors detected by the content verifier (e.g. trivial self-assignment).
    pub core_errors: Vec<CoreError>,
    pub files: codespan_reporting::files::SimpleFiles<String, String>,
    pub source_map: crate::common::sourcemap::SourceMap,
}

/// Run the bidirectional type checker by loading files through the pipeline.
///
/// Returns warnings, the SimpleFiles (for rendering), and the SourceMap (for
/// resolving Smids to source locations).  Returns an `Err` if the pipeline
/// fails (e.g. parse error in the source) — the caller logs this and continues
/// with only the annotation syntax check results.
pub fn run_type_checker(opt: &EucalyptOptions) -> Result<PipelineCheckResult, EucalyptError> {
    let mut loader = SourceLoader::new(opt.lib_path().to_vec());
    let inputs = opt.inputs();

    // Load all inputs (prelude + user files).
    for input in &inputs {
        loader.load(input)?;
    }

    // Report parse errors as diagnostics but do not abort — the partial tree
    // (with ERROR_STOWAWAYS nodes) allows the type checker to run on the
    // syntactically valid portions.
    let parse_errors = loader.drain_parse_errors();
    for e in &parse_errors {
        let diag = e.to_diagnostic(loader.source_map());
        loader.diagnose_to_stderr(&diag);
    }

    // Translate each input to core.
    for input in &inputs {
        loader.translate(input)?;
    }

    // Merge into a single expression.
    loader.merge_units(&inputs)?;

    // Extract operator metadata BEFORE cook strips Meta wrappers.
    //
    // The `distribute_fixities` step in cook removes `Meta` wrappers from
    // operator definitions (e.g. `(l < r): ...`) to move fixity info to call
    // sites.  This erases the `type:` annotation from operators.  We capture
    // them here, before cook, so that constraint discharge can verify that
    // e.g. `<(a, a) => a → a → a` is satisfiable for the argument types.
    loader.extract_operators();
    loader.extract_visibility();
    let operator_type_strings = loader.unit_interface().operator_type_strings();
    let (operator_overloads, operator_annotation_warnings) =
        parse_operator_overloads(&operator_type_strings);

    // Cook (resolve operator precedence).
    loader.cook()?;

    // Eliminate dead bindings.
    loader.eliminate()?;

    // Extract demand annotations AFTER eliminate so the prune pass has had a
    // chance to populate `CoreBinding::demand` with cardinality information.
    loader.extract_demands();

    // Run the deprecation reference checker before cook strips metadata.
    let deprecation_warnings =
        check_deprecated_references(&loader.core().expr, &loader.core().deprecations);

    // Run the type checker, seeded with the pre-cook operator overloads.
    let core_expr = loader.core().expr.clone();
    let mut warnings = if operator_overloads.is_empty() {
        type_check(&core_expr).0
    } else {
        type_check_with_operator_overloads(&core_expr, operator_overloads)
    };
    warnings.extend(operator_annotation_warnings);
    warnings.extend(deprecation_warnings);

    // Run content verifier to catch static errors (e.g. trivial self-assignment).
    let core_errors = loader.verify().unwrap_or_default();

    let (files, source_map, _, _, _) = loader.complete();
    Ok(PipelineCheckResult {
        warnings,
        core_errors,
        files,
        source_map,
    })
}

/// The four prelude-side prologue inputs, tagged to match the `(tag, expr)`
/// pairs baked into `PreludeBlob::desugared_unit_cores` by `cargo xtask
/// prelude-compile`. Order and construction mirror
/// `EucalyptOptions::finalize`'s `prepend_input` calls exactly (locator,
/// name, format) so the `Input`s hash-match what `opt.inputs()` produces.
fn tag_for_prelude_side_input(input: &Input) -> Option<&'static str> {
    match (input.locator(), input.name().as_deref()) {
        (Locator::Resource(name), Some("__build")) if name == "build-meta" => Some("build"),
        (Locator::Pseudo(name), Some("__io")) if name == "io" => Some("io"),
        (Locator::Pseudo(name), Some("__args")) if name == "args" => Some("args"),
        (Locator::Resource(name), None) if name == "prelude" => Some("prelude"),
        _ => None,
    }
}

/// Run the bidirectional type checker using the prelude blob's baked
/// desugared, per-unit cores instead of loading and translating prelude
/// source (eu-rb5n).
///
/// Mirrors [`run_type_checker`] exactly — same `merge_units` →
/// `extract_operators`/`extract_visibility` → `cook` → `eliminate` →
/// `extract_demands` → `check_deprecated_references` →
/// `type_check`/`type_check_with_operator_overloads` → `verify` pipeline —
/// except that the four prelude-side inputs (`__build`, `__io`, `__args`,
/// the prelude itself) are injected from `prelude_units` (as decoded via
/// [`crate::eval::stg::blob::PreludeBlob::decode_desugared_unit_cores`])
/// rather than loaded and translated from source. This is the only step
/// skipped; the resulting warning set must be byte-equal to
/// `run_type_checker`'s for the same inputs (verified by the eu-rb5n test
/// matrix).
///
/// `prelude_units` entries missing a recognised tag are silently not
/// injected — the corresponding input falls through to a normal load +
/// translate, degrading to `run_type_checker`'s cost for that unit only
/// (e.g. a partial or stale blob still produces a correct, if slower,
/// result).
///
/// ## `Smid` cross-process scoping (eu-rb5n / Wicket review)
///
/// The injected `prelude_units` cores carry `Smid` values minted by the
/// offline `cargo xtask prelude-compile` process's own `SourceMap`. `Smid`
/// (`src/common/sourcemap.rs`) is a bare index into a process-local `Vec`,
/// so these indices are foreign to this call's fresh `SourceLoader`/
/// `SourceMap`. In practice this is harmless: every diagnostic in the
/// eu-rb5n corpus sweep (515 files) cites the *user's* call site, never a
/// location inside the injected prelude core, because `prune::prune`
/// (`eliminate()`, called before `type_check` in this pipeline) only keeps
/// prelude bindings reachable from the user's own code, and `lib/prelude.eu`
/// carries no internal type mismatch today. If a future prelude change
/// introduced a genuine internal mismatch in a reachable binding, the
/// resulting warning's foreign `Smid` would either resolve to `None` (out
/// of range — the far more likely outcome, since the offline compile mints
/// thousands of indices a small check invocation's own `SourceMap` never
/// approaches) and render without a location label, or, for a coincidentally
/// in-range index, could alias to an unrelated real entry and show a
/// misleading location — see
/// `tests::in_prelude_smid_degrades_gracefully_not_misleadingly` below,
/// which constructs exactly this scenario with a synthetic prelude and
/// confirms the out-of-range (graceful) case. Fully closing this gap means
/// re-basing baked `Smid`s into the runtime `SourceMap` at injection time —
/// tracked as follow-on work, out of scope for this PR.
pub fn run_type_checker_from_blob_core(
    opt: &EucalyptOptions,
    prelude_units: &[(String, RcExpr)],
) -> Result<PipelineCheckResult, EucalyptError> {
    let mut loader = SourceLoader::new(opt.lib_path().to_vec());
    let inputs = opt.inputs();

    // Inject the prelude-side units the blob supplied a baked core for, and
    // record which inputs that covers so the load loop below can skip them.
    let mut injected: std::collections::HashSet<&Input> = std::collections::HashSet::new();
    for input in &inputs {
        let Some(tag) = tag_for_prelude_side_input(input) else {
            continue;
        };
        let Some((_, expr)) = prelude_units.iter().find(|(t, _)| t == tag) else {
            continue;
        };
        let unit = TranslationUnit {
            expr: expr.clone(),
            targets: Default::default(),
            own_targets: Default::default(),
            docs: Vec::new(),
            deprecations: Default::default(),
        };
        loader.inject_prelude_units(vec![(input.clone(), unit)]);
        injected.insert(input);
    }

    // Load every input that wasn't just injected (the user's files, plus any
    // prelude-side input the blob didn't cover).
    for input in &inputs {
        if injected.contains(input) {
            continue;
        }
        loader.load(input)?;
    }

    let parse_errors = loader.drain_parse_errors();
    for e in &parse_errors {
        let diag = e.to_diagnostic(loader.source_map());
        loader.diagnose_to_stderr(&diag);
    }

    // Translate every input — injected prelude-side units short-circuit
    // immediately (see `translate()`'s existing-key check), so this only
    // does real work for the user's files.
    for input in &inputs {
        loader.translate(input)?;
    }

    loader.merge_units(&inputs)?;

    loader.extract_operators();
    loader.extract_visibility();
    let operator_type_strings = loader.unit_interface().operator_type_strings();
    let (operator_overloads, operator_annotation_warnings) =
        parse_operator_overloads(&operator_type_strings);

    loader.cook()?;
    loader.eliminate()?;
    loader.extract_demands();

    let deprecation_warnings =
        check_deprecated_references(&loader.core().expr, &loader.core().deprecations);

    let core_expr = loader.core().expr.clone();
    let mut warnings = if operator_overloads.is_empty() {
        type_check(&core_expr).0
    } else {
        type_check_with_operator_overloads(&core_expr, operator_overloads)
    };
    warnings.extend(operator_annotation_warnings);
    warnings.extend(deprecation_warnings);

    let core_errors = loader.verify().unwrap_or_default();

    let (files, source_map, _, _, _) = loader.complete();
    Ok(PipelineCheckResult {
        warnings,
        core_errors,
        files,
        source_map,
    })
}

// ── Annotation syntax check helpers ─────────────────────────────────────────

/// Check annotation syntax in a single source string.
///
/// Returns `(annotation_syntax_error_count, file_parse_error_count)`: the
/// number of malformed `type:` annotation strings, and separately the number
/// of genuine rowan parse errors in the file itself (eu-3621). The two are
/// deliberately distinct counts — the caller (`check`) treats a file parse
/// error as an unconditional hard error (like content-verifier errors),
/// whereas an annotation syntax error's severity historically already gates
/// on `--strict` via the `syntax_errors` accumulator in `check`.
fn check_annotation_syntax(filename: &str, source: &str, strict: bool) -> (usize, usize) {
    let parse_result = parse_unit(source);

    // Report any eucalypt syntax errors first (always real errors) and count
    // them — a genuine parse error means the file never produced a usable
    // syntax tree, which is more fundamental than a type warning and must
    // fail `eu check` regardless of --strict (eu-3621; previously these were
    // printed here but silently uncounted, so `eu check --strict` exited 0
    // despite a parse error).
    let file_parse_error_count = parse_result.errors().len();
    for err in parse_result.errors() {
        eprintln!("{filename}: parse error: {err}");
    }

    let unit = parse_result.tree();
    let annotations = collect_annotations_from_unit(&unit, source);

    let mut error_count = 0;
    for ann in annotations {
        // Strip leading `!` (asserted annotation marker) before parsing
        let type_str = ann.value.strip_prefix('!').unwrap_or(&ann.value).trim();
        if let Err(e) = parse::parse_scheme(type_str).map(|_| ()) {
            let severity = if strict { "error" } else { "warning" };
            let line_col = byte_offset_to_line_col(source, ann.source_offset);
            eprintln!(
                "{filename}:{line_col}: {severity}: invalid type annotation on '{}': {}",
                ann.decl_name, e
            );
            error_count += 1;
        }
    }

    (error_count, file_parse_error_count)
}

/// Convert a byte offset to a `line:col` string (1-based).
fn byte_offset_to_line_col(source: &str, offset: usize) -> String {
    let clamped = offset.min(source.len());
    let prefix = &source[..clamped];
    let line = prefix.bytes().filter(|&b| b == b'\n').count() + 1;
    let col = match prefix.rfind('\n') {
        Some(last_nl) => clamped - last_nl,
        None => clamped + 1,
    };
    format!("{line}:{col}")
}

/// Collect all `type:` annotations from a `Unit`.
fn collect_annotations_from_unit(unit: &ast::Unit, _source: &str) -> Vec<Annotation> {
    let mut result = Vec::new();
    for decl in unit.declarations() {
        collect_annotations_from_decl(&decl, &mut result);
    }
    result
}

/// Collect `type:` annotations from a declaration (and any nested blocks in its body).
fn collect_annotations_from_decl(decl: &Declaration, out: &mut Vec<Annotation>) {
    let decl_name = declaration_name(decl);

    // Inspect the declaration's backtick metadata block for a `type:` field.
    if let Some(meta) = decl.meta() {
        if let Some(soup) = meta.soup() {
            let elems: Vec<ast::Element> = soup.elements().collect();
            if let Some(Element::Block(meta_block)) = elems.first() {
                if let Some(ann) = extract_type_annotation(meta_block, &decl_name) {
                    out.push(ann);
                }
            }
        }
    }

    // Recurse into nested block bodies.
    if let Some(body) = decl.body() {
        if let Some(body_soup) = body.soup() {
            for elem in body_soup.elements() {
                if let Element::Block(inner_block) = elem {
                    collect_annotations_from_block(&inner_block, out);
                }
            }
        }
    }
}

/// Collect `type:` annotations from all declarations within a block.
fn collect_annotations_from_block(block: &Block, out: &mut Vec<Annotation>) {
    for decl in block.declarations() {
        collect_annotations_from_decl(&decl, out);
    }
}

/// Try to extract a `type:` string literal from a metadata block.
fn extract_type_annotation(meta_block: &Block, decl_name: &str) -> Option<Annotation> {
    for inner_decl in meta_block.declarations() {
        let Some(head) = inner_decl.head() else {
            continue;
        };
        let ast::DeclarationKind::Property(prop) = head.classify_declaration() else {
            continue;
        };
        if prop.value() != "type" {
            continue;
        }

        // Found a `type:` field — extract its string literal value.
        let body = inner_decl.body()?;
        let body_soup = body.soup()?;
        let body_elems: Vec<ast::Element> = body_soup.elements().collect();

        if let Some(Element::Lit(lit)) = body_elems.first() {
            if let Some(lit_val) = lit.value() {
                if let Some(type_str) = lit_val.string_value() {
                    let node_offset = usize::from(lit.syntax().text_range().start());
                    return Some(Annotation {
                        source_offset: node_offset,
                        value: type_str.to_string(),
                        decl_name: decl_name.to_string(),
                    });
                }
            }
        }
    }
    None
}

/// Attempt to extract a human-readable name from a declaration head.
fn declaration_name(decl: &Declaration) -> String {
    let Some(head) = decl.head() else {
        return "<unknown>".to_string();
    };
    match head.classify_declaration() {
        ast::DeclarationKind::Property(id) => id.value().to_string(),
        ast::DeclarationKind::Function(name, _) => name.value().to_string(),
        _ => "<unknown>".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_errors_for_valid_annotations() {
        let src = r#"
` { type: "number -> number" }
double(x): x * 2
"#;
        assert_eq!(check_annotation_syntax("<test>", src, false), (0, 0));
    }

    #[test]
    fn reports_invalid_annotation() {
        let src = r#"
` { type: "number ->" }
bad(x): x
"#;
        assert_eq!(check_annotation_syntax("<test>", src, false), (1, 0));
    }

    #[test]
    fn ignores_declarations_without_type_annotation() {
        let src = r#"
` { doc: "just a doc" }
no_type(x): x
"#;
        assert_eq!(check_annotation_syntax("<test>", src, false), (0, 0));
    }

    #[test]
    fn multiple_valid_annotations() {
        let src = r#"
` { type: "(a -> b) -> [a] -> [b]" }
map(f, xs): __MAP

` { type: "(a -> bool) -> [a] -> [a]" }
filter(p, xs): __FILTER
"#;
        assert_eq!(check_annotation_syntax("<test>", src, false), (0, 0));
    }

    #[test]
    fn multiple_annotations_one_bad() {
        let src = r#"
` { type: "(a -> b) -> [a] -> [b]" }
map(f, xs): __MAP

` { type: "number ->" }
bad(x): x
"#;
        assert_eq!(check_annotation_syntax("<test>", src, false), (1, 0));
    }

    /// Regression for eu-3621: a genuine rowan parse error in the file body
    /// (not an annotation-string parse error) must be counted, not just
    /// printed. Previously `check_annotation_syntax` reported these via
    /// `eprintln!` but never incremented any counter, so `eu check --strict`
    /// exited 0 despite the file failing to parse at all.
    #[test]
    fn counts_file_parse_errors_separately_from_annotation_errors() {
        // `x: "hello" : "string"` is not valid syntax anywhere in Eucalypt —
        // a second top-level colon after a property's value is a malformed
        // declaration head, not an inline type ascription.
        let src = r#"
x: "hello" : "string"
"#;
        let (annotation_errors, file_parse_errors) = check_annotation_syntax("<test>", src, false);
        assert_eq!(annotation_errors, 0);
        assert!(
            file_parse_errors > 0,
            "expected at least one file-level parse error"
        );
    }

    /// Regression for eu-3621: `eu check` — with or without `--strict` —
    /// must exit non-zero on a file with a genuine parse error, not just
    /// print it. This is the actual bug reported (`eu check --strict`
    /// exited 0 for a file that never parsed at all) and exercises the full
    /// `check()` decision path end-to-end, unlike
    /// `counts_file_parse_errors_separately_from_annotation_errors` above,
    /// which only checks `check_annotation_syntax`'s tallied counts in
    /// isolation. Both strict and non-strict are asserted here: a parse
    /// error is a more fundamental failure than a type warning (the file
    /// produced no syntax tree at all) and the plain `eu` eval path already
    /// aborts unconditionally on one, so `eu check` must not report success
    /// either way — see the rationale comment on the `parse_errors > 0`
    /// branch in `check`.
    #[test]
    fn check_exits_nonzero_on_genuine_parse_error_strict_and_non_strict() {
        use crate::driver::options::EucalyptCli;
        use clap::Parser;
        use std::io::Write;

        let mut file = tempfile::Builder::new()
            .suffix(".eu")
            .tempfile()
            .expect("create temp file");
        // Same malformed-declaration-head shape as the original bug report:
        // a second top-level colon is not valid syntax anywhere in Eucalypt.
        writeln!(file, "x: \"hello\" : \"string\"").expect("write temp file");
        let path = file.path().to_str().expect("utf8 path").to_string();

        for strict_flag in [true, false] {
            let mut args = vec!["eu", "check"];
            if strict_flag {
                args.push("--strict");
            }
            args.push(&path);
            let cli = EucalyptCli::try_parse_from(args).expect("should parse");
            let opt = EucalyptOptions::from(cli);
            let exit = check(&opt).expect("check() should not itself error");
            assert_eq!(
                exit,
                1,
                "eu check{} on a file with a genuine parse error must exit 1, got {}",
                if strict_flag { " --strict" } else { "" },
                exit
            );
        }
    }

    #[test]
    fn byte_offset_line_col_first_line() {
        let src = "hello world";
        assert_eq!(byte_offset_to_line_col(src, 0), "1:1");
        assert_eq!(byte_offset_to_line_col(src, 5), "1:6");
    }

    #[test]
    fn byte_offset_line_col_second_line() {
        let src = "line1\nline2";
        assert_eq!(byte_offset_to_line_col(src, 6), "2:1");
    }

    /// eu-rb5n / Wicket review: the baked `desugared_unit_cores` carry
    /// `Smid` values minted by the offline `cargo xtask prelude-compile` process's
    /// own `SourceMap`. `Smid` (`src/common/sourcemap.rs`) is a bare index
    /// into a process-local `Vec`, so those indices are foreign once
    /// injected into a fresh runtime `SourceMap` — if a diagnostic's
    /// location ever cites an expression *inside* an injected prelude
    /// definition (rather than the user's call site, which is what every
    /// diagnostic in the full corpus sweep cites today), the runtime
    /// `SourceMap` never registered that index.
    ///
    /// This test proves the failure mode is graceful, not misleading: it
    /// builds a synthetic "prelude" unit (injected in place of the real one
    /// via `run_type_checker_from_blob_core` — `lib/prelude.eu` is untouched)
    /// containing a genuine internal type mismatch (`probe: number ->
    /// number` called with a string) at an `App` node tagged with a `Smid`
    /// far outside any range this test's tiny `SourceMap` will ever
    /// register — mirroring how a large offline prelude compile mints
    /// thousands of indices that a small runtime check invocation never
    /// sees. The resulting diagnostic must still carry its message, just
    /// with no primary location label — never a crash, and never an
    /// aliased, wrong location.
    #[test]
    fn in_prelude_smid_degrades_gracefully_not_misleadingly() {
        use crate::common::sourcemap::Smid;
        use crate::core::expr::core;
        use clap::Parser as _;

        let type_val = core::str(Smid::default(), "number -> number");
        let meta_block = core::block(Smid::default(), [("type".to_string(), type_val)]);
        let probe_lambda = core::lam(
            Smid::default(),
            vec!["x".to_string()],
            core::var(Smid::default(), "x".to_string()),
        );
        let probe = core::meta(Smid::default(), probe_lambda, meta_block);

        // Far beyond anything this test's tiny SourceMap will ever register
        // (a handful of entries for build-meta.yaml, __io/__args, and one
        // small user file) — simulating a foreign index from a much larger
        // offline prelude.eu compile (thousands of entries; see the
        // `smidprobe` scratch measurement in the PR: 7550 valid Smids in the
        // baked "prelude" unit alone).
        let in_prelude_smid = Smid::fake(999_999);
        let bad_call = core::app(
            in_prelude_smid,
            core::var(Smid::default(), "probe".to_string()),
            vec![core::str(Smid::default(), "boom")],
        );

        let synthetic_prelude = core::let_(
            Smid::default(),
            vec![
                ("probe".to_string(), probe),
                ("entry".to_string(), bad_call),
            ],
            core::var(Smid::default(), "entry".to_string()),
        );

        let dir = tempfile::tempdir().expect("tempdir");
        let file_path = dir.path().join("probe.eu");
        std::fs::write(&file_path, "result: entry\n").expect("write temp file");

        let cli = crate::driver::options::EucalyptCli::try_parse_from([
            "eu",
            "check",
            "--strict",
            file_path.to_str().expect("utf8 path"),
        ])
        .expect("parse cli");
        let mut opt = EucalyptOptions::from(cli);
        opt.process_defaults().expect("process_defaults");

        // Only "prelude" is injected — "build"/"io"/"args" load normally, so
        // this exercises the exact same injection/fallback machinery
        // `bin/eu.rs` uses, just with a hand-built prelude standing in for
        // the real one.
        let prelude_units = vec![("prelude".to_string(), synthetic_prelude)];
        let result = run_type_checker_from_blob_core(&opt, &prelude_units)
            .expect("run_type_checker_from_blob_core");

        assert!(
            !result.warnings.is_empty(),
            "expected a type mismatch warning from the synthetic prelude's internal probe(\"boom\") call"
        );

        let diag = result.warnings[0].to_diagnostic(&result.source_map);
        assert!(
            diag.labels.is_empty(),
            "an out-of-range in-prelude Smid must degrade to 'no location label', \
             not alias to an unrelated (wrong) location: {diag:?}"
        );
        // The message itself must still be present and informative — only
        // the *location* degrades, never the diagnostic itself.
        assert!(
            !diag.message.is_empty(),
            "diagnostic message should survive even without a location"
        );
    }
}
