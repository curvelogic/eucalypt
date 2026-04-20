//! Type annotation check command (`eu check`).
//!
//! Two phases:
//!
//! 1. **Annotation syntax check** — parse each source file's rowan AST,
//!    find all `type:` metadata annotations, and validate them against the
//!    type grammar.  This is fast and requires no pipeline processing.
//!
//! 2. **Bidirectional type check** — load source files through the full
//!    eucalypt pipeline (parse → desugar → cook → eliminate), then run
//!    `Checker` over the resulting core expression and report any
//!    `TypeWarning`s.
//!
//! Type issues are always warnings unless `--strict` is passed.

use std::fs;
use std::path::Path;
use std::time::Instant;

use crate::core::typecheck::{
    check::{type_check, type_check_full},
    parse,
};
use crate::driver::error::EucalyptError;
use crate::driver::options::EucalyptOptions;
use crate::driver::source::SourceLoader;
use crate::syntax::rowan::ast::{self, Block, Declaration, Element, HasSoup};
use crate::syntax::rowan::parse_unit;
use rowan::ast::AstNode;

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

        syntax_errors += check_annotation_syntax(&path_str, &source, opt.check_strict());
    }
    let phase1_elapsed = t_phase1.elapsed();

    // ── Phase 2: bidirectional type check via pipeline ─────────────────────
    let t_phase2 = Instant::now();
    let type_warning_count = match run_type_checker(opt) {
        Ok(result) => {
            let count = result.warnings.len();
            let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
                codespan_reporting::term::termcolor::ColorChoice::Auto,
            );
            let config = codespan_reporting::term::Config::default();
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
            count
        }
        Err(e) => {
            // Pipeline failure is not a type error — log at debug level and
            // continue with the annotation syntax results only.
            eprintln!("eu check: pipeline warning: {e}");
            0
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

    let total_issues = syntax_errors + type_warning_count;

    if total_issues == 0 {
        Ok(0)
    } else if opt.check_strict() {
        Ok(1)
    } else {
        // Warnings never cause non-zero exit unless --strict.
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
        if let Err(e) = parse::parse_type(&ann.value) {
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
/// If the pipeline fails at any stage, returns a single warning
/// explaining which stage failed rather than silently producing no
/// results.
pub fn type_check_path_full(path: &Path) -> PathCheckResult {
    use crate::core::typecheck::error::TypeWarning;
    use crate::syntax::input::{Input, Locator};

    let pipeline_error = |stage: &str, err: &dyn std::fmt::Display| PathCheckResult {
        warnings: vec![TypeWarning::new(format!(
            "type checking unavailable: {stage} failed: {err}"
        ))],
        types: std::collections::HashMap::new(),
        source_map: crate::common::sourcemap::SourceMap::new(),
    };

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

    let core_expr = loader.core().expr.clone();
    let result = type_check_full(&core_expr);
    let (_, source_map, _) = loader.complete();
    PathCheckResult {
        warnings: result.warnings,
        types: result.types,
        source_map,
    }
}

/// Result of running the type checker through the full pipeline.
struct PipelineCheckResult {
    warnings: Vec<crate::core::typecheck::error::TypeWarning>,
    files: codespan_reporting::files::SimpleFiles<String, String>,
    source_map: crate::common::sourcemap::SourceMap,
}

/// Run the bidirectional type checker by loading files through the pipeline.
///
/// Returns warnings, the SimpleFiles (for rendering), and the SourceMap (for
/// resolving Smids to source locations).  Returns an `Err` if the pipeline
/// fails (e.g. parse error in the source) — the caller logs this and continues
/// with only the annotation syntax check results.
fn run_type_checker(opt: &EucalyptOptions) -> Result<PipelineCheckResult, EucalyptError> {
    let mut loader = SourceLoader::new(opt.lib_path().to_vec());
    let inputs = opt.inputs();

    // Load all inputs (prelude + user files).
    for input in &inputs {
        loader.load(input)?;
    }

    // Translate each input to core.
    for input in &inputs {
        loader.translate(input)?;
    }

    // Merge into a single expression.
    loader.merge_units(&inputs)?;

    // Cook (resolve operator precedence).
    loader.cook()?;

    // Eliminate dead bindings.
    loader.eliminate()?;

    // Run the type checker.
    let core_expr = loader.core().expr.clone();
    let warnings = type_check(&core_expr);
    let (files, source_map, _) = loader.complete();
    Ok(PipelineCheckResult {
        warnings,
        files,
        source_map,
    })
}

// ── Annotation syntax check helpers ─────────────────────────────────────────

/// Check annotation syntax in a single source string.
///
/// Returns the number of annotation syntax errors found.
fn check_annotation_syntax(filename: &str, source: &str, strict: bool) -> usize {
    let parse_result = parse_unit(source);

    // Report any eucalypt syntax errors first (always real errors).
    for err in parse_result.errors() {
        eprintln!("{filename}: parse error: {err}");
    }

    let unit = parse_result.tree();
    let annotations = collect_annotations_from_unit(&unit, source);

    let mut error_count = 0;
    for ann in annotations {
        if let Err(e) = parse::parse_type(&ann.value) {
            let severity = if strict { "error" } else { "warning" };
            let line_col = byte_offset_to_line_col(source, ann.source_offset);
            eprintln!(
                "{filename}:{line_col}: {severity}: invalid type annotation on '{}': {}",
                ann.decl_name, e
            );
            error_count += 1;
        }
    }

    error_count
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
        assert_eq!(check_annotation_syntax("<test>", src, false), 0);
    }

    #[test]
    fn reports_invalid_annotation() {
        let src = r#"
` { type: "number ->" }
bad(x): x
"#;
        assert_eq!(check_annotation_syntax("<test>", src, false), 1);
    }

    #[test]
    fn ignores_declarations_without_type_annotation() {
        let src = r#"
` { doc: "just a doc" }
no_type(x): x
"#;
        assert_eq!(check_annotation_syntax("<test>", src, false), 0);
    }

    #[test]
    fn multiple_valid_annotations() {
        let src = r#"
` { type: "(a -> b) -> [a] -> [b]" }
map(f, xs): __MAP

` { type: "(a -> bool) -> [a] -> [a]" }
filter(p, xs): __FILTER
"#;
        assert_eq!(check_annotation_syntax("<test>", src, false), 0);
    }

    #[test]
    fn multiple_annotations_one_bad() {
        let src = r#"
` { type: "(a -> b) -> [a] -> [b]" }
map(f, xs): __MAP

` { type: "number ->" }
bad(x): x
"#;
        assert_eq!(check_annotation_syntax("<test>", src, false), 1);
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
}
