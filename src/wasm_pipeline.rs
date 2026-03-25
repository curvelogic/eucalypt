//! Core evaluation pipeline shared between the WASM API and native tests.
//!
//! This module extracts the evaluate-from-source logic so it can be exercised
//! by `#[cfg(test)]` unit tests on native targets without requiring
//! `wasm_bindgen` or `serde`.
//!
//! Two entry points:
//! - [`evaluate_unit`] — parses input as a unit (declarations), like a `.eu` file
//! - [`evaluate_expr`] — parses input as an expression, like CLI `-e`
//!
//! The WASM module (`wasm.rs`) exposes both as `evaluate` and `evaluate_expr`.

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::io;
use std::rc::Rc;

use codespan_reporting::files::Files;
use codespan_reporting::files::SimpleFiles;

use crate::common::sourcemap::{Smid, SourceMap};
use crate::core::cook;
use crate::core::desugar::desugarable::Desugarable;
use crate::core::desugar::{Content, Desugarer};
use crate::core::inline::{reduce, tag};
use crate::core::simplify::{compress, prune};
use crate::core::transform::fuse;
use crate::core::unit::TranslationUnit;
use crate::driver::io::{create_args_pseudoblock, create_io_pseudoblock};
use crate::driver::resources::Resources;
use crate::eval::machine::standard_machine;
use crate::eval::stg::{self, make_standard_runtime, RenderType, StgSettings};
use crate::export;
use crate::import;
use crate::syntax::input::{Input, Locator};
use crate::syntax::rowan;

/// Step limit for evaluation to prevent infinite loops from hanging the
/// browser tab (or a test runner).
const MAX_STEPS: usize = 1_000_000;

/// A `Write` implementation backed by an `Rc<RefCell<Vec<u8>>>`.
///
/// This avoids the borrow-checker conflict that arises when `&mut Vec<u8>` is
/// lent to an emitter that is then moved into the machine: the `Rc` clone held
/// outside the machine can be dereferenced after the machine is dropped.
struct SharedWriter(Rc<RefCell<Vec<u8>>>);

impl io::Write for SharedWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.borrow_mut().extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

// ── Error type ───────────────────────────────────────────────────────────────

/// Source location within the input, expressed as 1-based line/column pairs.
#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

/// Error returned by [`evaluate_pipeline`].
#[derive(Debug)]
pub struct PipelineError {
    pub message: String,
    pub location: Option<SourceLocation>,
    pub notes: Option<Vec<String>>,
}

impl fmt::Display for PipelineError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

// ── Pipeline ─────────────────────────────────────────────────────────────────

/// Evaluate a eucalypt unit (declarations) and return the rendered output.
///
/// Input is parsed as a unit — the same as a `.eu` file.  This is the
/// default mode for the playground.
pub fn evaluate_unit(source: &str, format: &str) -> Result<String, PipelineError> {
    run_pipeline(source, format, ParseMode::Unit)
}

/// Evaluate a eucalypt expression and return the rendered output.
///
/// Input is parsed as a bare expression — the same as CLI `-e`.
pub fn evaluate_expr(source: &str, format: &str) -> Result<String, PipelineError> {
    run_pipeline(source, format, ParseMode::Expr)
}

enum ParseMode {
    Unit,
    Expr,
}

fn run_pipeline(source: &str, format: &str, mode: ParseMode) -> Result<String, PipelineError> {
    let mut files: SimpleFiles<String, String> = SimpleFiles::new();
    let mut source_map = SourceMap::new();
    let resources = Resources::default();

    // 1. Parse prelude (embedded at compile time via Resources)
    let prelude_text = resources
        .get("prelude")
        .expect("prelude is embedded")
        .clone();
    let prelude_file_id = files.add("prelude".to_string(), prelude_text.clone());
    let prelude_parse = rowan::parse_unit(&prelude_text);
    if !prelude_parse.errors().is_empty() {
        return Err(PipelineError {
            message: "Internal error: prelude parse failed".to_string(),
            location: None,
            notes: Some(
                prelude_parse
                    .errors()
                    .iter()
                    .map(|e| e.to_string())
                    .collect(),
            ),
        });
    }
    let prelude_ast = prelude_parse.tree();

    // 2. Parse build-meta YAML (embedded at compile time)
    let build_meta_text = resources
        .get("build-meta")
        .expect("build-meta is embedded")
        .clone();
    let build_meta_file_id = files.add("build-meta.yaml".to_string(), build_meta_text);
    let build_meta_core =
        import::read_to_core("yaml", &mut files, &mut source_map, build_meta_file_id).map_err(
            |e| PipelineError {
                message: format!("Internal error loading build metadata: {e}"),
                location: None,
                notes: None,
            },
        )?;

    // 3. Parse user source — mode determines the parser entry point
    let source_file_id = files.add("<input>".to_string(), source.to_string());
    let source_ast: Box<dyn Desugarable> = match mode {
        ParseMode::Unit => {
            let parse = rowan::parse_unit(source);
            check_parse_errors(parse.errors(), source_file_id, &files)?;
            Box::new(parse.tree())
        }
        ParseMode::Expr => {
            let parse = rowan::parse_expr(source);
            check_parse_errors(parse.errors(), source_file_id, &files)?;
            Box::new(parse.tree())
        }
    };

    // 4. Desugar prelude
    let prelude_input = Input::new(Locator::Resource("prelude".to_string()), None, "eu");
    let prelude_unit = {
        let mut contents: HashMap<Input, Content> = HashMap::new();
        contents.insert(
            prelude_input.clone(),
            Content::new(prelude_file_id, &prelude_ast),
        );
        let mut d = Desugarer::new(&contents, &mut source_map);
        d.translate_unit(&prelude_input)
            .map_err(|e| core_error_to_pipeline_error(e, &source_map))?
    };

    // 5. Desugar user source
    let source_input = Input::new(Locator::Literal("<input>".to_string()), None, "eu");
    let source_unit = {
        let mut contents: HashMap<Input, Content> = HashMap::new();
        contents.insert(
            source_input.clone(),
            Content::new(source_file_id, source_ast.as_ref()),
        );
        let mut d = Desugarer::new(&contents, &mut source_map);
        d.translate_unit(&source_input)
            .map_err(|e| core_error_to_pipeline_error(e, &source_map))?
    };

    // 6. Create TranslationUnits for pseudo-inputs.
    //
    // Order matches the CLI driver: __args, __io, __build are prepended in
    // that order (each at front), giving final merge order:
    //   __args, __io, __build, prelude, user_source
    let args_unit = TranslationUnit {
        expr: create_args_pseudoblock(&[]).apply_name(Smid::default(), "__args"),
        targets: HashSet::new(),
        docs: Vec::new(),
    };
    let io_unit = TranslationUnit {
        expr: create_io_pseudoblock(None).apply_name(Smid::default(), "__io"),
        targets: HashSet::new(),
        docs: Vec::new(),
    };
    let build_unit = TranslationUnit {
        expr: build_meta_core.apply_name(Smid::default(), "__build"),
        targets: HashSet::new(),
        docs: Vec::new(),
    };

    // 7. Merge all units
    let merged = TranslationUnit::merge(
        [args_unit, io_unit, build_unit, prelude_unit, source_unit].into_iter(),
    )
    .map_err(|e| core_error_to_pipeline_error(e, &source_map))?;

    let mut expr = merged.expr;

    // 8. Cook — operator precedence and expression anaphora
    expr = cook::cook(expr).map_err(|e| core_error_to_pipeline_error(e, &source_map))?;

    // 9. Inline (2 passes)
    for _ in 0..2 {
        expr = tag::tag_combinators(&expr)
            .map_err(|e| core_error_to_pipeline_error(e, &source_map))?;
        expr =
            reduce::inline_pass(&expr).map_err(|e| core_error_to_pipeline_error(e, &source_map))?;
    }

    // 10. Fuse destructure patterns
    expr = fuse::fuse(&expr).map_err(|e| core_error_to_pipeline_error(e, &source_map))?;

    // 11. Prune unused bindings (2 passes)
    for _ in 0..2 {
        expr = prune::prune(&expr);
        expr =
            compress::compress(&expr).map_err(|e| core_error_to_pipeline_error(e, &source_map))?;
    }

    // 12. Compile to STG.
    //
    // Use RenderDoc directly (not Headless) since WASM does not support IO
    // monads, so we skip the headless->world-injection->RenderDoc dance that
    // the CLI driver performs.
    let stg_settings = StgSettings {
        generate_annotations: true,
        render_type: RenderType::RenderDoc,
        heap_limit_mib: Some(256),
        ..Default::default()
    };

    let rt = make_standard_runtime(&mut source_map);
    let syn = stg::compile(&stg_settings, expr, rt.as_ref())
        .map_err(|e| execution_error_to_pipeline_error(e.into(), &files, &source_map))?;

    // 13. Run the machine, capturing output via SharedWriter.
    let output_buf: Rc<RefCell<Vec<u8>>> = Rc::new(RefCell::new(Vec::new()));
    let mut writer = SharedWriter(output_buf.clone());
    let mut emitter = export::create_emitter(format, &mut writer).ok_or_else(|| PipelineError {
        message: format!("Unknown output format: {format}"),
        location: None,
        notes: Some(vec![
            "Supported formats: yaml, json, toml, text, edn, html".to_string()
        ]),
    })?;

    emitter.stream_start();
    let mut machine = standard_machine(&stg_settings, syn, emitter, rt.as_ref())
        .map_err(|e| execution_error_to_pipeline_error(e, &files, &source_map))?;

    let ret = machine.run(Some(MAX_STEPS));
    machine.take_emitter().stream_end();

    // Drop machine and writer so output_buf becomes the sole Rc owner.
    drop(machine);
    drop(writer);

    ret.map_err(|e| execution_error_to_pipeline_error(e, &files, &source_map))?;

    let output = Rc::try_unwrap(output_buf)
        .expect("output_buf should have no remaining owners")
        .into_inner();

    String::from_utf8(output).map_err(|e| PipelineError {
        message: format!("Output encoding error: {e}"),
        location: None,
        notes: None,
    })
}

// ── Parse error checking ─────────────────────────────────────────────────────

fn check_parse_errors(
    errors: &[crate::syntax::rowan::ParseError],
    source_file_id: usize,
    files: &SimpleFiles<String, String>,
) -> Result<(), PipelineError> {
    if errors.is_empty() {
        return Ok(());
    }
    let first_error = &errors[0];
    let location = extract_parse_error_location(first_error, source_file_id, files);
    let message = errors
        .iter()
        .map(|e| e.to_string())
        .collect::<Vec<_>>()
        .join("; ");
    Err(PipelineError {
        message: format!("Parse error: {message}"),
        location,
        notes: None,
    })
}

// ── Error conversion helpers ─────────────────────────────────────────────────

fn core_error_to_pipeline_error(
    e: crate::core::error::CoreError,
    source_map: &SourceMap,
) -> PipelineError {
    let diag = e.to_diagnostic(source_map);
    PipelineError {
        message: diag.message.clone(),
        location: None,
        notes: if diag.notes.is_empty() {
            None
        } else {
            Some(diag.notes.clone())
        },
    }
}

fn execution_error_to_pipeline_error(
    e: crate::eval::error::ExecutionError,
    files: &SimpleFiles<String, String>,
    source_map: &SourceMap,
) -> PipelineError {
    let diag = e.to_diagnostic(source_map);
    PipelineError {
        message: diag.message.clone(),
        location: extract_diagnostic_location(&diag, files),
        notes: if diag.notes.is_empty() {
            None
        } else {
            Some(diag.notes.clone())
        },
    }
}

/// Extract source location from the first primary label of a diagnostic.
fn extract_diagnostic_location(
    diag: &codespan_reporting::diagnostic::Diagnostic<usize>,
    files: &SimpleFiles<String, String>,
) -> Option<SourceLocation> {
    let label = diag.labels.first()?;
    let start = files.location(label.file_id, label.range.start).ok()?;
    let end = files.location(label.file_id, label.range.end).ok()?;
    Some(SourceLocation {
        line: start.line_number,
        column: start.column_number,
        end_line: end.line_number,
        end_column: end.column_number,
    })
}

/// Extract source location from a rowan parse error's text range.
fn extract_parse_error_location(
    error: &crate::syntax::rowan::ParseError,
    file_id: usize,
    files: &SimpleFiles<String, String>,
) -> Option<SourceLocation> {
    let range = parse_error_range(error)?;
    let start_offset = usize::from(range.start());
    let end_offset = usize::from(range.end());
    let start = files.location(file_id, start_offset).ok()?;
    let end = files.location(file_id, end_offset).ok()?;
    Some(SourceLocation {
        line: start.line_number,
        column: start.column_number,
        end_line: end.line_number,
        end_column: end.column_number,
    })
}

/// Extract the primary text range from a parse error.
fn parse_error_range(error: &rowan::ParseError) -> Option<::rowan::TextRange> {
    use crate::syntax::rowan::ParseError as PE;
    match error {
        PE::UnexpectedToken { range, .. } => Some(*range),
        PE::UnclosedSingleQuote { range } => Some(*range),
        PE::UnclosedDoubleQuote { range } => Some(*range),
        PE::InvalidParenExpr { range, .. } => Some(*range),
        PE::UnterminatedBlock { range, .. } => Some(*range),
        PE::EmptyDeclarationBody { range } => Some(*range),
        PE::MissingDeclarationColon { head_range } => Some(*head_range),
        PE::MalformedDeclarationHead { range } => Some(*range),
        PE::InvalidFormalParameter { range, .. } => Some(*range),
        PE::InvalidOperatorName { range, .. } => Some(*range),
        PE::InvalidPropertyName { range, .. } => Some(*range),
        PE::SurplusContent { range } => Some(*range),
        PE::ReservedCharacter { range } => Some(*range),
        PE::EmptyExpression { range } => Some(*range),
        PE::UnclosedStringInterpolation { range } => Some(*range),
        PE::InvalidZdtLiteral { range, .. } => Some(*range),
        PE::InvalidDoubleColon { range } => Some(*range),
        PE::UnclosedBracketExpr { range } => Some(*range),
        PE::MismatchedBrackets { close_range, .. } => Some(*close_range),
        PE::UnknownBracketPair { range, .. } => Some(*range),
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── Unit mode (declaration-style, like a .eu file) ──────────────

    #[test]
    fn test_unit_json() {
        let result = evaluate_unit("x: 1", "json").unwrap();
        let v: serde_json::Value = serde_json::from_str(result.trim()).unwrap();
        assert_eq!(v["x"], 1);
    }

    #[test]
    fn test_unit_yaml() {
        let result = evaluate_unit("hello: \"world\"", "yaml").unwrap();
        assert!(result.contains("hello:"));
    }

    #[test]
    fn test_unit_nested() {
        let result = evaluate_unit("a: {b: 2}", "json").unwrap();
        let v: serde_json::Value = serde_json::from_str(result.trim()).unwrap();
        assert_eq!(v["a"]["b"], 2);
    }

    #[test]
    fn test_unit_list() {
        let result = evaluate_unit("items: [1, 2, 3]", "json").unwrap();
        let v: serde_json::Value = serde_json::from_str(result.trim()).unwrap();
        assert_eq!(v["items"], serde_json::json!([1, 2, 3]));
    }

    #[test]
    fn test_unit_booleans() {
        let result = evaluate_unit("t: true\nf: false", "json").unwrap();
        let v: serde_json::Value = serde_json::from_str(result.trim()).unwrap();
        assert_eq!(v["t"], true);
        assert_eq!(v["f"], false);
    }

    #[test]
    fn test_unit_expression() {
        let result = evaluate_unit("x: 2 + 3", "json").unwrap();
        let v: serde_json::Value = serde_json::from_str(result.trim()).unwrap();
        assert_eq!(v["x"], 5);
    }

    #[test]
    fn test_unit_parse_error() {
        let result = evaluate_unit("{{{{", "json");
        assert!(result.is_err());
    }

    #[test]
    fn test_unit_unknown_format() {
        let result = evaluate_unit("x: 1", "nosuchformat");
        assert!(result.is_err());
    }

    // ── Expression mode (like CLI -e) ───────────────────────────────

    #[test]
    fn test_expr_block() {
        let result = evaluate_expr("{x: 1}", "json").unwrap();
        let v: serde_json::Value = serde_json::from_str(result.trim()).unwrap();
        assert_eq!(v["x"], 1);
    }

    #[test]
    fn test_expr_bare_number() {
        let result = evaluate_expr("42", "json").unwrap();
        let v: serde_json::Value = serde_json::from_str(result.trim()).unwrap();
        assert_eq!(v, 42);
    }

    #[test]
    fn test_expr_string() {
        let result = evaluate_expr("\"hello\"", "text").unwrap();
        assert_eq!(result.trim(), "hello");
    }

    #[test]
    fn test_expr_list() {
        let result = evaluate_expr("[1, 2, 3]", "json").unwrap();
        let v: serde_json::Value = serde_json::from_str(result.trim()).unwrap();
        assert_eq!(v, serde_json::json!([1, 2, 3]));
    }

    #[test]
    fn test_expr_step_limit() {
        let result = evaluate_expr("{ f(x): f(x)  main: f(0) }.main", "json");
        assert!(result.is_err());
    }
}
