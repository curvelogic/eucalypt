//! WASM API for eucalypt — browser-facing evaluation interface.
//!
//! This module exposes a minimal JS API via wasm-bindgen:
//!
//! - `evaluate(source, format)` — evaluate eucalypt source and return JSON
//! - `formats()` — return a JSON array of supported output format names
//!
//! The implementation directly uses the core eucalypt pipeline without going
//! through the CLI driver, since `clap`, `dirs`, and related crates are not
//! available in WASM targets.

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::io;
use std::rc::Rc;

use codespan_reporting::files::Files;
use wasm_bindgen::prelude::*;

use crate::common::sourcemap::{Smid, SourceMap};
use crate::core::cook;
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
use codespan_reporting::files::SimpleFiles;

/// Step limit for WASM evaluation to prevent infinite loops from
/// hanging the browser tab.
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

/// Result of evaluating eucalypt source, serialised as JSON for JS callers.
///
/// On success:
/// ```json
/// { "success": true, "output": "---\nhello: world\n" }
/// ```
/// On failure:
/// ```json
/// { "success": false, "error": { "message": "...", "location": { ... } } }
/// ```
#[derive(serde::Serialize)]
struct EvalResult {
    success: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    output: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<ErrorInfo>,
}

#[derive(serde::Serialize)]
struct ErrorInfo {
    message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    location: Option<SourceLocation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    notes: Option<Vec<String>>,
}

#[derive(serde::Serialize)]
struct SourceLocation {
    line: usize,
    column: usize,
    end_line: usize,
    end_column: usize,
}

/// Evaluate eucalypt source code and return the result as a JSON string.
///
/// # Arguments
/// * `source` — eucalypt source code
/// * `format` — output format: `"yaml"`, `"json"`, `"toml"`, `"text"`, `"edn"`, `"html"`
///
/// # Returns
/// A JSON string containing an `EvalResult`.
#[wasm_bindgen]
pub fn evaluate(source: &str, format: &str) -> String {
    let result = match evaluate_inner(source, format) {
        Ok(output) => EvalResult {
            success: true,
            output: Some(output),
            error: None,
        },
        Err(error_info) => EvalResult {
            success: false,
            output: None,
            error: Some(error_info),
        },
    };

    serde_json::to_string(&result).unwrap_or_else(|e| {
        format!(r#"{{"success":false,"error":{{"message":"serialisation error: {e}"}}}}"#)
    })
}

/// Return the list of supported output formats as a JSON array string.
#[wasm_bindgen]
pub fn formats() -> String {
    r#"["yaml","json","toml","text","edn","html"]"#.to_string()
}

// ── Internal pipeline ────────────────────────────────────────────────────────

fn evaluate_inner(source: &str, format: &str) -> Result<String, ErrorInfo> {
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
    // Prelude parse errors are internal — treat as hard failures without location
    if !prelude_parse.errors().is_empty() {
        return Err(ErrorInfo {
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
            |e| ErrorInfo {
                message: format!("Internal error loading build metadata: {e}"),
                location: None,
                notes: None,
            },
        )?;

    // 3. Parse user source
    let source_file_id = files.add("<input>".to_string(), source.to_string());
    let source_parse = rowan::parse_unit(source);
    if !source_parse.errors().is_empty() {
        let first_error = &source_parse.errors()[0];
        let location = extract_parse_error_location(first_error, source_file_id, &files);
        let message = source_parse
            .errors()
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("; ");
        return Err(ErrorInfo {
            message: format!("Parse error: {message}"),
            location,
            notes: None,
        });
    }
    let source_ast = source_parse.tree();

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
            .map_err(|e| core_error_to_info(e, &source_map))?
    };

    // 5. Desugar user source
    let source_input = Input::new(Locator::Literal("<input>".to_string()), None, "eu");
    let source_unit = {
        let mut contents: HashMap<Input, Content> = HashMap::new();
        contents.insert(
            source_input.clone(),
            Content::new(source_file_id, &source_ast),
        );
        let mut d = Desugarer::new(&contents, &mut source_map);
        d.translate_unit(&source_input)
            .map_err(|e| core_error_to_info(e, &source_map))?
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
    .map_err(|e| core_error_to_info(e, &source_map))?;

    let mut expr = merged.expr;

    // 8. Cook — operator precedence and expression anaphora
    expr = cook::cook(expr).map_err(|e| core_error_to_info(e, &source_map))?;

    // 9. Inline (2 passes)
    for _ in 0..2 {
        expr = tag::tag_combinators(&expr).map_err(|e| core_error_to_info(e, &source_map))?;
        expr = reduce::inline_pass(&expr).map_err(|e| core_error_to_info(e, &source_map))?;
    }

    // 10. Fuse destructure patterns
    expr = fuse::fuse(&expr).map_err(|e| core_error_to_info(e, &source_map))?;

    // 11. Prune unused bindings (2 passes)
    for _ in 0..2 {
        expr = prune::prune(&expr);
        expr = compress::compress(&expr).map_err(|e| core_error_to_info(e, &source_map))?;
    }

    // 12. Compile to STG.
    //
    // Use RenderDoc directly (not Headless) since WASM does not support IO
    // monads, so we skip the headless→world-injection→RenderDoc dance that
    // the CLI driver performs.
    let stg_settings = StgSettings {
        generate_annotations: true,
        render_type: RenderType::RenderDoc,
        heap_limit_mib: Some(256),
        ..Default::default()
    };

    let rt = make_standard_runtime(&mut source_map);
    let syn = stg::compile(&stg_settings, expr, rt.as_ref())
        .map_err(|e| execution_error_to_info(e.into(), &files, &source_map))?;

    // 13. Run the machine, capturing output via SharedWriter.
    //
    // We use `Rc<RefCell<Vec<u8>>>` instead of `&mut Vec<u8>` to avoid the
    // borrow-checker conflict: the emitter borrows from the writer with
    // lifetime 'a, and Machine<'a> holds the emitter. With a raw `&mut` we
    // cannot read the Vec after machine.take_emitter() because the compiler
    // sees the borrow as potentially lasting until Machine<'a> is dropped.
    // The Rc clone lets us access the buffer independently once the machine
    // and writer are both dropped.
    let output_buf: Rc<RefCell<Vec<u8>>> = Rc::new(RefCell::new(Vec::new()));
    let mut writer = SharedWriter(output_buf.clone());
    let mut emitter = export::create_emitter(format, &mut writer).ok_or_else(|| ErrorInfo {
        message: format!("Unknown output format: {format}"),
        location: None,
        notes: Some(vec![
            "Supported formats: yaml, json, toml, text, edn, html".to_string()
        ]),
    })?;

    emitter.stream_start();
    let mut machine = standard_machine(&stg_settings, syn, emitter, rt.as_ref())
        .map_err(|e| execution_error_to_info(e, &files, &source_map))?;

    let ret = machine.run(Some(MAX_STEPS));
    machine.take_emitter().stream_end();

    // Drop machine and writer so output_buf becomes the sole Rc owner.
    drop(machine);
    drop(writer);

    ret.map_err(|e| execution_error_to_info(e, &files, &source_map))?;

    let output = Rc::try_unwrap(output_buf)
        .expect("output_buf should have no remaining owners")
        .into_inner();

    String::from_utf8(output).map_err(|e| ErrorInfo {
        message: format!("Output encoding error: {e}"),
        location: None,
        notes: None,
    })
}

// ── Error conversion helpers ─────────────────────────────────────────────────

fn core_error_to_info(e: crate::core::error::CoreError, source_map: &SourceMap) -> ErrorInfo {
    let diag = e.to_diagnostic(source_map);
    ErrorInfo {
        message: diag.message.clone(),
        location: None,
        notes: if diag.notes.is_empty() {
            None
        } else {
            Some(diag.notes.clone())
        },
    }
}

fn execution_error_to_info(
    e: crate::eval::error::ExecutionError,
    files: &SimpleFiles<String, String>,
    source_map: &SourceMap,
) -> ErrorInfo {
    let diag = e.to_diagnostic(source_map);
    ErrorInfo {
        message: diag.message.clone(),
        location: extract_location(&diag, files),
        notes: if diag.notes.is_empty() {
            None
        } else {
            Some(diag.notes.clone())
        },
    }
}

/// Extract source location from the first primary label of a diagnostic.
fn extract_location(
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
