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
use std::collections::{BTreeMap, HashMap, HashSet};
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
use crate::eval::bytecode::{encode, BytecodeMachine};
use crate::eval::stg::{self, make_standard_runtime, runtime::Runtime, RenderType, StgSettings};
use crate::export;
use crate::import;
use crate::syntax::import::{scrape_soup_imports, scrape_unit_imports, ImportError, ImportGraph};
use crate::syntax::input::{Input, Locator};
use crate::syntax::rowan;

/// Default step limit for evaluation, so an infinite loop ends with an
/// error rather than hanging the browser tab (or a test runner).
/// [`EvalOptions::max_steps`] overrides it.
pub const DEFAULT_MAX_STEPS: usize = 1_000_000;

// ── Options ──────────────────────────────────────────────────────────────────

/// Options for [`evaluate_unit_with`] and [`evaluate_expr_with`].
#[derive(Debug, Clone)]
pub struct EvalOptions {
    /// Files that imports resolve against, by path (`helpers.eu`,
    /// `data/input.txt`). Paths are normalised (`./` and `..`).
    pub files: BTreeMap<String, String>,
    /// The machine's step limit: evaluation that runs longer fails with
    /// a "did not terminate" error.
    pub max_steps: usize,
}

impl Default for EvalOptions {
    fn default() -> Self {
        EvalOptions {
            files: BTreeMap::new(),
            max_steps: DEFAULT_MAX_STEPS,
        }
    }
}

/// The JSON form of [`EvalOptions`] the WASM API accepts.
#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
struct EvalOptionsJson {
    #[serde(default)]
    files: BTreeMap<String, String>,
    #[serde(default)]
    max_steps: Option<u64>,
}

impl EvalOptions {
    /// Read options from JSON:
    /// `{ "files": { "<path>": "<text>", ... }, "max_steps": <n> }`.
    /// Every key is optional; an empty string means the defaults. An
    /// unknown key, a path that names nothing or climbs above the root,
    /// two paths that normalise to one, and a `max_steps` that is not a
    /// positive integer are errors.
    pub fn from_json(json: &str) -> Result<Self, PipelineError> {
        let options_error = |message: String| PipelineError {
            message: format!("invalid options: {message}"),
            location: None,
            notes: None,
        };
        if json.trim().is_empty() {
            return Ok(EvalOptions::default());
        }
        let raw: EvalOptionsJson =
            serde_json::from_str(json).map_err(|e| options_error(e.to_string()))?;
        let mut files = BTreeMap::new();
        for (path, text) in raw.files {
            let normalised = normalise_path(&path)
                .ok_or_else(|| options_error(format!("file path {path:?} is not usable")))?;
            if files.insert(normalised.clone(), text).is_some() {
                return Err(options_error(format!(
                    "more than one file has the path {normalised:?}"
                )));
            }
        }
        let max_steps = match raw.max_steps {
            None => DEFAULT_MAX_STEPS,
            Some(0) => return Err(options_error("max_steps must be at least 1".to_string())),
            Some(n) => usize::try_from(n)
                .map_err(|_| options_error(format!("max_steps must be at most {}", usize::MAX)))?,
        };
        Ok(EvalOptions { files, max_steps })
    }
}

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
    /// The file the location is in: `<input>` for the source evaluated,
    /// else the path of a supplied file or an embedded library's name.
    pub file: String,
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
    evaluate_unit_with(source, format, &EvalOptions::default())
}

/// [`evaluate_unit`] with options: files that imports resolve against,
/// and a step limit.
pub fn evaluate_unit_with(
    source: &str,
    format: &str,
    options: &EvalOptions,
) -> Result<String, PipelineError> {
    run_pipeline(source, format, ParseMode::Unit, options)
}

/// Evaluate a eucalypt expression and return the rendered output.
///
/// Input is parsed as a bare expression — the same as CLI `-e`.
pub fn evaluate_expr(source: &str, format: &str) -> Result<String, PipelineError> {
    evaluate_expr_with(source, format, &EvalOptions::default())
}

/// [`evaluate_expr`] with options: files that imports resolve against,
/// and a step limit.
pub fn evaluate_expr_with(
    source: &str,
    format: &str,
    options: &EvalOptions,
) -> Result<String, PipelineError> {
    run_pipeline(source, format, ParseMode::Expr, options)
}

enum ParseMode {
    Unit,
    Expr,
}

fn run_pipeline(
    source: &str,
    format: &str,
    mode: ParseMode,
    options: &EvalOptions,
) -> Result<String, PipelineError> {
    let mut files: SimpleFiles<String, String> = SimpleFiles::new();
    let mut source_map = SourceMap::new();
    let resources = Resources::default();

    // 1. Parse prelude (embedded at compile time via Resources)
    let prelude_text = resources
        .get("prelude")
        .expect("prelude is embedded")
        .clone();
    let prelude_file_id = files.add("prelude".to_string(), prelude_text.clone());
    source_map.mark_resource_file(prelude_file_id);
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

    // 3. Parse user source — mode determines the parser entry point.
    //
    // Use `into_parts()` to always obtain the partial tree even when there are
    // parse errors.  ERROR_STOWAWAYS nodes in the partial tree produce
    // `ErrEliminated` sentinels during desugaring; DCE removes them so clean
    // sub-expressions still evaluate.  If an `ErrEliminated` node survives to
    // STG compilation a `CompileError` is returned.  Parse errors are collected
    // and included as notes on any downstream `PipelineError` so the caller
    // sees the full diagnostic picture.  Unlike the prelude (Step 1), a broken
    // user input is not fatal — the partial tree is always used.
    let source_file_id = files.add("<input>".to_string(), source.to_string());
    let (source_ast, source_imports, parse_notes): (Box<dyn Desugarable>, _, Vec<String>) =
        match mode {
            ParseMode::Unit => {
                let (tree, errors) = rowan::parse_unit(source).into_parts();
                let notes = format_parse_error_notes(&errors, source_file_id, &files);
                let imports = scrape_unit_imports(&tree);
                (Box::new(tree), imports, notes)
            }
            ParseMode::Expr => {
                let (tree, errors) = rowan::parse_expr(source).into_parts();
                let notes = format_parse_error_notes(&errors, source_file_id, &files);
                let imports = scrape_soup_imports(&tree);
                (Box::new(tree), imports, notes)
            }
        };
    let source_input = Input::new(Locator::Literal("<input>".to_string()), None, "eu");
    // 3a. Load the embedded libraries the source imports, and theirs in
    // turn. A failure keeps the parse errors as notes: while the source is
    // being typed, a parse error is often the real cause.
    let with_parse_notes = |mut e: PipelineError| {
        if !parse_notes.is_empty() {
            e.notes
                .get_or_insert_with(Vec::new)
                .extend(parse_notes.iter().cloned());
        }
        e
    };
    let libraries = source_imports
        .map_err(|e| PipelineError {
            message: e.to_string(),
            location: None,
            notes: None,
        })
        .and_then(|imports| {
            load_libraries(
                &source_input,
                imports,
                &resources,
                &options.files,
                &mut files,
                &mut source_map,
            )
        })
        .map_err(with_parse_notes)?;

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

    // 5. Desugar user source, with the libraries it imports
    let source_unit = {
        let mut contents: HashMap<Input, Content> = HashMap::new();
        contents.insert(
            source_input.clone(),
            Content::new(source_file_id, source_ast.as_ref()),
        );
        for library in &libraries.loaded {
            contents.insert(
                library.input.clone(),
                Content::new(library.file_id, library.parsed.as_desugarable()),
            );
        }
        for (input, index) in &libraries.aliases {
            let library = &libraries.loaded[*index];
            contents.insert(
                input.clone(),
                Content::new(library.file_id, library.parsed.as_desugarable()),
            );
        }
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
        own_targets: HashSet::new(),
        docs: Vec::new(),
        deprecations: HashMap::new(),
        blame: HashMap::new(),
    };
    let io_unit = TranslationUnit {
        expr: create_io_pseudoblock(None).apply_name(Smid::default(), "__io"),
        targets: HashSet::new(),
        own_targets: HashSet::new(),
        docs: Vec::new(),
        deprecations: HashMap::new(),
        blame: HashMap::new(),
    };
    let build_unit = TranslationUnit {
        expr: build_meta_core.apply_name(Smid::default(), "__build"),
        targets: HashSet::new(),
        own_targets: HashSet::new(),
        docs: Vec::new(),
        deprecations: HashMap::new(),
        blame: HashMap::new(),
    };

    // 7. Merge all units
    let merged = TranslationUnit::merge(
        [args_unit, io_unit, build_unit, prelude_unit, source_unit].into_iter(),
    )
    .map_err(|e| core_error_to_pipeline_error(e, &source_map))?;

    let mut expr = merged.expr;

    // 8. Cook — operator precedence and expression anaphora
    let cooked = cook::cook(expr, &mut source_map);
    expr = cooked.map_err(|e| core_error_to_pipeline_error(e, &source_map))?;

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
    let syn = stg::compile(&stg_settings, expr, rt.as_ref()).map_err(|e| {
        // Include any parse errors as notes so the caller sees the full
        // diagnostic picture when an ErrEliminated node survives to here.
        let mut pe = execution_error_to_pipeline_error(e.into(), &files, &source_map);
        if !parse_notes.is_empty() {
            pe.notes
                .get_or_insert_with(Vec::new)
                .extend(parse_notes.iter().cloned());
        }
        pe
    })?;

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

    // The wasm sink is an in-memory Vec, so these writes cannot fail; keep
    // the errors visible as pipeline errors rather than ignoring them, so a
    // future sink that can fail is not silently truncated (eu-1tkk.7.25).
    emitter.stream_start().map_err(|e| PipelineError {
        message: format!("Failed writing output: {e}"),
        location: None,
        notes: None,
    })?;
    let (prog, root, gforms) = encode(&syn, &rt.globals());
    let mut machine = BytecodeMachine::new(
        prog,
        root,
        &gforms,
        rt.intrinsics(),
        emitter,
        stg_settings.heap_limit_mib.unwrap_or(0),
        stg_settings.test_mode,
    )
    .map_err(|e| execution_error_to_pipeline_error(e, &files, &source_map))?;

    let ret = machine.run(Some(options.max_steps));
    let stream_end = machine.take_emitter().stream_end();

    // Drop machine and writer so output_buf becomes the sole Rc owner.
    drop(machine);
    drop(writer);

    ret.map_err(|e| {
        let step_limit = matches!(e, crate::eval::error::ExecutionError::DidntTerminate(_));
        let mut pe = execution_error_to_pipeline_error(e, &files, &source_map);
        if step_limit {
            pe.notes.get_or_insert_with(Vec::new).push(format!(
                "The step limit is {}; the max_steps option raises it.",
                options.max_steps
            ));
        }
        pe
    })?;
    stream_end.map_err(|e| PipelineError {
        message: format!("Failed writing output: {e}"),
        location: None,
        notes: None,
    })?;

    let output = Rc::try_unwrap(output_buf)
        .expect("output_buf should have no remaining owners")
        .into_inner();

    String::from_utf8(output).map_err(|e| PipelineError {
        message: format!("Output encoding error: {e}"),
        location: None,
        notes: None,
    })
}

// ── Imports ──────────────────────────────────────────────────────────────────

/// Where an import's content came from: a file the caller supplied, or a
/// library embedded in eucalypt. With the format, this identifies one
/// parsed and desugared content.
#[derive(Clone, PartialEq, Eq)]
enum Origin {
    /// A supplied file, by its normalised path.
    File(String),
    /// An embedded library, by name (`lens`).
    Library(&'static str),
}

/// An import's parsed content: an eucalypt unit, or data already read to
/// core.
enum Parsed {
    Unit(rowan::ast::Unit),
    Data(crate::core::expr::RcExpr),
}

impl Parsed {
    fn as_desugarable(&self) -> &dyn Desugarable {
        match self {
            Parsed::Unit(unit) => unit,
            Parsed::Data(expr) => expr,
        }
    }
}

/// An import a unit makes, loaded and parsed.
struct Library {
    origin: Origin,
    format: String,
    /// The import that loaded it: the desugarer finds its content under
    /// this key, or by its locator when it is imported again under
    /// another name.
    input: Input,
    file_id: usize,
    parsed: Parsed,
}

/// The library name an import gives, if it is a eucalypt import that
/// could name one: `lens.eu` (the CLI falls back to a baked-in library by
/// file stem) or `resource:lens`.
fn import_library_name(import: &Input) -> Option<&str> {
    if import.format() != "eu" {
        return None;
    }
    match import.locator() {
        Locator::Fs(path) => path.file_stem()?.to_str(),
        Locator::Resource(name) => Some(name.as_str()),
        _ => None,
    }
}

/// Normalise a `/`-separated relative path: drop empty and `.`
/// components and resolve `..`. `None` for a path that climbs above its
/// root or names nothing.
fn normalise_path(path: &str) -> Option<String> {
    let mut parts: Vec<&str> = Vec::new();
    for part in path.split(['/', '\\']) {
        match part {
            "" | "." => {}
            ".." => {
                parts.pop()?;
            }
            p => parts.push(p),
        }
    }
    if parts.is_empty() {
        None
    } else {
        Some(parts.join("/"))
    }
}

/// The directory part of a normalised path (`""` at the root).
fn directory_of(path: &str) -> &str {
    path.rfind('/').map_or("", |i| &path[..i])
}

/// The imports loaded for a unit, and every other import that reaches
/// one of them (for example `lens.eu` and `resource:lens`), so that each
/// is parsed and desugared once.
#[derive(Default)]
struct Libraries {
    loaded: Vec<Library>,
    aliases: Vec<(Input, usize)>,
}

/// Load what `root` imports, and what those import in turn. An import
/// resolves to one of the caller's `files` — by its path as written,
/// then relative to the importing file's directory, as the CLI resolves
/// a file import — or else to an embedded library. Anything else fails,
/// as does an import cycle: the browser has no file system.
fn load_libraries(
    root: &Input,
    imports: Vec<Input>,
    resources: &Resources,
    supplied: &BTreeMap<String, String>,
    files: &mut SimpleFiles<String, String>,
    source_map: &mut SourceMap,
) -> Result<Libraries, PipelineError> {
    let import_error = |e: ImportError| PipelineError {
        message: e.to_string(),
        location: None,
        notes: None,
    };
    // Look files up by normalised path, whoever built the options.
    let supplied: &BTreeMap<String, String> = &supplied
        .iter()
        .filter_map(|(path, text)| Some((normalise_path(path)?, text.clone())))
        .collect();
    let mut graph = ImportGraph::default();
    let mut libraries = Libraries::default();
    // Each importer with its imports, and the directory its own relative
    // imports also resolve against.
    let mut pending = vec![(root.clone(), imports, String::new())];

    while let Some((importer, imports, importer_dir)) = pending.pop() {
        let imports = graph
            .register_imports(importer, imports)
            .map_err(import_error)?;
        graph.check_for_cycles().map_err(import_error)?;
        for import in imports {
            // Seen before: the desugarer finds content by the import as
            // written, so one locator and format is one content, even
            // where relative resolution could have read it differently.
            let known =
                |i: &Input| i.locator() == import.locator() && i.format() == import.format();
            if libraries.loaded.iter().any(|l| known(&l.input))
                || libraries.aliases.iter().any(|(i, _)| known(i))
            {
                continue;
            }
            let format = import.format().to_string();
            let origin = resolve_import(&import, &importer_dir, resources, supplied)
                .ok_or_else(|| cannot_import(&import, supplied))?;
            if let Some(index) = libraries
                .loaded
                .iter()
                .position(|l| l.origin == origin && l.format == format)
            {
                libraries.aliases.push((import, index));
                continue;
            }
            let (file_name, text, embedded) = match &origin {
                Origin::File(path) => (path.clone(), supplied[path].clone(), false),
                Origin::Library(name) => {
                    let (_, text) = resources
                        .eu_library(name)
                        .expect("resolved library is embedded");
                    (format!("{name}.eu"), text.clone(), true)
                }
            };
            let file_id = files.add(file_name.clone(), text.clone());
            if embedded {
                source_map.mark_resource_file(file_id);
            }
            let parsed = if format == "eu" {
                let (ast, errors) = rowan::parse_unit(&text).into_parts();
                if !errors.is_empty() {
                    let message = if embedded {
                        format!("Internal error: {file_name} parse failed")
                    } else {
                        format!("cannot parse {file_name}")
                    };
                    return Err(PipelineError {
                        message,
                        location: None,
                        notes: Some(format_parse_error_notes(&errors, file_id, files)),
                    });
                }
                let nested = scrape_unit_imports(&ast).map_err(import_error)?;
                let dir = match &origin {
                    Origin::File(path) => directory_of(path).to_string(),
                    Origin::Library(_) => String::new(),
                };
                pending.push((import.clone(), nested, dir));
                Parsed::Unit(ast)
            } else if format.ends_with("-stream") {
                return Err(PipelineError {
                    message: format!("cannot import {}", import.locator()),
                    location: None,
                    notes: Some(vec![format!(
                        "The streaming format {format} is not available here."
                    )]),
                });
            } else {
                let core =
                    import::read_to_core(&format, files, source_map, file_id).map_err(|e| {
                        PipelineError {
                            message: format!("cannot import {file_name} as {format}: {e}"),
                            location: None,
                            notes: None,
                        }
                    })?;
                Parsed::Data(core)
            };
            libraries.loaded.push(Library {
                origin,
                format,
                input: import,
                file_id,
                parsed,
            });
        }
    }
    Ok(libraries)
}

/// Where `import` resolves: a supplied file (as written, then relative to
/// `importer_dir`), else an embedded library.
fn resolve_import(
    import: &Input,
    importer_dir: &str,
    resources: &Resources,
    supplied: &BTreeMap<String, String>,
) -> Option<Origin> {
    if let Locator::Fs(path) = import.locator() {
        let path = path.to_str()?;
        let as_written = normalise_path(path);
        let relative = if importer_dir.is_empty() {
            None
        } else {
            normalise_path(&format!("{importer_dir}/{path}"))
        };
        if let Some(found) = [as_written, relative]
            .into_iter()
            .flatten()
            .find(|p| supplied.contains_key(p))
        {
            return Some(Origin::File(found));
        }
    }
    import_library_name(import)
        .and_then(|name| resources.eu_library(name))
        .map(|(name, _)| Origin::Library(name))
}

fn cannot_import(import: &Input, supplied: &BTreeMap<String, String>) -> PipelineError {
    let libraries = Resources::eu_library_names()
        .map(|l| format!("{l}.eu"))
        .collect::<Vec<_>>()
        .join(", ");
    let note = if supplied.is_empty() {
        format!("Only the libraries embedded in eucalypt can be imported here: {libraries}.")
    } else {
        format!(
            "Only the files supplied ({}) and the libraries embedded in eucalypt ({libraries}) can be imported here.",
            supplied.keys().cloned().collect::<Vec<_>>().join(", ")
        )
    };
    PipelineError {
        message: format!("cannot import {}", import.locator()),
        location: None,
        notes: Some(vec![note]),
    }
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
    let diag = e.to_diagnostic_with_source(source_map, files);
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
        file: files.name(label.file_id).ok()?,
        line: start.line_number,
        column: start.column_number,
        end_line: end.line_number,
        end_column: end.column_number,
    })
}

/// Format parse errors as human-readable strings with source locations.
fn format_parse_error_notes(
    errors: &[crate::syntax::rowan::ParseError],
    file_id: usize,
    files: &SimpleFiles<String, String>,
) -> Vec<String> {
    errors
        .iter()
        .map(|e| {
            if let Some(loc) = extract_parse_error_location(e, file_id, files) {
                if loc.file == "<input>" {
                    format!("{}:{}: parse error: {e}", loc.line, loc.column)
                } else {
                    format!("{}:{}:{}: parse error: {e}", loc.file, loc.line, loc.column)
                }
            } else {
                format!("parse error: {e}")
            }
        })
        .collect()
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
        file: files.name(file_id).ok()?,
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
        PE::InvalidStringInterpolation { range } => Some(*range),
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
        // Under resilient parsing the partial tree may evaluate cleanly (Ok)
        // or fail at a later stage (Err).  Both are acceptable; when Err the
        // parse errors must appear in the notes.
        match result {
            Ok(_) => {}
            Err(e) => {
                let notes = e.notes.as_deref().unwrap_or(&[]);
                assert!(
                    notes.iter().any(|n| n.to_lowercase().contains("parse")),
                    "Err result must carry parse-error notes; got: {notes:?}"
                );
            }
        }
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

    #[test]
    fn test_import_lens_library() {
        let src = r#"{ import: "lens.eu" }
config: { db: { port: 5432 } }
port: config view(‹:db :port›)
bumped: config over(‹:db :port›, + 1)"#;
        let result = evaluate_unit(src, "json").unwrap();
        let v: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert_eq!(v["port"], 5432);
        assert_eq!(v["bumped"]["db"]["port"], 5433);
    }

    #[test]
    fn test_import_state_library_monadic_block() {
        let src = r#"{ import: "state.eu" }
` :suppress
tick: { :state
  n: state.query(_.count)
  _: state.put(:count, n + 1)
}.(n)
result: state.run(tick, { count: 41 }).state"#;
        let result = evaluate_unit(src, "json").unwrap();
        let v: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert_eq!(v["result"]["count"], 42);
    }

    #[test]
    fn test_import_named_library() {
        let src = r#"{ import: "optics=lens.eu" }
x: { a: 1 } optics.view(optics.at(:a))"#;
        let result = evaluate_unit(src, "json").unwrap();
        assert!(result.contains("\"x\": 1"), "got {result}");
    }

    #[test]
    fn test_import_of_a_file_fails_clearly() {
        let err = evaluate_unit(
            r#"{ import: "helpers.eu" }
x: 1"#,
            "json",
        )
        .unwrap_err();
        assert!(err.message.contains("cannot import"), "{}", err.message);
        assert!(err.notes.unwrap()[0].contains("lens.eu"));
    }

    #[test]
    fn test_import_of_a_data_resource_fails_clearly() {
        let err = evaluate_unit(
            r#"{ import: "package.eu" }
x: 1"#,
            "json",
        )
        .unwrap_err();
        assert!(err.message.contains("cannot import"), "{}", err.message);
        assert!(!err.notes.unwrap()[0].contains("package"));
    }

    #[test]
    fn test_import_error_keeps_parse_notes() {
        let err = evaluate_unit(
            r#"{ import: "helpers.eu" }
x: (1"#,
            "json",
        )
        .unwrap_err();
        assert!(err.message.contains("cannot import"), "{}", err.message);
        assert!(err.notes.unwrap().len() > 1);
    }

    // ── Options: supplied files and the step limit ─────────────────

    fn options(files: &[(&str, &str)], max_steps: Option<usize>) -> EvalOptions {
        EvalOptions {
            files: files
                .iter()
                .map(|(p, t)| (p.to_string(), t.to_string()))
                .collect(),
            max_steps: max_steps.unwrap_or(DEFAULT_MAX_STEPS),
        }
    }

    fn json(result: Result<String, PipelineError>) -> serde_json::Value {
        let output = result.unwrap_or_else(|e| panic!("{e}: {:?}", e.notes));
        serde_json::from_str(output.trim()).unwrap()
    }

    #[test]
    fn test_import_supplied_eu_file() {
        let opts = options(&[("helpers.eu", "double(x): x * 2")], None);
        let v = json(evaluate_unit_with(
            "{ import: \"helpers.eu\" }\nx: double(21)",
            "json",
            &opts,
        ));
        assert_eq!(v["x"], 42);
    }

    #[test]
    fn test_import_supplied_eu_file_from_expression() {
        let opts = options(&[("helpers.eu", "double(x): x * 2")], None);
        let v = json(evaluate_expr_with(
            "{ { import: \"helpers.eu\" } r: double(4) }.r",
            "json",
            &opts,
        ));
        assert_eq!(v, 8);
    }

    #[test]
    fn test_import_supplied_data_files() {
        let opts = options(
            &[("cfg.yaml", "port: 80\n"), ("./data/input.txt", "a\nb\n")],
            None,
        );
        let v = json(evaluate_unit_with(
            "{ import: [\"cfg=cfg.yaml\", \"lines=text@data/input.txt\"] }\n\
             port: cfg.port\nn: lines count",
            "json",
            &opts,
        ));
        assert_eq!(v["port"], 80);
        assert_eq!(v["n"], 2);
    }

    #[test]
    fn test_one_file_imported_in_two_formats() {
        let opts = options(&[("pts.json", "[1, 2, 3]")], None);
        let v = json(evaluate_unit_with(
            "{ import: [\"raw=text@pts.json\", \"pts=pts.json\"] }\n\
             n: raw count\nm: pts count",
            "json",
            &opts,
        ));
        assert_eq!(v["n"], 1);
        assert_eq!(v["m"], 3);
    }

    #[test]
    fn test_nested_import_resolves_relative_to_importer() {
        let opts = options(
            &[
                ("lib/a.eu", "{ import: \"b.eu\" }\na: b + 1"),
                ("lib/b.eu", "b: 1"),
            ],
            None,
        );
        let v = json(evaluate_unit_with(
            "{ import: \"lib/a.eu\" }\nx: a",
            "json",
            &opts,
        ));
        assert_eq!(v["x"], 2);
    }

    #[test]
    fn test_supplied_file_wins_over_embedded_library() {
        let opts = options(&[("lens.eu", "marker: 7")], None);
        let v = json(evaluate_unit_with(
            "{ import: \"lens.eu\" }\nx: marker",
            "json",
            &opts,
        ));
        assert_eq!(v["x"], 7);
    }

    #[test]
    fn test_embedded_library_still_imports_with_files_supplied() {
        let opts = options(&[("helpers.eu", "double(x): x * 2")], None);
        let v = json(evaluate_unit_with(
            "{ import: [\"helpers.eu\", \"lens.eu\"] }\nx: { a: 3 } view(at(:a)) double",
            "json",
            &opts,
        ));
        assert_eq!(v["x"], 6);
    }

    #[test]
    fn test_missing_import_lists_supplied_files() {
        let opts = options(&[("helpers.eu", "double(x): x * 2")], None);
        let err = evaluate_unit_with("{ import: \"other.eu\" }\nx: 1", "json", &opts).unwrap_err();
        assert!(err.message.contains("cannot import"), "{}", err.message);
        let note = &err.notes.unwrap()[0];
        assert!(
            note.contains("helpers.eu") && note.contains("lens.eu"),
            "{note}"
        );
    }

    #[test]
    fn test_parse_error_in_supplied_file_names_it() {
        let opts = options(&[("helpers.eu", "double(x): (x * 2")], None);
        let err =
            evaluate_unit_with("{ import: \"helpers.eu\" }\nx: 1", "json", &opts).unwrap_err();
        assert!(err.message.contains("helpers.eu"), "{}", err.message);
        assert!(
            err.notes
                .unwrap()
                .iter()
                .any(|n| n.starts_with("helpers.eu:1:")),
            "notes should locate the error in helpers.eu"
        );
    }

    #[test]
    fn test_error_location_names_supplied_file() {
        let opts = options(&[("helpers.eu", "\n\nboom: panic(\"no\")")], None);
        let err =
            evaluate_unit_with("{ import: \"helpers.eu\" }\nx: boom", "json", &opts).unwrap_err();
        let loc = err.location.expect("error should have a location");
        assert_eq!(loc.file, "helpers.eu");
        assert_eq!(loc.line, 3);
    }

    /// A program that needs a few million steps.
    const LONG_RUNNING: &str = "count(n): if(n = 0, 0, count(n - 1))\nx: count(200000)";

    #[test]
    fn test_default_step_limit_stops_a_long_program() {
        let err = evaluate_unit(LONG_RUNNING, "json").unwrap_err();
        assert!(err.message.contains("did not terminate"), "{}", err.message);
        assert!(err.notes.unwrap().iter().any(|n| n.contains("max_steps")));
    }

    #[test]
    fn test_raised_step_limit_lets_a_long_program_finish() {
        let v = json(evaluate_unit_with(
            LONG_RUNNING,
            "json",
            &options(&[], Some(100_000_000)),
        ));
        assert_eq!(v["x"], 0);
    }

    #[test]
    fn test_lowered_step_limit_stops_a_short_program() {
        let err = evaluate_unit_with("x: 1 + 2", "json", &options(&[], Some(5))).unwrap_err();
        assert!(err.message.contains("after 5 steps"), "{}", err.message);
    }

    #[test]
    fn test_options_from_json() {
        let o = EvalOptions::from_json("").unwrap();
        assert_eq!(o.max_steps, DEFAULT_MAX_STEPS);
        assert!(o.files.is_empty());
        let o = EvalOptions::from_json(r#"{"files": {"./a/../b.eu": "x: 1"}, "max_steps": 9}"#)
            .unwrap();
        assert_eq!(o.max_steps, 9);
        assert_eq!(o.files.keys().collect::<Vec<_>>(), vec!["b.eu"]);
    }

    #[test]
    fn test_options_from_json_rejects_bad_options() {
        for bad in [
            r#"{"maxSteps": 5}"#,
            r#"{"max_steps": 0}"#,
            r#"{"max_steps": -1}"#,
            r#"{"files": {"../up.eu": ""}}"#,
            r#"{"files": {"a.eu": "", "./a.eu": ""}}"#,
            r#"{"files": ["a.eu"]}"#,
            "not json",
        ] {
            let err = EvalOptions::from_json(bad).unwrap_err();
            assert!(
                err.message.starts_with("invalid options"),
                "{bad}: {}",
                err.message
            );
        }
    }
}
