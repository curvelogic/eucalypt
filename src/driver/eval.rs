//! Evaluate core
//!
//! This might be by various strategies including delegating to
//! another implementation or interpreting core directly
use crate::{
    common::{prettify, sourcemap::*},
    core::{expr::*, typecheck::error::TypeWarning},
    driver::{
        error::EucalyptError,
        io_run::{inject_world_and_run, io_run_and_render, render_headless_result, IoRunError},
        options::{ErrorFormat, EucalyptOptions},
        source::SourceLoader,
    },
    eval::{
        error::ExecutionError,
        machine::standard_machine,
        memory::infotable::InfoTable,
        stg::{self, make_standard_runtime, RenderType, StgSettings},
    },
    export,
};

/// Try to load the pre-compiled prelude blob and return it.
///
/// Returns `None` when:
/// - the blob is unavailable or stale (`cfg(prelude_blob_stale)`)
/// - `--source-prelude` was requested
/// - `EU_SOURCE_PRELUDE=1` is set
#[cfg(not(target_arch = "wasm32"))]
pub fn maybe_load_prelude_blob(
    opt: &EucalyptOptions,
) -> Option<crate::eval::stg::blob::PreludeBlob> {
    // Source-prelude override takes priority.
    if opt.source_prelude || std::env::var("EU_SOURCE_PRELUDE").as_deref() == Ok("1") {
        return None;
    }

    #[cfg(prelude_blob_ok)]
    {
        let bytes = crate::driver::resources::PRELUDE_BLOB_BYTES;
        match crate::eval::stg::blob::PreludeBlob::from_bytes(bytes) {
            Ok(blob) => Some(blob),
            Err(e) => {
                eprintln!("warning: failed to load prelude blob, falling back to source: {e}");
                None
            }
        }
    }

    #[cfg(not(prelude_blob_ok))]
    None
}
use codespan_reporting::{
    diagnostic::Diagnostic,
    files::{Files, SimpleFiles},
    term::{
        self,
        termcolor::{ColorChoice, NoColor, StandardStream},
    },
};

use std::{io::Write, time::Instant};

use super::statistics::Statistics;

/// Convert an `IoRunError` to an `ExecutionError`, using structured variants
/// for user-facing errors and `Panic` for internal machine errors.
fn io_run_error_to_execution(e: IoRunError) -> ExecutionError {
    match e {
        IoRunError::IoNotAllowed(smid) => ExecutionError::IoNotAllowed(smid),
        IoRunError::Fail(msg) => ExecutionError::IoFail(Smid::default(), msg),
        IoRunError::Timeout(smid, secs) => ExecutionError::IoTimeout(smid, secs),
        IoRunError::CommandError(smid, msg) => ExecutionError::IoCommandError(smid, msg),
        IoRunError::MachineError(boxed) => *boxed,
    }
}

/// Collect machine, GC, and heap statistics from the VM after all
/// execution phases (eval, render, IO) have completed.
fn collect_machine_stats(machine: &crate::eval::machine::vm::Machine<'_>, stats: &mut Statistics) {
    use crate::eval::machine::metrics::ThreadOccupation;

    // GC phase timings
    for (k, v) in machine.clock().report() {
        stats.timings_mut().record(k, v);
    }

    // Machine counters
    stats.set_ticks(machine.metrics().ticks());
    stats.set_allocs(machine.metrics().allocs());
    stats.set_max_stack(machine.metrics().max_stack());

    // Heap stats
    let heap_stats = machine.heap_stats();
    stats.set_blocks_allocated(heap_stats.blocks_allocated);
    stats.set_lobs_allocated(heap_stats.lobs_allocated);
    stats.set_blocks_used(heap_stats.used);
    stats.set_blocks_recycled(heap_stats.recycled);
    stats.set_collections_count(heap_stats.collections_count);
    stats.set_minor_collections(heap_stats.minor_collections);
    stats.set_major_collections(heap_stats.major_collections);
    stats.set_peak_heap_blocks(heap_stats.peak_heap_blocks);

    // Aggregate GC timings
    stats.set_total_mark_time(machine.clock().duration(ThreadOccupation::CollectorMark));
    stats.set_total_sweep_time(machine.clock().duration(ThreadOccupation::CollectorSweep));
}

/// Run the prepared core expression and output to selected emitter
/// Run result: statistics and an optional process exit code override.
///
/// The exit code is `Some(130)` when the program was interrupted by
/// SIGINT and `None` for normal completion.
pub struct RunResult {
    pub stats: Statistics,
    pub exit_code: Option<u8>,
}

pub fn run(opt: &EucalyptOptions, mut loader: SourceLoader) -> Result<RunResult, EucalyptError> {
    let format = determine_format(opt, &loader);
    let mut stats = Statistics::default();
    // Extract the blob before the loader is consumed by Executor::from().
    // The blob was loaded once in bin/eu.rs and is passed through without
    // a second deserialisation.
    #[cfg(not(target_arch = "wasm32"))]
    let blob = loader.take_prelude_blob();
    let mut executor = Executor::from(loader);
    #[cfg(not(target_arch = "wasm32"))]
    executor.set_prelude_blob(blob);
    let exit_code = executor
        .execute(opt, &mut stats, format)
        .map_err(|e| EucalyptError::Execution(Box::new(e)))?;
    Ok(RunResult { stats, exit_code })
}

/// Determine the output format from options and targets
fn determine_format(opt: &EucalyptOptions, loader: &SourceLoader) -> String {
    let target_format = opt
        .target()
        .and_then(|n| loader.core().target(n))
        .and_then(|t| t.format().as_ref());

    let main_format = loader
        .core()
        .target("main")
        .and_then(|t| t.format().as_ref());

    let opt_format = opt.export_type();

    opt_format
        .or(target_format)
        .or(main_format)
        .unwrap_or(&"yaml".to_string())
        .to_string()
}

#[allow(dead_code)]
pub struct Executor<'a> {
    /// Access to source file text for error reporting
    files: SimpleFiles<String, String>,

    /// SourceMap for references to source locations across all files
    source_map: SourceMap,

    /// Core expression to evaluate
    evaluand: RcExpr,

    /// Overriden output stream
    out: Option<Box<dyn Write + 'a>>,

    /// Error stream
    err: Option<Box<dyn Write + 'a>>,

    /// Pre-compiled prelude blob, loaded once and passed through from
    /// the source loader.  When `Some`, the runtime global table is extended
    /// with pre-compiled prelude `LambdaForm`s and the STG compiler resolves
    /// prelude free variables to `Ref::G` rather than raising `CompileError::FreeVar`.
    #[cfg(not(target_arch = "wasm32"))]
    prelude_blob: Option<crate::eval::stg::blob::PreludeBlob>,
}

impl From<SourceLoader> for Executor<'_> {
    fn from(loader: SourceLoader) -> Self {
        let (files, source_map, evaluand) = loader.complete();
        Self::new(files, source_map, evaluand)
    }
}

impl<'a> Executor<'a> {
    /// Construct a new Executor, taking ownership of reporting
    /// resources and the evaluand
    pub fn new(
        files: SimpleFiles<String, String>,
        source_map: SourceMap,
        evaluand: RcExpr,
    ) -> Self {
        Executor {
            files,
            source_map,
            evaluand,
            out: None,
            err: None,
            #[cfg(not(target_arch = "wasm32"))]
            prelude_blob: None,
        }
    }

    /// Attach a pre-compiled prelude blob.
    ///
    /// Called by `run()` after extracting the blob from the `SourceLoader`
    /// (which has already used it to seed the cook `Distributor`).
    /// Must be called before `execute()`.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn set_prelude_blob(&mut self, blob: Option<crate::eval::stg::blob::PreludeBlob>) {
        self.prelude_blob = blob;
    }

    /// Provide override streams to capture the output to stdout and stderr
    pub fn capture_output(&mut self, out: Box<dyn Write + 'a>, err: Box<dyn Write + 'a>) {
        self.out = Some(out);
        self.err = Some(err);
    }

    /// Use the STG machine
    pub fn execute(
        &mut self,
        opt: &EucalyptOptions,
        stats: &mut Statistics,
        format: String,
    ) -> Result<Option<u8>, ExecutionError> {
        let result = self.try_execute(opt, stats, format);
        self.diagnose(result, opt.error_format())
    }

    /// Execute using the STG machine
    fn try_execute(
        &mut self,
        opt: &EucalyptOptions,
        stats: &mut Statistics,
        format: String,
    ) -> Result<Option<u8>, ExecutionError> {
        let mut output: Box<dyn Write>;
        if let Some(out) = &mut self.out {
            output = Box::new(out);
        } else if let Some(outfile) = opt.output() {
            output = Box::new(std::fs::File::create(outfile)?);
        } else {
            output = Box::new(std::io::stdout());
        }

        let mut emitter = export::create_emitter(&format, output.as_mut())
            .ok_or_else(|| ExecutionError::UnknownFormat(format.to_string()))?;

        // ── Pre-compiled prelude blob path ────────────────────────────────────
        // The blob was loaded once in `bin/eu.rs` (or `eval::run()`) and stored
        // on this executor via `set_prelude_blob()`.  Use it to populate the
        // runtime global table and inject the prelude name→slot map into the
        // STG compiler so that free prelude references resolve to Ref::G.
        let mut rt = make_standard_runtime(&mut self.source_map);

        #[cfg(not(target_arch = "wasm32"))]
        if let Some(ref b) = self.prelude_blob {
            // Build a slot-ordered name list from name_to_slot (name → slot index).
            let mut names = vec![String::new(); b.binding_entries.len()];
            for (name, &slot) in &b.name_to_slot {
                if slot < names.len() {
                    names[slot] = name.clone();
                }
            }
            rt.set_prelude_bindings(
                b.nodes.clone(),
                b.forms_pool.clone(),
                b.binding_entries.clone(),
                names,
            );
        }

        if opt.dump_runtime() {
            println!("{}", prettify::prettify(rt.as_ref()));
            Ok(None)
        } else {
            // Compile with the render wrapper so that evaluation and
            // rendering happen in a single pass — no double evaluation.
            //
            // RENDER_DOC uses an IO-transparent force at the top level:
            // if the expression evaluates to an IO constructor or an IO
            // function (PAP waiting for world), the machine yields.  The
            // driver then handles the IO loop and renders the final pure
            // value.  Non-IO documents render in-place with no second
            // pass.
            //
            // When options already specify RenderFragment (test/fragment
            // mode), keep that setting; otherwise use RenderDoc.
            let render_type = match opt.stg_settings().render_type {
                RenderType::RenderFragment => RenderType::RenderFragment,
                _ => RenderType::RenderDoc,
            };
            let mut stg_settings = StgSettings {
                render_type,
                ..opt.stg_settings().clone()
            };

            // Inject prelude global slot map from blob so the STG compiler can
            // resolve free variable references to prelude names as Ref::G.
            #[cfg(not(target_arch = "wasm32"))]
            if let Some(ref b) = self.prelude_blob {
                stg_settings.prelude_globals = Some(b.name_to_slot.clone());
            }

            // Run demand analysis on the core expression before STG compile
            if !stg_settings.suppress_demand_analysis {
                let t = Instant::now();
                let (annotated, _signatures, named_signatures) =
                    crate::core::analyse_demand::analyse_demands(&self.evaluand);
                self.evaluand = annotated;
                stg_settings.user_demand_sigs = named_signatures;
                stats.timings_mut().record("demand-analysis", t.elapsed());
            }

            // Re-flatten nested Let/LetRec scopes after demand analysis.
            // SCC splitting decomposes LetRec into chains of nested scopes
            // for precise demand analysis; this pass merges them back into
            // single scopes (single EnvFrame at runtime) while preserving
            // the per-binding demand annotations.
            {
                let t = Instant::now();
                self.evaluand = crate::core::reflatten::reflatten(&self.evaluand);
                stats.timings_mut().record("reflatten", t.elapsed());
            }

            if opt.dump_reflatten() {
                if opt.quote_debug() {
                    println!("{:#?}", self.evaluand);
                } else {
                    let pretty = crate::common::prettify::prettify(&self.evaluand);
                    println!("{pretty}");
                }
                return Ok(None);
            }

            let syn = {
                let t = Instant::now();
                let syn = stg::compile(&stg_settings, self.evaluand.clone(), rt.as_ref())?;
                stats.timings_mut().record("stg-compile", t.elapsed());
                syn
            };

            if opt.dump_stg() {
                println!("{}", prettify::prettify(&*syn));
                Ok(None)
            } else {
                emitter.stream_start();
                let mut machine = standard_machine(&stg_settings, syn, emitter, rt.as_ref())?;

                let t_exec = Instant::now();
                let ret = machine.run(None);
                stats.timings_mut().record("stg-eval", t_exec.elapsed());

                // RENDER_DOC wraps the expression with an IO-transparent
                // force.  After the first run:
                //
                // 1. Normal termination → non-IO document rendered in
                //    place (single pass, no double evaluation).
                //
                // 2. IO yield with a function closure (arity > 0) → the
                //    expression produced an IO PAP.  Inject world, run
                //    the io-run loop, then render the pure result.
                //
                // 3. IO yield with an IO data constructor → the expression
                //    produced an IO constructor directly.  Run the
                //    io-run loop, then render the pure result.
                let result = if ret.is_ok() {
                    if machine.io_yielded() {
                        // The IO-transparent force caught an IO value.
                        // Check if it's a function (PAP waiting for world)
                        // or an IO data constructor.
                        let is_function = machine.current_closure().arity() > 0;
                        if is_function {
                            // Inject world to saturate the IO function, then
                            // resume — the empty stack means the IO constructor
                            // will yield normally.
                            let io_yielded = inject_world_and_run(&mut machine)
                                .map_err(io_run_error_to_execution);
                            match io_yielded {
                                Ok(true) => {
                                    let t_io = Instant::now();
                                    let io_result = io_run_and_render(&mut machine, opt.allow_io)
                                        .map_err(io_run_error_to_execution);
                                    stats.timings_mut().record("io-run", t_io.elapsed());
                                    machine.take_emitter().stream_end();
                                    io_result
                                }
                                Ok(false) => {
                                    // Not actually an IO function — render it.
                                    let t_render = Instant::now();
                                    let render_result = render_headless_result(&mut machine)
                                        .map_err(io_run_error_to_execution);
                                    stats.timings_mut().record("stg-render", t_render.elapsed());
                                    machine.take_emitter().stream_end();
                                    render_result
                                }
                                Err(e) => {
                                    machine.take_emitter().stream_end();
                                    Err(e)
                                }
                            }
                        } else {
                            // IO data constructor — run the io-run loop.
                            let t_io = Instant::now();
                            let io_result = io_run_and_render(&mut machine, opt.allow_io)
                                .map_err(io_run_error_to_execution);
                            stats.timings_mut().record("io-run", t_io.elapsed());
                            machine.take_emitter().stream_end();
                            io_result
                        }
                    } else {
                        // Normal termination — render happened in-place.
                        machine.take_emitter().stream_end();
                        ret
                    }
                } else {
                    machine.take_emitter().stream_end();
                    ret
                };

                // Collect machine/GC statistics from all execution phases,
                // even on error (e.g. Interrupted) so that -S output is
                // available.
                collect_machine_stats(&machine, stats);

                result
            }
        }
    }

    /// Print any errors as diagnoses to stderr
    fn diagnose(
        &mut self,
        result: Result<Option<u8>, ExecutionError>,
        error_format: &ErrorFormat,
    ) -> Result<Option<u8>, ExecutionError> {
        match result {
            Err(ref e) if e.is_interrupted() => {
                eprintln!("\nInterrupted (Ctrl-C) — partial statistics follow");
                Ok(Some(130))
            }
            Err(e) => {
                match error_format {
                    ErrorFormat::Human => {
                        let mut diagnostic = e.to_diagnostic(&self.source_map);

                        let mut notes = vec![];

                        if let Some(trace) = e.stack_trace() {
                            let stack_trace = self.source_map.format_trace(trace, &self.files);
                            if !stack_trace.is_empty() {
                                notes.push(format!("stack trace:\n{stack_trace}"));
                            }
                        }

                        diagnostic = diagnostic.with_notes(notes);

                        self.diagnose_to_stderr(&diagnostic);
                    }
                    ErrorFormat::Json => {
                        let json = self.error_to_json(&e);
                        self.write_json_to_stderr(&json);
                    }
                }

                Err(e)
            }
            Ok(code) => Ok(code),
        }
    }

    /// Emit type warnings to stderr and return whether any were emitted.
    ///
    /// When `strict` is `true`, the caller should treat the presence of warnings
    /// as an error (non-zero exit).  This method always renders warnings as
    /// `Diagnostic::warning()` regardless of the strict flag — the caller is
    /// responsible for propagating the exit code.
    pub fn emit_warnings(&mut self, warnings: &[TypeWarning]) -> bool {
        if warnings.is_empty() {
            return false;
        }
        for warning in warnings {
            let diag = warning.to_diagnostic(&self.source_map);
            self.diagnose_to_stderr(&diag);
        }
        true
    }

    /// Print a diagnostic to stderr
    fn diagnose_to_stderr(&mut self, diag: &Diagnostic<usize>) {
        let config = codespan_reporting::term::Config::default();
        match self.err {
            None => {
                term::emit(
                    &mut StandardStream::stderr(ColorChoice::Auto),
                    &config,
                    &self.files,
                    diag,
                )
                .expect("failed to write diagnostic to stderr");
            }
            Some(ref mut err) => {
                term::emit(&mut NoColor::new(err.as_mut()), &config, &self.files, diag)
                    .expect("failed to write diagnostic to buffer");
            }
        }
    }

    /// Convert an execution error to a JSON value
    fn error_to_json(&self, error: &ExecutionError) -> serde_json::Value {
        let message = format!("{error}");

        // Resolve source location if available
        let location = self.source_map.source_info(error).and_then(|info| {
            let file_id = info.file?;
            let span = info.span?;
            let name = self.files.name(file_id).ok()?;
            let loc = self.files.location(file_id, span.start().to_usize()).ok()?;
            let end_loc = self.files.location(file_id, span.end().to_usize()).ok()?;
            Some(serde_json::json!({
                "file": name,
                "start": {
                    "line": loc.line_number,
                    "column": loc.column_number,
                },
                "end": {
                    "line": end_loc.line_number,
                    "column": end_loc.column_number,
                }
            }))
        });

        // Build stack trace if present, filtering internal machinery
        let stack_trace: Option<Vec<serde_json::Value>> = error.stack_trace().map(|trace| {
            trace
                .iter()
                .filter_map(|smid| {
                    let info = self.source_map.source_info_for_smid(*smid)?;

                    // Map intrinsic names to display names, filtering internal ones
                    let display_name = info.annotation.as_deref().and_then(intrinsic_display_name);

                    let source_snippet = || {
                        let id = info.file?;
                        let source: &str = self.files.source(id).ok()?;
                        let span = info.span?;
                        source.get(std::ops::Range::from(span))
                    };

                    let label = display_name.or_else(source_snippet)?;

                    let file_loc = info.file.and_then(|id| {
                        let name = self.files.name(id).ok()?;
                        let span = info.span?;
                        let loc = self.files.location(id, span.start().to_usize()).ok()?;
                        Some(serde_json::json!({
                            "file": name,
                            "line": loc.line_number,
                            "column": loc.column_number,
                        }))
                    });

                    let mut entry = serde_json::Map::new();
                    if let Some(loc) = file_loc {
                        entry.insert("location".to_string(), loc);
                    }
                    entry.insert(
                        "name".to_string(),
                        serde_json::Value::String(label.to_string()),
                    );
                    Some(serde_json::Value::Object(entry))
                })
                .collect()
        });

        let mut result = serde_json::json!({
            "severity": "error",
            "message": message,
        });

        if let Some(loc) = location {
            result["location"] = loc;
        }
        if let Some(trace) = stack_trace {
            if !trace.is_empty() {
                result["stack_trace"] = serde_json::Value::Array(trace);
            }
        }

        result
    }

    /// Write JSON error to stderr
    fn write_json_to_stderr(&mut self, json: &serde_json::Value) {
        let output = serde_json::to_string(json).expect("failed to serialise error as JSON");
        match self.err {
            None => {
                eprintln!("{output}");
            }
            Some(ref mut err) => {
                writeln!(err, "{output}").expect("failed to write JSON error to buffer");
            }
        }
    }
}
