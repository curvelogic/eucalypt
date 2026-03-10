//! Evaluate core
//!
//! This might be by various strategies including delegating to
//! another implementation or interpreting core directly
use crate::{
    common::{prettify, sourcemap::*},
    core::expr::*,
    driver::{
        error::EucalyptError,
        io_run::{inject_world_and_run, io_run_and_render},
        options::{ErrorFormat, EucalyptOptions},
        source::SourceLoader,
    },
    eval::{
        error::ExecutionError,
        machine::standard_machine,
        stg::{self, make_standard_runtime, RenderType, StgSettings},
    },
    export,
};
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

/// Run the prepared core expression and output to selected emitter
pub fn run(opt: &EucalyptOptions, loader: SourceLoader) -> Result<Statistics, EucalyptError> {
    let format = determine_format(opt, &loader);
    let mut stats = Statistics::default();
    let mut executor = Executor::from(loader);
    executor
        .execute(opt, &mut stats, format)
        .map_err(|e| EucalyptError::Execution(Box::new(e)))?;
    Ok(stats)
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
        }
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

        let rt = make_standard_runtime(&mut self.source_map);

        if opt.dump_runtime() {
            println!("{}", prettify::prettify(rt.as_ref()));
            Ok(None)
        } else {
            // Compile in headless mode so that IO constructors can yield to
            // the io-run driver rather than being passed to RENDER_DOC.
            //
            // After the first run we inspect the result:
            //
            // • IO yield directly → run the io-run loop.
            //
            // • Normal termination, WHNF is a function (arity > 0) → inject
            //   the world token and re-run.  If this yields IO, run the io-run
            //   loop; otherwise the program is erroneous (should not happen in
            //   well-typed code).
            //
            // • Normal termination, WHNF is a data value (arity = 0) → the
            //   program is a plain document.  Re-compile WITH RENDER_DOC and
            //   run a fresh machine so that the render is GC-safe.
            let stg_settings_headless = StgSettings {
                render_type: RenderType::Headless,
                ..opt.stg_settings().clone()
            };
            // Keep normal settings for plain-document fallback
            let stg_settings = &stg_settings_headless;

            let syn = {
                let t = Instant::now();
                let syn = stg::compile(stg_settings, self.evaluand.clone(), rt.as_ref())?;
                stats.timings_mut().record("stg-compile", t.elapsed());
                syn
            };

            if opt.dump_stg() {
                println!("{}", prettify::prettify(&*syn));
                Ok(None)
            } else {
                emitter.stream_start();
                let mut machine = standard_machine(stg_settings, syn, emitter, rt.as_ref())?;

                let ret = {
                    let t = Instant::now();
                    let ret = machine.run(None);
                    stats.timings_mut().record("stg-execute", t.elapsed());

                    // copy finer grained GC timings
                    for (k, v) in machine.clock().report() {
                        stats.timings_mut().record(k, v);
                    }

                    // copy machine stats

                    stats.set_ticks(machine.metrics().ticks());
                    stats.set_allocs(machine.metrics().allocs());
                    stats.set_max_stack(machine.metrics().max_stack());

                    // copy heap stats

                    let heap_stats = machine.heap_stats();
                    stats.set_blocks_allocated(heap_stats.blocks_allocated);
                    stats.set_lobs_allocated(heap_stats.lobs_allocated);
                    stats.set_blocks_used(heap_stats.used);
                    stats.set_blocks_recycled(heap_stats.recycled);
                    stats.set_collections_count(heap_stats.collections_count);
                    stats.set_peak_heap_blocks(heap_stats.peak_heap_blocks);

                    // copy GC phase timings
                    use crate::eval::machine::metrics::ThreadOccupation;
                    stats.set_total_mark_time(
                        machine.clock().duration(ThreadOccupation::CollectorMark),
                    );
                    stats.set_total_sweep_time(
                        machine.clock().duration(ThreadOccupation::CollectorSweep),
                    );

                    // The machine always runs in headless mode (no RENDER_DOC
                    // wrapper).  After the first run, handle the three cases:
                    //
                    // 1. Machine yielded on an IO constructor directly → run
                    //    the io-run loop (honours opt.allow_io).
                    //
                    // 2. Machine terminated normally → the top-level value is
                    //    either an IO function (PAP waiting for world) or a
                    //    plain document.  Inject the world token and re-run;
                    //    then handle the result as case 1 or 3.
                    //
                    // 3. No IO yield after world injection → the value is a
                    //    plain document; feed it through RENDER_DOC explicitly.
                    if ret.is_ok() {
                        if machine.io_yielded() {
                            let io_result = io_run_and_render(&mut machine, opt.allow_io)
                                .map_err(ExecutionError::from);
                            machine.take_emitter().stream_end();
                            return io_result;
                        }
                        // Machine terminated without yielding.  Try world
                        // injection to handle IO functions (case 2).
                        let io_yielded = inject_world_and_run(&mut machine)
                            .map_err(|e| ExecutionError::Panic(e.to_string()));
                        match io_yielded {
                            Ok(true) => {
                                // World injection triggered an IO yield;
                                // proceed with the io-run loop.
                                let io_result = io_run_and_render(&mut machine, opt.allow_io)
                                    .map_err(ExecutionError::from);
                                machine.take_emitter().stream_end();
                                return io_result;
                            }
                            Ok(false) => {
                                // Still no IO yield after world injection;
                                // treat as a plain document.  Drop the headless
                                // machine, reclaim the emitter, and re-compile
                                // with RENDER_DOC for a fresh GC-safe run.
                                // (Running render_headless_result on the
                                // existing machine causes GC crashes because the
                                // heap may contain stale string pointers from
                                // the first run.)
                                let emitter = machine.take_emitter();
                                drop(machine);
                                let stg_settings_render = StgSettings {
                                    render_type: RenderType::RenderDoc,
                                    ..opt.stg_settings().clone()
                                };
                                let syn_render = stg::compile(
                                    &stg_settings_render,
                                    self.evaluand.clone(),
                                    rt.as_ref(),
                                )?;
                                let mut fresh_machine = standard_machine(
                                    &stg_settings_render,
                                    syn_render,
                                    emitter,
                                    rt.as_ref(),
                                )?;
                                let render_result = fresh_machine.run(None);
                                fresh_machine.take_emitter().stream_end();
                                return render_result;
                            }
                            Err(e) => {
                                machine.take_emitter().stream_end();
                                return Err(e);
                            }
                        }
                    }

                    ret
                };

                machine.take_emitter().stream_end();
                ret
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
