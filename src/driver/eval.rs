//! Evaluate core
//!
//! This might be by various strategies including delegating to
//! another implementation or interpreting core directly
use crate::{
    common::{prettify, sourcemap::*},
    core::expr::*,
    driver::{error::EucalyptError, options::EucalyptOptions, source::SourceLoader},
    eval::{
        error::ExecutionError,
        stg::{self, make_standard_runtime},
    },
    export,
};
use codespan_reporting::{
    diagnostic::Diagnostic,
    files::SimpleFiles,
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
    executor.execute(&opt, &mut stats, format)?;
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

impl<'a> From<SourceLoader> for Executor<'a> {
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
        self.diagnose(result)
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
            let syn = {
                let t = Instant::now();
                let syn = stg::compile(opt.stg_settings(), self.evaluand.clone(), rt.as_ref())?;
                stats.timings_mut().record("stg-compile", t.elapsed());
                syn
            };

            if opt.dump_stg() {
                println!("{}", prettify::prettify(&*syn));
                Ok(None)
            } else {
                emitter.stream_start();
                let mut machine =
                    stg::standard_machine(opt.stg_settings(), syn, emitter, rt.as_ref())?;

                let ret = {
                    let t = Instant::now();
                    let ret = machine.run();
                    stats.timings_mut().record("stg-execute", t.elapsed());
                    stats.set_ticks(machine.metrics().ticks());
                    stats.set_allocs(machine.metrics().allocs());
                    stats.set_max_stack(machine.metrics().max_stack());
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
    ) -> Result<Option<u8>, ExecutionError> {
        match result {
            Err(e) => {
                let mut diagnostic = e.to_diagnostic(&self.source_map);

                let mut notes = vec![];

                if let Some(trace) = e.env_trace() {
                    let env_trace = self.source_map.format_trace(&trace, &self.files);
                    notes.push(format!("environment trace:\n{}", env_trace));
                }

                if let Some(trace) = e.stack_trace() {
                    let stack_trace = self.source_map.format_trace(&trace, &self.files);
                    notes.push(format!("stack trace:\n{}", stack_trace));
                }

                diagnostic = diagnostic.with_notes(notes);

                self.diagnose_to_stderr(&diagnostic);

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
                .unwrap();
            }
            Some(ref mut err) => {
                term::emit(&mut NoColor::new(err.as_mut()), &config, &self.files, diag).unwrap();
            }
        }
    }
}
