extern crate eucalypt;

use std::process;
use std::thread;

use std::io::IsTerminal;

use eucalypt::driver::check;
use eucalypt::driver::format;
use eucalypt::driver::lsp;
use eucalypt::driver::options::EucalyptOptions;
use eucalypt::driver::prepare::{self, Command};
use eucalypt::driver::source::SourceLoader;
use eucalypt::driver::tester;
use eucalypt::driver::{eval, statistics::Statistics};

/// Stack size for the main execution thread (64 MiB).
///
/// The OS default (8 MiB on macOS, 2–8 MiB on Linux) is too small for
/// the in-process test runner, which accumulates significant stack depth
/// across 100+ tests in a single process. Spawning on a larger stack
/// prevents the SIGSEGV that otherwise occurs on macOS after test 115.
///
/// This is the standard Rust pattern for programs with deep call stacks
/// (rustc itself uses it).
const STACK_SIZE: usize = 64 * 1024 * 1024;

pub fn main() {
    eucalypt::eval::machine::crash::install_crash_handler();

    let exit_code = thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(run)
        .expect("failed to spawn main thread")
        .join()
        .expect("main thread panicked");
    process::exit(exit_code);
}

fn run() -> i32 {
    let opt = EucalyptOptions::from_args();

    // LSP mode runs the language server and exits
    if opt.lsp() {
        match lsp::run() {
            Ok(()) => return 0,
            Err(e) => {
                eprintln!("LSP server error: {e}");
                return 2;
            }
        }
    }

    // For a dry run, just explain the options
    if opt.explain() {
        println!("{}", opt.explanation());
        return 0;
    }

    // Test mode is substantially different, delegate everything to
    // the tester
    if opt.test() {
        match tester::test(&opt) {
            Ok(exit) => return exit,
            Err(e) => {
                eprintln!("{e}");
                return 2;
            }
        }
    }

    // Format mode handles its own input loading
    if opt.format() {
        match format::format(&opt) {
            Ok(exit) => return exit,
            Err(e) => {
                eprintln!("{e}");
                return 2;
            }
        }
    }

    // Check mode: validate type annotations then exit
    if opt.check() {
        match check::check(&opt) {
            Ok(exit) => return exit,
            Err(e) => {
                eprintln!("{e}");
                return 2;
            }
        }
    }

    // No files, no -e, and stdin is a terminal — show help.
    if opt.explicit_inputs.is_empty() && opt.evaluate.is_none() && std::io::stdin().is_terminal() {
        use clap::CommandFactory;
        eucalypt::driver::options::EucalyptCli::command()
            .print_help()
            .ok();
        println!();
        return 0;
    }

    // Anything else is going to involve reading the inputs
    let mut loader = SourceLoader::new(opt.lib_path().to_vec())
        .with_args(opt.args().to_vec())
        .with_seed(opt.seed());

    // Load and translate to core, handling errors by printing
    // diagnostic
    let mut statistics = Statistics::default();
    match prepare::prepare(&opt, &mut loader, statistics.timings_mut()) {
        Err(e) => {
            let diag = e.to_diagnostic(loader.source_map());
            loader.diagnose_to_stderr(&diag);
            return exit_code(&opt, 1, &statistics);
        }
        Ok(Command::Exit) => return exit_code(&opt, 0, &statistics),
        Ok(Command::Continue) => {}
    }

    // --type-check: run the type checker before evaluation, emit warnings
    if opt.type_check() {
        let t = std::time::Instant::now();
        let core_expr = loader.core().expr.clone();
        let warnings = eucalypt::core::typecheck::check::type_check(&core_expr);
        let elapsed = t.elapsed();

        for w in &warnings {
            let diag = w.to_diagnostic(loader.source_map());
            loader.diagnose_to_stderr(&diag);
        }

        if opt.statistics() {
            statistics.timings_mut().record("type-check", elapsed);
        }

        // --strict: abort before evaluation if there are type warnings
        if opt.check_strict() && !warnings.is_empty() {
            return exit_code(&opt, 1, &statistics);
        }
    }

    if opt.run() || opt.dump_stg() || opt.dump_runtime() {
        // run manages error reporting
        match eval::run(&opt, loader) {
            Ok(run_stats) => {
                statistics.merge(run_stats);
                return exit_code(&opt, 0, &statistics);
            }
            _ => return exit_code(&opt, 1, &statistics),
        }
    }

    exit_code(&opt, 0, &statistics)
}

/// Optionally dump stats to stderr and/or write JSON file, then return exit code.
pub fn exit_code(opts: &EucalyptOptions, code: i32, stats: &Statistics) -> i32 {
    if opts.statistics() {
        eprintln!();
        eprintln!("~~~~~~~~~~");
        eprintln!("STATISTICS");
        eprintln!("~~~~~~~~~~");
        eprintln!();
        eprintln!("{stats}");
    }

    if let Some(path) = opts.statistics_file() {
        let json = serde_json::to_string_pretty(&stats.to_json()).unwrap_or_default();
        if let Err(e) = std::fs::write(path, json) {
            eprintln!("Failed to write statistics file {}: {e}", path.display());
        }
    }

    code
}

/// Optionally dump stats to stderr and/or write JSON file, then exit.
pub fn exit(opts: &EucalyptOptions, code: i32, stats: &Statistics) {
    process::exit(exit_code(opts, code, stats))
}
