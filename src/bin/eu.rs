extern crate eucalypt;

use std::process;
use std::thread;

use std::io::{IsTerminal, Write};

use eucalypt::driver::check;
use eucalypt::driver::doc;
use eucalypt::driver::error::EucalyptError;
use eucalypt::driver::error_codes;
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

    ctrlc::set_handler(|| {
        eucalypt::eval::machine::vm::set_interrupted();
    })
    .expect("failed to set Ctrl-C handler");

    let exit_code = thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(run)
        .expect("failed to spawn main thread")
        .join()
        .expect("main thread panicked");
    // DIAGNOSTIC ONLY (bead eu-qm7f): no-op unless EU_ENV_DEPTH_HISTOGRAM=1.
    eucalypt::eval::machine::env::env_depth_diag::dump();
    // DIAGNOSTIC ONLY (bead eu-ttpl): no-op unless EU_ENV_LAYOUT_HISTOGRAM=1.
    eucalypt::eval::machine::env::env_layout_diag::dump();
    process::exit(exit_code);
}

fn run() -> i32 {
    let opt = EucalyptOptions::from_args();

    // LSP mode runs the language server and exits
    if opt.lsp() {
        match lsp::run() {
            Ok(code) => return code,
            Err(e) => {
                eprintln!("LSP server error: {e}");
                return 2;
            }
        }
    }

    // Past the LSP branch, this process is the plain CLI: evaluation runs on
    // this thread, and the only others are the original thread parked in
    // `join` and the Ctrl-C watcher parked in a signal wait — neither holds a
    // lock a forked child could deadlock on. Vouch for it, so process
    // parallelism (`par-map` and friends) may fork workers here. Nothing else
    // — the LSP server, the WASM API, embedders, the test harness — does, and
    // there `par-*` is simply sequential.
    #[cfg(unix)]
    eucalypt::eval::stg::parallel::fork::declare_fork_safe_host();

    // For a dry run, just explain the options
    if opt.explain() {
        return match writeln!(std::io::stdout(), "{}", opt.explanation()) {
            Err(e) => failure_exit_code(e.into()),
            Ok(()) => 0,
        };
    }

    // `eu error <CODE>`: look up a stable error code in the catalogue and exit
    if opt.error_code().is_some() {
        match error_codes::run(&opt) {
            Ok(exit) => return exit,
            Err(e) => {
                eprintln!("{e}");
                return 2;
            }
        }
    }

    // Test mode is substantially different, delegate everything to
    // the tester
    if opt.test() {
        match tester::test(&opt) {
            Ok(exit) => return exit,
            Err(e) => return failure_exit_code(e),
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

    // Doc mode: extract and render documentation then exit
    if opt.doc_mode() {
        match doc::doc(&opt) {
            Ok(exit) => return exit,
            Err(e) => return failure_exit_code(e),
        }
    }

    // No files, no -e, and stdin is a terminal — show help.
    if opt.explicit_inputs.is_empty() && opt.evaluate.is_none() && std::io::stdin().is_terminal() {
        use clap::CommandFactory;
        eucalypt::driver::options::EucalyptCli::command()
            .print_help()
            .ok();
        return match writeln!(std::io::stdout()) {
            Err(e) => failure_exit_code(e.into()),
            Ok(()) => 0,
        };
    }

    // Anything else is going to involve reading the inputs
    let mut loader = SourceLoader::new(opt.lib_path().to_vec())
        .with_args(opt.args().to_vec())
        .with_seed(opt.seed())
        // Inspection-only invocations (`eu dump <phase>`) embed the compiled
        // unit — including the `__io` pseudoblock — into their output.  Build
        // a sanitised `__io` (empty env) so the real process environment, and
        // any secrets held in env vars, are never printed.  Eval/run keeps the
        // real environment.
        .with_deterministic_io(opt.inspects_ir());

    // Load the pre-compiled prelude blob once and store it in the loader.
    // cook() will seed the Distributor with blob.operators; after prepare(),
    // eval::run() extracts the blob and passes it to the Executor for runtime
    // loading — no second deserialisation.
    #[cfg(not(target_arch = "wasm32"))]
    if let Some(blob) = eucalypt::driver::eval::maybe_load_prelude_blob(&opt) {
        loader.set_prelude_blob(blob);
    }

    // Load and translate to core, handling errors by printing
    // diagnostic
    let mut statistics = Statistics::default();
    match prepare::prepare(&opt, &mut loader, statistics.timings_mut()) {
        Err(e) if e.is_broken_pipe() => return exit_code(&opt, 0, &statistics),
        Err(e) => {
            let diag = e.to_diagnostic_with_source(loader.source_map(), loader.files());
            loader.diagnose_to_stderr(&diag);
            return exit_code(&opt, 1, &statistics);
        }
        Ok(Command::Exit) => return exit_code(&opt, 0, &statistics),
        Ok(Command::Continue) => {}
    }

    // Run the bidirectional type checker and emit warnings — the same check
    // `eu check` runs. Which checker the eval path uses (blob-core or
    // source) is decided in `check::eval_path_type_check`, shared with the
    // in-process test runner so the harness exercises the same dispatch
    // (eu-8ke8a; see eu-rb5n for the blob-core path).
    let type_findings = {
        let mut writer = codespan_reporting::term::termcolor::StandardStream::stderr(
            codespan_reporting::term::termcolor::ColorChoice::Auto,
        );
        let verdict = check::eval_path_type_check(&opt, &loader, &mut writer);
        match verdict {
            check::EvalPathCheck::Proceed(findings) => findings,
            check::EvalPathCheck::StrictFailure => return exit_code(&opt, 1, &statistics),
        }
    };

    // Resolve type aliases inside TypeData primitives using the pre-pruning
    // aliases (which include definitions removed by DCE).
    {
        let core_expr = loader.core().expr.clone();
        let aliases = loader.type_aliases();
        if !aliases.is_empty() {
            let resolved = eucalypt::core::typecheck::resolve_typedata::resolve_typedata_aliases(
                &core_expr, aliases,
            );
            loader.set_core_expr(resolved);
        }
    }

    if opt.run() || opt.dump_stg() || opt.dump_runtime() || opt.dump_reflatten() {
        // run manages error reporting
        match eval::run_with_type_findings(&opt, loader, type_findings) {
            Ok(result) => {
                statistics.merge(result.stats);
                let code = result.exit_code.map_or(0, |c| c as i32);
                return exit_code(&opt, code, &statistics);
            }
            _ => return exit_code(&opt, 1, &statistics),
        }
    }

    exit_code(&opt, 0, &statistics)
}

/// Report a driver error on stderr and return exit code 2.
///
/// A closed stdout pipe (`eu test … | head`) is not a failure — the reader
/// has what it wanted — so it exits 0 silently, as rendering already does
/// for a broken pipe (eu-1tkk.7.25).
fn failure_exit_code(e: EucalyptError) -> i32 {
    if e.is_broken_pipe() {
        0
    } else {
        eprintln!("{e}");
        2
    }
}

/// Optionally dump stats to stderr and/or write JSON file, then return exit code.
pub fn exit_code(opts: &EucalyptOptions, code: i32, stats: &Statistics) -> i32 {
    if opts.statistics() {
        eprintln!();
        eprintln!("══════════════════════════════════════════════════════");
        eprintln!("STATISTICS");
        eprintln!("══════════════════════════════════════════════════════");
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
