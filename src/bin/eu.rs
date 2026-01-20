extern crate eucalypt;

use std::process;

use eucalypt::driver::format;
use eucalypt::driver::options::EucalyptOptions;
use eucalypt::driver::prepare::{self, Command};
use eucalypt::driver::source::SourceLoader;
use eucalypt::driver::tester;
use eucalypt::driver::{eval, statistics::Statistics};

pub fn main() {
    let mut opt = EucalyptOptions::from_args();

    if opt.process_defaults().is_err() {
        process::exit(1);
    }

    // For a dry run, just explain the options
    if opt.explain() {
        println!("{}", opt.explanation());
        process::exit(0);
    }

    // Test mode is substantially different, delegate everything to
    // the tester
    if opt.test() {
        match tester::test(&opt) {
            Ok(exit) => process::exit(exit),
            Err(e) => {
                eprintln!("{e}");
                process::exit(2)
            }
        }
    }

    // Format mode handles its own input loading
    if opt.format() {
        match format::format(&opt) {
            Ok(exit) => process::exit(exit),
            Err(e) => {
                eprintln!("{e}");
                process::exit(2)
            }
        }
    }

    // Anything else is going to involve reading the inputs
    let mut loader = SourceLoader::new(opt.lib_path().to_vec());

    // Load and translate to core, handling errors by printing
    // diagnostic
    let mut statistics = Statistics::default();
    match prepare::prepare(&opt, &mut loader, statistics.timings_mut()) {
        Err(e) => {
            let diag = e.to_diagnostic(loader.source_map());
            loader.diagnose_to_stderr(&diag);
            exit(&opt, 1, &statistics);
        }
        Ok(Command::Exit) => exit(&opt, 0, &statistics),
        Ok(Command::Continue) => {}
    }

    if opt.run() || opt.dump_stg() || opt.dump_runtime() {
        // run manages error reporting
        match eval::run(&opt, loader) {
            Ok(run_stats) => {
                statistics.merge(run_stats);
                exit(&opt, 0, &statistics)
            }
            _ => exit(&opt, 1, &statistics),
        }
    }

    exit(&opt, 0, &statistics);
}

/// Optionally dump stats to stderr then exit
pub fn exit(opts: &EucalyptOptions, code: i32, stats: &Statistics) {
    if opts.statistics() {
        eprintln!();
        eprintln!("~~~~~~~~~~");
        eprintln!("STATISTICS");
        eprintln!("~~~~~~~~~~");
        eprintln!();
        eprintln!("{stats}");
    }
    process::exit(code)
}
