//! Prepare core expression for evaluation
use crate::core::expr::RcExpr;
use crate::core::target::Target;
use crate::driver::error::EucalyptError;
use crate::driver::options::EucalyptOptions;
use crate::driver::source::SourceLoader;
use crate::syntax::ast::Expression;
use crate::syntax::export::embed::Embed;
use crate::syntax::export::pretty;
use crate::{common::prettify::prettify, core::export};
use std::time::Instant;

use super::statistics::Timings;

/// Whether to exit or continue on to later phases
pub enum Command {
    /// Continue to next phase
    Continue,
    /// Exit command line
    Exit,
}

/// Use the loader to process the inputs according to the options
pub fn prepare(
    opt: &EucalyptOptions,
    loader: &mut SourceLoader,
    stats: &mut Timings,
) -> Result<Command, EucalyptError> {
    let inputs = opt.inputs();

    {
        let t = Instant::now();

        for i in &inputs {
            loader.load(i)?;
        }

        stats.record("parse", t.elapsed());
    }

    // If we're dumping parses, dump every file read during the load
    if opt.parse_only() {
        for (loc, ast) in loader.asts() {
            println!("--- {} ---\n", loc);
            dump_ast(ast, opt);
        }
        return Ok(Command::Exit);
    }

    // Consume the ASTs and transform into Core syntax
    {
        let t = Instant::now();

        for i in &inputs {
            loader.translate(i)?;
        }

        stats.record("translate", t.elapsed());
    }

    // Optionally collect explicit inputs together
    {
        let t = Instant::now();

        if let Some(collection_name) = opt.collection() {
            loader.collect_and_merge_units(
                collection_name,
                opt.name_inputs(),
                opt.prologue_inputs(),
                opt.explicit_inputs(),
                opt.epilogue_inputs(),
            )?;
        } else {
            loader.merge_units(&inputs)?;
        }

        stats.record("merge", t.elapsed());
    }

    // Replace body with reference to target if specified
    {
        let t = Instant::now();

        if let Some(target) = opt.target() {
            loader.retarget(target)?;
        } else if loader.core().has_target("main") && opt.evaluand().is_none() {
            loader.retarget("main")?;
        }

        stats.record("retarget", t.elapsed());
    }

    // List targets discovered
    if opt.list_targets() {
        println!("Available targets\n");
        for t in format_target_table(loader.core().targets.iter()) {
            println!("{}", t);
        }

        println!("\nFrom inputs\n");
        for i in &inputs {
            println!("  - {}", i);
        }
        return Ok(Command::Exit);
    }

    // Prior to further processing, dump?
    if opt.dump_desugared() {
        let c = loader.core();
        dump_core(c.expr.clone(), opt);
        return Ok(Command::Exit);
    }

    // Test plan processing only needs desugaring
    if opt.test() {
        return Ok(Command::Continue);
    }

    // Cook soup
    {
        let t = Instant::now();

        loader.cook()?;

        stats.record("cook", t.elapsed());
    }

    if opt.dump_cooked() {
        let c = loader.core();
        dump_core(c.expr.clone(), opt);
        return Ok(Command::Exit);
    }

    // Prune unused bindings to reduce inline overhead
    {
        let t = Instant::now();

        loader.eliminate()?;

        stats.record("eliminate-1", t.elapsed());
    }

    // Run inline pass
    {
        let t = Instant::now();

        for _ in 0..2 {
            loader.inline()?;
        }

        stats.record("inline", t.elapsed());
    }

    if opt.dump_inlined() {
        let c = loader.core();
        dump_core(c.expr.clone(), opt);
        return Ok(Command::Exit);
    }

    // Prune unused bindings
    {
        let t = Instant::now();

        loader.eliminate()?;

        stats.record("eliminate-2", t.elapsed());
    }

    if opt.dump_pruned() {
        let c = loader.core();
        dump_core(c.expr.clone(), opt);
        return Ok(Command::Exit);
    }

    // Verify
    let errors = {
        let t = Instant::now();

        let errors = loader.verify()?;

        stats.record("eliminate", t.elapsed());

        errors
    };

    for e in errors {
        let diag = e.to_diagnostic(loader.source_map());
        loader.diagnose_to_stderr(&diag);
    }

    Ok(Command::Continue)
}

/// Dump AST expression using whatever format specified by options
fn dump_ast(ast: &Expression, opt: &EucalyptOptions) {
    if opt.quote_embed() {
        println!("{}\n\n", pretty::express_unit(&ast.embed()));
    } else if opt.quote_debug() {
        println!("{:#?}", ast);
    } else {
        println!("{}\n\n", pretty::express_unit(ast));
    }
}

/// Dump core expression using whatever format specified by options
fn dump_core(expr: RcExpr, opt: &EucalyptOptions) {
    if opt.quote_embed() {
        println!("{}\n\n", export::quote_embed_core_unit(&expr));
    } else if opt.quote_debug() {
        println!("{:#?}", expr);
    } else {
        // direct expression
        print!("{}", prettify(&expr));
    }
}

/// Format the target list
fn format_target_table<'a>(targets: impl Iterator<Item = &'a Target>) -> Vec<String> {
    let mut pairs: Vec<(String, String)> = vec![];
    let mut len = 0;
    for t in targets {
        let prefix = format!(" - {}", t);
        len = len.max(prefix.len());
        pairs.push((prefix, t.doc().clone()));
    }

    let doc_column = len + 2;
    let mut lines: Vec<String> = pairs
        .into_iter()
        .map(|(pre, doc)| format!("{:width$}{}", pre, doc, width = doc_column))
        .collect();
    lines.sort();
    lines
}
