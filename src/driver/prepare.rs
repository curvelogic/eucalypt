//! Prepare core expression for evaluation
use crate::core::expr::RcExpr;
use crate::core::target::Target;
use crate::driver::error::EucalyptError;
use crate::driver::options::EucalyptOptions;
use crate::driver::source::ParsedAst;
use crate::driver::source::SourceLoader;
use crate::syntax::export::embed::Embed;
use crate::syntax::export::pretty::express;
use crate::syntax::rowan::kind::SyntaxNode;
use crate::{common::prettify::prettify, core::export};
use rowan::ast::AstNode;
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
        let mut load_errors: Vec<EucalyptError> = Vec::new();

        for i in &inputs {
            if let Err(e) = loader.load(i) {
                // Only I/O and structural errors abort here; parse errors are
                // collected inside the loader and drained separately below.
                load_errors.push(e);
            }
        }

        stats.record("parse", t.elapsed());

        if !load_errors.is_empty() {
            diagnose_additional(loader, &load_errors);
            return Err(load_errors.into_iter().next().unwrap());
        }

        // Drain parse errors collected while loading.  All errors are reported
        // as diagnostics.  The partial tree (with ERROR_STOWAWAYS nodes) has
        // been stored regardless of errors, so `dump ast` and the LSP can
        // access it.  For evaluation we abort after reporting all errors so
        // the user receives complete diagnostic output.
        let parse_errors = loader.drain_parse_errors();
        if !parse_errors.is_empty() {
            // Report all parse errors as diagnostics.
            for e in &parse_errors {
                let diag = e.to_diagnostic(loader.source_map());
                loader.diagnose_to_stderr(&diag);
            }
            // If we're only dumping the parse tree, continue — the partial
            // tree is already stored.  Otherwise abort so we don't produce
            // garbled output from an incomplete program.
            if !opt.parse_only() {
                return Err(parse_errors.into_iter().next().unwrap());
            }
        }
    }

    // If we're dumping parses, dump every file read during the load
    if opt.parse_only() {
        for (loc, ast) in loader.asts() {
            println!("--- {loc} ---\n");
            dump_ast(ast, opt);
        }
        return Ok(Command::Exit);
    }

    // Consume the ASTs and transform into Core syntax
    {
        let t = Instant::now();
        let mut translate_errors: Vec<EucalyptError> = Vec::new();

        for i in &inputs {
            if let Err(e) = loader.translate(i) {
                translate_errors.push(e);
            }
        }

        stats.record("translate", t.elapsed());

        if !translate_errors.is_empty() {
            diagnose_additional(loader, &translate_errors);
            return Err(translate_errors.into_iter().next().unwrap());
        }
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
            println!("{t}");
        }

        println!("\nFrom inputs\n");
        for i in &inputs {
            println!("  - {i}");
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
    if !opt.no_dce() {
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

    // Run destructure fusion pass: fold static Lookup(Block{...}, key) and
    // HEAD/TAIL(List[...]) patterns that arise after the inline pass
    // distributes destructuring lambdas to their call sites.
    {
        let t = Instant::now();
        loader.fuse_destructure()?;
        stats.record("fuse-destructure", t.elapsed());
    }

    if opt.dump_inlined() {
        let c = loader.core();
        dump_core(c.expr.clone(), opt);
        return Ok(Command::Exit);
    }

    // Prune unused bindings
    if !opt.no_dce() {
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

/// Recursively print a Rowan SyntaxNode tree with indentation.
///
/// Interior nodes show their kind and text range.
/// Leaf tokens show their kind, text range, and quoted text content.
fn print_syntax_tree(node: &SyntaxNode, indent: usize) {
    let pad = "  ".repeat(indent);
    let range = node.text_range();
    println!(
        "{pad}{:?} {}..{}",
        node.kind(),
        u32::from(range.start()),
        u32::from(range.end())
    );
    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(n) => print_syntax_tree(&n, indent + 1),
            rowan::NodeOrToken::Token(t) => {
                let tr = t.text_range();
                println!(
                    "{pad}  {:?} {}..{} {:?}",
                    t.kind(),
                    u32::from(tr.start()),
                    u32::from(tr.end()),
                    t.text()
                );
            }
        }
    }
}

/// Dump AST expression using whatever format specified by options
fn dump_ast(ast: &ParsedAst, opt: &EucalyptOptions) {
    if opt.quote_embed() {
        match ast {
            ParsedAst::Unit(unit) => {
                let embedded = unit.embed();
                println!("{}\n", express(&embedded));
            }
            ParsedAst::Soup(soup) => {
                let embedded = soup.embed();
                println!("{}\n", express(&embedded));
            }
        }
    } else if opt.quote_debug() {
        println!("{ast:#?}");
    } else {
        let node = match ast {
            ParsedAst::Unit(unit) => unit.syntax().clone(),
            ParsedAst::Soup(soup) => soup.syntax().clone(),
        };
        print_syntax_tree(&node, 0);
    }
}

/// Dump core expression using whatever format specified by options
fn dump_core(expr: RcExpr, opt: &EucalyptOptions) {
    if opt.quote_embed() {
        println!("{}\n\n", export::quote_embed_core_unit(&expr));
    } else if opt.quote_debug() {
        println!("{expr:#?}");
    } else {
        // direct expression
        print!("{}", prettify(&expr));
    }
}

/// Print diagnostics for all errors beyond the first.
///
/// The first error is returned to the caller which will print it
/// separately, so we only diagnose errors at index 1 onwards. This
/// allows the user to see all errors from a phase at once rather than
/// stopping at the first one.
fn diagnose_additional(loader: &SourceLoader, errors: &[EucalyptError]) {
    for e in errors.iter().skip(1) {
        let diag = e.to_diagnostic(loader.source_map());
        loader.diagnose_to_stderr(&diag);
    }
}

/// Format the target list
fn format_target_table<'a>(targets: impl Iterator<Item = &'a Target>) -> Vec<String> {
    let mut pairs: Vec<(String, String)> = vec![];
    let mut len = 0;
    for t in targets {
        let prefix = format!(" - {t}");
        len = len.max(prefix.len());
        pairs.push((prefix, t.doc().clone()));
    }

    let doc_column = len + 2;
    let mut lines: Vec<String> = pairs
        .into_iter()
        .map(|(pre, doc)| format!("{pre:doc_column$}{doc}"))
        .collect();
    lines.sort();
    lines
}
