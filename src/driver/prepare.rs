//! Prepare core expression for evaluation
use crate::core::expr::RcExpr;
use crate::core::target::Target;
use crate::driver::error::EucalyptError;
use crate::driver::options::EucalyptOptions;
use crate::driver::source::ParsedAst;
use crate::driver::source::SourceLoader;
use crate::syntax::export::embed::Embed;
use crate::syntax::export::pretty::express;
use crate::syntax::input::{Input, Locator};
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

    // Before the main load loop, check the explicit inputs for a `prelude:`
    // metadata key.  If found, the alternative prelude is loaded and stored
    // as a prelude override.  This pre-pass runs before any other loading so
    // that the override only applies to the primary invocation — not to
    // re-imports triggered by the test harness's validate phase.
    loader.detect_and_load_prelude_override(opt.explicit_inputs())?;

    {
        let t = Instant::now();
        let mut parse_errors: Vec<EucalyptError> = Vec::new();

        for i in &inputs {
            if let Err(e) = loader.load(i) {
                parse_errors.push(e);
            }
        }

        stats.record("parse", t.elapsed());

        if !parse_errors.is_empty() {
            // Diagnose all errors beyond the first; the caller will
            // print the returned error as a diagnostic itself.
            diagnose_additional(loader, &parse_errors);
            return Err(parse_errors.into_iter().next().unwrap());
        }
    }

    // If any loaded unit requested a prelude override, substitute it in the
    // inputs list for the translate and merge phases.  The default prelude
    // (`Resource("prelude")`) is replaced by the user-specified prelude.
    // If no default prelude was present (--no-prelude mode), the override is
    // prepended so it is still available for merging.
    let effective_inputs: Vec<Input> =
        if let Some(override_input) = loader.prelude_override().cloned() {
            let default_prelude = Input::new(Locator::Resource("prelude".to_string()), None, "eu");
            let mut replaced = false;
            let mut result: Vec<Input> = inputs
                .iter()
                .map(|i| {
                    if i == &default_prelude && !replaced {
                        replaced = true;
                        override_input.clone()
                    } else {
                        i.clone()
                    }
                })
                .collect();
            if !replaced {
                // --no-prelude was in effect; prepend the override so it participates in merge.
                result.insert(0, override_input);
            }
            result
        } else {
            inputs.clone()
        };

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

        for i in &effective_inputs {
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
            // For collection mode, substitute prelude in each segment.
            let eff_prologue =
                apply_prelude_override(opt.prologue_inputs(), loader.prelude_override());
            loader.collect_and_merge_units(
                collection_name,
                opt.name_inputs(),
                &eff_prologue,
                opt.explicit_inputs(),
                opt.epilogue_inputs(),
            )?;
        } else {
            loader.merge_units(&effective_inputs)?;
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

/// Apply a prelude override to a slice of inputs, replacing the default
/// `Resource("prelude")` entry if present.
fn apply_prelude_override(inputs: &[Input], override_input: Option<&Input>) -> Vec<Input> {
    let Some(override_input) = override_input else {
        return inputs.to_vec();
    };
    let default_prelude = Input::new(Locator::Resource("prelude".to_string()), None, "eu");
    let mut replaced = false;
    inputs
        .iter()
        .map(|i| {
            if i == &default_prelude && !replaced {
                replaced = true;
                override_input.clone()
            } else {
                i.clone()
            }
        })
        .collect()
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
