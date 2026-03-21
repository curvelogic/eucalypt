//! PARSE_STRING intrinsic — parses a string as structured data
//!
//! `PARSE_STRING(fmt_sym, str)` is the inverse of `RENDER_TO_STRING`.
//! It accepts a format symbol and a string, parses the string using the
//! appropriate import parser with `data_only: true` (so no `!eu` evaluation
//! or ZDT.PARSE calls occur), compiles the resulting `RcExpr` to STG, loads
//! it onto the heap, and sets it as the new machine closure.
//!
//! Supported format symbols: `yaml`, `json`, `toml`, `csv`, `xml`, `edn`,
//! `jsonl`, `text`.  `:json` and `:yaml` share the same parser.

use std::{cell::RefCell, rc::Rc};

use codespan_reporting::files::SimpleFiles;

use crate::{
    common::sourcemap::SourceMap,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::{
            env::SynClosure,
            intrinsic::{CallGlobal2, IntrinsicMachine, StgIntrinsic},
        },
        memory::{loader::load, mutator::MutatorHeapView, syntax::Ref},
        stg::{
            compiler::Compiler,
            support::{str_arg, sym_arg},
            RenderType,
        },
    },
    import,
};

/// PARSE_STRING(fmt_sym, str) → value
///
/// Parses `str` using the import parser for `fmt_sym` with `data_only: true`,
/// compiles the resulting core expression to STG, and evaluates it inline by
/// setting it as the machine's new closure.
///
/// Both arguments are strict: the format symbol and string must already be
/// at WHNF before this intrinsic is called.
pub struct ParseString;

impl StgIntrinsic for ParseString {
    fn name(&self) -> &str {
        "PARSE_STRING"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        // args[0] = format symbol (strict)
        // args[1] = string to parse (strict)
        let smid = machine.annotation();
        let format_name = sym_arg(machine, view, &args[0])?;
        let input = str_arg(machine, view, &args[1])?;

        // Parse with data_only: true — no !eu evaluation, no ZDT.PARSE
        let mut files: SimpleFiles<String, String> = SimpleFiles::new();
        let mut source_map = SourceMap::default();
        let file_id = files.add(format!("parse-as:{format_name}"), input.to_string());

        let core_expr =
            import::read_to_core_data_only(&format_name, &mut files, &mut source_map, file_id)
                .map_err(|e| {
                    ExecutionError::ParseError(
                        machine.annotation(),
                        format_name.clone(),
                        e.to_string(),
                    )
                })?;

        // Compile the data-only RcExpr to STG.
        // We use an empty intrinsics list because data-only expressions
        // contain no bif references (all code-execution paths are suppressed).
        // suppress_inlining=true and suppress_optimiser=false are safe choices
        // for data literals.
        let compiler = Compiler::new(
            false, // generate_annotations
            RenderType::Headless,
            false,  // suppress_updates
            true,   // suppress_inlining (no intrinsics to inline)
            false,  // suppress_optimiser
            vec![], // intrinsics (none needed for data literals)
        );
        let syntax: Rc<_> = compiler
            .compile(core_expr)
            .map_err(|e| ExecutionError::Panic(smid, format!("parse-as compile error: {e}")))?;

        // Load the compiled STG onto the machine heap and set as closure.
        let pool = RefCell::new(machine.symbol_pool_mut().clone());
        let heap_ptr = load(&view, &mut pool.borrow_mut(), syntax)
            .map_err(|e| ExecutionError::Panic(smid, format!("parse-as load error: {e}")))?;

        // Update the machine's symbol pool with any new symbols interned during load
        *machine.symbol_pool_mut() = pool.into_inner();

        machine.set_closure(SynClosure::new(heap_ptr, machine.root_env()))
    }
}

impl CallGlobal2 for ParseString {}
