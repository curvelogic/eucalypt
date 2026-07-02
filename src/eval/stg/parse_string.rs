//! PARSE_STRING intrinsic — parses a string as structured data
//!
//! `PARSE_STRING(fmt_sym, str)` is the inverse of `RENDER_TO_STRING`.
//! It accepts a format symbol and a string, parses the string using the
//! appropriate import parser with `data_only: true` (so no `!eu` evaluation
//! or ZDT.PARSE calls occur), and materialises the resulting `RcExpr`
//! directly as a value via the neutral data-construction ABI (BV1 §5.5,
//! `data_literal::build_value`) — the parsed expression is a pure data
//! literal (no free variables, no intrinsic application), so no STG
//! compilation or runtime code loading is needed.
//!
//! Supported format symbols: `yaml`, `json`, `toml`, `csv`, `xml`, `edn`,
//! `jsonl`, `text`.  `:json` and `:yaml` share the same parser.

use codespan_reporting::files::SimpleFiles;

use crate::{
    common::sourcemap::SourceMap,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{CallGlobal2, IntrinsicMachine, StgIntrinsic},
        memory::{mutator::MutatorHeapView, syntax::Ref},
        stg::{
            data_literal,
            support::{str_arg, sym_arg},
        },
    },
    import,
};

/// PARSE_STRING(fmt_sym, str) → value
///
/// Parses `str` using the import parser for `fmt_sym` with `data_only: true`
/// and builds the resulting data literal directly as a value.
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

        let value = data_literal::build_value(machine, view, &core_expr)?;
        machine.set_result(value)
    }
}

impl CallGlobal2 for ParseString {}
