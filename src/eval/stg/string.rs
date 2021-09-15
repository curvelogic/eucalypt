//! String and regex intrinsics

use std::iter;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{
            CallGlobal0, CallGlobal1, CallGlobal2, IntrinsicMachine, StgIntrinsic,
        },
        memory::{
            mutator::MutatorHeapView,
            syntax::{Native, Ref},
        },
        stg::support::call,
    },
};

use super::{
    force::SeqStrList,
    printf::{self, PrintfError},
    support::{
        machine_return_num, machine_return_str, machine_return_str_list, machine_return_sym,
        str_arg, str_list_arg,
    },
    syntax::{
        dsl::{
            annotated_lambda, atom, box_str, data, force, let_, local, lref, str, switch,
            unbox_str, value,
        },
        LambdaForm,
    },
    tags::DataConstructor,
};

use itertools::Itertools;
use regex::Regex;
use serde_json::Number;

/// Get (or store and get) regex from machine rcache
///
/// (NB. not threadsafe but we're single threaded anyway for now)
fn cached_regex<T: AsRef<str>>(
    machine: &mut dyn IntrinsicMachine,
    text: T,
) -> Result<&Regex, ExecutionError> {
    let rcache = machine.rcache();
    let key = text.as_ref().to_string();

    if !rcache.contains(&key) {
        let re = Regex::new(text.as_ref()).map_err(|_| ExecutionError::BadRegex(key.clone()))?;
        rcache.put(key.clone(), re);
    }

    Ok(rcache.get(&key).unwrap())
}

/// SYM(str) to convert strings to symbols
pub struct Sym;

impl StgIntrinsic for Sym {
    fn name(&self) -> &str {
        "SYM"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let text = str_arg(machine, view, &args[0])?;
        machine_return_sym(machine, view, text)
    }
}

impl CallGlobal1 for Sym {}

/// STR(x) to convert symbols and numbers to strings
pub struct Str;

impl StgIntrinsic for Str {
    fn name(&self) -> &str {
        "STR"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            1,
            let_(
                vec![value(switch(
                    local(0),
                    vec![
                        (
                            DataConstructor::BoxedNumber.tag(),
                            force(local(0), call::bif::str(lref(0))),
                        ),
                        (
                            DataConstructor::BoxedSymbol.tag(),
                            force(local(0), call::bif::str(lref(0))),
                        ),
                        (DataConstructor::BoxedString.tag(), local(0)),
                        (DataConstructor::BoolFalse.tag(), atom(str("false"))),
                        (DataConstructor::BoolTrue.tag(), atom(str("true"))),
                        (DataConstructor::Unit.tag(), atom(str("null"))),
                    ],
                ))],
                data(DataConstructor::BoxedString.tag(), vec![lref(0)]),
            ),
            annotation,
        )
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let nat = machine.nav(view).resolve_native(&args[0])?;
        let text = match nat {
            Native::Sym(s) => (*view.scoped(s)).as_str().to_string(),
            Native::Str(s) => (*view.scoped(s)).as_str().to_string(),
            Native::Num(n) => format!("{}", n),
            Native::Zdt(d) => format!("{}", d),
        };
        machine_return_str(machine, view, text)
    }
}

impl CallGlobal1 for Str {}

/// JOIN(list, sep)
pub struct Join;

impl StgIntrinsic for Join {
    fn name(&self) -> &str {
        "JOIN"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            2, // [list sep]
            force(
                SeqStrList.global(lref(0)),
                // [seqlist] [list sep]
                unbox_str(
                    local(2),
                    // [unbox-sep] [sl] [l] [s]
                    let_(
                        vec![value(call::bif::join(lref(1), lref(0)))],
                        data(DataConstructor::BoxedString.tag(), vec![lref(0)]),
                    ),
                ),
            ),
            annotation,
        )
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let sep = str_arg(machine, view, &args[1])?;
        let result = str_list_arg(machine, view, args[0].clone())?.join(&sep);
        machine_return_str(machine, view, result)
    }
}

impl CallGlobal2 for Join {}

/// MATCH(string, regex-string)
///
/// Return captures resulting from matching string with regex-string
pub struct Match;

impl StgIntrinsic for Match {
    fn name(&self) -> &str {
        "MATCH"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let string = str_arg(machine, view, &args[0])?;
        let regex = str_arg(machine, view, &args[1])?;
        let re = cached_regex(machine, regex)?;
        let v: Vec<_> = if let Some(captures) = re.captures(&string) {
            captures
                .iter()
                .map(|m| match m {
                    Some(mch) => mch.as_str().to_string(),
                    None => "".to_string(),
                })
                .collect()
        } else {
            vec![]
        };
        machine_return_str_list(machine, view, v)
    }
}

/// MATCHES(string, regex-string)
///
/// Return all matches of regex-string in string (with no capture information)
pub struct Matches;

impl StgIntrinsic for Matches {
    fn name(&self) -> &str {
        "MATCHES"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let string = str_arg(machine, view, &args[0])?;
        let regex = str_arg(machine, view, &args[1])?;
        let re = cached_regex(machine, regex)?;

        let matches = re
            .find_iter(&string)
            .map(|m| m.as_str().to_string())
            .collect();
        machine_return_str_list(machine, view, matches)
    }
}

impl CallGlobal2 for Matches {}

/// SPLIT(string, regex-string)
///
/// Return all split of regex-string in string (with no capture
/// information).
///
/// Splitting with an empty regex returns the string unchanged. (For
/// compatibility with old haskell implementation.)
pub struct Split;

impl StgIntrinsic for Split {
    fn name(&self) -> &str {
        "SPLIT"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let string = str_arg(machine, view, &args[0])?;
        let regex = str_arg(machine, view, &args[1])?;

        if regex.is_empty() {
            machine_return_str_list(machine, view, vec![string])
        } else {
            let re = cached_regex(machine, regex)?;
            let split = re.split(&string).map(|it| it.to_string()).collect();
            machine_return_str_list(machine, view, split)
        }
    }
}

impl CallGlobal2 for Split {}

/// NUMPARSE(str) - parse a number
pub struct NumParse;

impl StgIntrinsic for NumParse {
    fn name(&self) -> &str {
        "NUMPARSE"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let string = str_arg(machine, view, &args[0])?;

        if let Ok(num) = str::parse::<Number>(&string) {
            machine_return_num(machine, view, num)
        } else {
            Err(ExecutionError::BadNumberFormat(string))
        }
    }
}

impl CallGlobal1 for NumParse {}

/// FMT(obj, fmtstring) - format an object using platform fmt
/// only supports number right now
pub struct Fmt;

impl StgIntrinsic for Fmt {
    fn name(&self) -> &str {
        "FMT"
    }

    fn wrapper(&self, annotation: Smid) -> LambdaForm {
        annotated_lambda(
            2, // [x fmtstring]
            let_(
                vec![value(switch(
                    local(0),
                    vec![
                        (
                            DataConstructor::BoxedNumber.tag(),
                            // [n] [x fmtstring]
                            force(
                                local(0),
                                // [n'] [n] [x fmtstring]
                                switch(
                                    local(3),
                                    vec![(
                                        DataConstructor::BoxedString.tag(),
                                        // [fmt] [n'] [n] [x fmtstring]
                                        force(
                                            local(0),
                                            // [fmt'] [fmt] [n'] [n] [x fmtstring]
                                            call::bif::fmt(lref(2), lref(0)),
                                        ),
                                    )],
                                ),
                            ),
                        ),
                        (
                            DataConstructor::BoxedSymbol.tag(),
                            // [k] [x fmtstring]
                            force(
                                local(0),
                                // [k'] [k] [x fmtstring]
                                switch(
                                    local(3),
                                    vec![(
                                        DataConstructor::BoxedString.tag(),
                                        // [fmt] [k'] [k] [x fmtstring]
                                        force(
                                            local(0),
                                            // [fmt'] [fmt] [k'] [k] [x fmtstring]
                                            call::bif::fmt(lref(2), lref(0)),
                                        ),
                                    )],
                                ),
                            ),
                        ),
                        (
                            DataConstructor::BoxedString.tag(),
                            // [k] [x fmtstring]
                            force(
                                local(0),
                                // [s'] [s] [x fmtstring]
                                switch(
                                    local(3),
                                    vec![(
                                        DataConstructor::BoxedString.tag(),
                                        // [fmt] [s'] [s] [x fmtstring]
                                        force(
                                            local(0),
                                            // [fmt'] [fmt] [s'] [s] [x fmtstring]
                                            call::bif::fmt(lref(2), lref(0)),
                                        ),
                                    )],
                                ),
                            ),
                        ),
                        (DataConstructor::BoolFalse.tag(), atom(str("false"))),
                        (DataConstructor::BoolTrue.tag(), atom(str("true"))),
                        (DataConstructor::Unit.tag(), atom(str("null"))),
                    ],
                ))],
                data(DataConstructor::BoxedString.tag(), vec![lref(0)]),
            ),
            annotation,
        )
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let nat = machine.nav(view).resolve_native(&args[0])?;
        let fmt_string = str_arg(machine, view, &args[1])?;

        match printf::fmt(view, &fmt_string, &nat) {
            Ok(text) => machine_return_str(machine, view, text),
            Err(PrintfError::InvalidFormatString(s)) => Err(ExecutionError::BadFormatString(s)),
            Err(PrintfError::FmtError(_)) => Err(ExecutionError::FormatFailure),
            Err(PrintfError::ConversionError) => Err(ExecutionError::BadNumericTypeForFormat),
        }
    }
}

impl CallGlobal2 for Fmt {}

/// LETTERS(string) - return list of characters in a string, each as a string
pub struct Letters;

impl StgIntrinsic for Letters {
    fn name(&self) -> &str {
        "LETTERS"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let string = str_arg(machine, view, &args[0])?;
        let letters: Vec<String> = string.chars().map(|c| iter::once(c).collect()).collect();
        machine_return_str_list(machine, view, letters)
    }
}

impl CallGlobal1 for Letters {}

/// __DQ - constant for the double quote character
pub struct Dq;

impl StgIntrinsic for Dq {
    fn name(&self) -> &str {
        "DQ"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        value(box_str("\""))
    }
}

impl CallGlobal0 for Dq {}
