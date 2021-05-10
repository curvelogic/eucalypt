//! String and regex intrinsics

use std::iter;

use crate::{common::sourcemap::SourceMap, eval::error::ExecutionError};

use super::{
    force::SeqStrList,
    intrinsic::{CallGlobal1, CallGlobal2, StgIntrinsic},
    machine::Machine,
    printf::{self, PrintfError},
    runtime::{
        call, machine_return_num, machine_return_str, machine_return_str_list, machine_return_sym,
        str_arg, str_list_arg,
    },
    syntax::{
        dsl::{
            annotated_lambda, atom, data, force, let_, local, lref, str, switch, unbox_str, value,
        },
        tags, LambdaForm, Native, Ref,
    },
};

use itertools::Itertools;
use regex::Regex;
use serde_json::Number;

/// SYM(str) to convert strings to symbols
pub struct Sym;

impl StgIntrinsic for Sym {
    fn name(&self) -> &str {
        "SYM"
    }

    fn execute(&self, machine: &mut Machine, args: &[Ref]) -> Result<(), ExecutionError> {
        let text = str_arg(machine, &args[0])?;
        machine_return_sym(machine, text)
    }
}

impl CallGlobal1 for Sym {}

/// STR(x) to convert symbols and numbers to strings
pub struct Str;

impl StgIntrinsic for Str {
    fn name(&self) -> &str {
        "STR"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            1,
            let_(
                vec![value(switch(
                    local(0),
                    vec![
                        (tags::BOXED_NUMBER, force(local(0), call::bif::str(lref(0)))),
                        (tags::BOXED_SYMBOL, force(local(0), call::bif::str(lref(0)))),
                        (tags::BOXED_STRING, local(0)),
                        (tags::BOOL_FALSE, atom(str("false"))),
                        (tags::BOOL_TRUE, atom(str("true"))),
                        (tags::UNIT, atom(str("null"))),
                    ],
                ))],
                data(tags::BOXED_STRING, vec![lref(0)]),
            ),
            source_map.add_synthetic(self.name()),
        )
    }

    fn execute(&self, machine: &mut Machine, args: &[Ref]) -> Result<(), ExecutionError> {
        let nat = machine.resolve_native(&args[0])?;
        let text = match nat {
            Native::Sym(s) => s,
            Native::Str(s) => s,
            Native::Num(n) => format!("{}", n),
            Native::Zdt(d) => format!("{}", d),
        };
        machine_return_str(machine, text)
    }
}

impl CallGlobal1 for Str {}

/// JOIN(list, sep)
pub struct Join;

impl StgIntrinsic for Join {
    fn name(&self) -> &str {
        "JOIN"
    }

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
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
                        data(tags::BOXED_STRING, vec![lref(0)]),
                    ),
                ),
            ),
            source_map.add_synthetic(self.name()),
        )
    }

    fn execute(&self, machine: &mut Machine, args: &[Ref]) -> Result<(), ExecutionError> {
        let sep = str_arg(machine, &args[1])?;
        let result = str_list_arg(machine, args[0].clone())?.join(&sep);
        machine_return_str(machine, result)
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

    fn execute(&self, machine: &mut Machine, args: &[Ref]) -> Result<(), ExecutionError> {
        let string = str_arg(machine, &args[0])?;
        let regex = str_arg(machine, &args[1])?;
        if let Ok(re) = Regex::new(&regex) {
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
            machine_return_str_list(machine, v)
        } else {
            Err(ExecutionError::BadRegex(regex.to_string()))
        }
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

    fn execute(&self, machine: &mut Machine, args: &[Ref]) -> Result<(), ExecutionError> {
        let string = str_arg(machine, &args[0])?;
        let regex = str_arg(machine, &args[1])?;
        if let Ok(re) = Regex::new(&regex) {
            let matches = re
                .find_iter(&string)
                .map(|m| m.as_str().to_string())
                .collect();
            machine_return_str_list(machine, matches)
        } else {
            Err(ExecutionError::BadRegex(regex.to_string()))
        }
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

    fn execute(&self, machine: &mut Machine, args: &[Ref]) -> Result<(), ExecutionError> {
        let string = str_arg(machine, &args[0])?;
        let regex = str_arg(machine, &args[1])?;

        if regex.is_empty() {
            machine_return_str_list(machine, vec![string])
        } else if let Ok(re) = Regex::new(&regex) {
            let split = re.split(&string).map(|it| it.to_string()).collect();
            machine_return_str_list(machine, split)
        } else {
            Err(ExecutionError::BadRegex(regex.to_string()))
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

    fn execute(&self, machine: &mut Machine, args: &[Ref]) -> Result<(), ExecutionError> {
        let string = str_arg(machine, &args[0])?;

        if let Ok(num) = str::parse::<Number>(&string) {
            machine_return_num(machine, num)
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

    fn wrapper(&self, source_map: &mut SourceMap) -> LambdaForm {
        annotated_lambda(
            2, // [x fmtstring]
            let_(
                vec![value(switch(
                    local(0),
                    vec![
                        (
                            tags::BOXED_NUMBER,
                            // [n] [x fmtstring]
                            force(
                                local(0),
                                // [n'] [n] [x fmtstring]
                                switch(
                                    local(3),
                                    vec![(
                                        tags::BOXED_STRING,
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
                            tags::BOXED_SYMBOL,
                            // [k] [x fmtstring]
                            force(
                                local(0),
                                // [k'] [k] [x fmtstring]
                                switch(
                                    local(3),
                                    vec![(
                                        tags::BOXED_STRING,
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
                            tags::BOXED_STRING,
                            // [k] [x fmtstring]
                            force(
                                local(0),
                                // [s'] [s] [x fmtstring]
                                switch(
                                    local(3),
                                    vec![(
                                        tags::BOXED_STRING,
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
                        (tags::BOOL_FALSE, atom(str("false"))),
                        (tags::BOOL_TRUE, atom(str("true"))),
                        (tags::UNIT, atom(str("null"))),
                    ],
                ))],
                data(tags::BOXED_STRING, vec![lref(0)]),
            ),
            source_map.add_synthetic(self.name()),
        )
    }

    fn execute(&self, machine: &mut Machine, args: &[Ref]) -> Result<(), ExecutionError> {
        let nat = machine.resolve_native(&args[0])?;
        let fmt_string = str_arg(machine, &args[1])?;

        match printf::fmt(&fmt_string, &nat) {
            Ok(text) => machine_return_str(machine, text),
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

    fn execute(&self, machine: &mut Machine, args: &[Ref]) -> Result<(), ExecutionError> {
        let string = str_arg(machine, &args[0])?;
        let letters: Vec<String> = string.chars().map(|c| iter::once(c).collect()).collect();
        machine_return_str_list(machine, letters)
    }
}

impl CallGlobal1 for Letters {}
