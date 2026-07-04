//! String and regex intrinsics

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{
            CallGlobal0, CallGlobal1, CallGlobal2, CallGlobal3, IntrinsicMachine, StgIntrinsic,
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
        machine_return_bool, machine_return_num, machine_return_str, machine_return_str_iter,
        machine_return_str_list, machine_return_sym, str_arg, str_arg_ref, str_list_arg,
    },
    syntax::{
        dsl::{atom, box_str, case, data, force, lambda, let_, local, lref, str, unbox_str, value},
        LambdaForm,
    },
    tags::DataConstructor,
};

use regex::Regex;
use serde_json::Number;

/// Get (or store and get) regex from machine rcache
///
/// (NB. not threadsafe but we're single threaded anyway for now)
fn cached_regex<T: AsRef<str>>(
    machine: &mut dyn IntrinsicMachine,
    text: T,
) -> Result<&Regex, ExecutionError> {
    let smid = machine.annotation();
    let text_owned = text.as_ref().to_string();
    let rcache = machine.rcache();

    if !rcache.contains(&text_owned) {
        let re = Regex::new(&text_owned)
            .map_err(|e| ExecutionError::BadRegex(smid, text_owned.clone(), e.to_string()))?;
        rcache.put(text_owned.clone(), re);
    }

    Ok(rcache.get(&text_owned).unwrap())
}

/// SYM(str) to convert strings to symbols
pub struct Sym;

impl StgIntrinsic for Sym {
    fn name(&self) -> &str {
        "SYM"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            1,
            let_(
                vec![value(case(
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
                    // native atom fallback — from_closure puts native at local(0)
                    call::bif::str(lref(0)),
                ))],
                data(DataConstructor::BoxedString.tag(), vec![lref(0)]),
            ),
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let nat = machine.resolve_native(view, &args[0])?;
        let text = match nat {
            Native::Sym(id) => machine.symbol_pool().resolve(id).to_string(),
            Native::Str(s) => (*view.scoped(s)).as_str().to_string(),
            Native::Num(n) => format!("{n}"),
            Native::Zdt(d) => format!("{d}"),
            Native::Index(idx) => format!("<index:{}>", idx.len()),
            Native::Set(_) => "<set>".to_string(),
            Native::NdArray(_) => "<array>".to_string(),
            Native::Vec(_) => "<vec>".to_string(),
            Native::Prng(_) => "<prng>".to_string(),
            Native::Producer(_) => "<producer>".to_string(),
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
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
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let sep = str_arg(machine, view, &args[1])?;
        let strings: Vec<String> = str_list_arg(machine, view, args[0].clone())?;
        let result = strings.join(&sep);
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

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let string = str_arg_ref(machine, view, &args[0])?;
        let regex = str_arg_ref(machine, view, &args[1])?;
        let re = cached_regex(machine, &*regex)?;
        let v: Vec<String> = if let Some(captures) = re.captures(&string) {
            captures
                .iter()
                .map(|m| match m {
                    Some(mch) => mch.as_str().to_string(),
                    None => String::new(),
                })
                .collect()
        } else {
            vec![]
        };
        machine_return_str_iter(machine, view, v.into_iter())
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

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let string = str_arg_ref(machine, view, &args[0])?;
        let regex = str_arg_ref(machine, view, &args[1])?;
        let re = cached_regex(machine, &*regex)?;

        let v: Vec<String> = re
            .find_iter(&string)
            .map(|m| m.as_str().to_string())
            .collect();
        machine_return_str_iter(machine, view, v.into_iter())
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

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let string = str_arg_ref(machine, view, &args[0])?;
        let regex = str_arg_ref(machine, view, &args[1])?;

        if regex.is_empty() {
            machine_return_str_list(machine, view, vec![string.to_string()])
        } else {
            let re = cached_regex(machine, &*regex)?;
            let v: Vec<String> = re.split(&string).map(|it| it.to_string()).collect();
            machine_return_str_iter(machine, view, v.into_iter())
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

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let string = str_arg(machine, view, &args[0])?;

        if let Ok(num) = str::parse::<Number>(&string) {
            machine_return_num(machine, view, num)
        } else {
            Err(ExecutionError::BadNumberFormat(
                machine.annotation(),
                string,
            ))
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

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        // Helper: given unboxed x at local(0) and fmtstring at an offset,
        // unbox the format string (handling both BoxedString and raw native)
        // then call the FMT BIF.
        //
        // `fmtstring_offset` is the de Bruijn index of the original fmtstring
        // argument relative to the current environment.
        //
        // For a boxed x branch: env is [x_inner] [x fmtstring], so after
        // force(local(0)) we get [x_raw] [x_inner] [x fmtstring] and
        // fmtstring is at local(3).
        //
        // For the native x fallback: env is [x_native] [x fmtstring] and
        // fmtstring is at local(2).
        let unbox_fmt_and_call =
            |fmtstring_idx: usize, x_raw_idx: usize| -> std::rc::Rc<super::syntax::StgSyn> {
                // Dispatch on fmtstring: if BoxedString, unbox; if raw native
                // string, use directly.
                case(
                    local(fmtstring_idx),
                    vec![(
                        DataConstructor::BoxedString.tag(),
                        // [fmt_inner] ... env grows by 1
                        force(
                            local(0),
                            // [fmt_raw] [fmt_inner] ...
                            call::bif::fmt(lref(x_raw_idx + 2), lref(0)),
                        ),
                    )],
                    // Native fallback: fmtstring is already a raw native string.
                    // case puts the native at local(0), pushing x_raw up by 1.
                    call::bif::fmt(lref(x_raw_idx + 1), lref(0)),
                )
            };

        lambda(
            2, // [x fmtstring]
            let_(
                vec![value(case(
                    local(0),
                    vec![
                        (
                            DataConstructor::BoxedNumber.tag(),
                            // [n] [x fmtstring]
                            force(
                                local(0),
                                // [n'] [n] [x fmtstring]
                                unbox_fmt_and_call(3, 0),
                            ),
                        ),
                        (
                            DataConstructor::BoxedSymbol.tag(),
                            // [k] [x fmtstring]
                            force(
                                local(0),
                                // [k'] [k] [x fmtstring]
                                unbox_fmt_and_call(3, 0),
                            ),
                        ),
                        (
                            DataConstructor::BoxedString.tag(),
                            // [s] [x fmtstring]
                            force(
                                local(0),
                                // [s'] [s] [x fmtstring]
                                unbox_fmt_and_call(3, 0),
                            ),
                        ),
                        (DataConstructor::BoolFalse.tag(), atom(str("false"))),
                        (DataConstructor::BoolTrue.tag(), atom(str("true"))),
                        (DataConstructor::Unit.tag(), atom(str("null"))),
                    ],
                    // Native x fallback: x is already a raw native value.
                    // case puts it at local(0); env is [x_native] [x fmtstring]
                    // so fmtstring is at local(2).
                    unbox_fmt_and_call(2, 0),
                ))],
                data(DataConstructor::BoxedString.tag(), vec![lref(0)]),
            ),
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let nat = machine.resolve_native(view, &args[0])?;
        let fmt_string = str_arg(machine, view, &args[1])?;

        match printf::fmt(view, machine.symbol_pool(), &fmt_string, &nat) {
            Ok(text) => machine_return_str(machine, view, text),
            Err(PrintfError::InvalidFormatString(s)) => {
                Err(ExecutionError::BadFormatString(machine.annotation(), s))
            }
            Err(PrintfError::FmtError(_)) => {
                Err(ExecutionError::FormatFailure(machine.annotation()))
            }
            Err(PrintfError::ConversionError) => Err(ExecutionError::BadNumericTypeForFormat(
                machine.annotation(),
            )),
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

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let string = str_arg_ref(machine, view, &args[0])?;
        let iter = string.chars().map(|c| c.to_string());
        machine_return_str_iter(machine, view, iter)
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

/// __UPPER(s) - convert to upper case
pub struct Upper;

impl StgIntrinsic for Upper {
    fn name(&self) -> &str {
        "UPPER"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let string = str_arg(machine, view, &args[0])?;
        machine_return_str(machine, view, string.to_uppercase())
    }
}

/// __LOWER(s) - convert to lower case
pub struct Lower;

impl StgIntrinsic for Lower {
    fn name(&self) -> &str {
        "LOWER"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let string = str_arg(machine, view, &args[0])?;
        machine_return_str(machine, view, string.to_lowercase())
    }
}

/// STR_REPLACE(pattern, replacement, string) — regex-based replace all
pub struct Replace;

impl StgIntrinsic for Replace {
    fn name(&self) -> &str {
        "STR_REPLACE"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let pattern = str_arg_ref(machine, view, &args[0])?;
        let replacement = str_arg(machine, view, &args[1])?;
        let string = str_arg_ref(machine, view, &args[2])?;

        let re = cached_regex(machine, &pattern)?;
        let result = re.replace_all(&string, replacement.as_str()).to_string();
        machine_return_str(machine, view, result)
    }
}

impl CallGlobal3 for Replace {}

/// STR_CONTAINS(pattern, string) — regex-based containment check
pub struct Contains;

impl StgIntrinsic for Contains {
    fn name(&self) -> &str {
        "STR_CONTAINS"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let pattern = str_arg_ref(machine, view, &args[0])?;
        let string = str_arg_ref(machine, view, &args[1])?;

        let re = cached_regex(machine, &pattern)?;
        let matched = re.is_match(&string);
        machine_return_bool(machine, view, matched)
    }
}

impl CallGlobal2 for Contains {}

/// STR_TRIM(string) — trim leading and trailing whitespace
pub struct Trim;

impl StgIntrinsic for Trim {
    fn name(&self) -> &str {
        "STR_TRIM"
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let string = str_arg(machine, view, &args[0])?;
        machine_return_str(machine, view, string.trim().to_string())
    }
}

impl CallGlobal1 for Trim {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::{
        intrinsics,
        stg::tags::DataConstructor,
        stg::{arith::Add, panic::Panic, runtime::Runtime, syntax::dsl::*, testing},
    };

    fn runtime() -> Box<dyn Runtime> {
        testing::runtime(vec![
            Box::new(Str),
            Box::new(Fmt),
            Box::new(Add),
            Box::new(Panic),
        ])
    }

    // ── STR wrapper tests ─────────────────────────────────────────

    #[test]
    fn test_str_boxed_num() {
        // STR called through the wrapper with a boxed number
        let syntax = letrec_(
            vec![value(box_num(42))],
            // STR returns BoxedString; unbox to get the raw string
            case(
                Str.global(lref(0)),
                vec![(DataConstructor::BoxedString.tag(), local(0))],
                unit(),
            ),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        assert_eq!(m.string_return(), Some("42".to_string()));
    }

    #[test]
    fn test_str_native_num() {
        // STR called through the wrapper with a raw native number
        // (simulates the result of machine_return_num, e.g. from arithmetic)
        let syntax = case(
            Str.global(num(42)),
            vec![(DataConstructor::BoxedString.tag(), local(0))],
            unit(),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        assert_eq!(m.string_return(), Some("42".to_string()));
    }

    #[test]
    fn test_str_native_from_arithmetic() {
        // A number produced by ADD (which uses machine_return_num, yielding
        // a raw native) is then passed to STR through the wrapper.
        let syntax = letrec_(
            vec![
                // [0]: result of ADD via BIF — raw native num
                value(app_bif(intrinsics::index_u8("ADD"), vec![num(40), num(2)])),
            ],
            case(
                Str.global(lref(0)),
                vec![(DataConstructor::BoxedString.tag(), local(0))],
                unit(),
            ),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        assert_eq!(m.string_return(), Some("42".to_string()));
    }

    // ── FMT wrapper tests ─────────────────────────────────────────

    #[test]
    fn test_fmt_boxed_num_boxed_fmtstr() {
        // FMT with both arguments boxed (the traditional path)
        let syntax = letrec_(
            vec![value(box_num(42)), value(box_str("%04d"))],
            case(
                Fmt.global(lref(0), lref(1)),
                vec![(DataConstructor::BoxedString.tag(), local(0))],
                unit(),
            ),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        assert_eq!(m.string_return(), Some("0042".to_string()));
    }

    #[test]
    fn test_fmt_native_num_boxed_fmtstr() {
        // FMT with a raw native number and a boxed format string.
        // This is the bug case: the outer switch had no native fallback.
        let syntax = letrec_(
            vec![value(box_str("%04d"))],
            case(
                Fmt.global(num(42), lref(0)),
                vec![(DataConstructor::BoxedString.tag(), local(0))],
                unit(),
            ),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        assert_eq!(m.string_return(), Some("0042".to_string()));
    }

    #[test]
    fn test_fmt_boxed_num_native_fmtstr() {
        // FMT with a boxed number and a raw native format string.
        // Tests the inner format-string dispatch fallback.
        let syntax = letrec_(
            vec![value(box_num(42))],
            case(
                Fmt.global(lref(0), str("%04d")),
                vec![(DataConstructor::BoxedString.tag(), local(0))],
                unit(),
            ),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        assert_eq!(m.string_return(), Some("0042".to_string()));
    }

    #[test]
    fn test_fmt_native_num_native_fmtstr() {
        // FMT with both arguments as raw natives.
        // Both the outer and inner fallbacks are exercised.
        let syntax = case(
            Fmt.global(num(42), str("%04d")),
            vec![(DataConstructor::BoxedString.tag(), local(0))],
            unit(),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        assert_eq!(m.string_return(), Some("0042".to_string()));
    }

    #[test]
    fn test_fmt_arithmetic_result() {
        // A number produced by ADD (raw native) formatted through FMT wrapper.
        // This reproduces the real-world scenario from eu-ynhu.
        let syntax = letrec_(
            vec![
                value(app_bif(intrinsics::index_u8("ADD"), vec![num(40), num(2)])),
                value(box_str("%02d")),
            ],
            case(
                Fmt.global(lref(0), lref(1)),
                vec![(DataConstructor::BoxedString.tag(), local(0))],
                unit(),
            ),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        assert_eq!(m.string_return(), Some("42".to_string()));
    }

    #[test]
    fn test_fmt_bool_false() {
        // FMT with a boolean false — returns "false" regardless of format string.
        let syntax = letrec_(
            vec![value(f())],
            case(
                Fmt.global(lref(0), str("%s")),
                vec![(DataConstructor::BoxedString.tag(), local(0))],
                unit(),
            ),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        assert_eq!(m.string_return(), Some("false".to_string()));
    }

    #[test]
    fn test_fmt_unit() {
        // FMT with unit — returns "null".
        let syntax = letrec_(
            vec![value(unit())],
            case(
                Fmt.global(lref(0), str("%s")),
                vec![(DataConstructor::BoxedString.tag(), local(0))],
                unit(),
            ),
        );
        let rt = runtime();
        let mut m = testing::machine(rt.as_ref(), syntax);
        m.run(Some(200)).unwrap();
        assert_eq!(m.string_return(), Some("null".to_string()));
    }
}
