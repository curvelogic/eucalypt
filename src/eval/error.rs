//! Execution errors
use crate::common::sourcemap::{HasSmid, Smid, SourceMap};
use crate::eval::stg::tags::DataConstructor;
use crate::eval::types::IntrinsicType;
use codespan_reporting::diagnostic::Diagnostic;
use serde_json::Number;
use std::convert::TryFrom;
use std::io;
use thiserror::Error;

use super::{memory::bump, stg::compiler::CompileError};

/// Generate contextual help notes for type mismatch errors
fn type_mismatch_notes(expected: &IntrinsicType, actual: &IntrinsicType) -> Vec<String> {
    use IntrinsicType::*;
    match (expected, actual) {
        (Record(_), List(_)) => vec![
            "the '.' operator performs key lookup on blocks, not lists".to_string(),
            "for lists, use the index operator for indexing (e.g. xs index 0) or \
             pipeline functions like 'head', 'nth'"
                .to_string(),
        ],
        (Number, String) => {
            vec![
                "if you need to convert a string to a number, use 'parse-num'".to_string(),
                "to concatenate strings, use string interpolation or 'join-on' \
                 instead of '+'"
                    .to_string(),
            ]
        }
        (String, Number) => vec!["if you need to convert a number to a string, use 'str' or \
             string interpolation"
            .to_string()],
        _ => vec![],
    }
}

/// Generate contextual help notes for data tag mismatch errors
fn data_tag_mismatch_notes(actual: u8, expected: &[u8]) -> Vec<String> {
    let is_list =
        actual == DataConstructor::ListCons.tag() || actual == DataConstructor::ListNil.tag();
    let is_string = actual == DataConstructor::BoxedString.tag();
    let is_number = actual == DataConstructor::BoxedNumber.tag();
    let expects_block = expected.contains(&DataConstructor::Block.tag());
    let expects_number = expected.contains(&DataConstructor::BoxedNumber.tag());
    let expects_string = expected.contains(&DataConstructor::BoxedString.tag());

    if is_list && expects_block {
        vec![
            "the '.' operator performs key lookup on blocks, not lists".to_string(),
            "for lists, use the index operator for indexing (e.g. xs index 0) or \
             pipeline functions like 'head', 'nth'"
                .to_string(),
        ]
    } else if is_string && expects_number {
        vec![
            "to convert a string to a number, use 'parse-num'".to_string(),
            "to concatenate strings, use string interpolation or 'join-on' \
             instead of '+'"
                .to_string(),
        ]
    } else if is_number && expects_string {
        vec!["to convert a number to a string, use 'str' or string interpolation".to_string()]
    } else {
        vec![]
    }
}

/// Compute the Levenshtein edit distance between two strings.
///
/// This is a simple dynamic programming implementation suitable for
/// short identifier names in error messages.
pub fn levenshtein_distance(a: &str, b: &str) -> usize {
    let a_len = a.len();
    let b_len = b.len();

    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }

    // Use a single row with rolling update to save memory
    let mut prev_row: Vec<usize> = (0..=b_len).collect();
    let mut curr_row = vec![0; b_len + 1];

    for (i, a_ch) in a.chars().enumerate() {
        curr_row[0] = i + 1;
        for (j, b_ch) in b.chars().enumerate() {
            let cost = if a_ch == b_ch { 0 } else { 1 };
            curr_row[j + 1] = (prev_row[j] + cost)
                .min(prev_row[j + 1] + 1)
                .min(curr_row[j] + 1);
        }
        std::mem::swap(&mut prev_row, &mut curr_row);
    }

    prev_row[b_len]
}

/// Find similar names from a list of candidates, ranked by edit distance.
///
/// Returns up to `max_suggestions` candidates with edit distance at most
/// `max_distance`. Results are sorted by distance (closest first).
pub fn suggest_similar(
    target: &str,
    candidates: &[String],
    max_suggestions: usize,
    max_distance: usize,
) -> Vec<String> {
    let mut scored: Vec<(usize, &String)> = candidates
        .iter()
        .map(|c| (levenshtein_distance(target, c), c))
        .filter(|(d, _)| *d > 0 && *d <= max_distance)
        .collect();

    scored.sort_by_key(|(d, _)| *d);
    scored
        .into_iter()
        .take(max_suggestions)
        .map(|(_, name)| name.clone())
        .collect()
}

/// Format a "did you mean?" hint from a list of suggestions.
pub fn format_suggestions(suggestions: &[String]) -> Option<String> {
    if suggestions.is_empty() {
        return None;
    }
    let quoted: Vec<String> = suggestions.iter().map(|s| format!("'{s}'")).collect();
    Some(format!("similar keys: {}", quoted.join(", ")))
}

/// Format a lookup failure message, including suggestions if available
fn format_lookup_failure(key: &str, suggestions: &[String]) -> String {
    let mut msg = format!("key '{key}' not found in block");
    if let Some(hint) = format_suggestions(suggestions) {
        msg.push_str(&format!("\n  help: {hint}"));
    }
    msg
}

/// Format a "not a value" error message when a non-value expression is found
/// where a primitive value was expected.
fn format_not_value(context: &str) -> String {
    if context.is_empty() {
        "expected a value but found an unevaluated expression\n  \
         help: this can occur when a function or structured value appears \
         where a primitive (number, string, etc.) was expected"
            .to_string()
    } else {
        format!(
            "expected a value but found {context}\n  \
             help: this can occur when a function or structured value appears \
             where a primitive (number, string, etc.) was expected"
        )
    }
}

/// Format a "not callable" error message with the actual type of value found
fn format_not_callable(actual_type: &str) -> String {
    if actual_type.is_empty() {
        "tried to call a value that is not a function".to_string()
    } else {
        format!(
            "tried to call a {actual_type} as a function\n  \
             help: only functions and blocks can be called with arguments\n  \
             help: this often means too many arguments were passed to a function"
        )
    }
}

/// Format a "function where value expected" error message.
///
/// This error typically occurs when operator precedence causes a
/// function (e.g. a partially applied operator) to be passed where a
/// scalar value was expected. The most common trigger is the low
/// precedence of catenation (juxtaposition) in expressions like
/// `xs head + 1` which parses as `xs (head + 1)` rather than the
/// intended `(xs head) + 1`.
fn format_cannot_return_fun(expected_tags: &[u8]) -> String {
    let mut msg = if expected_tags.is_empty() {
        "type mismatch: received a function where a value was expected".to_string()
    } else {
        format!(
            "type mismatch: received a function where {} was expected",
            display_expected_tags(expected_tags)
        )
    };
    msg.push_str(
        "\n  help: this often happens due to operator precedence; \
         try adding parentheses, e.g. `(xs f) + 1` instead of `xs f + 1`",
    );
    msg
}

/// Format a division by zero error message with the operation context
fn format_division_by_zero(operation: &str) -> String {
    if operation.is_empty() {
        "division by zero".to_string()
    } else {
        format!(
            "division by zero in {operation}\n  \
             help: the divisor evaluated to zero"
        )
    }
}

/// Format an unknown format error message listing valid export formats
fn format_unknown_format(name: &str) -> String {
    format!(
        "unknown export format '{name}'\n  \
         help: supported formats are: yaml, json, toml, text, edn, html"
    )
}

/// Format a bad format string error message with help about valid specifiers
fn format_bad_format_string(detail: &str) -> String {
    format!(
        "bad format string: {detail}\n  \
         help: format strings use printf-style %-specifiers, \
         e.g. %d (integer), %f (float), %s (string), %e (scientific)"
    )
}

/// Convert a data tag number to a human-readable type name for error messages
fn display_data_tag(tag: u8) -> String {
    match DataConstructor::try_from(tag) {
        Ok(dc) => dc.to_string(),
        Err(()) => format!("unknown type (tag {tag})"),
    }
}

/// Format expected tags as a human-readable list
fn display_expected_tags(tags: &[u8]) -> String {
    let names: Vec<String> = tags.iter().map(|t| display_data_tag(*t)).collect();
    match names.len() {
        0 => "unknown".to_string(),
        1 => names[0].clone(),
        _ => {
            let (last, rest) = names.split_last().unwrap();
            format!("{} or {}", rest.join(", "), last)
        }
    }
}

#[derive(Debug, Error)]
pub enum ExecutionError {
    /// wrapped, env trace and stack trace
    #[error("{0}")]
    Traced(Box<ExecutionError>, Vec<Smid>, Vec<Smid>),
    #[error("allocation error")]
    AllocationError,
    #[error("expected {1} received {2}")]
    ArityMismatch(Smid, usize, usize),
    #[error("binding missing from environment")]
    NotFound(Smid),
    #[error("empty environment")]
    EmptyEnvironment,
    #[error("bad index {0} into environment")]
    BadEnvironmentIndex(usize),
    #[error("bad index {0} into globals")]
    BadGlobalIndex(usize),
    #[error("{}", format_bad_format_string(.0))]
    BadFormatString(String),
    #[error("found free var {1}")]
    FreeVar(Smid, String),
    #[error("code not valid for execution")]
    InvalidCode(Smid),
    #[error("{}", format_lookup_failure(.1, .2))]
    LookupFailure(Smid, String, Vec<String>),
    #[error("type mismatch: expected {1}, found {2}")]
    TypeMismatch(Smid, IntrinsicType, IntrinsicType),
    #[error("unknown intrinsic {1}")]
    UnknownIntrinsic(Smid, String),
    #[error("{}", format_not_callable(.1))]
    NotCallable(Smid, String),
    #[error("{}", format_not_value(.1))]
    NotValue(Smid, String),
    #[error("bad regex: {1}\n  help: the pattern '{0}' is not a valid regular expression")]
    BadRegex(String, String),
    #[error("bad date / time components ({0}, {1}, {2}, {3}, {4}, {5}, {6})")]
    BadDateTimeComponents(Number, Number, Number, Number, Number, Number, String),
    #[error("bad time zone ({0})")]
    BadTimeZone(String),
    #[error("bad timestamp ({0})")]
    BadTimestamp(Number),
    #[error("bad datetime format ({0})")]
    BadDateTimeString(String),
    #[error("failed to apply format string")]
    FormatFailure,
    #[error("failed to convert numeric type as required by format string")]
    BadNumericTypeForFormat,
    #[error("bad number format: {0}")]
    BadNumberFormat(String),
    #[error("could not format {1} with {0}")]
    FormatError(String, Number),
    #[error("expected scalar value")]
    NotScalar(Smid),
    #[error("{}", format_unknown_format(.0))]
    UnknownFormat(String),
    #[error("cannot combine numbers ({0}, {1}) into same numeric domain\n  help: this can happen when mixing integer and floating-point arithmetic in ways that lose precision")]
    NumericDomainError(Number, Number),
    #[error("numeric overflow: result of operating on {0} and {1} is out of range\n  help: the result exceeds the representable range for this numeric type")]
    NumericRangeError(Number, Number),
    #[error("{}", format_division_by_zero(.0))]
    DivisionByZero(String),
    #[error("expected branch continuation")]
    ExpectedBranchContinuation,
    #[error("type mismatch: expected {}, found {}", display_expected_tags(.2), display_data_tag(*.1))]
    NoBranchForDataTag(Smid, u8, Vec<u8>),
    #[error("type mismatch: received a {1} where a structured value (block or list) was expected")]
    NoBranchForNative(Smid, String),
    #[error("{}", format_cannot_return_fun(.1))]
    CannotReturnFunToCase(Smid, Vec<u8>),
    #[error("panic: {0}")]
    Panic(String),
    #[error("machine did not terminate after {0} steps")]
    DidntTerminate(usize),
    #[error("infinite loop detected: binding refers to itself")]
    BlackHole(Smid),
    #[error(transparent)]
    Compile(#[from] CompileError),
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error("expected label on stack")]
    ExpectedLabel,
    #[error("expected closure on stack")]
    ExpectedClosure,
    #[error("array bounds error: index {index} out of bounds for array of length {length}")]
    ArrayBoundsError { index: usize, length: usize },
}

impl From<bump::AllocError> for ExecutionError {
    fn from(_: bump::AllocError) -> Self {
        ExecutionError::AllocationError
    }
}

impl From<super::memory::heap::HeapError> for ExecutionError {
    fn from(e: super::memory::heap::HeapError) -> Self {
        match e {
            super::memory::heap::HeapError::OutOfMemory { .. } => ExecutionError::AllocationError,
            super::memory::heap::HeapError::EmergencyCollectionFailed { .. } => {
                ExecutionError::AllocationError
            }
            super::memory::heap::HeapError::InvalidAllocationSize { .. } => {
                ExecutionError::AllocationError
            }
            super::memory::heap::HeapError::FragmentationError { .. } => {
                ExecutionError::AllocationError
            }
            super::memory::heap::HeapError::BlockAllocationFailed { .. } => {
                ExecutionError::AllocationError
            }
            super::memory::heap::HeapError::EmergencyCollectionInsufficient { .. } => {
                ExecutionError::AllocationError
            }
        }
    }
}

impl HasSmid for ExecutionError {
    fn smid(&self) -> Smid {
        match self {
            ExecutionError::Traced(e, _, _) => e.smid(),
            ExecutionError::ArityMismatch(s, _, _) => *s,
            ExecutionError::NotFound(s) => *s,
            ExecutionError::FreeVar(s, _) => *s,
            ExecutionError::InvalidCode(s) => *s,
            ExecutionError::LookupFailure(s, _, _) => *s,
            ExecutionError::TypeMismatch(s, _, _) => *s,
            ExecutionError::UnknownIntrinsic(s, _) => *s,
            ExecutionError::NotCallable(s, _) => *s,
            ExecutionError::NotValue(s, _) => *s,
            ExecutionError::NotScalar(s) => *s,
            ExecutionError::NoBranchForDataTag(s, _, _) => *s,
            ExecutionError::NoBranchForNative(s, _) => *s,
            ExecutionError::CannotReturnFunToCase(s, _) => *s,
            ExecutionError::BlackHole(s) => *s,
            ExecutionError::Compile(compile_error) => compile_error.smid(),
            _ => Smid::default(),
        }
    }
}

impl ExecutionError {
    pub fn to_diagnostic(&self, source_map: &SourceMap) -> Diagnostic<usize> {
        // Delegate CompileError to its own diagnostic
        if let ExecutionError::Compile(e) = self {
            return e.to_diagnostic(source_map);
        }
        let diag = source_map.diagnostic(self);
        // Unwrap Traced to get at the inner error for note generation
        let inner = match self {
            ExecutionError::Traced(e, _, _) => e.as_ref(),
            other => other,
        };
        let notes = match inner {
            ExecutionError::TypeMismatch(_, expected, actual) => {
                type_mismatch_notes(expected, actual)
            }
            ExecutionError::NoBranchForDataTag(_, actual, expected) => {
                data_tag_mismatch_notes(*actual, expected)
            }
            ExecutionError::BlackHole(_) => {
                vec![
                    "a binding that references itself directly or indirectly creates an infinite loop"
                        .to_string(),
                    "use a function parameter instead of self-reference, or break the cycle"
                        .to_string(),
                ]
            }
            _ => vec![],
        };
        if notes.is_empty() {
            diag
        } else {
            diag.with_notes(notes)
        }
    }

    /// Access environment trace if present
    pub fn env_trace(&self) -> Option<&[Smid]> {
        if let ExecutionError::Traced(_, trace, _) = self {
            Some(trace)
        } else {
            None
        }
    }

    /// Access stack trace if present
    pub fn stack_trace(&self) -> Option<&[Smid]> {
        if let ExecutionError::Traced(_, _, trace) = self {
            Some(trace)
        } else {
            None
        }
    }
}
