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
        (Record(_), Number) => vec![
            "the '.' operator performs key lookup on blocks; \
             numbers do not have fields"
                .to_string(),
            "to apply a function to a number, use pipeline catenation, \
             e.g. 'x abs' or 'x negate' instead of 'x.abs'"
                .to_string(),
        ],
        (Record(_), String) => vec![
            "the '.' operator performs key lookup on blocks; \
             strings do not have fields"
                .to_string(),
            "to apply string functions, use pipeline catenation, \
             e.g. 'x str.to-upper' or 'str.to-lower(x)' instead of 'x.upper'"
                .to_string(),
        ],
        (Number, Record(_)) => vec![
            "blocks (structured values) cannot be used in arithmetic".to_string(),
            "did you mean to access a specific field? use 'block.field' to extract a value \
             before applying arithmetic"
                .to_string(),
        ],
        (Number, String) => {
            vec![
                "to convert a string to a number, use 'num', e.g. 's num'".to_string(),
                "to concatenate strings, use string interpolation or 'join-on' \
                 instead of '+'"
                    .to_string(),
            ]
        }
        (String, Number) => vec!["if you need to convert a number to a string, use 'str' or \
             string interpolation"
            .to_string()],
        (Number, List(_)) => vec![
            "arithmetic operators like '+' work on numbers, not lists".to_string(),
            "to concatenate two lists, use 'append(xs, ys)' or the '++' operator".to_string(),
        ],
        (Symbol, String) => vec![
            "eucalypt uses symbol literals (`:name`) not strings for key names".to_string(),
            "for example, use `has(:x)` instead of `has(\"x\")`; \
             symbols are written with a leading colon"
                .to_string(),
        ],
        _ => vec![],
    }
}

/// Generate contextual help notes for data tag mismatch errors
fn data_tag_mismatch_notes(actual: u8, expected: &[u8]) -> Vec<String> {
    let is_list =
        actual == DataConstructor::ListCons.tag() || actual == DataConstructor::ListNil.tag();
    let is_string = actual == DataConstructor::BoxedString.tag();
    let is_number = actual == DataConstructor::BoxedNumber.tag();
    let is_block = actual == DataConstructor::Block.tag();
    let is_symbol = actual == DataConstructor::BoxedSymbol.tag();
    let expects_block = expected.contains(&DataConstructor::Block.tag());
    let expects_number = expected.contains(&DataConstructor::BoxedNumber.tag());
    let expects_string = expected.contains(&DataConstructor::BoxedString.tag());
    let expects_symbol = expected.contains(&DataConstructor::BoxedSymbol.tag());

    if is_list && expects_block {
        vec![
            "the '.' operator performs key lookup on blocks, not lists".to_string(),
            "for lists, use the index operator for indexing (e.g. xs index 0) or \
             pipeline functions like 'head', 'nth'"
                .to_string(),
        ]
    } else if is_list && expects_number {
        vec![
            "arithmetic operators like '+' work on numbers, not lists".to_string(),
            "to concatenate two lists, use 'append(xs, ys)' or the '++' operator".to_string(),
        ]
    } else if is_number && expects_block {
        vec![
            "the '.' operator performs key lookup on blocks; \
             numbers do not have fields"
                .to_string(),
            "to apply a function to a number, use pipeline catenation, \
             e.g. 'x abs' or 'x negate' instead of 'x.abs'"
                .to_string(),
            "note: 'str' is a namespace of string functions, not a type conversion function; \
             to convert a number to a string use 'str.of(x)' or string interpolation"
                .to_string(),
        ]
    } else if is_string && expects_block {
        vec![
            "the '.' operator performs key lookup on blocks; \
             strings do not have fields"
                .to_string(),
            "to apply string functions, use pipeline catenation, \
             e.g. 'x str.to-upper' or 'str.to-lower(x)' instead of 'x.upper'"
                .to_string(),
            "note: 'str' is a namespace of string functions, not a type conversion function; \
             use 'str.of(x)' or string interpolation to convert values to strings"
                .to_string(),
        ]
    } else if is_block && expects_number {
        vec![
            "blocks (structured values) cannot be used in arithmetic".to_string(),
            "did you mean to access a specific field? use 'block.field' to extract a value \
             before applying arithmetic"
                .to_string(),
        ]
    } else if is_string && expects_number {
        vec![
            "to convert a string to a number, use 'num', e.g. 's num'".to_string(),
            "to concatenate strings, use string interpolation or 'join-on' \
             instead of '+'"
                .to_string(),
        ]
    } else if is_number && expects_string {
        vec!["to convert a number to a string, use 'str' or string interpolation".to_string()]
    } else if is_string && expects_symbol {
        vec![
            "eucalypt uses symbol literals (`:name`) not strings for key names".to_string(),
            "for example, use `has(:x)` instead of `has(\"x\")`; \
             symbols are written with a leading colon"
                .to_string(),
        ]
    } else if is_symbol && expects_string {
        vec![
            "a symbol (`:name`) was found where a string was expected".to_string(),
            "to convert a symbol to a string, use 'str.of', e.g. `str.of(:name)` \
             gives the string `\"name\"`"
                .to_string(),
        ]
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

/// Generate contextual notes for lookup failure errors.
///
/// Recognises common Python/Ruby string method names used as block keys and
/// suggests the correct eucalypt equivalents in the `str` namespace.
fn lookup_failure_notes(key: &str, suggestions: &[String]) -> Vec<String> {
    // Only produce extra notes when there are no edit-distance suggestions,
    // i.e. the key is genuinely unknown and not a near-miss of an existing key.
    if !suggestions.is_empty() {
        return vec![];
    }
    match key {
        "upper" | "toUpper" | "toUpperCase" | "toupper" => {
            vec!["to convert a string to upper case, use 'str.to-upper', \
             e.g. 'text str.to-upper'"
                .to_string()]
        }
        "lower" | "toLower" | "toLowerCase" | "tolower" => {
            vec!["to convert a string to lower case, use 'str.to-lower', \
             e.g. 'text str.to-lower'"
                .to_string()]
        }
        "replace" | "sub" | "gsub" => vec!["eucalypt has no 'replace' function; \
             use 'str.matches-of(re, s)' to find matches, or construct a replacement \
             by splitting and re-joining: 's str.split-on(re) join-on(replacement)'"
            .to_string()],
        "strip" | "trim" | "rstrip" | "lstrip" => vec!["eucalypt has no 'trim'/'strip' function; \
             to remove surrounding whitespace use a regex: \
             'text str.extract(\"^\\\\s*(.*?)\\\\s*$\")'"
            .to_string()],
        "startswith" | "starts_with" | "startsWith" | "hasPrefix" => vec![
            "to test if a string starts with a prefix, use 'str.matches?(re, s)' \
             with an anchored regex, e.g. 'text str.matches?(\"^prefix\")'"
                .to_string(),
        ],
        "endswith" | "ends_with" | "endsWith" | "hasSuffix" => vec![
            "to test if a string ends with a suffix, use 'str.matches?(re, s)' \
             with an anchored regex, e.g. 'text str.matches?(\"suffix$\")'"
                .to_string(),
        ],
        "find" | "index" | "indexOf" | "indexof" => {
            vec!["eucalypt has no string 'find'/'index' function; \
             to extract a substring use 'str.extract(re, s)' with a capture group, \
             e.g. 'text str.extract(\"(pattern)\")'"
                .to_string()]
        }
        "encode" | "decode" => vec!["to encode a string as base64 use 'str.base64-encode', \
             to decode use 'str.base64-decode'"
            .to_string()],
        "format" | "sprintf" | "printf" => vec!["to format a value as a string, use 'str.fmt', \
             e.g. 'str.fmt(x, \"%.2f\")' for two decimal places"
            .to_string()],
        "contains?" | "contains" | "includes?" | "includes" => vec![
            "to test if a string contains a pattern, use 'str.matches?', \
             e.g. `text str.matches?(\"pattern\")`"
                .to_string(),
            "note: 'str.matches?' uses a regular expression, so special \
             characters like '.', '+', '*' must be escaped with '\\'"
                .to_string(),
        ],
        "replace-all" | "substitute" => vec!["eucalypt has no 'replace' function; \
             use 'str.matches-of(re, s)' to find matches, or construct a replacement \
             by splitting and re-joining: 's str.split-on(re) join-on(replacement)'"
            .to_string()],
        "substring" | "substr" => vec!["eucalypt has no substring function; \
             use 'str.extract(re)' with a capturing regex to extract a portion of a string"
            .to_string()],
        "reverse" => vec!["eucalypt has no built-in string reverse function; \
             use 'str.letters' to get individual characters, then 'reverse' the list, \
             then 'str.join-on(\"\")'"
            .to_string()],
        "to-string" | "to_string" | "toString" => vec![
            "to convert a value to a string, use 'str.of', e.g. `str.of(42)`, \
             or string interpolation: `\"{42}\"`"
                .to_string(),
        ],
        "pad" | "pad-left" | "pad-right" | "rpad" | "lpad" | "padStart" | "padEnd" => vec![
            "to pad a string, use 'str.fmt' with a printf width specifier, \
             e.g. `42 str.fmt(\"%10d\")` for right-padding"
                .to_string(),
        ],
        _ => vec![],
    }
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

/// Generate contextual notes for "not callable" errors based on the actual type
fn not_callable_notes(actual_type: &str) -> Vec<String> {
    match actual_type {
        "list" => vec![
            "lists are not callable; to access elements by index use 'nth(index, list)' \
             or pipeline form 'list nth(index)'"
                .to_string(),
            "for the first element use 'list head'; for the rest use 'list tail'".to_string(),
        ],
        "true" | "false" => vec![
            format!(
                "a {actual_type} value is not a function; if this result of a boolean \
                 expression, check operator precedence"
            ),
            "note: 'and' and 'or' are identifiers in eucalypt, not logical operators; \
             use '&&' and '||' for logical conjunction and disjunction"
                .to_string(),
        ],
        "number" | "string" | "symbol" | "datetime" | "empty list" => vec![
            format!(
                "a {actual_type} value is not a function; check for a missing operator or \
                 extra argument"
            ),
            "note: catenation (juxtaposition) has low precedence — 'f(a) + 1' binds as \
             'f(a + 1)'; add parentheses to disambiguate"
                .to_string(),
        ],
        _ => vec![],
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
    #[error("assertion failed: expected {2}, got {1}")]
    AssertionFailed(Smid, String, String),
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
            ExecutionError::AssertionFailed(s, _, _) => *s,
            ExecutionError::Compile(compile_error) => compile_error.smid(),
            _ => Smid::default(),
        }
    }
}

impl ExecutionError {
    pub fn to_diagnostic(&self, source_map: &SourceMap) -> Diagnostic<usize> {
        use codespan_reporting::diagnostic::Label;

        // Delegate CompileError to its own diagnostic
        if let ExecutionError::Compile(e) = self {
            return e.to_diagnostic(source_map);
        }
        let mut diag = source_map.diagnostic(self);
        // Unwrap Traced to get at the inner error for note generation
        let (inner, env_trace, stack_trace) = match self {
            ExecutionError::Traced(e, env, stack) => (e.as_ref(), env.as_slice(), stack.as_slice()),
            other => (other, [].as_slice(), [].as_slice()),
        };

        // If the error's own Smid has no file location (e.g. it points to a
        // synthetic intrinsic label), try to find a source location from the
        // environment trace.  The env trace contains annotations from the let
        // frames that were live at the time of the error, including any Ann
        // nodes the compiler injected at call sites.
        let has_source_label = source_map
            .source_info(inner)
            .map(|info| info.file.is_some())
            .unwrap_or(false);

        if !has_source_label {
            // Prefer the env trace (innermost call site) over the stack trace
            let fallback_smid = source_map
                .first_source_smid(env_trace)
                .or_else(|| source_map.first_source_smid(stack_trace));
            if let Some(smid) = fallback_smid {
                if let Some(info) = source_map.source_info_for_smid(smid) {
                    if let (Some(file), Some(span)) = (info.file, info.span) {
                        diag = diag.with_labels(vec![Label::primary(file, span)]);
                    }
                }
            }
        }
        let notes = match inner {
            ExecutionError::TypeMismatch(_, expected, actual) => {
                type_mismatch_notes(expected, actual)
            }
            ExecutionError::NoBranchForDataTag(_, actual, expected) => {
                data_tag_mismatch_notes(*actual, expected)
            }
            ExecutionError::NoBranchForNative(_, type_desc) => {
                let mut notes = vec![
                    "list operations such as 'head', 'tail', '++', 'map', 'filter' require \
                     list arguments"
                        .to_string(),
                ];
                if type_desc == "string" {
                    // This type description arises in two common scenarios:
                    // 1. The user passed an actual string to a list operation (e.g. '++').
                    // 2. The user called 'head' or 'tail' on an empty list — eucalypt
                    //    represents the empty-list sentinel internally as a string native,
                    //    so the error message mentions "string" even though the user wrote [].
                    notes.push(
                        "if you called 'head' or 'tail' on an empty list '[]', that is the \
                         likely cause — 'head' and 'tail' are only defined on non-empty lists"
                            .to_string(),
                    );
                    notes.push(
                        "guard against empty lists with 'nil?', e.g. \
                         'if(xs nil?, default, xs head)'"
                            .to_string(),
                    );
                    notes.push(
                        "note: to concatenate strings, use string interpolation \
                         or 'join-on' on a list of strings; '++' is for list append"
                            .to_string(),
                    );
                } else {
                    notes.push(
                        "check that the value is a list before applying list operations; \
                         use 'nil?' to test for an empty list"
                            .to_string(),
                    );
                }
                notes
            }
            ExecutionError::BlackHole(_) => {
                vec![
                    "a binding that references itself directly or indirectly creates an infinite loop"
                        .to_string(),
                    "use a function parameter instead of self-reference, or break the cycle"
                        .to_string(),
                ]
            }
            ExecutionError::BadNumberFormat(_) => {
                vec![
                    "valid number formats are: integers (e.g. 42), decimals (e.g. 3.14), and \
                     scientific notation (e.g. 1.5e10)"
                        .to_string(),
                    "use 'str' to convert numbers to strings; to convert strings to numbers use 'num'"
                        .to_string(),
                ]
            }
            ExecutionError::NotCallable(_, type_name) => not_callable_notes(type_name),
            ExecutionError::LookupFailure(_, key, suggestions) => {
                lookup_failure_notes(key, suggestions)
            }
            ExecutionError::CannotReturnFunToCase(_, expected_tags) => {
                let expects_bool = expected_tags.contains(&DataConstructor::BoolTrue.tag())
                    || expected_tags.contains(&DataConstructor::BoolFalse.tag());
                if expects_bool {
                    vec![
                        "if using '_' anaphora in a predicate, note that each '_' creates a \
                         separate parameter — '(_ > _ - 1)' is a two-argument function, not \
                         a one-argument predicate"
                            .to_string(),
                        "to reuse the same value, use '_0' (or '_1', '_2' for multiple \
                         parameters): '(_0 > _0 - 1)' correctly compares the argument to itself"
                            .to_string(),
                    ]
                } else {
                    vec![]
                }
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
