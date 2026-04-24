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

/// Format a type mismatch error message, including the actual value when available.
fn format_type_mismatch(
    expected: &IntrinsicType,
    actual: &IntrinsicType,
    value: &Option<String>,
) -> String {
    match value {
        Some(v) => format!("type mismatch: expected {expected}, found {actual} {v}"),
        None => format!("type mismatch: expected {expected}, found {actual}"),
    }
}

/// Generate contextual help notes for type mismatch errors
fn type_mismatch_notes(expected: &IntrinsicType, actual: &IntrinsicType) -> Vec<String> {
    use IntrinsicType::*;
    match (expected, actual) {
        (Record(_), List(_)) => vec![
            "the '.' operator performs key lookup on blocks, not lists".to_string(),
            "for lists, use the index operator for indexing (e.g. xs index 0) or \
             pipeline functions like 'head', 'nth'"
                .to_string(),
            "to extract a field from every item in a list of blocks, \
             use 'map', e.g. 'records map(.name)' or 'map(.name, records)'"
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
                "to concatenate strings, use string interpolation or 'str.join-on' \
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
            "a symbol was expected but a string was passed; \
             add a colon prefix to make it a symbol: ':yaml' not \"yaml\", \
             ':key' not \"key\""
                .to_string(),
            "symbols (`:name`) and strings (`\"name\"`) are distinct in eucalypt; \
             functions like 'render-as', 'has', 'lookup-or' take symbol arguments"
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
    let is_null = actual == DataConstructor::Unit.tag();
    let expects_block = expected.contains(&DataConstructor::Block.tag());
    let expects_number = expected.contains(&DataConstructor::BoxedNumber.tag());
    let expects_string = expected.contains(&DataConstructor::BoxedString.tag());
    let expects_symbol = expected.contains(&DataConstructor::BoxedSymbol.tag());

    if is_null {
        return vec![
            "a null value was found where a non-null value was expected".to_string(),
            "use 'null?(value)' to check for null before passing to functions that require a value"
                .to_string(),
        ];
    }

    if is_list && expects_block {
        vec![
            "the '.' operator performs key lookup on blocks, not lists".to_string(),
            "for lists, use pipeline functions: 'xs count' (length), 'xs head' (first element), \
             'xs tail' (rest), 'xs !! 0' (nth element, 0-indexed)"
                .to_string(),
        ]
    } else if is_list && expects_number && expects_string {
        // The STR intrinsic (used for string interpolation) expects a scalar type.
        // A list was passed where a scalar was required — likely in string interpolation.
        vec![
            "lists cannot be used directly in string interpolation or string conversion"
                .to_string(),
            "to render a list as a string, use 'render-as', \
             e.g. 'my_list render-as(:json)' or 'my_list render-as(:yaml)'"
                .to_string(),
            "to join a list of strings with a separator, use 'str.join-on', \
             e.g. 'items str.join-on(\", \")'"
                .to_string(),
            "to extract a field from every item in a list of blocks, \
             use 'map', e.g. 'records map(.name)' or 'map(.name, records)'"
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
    } else if is_block && expects_number && expects_string {
        // Block found where a scalar value was expected — this typically happens when
        // a block is passed to string interpolation or a scalar conversion function.
        vec![
            "blocks (structured values) cannot be used as scalar values in string interpolation or conversion functions".to_string(),
            "to include a block in a string, render it to a format: e.g. 'block render-as(:json)'".to_string(),
            "to extract a specific field from the block, use 'block.field' before passing it to a string operation".to_string(),
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
            "to concatenate strings, use string interpolation or 'str.join-on' \
             instead of '+'"
                .to_string(),
        ]
    } else if is_number && expects_string {
        vec!["to convert a number to a string, use 'str' or string interpolation".to_string()]
    } else if is_string && expects_symbol {
        vec![
            "a symbol was expected but a string was passed; \
             add a colon prefix: ':yaml' not \"yaml\", ':key' not \"key\""
                .to_string(),
            "symbols (`:name`) and strings (`\"name\"`) are distinct in eucalypt; \
             functions like 'render-as', 'has', 'lookup-or' take symbol arguments"
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

/// Format a lookup failure message, including suggestions or available keys
fn format_lookup_failure(key: &str, suggestions: &[String], available: &[String]) -> String {
    let mut msg = format!("key '{key}' not found in block");
    if let Some(hint) = format_suggestions(suggestions) {
        msg.push_str(&format!("\n  help: {hint}"));
    } else if !available.is_empty() {
        // No close matches — show the full set of available keys (capped at 8)
        let shown: Vec<String> = available.iter().take(8).map(|s| format!("'{s}'")).collect();
        let suffix = if available.len() > 8 {
            format!(" (and {} more)", available.len() - 8)
        } else {
            String::new()
        };
        msg.push_str(&format!(
            "\n  help: available keys: {}{}",
            shown.join(", "),
            suffix
        ));
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
             by splitting and re-joining: 's str.split-on(re) str.join-on(replacement)'"
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
             by splitting and re-joining: 's str.split-on(re) str.join-on(replacement)'"
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
    match context {
        "a function application" => {
            "expected a primitive value but found an unevaluated function application\n  \
             help: this often means a function received fewer arguments than it needs, \
             producing a partial application instead of a result\n  \
             help: check that all functions in the pipeline receive the correct number of \
             arguments — e.g. 'add(x, y)' needs two args but 'xs map(add)' applies 'add' \
             to each element with only one arg"
                .to_string()
        }
        "a data constructor (e.g. block or list)" | "a data constructor" => {
            "expected a primitive value but found a structured value (block or list)\n  \
             help: to extract a field from a block, use '.field' notation; \
             to extract an element from a list, use 'head' or 'nth(n, list)'"
                .to_string()
        }
        "a boolean (true)" | "a boolean (false)" => {
            "expected a primitive number, string, or symbol but found a boolean\n  \
             help: use 'if(condition, then_value, else_value)' to convert a boolean to a value\n  \
             help: eucalypt booleans cannot be used in arithmetic or as strings directly"
                .to_string()
        }
        "a list" => "expected a primitive value but found a list\n  \
             help: to access elements, use 'head' (first element) or 'xs !! n' (nth element)\n  \
             help: to convert a list of strings to a single string, use 'str.join-on'"
            .to_string(),
        "a block" => "expected a primitive value but found a block\n  \
             help: to extract a field, use '.field' notation, e.g. 'block.field'"
            .to_string(),
        c if !c.is_empty() => {
            format!(
                "expected a primitive value but found {c}\n  \
                 help: this can occur when a function or structured value appears \
                 where a primitive (number, string, etc.) was expected"
            )
        }
        _ => "expected a value but found an unevaluated expression\n  \
              help: this can occur when a function or structured value appears \
              where a primitive (number, string, etc.) was expected"
            .to_string(),
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

/// Format a bad timezone string error message
fn format_bad_timezone(tz: &str) -> String {
    format!(
        "unrecognised time zone '{tz}'\n  \
         help: use an IANA timezone name (e.g. 'Europe/London', 'America/New_York') \
         or a UTC offset string (e.g. '+0100', '-0530', 'UTC')"
    )
}

/// Format a bad datetime components error with a hint about valid ranges
fn format_bad_datetime_components(
    y: &Number,
    m: &Number,
    d: &Number,
    h: &Number,
    min: &Number,
    s: &Number,
    tz: &str,
) -> String {
    // Try to identify which component is out of range
    let hint = if m.as_u64().is_some_and(|v| v == 0 || v > 12) {
        format!("month {m} is out of range; months are numbered 1–12")
    } else if d.as_u64().is_some_and(|v| v == 0 || v > 31) {
        format!("day {d} is out of range; days are numbered 1–31 (depending on month)")
    } else if h.as_u64().is_some_and(|v| v > 23) {
        format!("hour {h} is out of range; valid hours are 0–23")
    } else if min.as_u64().is_some_and(|v| v > 59) {
        format!("minute {min} is out of range; valid minutes are 0–59")
    } else if s.as_u64().is_some_and(|v| v > 59) {
        format!("second {s} is out of range; valid seconds are 0–59")
    } else {
        format!(
            "the combination ({y}, {m}, {d}) does not form a valid calendar date \
             (e.g. February has at most 29 days)"
        )
    };
    format!(
        "invalid date/time components: {hint}\n  \
         help: cal.zdt takes (year, month, day, hour, minute, second, timezone), \
         e.g. cal.zdt(2023, 1, 15, 10, 30, 0, \"UTC\")\n  \
         note: given components were ({y}, {m}, {d}, {h}, {min}, {s}, {tz})"
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
    #[error("{}", format_bad_format_string(.1))]
    BadFormatString(Smid, String),
    #[error("found free var {1}")]
    FreeVar(Smid, String),
    #[error("code not valid for execution")]
    InvalidCode(Smid),
    #[error("{}", format_lookup_failure(.1, .2, .3))]
    LookupFailure(Smid, String, Vec<String>, Vec<String>),
    #[error("{}", format_type_mismatch(.1, .2, .3))]
    TypeMismatch(Smid, Box<IntrinsicType>, Box<IntrinsicType>, Option<String>),
    #[error("unknown intrinsic {1}")]
    UnknownIntrinsic(Smid, String),
    #[error("{}", format_not_callable(.1))]
    NotCallable(Smid, String),
    #[error("{}", format_not_value(.1))]
    NotValue(Smid, String),
    #[error("bad regex: {2}\n  help: the pattern '{1}' is not a valid regular expression")]
    BadRegex(Smid, String, String),
    #[error("{}", format_bad_datetime_components(.1, .2, .3, .4, .5, .6, .7))]
    BadDateTimeComponents(Smid, Number, Number, Number, Number, Number, Number, String),
    #[error("{}", format_bad_timezone(.1))]
    BadTimeZone(Smid, String),
    #[error("bad timestamp ({1})")]
    BadTimestamp(Smid, Number),
    #[error("bad datetime format '{1}': not a recognised ISO 8601 datetime string\n  help: expected a format like '2024-01-15T09:30:00+00:00' or '2024-01-15'")]
    BadDateTimeString(Smid, String),
    #[error("failed to apply format string")]
    FormatFailure(Smid),
    #[error(
        "failed to convert numeric type as required by format string\n  \
         help: integer specifiers like %d, %o, %x require an integer value; \
         use %f or %g for floating-point numbers"
    )]
    BadNumericTypeForFormat(Smid),
    #[error("bad number format: {1}")]
    BadNumberFormat(Smid, String),
    #[error("could not format {2} with {1}")]
    FormatError(Smid, String, Number),
    #[error("expected scalar value")]
    NotScalar(Smid),
    #[error("{}", format_unknown_format(.0))]
    UnknownFormat(String),
    #[error("cannot combine numbers ({1}, {2}) into same numeric domain\n  help: this can happen when mixing integer and floating-point arithmetic in ways that lose precision")]
    NumericDomainError(Smid, Number, Number),
    #[error("result of ({1})^({2}) is not a real number\n  help: raising a negative base to a fractional exponent yields a complex result; use a non-negative base or an integer exponent")]
    ComplexResult(Smid, Number, Number),
    #[error("numeric overflow: result of operating on {1} and {2} is out of range\n  help: the result exceeds the representable range for this numeric type")]
    NumericRangeError(Smid, Number, Number),
    #[error("{}", format_division_by_zero(.1))]
    DivisionByZero(Smid, String),
    #[error("expected branch continuation")]
    ExpectedBranchContinuation,
    #[error("type mismatch: expected {}, found {}", display_expected_tags(.2), display_data_tag(*.1))]
    NoBranchForDataTag(Smid, u8, Vec<u8>),
    #[error("type mismatch: received a {1} where a structured value (block or list) was expected")]
    NoBranchForNative(Smid, String),
    #[error("{}", format_cannot_return_fun(.1))]
    CannotReturnFunToCase(Smid, Vec<u8>),
    #[error("panic: {1}")]
    Panic(Smid, String),
    #[error("parse-as({1}): {2}")]
    ParseError(Smid, String, String),
    #[error("version requirement not satisfied: eucalypt {1} does not satisfy '{2}'")]
    VersionRequirementFailed(Smid, String, String),
    #[error("IO operations are not permitted; use the --allow-io (-I) flag to enable")]
    IoNotAllowed(Smid),
    #[error("io.fail: {1}")]
    IoFail(Smid, String),
    #[error("io.shell-with: command timed out after {1} seconds")]
    IoTimeout(Smid, u64),
    #[error("io.shell-with: command execution error: {1}")]
    IoCommandError(Smid, String),
    #[error("str.base64-decode: {1}")]
    InvalidBase64(Smid, String),
    #[error("str.base64-decode: decoded bytes are not valid UTF-8: {1}")]
    InvalidBase64Utf8(Smid, String),
    #[error("assertion failed: expected {2}, got {1}")]
    AssertionFailed(Smid, String, String),
    #[error("shift amount {1} is out of range: must be between 0 and 63 for 64-bit integers")]
    BitshiftRangeError(Smid, i64),
    #[error("unknown render format '{1}'\n  help: supported formats for render-as are: :yaml, :json, :toml, :text, :edn, :html")]
    UnknownRenderFormat(Smid, String),
    #[error("cannot compare incompatible types with '{1}': operands must both be numbers, strings, symbols, or datetimes")]
    ComparisonTypeMismatch(Smid, String),
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
    #[error("bitwise operations require integer arguments ({0})")]
    BitwiseIntegerRequired(String),
    #[error("array index out of bounds: coordinates {1:?} are outside the array bounds")]
    ArrayNdIndexOutOfBounds(Smid, Vec<usize>),
    #[error("array shape mismatch: {1}")]
    ArrayShapeMismatch(Smid, String),
    #[error("negative array index or dimension: {1} — array indices and dimensions must be non-negative")]
    ArrayNegativeIndex(Smid, f64),
    #[error("list index out of bounds: index {1} is beyond the end of the list")]
    ListIndexOutOfBounds(Smid, usize),
    #[error("head requires a list, found {1}")]
    HeadOfNonList(Smid, String),
    #[error("tail requires a list, found {1}")]
    TailOfNonList(Smid, String),
    #[error("head of empty list")]
    HeadOfEmptyList(Smid),
    #[error("tail of empty list")]
    TailOfEmptyList(Smid),
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
            ExecutionError::LookupFailure(s, _, _, _) => *s,
            ExecutionError::TypeMismatch(s, _, _, _) => *s,
            ExecutionError::UnknownIntrinsic(s, _) => *s,
            ExecutionError::NotCallable(s, _) => *s,
            ExecutionError::NotValue(s, _) => *s,
            ExecutionError::NotScalar(s) => *s,
            ExecutionError::NoBranchForDataTag(s, _, _) => *s,
            ExecutionError::NoBranchForNative(s, _) => *s,
            ExecutionError::CannotReturnFunToCase(s, _) => *s,
            ExecutionError::BlackHole(s) => *s,
            ExecutionError::Panic(s, _) => *s,
            ExecutionError::ParseError(s, _, _) => *s,
            ExecutionError::VersionRequirementFailed(s, _, _) => *s,
            ExecutionError::InvalidBase64(s, _) => *s,
            ExecutionError::InvalidBase64Utf8(s, _) => *s,
            ExecutionError::AssertionFailed(s, _, _) => *s,
            ExecutionError::BitshiftRangeError(s, _) => *s,
            ExecutionError::UnknownRenderFormat(s, _) => *s,
            ExecutionError::ComparisonTypeMismatch(s, _) => *s,
            ExecutionError::Compile(compile_error) => compile_error.smid(),
            ExecutionError::ArrayNdIndexOutOfBounds(s, _) => *s,
            ExecutionError::ArrayShapeMismatch(s, _) => *s,
            ExecutionError::ArrayNegativeIndex(s, _) => *s,
            ExecutionError::IoNotAllowed(s) => *s,
            ExecutionError::IoFail(s, _) => *s,
            ExecutionError::IoTimeout(s, _) => *s,
            ExecutionError::IoCommandError(s, _) => *s,
            ExecutionError::ListIndexOutOfBounds(s, _) => *s,
            ExecutionError::HeadOfNonList(s, _) => *s,
            ExecutionError::TailOfNonList(s, _) => *s,
            ExecutionError::HeadOfEmptyList(s) => *s,
            ExecutionError::TailOfEmptyList(s) => *s,
            ExecutionError::BadDateTimeString(s, _) => *s,
            ExecutionError::BadRegex(s, _, _) => *s,
            ExecutionError::BadFormatString(s, _) => *s,
            ExecutionError::BadTimeZone(s, _) => *s,
            ExecutionError::BadTimestamp(s, _) => *s,
            ExecutionError::FormatFailure(s) => *s,
            ExecutionError::BadNumericTypeForFormat(s) => *s,
            ExecutionError::BadNumberFormat(s, _) => *s,
            ExecutionError::FormatError(s, _, _) => *s,
            ExecutionError::NumericDomainError(s, _, _) => *s,
            ExecutionError::ComplexResult(s, _, _) => *s,
            ExecutionError::NumericRangeError(s, _, _) => *s,
            ExecutionError::DivisionByZero(s, _) => *s,
            ExecutionError::BadDateTimeComponents(s, _, _, _, _, _, _, _) => *s,
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

        // Determine the best primary source location:
        // - Prefer a user-file location (the call site in user code) over a
        //   prelude/resource location (the site of the failure inside a library).
        // - If the error's own Smid already points to a user file, use that.
        // - Otherwise, search the environment trace (innermost call annotations)
        //   then the stack trace for the first user-file Smid.
        // - As a last resort, fall back to any Smid with a file location.
        let error_info = source_map.source_info(inner);
        let error_has_user_file = error_info
            .and_then(|info| info.file)
            .map(|fid| source_map.is_user_file(fid))
            .unwrap_or(false);

        let primary_file_span: Option<(usize, codespan::Span)> = if error_has_user_file {
            error_info.and_then(|info| info.file.zip(info.span))
        } else {
            // Try to find a user-file location in the traces
            let user_smid = source_map
                .first_user_source_smid(env_trace)
                .or_else(|| source_map.first_user_source_smid(stack_trace));

            if let Some(smid) = user_smid {
                source_map
                    .source_info_for_smid(smid)
                    .and_then(|info| info.file.zip(info.span))
            } else {
                // No user-file location; fall back to the error's own Smid or
                // any location in the traces (handles rare all-library scenarios).
                error_info
                    .and_then(|info| info.file.zip(info.span))
                    .or_else(|| {
                        let smid = source_map
                            .first_source_smid(env_trace)
                            .or_else(|| source_map.first_source_smid(stack_trace))?;
                        source_map
                            .source_info_for_smid(smid)
                            .and_then(|info| info.file.zip(info.span))
                    })
            }
        };

        // Apply the primary label.  We clear any label that source_map.diagnostic()
        // may have set (which could be a prelude location) and replace it with the
        // best location determined above.
        if let Some((file, span)) = primary_file_span {
            diag.labels.clear();
            diag.labels.push(Label::primary(file, span));
        }

        // Add secondary labels from the env trace for call-chain context.
        //
        // We collect env trace entries that have real source locations and differ
        // from the primary label (to avoid redundant markers).  Limited to 3
        // secondary labels to keep output readable.
        {
            let mut secondary_labels: Vec<Label<usize>> = vec![];
            let mut seen_spans: Vec<(usize, codespan::Span)> = vec![];

            // Skip the first env_trace entry if it matches the fallback primary (already shown)
            for &smid in env_trace.iter() {
                if secondary_labels.len() >= 3 {
                    break;
                }
                let info = match source_map.source_info_for_smid(smid) {
                    Some(i) => i,
                    None => continue,
                };
                let (file, span) = match (info.file, info.span) {
                    (Some(f), Some(s)) => (f, s),
                    _ => continue,
                };

                // Skip if same as primary label
                if let Some((pf, ps)) = primary_file_span {
                    if file == pf && span == ps {
                        continue;
                    }
                }

                // Skip duplicates
                if seen_spans.iter().any(|&(f, s)| f == file && s == span) {
                    continue;
                }
                seen_spans.push((file, span));

                // Build the secondary label message
                let msg = if let Some(ann) = &info.annotation {
                    use crate::common::sourcemap::intrinsic_display_name;
                    if let Some(display) = intrinsic_display_name(ann) {
                        format!("in '{display}'")
                    } else {
                        "called from here".to_string()
                    }
                } else {
                    "called from here".to_string()
                };

                secondary_labels.push(Label::secondary(file, span).with_message(msg));
            }

            if !secondary_labels.is_empty() {
                diag = diag.with_labels(secondary_labels);
            }
        }

        // Diagnostic trace dump: when EU_ERROR_TRACE_DUMP is set, emit all
        // available source locations as notes so we can study what information
        // is available at error time.
        if std::env::var("EU_ERROR_TRACE_DUMP").is_ok() {
            let mut dump = vec!["--- ERROR TRACE DUMP ---".to_string()];

            // Error's own Smid
            let error_smid = inner.smid();
            dump.push(format!(
                "error smid: {}",
                Self::format_smid_detail(error_smid, source_map)
            ));

            // Whether the error's own Smid points to a user file
            dump.push(format!("error_has_user_file: {error_has_user_file}"));
            dump.push(format!(
                "primary_file_span: {:?}",
                primary_file_span.map(|(f, s)| (f, s.start(), s.end()))
            ));

            // Environment trace
            if env_trace.is_empty() {
                dump.push("env_trace: (empty)".to_string());
            } else {
                dump.push(format!("env_trace ({} entries):", env_trace.len()));
                for (i, smid) in env_trace.iter().enumerate() {
                    dump.push(format!(
                        "  [{i}] {}",
                        Self::format_smid_detail(*smid, source_map)
                    ));
                }
            }

            // Stack trace
            if stack_trace.is_empty() {
                dump.push("stack_trace: (empty)".to_string());
            } else {
                dump.push(format!("stack_trace ({} entries):", stack_trace.len()));
                for (i, smid) in stack_trace.iter().enumerate() {
                    dump.push(format!(
                        "  [{i}] {}",
                        Self::format_smid_detail(*smid, source_map)
                    ));
                }
            }

            dump.push("--- END TRACE DUMP ---".to_string());
            diag = diag.with_notes(dump);
        }

        let notes = match inner {
            ExecutionError::TypeMismatch(_, expected, actual, _) => {
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
                         or 'str.join-on' on a list of strings; '++' is for list append"
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
            ExecutionError::BadNumberFormat(_, _) => {
                vec![
                    "valid number formats are: integers (e.g. 42), decimals (e.g. 3.14), and \
                     scientific notation (e.g. 1.5e10)"
                        .to_string(),
                    "use 'str' to convert numbers to strings; to convert strings to numbers use 'num'"
                        .to_string(),
                ]
            }
            ExecutionError::NotCallable(_, type_name) => not_callable_notes(type_name),
            ExecutionError::LookupFailure(_, key, suggestions, _) => {
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
            ExecutionError::ListIndexOutOfBounds(_, _) => {
                vec![
                    "use 'nth(n, list)' only when you know the list has at least n+1 elements"
                        .to_string(),
                    "check the list length with 'count' before indexing, or use pattern matching \
                     to handle empty or short lists safely"
                        .to_string(),
                ]
            }
            ExecutionError::HeadOfNonList(_, found) => {
                let mut notes = vec![
                    "head and tail require a list (e.g. [1, 2, 3]) as their argument".to_string(),
                ];
                if found.starts_with('"') {
                    notes.push(
                        "to concatenate strings, use string interpolation (e.g. \"{a}{b}\") \
                         or 'str.join-on', not '++' (which is for list append)"
                            .to_string(),
                    );
                    notes.push(
                        "to get individual characters of a string, use 'str.letters'".to_string(),
                    );
                } else if found == "number" || found.parse::<f64>().is_ok() {
                    notes.push("to create a list from a number, wrap it: '[n]'".to_string());
                }
                notes
            }
            ExecutionError::TailOfNonList(_, found) => {
                let mut notes = vec![
                    "head and tail require a list (e.g. [1, 2, 3]) as their argument".to_string(),
                ];
                if found.starts_with('"') {
                    notes.push(
                        "to concatenate strings, use string interpolation (e.g. \"{a}{b}\") \
                         or 'str.join-on', not '++' (which is for list append)"
                            .to_string(),
                    );
                }
                notes
            }
            ExecutionError::BadDateTimeComponents(_, _, _, _, _, _, _, _) => {
                vec![
                    "valid ranges: year (any integer), month (1–12), day (1–28/29/30/31 \
                     depending on month), hour (0–23), minute (0–59), second (0–59)"
                        .to_string(),
                ]
            }
            ExecutionError::BadTimeZone(_, _) => {
                vec![
                    "timezone must be a UTC offset string like '+0100', '-0530', or 'UTC'"
                        .to_string(),
                ]
            }
            ExecutionError::HeadOfEmptyList(_) | ExecutionError::TailOfEmptyList(_) => {
                vec![
                    "guard against empty lists with 'nil?', e.g. \
                     'if(xs nil?, default, xs head)'"
                        .to_string(),
                    "'head' and 'tail' are only defined on non-empty lists; \
                     use 'nth(0, xs)' or pattern matching if the list may be empty"
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

    /// Format a Smid with all available detail for trace dump diagnostics
    fn format_smid_detail(smid: Smid, source_map: &SourceMap) -> String {
        if !smid.is_valid() {
            return "invalid (no location)".to_string();
        }
        match source_map.source_info_for_smid(smid) {
            Some(info) => {
                let file_part = match info.file {
                    Some(f) => format!("file={f}"),
                    None => "file=none".to_string(),
                };
                let span_part = match info.span {
                    Some(span) => format!("span={}..{}", span.start(), span.end()),
                    None => "span=none".to_string(),
                };
                let ann_part = match &info.annotation {
                    Some(ann) => format!("ann=\"{ann}\""),
                    None => "ann=none".to_string(),
                };
                format!("smid={} {file_part} {span_part} {ann_part}", smid.get())
            }
            None => format!("smid={} (no source info)", smid.get()),
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
