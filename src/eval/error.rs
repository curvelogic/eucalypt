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
    #[error("bad format string: {0}")]
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
    #[error("tried to call a value that is not a function")]
    NotCallable(Smid),
    #[error("intrinsic {1} expected value in strict position")]
    NotValue(Smid, String),
    #[error("bad regex ({0})")]
    BadRegex(String),
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
    #[error("unknown format ({0})")]
    UnknownFormat(String),
    #[error("cannot combine numbers ({0}, {1}) into same numeric domain")]
    NumericDomainError(Number, Number),
    #[error("out of range error operating on numbers ({0}, {1})")]
    NumericRangeError(Number, Number),
    #[error("division by zero")]
    DivisionByZero,
    #[error("expected branch continuation")]
    ExpectedBranchContinuation,
    #[error("type mismatch: expected {}, found {}", display_expected_tags(.1), display_data_tag(*.0))]
    NoBranchForDataTag(u8, Vec<u8>),
    #[error("no branch for native")]
    NoBranchForNative,
    #[error("cannot return function into case table without default")]
    CannotReturnFunToCase,
    #[error("panic: {0}")]
    Panic(String),
    #[error("machine did not terminate after {0} steps")]
    DidntTerminate(usize),
    #[error("infinite loop detected: binding refers to itself")]
    BlackHole,
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
            ExecutionError::NotCallable(s) => *s,
            ExecutionError::NotValue(s, _) => *s,
            ExecutionError::NotScalar(s) => *s,
            ExecutionError::Compile(compile_error) => compile_error.smid(),
            _ => Smid::default(),
        }
    }
}

impl ExecutionError {
    pub fn to_diagnostic(&self, source_map: &SourceMap) -> Diagnostic<usize> {
        source_map.diagnostic(self)
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
