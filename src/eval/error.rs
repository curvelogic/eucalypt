//! Execution errors
use crate::common::sourcemap::{HasSmid, Smid, SourceMap};
use crate::eval::types::IntrinsicType;
use codespan_reporting::diagnostic::Diagnostic;
use serde_json::Number;
use std::io;
use thiserror::Error;

use super::{memory::bump, stg::compiler::CompileError};

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
    #[error("lookup failure")]
    LookupFailure(Smid),
    #[error("type mismatch: expected {1}, found {2}")]
    TypeMismatch(Smid, IntrinsicType, IntrinsicType),
    #[error("unknown intrinsic {1}")]
    UnknownIntrinsic(Smid, String),
    #[error("call of not callable")]
    NotCallable(Smid),
    #[error("intrinsic {1} expected value in strict position")]
    NotValue(Smid, String),
    #[error("intrinsic expected numeric value in strict position")]
    NotEvaluatedNumber(Smid),
    #[error("intrinsic expected string value in strict position")]
    NotEvaluatedString(Smid),
    #[error("intrinsic expected zoned datetime value in strict position")]
    NotEvaluatedZdt(Smid),
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
    #[error("expected branch continuation")]
    ExpectedBranchContinuation,
    #[error("no branch for data tag {0}")]
    NoBranchForDataTag(u8),
    #[error("no branch for native")]
    NoBranchForNative,
    #[error("cannot return function into case table without default")]
    CannotReturnFunToCase,
    #[error("panic: {0}")]
    Panic(String),
    #[error("machine did not terminate after {0} steps")]
    DidntTerminate(usize),
    #[error("entered a black hole")]
    BlackHole,
    #[error(transparent)]
    Compile(#[from] CompileError),
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error("expected label on stack")]
    ExpectedLabel,
    #[error("expected closure on stack")]
    ExpectedClosure,
}

impl From<bump::AllocError> for ExecutionError {
    fn from(_: bump::AllocError) -> Self {
        ExecutionError::AllocationError
    }
}

impl From<super::memory::heap::HeapError> for ExecutionError {
    fn from(e: super::memory::heap::HeapError) -> Self {
        match e {
            super::memory::heap::HeapError::OutOfMemory => ExecutionError::AllocationError,
            super::memory::heap::HeapError::EmergencyCollectionFailed => ExecutionError::AllocationError,
            super::memory::heap::HeapError::InvalidAllocationSize => ExecutionError::AllocationError,
            super::memory::heap::HeapError::FragmentationError => ExecutionError::AllocationError,
            super::memory::heap::HeapError::BlockAllocationFailed => ExecutionError::AllocationError,
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
            ExecutionError::LookupFailure(s) => *s,
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
    pub fn env_trace(&self) -> Option<Vec<Smid>> {
        if let ExecutionError::Traced(_, trace, _) = self {
            Some(trace.clone())
        } else {
            None
        }
    }

    /// Access stack trace if present
    pub fn stack_trace(&self) -> Option<Vec<Smid>> {
        if let ExecutionError::Traced(_, _, trace) = self {
            Some(trace.clone())
        } else {
            None
        }
    }
}
