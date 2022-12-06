//! Errors encountered during desugaring to and processing core
use crate::common::sourcemap::SourceMap;
use crate::common::sourcemap::{HasSmid, Smid};
use crate::syntax::input::Input;
use codespan_reporting::diagnostic::Diagnostic;
use thiserror::Error;

#[derive(Eq, PartialEq, Debug, Clone, Error)]
#[allow(clippy::result_large_err)]
pub enum CoreError {
    #[error("invalid metadata")]
    InvalidMetadataDesugarPhase(Smid),
    #[error("unknown embedding: {0}")]
    UnknownEmbedding(String),
    #[error("invalid embedding - {0}")]
    InvalidEmbedding(String, Smid),
    #[error("no parsed AST found for input {0}")]
    NoParsedAstFor(Input),
    #[error("too few operands available for operator")]
    TooFewOperands(Smid),
    #[error("cannot mix anaphora types (numberless, numbered and section)")]
    MixedAnaphora(Smid),
    #[error("found temporary pseudo-operators remaining in evaluand")]
    UneliminatedPseudoOperators,
    #[error("found operator soup within unresolved precedence")]
    UneliminatedSoup(Smid),
    #[error("found eliminated code markers remaining in evaluand")]
    UnprunedEliminations,
    #[error("unresolved variable: {1}")]
    UnresolvedVariable(Smid, String),
    #[error("variable redeclared: {1}")]
    RedeclaredVariable(Smid, String),
    #[error("empty merge")]
    EmptyMerge(),
    #[error("merge base was not valid for merge")]
    InvalidMergeBase(),
    #[error("target {0} not found")]
    TargetNotFound(String),
    #[error("target {0} could not be referenced")]
    BadTarget(String),
}

impl HasSmid for CoreError {
    fn smid(&self) -> Smid {
        use self::CoreError::*;

        match *self {
            InvalidMetadataDesugarPhase(s) => s,
            InvalidEmbedding(_, s) => s,
            TooFewOperands(s) => s,
            MixedAnaphora(s) => s,
            UnresolvedVariable(s, _) => s,
            RedeclaredVariable(s, _) => s,
            _ => Smid::default(),
        }
    }
}

impl CoreError {
    pub fn to_diagnostic(&self, source_map: &SourceMap) -> Diagnostic<usize> {
        match self {
            CoreError::InvalidMergeBase() => {
                source_map.diagnostic(self).with_notes(vec![
		    "some input formats (csv, text, etc.) that read as lists need to be assigned names".to_string(),
		    "perhaps you need to name one or more of your inputs (<name>=<input>)".to_string()])
            }
            _ => source_map.diagnostic(self),
        }
    }
}
