//! Errors encountered during desugaring to and processing core
use crate::common::sourcemap::SourceMap;
use crate::common::sourcemap::{HasSmid, Smid};
use crate::syntax::input::Input;
use codespan_reporting::diagnostic::Diagnostic;
use thiserror::Error;

#[derive(Eq, PartialEq, Debug, Clone, Error)]
pub enum CoreError {
    #[error("invalid metadata")]
    InvalidMetadataDesugarPhase(Smid),
    #[error("unknown embedding: {0}")]
    UnknownEmbedding(String),
    #[error("invalid embedding - {0}")]
    InvalidEmbedding(String, Smid),
    #[error("no parsed AST found for input {0}")]
    NoParsedAstFor(Box<Input>),
    #[error("too few operands available for operator")]
    TooFewOperands(Smid),
    /// Anaphora of different kinds ('_', '_0', sections) were mixed in one expression.
    ///
    /// The three anaphora kinds are:
    /// - *Numberless*: `_` — one anonymous parameter per use
    /// - *Numbered*: `_0`, `_1`, `_2`, … — creates explicitly-indexed parameters
    /// - *Section*: `(+ 1)`, `(< 5)` — creates an operator section
    #[error(
        "mixed anaphora: cannot use '_', '_0'/'_N', and section expressions in the same expression"
    )]
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
    #[error("target '{0}' not found\n  help: use 'eu list-targets' to see available targets")]
    TargetNotFound(String),
    #[error("target {0} could not be referenced")]
    BadTarget(String),
    #[error("monadic block used without a monad spec — bracket pair '{0}' has no 'bind'/'return' metadata")]
    NoMonadSpec(String, Smid),
    #[error("monadic block must contain at least one declaration")]
    EmptyMonadicBlock(Smid),
    #[error("bracket block definition body must be marked with ':monad' — use '{{ :monad bind: {0} return: {1} }}'")]
    MonadSpecMissingMarker(String, String, Smid),
    /// An implicit anaphor was required to fill adjacent operators but no
    /// source location was available to attach to it.  This typically
    /// happens when operators in a metadata block key use dot notation
    /// (e.g. `` ` { x.y: val } ``) which is not valid eucalypt syntax.
    #[error("invalid block key: dotted names are not allowed as block keys")]
    NoSmidForImplicitAnaphor,
    #[error("nested list destructuring is limited to one level of nesting")]
    DeepNestedListDestructure(Smid),
    #[error(
        "nested block destructuring is not supported; use dot-lookup in the function body instead"
    )]
    NestedBlockDestructure(Smid),
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
            NoMonadSpec(_, s) => s,
            EmptyMonadicBlock(s) => s,
            MonadSpecMissingMarker(_, _, s) => s,
            DeepNestedListDestructure(s) => s,
            NestedBlockDestructure(s) => s,
            _ => Smid::default(),
        }
    }
}

impl CoreError {
    pub fn to_diagnostic(&self, source_map: &SourceMap) -> Diagnostic<usize> {
        match self {
            CoreError::MixedAnaphora(_) => source_map.diagnostic(self).with_notes(vec![
                "there are three kinds of expression anaphora and they cannot be mixed".to_string(),
                "unnumbered '_' (one parameter per use), numbered '_0', '_1', … (parameters at explicit positions), \
                 and sections like (+ 1) each create a different kind of anonymous function"
                    .to_string(),
                "use one kind consistently: e.g. `_0 > _1` not `_0 > _`".to_string(),
            ]),
            CoreError::InvalidMergeBase() => source_map.diagnostic(self).with_notes(vec![
                "some input formats (csv, text, etc.) that read as lists need to be assigned names"
                    .to_string(),
                "perhaps you need to name one or more of your inputs (<name>=<input>)".to_string(),
            ]),
            CoreError::NoSmidForImplicitAnaphor => source_map.diagnostic(self).with_notes(vec![
                "block keys must be simple names (e.g. `x`), not dotted paths (e.g. `x.y`)".to_string(),
                "example of valid metadata: `` ` { x: val } `` — use 'x', not 'x.y' as the key".to_string(),
                "if you need nested metadata, use nested blocks: `` ` { x: { y: val } } ``".to_string(),
            ]),
            _ => source_map.diagnostic(self),
        }
    }
}
