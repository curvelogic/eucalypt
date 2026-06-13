//! Demand annotation for core bindings.
//!
//! A `Demand` captures what is known about how a binding is used — its
//! cardinality and strictness — at the point the annotation is set.  The
//! annotation starts conservative (`Unknown`) and is refined by analysis
//! passes or by the STG compiler's structural heuristics.
//!
//! ## Soundness
//!
//! The annotation is **conservative by default**.  `Unknown` cardinality and
//! `Unknown` strictness cause the STG compiler to emit a `Thunk` (the safe
//! choice).  A wrong `AtMostOnce` at worst causes redundant re-evaluation
//! for a pure binding — never an incorrect result.  The annotation therefore
//! only affects performance, never correctness.
//!
//! ## Lattice
//!
//! Both axes are deliberately minimal two-point lattices for 0.9.  Richer
//! demands (head-strict, spine-strict, projection-based) are deferred to
//! post-W11 refinement.

use serde::{Deserialize, Serialize};

/// Demand information for a binding, accumulated through the pipeline.
///
/// The STG compiler consults this at `take_lambda_form` time to decide
/// whether to emit a `Value` or a `Thunk`.  Populators set individual
/// fields; unknown fields stay at their `Default` (conservative) value.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Demand {
    /// How many times is this binding used?
    pub cardinality: Cardinality,
    /// Must this binding be evaluated by its context?
    pub strictness: Strictness,
    /// Is this known to already be in WHNF (set during STG compilation
    /// after compiling the binding's body to `StgSyn`)?
    pub whnf: bool,
}

impl Demand {
    /// Returns `true` when the STG compiler should skip the Update frame
    /// (i.e. emit `Value` rather than `Thunk`).
    pub fn skip_update(self) -> bool {
        self.whnf || self.cardinality == Cardinality::AtMostOnce
    }

    /// Convenience: a demand marking a binding as used at most once.
    pub fn at_most_once() -> Self {
        Self {
            cardinality: Cardinality::AtMostOnce,
            ..Default::default()
        }
    }

    /// Convenience: a demand marking a binding as strict.
    pub fn strict() -> Self {
        Self {
            strictness: Strictness::Strict,
            ..Default::default()
        }
    }

    /// Convenience: a demand marking a binding whose body is already in WHNF.
    pub fn whnf() -> Self {
        Self {
            whnf: true,
            ..Default::default()
        }
    }
}

/// Usage cardinality — how many times a binding is used.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum Cardinality {
    /// No cardinality information yet (conservative default).
    #[default]
    Unknown,
    /// Used at most once — safe to skip Update (no memoisation benefit).
    AtMostOnce,
    /// Used more than once — memoisation may be worthwhile.
    Multi,
}

/// Strictness status — whether the binding will definitely be evaluated.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum Strictness {
    /// No strictness information yet (conservative default).
    #[default]
    Unknown,
    /// Definitely lazy — not necessarily evaluated.
    Lazy,
    /// Definitely strict — will be evaluated by context.
    Strict,
}
