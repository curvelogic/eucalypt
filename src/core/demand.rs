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
    /// Is this binding part of a recursive cycle?  Set by demand
    /// analysis for bindings that can reach themselves through the
    /// dependency graph.  When true, black-holing is required to
    /// detect infinite loops, so update elision must be suppressed
    /// even for Strict bindings.
    pub recursive: bool,
}

impl Demand {
    /// Returns `true` when the STG compiler should skip the Update frame
    /// (i.e. emit `Value` rather than `Thunk`).
    ///
    /// Fires for bindings already in WHNF, bindings used at most once,
    /// and absent bindings.  The demand analysis fixup pass forces Multi
    /// on `DefaultBlockLet` bindings whose body is a `Block` constructor
    /// (rendered scopes), so `AtMostOnce` here only applies to
    /// generalised-lookup blocks and non-block `Let` scopes where the
    /// cardinality is genuinely sound.
    ///
    /// Note: Strict demand alone does NOT trigger update elision here.
    /// Multi-use Strict bindings would re-evaluate on each Enter as
    /// Values, which is catastrophically expensive.  Strict bindings
    /// are instead handled by the `LetStrict` compilation path which
    /// evaluates eagerly at definition time and stores the result.
    pub fn skip_update(self) -> bool {
        self.whnf
            || self.cardinality == Cardinality::AtMostOnce
            || self.cardinality == Cardinality::Absent
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
    /// Used for unused function arguments (distinct from dead bindings).
    Absent,
    /// Used at most once — safe to skip Update (no memoisation benefit).
    AtMostOnce,
    /// Used more than once — memoisation may be worthwhile.
    Multi,
}

impl Cardinality {
    /// Least upper bound: combine cardinalities from alternative branches.
    pub fn join(self, other: Cardinality) -> Cardinality {
        match (self, other) {
            (Cardinality::Absent, x) | (x, Cardinality::Absent) => x,
            (Cardinality::AtMostOnce, Cardinality::AtMostOnce) => Cardinality::AtMostOnce,
            _ => Cardinality::Multi,
        }
    }
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

impl Strictness {
    /// Least upper bound: combine strictness from alternative branches.
    pub fn join(self, other: Strictness) -> Strictness {
        match (self, other) {
            (Strictness::Strict, Strictness::Strict) => Strictness::Strict,
            _ => Strictness::Lazy,
        }
    }
}

impl Demand {
    /// Least upper bound: combine demands from alternative branches.
    pub fn lub(self, other: Demand) -> Demand {
        Demand {
            cardinality: self.cardinality.join(other.cardinality),
            strictness: self.strictness.join(other.strictness),
            whnf: self.whnf && other.whnf,
            recursive: self.recursive || other.recursive,
        }
    }

    /// Conservative absent demand (unused argument).
    pub fn absent() -> Self {
        Self {
            cardinality: Cardinality::Absent,
            ..Default::default()
        }
    }

    /// A demand marking a binding as strict and used at most once.
    pub fn strict_once() -> Self {
        Self {
            strictness: Strictness::Strict,
            cardinality: Cardinality::AtMostOnce,
            ..Default::default()
        }
    }

    /// A demand marking a binding as lazy and used at most once.
    pub fn lazy_once() -> Self {
        Self {
            strictness: Strictness::Lazy,
            cardinality: Cardinality::AtMostOnce,
            ..Default::default()
        }
    }

    /// A demand marking a binding as strict and used multiple times.
    pub fn strict_multi() -> Self {
        Self {
            strictness: Strictness::Strict,
            cardinality: Cardinality::Multi,
            ..Default::default()
        }
    }

    /// A demand marking a binding as lazy and used multiple times.
    pub fn lazy_multi() -> Self {
        Self {
            strictness: Strictness::Lazy,
            cardinality: Cardinality::Multi,
            ..Default::default()
        }
    }

    /// Scale a demand by an outer demand's cardinality.
    ///
    /// Used in LetRec fixed-point propagation: if binding `j` is demanded
    /// with cardinality `outer`, and `j`'s RHS uses binding `i` with demand
    /// `self`, then `i`'s indirect demand through `j` is `self.scale(outer)`.
    ///
    /// Cardinality multiplication:
    /// - `Absent × _ = Absent` (j never evaluated → no propagation)
    /// - `_ × Absent = Absent`
    /// - `Multi × AtMostOnce = Multi` (j evaluated many times, each using i once)
    /// - `AtMostOnce × Multi = Multi`
    /// - `AtMostOnce × AtMostOnce = AtMostOnce`
    /// - `Multi × Multi = Multi`
    pub fn scale(self, outer: Demand) -> Demand {
        let cardinality = match (self.cardinality, outer.cardinality) {
            (Cardinality::Absent, _) | (_, Cardinality::Absent) => Cardinality::Absent,
            (Cardinality::AtMostOnce, Cardinality::AtMostOnce) => Cardinality::AtMostOnce,
            _ => Cardinality::Multi,
        };
        // Strictness: strict only if both are strict
        let strictness = match (self.strictness, outer.strictness) {
            (crate::core::demand::Strictness::Strict, crate::core::demand::Strictness::Strict) => {
                crate::core::demand::Strictness::Strict
            }
            (crate::core::demand::Strictness::Lazy, _)
            | (_, crate::core::demand::Strictness::Lazy) => crate::core::demand::Strictness::Lazy,
            _ => crate::core::demand::Strictness::Unknown,
        };
        Demand {
            cardinality,
            strictness,
            whnf: false,
            recursive: self.recursive || outer.recursive,
        }
    }

    /// Sequentially combine demands: used in both `self` and `other` contexts.
    ///
    /// Unlike `lub` (which combines alternative branches), `plus` combines
    /// sequential uses. Cardinalities add (once + once = multi), and
    /// strictness takes the stricter value.
    pub fn plus(self, other: Demand) -> Demand {
        let cardinality = match (self.cardinality, other.cardinality) {
            (Cardinality::Absent, x) | (x, Cardinality::Absent) => x,
            (Cardinality::Unknown, _) | (_, Cardinality::Unknown) => Cardinality::Unknown,
            _ => Cardinality::Multi,
        };
        let strictness = match (self.strictness, other.strictness) {
            (Strictness::Strict, _) | (_, Strictness::Strict) => Strictness::Strict,
            (Strictness::Lazy, _) | (_, Strictness::Lazy) => Strictness::Lazy,
            _ => Strictness::Unknown,
        };
        Demand {
            cardinality,
            strictness,
            whnf: self.whnf && other.whnf,
            recursive: self.recursive || other.recursive,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unknown_demand_does_not_skip_update() {
        let d = Demand::default();
        assert!(!d.skip_update(), "Unknown demand should NOT skip update");
    }

    #[test]
    fn at_most_once_skips_update() {
        // AtMostOnce → Value is now sound: the demand analysis fixup
        // forces Multi on rendered block scopes (DefaultBlockLet with
        // Block body), so AtMostOnce only survives for genuinely
        // single-use bindings.
        let d = Demand::at_most_once();
        assert!(
            d.skip_update(),
            "AtMostOnce should skip update (sound after rendered-block fixup)"
        );
    }

    #[test]
    fn absent_skips_update() {
        let d = Demand::absent();
        assert!(d.skip_update(), "Absent demand should skip update");
    }

    #[test]
    fn multi_does_not_skip_update() {
        let d = Demand {
            cardinality: Cardinality::Multi,
            ..Default::default()
        };
        assert!(!d.skip_update(), "Multi cardinality should NOT skip update");
    }

    #[test]
    fn whnf_skips_update() {
        let d = Demand::whnf();
        assert!(d.skip_update(), "WHNF demand should skip update");
    }

    #[test]
    fn strict_alone_does_not_skip_update() {
        let d = Demand::strict();
        assert!(
            !d.skip_update(),
            "Strict-only demand should NOT skip update (handled by LetStrict instead)"
        );
    }

    #[test]
    fn whnf_overrides_multi_cardinality() {
        let d = Demand {
            cardinality: Cardinality::Multi,
            whnf: true,
            ..Default::default()
        };
        assert!(
            d.skip_update(),
            "WHNF should skip update even with Multi cardinality"
        );
    }

    #[test]
    fn default_demand_is_conservative() {
        let d = Demand::default();
        assert_eq!(d.cardinality, Cardinality::Unknown);
        assert_eq!(d.strictness, Strictness::Unknown);
        assert!(!d.whnf);
    }
}
