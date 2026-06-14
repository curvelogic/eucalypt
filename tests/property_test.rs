#![cfg(not(target_arch = "wasm32"))]
//! Property-based tests for eucalypt — verifying structural invariants
//! across randomly generated inputs.
//!
//! Tests are organised into three groups:
//!
//! - **Group 1**: Round-trip and idempotence (formatter, source rendering)
//! - **Group 2**: Type-system invariants (reflexivity, transitivity, symmetry)
//! - **Group 3**: GC invariants (allocation + evaluation under GC verification)
//!
//! Run with `cargo test --test property_test`.
//! For deeper runs: `PROPTEST_CASES=10000 cargo test --test property_test`.
//!
//! The GC properties exercise the verifier enabled by `EU_GC_VERIFY=1`;
//! set that env var when running locally to activate GC assertions.

use eucalypt::core::typecheck::subtype::{is_consistent, is_subtype};
use eucalypt::core::typecheck::types::{Kind, Type, TypeVarId};
use eucalypt::syntax::export::format::{format_source, FormatterConfig};

use proptest::collection::vec as pvec;
use proptest::prelude::*;

// ── Configuration ─────────────────────────────────────────────────────────────

/// Proptest configuration driven by PROPTEST_CASES (default 256).
fn config() -> ProptestConfig {
    let cases = std::env::var("PROPTEST_CASES")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(256);
    ProptestConfig::with_cases(cases)
}

// ── Group 2: Type generators ──────────────────────────────────────────────────

/// Generate a leaf type (no recursive structure).
fn leaf_type() -> BoxedStrategy<Type> {
    prop_oneof![
        Just(Type::Number),
        Just(Type::String),
        Just(Type::Symbol),
        Just(Type::Bool),
        Just(Type::Null),
        Just(Type::Any),
        Just(Type::Top),
        Just(Type::Never),
        Just(Type::Set),
        Just(Type::Vec),
        // Literal types
        "[a-z]{1,5}".prop_map(Type::LiteralSymbol),
        "[a-z]{1,5}".prop_map(Type::LiteralString),
        // Star-kinded type variable
        "[a-z]{1,3}".prop_map(|s| Type::Var(TypeVarId(s), Kind::Star)),
    ]
    .boxed()
}

/// Generate an arbitrary `Type` with bounded depth to prevent blow-up.
fn arb_type_at(depth: u32) -> BoxedStrategy<Type> {
    if depth == 0 {
        return leaf_type();
    }

    let d = depth - 1;
    prop_oneof![
        // Leaves are heavily weighted at every level
        4 => leaf_type(),
        // Homogeneous containers
        1 => arb_type_at(d).prop_map(Type::list),
        1 => arb_type_at(d).prop_map(Type::non_empty),
        1 => arb_type_at(d).prop_map(Type::dict),
        // Partial (T | ExecutionError)
        1 => arb_type_at(d).prop_map(Type::partial),
        // Function type
        1 => (arb_type_at(d), arb_type_at(d))
                .prop_map(|(a, b)| Type::Function(Box::new(a), Box::new(b))),
        // Union of 2–3 types
        1 => pvec(arb_type_at(d), 2..=3).prop_map(Type::Union),
    ]
    .boxed()
}

/// Generate arbitrary `Type` values up to depth 4.
fn arb_type() -> BoxedStrategy<Type> {
    arb_type_at(4)
}

// ── Transitivity chain generator ──────────────────────────────────────────────

/// Generate a chain `(A, B, C)` where `A <: B` and `B <: C` both hold.
///
/// This avoids the `prop_assume!` filter-rejection problem by constructing
/// valid chains directly.
fn arb_transitivity_chain() -> BoxedStrategy<(Type, Type, Type)> {
    // Strategy: pick B, then construct A <: B and B <: C using known-good forms.
    arb_type()
        .prop_flat_map(|b| {
            let a_strategy = {
                let b2 = b.clone();
                prop_oneof![
                    // Never <: B
                    Just(Type::Never),
                    // B <: B (reflexive)
                    Just(b2.clone()),
                    // LiteralString <: String (if B is String)
                    "[a-z]{1,5}".prop_map(Type::LiteralString),
                    // LiteralSymbol <: Symbol (if B is Symbol)
                    "[a-z]{1,5}".prop_map(Type::LiteralSymbol),
                ]
            };
            let c_strategy = {
                let b3 = b.clone();
                prop_oneof![
                    // B <: Top
                    Just(Type::Top),
                    // B <: B (reflexive)
                    Just(b3.clone()),
                    // B <: B | X
                    arb_type().prop_map(move |x| Type::Union(vec![b3.clone(), x])),
                    // B <: Any
                    Just(Type::Any),
                ]
            };
            (a_strategy, Just(b), c_strategy)
        })
        .boxed()
}

// ── Group 1: Source generators ────────────────────────────────────────────────

/// Generate a eucalypt number literal.
fn arb_number_source() -> BoxedStrategy<String> {
    prop_oneof![
        (-1000i64..=1000i64).prop_map(|n| n.to_string()),
        (0.0f64..=1000.0f64).prop_map(|f| format!("{f:.3}")),
    ]
    .boxed()
}

/// Generate content safe to embed in a eucalypt double-quoted string.
fn arb_safe_string_content() -> BoxedStrategy<String> {
    "[a-zA-Z0-9 _-]{0,20}".prop_map(|s| s).boxed()
}

/// Generate a simple valid eucalypt literal expression (one value).
fn arb_literal_source() -> BoxedStrategy<String> {
    prop_oneof![
        arb_number_source(),
        arb_safe_string_content().prop_map(|s| format!("\"{s}\"")),
        "[a-z][a-z0-9-]{0,8}".prop_map(|s| format!(":{s}")),
        Just("true".to_string()),
        Just("false".to_string()),
        Just("null".to_string()),
    ]
    .boxed()
}

/// Generate a simple binding declaration (key: literal-value).
fn arb_binding_source() -> BoxedStrategy<String> {
    ("[a-z][a-z0-9-]{0,6}", arb_literal_source())
        .prop_map(|(key, val)| format!("{key}: {val}\n"))
        .boxed()
}

/// Generate a multi-binding eucalypt block as source.
fn arb_multi_block_source() -> BoxedStrategy<String> {
    pvec(arb_binding_source(), 1..=5)
        .prop_map(|decls| decls.join(""))
        .boxed()
}

/// Generate a list literal.
fn arb_list_source() -> BoxedStrategy<String> {
    pvec(arb_literal_source(), 0..=6)
        .prop_map(|items| format!("[{}]", items.join(", ")))
        .boxed()
}

/// Generate a small valid eucalypt source program.
fn arb_program_source() -> BoxedStrategy<String> {
    prop_oneof![
        arb_literal_source(),
        arb_list_source(),
        arb_multi_block_source(),
    ]
    .boxed()
}

// ── Group 2: Type system property tests ──────────────────────────────────────

proptest! {
    #![proptest_config(config())]

    /// Every type is a subtype of itself.
    #[test]
    fn prop_subtyping_reflexivity(t in arb_type()) {
        prop_assert!(
            is_subtype(&t, &t),
            "is_subtype(T, T) must hold, but failed for {:?}", t
        );
    }

    /// Consistency is symmetric: `T ~ U` iff `U ~ T`.
    #[test]
    fn prop_consistency_symmetry(s in arb_type(), t in arb_type()) {
        let forward = is_consistent(&s, &t);
        let backward = is_consistent(&t, &s);
        prop_assert_eq!(
            forward, backward,
            "is_consistent must be symmetric: {:?} ~ {:?} = {}, {:?} ~ {:?} = {}",
            s, t, forward, t, s, backward
        );
    }

    /// Subtyping is transitive: A <: B and B <: C implies A <: C.
    ///
    /// The chain (A, B, C) is constructed so that A <: B and B <: C are known
    /// to hold by construction, avoiding the filter-rejection problem.
    #[test]
    fn prop_subtyping_transitivity((a, b, c) in arb_transitivity_chain()) {
        // A <: B should hold by construction — verify the precondition.
        prop_assume!(is_subtype(&a, &b));
        // B <: C should hold by construction — verify the precondition.
        prop_assume!(is_subtype(&b, &c));

        prop_assert!(
            is_subtype(&a, &c),
            "transitivity failed: {:?} <: {:?} and {:?} <: {:?}, but NOT {:?} <: {:?}",
            a, b, b, c, a, c
        );
    }
}

// ── Group 1: Formatter property tests ────────────────────────────────────────

proptest! {
    #![proptest_config(config())]

    /// `eu fmt` is idempotent: `format_source(format_source(s)) == format_source(s)`.
    #[test]
    fn prop_fmt_idempotence(source in arb_multi_block_source()) {
        let cfg = FormatterConfig::new(80, 4, false);

        // If parsing fails, skip — we're testing formatter behaviour, not parser.
        let first = match format_source(&source, &cfg) {
            Ok(s) => s,
            Err(_) => return Ok(()),
        };

        let second = match format_source(&first, &cfg) {
            Ok(s) => s,
            Err(e) => return Err(TestCaseError::fail(format!(
                "second format_source failed: {e}\nsource: {source:?}\nfirst: {first:?}"
            ))),
        };

        prop_assert_eq!(
            first, second,
            "eu fmt is not idempotent on: {:?}", source
        );
    }

    /// Rendering is deterministic: two calls on the same source are identical.
    #[test]
    fn prop_fmt_determinism(source in arb_program_source()) {
        let cfg = FormatterConfig::new(80, 4, false);
        let first = format_source(&source, &cfg);
        let second = format_source(&source, &cfg);
        prop_assert_eq!(first, second,
            "format_source is not deterministic on {:?}", source);
    }
}

// ── Group 3: GC invariants ────────────────────────────────────────────────────
//
// These tests run generated programs through the full eucalypt evaluation
// pipeline.  When EU_GC_VERIFY=1 (set in the GC-verified CI job), any reachable
// object missed by the mark phase causes a panic that proptest catches and shrinks.
//
// In CI the GC-verified job sets EU_GC_VERIFY=2 automatically.

#[cfg(all(not(target_arch = "wasm32"), not(target_os = "windows")))]
mod gc_properties {
    use super::*;
    use eucalypt::driver::error::EucalyptError;
    use eucalypt::driver::statistics::Statistics;
    use eucalypt::driver::statistics::Timings;
    use eucalypt::driver::{eval, options::EucalyptOptions, prepare, source::SourceLoader};
    use eucalypt::syntax::input::Input;
    use std::str::FromStr;

    /// Evaluate a eucalypt source snippet and return whether it succeeded.
    ///
    /// Any GC panic propagates out (proptest catches it); evaluation errors
    /// (type mismatch, unresolved names, etc.) are ignored — we are testing
    /// GC correctness, not evaluation correctness.
    fn eval_source(source: &str) -> Result<(), EucalyptError> {
        let dir = tempfile::tempdir().map_err(|e| {
            EucalyptError::FileCouldNotBeRead("tempdir".to_string(), Some(e.to_string()))
        })?;
        let path = dir.path().join("prop.eu");
        std::fs::write(&path, source).map_err(|e| {
            EucalyptError::FileCouldNotBeRead(path.display().to_string(), Some(e.to_string()))
        })?;

        let path_str = path.to_string_lossy().into_owned();
        let input = Input::from_str(&path_str)
            .map_err(|e| EucalyptError::UnknownResource(e.to_string()))?;

        let opt = EucalyptOptions::default()
            .with_explicit_inputs(vec![input])
            .build();

        let mut loader = SourceLoader::new(vec![]);
        let mut timings = Timings::default();

        let _cmd = prepare::prepare(&opt, &mut loader, &mut timings)?;

        let mut stats = Statistics::default();
        let mut out: Vec<u8> = Vec::new();
        let mut err_buf: Vec<u8> = Vec::new();
        let mut executor = eval::Executor::from(loader);
        executor.capture_output(Box::new(&mut out), Box::new(&mut err_buf));
        // Ignore evaluation errors — only GC panics matter here.
        let _ = executor.execute(&opt, &mut stats, "yaml".to_string());

        Ok(())
    }

    /// GC-specific configuration: fewer cases since each spawns the full pipeline.
    fn gc_config() -> ProptestConfig {
        let cases = std::env::var("PROPTEST_GC_CASES")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(64);
        ProptestConfig::with_cases(cases)
    }

    proptest! {
        #![proptest_config(gc_config())]

        /// Literal programs evaluate without GC violations.
        ///
        /// When `EU_GC_VERIFY=1` is set, any missed-mark panic propagates out
        /// and proptest reports a shrunk counterexample.
        #[test]
        fn prop_gc_no_violations_literal(source in arb_literal_source()) {
            let _ = eval_source(&source);
        }

        /// List programs evaluate without GC violations.
        #[test]
        fn prop_gc_no_violations_list(source in arb_list_source()) {
            let _ = eval_source(&source);
        }

        /// Block programs evaluate without GC violations.
        #[test]
        fn prop_gc_no_violations_block(source in arb_multi_block_source()) {
            let _ = eval_source(&source);
        }
    }
}
