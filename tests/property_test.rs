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

use eucalypt::core::expr::{BlockMap, Expr, Primitive, RcExpr};
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

/// Generate a valid eucalypt identifier.
fn arb_ident_source() -> BoxedStrategy<String> {
    "[a-z][a-z0-9-]{0,6}".prop_map(|s| s).boxed()
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

/// Generate a binary operator.
fn arb_operator() -> BoxedStrategy<&'static str> {
    prop_oneof![
        Just("+"),
        Just("-"),
        Just("*"),
        Just("/"),
        Just("="),
        Just("!="),
        Just("<"),
        Just(">"),
        Just("<="),
        Just(">="),
        Just("++"),
    ]
    .boxed()
}

/// Generate an operator section like `(+ 1)` or `(* 2)`.
fn arb_section_source() -> BoxedStrategy<String> {
    (arb_operator(), arb_number_source())
        .prop_map(|(op, n)| format!("({op} {n})"))
        .boxed()
}

/// Generate an expression anaphor like `(_ + 1)`.
fn arb_anaphora_source() -> BoxedStrategy<String> {
    (arb_operator(), arb_number_source())
        .prop_map(|(op, n)| format!("(_ {op} {n})"))
        .boxed()
}

/// Generate a function call expression like `f(x)` or `f(x, y)`.
/// Note: NO whitespace before `(` — `f(x)` is a call, `f (x)` is catenation.
fn arb_call_source() -> BoxedStrategy<String> {
    (arb_ident_source(), pvec(arb_literal_source(), 1..=3))
        .prop_map(|(name, args)| format!("{name}({})", args.join(", ")))
        .boxed()
}

/// Generate an infix operator expression like `1 + 2`.
fn arb_infix_source() -> BoxedStrategy<String> {
    (arb_literal_source(), arb_operator(), arb_literal_source())
        .prop_map(|(l, op, r)| format!("({l} {op} {r})"))
        .boxed()
}

/// Generate a lookup expression like `.key`.
fn arb_lookup_source() -> BoxedStrategy<String> {
    arb_ident_source().prop_map(|key| format!(".{key}")).boxed()
}

/// Generate a catenation expression like `xs map(f)`.
fn arb_catenation_source() -> BoxedStrategy<String> {
    (
        arb_literal_source(),
        arb_ident_source(),
        arb_literal_source(),
    )
        .prop_map(|(receiver, func, arg)| format!("({receiver} {func}({arg}))"))
        .boxed()
}

/// Generate a depth-limited expression for source-level testing.
fn arb_expr_source_at(depth: u32) -> BoxedStrategy<String> {
    if depth == 0 {
        return arb_literal_source();
    }

    prop_oneof![
        4 => arb_literal_source(),
        1 => arb_infix_source(),
        1 => arb_section_source(),
        1 => arb_anaphora_source(),
        1 => arb_call_source(),
        1 => arb_lookup_source(),
        1 => arb_catenation_source(),
    ]
    .boxed()
}

/// Generate a binding whose value may be a richer expression.
fn arb_rich_binding_source() -> BoxedStrategy<String> {
    (arb_ident_source(), arb_expr_source_at(1))
        .prop_map(|(key, val)| format!("{key}: {val}\n"))
        .boxed()
}

/// Generate a block with richer bindings (including operators, calls, lookups).
fn arb_rich_block_source() -> BoxedStrategy<String> {
    pvec(arb_rich_binding_source(), 1..=5)
        .prop_map(|decls| decls.join(""))
        .boxed()
}

/// Generate a small valid eucalypt source program.
fn arb_program_source() -> BoxedStrategy<String> {
    prop_oneof![
        4 => arb_literal_source(),
        2 => arb_list_source(),
        2 => arb_multi_block_source(),
        1 => arb_infix_source(),
        1 => arb_call_source(),
        1 => arb_section_source(),
        1 => arb_anaphora_source(),
        1 => arb_lookup_source(),
        1 => arb_catenation_source(),
        1 => arb_rich_block_source(),
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

// ── W5p2: RcExpr generators and round-trip properties ────────────────────────
//
// Generates simple core expressions (literals, lists, blocks), renders them
// to eucalypt source strings, and verifies that the rendered source is
// parseable and that re-formatting it is idempotent (render → format ≡
// render → format → format).

/// Render a `Primitive` value as eucalypt source text.
fn render_primitive(p: &Primitive) -> String {
    match p {
        Primitive::Str(s) => {
            // Escape backslashes and double-quotes for embedding in a
            // double-quoted string literal.
            let escaped = s.replace('\\', "\\\\").replace('"', "\\\"");
            format!("\"{escaped}\"")
        }
        Primitive::Sym(s) => format!(":{s}"),
        Primitive::Num(n) => n.to_string(),
        Primitive::Bool(b) => (if *b { "true" } else { "false" }).to_string(),
        Primitive::Null => "null".to_string(),
        Primitive::TypeData(s) => {
            let escaped = s.replace('\\', "\\\\").replace('"', "\\\"");
            format!("s\"{escaped}\"")
        }
    }
}

/// Render a depth-limited `RcExpr` as eucalypt source text.
///
/// Only the subset generated by `arb_rc_expr_at` is handled (literals, lists,
/// blocks).  Anything else is rendered as `null`.
fn render_rc_expr(expr: &RcExpr) -> String {
    match &*expr.inner {
        Expr::Literal(_, p) => render_primitive(p),
        Expr::List(_, items) => {
            let inner: Vec<String> = items.iter().map(render_rc_expr).collect();
            format!("[{}]", inner.join(", "))
        }
        Expr::Block(_, block_map) => {
            if block_map.is_empty() {
                return "{}".to_string();
            }
            let entries: Vec<String> = block_map
                .iter()
                .map(|(k, v)| format!("{k}: {}", render_rc_expr(v)))
                .collect();
            format!("{{ {} }}", entries.join("\n"))
        }
        // Fallback: render as null for any unsupported variant.
        _ => "null".to_string(),
    }
}

/// Generate a leaf `RcExpr` — one of the primitive literal types.
fn arb_rc_literal() -> BoxedStrategy<RcExpr> {
    prop_oneof![
        (-1000i64..=1000i64).prop_map(|n| RcExpr::from(Expr::Literal(
            eucalypt::common::sourcemap::Smid::default(),
            Primitive::Num(serde_json::Number::from(n)),
        ))),
        "[a-zA-Z0-9 _-]{0,20}".prop_map(|s| RcExpr::from(Expr::Literal(
            eucalypt::common::sourcemap::Smid::default(),
            Primitive::Str(s),
        ))),
        "[a-z][a-z0-9-]{0,8}".prop_map(|s| RcExpr::from(Expr::Literal(
            eucalypt::common::sourcemap::Smid::default(),
            Primitive::Sym(s),
        ))),
        proptest::bool::ANY.prop_map(|b| RcExpr::from(Expr::Literal(
            eucalypt::common::sourcemap::Smid::default(),
            Primitive::Bool(b),
        ))),
        Just(RcExpr::from(Expr::Literal(
            eucalypt::common::sourcemap::Smid::default(),
            Primitive::Null,
        ))),
    ]
    .boxed()
}

/// Generate a valid eucalypt identifier (for use as a block key).
fn arb_key() -> BoxedStrategy<String> {
    "[a-z][a-z0-9-]{0,6}".prop_map(|s| s).boxed()
}

/// Generate an arbitrary `RcExpr` up to the given depth.
///
/// Depth-limited to prevent exponential blow-up.  At depth 0, only literals
/// are generated.
fn arb_rc_expr_at(depth: u32) -> BoxedStrategy<RcExpr> {
    if depth == 0 {
        return arb_rc_literal();
    }

    let d = depth - 1;
    prop_oneof![
        // Leaves are heavily weighted at every level.
        4 => arb_rc_literal(),
        // List of sub-expressions.
        1 => pvec(arb_rc_expr_at(d), 0..=4).prop_map(|items| RcExpr::from(Expr::List(
            eucalypt::common::sourcemap::Smid::default(),
            items,
        ))),
        // Block with generated keys and sub-expression values.
        1 => pvec((arb_key(), arb_rc_expr_at(d)), 0..=4).prop_map(|entries| {
            let block_map: BlockMap<RcExpr> = entries.into_iter().collect();
            RcExpr::from(Expr::Block(
                eucalypt::common::sourcemap::Smid::default(),
                block_map,
            ))
        }),
    ]
    .boxed()
}

/// Generate arbitrary `RcExpr` values up to depth 3.
fn arb_rc_expr() -> BoxedStrategy<RcExpr> {
    arb_rc_expr_at(3)
}

proptest! {
    #![proptest_config(config())]

    /// Rendering an `RcExpr` is deterministic: two calls produce identical output.
    #[test]
    fn prop_rc_expr_render_determinism(expr in arb_rc_expr()) {
        let first = render_rc_expr(&expr);
        let second = render_rc_expr(&expr);
        prop_assert_eq!(&first, &second,
            "render_rc_expr is not deterministic for {:?}", expr);
    }

    /// Round-trip: an `RcExpr` rendered to eucalypt source, then formatted,
    /// produces output identical to formatting it a second time
    /// (`format(render(e)) == format(format(render(e)))`).
    ///
    /// This verifies that the rendered source is parseable by the formatter
    /// and that the formatter is idempotent on it.
    #[test]
    fn prop_rc_expr_render_parse_round_trip(expr in arb_rc_expr()) {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static TOTAL: AtomicUsize = AtomicUsize::new(0);
        static SKIPPED: AtomicUsize = AtomicUsize::new(0);

        let total = TOTAL.fetch_add(1, Ordering::Relaxed) + 1;
        let source = render_rc_expr(&expr);
        let cfg = FormatterConfig::new(80, 2, false);

        let first = match format_source(&source, &cfg) {
            Ok(s) => s,
            Err(_) => {
                let skipped = SKIPPED.fetch_add(1, Ordering::Relaxed) + 1;
                // Fail if more than 50% of inputs are skipped — the
                // generator is not producing enough parseable output.
                if total >= 20 && skipped * 2 > total {
                    return Err(TestCaseError::fail(format!(
                        "too many skipped inputs: {skipped}/{total} ({:.0}%)",
                        skipped as f64 / total as f64 * 100.0
                    )));
                }
                return Ok(());
            }
        };

        let second = match format_source(&first, &cfg) {
            Ok(s) => s,
            Err(e) => {
                return Err(TestCaseError::fail(format!(
                    "second format_source failed: {e}\nsource: {source:?}\nfirst: {first:?}"
                )));
            }
        };

        prop_assert_eq!(
            first, second,
            "render-parse round-trip not idempotent for rendered source: {:?}",
            source
        );
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

        /// Programs with function application and operators evaluate without
        /// GC violations.  These exercise closures, application nodes, and
        /// continuation frames — the constructs most likely to trigger GC bugs.
        #[test]
        fn prop_gc_no_violations_application(source in arb_application_source()) {
            let _ = eval_source(&source);
        }
    }

    /// Generate source programs that exercise function application,
    /// operator evaluation, and map/filter — stressing the GC with
    /// closure allocation and continuation frames.
    fn arb_application_source() -> BoxedStrategy<String> {
        prop_oneof![
            // Infix operators: allocate continuations
            (arb_number_source(), arb_number_source()).prop_map(|(a, b)| format!("{a} + {b}")),
            // map over a list: allocate closures
            pvec(arb_number_source(), 1..=8)
                .prop_map(|ns| format!("[{}] map((+ 1))", ns.join(", "))),
            // filter: closures + conditional evaluation
            pvec(arb_number_source(), 1..=8)
                .prop_map(|ns| format!("[{}] filter((> 0))", ns.join(", "))),
            // Nested function application via let-binding
            (arb_number_source(), arb_number_source())
                .prop_map(|(a, b)| format!("f(x): x + 1\nresult: f({a}) + f({b})")),
            // Recursive-style: foldl over a list
            pvec(arb_number_source(), 1..=6)
                .prop_map(|ns| format!("[{}] foldl((+), 0)", ns.join(", "))),
        ]
        .boxed()
    }
}
