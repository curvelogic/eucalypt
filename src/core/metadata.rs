//! Facilities for dealing with metadata in core expressions at
//! various phases
use crate::common::sourcemap::*;
use crate::core::binding::Var;
use crate::core::error::*;
use crate::core::expr::*;
use crate::syntax::input::*;

/// Deprecation specification for a declaration.
///
/// Collected during desugaring and used during binding verification to
/// emit warnings when a deprecated declaration is referenced.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DeprecationSpec {
    /// Optional human-readable deprecation message.
    pub message: Option<String>,
    /// Optional name of the replacement declaration.
    pub replacement: Option<String>,
}

/// Read typed metadata out of core expressions, mutating to persist
/// any evaluations or transformations made along the way.
pub trait ReadMetadata<M> {
    fn read_metadata(&mut self) -> Result<M, CoreError>;
}

/// Specifies the tracing behaviour for a declaration.
///
/// Set via backtick metadata: `` ` :trace `` (shorthand for lazy) or
/// `` ` { trace: :strict } `` etc.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TraceSpec {
    /// Trace entry only; argument values are not forced before logging.
    Lazy,
    /// Trace entry only; argument values are forced (evaluated to WHNF) before logging.
    Strict,
    /// Trace both entry and exit; argument values are not forced.
    Exit,
    /// Trace both entry and exit; argument values are forced before logging.
    StrictExit,
}

/// Extract a `TraceSpec` from a core expression.
///
/// Accepts the symbol variants `:lazy`, `:strict`, `:exit`, `:strict-exit`.
fn extract_trace_spec(expr: &RcExpr) -> Option<TraceSpec> {
    let s: Option<String> = (expr as &dyn Extract<String>).extract();
    s.and_then(|s| match s.as_str() {
        "lazy" => Some(TraceSpec::Lazy),
        "strict" => Some(TraceSpec::Strict),
        "exit" => Some(TraceSpec::Exit),
        "strict-exit" => Some(TraceSpec::StrictExit),
        _ => None,
    })
}

/// Diagnostic blame classification for a declaration (eu-1tkk.7.11).
///
/// Set via backtick metadata: `` ` :transparent `` or `` ` :boundary ``.
/// Consumed by the Phase 2 curated-trace classifier (not by codegen): a
/// `Transparent` combinator's own frames are dropped from a curated trace
/// (blame passes through to its caller); a `Boundary` combinator is kept as
/// a named secondary while the primary location is reassigned to the
/// nearest user call-site. See design spec §4.3.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlameSpec {
    /// Internal plumbing (e.g. `map`, `foldl`'s own recursion): if its
    /// function argument faults, blame passes through to the caller.
    Transparent,
    /// The combinator the user actually named (e.g. `nth`, `head`): an
    /// error here is the combinator's own contract, named explicitly.
    Boundary,
}

/// Extract a `BlameSpec` from a core expression.
///
/// Accepts the symbol variants `:transparent`, `:boundary`. Any other
/// value (or absent metadata) extracts to `None` — never silently defaults
/// to a classification, so an unrecognised value is caught rather than
/// misclassified.
fn extract_blame_spec(expr: &RcExpr) -> Option<BlameSpec> {
    let s: Option<String> = (expr as &dyn Extract<String>).extract();
    s.and_then(|s| match s.as_str() {
        "transparent" => Some(BlameSpec::Transparent),
        "boundary" => Some(BlameSpec::Boundary),
        _ => None,
    })
}

/// Extract a `DeprecationSpec` from a metadata block map.
///
/// Recognises the following patterns:
/// - `{ deprecated: true }` — bare deprecation with no message
/// - `{ deprecated: "message" }` — deprecation with an explanation
/// - `{ replaced-by: "new-fn" }` — pointer to replacement (implies deprecated)
///
/// Returns `None` if neither `deprecated` nor `replaced-by` keys are present.
fn extract_deprecation_spec(imap: &BlockMap<RcExpr>) -> Option<DeprecationSpec> {
    let deprecated_val = imap.get("deprecated");
    let replaced_by: Option<String> = imap.get("replaced-by").and_then(|e| e.extract());

    match deprecated_val {
        Some(e) => {
            // `deprecated: "message"` — string value is the message
            // `deprecated: true` — bool true means no message
            let message: Option<String> = (e as &dyn Extract<String>).extract();
            Some(DeprecationSpec {
                message,
                replacement: replaced_by,
            })
        }
        None if replaced_by.is_some() => {
            // `replaced-by:` alone implies deprecated
            Some(DeprecationSpec {
                message: None,
                replacement: replaced_by,
            })
        }
        None => None,
    }
}

/// Extract a function name from a metadata value.
///
/// Accepts:
/// - String literal `"name"` → `"name"`
/// - Symbol literal `:name` → `"name"`
/// - Var expression (desugared identifier) → the name
fn extract_function_name(expr: &RcExpr) -> Option<String> {
    // Try string or symbol literal first
    if let Some(s) = (expr as &dyn Extract<String>).extract() {
        return Some(s);
    }
    // Then try Var (desugared identifier reference)
    if let Expr::Var(_, Var::Free(name)) = &*expr.inner {
        return Some(name.clone());
    }
    None
}

/// Support shortcuts for metadata symbols.
///
/// Strings are values for `:doc`.  `:suppress` is an export specifier.
/// `:main` is a target.  `:target` uses the declaration's own name as
/// the target.  Any other unrecognised symbol becomes a target shortcut.
pub fn normalise_metadata(expr: &RcExpr, decl_name: Option<&str>) -> RcExpr {
    match &*expr.inner {
        Expr::Literal(smid, prim) => match prim {
            Primitive::Str(_) => {
                core::block(*smid, [("doc".to_string(), expr.clone())].iter().cloned())
            }
            Primitive::Sym(s) => match s.as_ref() {
                "suppress" | "internal" => core::block(
                    *smid,
                    [("export".to_string(), expr.clone())].iter().cloned(),
                ),
                "trace" => core::block(
                    *smid,
                    [("trace".to_string(), core::sym(*smid, "lazy"))]
                        .iter()
                        .cloned(),
                ),
                "deprecated" => core::block(
                    *smid,
                    [("deprecated".to_string(), core::bool_(*smid, true))]
                        .iter()
                        .cloned(),
                ),
                // Blame annotation shorthand (eu-1tkk.7.11): `:transparent`
                // / `:boundary` ≡ `{ blame: :transparent }` / `{ blame: :boundary }`.
                "transparent" => core::block(
                    *smid,
                    [("blame".to_string(), core::sym(*smid, "transparent"))]
                        .iter()
                        .cloned(),
                ),
                "boundary" => core::block(
                    *smid,
                    [("blame".to_string(), core::sym(*smid, "boundary"))]
                        .iter()
                        .cloned(),
                ),
                // Lone-keyword shorthand: `:type-def` ≡ `{ type-def: true }`,
                // `:result-def` ≡ `{ result-def: true }`.
                "type-def" => core::block(
                    *smid,
                    [("type-def".to_string(), core::bool_(*smid, true))]
                        .iter()
                        .cloned(),
                ),
                "result-def" => core::block(
                    *smid,
                    [("result-def".to_string(), core::bool_(*smid, true))]
                        .iter()
                        .cloned(),
                ),
                "target" => {
                    if let Some(name) = decl_name {
                        let target_sym = core::sym(*smid, name);
                        core::block(*smid, [("target".to_string(), target_sym)].iter().cloned())
                    } else {
                        // Unit-level :target — no declaration name, pass through
                        expr.clone()
                    }
                }
                _ if decl_name.is_some() => {
                    // Unrecognised symbol on a declaration → target shortcut
                    core::block(
                        *smid,
                        [("target".to_string(), expr.clone())].iter().cloned(),
                    )
                }
                _ => expr.clone(),
            },
            _ => expr.clone(),
        },
        _ => expr.clone(),
    }
}

pub fn strip_desugar_phase_metadata(expr: &RcExpr) -> RcExpr {
    match &*expr.inner {
        Expr::Block(s, entries) => {
            let imap: BlockMap<RcExpr> = entries
                .iter()
                .filter(|(k, _)| {
                    !matches!(
                        k.as_str(),
                        "fixity"
                            | "precedence"
                            | "target"
                            | "format"
                            | "import"
                            | "embedding"
                            | "parse-embed"
                            | "trace"
                            | "prelude"
                            | "requires"
                            | "deprecated"
                            | "replaced-by"
                            | "blame"
                    )
                })
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();

            if imap.is_empty() {
                RcExpr::from(Expr::ErrEliminated)
            } else {
                RcExpr::from(Expr::Block(*s, imap))
            }
        }
        _ => expr.clone(),
    }
}

/// The items that can be read from metadata during desugaring into
/// Core syntax.
///
/// All values for these keys must be simple literals.
#[derive(Default, Debug)]
pub struct DesugarPhaseDeclarationMetadata {
    /// Operator fixity
    pub fixity: Option<Fixity>,
    /// Precedence
    pub precedence: Option<i32>,
    /// Target name
    pub target: Option<String>,
    /// Target format
    pub format: Option<String>,
    /// Target validations
    pub validations: Option<Vec<String>>,
    /// Import specification
    pub imports: Option<Vec<Input>>,
    /// Documentation
    pub doc: Option<String>,
    /// Embedding - describes how / what is embedded (e.g. core quote-embed)
    pub embedding: Option<String>,
    /// Trace specification — controls entry/exit tracing of this declaration.
    pub trace: Option<TraceSpec>,
    /// Deprecation specification — marks this declaration as deprecated.
    pub deprecated: Option<DeprecationSpec>,
    /// Diagnostic blame classification (eu-1tkk.7.11) — consumed at trace
    /// classification time (Phase 2 curation), not by codegen. See
    /// `BlameSpec`.
    pub blame: Option<BlameSpec>,
}

/// Public wrapper for extract_function_name for use in other modules.
pub fn extract_function_name_from_expr(expr: &RcExpr) -> Option<String> {
    extract_function_name(expr)
}
impl ReadMetadata<DesugarPhaseDeclarationMetadata> for RcExpr {
    /// Read desugar phase metadata. At this point we cannot evaluate
    /// anything, but we can expand out top level lets to inline
    /// definitions back into the block structure we intend to scrape
    /// for the values.
    fn read_metadata(&mut self) -> Result<DesugarPhaseDeclarationMetadata, CoreError> {
        match &*self.inner {
            Expr::Block(_, imap) => Ok(DesugarPhaseDeclarationMetadata {
                fixity: imap.get("associates").and_then(|e| e.extract()),
                precedence: imap.get("precedence").and_then(extract_precedence),
                target: imap.get("target").and_then(|e| e.extract()),
                format: imap.get("format").and_then(|e| e.extract()),
                validations: imap.get("verify").and_then(|e| e.extract()),
                imports: imap.get("import").and_then(|e| e.extract()),
                doc: imap.get("doc").and_then(|e| e.extract()),
                embedding: imap.get("embedding").and_then(|e| e.extract()),
                trace: imap.get("trace").and_then(extract_trace_spec),
                deprecated: extract_deprecation_spec(imap),
                blame: imap.get("blame").and_then(extract_blame_spec),
            }),
            Expr::Let(_, _, _) => {
                self.inner = self.clone().instantiate_lets().inner;
                self.read_metadata()
                    .or_else(|_| Ok(DesugarPhaseDeclarationMetadata::default()))
            }
            _ => Ok(DesugarPhaseDeclarationMetadata::default()),
        }
    }
}

/// The items that can be read from block metadata during desugaring.
///
/// All values for these keys must be simple literals.
#[derive(Default, Debug)]
pub struct DesugarPhaseBlockMetadata {
    /// Import specification
    pub imports: Option<Vec<Input>>,
    /// Documentation
    pub doc: Option<String>,
    /// Parse-embed
    pub parse_embed: Option<String>,
    /// Prelude selection: symbol `:name` → resource name, string → file path
    pub prelude: Option<String>,
    /// Version constraint: semver range string (e.g. ">=0.8")
    pub requires: Option<String>,
}

impl ReadMetadata<DesugarPhaseBlockMetadata> for RcExpr {
    /// Read desugar phase metadata. At this point we cannot evaluate
    /// anything, but we can expand out top level lets to inline
    /// definitions back into the block structure we intend to scrape
    /// for the values.
    fn read_metadata(&mut self) -> Result<DesugarPhaseBlockMetadata, CoreError> {
        match &*self.inner {
            Expr::Block(_, imap) => Ok(DesugarPhaseBlockMetadata {
                imports: imap.get("import").and_then(|e| e.extract()),
                doc: imap.get("doc").and_then(|e| e.extract()),
                parse_embed: imap.get("parse-embed").and_then(|e| e.extract()),
                prelude: imap.get("prelude").and_then(|e| e.extract()),
                requires: imap.get("requires").and_then(|e| e.extract()),
            }),
            Expr::Let(_, _, _) => {
                self.inner = self.clone().instantiate_lets().inner;
                self.read_metadata()
                    .map_err(|_| CoreError::InvalidMetadataDesugarPhase(self.smid()))
            }
            _ => Err(CoreError::InvalidMetadataDesugarPhase(self.smid())),
        }
    }
}

/// Extract an operator precedence which can be numeric or symbolic
fn extract_precedence(expr: &RcExpr) -> Option<i32> {
    expr.extract()
        .or_else(|| expr.extract().and_then(named_precedence))
}

/// Resolve a named precedence to its precedence value
fn named_precedence(name: String) -> Option<i32> {
    match name.as_str() {
        "lookup" => Some(90),
        "call" => Some(90),
        "bool-unary" => Some(88),
        "exp" => Some(85),
        "prod" => Some(80),
        "sum" => Some(75),
        "shift" => Some(60),
        "bitwise" => Some(55),
        "cmp" => Some(50),
        "append" => Some(45),
        "map" => Some(42),
        "eq" => Some(40),
        "bool-prod" => Some(35),
        "bool-sum" => Some(30),
        "cat" => Some(20),
        "apply" => Some(10),
        "meta" => Some(5),
        _ => None,
    }
}

/// Test file header metadata
#[derive(Debug, Default)]
pub struct TestHeaderMetadata {
    /// Title for the test
    pub title: Option<String>,
    /// Formats to output for each test target
    pub formats: Vec<String>,
    /// Shell process to run prior to test expection
    pub shell: Option<String>,
    /// Whether this test requires `--allow-io` to run.
    ///
    /// When `true` and `--allow-io` is not set, `eu test` skips the
    /// test rather than failing.
    pub requires_io: bool,
}

impl ReadMetadata<TestHeaderMetadata> for RcExpr {
    /// Read desugar phase metadata. At this point we cannot evaluate
    /// anything, but we can expand out top level lets to inline
    /// definitions back into the block structure we intend to scrape
    /// for the values.
    fn read_metadata(&mut self) -> Result<TestHeaderMetadata, CoreError> {
        match &*self.inner {
            Expr::Block(_, imap) => Ok(TestHeaderMetadata {
                title: imap.get("title").and_then(|e| e.extract()),
                formats: imap
                    .get("test-formats")
                    .and_then(|e| e.extract())
                    .unwrap_or_default(),
                shell: imap.get("test-shell").and_then(|e| e.extract()),
                requires_io: imap
                    .get("requires-io")
                    .and_then(|e| e.extract())
                    .unwrap_or(false),
            }),
            Expr::Let(_, _, _) => {
                self.inner = self.clone().instantiate_lets().inner;
                self.read_metadata()
                    .map_err(|_| CoreError::InvalidMetadataDesugarPhase(self.smid()))
            }
            _ => Err(CoreError::InvalidMetadataDesugarPhase(self.smid())),
        }
    }
}

#[cfg(test)]
mod blame_spec_tests {
    //! eu-1tkk.7.11 spike: prove `BlameSpec` is declarable (via bare
    //! backtick metadata symbols, through `normalise_metadata`) and
    //! readable (via `extract_blame_spec`, feeding
    //! `DesugarPhaseDeclarationMetadata.blame`) on the source-compiled
    //! prelude path — mirrors the shape of `TraceSpec`/`extract_trace_spec`.

    use super::*;
    use crate::core::expr::acore;

    #[test]
    fn extracts_transparent() {
        let expr = acore::sym("transparent");
        assert_eq!(extract_blame_spec(&expr), Some(BlameSpec::Transparent));
    }

    #[test]
    fn extracts_boundary() {
        let expr = acore::sym("boundary");
        assert_eq!(extract_blame_spec(&expr), Some(BlameSpec::Boundary));
    }

    #[test]
    fn unrecognised_symbol_extracts_to_none() {
        let expr = acore::sym("bogus");
        assert_eq!(extract_blame_spec(&expr), None);
    }

    #[test]
    fn non_symbol_extracts_to_none() {
        let expr = acore::num(42);
        assert_eq!(extract_blame_spec(&expr), None);
    }

    /// Fault-injection check for the extraction function itself (mirrors
    /// the plan's Step 6 fallback): if `extract_blame_spec` regresses to
    /// always returning `None`, this test must fail.
    #[test]
    fn fault_injection_extract_blame_spec_must_discriminate() {
        let transparent = extract_blame_spec(&acore::sym("transparent"));
        let boundary = extract_blame_spec(&acore::sym("boundary"));
        assert_ne!(
            transparent, boundary,
            "extract_blame_spec must discriminate :transparent from :boundary"
        );
        assert!(transparent.is_some() && boundary.is_some());
    }

    #[test]
    fn bare_symbol_transparent_normalises_to_blame_block() {
        let smid = Smid::default();
        let expr = core::sym(smid, "transparent");
        let normalised = normalise_metadata(&expr, Some("map"));
        match &*normalised.inner {
            Expr::Block(_, imap) => {
                let blame_expr = imap.get("blame").expect("blame key present");
                assert_eq!(extract_blame_spec(blame_expr), Some(BlameSpec::Transparent));
            }
            other => panic!("expected a block, got {other:?}"),
        }
    }

    #[test]
    fn bare_symbol_boundary_normalises_to_blame_block() {
        let smid = Smid::default();
        let expr = core::sym(smid, "boundary");
        let normalised = normalise_metadata(&expr, Some("nth"));
        match &*normalised.inner {
            Expr::Block(_, imap) => {
                let blame_expr = imap.get("blame").expect("blame key present");
                assert_eq!(extract_blame_spec(blame_expr), Some(BlameSpec::Boundary));
            }
            other => panic!("expected a block, got {other:?}"),
        }
    }

    /// End-to-end: declaration metadata read via `ReadMetadata` (the same
    /// call sites `rowan_ast.rs` uses at desugar time) picks up `blame`
    /// from a normalised bare-symbol block, for both vocabulary items.
    #[test]
    fn declaration_metadata_reads_blame_transparent() {
        let smid = Smid::default();
        let mut block = normalise_metadata(&core::sym(smid, "transparent"), Some("map"));
        let metadata: DesugarPhaseDeclarationMetadata =
            block.read_metadata().expect("read_metadata succeeds");
        assert_eq!(metadata.blame, Some(BlameSpec::Transparent));
    }

    #[test]
    fn declaration_metadata_reads_blame_boundary() {
        let smid = Smid::default();
        let mut block = normalise_metadata(&core::sym(smid, "boundary"), Some("nth"));
        let metadata: DesugarPhaseDeclarationMetadata =
            block.read_metadata().expect("read_metadata succeeds");
        assert_eq!(metadata.blame, Some(BlameSpec::Boundary));
    }

    /// `strip_desugar_phase_metadata` must remove `blame` before metadata
    /// reaches runtime (mirrors `trace`/`deprecated`) — otherwise it would
    /// leak into e.g. type-check or doc metadata processing.
    #[test]
    fn strip_desugar_phase_metadata_removes_blame() {
        let smid = Smid::default();
        let normalised = normalise_metadata(&core::sym(smid, "boundary"), Some("nth"));
        let stripped = strip_desugar_phase_metadata(&normalised);
        assert!(matches!(&*stripped.inner, Expr::ErrEliminated));
    }
}
