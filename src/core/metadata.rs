//! Facilities for dealing with metadata in core expressions at
//! various phases
use crate::common::sourcemap::*;
use crate::core::error::*;
use crate::core::expr::*;
use crate::syntax::input::*;

/// Read typed metadata out of core expressions, mutating to persist
/// any evaluations or transformations made along the way.
pub trait ReadMetadata<M> {
    fn read_metadata(&mut self) -> Result<M, CoreError>;
}

/// Support a few shortcuts for metadata
///
/// Strings are values for :doc. :main is a target. :suppress is an
/// export specifier.
pub fn normalise_metadata(expr: &RcExpr) -> RcExpr {
    match &*expr.inner {
        Expr::Literal(smid, prim) => match prim {
            Primitive::Str(_) => {
                core::block(*smid, [("doc".to_string(), expr.clone())].iter().cloned())
            }
            Primitive::Sym(s) => match s.as_ref() {
                "suppress" => core::block(
                    *smid,
                    [("export".to_string(), expr.clone())].iter().cloned(),
                ),
                "main" => core::block(
                    *smid,
                    [("target".to_string(), expr.clone())].iter().cloned(),
                ),
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
                            | "doc"
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
    /// Import specification
    pub imports: Option<Vec<Input>>,
    /// Documentation
    pub doc: Option<String>,
    /// Embedding - describes how / what is embedded (e.g. core quote-embed)
    pub embedding: Option<String>,
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
                imports: imap.get("import").and_then(|e| e.extract()),
                doc: imap.get("doc").and_then(|e| e.extract()),
                embedding: imap.get("embedding").and_then(|e| e.extract()),
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
    /// Header level documentation
    pub doc: Option<String>,
    /// Formats to output for each test target
    pub formats: Vec<String>,
    /// Shell process to run prior to test expection
    pub shell: Option<String>,
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
                doc: imap.get("doc").and_then(|e| e.extract()),
                formats: imap
                    .get("test-formats")
                    .and_then(|e| e.extract())
                    .unwrap_or_default(),
                shell: imap.get("test-shell").and_then(|e| e.extract()),
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
