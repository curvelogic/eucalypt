//! Desugarable implementations for AST elements

use super::{
    desugarable::Desugarable, desugarer::Desugarer, disembed::core_from_embedding,
    literal::desugar_literal,
};
use crate::{
    common::sourcemap::HasSmid,
    core::{
        anaphora::{self, BLOCK_ANAPHORA, EXPR_ANAPHORA},
        error::CoreError,
        expr::*,
        metadata::*,
        rt,
        transform::{dynamise, succ},
    },
    syntax::{ast::*, span::HasSpan},
};
use codespan::Span;
use moniker::{Binder, Embed, Rec, Scope};

impl Desugarable for RcExpr {
    /// Core expressions are trivially desugarable as themselves
    fn desugar(&self, _: &mut Desugarer) -> Result<RcExpr, CoreError> {
        Ok(self.clone())
    }
}

/// Literals desugar into core Primitives
impl Desugarable for Literal {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        Ok(desugar_literal(desugarer, self))
    }
}

/// If operator fixity can be determined without consulting metadata,
/// return it.
fn static_fixity(decl: &Declaration) -> Option<Fixity> {
    match *decl {
        Declaration::PrefixOperatorDeclaration(_, _, _, _, _) => Some(Fixity::UnaryPrefix),
        Declaration::PostfixOperatorDeclaration(_, _, _, _, _) => Some(Fixity::UnaryPostfix),
        _ => None,
    }
}

/// Convert a declaration to a let binding
fn declaration_to_binding(
    decl: &Declaration,
    desugarer: &mut Desugarer,
) -> Result<(Binder<String>, Embed<RcExpr>), CoreError> {
    desugarer.push(decl.name().name());

    let is_op = decl.is_operator();
    let unary_fixity = static_fixity(decl);
    let components = DeclarationComponents::from(decl);

    // Read metadata up front as it might affect translation of the
    // expression
    let (core_meta, metadata) = match components.metadata {
        Some(m) => {
            let mut core_meta = normalise_metadata(&m.desugar(desugarer)?);
            let metadata = core_meta.read_metadata()?;
            (Some(core_meta), metadata)
        }
        None => {
            let metadata = DesugarPhaseDeclarationMetadata::default();
            (None, metadata)
        }
    };

    if let Some(doc) = &metadata.doc {
        if desugarer.in_base_file() {
            desugarer.record_doc(doc.to_string(), &components);
        }
    }

    if let Some(target) = &metadata.target {
        desugarer.record_target(
            target.to_string(),
            metadata.doc.unwrap_or_default(),
            metadata.format,
            metadata.validations.unwrap_or_default(),
        );
    }

    // If there is an import, we need to switch context to desugar the
    // import first
    let mut imports = Vec::new();
    if let Some(inputs) = metadata.imports {
        for input in inputs {
            let import_smid = desugarer.new_smid(components.span);
            imports.push(
                desugarer
                    .translate_import(import_smid, input)
                    .expect("failure translating import"),
            );
        }
    }

    // if we have a lambda, there are more variables we need to have
    // in scope:
    desugarer
        .env_mut()
        .push_keys(components.args.iter().map(|n| n.name().to_string()));

    let mut expr = if let Some(embedding) = &metadata.embedding {
        // watch out for core syntax embedded as AST
        if embedding == "core" {
            core_from_embedding(desugarer, components.body)?
        } else {
            return Err(CoreError::UnknownEmbedding(embedding.clone()));
        }
    } else {
        // straightforward desugaring
        let mut expr = components.body.desugar(desugarer)?;
        expr = desugarer.varify(expr);
        expr
    };

    if !components.args.is_empty() {
        expr = core::lam(
            desugarer.new_annotated_smid(components.span, components.name.name()),
            components
                .args
                .into_iter()
                .map(|n| desugarer.env().get(&n.name().to_string()).unwrap().clone())
                .collect(),
            expr.clone(),
        );
    }

    if is_op {
        expr = RcExpr::from(Expr::Operator(
            desugarer.new_smid(components.span),
            unary_fixity
                .or(metadata.fixity)
                .unwrap_or(Fixity::InfixLeft),
            metadata.precedence.unwrap_or(50),
            expr.clone(),
        ));
    }

    if let Some(m) = core_meta {
        let stripped_meta = strip_desugar_phase_metadata(&m);
        if !matches!(&*stripped_meta.inner, Expr::ErrEliminated) {
            expr = RcExpr::from(Expr::Meta(
                desugarer.new_smid(decl.span()),
                expr,
                stripped_meta,
            ));
        }
    }

    let declared_var = desugarer
        .env()
        .get(&components.name.name().to_string())
        .expect("declaration var should have been prepared in block");

    // embed the expression in our imports if required
    expr = imports.iter().rfold(expr, |acc, import| import.rebody(acc));

    let ret = (Binder(declared_var.clone()), Embed(expr));

    desugarer.env_mut().pop();
    desugarer.pop();
    Ok(ret)
}

/// Blocks desugar into Core lets with block bodies
impl Desugarable for Block {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        // Transform metadata for attachment later
        let mut metadata = match self.metadata.clone() {
            Some(e) => Some(normalise_metadata(&e.desugar(desugarer)?)),
            None => None,
        };

        // Block (or unit) metadata may have imports - extract
        let block_meta = match metadata {
            Some(ref mut concrete_meta) => concrete_meta.read_metadata().unwrap_or_default(),
            None => DesugarPhaseBlockMetadata::default(),
        };

        // If there is an import, we need to switch context to desugar the
        // import first
        let mut imports = Vec::new();
        if let Some(inputs) = block_meta.imports {
            let meta_smid = metadata.as_ref().unwrap().smid();
            for input in inputs {
                imports.push(
                    desugarer
                        .translate_import(meta_smid, input)
                        .expect("failure translating import"),
                );
            }
        }

        // provide FreeVars that can be reused the names brought into scope
        let keys: Vec<_> = self
            .declarations
            .iter()
            .map(|decl| decl.name().name().to_string())
            .collect();
        desugarer.env_mut().push_keys(keys);

        // convert the declarations into bindings
        let bindings = self
            .declarations
            .iter()
            .map(|decl| declaration_to_binding(decl, desugarer))
            .collect::<Result<Vec<(Binder<String>, Embed<RcExpr>)>, CoreError>>()?;

        // using the binders to create a let body that creates a
        // simple block
        let body_elements: BlockMap<RcExpr> = bindings
            .iter()
            .map(|(Binder(v), Embed(e))| {
                (
                    v.clone().pretty_name.unwrap(),
                    core::var(e.smid(), v.clone()),
                )
            })
            .collect();

        // if `parse-embed: :k` simply use k as the body
        let body = if let Some(embed_key) = block_meta.parse_embed {
            match desugarer.env().get(&embed_key) {
                Some(fv) => acore::var(fv.clone()),
                None => {
                    let meta_smid = metadata.as_ref().map(|m| m.smid()).unwrap_or_default();
                    RcExpr::from(Expr::ErrUnresolved(meta_smid, embed_key))
                }
            }
        } else {
            RcExpr::from(Expr::Block(desugarer.new_smid(self.span), body_elements))
        };

        let mut expr = RcExpr::from(Expr::Let(
            desugarer.new_smid(self.span),
            Scope::new(Rec::new(bindings), body),
            LetType::DefaultBlockLet,
        ));

        // Tack on metadata if required
        if let Some(m) = metadata {
            let stripped_meta = strip_desugar_phase_metadata(&m);
            if !matches!(&*stripped_meta.inner, Expr::ErrEliminated) {
                expr = RcExpr::from(Expr::Meta(
                    desugarer.new_smid(self.span),
                    expr,
                    stripped_meta,
                ));
            }
        }

        // embed the expression in our imports if required
        expr = imports.iter().rfold(expr, |acc, import| import.rebody(acc));

        desugarer.env_mut().pop();
        Ok(expr)
    }
}

/// Translate special operator names (".") to operators but all other
/// names to Expr::Name for further analysis
fn desugar_name(name: &Name, desugarer: &mut Desugarer) -> RcExpr {
    match name {
        Name::Operator(s, n) => {
            if n == "." {
                RcExpr::from(ops::dot())
            } else {
                RcExpr::from(Expr::Name(desugarer.new_smid(*s), n.to_string()))
            }
        }
        Name::Normal(s, n) => {
            if n.starts_with("__") && n.chars().nth(2).is_some_and(|c| c.is_uppercase()) {
                RcExpr::from(Expr::Intrinsic(desugarer.new_smid(*s), n[2..].to_string()))
            } else if BLOCK_ANAPHORA.is_anaphor(n) {
                let smid = desugarer.new_smid(*s);
                RcExpr::from(Expr::BlockAnaphor(
                    smid,
                    BLOCK_ANAPHORA.to_explicit_anaphor(smid, n),
                ))
            } else if EXPR_ANAPHORA.is_anaphor(n) {
                let smid = desugarer.new_smid(*s);
                RcExpr::from(Expr::ExprAnaphor(
                    smid,
                    EXPR_ANAPHORA.to_explicit_anaphor(smid, n),
                ))
            } else {
                RcExpr::from(Expr::Name(desugarer.new_smid(*s), n.to_string()))
            }
        }
    }
}

#[derive(Eq, PartialEq)]
pub enum PendingLookup {
    None,
    Static,
    Dynamic,
}

/// Desugar operator soup
///
/// Soup with a single item is simplified into the item.
fn desugar_soup(
    span: Span,
    exprs: &[Expression],
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    let mut soup: Vec<RcExpr> = Vec::with_capacity(exprs.len() * 2);
    let mut lookup = PendingLookup::None;

    for ast_expr in exprs {
        let expr = ast_expr.desugar(desugarer)?;
        let top: Option<RcExpr> = soup.last().cloned();

        match &*expr.inner {
            // Insert explicit call ops
            Expr::ArgTuple(_, _) => {
                soup.push(RcExpr::from(ops::call()));
                soup.push(expr.clone());
            }
            // Initial name in lookup chains must become a var
            Expr::Name(s, ref n) => {
                if lookup == PendingLookup::Static {
                    // simple static lookup
                    soup.pop();
                    if let Some(dlet) = soup.pop() {
                        soup.push(dlet.rebody(core::var(*s, desugarer.var(n))));
                    } else {
                        panic!("Expected default let");
                    }
                    lookup = PendingLookup::None;
                } else if lookup == PendingLookup::Dynamic {
                    // simple dynamic lookup
                    soup.push(expr.clone());
                    lookup = PendingLookup::None;
                } else {
                    soup.push(core::var(*s, desugarer.var(n)))
                }
            }
            Expr::Operator(s, _, _, ref body) => {
                // If we're propagating a dot, we must classify it first
                if body.inner.is_pseudodot() {
                    if soup.is_empty() {
                        soup.push(RcExpr::from(Expr::ExprAnaphor(
                            *s,
                            Anaphor::Implicit(*s, ImplicitAnaphorSide::Left),
                        )));
                        lookup = PendingLookup::Dynamic;
                    } else if top.unwrap().inner.is_default_let() {
                        lookup = PendingLookup::Static;
                    } else {
                        lookup = PendingLookup::Dynamic;
                    }
                }
                soup.push(expr.clone());
            }
            _ => {
                if lookup == PendingLookup::Dynamic {
                    soup.push(dynamise::dynamise(&expr)?);
                    lookup = PendingLookup::None;
                } else if lookup == PendingLookup::Static {
                    soup.pop();
                    if let Some(dlet) = soup.pop() {
                        soup.push(dlet.rebody(expr.clone()));
                    } else {
                        panic!("Expected default let");
                    }
                    lookup = PendingLookup::None;
                } else {
                    soup.push(expr.clone());
                }
            }
        }
    }

    // Optimise away the soup wrapping in cases where it is definitely
    // not needed but leave vars for cooking phase (in case of sections)
    if soup.len() == 1 {
        match &*soup.first().unwrap().inner {
            Expr::Var(_, _) => Ok(RcExpr::from(Expr::Soup(desugarer.new_smid(span), soup))),
            _ => Ok(soup.first().unwrap().clone()),
        }
    } else {
        Ok(RcExpr::from(Expr::Soup(desugarer.new_smid(span), soup)))
    }
}

/// Vectors of expressions desugar as operator soup
impl Desugarable for Vec<Expression> {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        let mut spans = self.iter().map(|x| x.span());
        let span = if let Some(initial) = spans.next() {
            spans.fold(initial, |l, r| l.merge(r))
        } else {
            Span::default()
        };
        desugar_soup(span, self, desugarer)
    }
}

/// String interpolation targets desugar to vars or anaphora
impl Desugarable for InterpolationTarget {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        match self {
            InterpolationTarget::StringAnaphor(span, Some(i)) => {
                let smid = desugarer.new_smid(*span);
                let fv = desugarer.add_pending_string_anaphor(Anaphor::ExplicitNumbered(*i));
                Ok(core::var(smid, fv))
            }
            InterpolationTarget::StringAnaphor(span, None) => {
                let smid = desugarer.new_smid(*span);
                let fv = desugarer.add_pending_string_anaphor(Anaphor::ExplicitAnonymous(smid));
                Ok(core::var(smid, fv))
            }
            InterpolationTarget::Reference(span, xs) => {
                let smid = desugarer.new_smid(*span);
                let mut items = xs.clone();
                if items.len() == 1 {
                    Ok(desugar_name(&items.remove(0), desugarer)).map(|e| desugarer.varify(e))
                } else {
                    let mut v = vec![];
                    while items.len() > 1 {
                        let n = items.pop().unwrap();
                        v.push(desugar_name(&n, desugarer));
                        v.push(acore::dot());
                    }
                    let name = desugar_name(&items.pop().unwrap(), desugarer);
                    v.push(desugarer.varify(name));
                    v.reverse();
                    Ok(core::soup(smid, v))
                }
            }
        }
    }
}

/// Interpolations may use __FMT or apply a fn var
impl Desugarable for InterpolationRequest {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        let mut expr = self.target.desugar(desugarer)?;

        if let Some(ref format) = self.format {
            if !format.is_empty() && format.as_bytes()[0] as char == '%' {
                expr = acore::app(rt::fmt(), vec![expr, acore::str(format)]);
            } else {
                let smid = desugarer.new_smid(self.span);
                expr = acore::app(core::var(smid, desugarer.var(format)), vec![expr]);
            }
        } else {
            expr = acore::app(rt::str(), vec![expr]);
        }

        Ok(expr)
    }
}

/// String chunks desugar as literals or interpolations
impl Desugarable for StringChunk {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        match self {
            StringChunk::Interpolation(_sp, rq) => rq.desugar(desugarer),

            StringChunk::LiteralContent(sp, s) => Ok(RcExpr::from(Expr::Literal(
                desugarer.new_smid(*sp),
                Primitive::Str(s.to_string()),
            ))),
        }
    }
}

/// Desugar a string pattern
///
/// Uses intrinsics "FMT", "STR", "JOIN" to stitch together the
/// various components at run time.
pub fn desugar_string_pattern(
    span: Span,
    chunks: &[StringChunk],
    desugarer: &mut Desugarer,
) -> Result<RcExpr, CoreError> {
    let translated = chunks
        .iter()
        .map(|c| c.desugar(desugarer))
        .collect::<Result<Vec<RcExpr>, CoreError>>()?;

    let smid = desugarer.new_smid(span);
    let mut expr = core::app(
        smid,
        rt::join(),
        vec![core::list(smid, translated), acore::str("")],
    );

    // if there are any anaphora wrap into a lambda
    if desugarer.has_pending_string_anaphora() {
        let binders = anaphora::to_binding_pattern(desugarer.pending_string_anaphora())?;
        desugarer.clear_pending_string_anaphora();
        expr = core::lam(expr.smid(), binders, succ::succ(&expr)?);
    }

    Ok(expr)
}

/// Desugar block into core expression
///
/// In the process, unnumbered block and expression anaphora are
/// numbered.
impl Desugarable for Expression {
    fn desugar(&self, desugarer: &mut Desugarer) -> Result<RcExpr, CoreError> {
        use Expression::*;
        match self {
            Lit(lit) => lit.desugar(desugarer),
            Block(block) => (*block).desugar(desugarer),
            List(sp, exprs) => Ok(RcExpr::from(Expr::List(
                desugarer.new_smid(*sp),
                exprs
                    .iter()
                    .map(|x| x.desugar(desugarer).map(|e| desugarer.varify(e)))
                    .collect::<Result<Vec<RcExpr>, CoreError>>()?, //TODO: multiple errors
            ))),
            OpSoup(sp, exprs) => desugar_soup(*sp, exprs, desugarer),
            Name(n) => Ok(desugar_name(n, desugarer)),
            StringPattern(sp, chunks) => desugar_string_pattern(*sp, chunks, desugarer),
            ApplyTuple(sp, exprs) => Ok(RcExpr::from(Expr::ArgTuple(
                desugarer.new_smid(*sp),
                exprs
                    .iter()
                    .map(|x| x.desugar(desugarer).map(|x| desugarer.varify(x)))
                    .collect::<Result<Vec<RcExpr>, CoreError>>()?, //TODO: multiple errors
            ))),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::{common::sourcemap::Smid, core::expr::core, syntax::input::Locator};
    use crate::{common::sourcemap::SourceMap, core::expr::acore};
    use crate::{core::desugar::desugarable::Content, syntax::ast};
    use crate::{driver::source::SourceLoader, syntax::input::Input};
    use codespan_reporting::files::SimpleFiles;
    use moniker::{assert_term_eq, FreeVar};

    pub struct Fixture {
        source_map: SourceMap,
        files: SimpleFiles<String, String>,
    }

    impl Fixture {
        pub fn desugar(mut self, ast: impl Desugarable) -> RcExpr {
            let file_id = self.files.add("<test>".to_string(), "".into());
            let mut hm: HashMap<Input, Content> = HashMap::new();
            hm.insert(
                Input::from(Locator::Cli("test".to_string())),
                Content::new(file_id, &ast),
            );
            let mut desugarer = Desugarer::new(&hm, &mut self.source_map);
            desugarer.translate_simple(file_id, &ast).unwrap()
        }
    }

    impl Default for Fixture {
        fn default() -> Self {
            let source_map = SourceMap::new();
            let files = SimpleFiles::<String, String>::new();

            Fixture { source_map, files }
        }
    }

    #[test]
    pub fn test_literals() {
        assert_eq!(
            Fixture::default().desugar(ast::str("blah")),
            core::str(Smid::from(1), "blah")
        );
        assert_eq!(
            Fixture::default().desugar(ast::sym("blah")),
            core::sym(Smid::from(1), "blah")
        );
        assert_eq!(
            Fixture::default().desugar(ast::num(900)),
            core::num(Smid::from(1), 900)
        );
    }

    #[test]
    pub fn test_inserts_call_operator() {
        let fixture = Fixture::default();

        let soup = vec![
            ast::lit(ast::num(5)),
            ast::name(ast::normal("x")),
            ast::name(ast::normal("f")),
            ast::tuple(vec![ast::lit(ast::num(4)), ast::lit(ast::num(7))]),
        ];

        let core_soup = core::soup(
            Smid::from(7),
            vec![
                core::num(Smid::from(1), 5),
                core::var(Smid::from(2), free("x")),
                core::var(Smid::from(3), free("f")),
                core::call(),
                core::arg_tuple(
                    Smid::from(4),
                    vec![core::num(Smid::from(5), 4), core::num(Smid::from(6), 7)],
                ),
            ],
        );

        assert_term_eq!(bound(fixture.desugar(soup)), bound(core_soup));
    }

    #[test]
    pub fn test_handles_iterated_calls() {
        let fixture = Fixture::default();

        let soup = vec![
            ast::name(ast::normal("f")),
            ast::tuple(vec![ast::name(ast::normal("x"))]),
            ast::tuple(vec![ast::name(ast::normal("y"))]),
        ];

        let core_soup = acore::soup(vec![
            acore::var(free("f")),
            acore::call(),
            acore::arg_tuple(vec![acore::var(free("x"))]),
            acore::call(),
            acore::arg_tuple(vec![acore::var(free("y"))]),
        ]);

        assert_term_eq!(bound(fixture.desugar(soup)), bound(core_soup));
    }

    #[test]
    pub fn test_handles_relative_names() {
        let fixture = Fixture::default();

        let soup = vec![
            ast::name(ast::normal("x")),
            ast::name(ast::operator(".")),
            ast::name(ast::normal("y")),
            ast::name(ast::operator(".")),
            ast::name(ast::normal("z")),
        ];

        let core_soup = acore::soup(vec![
            acore::var(free("x")),
            acore::dot(),
            acore::name("y"),
            acore::dot(),
            acore::name("z"),
        ]);

        assert_term_eq!(bound(fixture.desugar(soup)), bound(core_soup));
    }

    #[test]
    pub fn test_creates_var_for_lonely_names() {
        let fixture = Fixture::default();

        let block = ast::block(
            None,
            vec![ast::prop(
                None,
                ast::normal("x"),
                ast::name(ast::normal("y")),
            )],
        );

        let core_block = acore::default_let(vec![(free("x"), acore::var(free("y")))]);

        assert_term_eq!(bound(fixture.desugar(block)), bound(core_block));
    }

    #[test]
    pub fn test_handles_built_ins() {
        let fixture = Fixture::default();

        let block = ast::block(
            None,
            vec![
                ast::prop(None, ast::normal("null"), ast::name(ast::normal("__NULL"))),
                ast::prop(None, ast::normal("a"), ast::name(ast::normal("null"))),
            ],
        );

        let null = free("null");
        let a = free("a");

        let core_block = acore::default_let(vec![
            (null.clone(), acore::bif("NULL")),
            (a, acore::var(null)),
        ]);

        assert_term_eq!(bound(fixture.desugar(block)), bound(core_block));
    }

    #[test]
    pub fn test_sample_1() {
        let mut loader = SourceLoader::default();
        let loc = Locator::Cli("eq(or(f, and(t, t)), t)".to_string());
        loader.load(&Input::from(loc.clone())).unwrap();
        let unit = loader.translate(&Input::from(loc)).unwrap();

        // free vars
        let f = free("f");
        let eq = free("eq");
        let and = free("and");
        let or = free("or");
        let t = free("t");

        let core_expr = acore::soup(vec![
            acore::var(eq),
            acore::call(),
            acore::arg_tuple(vec![
                acore::soup(vec![
                    acore::var(or),
                    acore::call(),
                    acore::arg_tuple(vec![
                        acore::var(f),
                        acore::soup(vec![
                            acore::var(and),
                            acore::call(),
                            acore::arg_tuple(vec![acore::var(t.clone()), acore::var(t.clone())]),
                        ]),
                    ]),
                ]),
                acore::var(t),
            ]),
        ]);

        assert_term_eq!(bound(unit.expr.clone()), bound(core_expr));
    }

    #[test]
    pub fn test_sample_2() {
        let mut loader = SourceLoader::default();
        let loc = Locator::Cli("__HEAD(__CONS([1, 2, 3] __HEAD, [1, 2, 3] __TAIL))".to_string());
        loader.load(&Input::from(loc.clone())).unwrap();
        let unit = loader.translate(&Input::from(loc)).unwrap();

        let core_expr = acore::soup(vec![
            acore::bif("HEAD"),
            acore::call(),
            acore::arg_tuple(vec![acore::soup(vec![
                acore::bif("CONS"),
                acore::call(),
                acore::arg_tuple(vec![
                    acore::soup(vec![
                        acore::list(vec![acore::num(1), acore::num(2), acore::num(3)]),
                        acore::bif("HEAD"),
                    ]),
                    acore::soup(vec![
                        acore::list(vec![acore::num(1), acore::num(2), acore::num(3)]),
                        acore::bif("TAIL"),
                    ]),
                ]),
            ])]),
        ]);

        assert_term_eq!(bound(unit.expr.clone()), bound(core_expr));
    }

    #[test]
    pub fn test_sample_3() {
        // let mut loader = SourceLoader::default();
        // let loc = Locator::CLI("x - 1".to_string());
        // loader.load(&Input::from(loc.clone())).unwrap();
        // let unit = loader.translate(&Input::from(loc)).unwrap();

        // let x = FreeVar::fresh_named("x");
        // let minus = FreeVar::fresh_named("-");

        // let core_expr = acore::soup(vec![
        //     acore::var(x.clone()),
        //     acore::var(minus.clone()),
        //     acore::num(1),
        // ]);

        // assert_term_eq!(bound(unit.expr.clone()), bound(core_expr));
    }

    #[test]
    pub fn test_sample_4() {
        let mut loader = SourceLoader::default();
        let loc = Locator::Cli("f(x).v".to_string());
        loader.load(&Input::from(loc.clone())).unwrap();
        let unit = loader.translate(&Input::from(loc)).unwrap();

        let f = FreeVar::fresh_named("f");
        let x = FreeVar::fresh_named("x");

        let core_expr = acore::soup(vec![
            acore::var(f),
            acore::call(),
            acore::arg_tuple(vec![acore::var(x)]),
            acore::dot(),
            acore::name("v"),
        ]);

        assert_term_eq!(bound(unit.expr.clone()), bound(core_expr));
    }
}
