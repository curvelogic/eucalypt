//! Export core as structure of eucalypt lists and blocks for
//! processing within eucalypt.
use crate::core::expr::*;
use crate::syntax::export::embed::Embed;
use crate::syntax::export::pretty;
use crate::syntax::rowan::ast as rowan_ast;
use crate::syntax::rowan::kind::{SyntaxKind, SyntaxNode};
use moniker;
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;

/// Embed a representation of the core expression in an AST then
/// render as parse-embed unit.
///
/// The core emitted can be consumed by `parse-embed` functionality
/// which reads such a representation out of the AST to build core,
/// instead of using the normal translation process.
pub fn quote_embed_core_unit(expr: &RcExpr) -> String {
    if let Some(soup) = expr.embed() {
        format!(
            " {{ parse-embed: :CORE }}

` {{ embedding: :core }}
CORE: {}

",
            pretty::express_unit(&soup)
        )
    } else {
        "# Error: could not embed core expression".to_string()
    }
}

/// Builder for constructing tagged list embeddings like `[tag, elem1, elem2, ...]`
struct EmbedBuilder {
    builder: GreenNodeBuilder<'static>,
    need_comma: bool,
}

impl EmbedBuilder {
    fn new(tag: &str) -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::SOUP.into());
        builder.start_node(SyntaxKind::LIST.into());
        builder.token(SyntaxKind::OPEN_SQUARE.into(), "[");

        // Tag element
        builder.start_node(SyntaxKind::SOUP.into());
        builder.token(SyntaxKind::UNQUOTED_IDENTIFIER.into(), tag);
        builder.finish_node();

        EmbedBuilder {
            builder,
            need_comma: true,
        }
    }

    fn token(&mut self, text: &str) -> &mut Self {
        if self.need_comma {
            self.builder.token(SyntaxKind::COMMA.into(), ",");
            self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
        }
        self.builder.start_node(SyntaxKind::SOUP.into());
        self.builder
            .token(SyntaxKind::UNQUOTED_IDENTIFIER.into(), text);
        self.builder.finish_node();
        self.need_comma = true;
        self
    }

    fn string(&mut self, s: &str) -> &mut Self {
        self.token(&format!("\"{}\"", s.replace('"', "\\\"")))
    }

    fn symbol(&mut self, s: &str) -> &mut Self {
        self.token(&format!(":{s}"))
    }

    fn embed(&mut self, expr: &impl Embed) -> Option<&mut Self> {
        let soup = expr.embed()?;
        if self.need_comma {
            self.builder.token(SyntaxKind::COMMA.into(), ",");
            self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
        }
        // Splice the embedded soup's green node directly
        self.builder.start_node(SyntaxKind::SOUP.into());
        // Re-emit the soup content as a single identifier token containing
        // its pretty-printed form. This is a simplification that works
        // because the embedding is consumed via pretty-printing.
        let text = pretty::express(&soup);
        self.builder
            .token(SyntaxKind::UNQUOTED_IDENTIFIER.into(), &text);
        self.builder.finish_node();
        self.need_comma = true;
        Some(self)
    }

    fn finish(mut self) -> Option<rowan_ast::Soup> {
        self.builder.token(SyntaxKind::CLOSE_SQUARE.into(), "]");
        self.builder.finish_node(); // LIST
        self.builder.finish_node(); // SOUP
        let green = self.builder.finish();
        let syntax = SyntaxNode::new_root(green);
        rowan_ast::Soup::cast(syntax)
    }
}

impl Embed for CoreExpr {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        match self {
            CoreExpr::Var(_, var) => match var {
                moniker::Var::Free(freevar) => freevar.pretty_name.as_ref().and_then(|name| {
                    let mut b = EmbedBuilder::new("c-var");
                    b.string(name);
                    b.finish()
                }),
                moniker::Var::Bound(bound) => {
                    let mut b = EmbedBuilder::new("c-var");
                    b.token(&format!("${}.{}", bound.scope.0, bound.binder.to_usize()));
                    b.finish()
                }
            },
            CoreExpr::Literal(_, primitive) => primitive.embed(),
            CoreExpr::Name(_, name) => {
                let mut b = EmbedBuilder::new("c-name");
                b.string(name);
                b.finish()
            }
            CoreExpr::Intrinsic(_, name) => {
                let mut b = EmbedBuilder::new("c-bif");
                b.symbol(name);
                b.finish()
            }
            CoreExpr::List(_, items) => {
                let mut b = EmbedBuilder::new("c-list");
                for item in items {
                    b.embed(item)?;
                }
                b.finish()
            }
            CoreExpr::App(_, func, args) => {
                let mut b = EmbedBuilder::new("c-app");
                b.embed(func)?;
                // Embed args as a sub-list
                let mut args_builder = EmbedBuilder::new("c-args");
                for arg in args {
                    args_builder.embed(arg)?;
                }
                let args_soup = args_builder.finish()?;
                b.embed(&args_soup)?;
                b.finish()
            }
            CoreExpr::Soup(_, items) => {
                let mut b = EmbedBuilder::new("c-soup");
                for item in items {
                    b.embed(item)?;
                }
                b.finish()
            }
            CoreExpr::ArgTuple(_, args) => {
                let mut b = EmbedBuilder::new("c-args");
                for arg in args {
                    b.embed(arg)?;
                }
                b.finish()
            }
            CoreExpr::Meta(_, expr, meta) => {
                let mut b = EmbedBuilder::new("c-meta");
                b.embed(expr)?;
                b.embed(meta)?;
                b.finish()
            }
            CoreExpr::Lookup(_, obj, key, fallback) => {
                let mut b = EmbedBuilder::new("c-lookup");
                b.embed(obj)?;
                b.string(key);
                if let Some(fb) = fallback {
                    b.embed(fb)?;
                }
                b.finish()
            }
            CoreExpr::Operator(_, fixity, prec, expr) => {
                let fix_str = match fixity {
                    Fixity::Nullary => "nullary",
                    Fixity::UnaryPrefix => "prefix",
                    Fixity::UnaryPostfix => "postfix",
                    Fixity::InfixLeft => "infixl",
                    Fixity::InfixRight => "infixr",
                };
                let mut b = EmbedBuilder::new("c-op");
                b.symbol(fix_str);
                b.token(&format!("{prec}"));
                b.embed(expr)?;
                b.finish()
            }
            CoreExpr::BlockAnaphor(_, _) => EmbedBuilder::new("c-bk-ana").finish(),
            CoreExpr::ExprAnaphor(_, _) => EmbedBuilder::new("c-ex-ana").finish(),
            CoreExpr::ErrUnresolved(_, name) => {
                let mut b = EmbedBuilder::new("e-unresolved");
                b.string(name);
                b.finish()
            }
            CoreExpr::ErrRedeclaration(_, name) => {
                let mut b = EmbedBuilder::new("e-redeclaration");
                b.string(name);
                b.finish()
            }
            CoreExpr::ErrEliminated => EmbedBuilder::new("e-eliminated").finish(),
            CoreExpr::ErrPseudoDot => EmbedBuilder::new("e-pseudodot").finish(),
            CoreExpr::ErrPseudoCall => EmbedBuilder::new("e-pseudocall").finish(),
            CoreExpr::ErrPseudoCat => EmbedBuilder::new("e-pseudocat").finish(),
            // Let and Lam require scope manipulation which is complex;
            // return None for these uncommon embedding cases
            CoreExpr::Let(..) | CoreExpr::Lam(..) | CoreExpr::Block(..) => None,
        }
    }
}

impl Embed for Primitive {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        let mut b = EmbedBuilder::new("c-lit");
        match self {
            Primitive::Sym(s) => {
                b.symbol(s);
            }
            Primitive::Str(s) => {
                b.string(s);
            }
            Primitive::Num(n) => {
                b.token(&format!("{n}"));
            }
            Primitive::Bool(true) => {
                b.token("true");
            }
            Primitive::Bool(false) => {
                b.token("false");
            }
            Primitive::Null => {
                b.token("null");
            }
        }
        b.finish()
    }
}

impl Embed for RcExpr {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        self.inner.embed()
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::common::sourcemap::Smid;
    use crate::syntax::export::pretty;

    #[test]
    pub fn test_simple_embedding() {
        let core_expr = acore::name("test");
        let embedding = core_expr.embed();
        assert!(embedding.is_some(), "Should be able to embed simple name");

        if let Some(soup) = embedding {
            let output = pretty::express(&soup);
            assert!(!output.is_empty(), "Should produce non-empty output");
        }
    }

    #[test]
    pub fn test_intrinsic_embedding() {
        let core_expr = CoreExpr::Intrinsic(Smid::default(), "ADD".to_string());
        let embedding = core_expr.embed();
        assert!(embedding.is_some(), "Should embed intrinsic");
    }

    #[test]
    pub fn test_literal_bool_embedding() {
        let core_expr = CoreExpr::Literal(Smid::default(), Primitive::Bool(true));
        let embedding = core_expr.embed();
        assert!(embedding.is_some(), "Should embed boolean literal");
    }

    #[test]
    pub fn test_literal_null_embedding() {
        let core_expr = CoreExpr::Literal(Smid::default(), Primitive::Null);
        let embedding = core_expr.embed();
        assert!(embedding.is_some(), "Should embed null literal");
    }

    #[test]
    pub fn test_error_markers() {
        assert!(CoreExpr::ErrEliminated.embed().is_some());
        assert!(CoreExpr::ErrPseudoDot.embed().is_some());
        assert!(CoreExpr::ErrPseudoCall.embed().is_some());
        assert!(CoreExpr::ErrPseudoCat.embed().is_some());
    }
}
