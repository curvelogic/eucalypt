//! Export core as structure of eucalypt lists and blocks for
//! processing within eucalypt.
use crate::core::expr::*;
use crate::syntax::export::embed::Embed;
use crate::syntax::export::pretty;
use crate::syntax::rowan::ast as rowan_ast;
use moniker;
use rowan::ast::AstNode;

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


/// Helper function to create a soup with a tagged name (e.g., c-var, c-name)
fn embed_tagged_name(tag: &str, name: &str) -> Option<rowan_ast::Soup> {
    use crate::syntax::rowan::kind::{SyntaxKind, SyntaxNode};
    use rowan::GreenNodeBuilder;

    let mut builder = GreenNodeBuilder::new();
    builder.start_node(SyntaxKind::SOUP.into());

    // Create list with tag and name: [tag, name]
    builder.start_node(SyntaxKind::LIST.into());
    builder.token(SyntaxKind::OPEN_SQUARE.into(), "[");

    // Tag element (e.g., "c-var")
    builder.start_node(SyntaxKind::SOUP.into());
    builder.token(SyntaxKind::UNQUOTED_IDENTIFIER.into(), tag);
    builder.finish_node();

    builder.token(SyntaxKind::COMMA.into(), ",");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");

    // Name element
    builder.start_node(SyntaxKind::SOUP.into());
    builder.token(SyntaxKind::UNQUOTED_IDENTIFIER.into(), name);
    builder.finish_node();

    builder.token(SyntaxKind::CLOSE_SQUARE.into(), "]");
    builder.finish_node();

    builder.finish_node();
    let green = builder.finish();
    let syntax = SyntaxNode::new_root(green);
    rowan_ast::Soup::cast(syntax)
}

/// Helper function to create a soup with a tagged literal (e.g., c-lit)
fn embed_tagged_literal(tag: &str, literal: &str) -> Option<rowan_ast::Soup> {
    use crate::syntax::rowan::kind::{SyntaxKind, SyntaxNode};
    use rowan::GreenNodeBuilder;

    let mut builder = GreenNodeBuilder::new();
    builder.start_node(SyntaxKind::SOUP.into());

    // Create list with tag and literal: [tag, literal]
    builder.start_node(SyntaxKind::LIST.into());
    builder.token(SyntaxKind::OPEN_SQUARE.into(), "[");

    // Tag element (e.g., "c-lit")
    builder.start_node(SyntaxKind::SOUP.into());
    builder.token(SyntaxKind::UNQUOTED_IDENTIFIER.into(), tag);
    builder.finish_node();

    builder.token(SyntaxKind::COMMA.into(), ",");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");

    // Literal element
    builder.start_node(SyntaxKind::SOUP.into());
    builder.token(SyntaxKind::UNQUOTED_IDENTIFIER.into(), literal);
    builder.finish_node();

    builder.token(SyntaxKind::CLOSE_SQUARE.into(), "]");
    builder.finish_node();

    builder.finish_node();
    let green = builder.finish();
    let syntax = SyntaxNode::new_root(green);
    rowan_ast::Soup::cast(syntax)
}

impl Embed for CoreExpr {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        match self {
            CoreExpr::Var(_, var) => {
                // Create a soup with c-var tag for variables
                match var {
                    moniker::Var::Free(freevar) => {
                        if let Some(name) = &freevar.pretty_name {
                            embed_tagged_name("c-var", name)
                        } else {
                            None // Can't embed anonymous variables
                        }
                    }
                    moniker::Var::Bound(_) => None, // Bound variables can't be embedded in simple form
                }
            }
            CoreExpr::Literal(_, primitive) => {
                // Create a soup with c-lit tag for literals
                primitive.embed()
            }
            CoreExpr::Name(_, name) => {
                // Create a soup with c-name tag for names
                embed_tagged_name("c-name", name)
            }
            _ => {
                // For complex expressions, return None for now
                // TODO: Implement embedding for all expression types
                None
            }
        }
    }
}

impl Embed for Primitive {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        match self {
            Primitive::Sym(s) => embed_tagged_literal("c-lit", &format!(":{s}")),
            Primitive::Str(s) => {
                embed_tagged_literal("c-lit", &format!("\"{}\"", s.replace('"', "\\\"")))
            }
            Primitive::Num(n) => embed_tagged_literal("c-lit", &format!("{n}")),
            _ => None, // For other primitive types, return None for now
        }
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
    use crate::syntax::export::pretty;

    #[test]
    pub fn test_simple_embedding() {
        let core_expr = acore::name("test");
        let embedding = core_expr.embed();
        assert!(embedding.is_some(), "Should be able to embed simple name");

        if let Some(soup) = embedding {
            let output = pretty::express(&soup);
            // Just check that it doesn't crash and produces some output
            assert!(!output.is_empty(), "Should produce non-empty output");
        }
    }
}
