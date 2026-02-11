//! Embed STG syntax as eucalypt structure for inspection and testing.
//!
//! Each STG expression type has a tag-based representation:
//!
//! - `Atom`:     `[:s-atom ref]`
//! - `Case`:     `[:s-case scrutinee [[:s-branch tag body] ...] fallback]`
//! - `Cons`:     `[:s-cons tag ref1 ref2 ...]`
//! - `App`:      `[:s-app callable ref1 ref2 ...]`
//! - `Bif`:      `[:s-bif :NAME ref1 ref2 ...]`
//! - `Let`:      `[:s-let [binding1 binding2 ...] body]`
//! - `LetRec`:   `[:s-letrec [binding1 binding2 ...] body]`
//! - `Ann`:      `[:s-ann smid body]`
//! - `Meta`:     `[:s-meta meta-ref body-ref]`
//! - `DeMeta`:   `[:s-demeta scrutinee handler or-else]`
//! - `BlackHole`: `[:s-hole]`
//!
//! Lambda forms:
//! - `Lambda`:   `[:s-lambda bound body]`
//! - `Thunk`:    `[:s-thunk body]`
//! - `Value`:    `[:s-value body]`
//!
//! References are represented as:
//! - Local:  `"L0"`, `"L1"`, etc.
//! - Global: `"G0"`, `"G1"`, etc.
//! - Value:  the native value directly (string, symbol, number)

use crate::eval::intrinsics;
use crate::syntax::export::embed::Embed;
use crate::syntax::export::pretty;
use crate::syntax::rowan::ast as rowan_ast;
use crate::syntax::rowan::kind::{SyntaxKind, SyntaxNode};
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;
use std::rc::Rc;

use super::syntax::{LambdaForm, Native, Ref, Reference, StgSyn};

/// Builder for constructing tagged list embeddings like `[tag, elem1, elem2, ...]`
struct StgEmbedBuilder {
    builder: GreenNodeBuilder<'static>,
    need_comma: bool,
}

impl StgEmbedBuilder {
    fn new(tag: &str) -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::SOUP.into());
        builder.start_node(SyntaxKind::LIST.into());
        builder.token(SyntaxKind::OPEN_SQUARE.into(), "[");

        // Tag element
        builder.start_node(SyntaxKind::SOUP.into());
        builder.token(SyntaxKind::UNQUOTED_IDENTIFIER.into(), tag);
        builder.finish_node();

        StgEmbedBuilder {
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

    fn symbol(&mut self, s: &str) -> &mut Self {
        self.token(&format!(":{s}"))
    }

    fn embed_soup(&mut self, soup: &rowan_ast::Soup) -> &mut Self {
        if self.need_comma {
            self.builder.token(SyntaxKind::COMMA.into(), ",");
            self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
        }
        self.builder.start_node(SyntaxKind::SOUP.into());
        let text = pretty::express(soup);
        self.builder
            .token(SyntaxKind::UNQUOTED_IDENTIFIER.into(), &text);
        self.builder.finish_node();
        self.need_comma = true;
        self
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

/// Format a reference for embedding
fn embed_ref(r: &Ref) -> String {
    match r {
        Reference::L(i) => format!("\"L{i}\""),
        Reference::G(i) => format!("\"G{i}\""),
        Reference::V(native) => match native {
            Native::Sym(s) => format!(":{s}"),
            Native::Str(s) => format!("\"{}\"", s.replace('"', "\\\"")),
            Native::Num(n) => format!("{n}"),
            Native::Zdt(dt) => format!("\"{dt}\""),
        },
    }
}

fn embed_stg(syn: &StgSyn) -> Option<rowan_ast::Soup> {
    match syn {
        StgSyn::Atom { evaluand } => {
            let mut b = StgEmbedBuilder::new("s-atom");
            b.token(&embed_ref(evaluand));
            b.finish()
        }
        StgSyn::Case {
            scrutinee,
            branches,
            fallback,
        } => {
            let mut b = StgEmbedBuilder::new("s-case");
            let scrutinee_soup = embed_stg(scrutinee)?;
            b.embed_soup(&scrutinee_soup);

            // Branches as a list of [tag, body] pairs
            let mut branch_parts = Vec::new();
            for (tag, body) in branches {
                let body_soup = embed_stg(body)?;
                let body_text = pretty::express(&body_soup);
                branch_parts.push(format!("[{tag}, {body_text}]"));
            }
            let branches_text = format!("[{}]", branch_parts.join(", "));
            b.token(&branches_text);

            if let Some(fb) = fallback {
                let fb_soup = embed_stg(fb)?;
                b.embed_soup(&fb_soup);
            }
            b.finish()
        }
        StgSyn::Cons { tag, args } => {
            let mut b = StgEmbedBuilder::new("s-cons");
            b.token(&format!("{tag}"));
            for arg in args {
                b.token(&embed_ref(arg));
            }
            b.finish()
        }
        StgSyn::App { callable, args } => {
            let mut b = StgEmbedBuilder::new("s-app");
            b.token(&embed_ref(callable));
            for arg in args {
                b.token(&embed_ref(arg));
            }
            b.finish()
        }
        StgSyn::Bif { intrinsic, args } => {
            let name = intrinsics::intrinsic(*intrinsic as usize).name();
            let mut b = StgEmbedBuilder::new("s-bif");
            b.symbol(name);
            for arg in args {
                b.token(&embed_ref(arg));
            }
            b.finish()
        }
        StgSyn::Let { bindings, body } => {
            let mut b = StgEmbedBuilder::new("s-let");

            let mut binding_parts = Vec::new();
            for lam in bindings {
                let lam_soup = embed_lambda_form(lam)?;
                binding_parts.push(pretty::express(&lam_soup));
            }
            let bindings_text = format!("[{}]", binding_parts.join(", "));
            b.token(&bindings_text);

            let body_soup = embed_stg(body)?;
            b.embed_soup(&body_soup);
            b.finish()
        }
        StgSyn::LetRec { bindings, body } => {
            let mut b = StgEmbedBuilder::new("s-letrec");

            let mut binding_parts = Vec::new();
            for lam in bindings {
                let lam_soup = embed_lambda_form(lam)?;
                binding_parts.push(pretty::express(&lam_soup));
            }
            let bindings_text = format!("[{}]", binding_parts.join(", "));
            b.token(&bindings_text);

            let body_soup = embed_stg(body)?;
            b.embed_soup(&body_soup);
            b.finish()
        }
        StgSyn::Ann { smid, body } => {
            let mut b = StgEmbedBuilder::new("s-ann");
            b.token(&format!("{smid}"));
            let body_soup = embed_stg(body)?;
            b.embed_soup(&body_soup);
            b.finish()
        }
        StgSyn::Meta { meta, body } => {
            let mut b = StgEmbedBuilder::new("s-meta");
            b.token(&embed_ref(meta));
            b.token(&embed_ref(body));
            b.finish()
        }
        StgSyn::DeMeta {
            scrutinee,
            handler,
            or_else,
        } => {
            let mut b = StgEmbedBuilder::new("s-demeta");
            let scrutinee_soup = embed_stg(scrutinee)?;
            b.embed_soup(&scrutinee_soup);
            let handler_soup = embed_stg(handler)?;
            b.embed_soup(&handler_soup);
            let or_else_soup = embed_stg(or_else)?;
            b.embed_soup(&or_else_soup);
            b.finish()
        }
        StgSyn::BlackHole => StgEmbedBuilder::new("s-hole").finish(),
    }
}

fn embed_lambda_form(lam: &LambdaForm) -> Option<rowan_ast::Soup> {
    match lam {
        LambdaForm::Lambda { bound, body, .. } => {
            let mut b = StgEmbedBuilder::new("s-lambda");
            b.token(&format!("{bound}"));
            let body_soup = embed_stg(body)?;
            b.embed_soup(&body_soup);
            b.finish()
        }
        LambdaForm::Thunk { body } => {
            let mut b = StgEmbedBuilder::new("s-thunk");
            let body_soup = embed_stg(body)?;
            b.embed_soup(&body_soup);
            b.finish()
        }
        LambdaForm::Value { body } => {
            let mut b = StgEmbedBuilder::new("s-value");
            let body_soup = embed_stg(body)?;
            b.embed_soup(&body_soup);
            b.finish()
        }
    }
}

impl Embed for StgSyn {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        embed_stg(self)
    }
}

impl Embed for LambdaForm {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        embed_lambda_form(self)
    }
}

impl Embed for Rc<StgSyn> {
    fn embed(&self) -> Option<rowan_ast::Soup> {
        embed_stg(self)
    }
}

/// Embed a representation of the STG expression rendered as a parse-embed unit.
pub fn quote_embed_stg_unit(syn: &StgSyn) -> String {
    if let Some(soup) = syn.embed() {
        format!(
            " {{ parse-embed: :STG }}

` {{ embedding: :stg }}
STG: {}

",
            pretty::express(&soup)
        )
    } else {
        "# Error: could not embed STG expression".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::stg::syntax::dsl;

    #[test]
    fn test_atom_embed() {
        let syn = StgSyn::Atom {
            evaluand: dsl::lref(0),
        };
        assert!(syn.embed().is_some(), "Should embed atom");
    }

    #[test]
    fn test_cons_embed() {
        let syn = dsl::data(0, vec![dsl::lref(0), dsl::lref(1)]);
        assert!(syn.embed().is_some(), "Should embed data constructor");
    }

    #[test]
    fn test_app_embed() {
        let syn = dsl::app(dsl::lref(0), vec![dsl::lref(1)]);
        assert!(syn.embed().is_some(), "Should embed application");
    }

    #[test]
    fn test_bif_embed() {
        let syn = dsl::app_bif(0, vec![dsl::lref(0), dsl::lref(1)]);
        assert!(syn.embed().is_some(), "Should embed built-in function");
    }

    #[test]
    fn test_let_embed() {
        let syn = dsl::let_(
            vec![dsl::value(dsl::local(0))],
            dsl::app(dsl::lref(0), vec![dsl::lref(1)]),
        );
        assert!(syn.embed().is_some(), "Should embed let");
    }

    #[test]
    fn test_lambda_form_embed() {
        let lam = dsl::lambda(2, dsl::app(dsl::lref(0), vec![dsl::lref(1)]));
        assert!(lam.embed().is_some(), "Should embed lambda form");
    }

    #[test]
    fn test_thunk_embed() {
        let lam = dsl::thunk(dsl::local(0));
        assert!(lam.embed().is_some(), "Should embed thunk");
    }

    #[test]
    fn test_case_embed() {
        let syn = dsl::case(dsl::local(0), vec![(1, dsl::local(0))], dsl::local(1));
        assert!(syn.embed().is_some(), "Should embed case");
    }

    #[test]
    fn test_blackhole_embed() {
        let syn = StgSyn::BlackHole;
        assert!(syn.embed().is_some(), "Should embed blackhole");
    }

    #[test]
    fn test_native_ref_embed() {
        let syn = StgSyn::Atom {
            evaluand: dsl::str("hello"),
        };
        assert!(syn.embed().is_some(), "Should embed native string ref");
    }
}
