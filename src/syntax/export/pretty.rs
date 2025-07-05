//! Pretty printing ASTs in Eucalypt syntax (i.e. writing Eucalypt)
use crate::syntax::rowan::ast as rowan_ast;
use pretty::RcDoc;
use rowan::ast::AstNode;

/// Express AST as eucalypt, pretty printing to string.
pub fn express<I>(expr: &I) -> String
where
    I: ToSourceDoc,
{
    let doc = expr.source_doc();
    let mut w = Vec::new();
    doc.render(80, &mut w).unwrap();
    String::from_utf8(w).unwrap()
}

/// Express AST as eucalypt, pretty printing to string.
pub fn express_unit(expr: &rowan_ast::Soup) -> String {
    // Simplified implementation for now
    format!("/* {} */", expr.syntax().text())
}

/// Rendering AST as Eucalypt source text (without regard for
/// preserving whitespace implied by Spans...)
pub trait ToSourceDoc {
    fn source_doc(&self) -> RcDoc<'static, ()>;
}

// Simplified implementations to get compilation working
impl ToSourceDoc for rowan_ast::Literal {
    fn source_doc(&self) -> RcDoc<'static, ()> {
        RcDoc::text(self.syntax().text().to_string())
    }
}

impl ToSourceDoc for rowan_ast::Name {
    fn source_doc(&self) -> RcDoc<'static, ()> {
        RcDoc::text(self.syntax().text().to_string())
    }
}

impl ToSourceDoc for rowan_ast::ApplyTuple {
    fn source_doc(&self) -> RcDoc<'static, ()> {
        RcDoc::text(self.syntax().text().to_string())
    }
}

impl ToSourceDoc for rowan_ast::Declaration {
    fn source_doc(&self) -> RcDoc<'static, ()> {
        RcDoc::text(self.syntax().text().to_string())
    }
}

impl ToSourceDoc for rowan_ast::Block {
    fn source_doc(&self) -> RcDoc<'static, ()> {
        RcDoc::text(self.syntax().text().to_string())
    }
}

impl ToSourceDoc for rowan_ast::Element {
    fn source_doc(&self) -> RcDoc<'static, ()> {
        RcDoc::text(self.syntax().text().to_string())
    }
}

impl ToSourceDoc for rowan_ast::Soup {
    fn source_doc(&self) -> RcDoc<'static, ()> {
        RcDoc::text(self.syntax().text().to_string())
    }
}

impl ToSourceDoc for Option<rowan_ast::Soup> {
    fn source_doc(&self) -> RcDoc<'static, ()> {
        match self {
            Some(soup) => soup.source_doc(),
            None => RcDoc::text("/* no content */"),
        }
    }
}

/// The top level block is a unit and doesn't have nesting or braces
pub fn unit_source_doc(block: &rowan_ast::Block) -> RcDoc<'static, ()> {
    block.source_doc()
}
