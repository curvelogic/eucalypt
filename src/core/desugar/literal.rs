use crate::{
    core::expr::{Expr, Primitive, RcExpr},
    syntax::rowan::ast as rowan_ast,
};

use rowan::ast::AstNode;
use rowan::TextRange;

use super::Desugarer;

/// Convert a TextRange to a Span for legacy compatibility
fn text_range_to_span(range: TextRange) -> codespan::Span {
    let start = codespan::ByteIndex(range.start().into());
    let end = codespan::ByteIndex(range.end().into());
    codespan::Span::new(start, end)
}

/// Desugarer literal for Rowan AST
///
/// Required by disembedding and standard desugaring
pub fn desugar_literal(desugarer: &mut Desugarer, lit: &rowan_ast::Literal) -> RcExpr {
    let span = text_range_to_span(lit.syntax().text_range());
    let smid = desugarer.new_smid(span);

    if let Some(value) = lit.value() {
        let primitive = match value {
            rowan_ast::LiteralValue::Sym(s) => {
                if let Some(text) = s.value() {
                    Primitive::Sym(text.to_string())
                } else {
                    Primitive::Sym("invalid".to_string())
                }
            }
            rowan_ast::LiteralValue::Str(s) => {
                if let Some(text) = s.value() {
                    Primitive::Str(text.to_string())
                } else {
                    Primitive::Str("".to_string())
                }
            }
            rowan_ast::LiteralValue::Num(n) => {
                if let Some(num) = n.value() {
                    Primitive::Num(num)
                } else {
                    Primitive::Num(0.into())
                }
            }
        };
        RcExpr::from(Expr::Literal(smid, primitive))
    } else {
        // Fallback for malformed literals
        RcExpr::from(Expr::Literal(smid, Primitive::Str("invalid".to_string())))
    }
}
