use crate::{
    core::{
        error::CoreError,
        expr::{self, Expr, Primitive, RcExpr},
    },
    syntax::rowan::{ast as rowan_ast, lex},
};

use rowan::ast::AstNode;
use rowan::TextRange;

use super::escape;
use super::Desugarer;

/// Convert a TextRange to a Span for legacy compatibility
fn text_range_to_span(range: TextRange) -> codespan::Span {
    let start = codespan::ByteIndex(range.start().into());
    let end = codespan::ByteIndex(range.end().into());
    codespan::Span::new(start, end)
}

/// Desugar a literal from the Rowan AST into a core expression.
///
/// Returns an error with span information for malformed literals
/// (e.g. invalid escape sequences in c-strings).
pub fn desugar_literal(
    desugarer: &mut Desugarer,
    lit: &rowan_ast::Literal,
) -> Result<RcExpr, CoreError> {
    let span = text_range_to_span(lit.syntax().text_range());
    let smid = desugarer.new_smid(span);

    if let Some(value) = lit.value() {
        let primitive = match value {
            rowan_ast::LiteralValue::Sym(s) => {
                if let Some(text) = s.value() {
                    Primitive::Sym(text.to_string())
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "invalid symbol literal".to_string(),
                        smid,
                    ));
                }
            }
            rowan_ast::LiteralValue::Str(s) => {
                if let Some(text) = s.value() {
                    Primitive::Str(text.to_string())
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "invalid string literal".to_string(),
                        smid,
                    ));
                }
            }
            rowan_ast::LiteralValue::CStr(s) => {
                if let Some(raw_text) = s.raw_value() {
                    match escape::process_escapes(raw_text) {
                        Ok(processed) => Primitive::Str(processed),
                        Err(e) => {
                            return Err(CoreError::InvalidEmbedding(
                                format!("invalid c-string escape: {e}"),
                                smid,
                            ));
                        }
                    }
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "invalid c-string literal".to_string(),
                        smid,
                    ));
                }
            }
            rowan_ast::LiteralValue::RawStr(s) => {
                if let Some(text) = s.value() {
                    Primitive::Str(text.to_string())
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "invalid raw string literal".to_string(),
                        smid,
                    ));
                }
            }
            rowan_ast::LiteralValue::TStr(s) => {
                if let Some(content) = s.value() {
                    let normalized = lex::normalize_zdt_for_parse(content);
                    return Ok(expr::core::app(
                        smid,
                        expr::core::bif(smid, "ZDT.PARSE"),
                        vec![expr::core::str(smid, normalized)],
                    ));
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "invalid ZDT literal".to_string(),
                        smid,
                    ));
                }
            }
            rowan_ast::LiteralValue::Num(n) => {
                if let Some(num) = n.value() {
                    Primitive::Num(num)
                } else {
                    return Err(CoreError::InvalidEmbedding(
                        "invalid number literal".to_string(),
                        smid,
                    ));
                }
            }
        };
        Ok(RcExpr::from(Expr::Literal(smid, primitive)))
    } else {
        Err(CoreError::InvalidEmbedding(
            "malformed literal".to_string(),
            smid,
        ))
    }
}
