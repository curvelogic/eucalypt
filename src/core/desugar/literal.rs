use crate::{
    core::expr::{Expr, Primitive, RcExpr},
    syntax::ast::Literal,
};

use super::Desugarer;

/// Desugarer literal
///
/// Required by disembedding and standard desugaring
pub fn desugar_literal(desugarer: &mut Desugarer, lit: &Literal) -> RcExpr {
    match lit {
        Literal::Sym(s, v) => RcExpr::from(Expr::Literal(
            desugarer.new_smid(*s),
            Primitive::Sym(v.to_string()),
        )),
        Literal::Str(s, v) => RcExpr::from(Expr::Literal(
            desugarer.new_smid(*s),
            Primitive::Str(v.to_string()),
        )),
        Literal::Num(s, v) => RcExpr::from(Expr::Literal(
            desugarer.new_smid(*s),
            Primitive::Num(v.clone()),
        )),
    }
}
