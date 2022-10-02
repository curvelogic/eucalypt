//! Export pretty printed version of core.
use crate::common::{prettify::ToPretty, truncate::Truncated};
use crate::core::expr::*;
use moniker::Binder;
use moniker::Embed;
use moniker::FreeVar;
use moniker::Var;
use pretty::{DocAllocator, DocBuilder};

impl ToPretty for Var<String> {
    fn pretty<'b, D, A>(&'b self, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        allocator.text(
            self.pretty_name()
                .cloned()
                .unwrap_or_else(|| "?".to_string()),
        )
    }
}

impl ToPretty for FreeVar<String> {
    fn pretty<'b, D, A>(&'b self, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        allocator.text(
            self.pretty_name
                .as_ref()
                .cloned()
                .unwrap_or_else(|| "?".to_string()),
        )
    }
}

impl ToPretty for Primitive {
    fn pretty<'b, D, A>(&'b self, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match self {
            Primitive::Str(s) => allocator
                .text("\"")
                .append(allocator.text(s)) //TODO: escape quotes
                .append(allocator.text("\"")),
            Primitive::Sym(s) => allocator.text(":").append(allocator.text(s)),
            Primitive::Num(n) => allocator.text(format!("{}", n)),
            Primitive::Bool(b) => allocator.text(if *b { "true" } else { "false" }),
            Primitive::Null => allocator.text("null"),
        }
    }
}

impl ToPretty for Truncated<Primitive> {
    fn pretty<'b, D, A>(&'b self, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        self.content().pretty(allocator)
    }
}

impl ToPretty for RcExpr {
    /// Arrange core expression into pretty doc
    fn pretty<'b, D, A>(&'b self, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match &*self.inner {
            Expr::Var(_, v) => v.pretty(allocator),
            Expr::Name(_, n) => allocator.text(n),
            Expr::Let(_, scope, _) => {
                let body_doc = scope.unsafe_body.pretty(allocator);
                let binding_docs =
                    scope
                        .unsafe_pattern
                        .unsafe_pattern
                        .iter()
                        .map(|(Binder(fv), Embed(def))| {
                            fv.pretty(allocator)
                                .append(allocator.text(" = "))
                                .append(def.pretty(allocator))
                                .group()
                        });

                allocator
                    .text("let")
                    .append(allocator.space())
                    .append(
                        allocator
                            .intersperse(binding_docs, allocator.text(",").append(allocator.line()))
                            .align()
                            .append(allocator.line()),
                    )
                    .append(allocator.text(" in").append(allocator.line()))
                    .align()
                    .append(body_doc.indent(4))
            }
            Expr::Intrinsic(_, name) => allocator.text("__").append(allocator.text(name)),
            Expr::Literal(_, prim) => prim.pretty(allocator),
            Expr::Lookup(_, target, field, fallback) => {
                let base_doc = target
                    .pretty(allocator)
                    .append(allocator.text("."))
                    .append(allocator.text(field));
                match fallback {
                    None => base_doc,
                    Some(fb) => base_doc
                        .append(allocator.text("?"))
                        .append(fb.pretty(allocator)),
                }
            }
            Expr::Block(_, bm) => {
                let decl_docs = bm.iter().map(|(k, v)| {
                    allocator
                        .text(k)
                        .append(":")
                        .append(allocator.space())
                        .append(v.pretty(allocator))
                        .group()
                });
                allocator
                    .line()
                    .append(
                        allocator
                            .intersperse(decl_docs, allocator.text(",").append(allocator.line()))
                            .align()
                            .indent(2),
                    )
                    .append(allocator.line())
                    .braces()
                    .align()
            }
            Expr::List(_, xs) => {
                let elements_docs = xs.iter().map(|x| x.pretty(allocator));
                allocator
                    .text("[")
                    .append(
                        allocator.intersperse(elements_docs, allocator.text(",").group().nest(2)),
                    )
                    .append(allocator.text("]"))
            }
            Expr::BlockAnaphor(_, anaphor) => allocator.text(format!("•{}", anaphor)),
            Expr::ExprAnaphor(_, anaphor) => allocator.text(format!("{}", anaphor)),
            Expr::Meta(_, e, m) => {
                let meta_doc = allocator
                    .text("`")
                    .append(allocator.space())
                    .append(m.pretty(allocator))
                    .group();

                meta_doc
                    .append(allocator.softline())
                    .append(e.pretty(allocator))
                    .group()
            }
            Expr::ArgTuple(_, xs) => {
                let elements_docs = xs.iter().map(|x| x.pretty(allocator));
                allocator
                    .text("(")
                    .append(
                        allocator.intersperse(elements_docs, allocator.text(",").group().nest(2)),
                    )
                    .append(allocator.text(")"))
            }
            Expr::Lam(_, _, scope) => {
                let parameter_docs = scope
                    .unsafe_pattern
                    .iter()
                    .map(|Binder(fv)| fv.pretty(allocator));

                allocator
                    .text("λ")
                    .append(allocator.space())
                    .append(allocator.intersperse(parameter_docs, allocator.space()))
                    .append(allocator.space())
                    .append(allocator.text("."))
                    .append(allocator.line())
                    .append(scope.unsafe_body.pretty(allocator))
                    .group()
            }
            Expr::App(_, f, xs) => {
                let args_docs = xs.iter().map(|x| x.pretty(allocator));

                // parenthesise if not simple
                let target = if let Expr::Lam(_, _, _) = &*f.inner {
                    allocator
                        .text("(")
                        .append(f.pretty(allocator))
                        .append(allocator.text(")"))
                } else {
                    f.pretty(allocator)
                };

                target
                    .append(allocator.text("("))
                    .append(
                        allocator
                            .intersperse(args_docs, allocator.text(",").append(allocator.space()))
                            .group()
                            .nest(2),
                    )
                    .append(allocator.text(")"))
                    .group()
            }
            Expr::Soup(_, xs) => {
                let elements_docs = xs.iter().map(|x| x.pretty(allocator));
                allocator
                    .text("(")
                    .append(
                        allocator
                            .intersperse(elements_docs, allocator.space())
                            .group()
                            .nest(2),
                    )
                    .append(allocator.text(")"))
            }
            Expr::Operator(_, f, p, e) => allocator
                .text(format!("^{}({})^", f, p))
                .append(e.pretty(allocator)),
            Expr::ErrUnresolved(_, name) => allocator.text(format!("⚠unresolved:{}⚠", name)),
            Expr::ErrRedeclaration(_, name) => allocator.text(format!("⚠redecl:{}⚠", name)),
            Expr::ErrEliminated => allocator.text("⃝"),
            Expr::ErrPseudoDot => allocator.text("."),
            Expr::ErrPseudoCat => allocator.text("__CAT"),
            Expr::ErrPseudoCall => allocator.nil(),
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::{common::prettify::prettify, core::expr::acore::*};

    #[test]
    pub fn test_var() {
        assert_eq!(prettify(&free("x")), "x\n");
    }

    #[test]
    pub fn test_primitives() {
        assert_eq!(prettify(&Primitive::Str("blah".to_string())), "\"blah\"\n");
        assert_eq!(prettify(&Primitive::Sym("foo".to_string())), ":foo\n");
    }

    #[test]
    pub fn test_core_exprs() {
        assert_eq!(prettify(&num(20)), "20\n");
        assert_eq!(prettify(&bif("HEAD")), "__HEAD\n");

        let pseudocat = RcExpr::from(Expr::ErrPseudoCat);
        let sample = app(
            bif("HEAD"),
            vec![app(
                bif("CONS"),
                vec![
                    app(
                        pseudocat.clone(),
                        vec![list(vec![num(1), num(2), num(3)]), bif("HEAD")],
                    ),
                    app(
                        pseudocat,
                        vec![list(vec![num(1), num(2), num(3)]), bif("TAIL")],
                    ),
                ],
            )],
        );
        assert_eq!(
            prettify(&sample),
            "__HEAD(__CONS(__CAT([1,2,3], __HEAD), __CAT([1,2,3], __TAIL)))\n"
        );
    }
}
