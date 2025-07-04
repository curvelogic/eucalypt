//! Export pretty printed version of core.

use pretty::{DocAllocator, DocBuilder};

use super::syntax::{LambdaForm, StgSyn};
use crate::common::prettify::ToPretty;

impl ToPretty for LambdaForm {
    fn pretty<'b, D, A>(&'b self, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match self {
            LambdaForm::Lambda { bound, body, .. } => allocator
                .text(format!("λ{{{}}}", bound))
                .append(body.pretty(allocator).parens()),
            LambdaForm::Thunk { body } => {
                allocator.text("Th").append(body.pretty(allocator).parens())
            }
            LambdaForm::Value { body } => body.pretty(allocator),
        }
    }
}

impl ToPretty for StgSyn {
    fn pretty<'b, D, A>(&'b self, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match self {
            StgSyn::Atom { evaluand } => allocator.text(format!("{evaluand}")),
            StgSyn::Case {
                scrutinee,
                branches,
                fallback,
            } => {
                let scrutinee_doc = allocator
                    .text("CASE")
                    .append(scrutinee.pretty(allocator).parens())
                    .append(allocator.line());
                let mut branch_docs: Vec<_> = branches
                    .iter()
                    .map(|(t, c)| {
                        allocator
                            .text(format!("{}: ", t))
                            .append(c.pretty(allocator))
                    })
                    .collect();
                if let Some(fb) = fallback {
                    branch_docs.push(allocator.text("…: ").append(fb.pretty(allocator)));
                }

                let branch_doc = allocator.intersperse(branch_docs, allocator.line()).align();
                scrutinee_doc.append(branch_doc).hang(2)
            }
            StgSyn::Cons { tag, args } => {
                let args_docs = args.iter().map(|r| allocator.text(format!("{r}")));
                allocator.text(format!("DATA[{}]", tag)).append(
                    allocator
                        .intersperse(args_docs, allocator.text(" "))
                        .parens(),
                )
            }
            StgSyn::App { callable, args } => {
                let args_docs = args.iter().map(|r| allocator.text(format!("{r}")));
                allocator.text(format!("{callable}")).append(
                    allocator
                        .intersperse(args_docs, allocator.text(" "))
                        .parens(),
                )
            }
            StgSyn::Bif { intrinsic, args } => {
                let args_docs = args.iter().map(|r| allocator.text(format!("{r}")));
                allocator.text(format!("BIF[{}]", intrinsic)).append(
                    allocator
                        .intersperse(args_docs, allocator.text(" "))
                        .parens(),
                )
            }
            StgSyn::Let { bindings, body } => {
                let binding_docs = bindings.iter().map(|d| d.pretty(allocator));
                let body_doc = body.pretty(allocator);
                allocator
                    .text("LET")
                    .append(allocator.space())
                    .append(
                        allocator
                            .intersperse(binding_docs, allocator.line())
                            .align(),
                    )
                    .append(allocator.line())
                    .append(body_doc.parens().indent(2))
            }
            StgSyn::LetRec { bindings, body } => {
                let binding_docs = bindings.iter().map(|d| d.pretty(allocator));
                let body_doc = body.pretty(allocator);

                allocator
                    .text("LETREC")
                    .append(allocator.line())
                    .append(
                        allocator
                            .intersperse(binding_docs, allocator.line())
                            .align(),
                    )
                    .hang(2)
                    .append(allocator.line())
                    .append(body_doc)
                    .hang(2)
            }
            StgSyn::Ann { smid, body } => allocator
                .text(format!("♩{}:", smid))
                .append(body.pretty(allocator)),
            StgSyn::Meta { meta, body } => allocator
                .text("`")
                .append(allocator.text(format!("{meta}")))
                .append(allocator.text(format!("{body}"))),
            StgSyn::DeMeta {
                scrutinee,
                handler,
                or_else,
            } => {
                let scrutinee_body = scrutinee.pretty(allocator);
                let handler_case = allocator
                    .text("`:")
                    .append(handler.pretty(allocator))
                    .group();
                let else_case = allocator
                    .text("•:")
                    .append(or_else.pretty(allocator))
                    .group();

                let branch_doc = (handler_case
                    .append(",")
                    .append(allocator.line())
                    .append(else_case))
                .angles();

                allocator
                    .text("ƒ")
                    .append(scrutinee_body.parens())
                    .append("⑂")
                    .append(branch_doc)
            }
            StgSyn::BlackHole => allocator.text("HOLE"),
        }
    }
}
