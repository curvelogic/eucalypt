//! Export pretty printed version of STG syntax.

use std::convert::TryFrom;

use pretty::{DocAllocator, DocBuilder};

use super::syntax::{LambdaForm, StgSyn};
use super::tags::DataConstructor;
use crate::common::prettify::ToPretty;
use crate::eval::intrinsics;

/// Format a data constructor tag as a readable name where possible.
fn tag_name(tag: u8) -> String {
    match DataConstructor::try_from(tag) {
        Ok(DataConstructor::Unit) => "Unit".to_string(),
        Ok(DataConstructor::BoolTrue) => "True".to_string(),
        Ok(DataConstructor::BoolFalse) => "False".to_string(),
        Ok(DataConstructor::BoxedNumber) => "Num".to_string(),
        Ok(DataConstructor::BoxedSymbol) => "Sym".to_string(),
        Ok(DataConstructor::BoxedString) => "Str".to_string(),
        Ok(DataConstructor::ListNil) => "Nil".to_string(),
        Ok(DataConstructor::ListCons) => "Cons".to_string(),
        Ok(DataConstructor::Block) => "Block".to_string(),
        Ok(DataConstructor::BlockPair) => "Pair".to_string(),
        Ok(DataConstructor::BlockKvList) => "KvList".to_string(),
        Ok(DataConstructor::BoxedZdt) => "Zdt".to_string(),
        Err(()) => format!("#{tag}"),
    }
}

impl ToPretty for LambdaForm {
    fn pretty<'b, D, A>(&'b self, allocator: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        match self {
            LambdaForm::Lambda { bound, body, .. } => allocator
                .text(format!("\u{03bb}{{{bound}}}"))
                .append(allocator.space())
                .append(body.pretty(allocator))
                .group(),
            LambdaForm::Thunk { body } => allocator
                .text("thunk ")
                .append(body.pretty(allocator))
                .group(),
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
                    .text("case ")
                    .append(scrutinee.pretty(allocator))
                    .append(allocator.text(" of"));

                let mut branch_docs: Vec<_> = branches
                    .iter()
                    .map(|(t, c)| {
                        allocator
                            .text(tag_name(*t))
                            .append(allocator.text(" \u{2192} "))
                            .append(c.pretty(allocator))
                            .group()
                    })
                    .collect();
                if let Some(fb) = fallback {
                    branch_docs.push(
                        allocator
                            .text("_ \u{2192} ")
                            .append(fb.pretty(allocator))
                            .group(),
                    );
                }

                scrutinee_doc
                    .append(
                        allocator
                            .line()
                            .append(allocator.intersperse(branch_docs, allocator.line()).align())
                            .nest(2),
                    )
                    .group()
            }
            StgSyn::Cons { tag, args } => {
                let name = tag_name(*tag);
                if args.is_empty() {
                    return allocator.text(name);
                }
                let args_docs = args.iter().map(|r| allocator.text(format!("{r}")));
                allocator.text(name).append(
                    allocator
                        .intersperse(args_docs, allocator.text(", "))
                        .parens(),
                )
            }
            StgSyn::App { callable, args } => {
                let args_docs = args.iter().map(|r| allocator.text(format!("{r}")));
                allocator.text(format!("{callable}")).append(
                    allocator
                        .intersperse(args_docs, allocator.text(", "))
                        .parens(),
                )
            }
            StgSyn::Bif { intrinsic, args } => {
                let name = intrinsics::intrinsic(*intrinsic as usize).name();
                let args_docs = args.iter().map(|r| allocator.text(format!("{r}")));
                allocator.text(name).append(
                    allocator
                        .intersperse(args_docs, allocator.text(", "))
                        .parens(),
                )
            }
            StgSyn::Let { bindings, body } => {
                let binding_docs = bindings.iter().enumerate().map(|(i, d)| {
                    allocator
                        .text(format!("[{i}] "))
                        .append(d.pretty(allocator))
                        .group()
                });
                allocator
                    .text("let ")
                    .append(
                        allocator
                            .intersperse(binding_docs, allocator.text(";").append(allocator.line()))
                            .align(),
                    )
                    .append(allocator.line())
                    .append(allocator.text("in "))
                    .append(body.pretty(allocator))
                    .group()
            }
            StgSyn::LetRec { bindings, body } => {
                let binding_docs = bindings.iter().enumerate().map(|(i, d)| {
                    allocator
                        .text(format!("[{i}] "))
                        .append(d.pretty(allocator))
                        .group()
                });
                allocator
                    .text("letrec ")
                    .append(
                        allocator
                            .intersperse(binding_docs, allocator.text(";").append(allocator.line()))
                            .align(),
                    )
                    .append(allocator.line())
                    .append(allocator.text("in "))
                    .append(body.pretty(allocator))
                    .group()
            }
            StgSyn::Ann { smid, body } => allocator
                .text(format!("@{smid} "))
                .append(body.pretty(allocator)),
            StgSyn::Meta { meta, body } => allocator
                .text("`")
                .append(allocator.text(format!("{meta}")))
                .append(allocator.text(" "))
                .append(allocator.text(format!("{body}"))),
            StgSyn::DeMeta {
                scrutinee,
                handler,
                or_else,
            } => {
                let scrutinee_doc = scrutinee.pretty(allocator);
                let handler_doc = allocator
                    .text("meta \u{2192} ")
                    .append(handler.pretty(allocator))
                    .group();
                let else_doc = allocator
                    .text("_ \u{2192} ")
                    .append(or_else.pretty(allocator))
                    .group();

                allocator
                    .text("demeta ")
                    .append(scrutinee_doc)
                    .append(allocator.text(" of"))
                    .append(
                        allocator
                            .line()
                            .append(handler_doc)
                            .append(allocator.line())
                            .append(else_doc)
                            .nest(2),
                    )
                    .group()
            }
            StgSyn::BlackHole => allocator.text("HOLE"),
        }
    }
}
