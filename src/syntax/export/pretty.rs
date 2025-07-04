//! Pretty printing ASTs in Eucalypt syntax (i.e. writing Eucalypt)
use crate::syntax::ast::*;
use crate::syntax::rowan::lex as lexer;
use pretty::RcDoc;

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
pub fn express_unit(expr: &Expression) -> String {
    let doc = match expr {
        Expression::Block(b) => unit_source_doc(b),
        _ => expr.source_doc(),
    };
    let mut w = Vec::new();
    doc.render(80, &mut w).unwrap();
    String::from_utf8(w).unwrap()
}

/// Rendering AST as Eucalypt source text (without regard for
/// preserving whitespace implied by Spans...)
pub trait ToSourceDoc {
    fn source_doc(&self) -> RcDoc<()>;
}

fn escape_string_literal(s: &str) -> String {
    s.replace('{', "{{").replace('}', "}}")
}

impl ToSourceDoc for Literal {
    fn source_doc(&self) -> RcDoc<()> {
        match self {
            Literal::Sym(_, s) => {
                if is_normal(s) {
                    RcDoc::text(format!(":{s}"))
                } else {
                    RcDoc::text(format!(":'{}'", s))
                }
            }
            Literal::Str(_, s) => RcDoc::text(format!("\"{}\"", escape_string_literal(s))),
            Literal::Num(_, n) => RcDoc::text(format!("{n}")),
        }
    }
}

impl ToSourceDoc for Name {
    fn source_doc(&self) -> RcDoc<()> {
        match self {
            Name::Normal(_, s) => RcDoc::text(s),
            Name::Operator(_, s) => RcDoc::text(s),
        }
    }
}

/// Determine if a string obeys the rules of a normal name
///
/// Used to determine when to single quote identifiers.
fn is_normal(n: &str) -> bool {
    let mut i = n.chars();

    i.next().is_some_and(lexer::is_normal_start) && i.all(lexer::is_normal_continuation)
}

/// Represent a name in contexts which must be normal (non-operator).
///
/// Will single-quote if necessary.
fn quote_to_normal(name: &Name) -> RcDoc<()> {
    match name {
        Name::Normal(_, s) => {
            if is_normal(s) {
                RcDoc::text(s)
            } else {
                RcDoc::text("'")
                    .append(RcDoc::text(s))
                    .append(RcDoc::text("'"))
            }
        }
        Name::Operator(_, s) => RcDoc::text("'")
            .append(RcDoc::text(s))
            .append(RcDoc::text("'")),
    }
}

impl ToSourceDoc for ArgTuple {
    fn source_doc(&self) -> RcDoc<()> {
        RcDoc::text("(")
            .append(RcDoc::intersperse(
                self.names().iter().map(|x| x.source_doc()),
                RcDoc::text(",").append(RcDoc::space()),
            ))
            .append(RcDoc::text(")"))
    }
}

/// Helper for formatting a declaration
fn declaration<'decl>(
    meta: &'decl Option<Expression>,
    head: RcDoc<'decl, ()>,
    expr: &'decl Expression,
) -> RcDoc<'decl, ()> {
    let prefix = match meta {
        Some(m) => RcDoc::text("`")
            .append(RcDoc::space())
            .append(m.source_doc())
            .append(RcDoc::line()),
        None => RcDoc::nil(),
    };

    prefix
        .append(head)
        .append(RcDoc::text(":"))
        .append(RcDoc::space())
        .append(expr.source_doc())
}

impl ToSourceDoc for Declaration {
    fn source_doc(&self) -> RcDoc<()> {
        use crate::syntax::ast::Declaration::*;

        match self {
            PropertyDeclaration(_, m, n, expr) => declaration(m, quote_to_normal(n), expr),
            FunctionDeclaration(_, m, f, xs, expr) => {
                declaration(m, quote_to_normal(f).append(xs.source_doc()), expr)
            }
            InfixOperatorDeclaration(_, m, l, op, r, expr) => declaration(
                m,
                RcDoc::text("(")
                    .append(l.source_doc())
                    .append(RcDoc::space())
                    .append(op.source_doc())
                    .append(RcDoc::space())
                    .append(r.source_doc())
                    .append(RcDoc::text(")")),
                expr,
            ),
            PrefixOperatorDeclaration(_, m, op, r, expr) => declaration(
                m,
                RcDoc::text("(")
                    .append(op.source_doc())
                    .append(RcDoc::space())
                    .append(r.source_doc())
                    .append(RcDoc::text(")")),
                expr,
            ),
            PostfixOperatorDeclaration(_, m, l, op, expr) => declaration(
                m,
                RcDoc::text("(")
                    .append(l.source_doc())
                    .append(RcDoc::space())
                    .append(op.source_doc())
                    .append(RcDoc::text(")")),
                expr,
            ),
        }
    }
}

/// Block content is identical between unit and block
fn block_content(block: &Block) -> RcDoc<()> {
    let prefix = match &block.metadata {
        Some(m) => m.source_doc().append(RcDoc::line()),
        None => RcDoc::nil(),
    };

    let body = RcDoc::intersperse(
        block.declarations.iter().map(|d| d.source_doc()),
        RcDoc::line(),
    )
    .group();

    prefix.append(RcDoc::line()).append(body)
}

impl ToSourceDoc for Block {
    fn source_doc(&self) -> RcDoc<()> {
        let content = block_content(self).nest(2);

        RcDoc::text("{")
            .append(content)
            .append(RcDoc::line())
            .append(RcDoc::text("}"))
            .group()
    }
}

/// The top level block is a unit and doesn't have nesting or braces
pub fn unit_source_doc(block: &Block) -> RcDoc<()> {
    block_content(block)
}

/// Ensure arg tuples are next to function but everything else is WS-separated
fn soup_source_doc(elts: &[Expression]) -> RcDoc<()> {
    let mut doc = RcDoc::nil();
    let mut first = true;

    for elt in elts {
        match elt {
            Expression::ApplyTuple(_, _) => doc = doc.append(elt.source_doc()),
            _ => {
                if first {
                    first = false;
                } else {
                    doc = doc.append(RcDoc::space());
                }
                doc = doc.append(elt.source_doc())
            }
        }
    }

    doc.group()
}

impl ToSourceDoc for Expression {
    fn source_doc(&self) -> RcDoc<()> {
        use crate::syntax::ast::Expression::*;

        match self {
            Lit(lit) => lit.source_doc(),
            Block(block) => block.source_doc(),
            List(_, elts) => RcDoc::text("[")
                .append(
                    RcDoc::intersperse(
                        elts.iter().map(|e| e.source_doc()),
                        RcDoc::text(",").append(RcDoc::space()),
                    )
                    .nest(1)
                    .group(),
                )
                .append(RcDoc::text("]")),
            Name(name) => name.source_doc(),
            StringPattern(_, _) => RcDoc::text("\"TODO\""),
            OpSoup(_, elts) => soup_source_doc(elts),
            ApplyTuple(_, elts) => RcDoc::text("(")
                .append(
                    RcDoc::intersperse(elts.iter().map(|e| e.source_doc()), RcDoc::text(","))
                        .nest(1)
                        .group(),
                )
                .append(RcDoc::text(")")),
        }
    }
}
