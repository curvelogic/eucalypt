//! Allow parsed AST to be quote-embedded as eucalypt
use crate::syntax::ast::*;

/// Embed core representation in eucalypt syntax
pub trait Embed {
    /// Represent core expression in a eucalypt AST structure
    fn embed(&self) -> Expression;
}

impl Embed for Literal {
    fn embed(&self) -> Expression {
        match &*self {
            Literal::Sym(_, s) => lit(sym(s)),
            Literal::Str(_, s) => lit(str(&s)),
            Literal::Num(_, n) => lit(num(n.clone())),
        }
    }
}

impl Embed for Name {
    fn embed(&self) -> Expression {
        match &*self {
            Name::Normal(_, s) => list(vec![lit(sym("a-norm")), lit(str(s))]),
            Name::Operator(_, s) => list(vec![lit(sym("a-oper")), lit(str(s))]),
        }
    }
}

fn list_elements(tag: &str, xs: &[impl Embed]) -> Expression {
    let mut elements: Vec<_> = vec![lit(sym(tag))];
    elements.extend(xs.iter().map(|x| x.embed()));
    list(elements)
}

impl Embed for Expression {
    fn embed(&self) -> Expression {
        match &*self {
            Expression::Lit(x) => list(vec![lit(sym("a-lit")), x.embed()]),
            Expression::Block(b) => list(vec![lit(sym("a-block")), b.embed()]),
            Expression::List(_, xs) => list_elements("a-list", xs),
            Expression::OpSoup(_, xs) => list_elements("a-soup", xs),
            Expression::Name(n) => list(vec![lit(sym("a-name")), n.embed()]),
            Expression::StringPattern(_, _) => unimplemented!(),
            Expression::ApplyTuple(_, xs) => list([&[lit(sym("a-applytuple"))], &xs[..]].concat()),
        }
    }
}

fn embed_meta(expr: &Option<Expression>) -> Expression {
    expr.as_ref()
        .map_or(list(vec![lit(sym("a-no-meta"))]), |x| {
            list(vec![lit(sym("a-meta")), x.embed()])
        })
}

impl Embed for ArgTuple {
    fn embed(&self) -> Expression {
        list_elements("a-args", self.names())
    }
}

impl Embed for Declaration {
    fn embed(&self) -> Expression {
        match &*self {
            Declaration::PropertyDeclaration(_, meta, n, expr) => list(vec![
                lit(sym("a-prop")),
                n.embed(),
                expr.embed(),
                embed_meta(meta),
            ]),
            Declaration::FunctionDeclaration(_, meta, n, args, expr) => list(vec![
                lit(sym("a-prop")),
                n.embed(),
                args.embed(),
                expr.embed(),
                embed_meta(meta),
            ]),
            Declaration::InfixOperatorDeclaration(_, meta, l, op, r, expr) => list(vec![
                lit(sym("a-infix")),
                l.embed(),
                op.embed(),
                r.embed(),
                expr.embed(),
                embed_meta(meta),
            ]),
            Declaration::PrefixOperatorDeclaration(_, meta, op, r, expr) => list(vec![
                lit(sym("a-infix")),
                op.embed(),
                r.embed(),
                expr.embed(),
                embed_meta(meta),
            ]),
            Declaration::PostfixOperatorDeclaration(_, meta, l, op, expr) => list(vec![
                lit(sym("a-infix")),
                l.embed(),
                op.embed(),
                expr.embed(),
                embed_meta(meta),
            ]),
        }
    }
}

impl Embed for Block {
    fn embed(&self) -> Expression {
        let mut elements: Vec<_> = vec![];
        elements.extend(self.declarations.iter().map(|x| x.embed()));
        elements.push(embed_meta(&self.metadata));
        list(elements)
    }
}
