//! Export core as structure of eucalypt lists and blocks for
//! processing within eucalypt.
use crate::core::expr::*;
use crate::syntax::ast::*;
use crate::syntax::export::embed::Embed;
use crate::syntax::export::pretty;

/// Embed a representation of the core expression in an AST then
/// render as parse-embed unit.
///
/// The core emitted can be consumed by `parse-embed` functionality
/// which reads such a representation out of the AST to build core,
/// instead of using the normal translation process.
pub fn quote_embed_core_unit(expr: &RcExpr) -> String {
    format!(
        " {{ parse-embed: :CORE }}

` {{ embedding: :core }}
CORE: {}

",
        pretty::express_unit(&expr.embed())
    )
}

impl Embed for CoreExpr {
    fn embed(&self) -> Expression {
        // All expressions are represented as a list with a leading
        // symbol tag identifying the element type.
        let mut elements: Vec<Expression> = Vec::new();
        match self {
            Expr::Var(_, v) => {
                elements.push(lit(sym("c-var")));
                elements.push(lit(str(v.pretty_name().unwrap_or(&"?".to_string()))));
            }
            Expr::Let(_, scope, _) => {
                let (p, t) = scope.clone().unbind();
                let bindings = p.unrec();
                elements.push(lit(sym("c-let")));

                let mut decls: Vec<Declaration> = vec![];
                for (binder, expr) in bindings {
                    let name = binder.0.pretty_name.unwrap();
                    let value = expr.0.embed();
                    decls.push(prop(None, normal(&name), value)); // TODO: ops
                }

                let block = block(None, decls);
                elements.push(Expression::Block(Box::from(block)));
                elements.push(t.embed());
            }
            Expr::Intrinsic(_, n) => {
                elements.push(lit(sym("c-bif")));
                elements.push(lit(sym(n)));
            }
            Expr::Literal(_, x) => {
                elements.push(lit(sym("c-lit")));
                elements.push(x.embed());
            }
            Expr::Lookup(_, e, n, fb) => {
                elements.push(lit(sym("c-lookup")));
                elements.push(e.embed());
                elements.push(lit(str(n)));
                if let Some(x) = fb {
                    elements.push(x.embed());
                }
            }
            Expr::Name(_, n) => {
                elements.push(lit(sym("c-name")));
                elements.push(lit(str(n)));
            }
            Expr::BlockAnaphor(_, anaphor) => {
                elements.push(lit(sym("c-bk-ana")));
                elements.push(lit(str(format!("{}", anaphor))));
            }
            Expr::ExprAnaphor(_, anaphor) => {
                elements.push(lit(sym("c-ex-ana")));
                elements.push(lit(str(format!("{}", anaphor))));
            }
            Expr::List(_, items) => {
                elements.push(lit(sym("c-list")));
                for e in items {
                    elements.push(e.embed());
                }
            }
            Expr::Block(_, block_map) => {
                elements.push(lit(sym("c-block")));

                let mut decls: Vec<Declaration> = vec![];
                for (k, v) in block_map.clone().into_iter() {
                    decls.push(prop(None, normal(&k), v.embed())); //TODO: op name
                }

                let block = block(None, decls);
                elements.push(Expression::Block(Box::from(block)));
            }
            Expr::Meta(_, e, m) => {
                elements.push(lit(sym("c-meta")));
                elements.push(e.embed());
                elements.push(m.embed());
            }
            Expr::ArgTuple(_, args) => {
                elements.push(lit(sym("c-args")));
                for a in args {
                    elements.push(a.embed());
                }
            }
            Expr::Lam(_, _, scope) => {
                elements.push(lit(sym("c-lam")));

                let (binders, t) = scope.clone().unbind();

                let mut args: Vec<Expression> = vec![];
                for binder in binders {
                    let name = binder.0.pretty_name;
                    args.push(lit(str(name.unwrap())));
                }

                let arg_list = list(args);
                elements.push(arg_list);
                elements.push(t.embed());
            }
            Expr::App(_, f, xs) => {
                elements.push(lit(sym("c-app")));
                elements.push(f.embed());

                let mut args: Vec<Expression> = vec![];
                for x in xs {
                    args.push(x.embed());
                }
                elements.push(list(args));
            }
            Expr::Soup(_, xs) => {
                elements.push(lit(sym("c-soup")));
                for x in xs {
                    elements.push(x.embed());
                }
            }
            Expr::Operator(_, f, p, e) => {
                elements.push(lit(sym("c-op")));

                let fixity = match f {
                    Fixity::UnaryPrefix => "unary-prefix",
                    Fixity::UnaryPostfix => "unary-postfix",
                    Fixity::InfixLeft => "infix-left",
                    Fixity::InfixRight => "infix-right",
                };
                elements.push(lit(sym(fixity)));
                elements.push(lit(num(*p)));
                elements.push(e.embed());
            }
            Expr::ErrUnresolved(_, x) => {
                elements.push(lit(sym("e-unresolved")));
                elements.push(lit(str(x)));
            }
            Expr::ErrRedeclaration(_, x) => {
                elements.push(lit(sym("e-redeclaration")));
                elements.push(lit(str(x)));
            }
            Expr::ErrEliminated => {
                elements.push(lit(sym("e-eliminated")));
            }
            Expr::ErrPseudoDot => {
                elements.push(lit(sym("e-pseudodot")));
            }
            Expr::ErrPseudoCall => {
                elements.push(lit(sym("e-pseudocall")));
            }
            Expr::ErrPseudoCat => {
                elements.push(lit(sym("e-pseudocat")));
            }
        }

        list(elements)
    }
}

impl Embed for Primitive {
    fn embed(&self) -> Expression {
        match self {
            Primitive::Str(s) => lit(str(s)),
            Primitive::Sym(s) => lit(sym(s)),
            Primitive::Num(n) => lit(num(n.clone())),
            Primitive::Bool(b) => list(vec![
                lit(sym("c-bool")),
                if *b { lit(sym("t")) } else { lit(sym("f")) },
            ]),
            Primitive::Null => list(vec![lit(sym("c-null"))]),
        }
    }
}

impl Embed for RcExpr {
    fn embed(&self) -> Expression {
        self.inner.embed()
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::syntax::export::pretty;
    use crate::syntax::parser;
    use codespan_reporting::files::SimpleFiles;
    use moniker::FreeVar;

    pub fn parse_expression(txt: &'static str) -> Expression {
        let mut files = SimpleFiles::<String, String>::new();
        let file_id = files.add("test".to_string(), txt.to_string());
        parser::parse_expression(&files, file_id).unwrap()
    }

    #[test]
    pub fn test_simple() {
        let f = FreeVar::fresh_named("f");
        let x = FreeVar::fresh_named("x");

        let core_expr = acore::soup(vec![
            acore::var(f),
            acore::call(),
            acore::arg_tuple(vec![acore::var(x)]),
            acore::dot(),
            acore::name("v"),
        ]);

        let embedding = core_expr.embed();

        let expected = parse_expression(
            "[:c-soup,
					   [:c-var, \"f\"],
					   [:c-op, :infix-left, 90, [:e-pseudocall]],
					   [:c-args, [:c-var, \"x\"]],
					   [:c-op, :infix-left, 90, [:e-pseudodot]],
					   [:c-name, \"v\"]]",
        );

        assert_eq!(pretty::express(&embedding), pretty::express(&expected));
    }

    #[test]
    pub fn test_lambda() {
        let x = FreeVar::fresh_named("x");

        let core_expr = acore::lam(
            vec![x.clone()],
            acore::soup(vec![
                acore::var(x.clone()),
                acore::op(Fixity::InfixLeft, 40, acore::bif("__ADD")),
                acore::var(x),
            ]),
        );

        let embedding = core_expr.embed();

        let expected = parse_expression(
            "[:c-lam, [\"x\"],
					   [:c-soup,
					     [:c-var, \"x\"],
					     [:c-op, :infix-left, 40, [:c-bif, :__ADD]],
					     [:c-var, \"x\"]]]",
        );

        assert_eq!(pretty::express(&embedding), pretty::express(&expected));
    }
}
