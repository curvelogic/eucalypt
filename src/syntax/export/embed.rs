//! Allow parsed AST to be quote-embedded as eucalypt
use crate::syntax::ast::*;
use crate::syntax::rowan::ast as rowan;
use crate::syntax::rowan::ast::{AstToken, HasSoup};

/// Embed core representation in eucalypt syntax
pub trait Embed {
    /// Represent core expression in a eucalypt AST structure
    fn embed(&self) -> Expression;
}

impl Embed for Literal {
    fn embed(&self) -> Expression {
        match self {
            Literal::Sym(_, s) => lit(sym(s)),
            Literal::Str(_, s) => lit(str(s)),
            Literal::Num(_, n) => lit(num(n.clone())),
        }
    }
}

impl Embed for Name {
    fn embed(&self) -> Expression {
        match self {
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
        match self {
            Expression::Lit(x) => list(vec![lit(sym("a-lit")), x.embed()]),
            Expression::Block(b) => list(vec![lit(sym("a-block")), b.embed()]),
            Expression::List(_, xs) => list_elements("a-list", xs),
            Expression::OpSoup(_, xs) => list_elements("a-soup", xs),
            Expression::Name(n) => list(vec![lit(sym("a-name")), n.embed()]),
            Expression::StringPattern(_, chunks) => list_elements("a-string-pattern", chunks),
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
        match self {
            Declaration::PropertyDeclaration(_, meta, n, expr) => list(vec![
                lit(sym("a-prop")),
                n.embed(),
                expr.embed(),
                embed_meta(meta),
            ]),
            Declaration::FunctionDeclaration(_, meta, n, args, expr) => list(vec![
                lit(sym("a-func")),
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
                lit(sym("a-prefix")),
                op.embed(),
                r.embed(),
                expr.embed(),
                embed_meta(meta),
            ]),
            Declaration::PostfixOperatorDeclaration(_, meta, l, op, expr) => list(vec![
                lit(sym("a-postfix")),
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

impl Embed for InterpolationTarget {
    fn embed(&self) -> Expression {
        match self {
            InterpolationTarget::StringAnaphor(_, index) => {
                if let Some(idx) = index {
                    list(vec![lit(sym("a-str-anaphor")), lit(num(*idx))])
                } else {
                    list(vec![lit(sym("a-str-anaphor")), lit(sym("a-no-index"))])
                }
            }
            InterpolationTarget::Reference(_, names) => {
                list_elements("a-str-reference", names)
            }
        }
    }
}

impl Embed for InterpolationRequest {
    fn embed(&self) -> Expression {
        let target = self.target.embed();
        let format = if let Some(fmt) = &self.format {
            lit(str(fmt))
        } else {
            lit(sym("a-no-format"))
        };
        let conversion = if let Some(conv) = &self.conversion {
            lit(str(conv))
        } else {
            lit(sym("a-no-conversion"))
        };
        
        list(vec![lit(sym("a-str-interp")), target, format, conversion])
    }
}

impl Embed for StringChunk {
    fn embed(&self) -> Expression {
        match self {
            StringChunk::LiteralContent(_, text) => {
                list(vec![lit(sym("a-str-literal")), lit(str(text))])
            }
            StringChunk::Interpolation(_, request) => {
                request.embed()
            }
        }
    }
}

//
// Rowan AST Embed implementations
//

impl Embed for rowan::LiteralValue {
    fn embed(&self) -> Expression {
        match self {
            rowan::LiteralValue::Sym(s) => {
                if let Some(name) = s.value() {
                    list(vec![lit(sym("a-sym")), lit(str(name))])
                } else {
                    list(vec![lit(sym("a-sym")), lit(str(""))])
                }
            }
            rowan::LiteralValue::Str(s) => {
                if let Some(content) = s.value() {
                    list(vec![lit(sym("a-str")), lit(str(content))])
                } else {
                    list(vec![lit(sym("a-str")), lit(str(""))])
                }
            }
            rowan::LiteralValue::Num(n) => {
                if let Some(number) = n.value() {
                    list(vec![lit(sym("a-num")), lit(num(number))])
                } else {
                    list(vec![lit(sym("a-num")), lit(num(0))])
                }
            }
        }
    }
}

impl Embed for rowan::Literal {
    fn embed(&self) -> Expression {
        if let Some(value) = self.value() {
            list(vec![lit(sym("a-literal")), value.embed()])
        } else {
            list(vec![lit(sym("a-literal")), lit(sym("a-invalid"))])
        }
    }
}

impl Embed for rowan::NormalIdentifier {
    fn embed(&self) -> Expression {
        list(vec![lit(sym("a-norm")), lit(str(self.text()))])
    }
}

impl Embed for rowan::OperatorIdentifier {
    fn embed(&self) -> Expression {
        list(vec![lit(sym("a-oper")), lit(str(self.text()))])
    }
}

impl Embed for rowan::Identifier {
    fn embed(&self) -> Expression {
        match self {
            rowan::Identifier::NormalIdentifier(id) => id.embed(),
            rowan::Identifier::OperatorIdentifier(id) => id.embed(),
        }
    }
}

impl Embed for rowan::Name {
    fn embed(&self) -> Expression {
        if let Some(identifier) = self.identifier() {
            list(vec![lit(sym("a-name")), identifier.embed()])
        } else {
            list(vec![lit(sym("a-name")), lit(sym("a-invalid"))])
        }
    }
}

impl Embed for rowan::Soup {
    fn embed(&self) -> Expression {
        let elements: Vec<_> = self.elements().map(|e| e.embed()).collect();
        let mut result = vec![lit(sym("a-soup"))];
        result.extend(elements);
        list(result)
    }
}

impl Embed for rowan::ParenExpr {
    fn embed(&self) -> Expression {
        if let Some(soup) = self.soup() {
            list(vec![lit(sym("a-paren-expr")), soup.embed()])
        } else {
            list(vec![lit(sym("a-paren-expr")), list(vec![lit(sym("a-soup"))])])
        }
    }
}

impl Embed for rowan::List {
    fn embed(&self) -> Expression {
        let items: Vec<_> = self.items().map(|item| item.embed()).collect();
        let mut result = vec![lit(sym("a-list"))];
        result.extend(items);
        list(result)
    }
}

impl Embed for rowan::ApplyTuple {
    fn embed(&self) -> Expression {
        let items: Vec<_> = self.items().map(|soup| {
            // If the soup has a single element, unwrap it like LALRPOP does
            if let Some(elem) = soup.singleton() {
                elem.embed()
            } else {
                soup.embed()
            }
        }).collect();
        let mut result = vec![lit(sym("a-applytuple"))];
        result.extend(items);
        list(result)
    }
}

impl Embed for rowan::StringChunk {
    fn embed(&self) -> Expression {
        match self {
            rowan::StringChunk::LiteralContent(content) => {
                if let Some(text) = content.value() {
                    list(vec![lit(sym("a-str-literal")), lit(str(&text))])
                } else {
                    list(vec![lit(sym("a-str-literal")), lit(str(""))])
                }
            }
            rowan::StringChunk::Interpolation(interp) => {
                let target = if let Some(target) = interp.target() {
                    if let Some(text) = target.value() {
                        lit(str(&text))
                    } else {
                        lit(str(""))
                    }
                } else {
                    lit(str(""))
                };
                
                let format = if let Some(format) = interp.format_spec() {
                    if let Some(text) = format.value() {
                        lit(str(&text))
                    } else {
                        lit(sym("a-no-format"))
                    }
                } else {
                    lit(sym("a-no-format"))
                };
                
                let conversion = if let Some(conversion) = interp.conversion_spec() {
                    if let Some(text) = conversion.value() {
                        lit(str(&text))
                    } else {
                        lit(sym("a-no-conversion"))
                    }
                } else {
                    lit(sym("a-no-conversion"))
                };
                
                list(vec![lit(sym("a-str-interp")), target, format, conversion])
            }
            rowan::StringChunk::EscapedOpen(_) => {
                list(vec![lit(sym("a-str-esc-open"))])
            }
            rowan::StringChunk::EscapedClose(_) => {
                list(vec![lit(sym("a-str-esc-close"))])
            }
        }
    }
}

impl Embed for rowan::StringPattern {
    fn embed(&self) -> Expression {
        let chunks: Vec<_> = self.chunks().map(|chunk| chunk.embed()).collect();
        let mut result = vec![lit(sym("a-string-pattern"))];
        result.extend(chunks);
        list(result)
    }
}


impl Embed for rowan::Element {
    fn embed(&self) -> Expression {
        match self {
            rowan::Element::Lit(literal) => literal.embed(),
            rowan::Element::Block(block) => block.embed(),
            rowan::Element::List(list) => list.embed(),
            rowan::Element::ParenExpr(paren) => paren.embed(),
            rowan::Element::Name(name) => name.embed(),
            rowan::Element::StringPattern(pattern) => pattern.embed(),
            rowan::Element::ApplyTuple(tuple) => tuple.embed(),
        }
    }
}

// Helper function to embed optional metadata
fn embed_rowan_meta(meta: Option<rowan::BlockMetadata>) -> Expression {
    if let Some(metadata) = meta {
        if let Some(soup) = metadata.soup() {
            list(vec![lit(sym("a-meta")), soup.embed()])
        } else {
            list(vec![lit(sym("a-no-meta"))])
        }
    } else {
        list(vec![lit(sym("a-no-meta"))])
    }
}

fn embed_rowan_decl_meta(meta: Option<rowan::DeclarationMetadata>) -> Expression {
    if let Some(metadata) = meta {
        if let Some(soup) = metadata.soup() {
            list(vec![lit(sym("a-meta")), soup.embed()])
        } else {
            list(vec![lit(sym("a-no-meta"))])
        }
    } else {
        list(vec![lit(sym("a-no-meta"))])
    }
}

impl Embed for rowan::Declaration {
    fn embed(&self) -> Expression {
        // For now, we embed the declaration structure generically
        // A more sophisticated implementation would classify the declaration kind
        let meta = embed_rowan_decl_meta(self.meta());
        
        let head = if let Some(_head) = self.head() {
            // We'd need to analyze the head to determine the declaration type
            // For now, just embed as generic head
            list(vec![lit(sym("a-decl-head")), lit(str("generic"))])
        } else {
            list(vec![lit(sym("a-decl-head")), lit(str("missing"))])
        };
        
        let body = if let Some(body) = self.body() {
            if let Some(soup) = body.soup() {
                soup.embed()
            } else {
                list(vec![lit(sym("a-soup"))])
            }
        } else {
            list(vec![lit(sym("a-soup"))])
        };
        
        list(vec![
            lit(sym("a-declaration")),
            head,
            body,
            meta,
        ])
    }
}

impl Embed for rowan::Block {
    fn embed(&self) -> Expression {
        let mut elements = vec![lit(sym("a-block"))];
        
        // Add all declarations
        for decl in self.declarations() {
            elements.push(decl.embed());
        }
        
        // Add metadata
        elements.push(embed_rowan_meta(self.meta()));
        
        list(elements)
    }
}

impl Embed for rowan::Unit {
    fn embed(&self) -> Expression {
        let mut elements = vec![lit(sym("a-unit"))];
        
        // Add all declarations
        for decl in self.declarations() {
            elements.push(decl.embed());
        }
        
        // Add metadata
        elements.push(embed_rowan_meta(self.meta()));
        
        list(elements)
    }
}
