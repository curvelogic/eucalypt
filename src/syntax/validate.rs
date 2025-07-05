//! Validation of lossless AST nodes

use rowan::ast::AstNode;

use super::{ast::*, error::ParseError};

/// Trait for `AstNode`s and `AstToken`s that may be created for
/// invalid source text.
///
/// If a validation completes with no errors, translation to core
/// syntax must not fail for any syntactic reason.
pub trait Validatable {
    /// Validate this node or token, creating errors in `errors`
    fn validate(&self, _errors: &mut Vec<ParseError>) {}
}

mod support {
    use crate::syntax::{
        kind::{SyntaxKind, SyntaxNode},
        error::ParseError,
    };

    pub fn no_error_children(node: &SyntaxNode, errors: &mut Vec<ParseError>) {
        for child in node.children_with_tokens() {
            match child.kind() {
                SyntaxKind::RESERVED_OPEN
                | SyntaxKind::RESERVED_CLOSE
                | SyntaxKind::ERROR_RESERVED_CHAR => errors.push(ParseError::ReservedCharacter {
                    range: child.text_range(),
                }),
                SyntaxKind::ERROR_STOWAWAYS => errors.push(ParseError::SurplusContent {
                    range: child.text_range(),
                }),
                _ => {}
            }
        }
    }

    pub fn non_empty(node: &SyntaxNode, errors: &mut Vec<ParseError>) {
        for child in node.children() {
            if !child.kind().is_trivial() {
                return;
            }
        }
        errors.push(ParseError::EmptyExpression {
            range: node.text_range(),
        });
    }
}

impl Validatable for Sym {
    /// If a symbol is quoted, it must be quoted correctly
    fn validate(&self, errors: &mut Vec<ParseError>) {
        if self.text().starts_with('\'') && !self.text().ends_with('\'') {
            errors.push(ParseError::UnclosedSingleQuote {
                range: self.syntax().text_range(),
            })
        }
    }
}

impl Validatable for Str {
    /// String quotes must be closed
    fn validate(&self, errors: &mut Vec<ParseError>) {
        if self.text().starts_with('"') && !self.text().ends_with('"') {
            errors.push(ParseError::UnclosedDoubleQuote {
                range: self.syntax().text_range(),
            })
        }
    }
}

impl Validatable for Num {}

impl Validatable for LiteralValue {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        match self {
            LiteralValue::Num(n) => n.validate(errors),
            LiteralValue::Str(s) => s.validate(errors),
            LiteralValue::Sym(s) => s.validate(errors),
        }
    }
}

impl Validatable for Literal {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        if let Some(value) = self.value() {
            value.validate(errors);
        } else {
            panic!("parse generated empty literal")
        }
    }
}

impl Validatable for UnquotedIdentifier {}

impl Validatable for OperatorIdentifier {}

impl Validatable for SingleQuoteIdentifier {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        if self.text().starts_with('\'') && !self.text().ends_with('\'') {
            errors.push(ParseError::UnclosedSingleQuote {
                range: self.syntax().text_range(),
            })
        }
    }
}

impl Validatable for NormalIdentifier {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        match self {
            NormalIdentifier::UnquotedIdentifier(id) => id.validate(errors),
            NormalIdentifier::SingleQuoteIdentifier(id) => id.validate(errors),
        }
    }
}

impl Validatable for Identifier {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        match self {
            Identifier::NormalIdentifier(id) => id.validate(errors),
            Identifier::OperatorIdentifier(id) => id.validate(errors),
        }
    }
}

impl Validatable for Name {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        if let Some(value) = self.identifier() {
            value.validate(errors);
        } else {
            panic!("parse generated empty name")
        }
    }
}

impl Validatable for Soup {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        for element in self.elements() {
            element.validate(errors);
        }
        support::no_error_children(self.syntax(), errors);
        support::non_empty(self.syntax(), errors);
    }
}

impl Validatable for ParenExpr {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        if self.close_paren().is_none() {
            // we parse reserved character pairs (e.g. "« a b c »") as
            // paren exprs so this may indicate unclosed parens or
            // non-standard reserved character exprs
            errors.push(ParseError::InvalidParenExpr {
                open_paren_range: self.open_paren().map(|x| x.text_range()),
                range: self.syntax().text_range(),
            })
        }

        if let Some(s) = self.soup() {
            s.validate(errors);
        }

        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for DeclarationMetadata {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        if let Some(s) = self.soup() {
            s.validate(errors);
        }
        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for DeclarationHead {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        let mut dk = self.classify_declaration();
        if let DeclarationKind::MalformedHead(ref mut es) = dk {
            errors.append(es);
        }

        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for DeclarationBody {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        if let Some(s) = self.soup() {
            s.validate(errors)
        } else {
            errors.push(ParseError::EmptyDeclarationBody {
                range: self.syntax().text_range(),
            })
        }

        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for Declaration {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        if let Some(m) = self.meta() {
            m.validate(errors)
        }

        if let Some(h) = self.head() {
            h.validate(errors);
            if self.colon().is_none() {
                errors.push(ParseError::MissingDeclarationColon {
                    range: h.syntax().text_range(),
                })
            }
        } else {
            panic!("parse created headless declaration");
        }

        if let Some(b) = self.body() {
            b.validate(errors)
        } else {
            errors.push(ParseError::EmptyDeclarationBody {
                range: self.syntax().text_range(),
            })
        }

        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for BlockMetadata {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        if let Some(soup) = self.soup() {
            soup.validate(errors);
        }

        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for Block {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        if let Some(m) = self.meta() {
            m.validate(errors);
        }
        for d in self.declarations() {
            d.validate(errors);
        }
        if self.close_brace().is_none() {
            errors.push(ParseError::UnterminatedBlock {
                range: self.syntax().text_range(),
            });
        }

        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for Unit {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        if let Some(m) = self.meta() {
            m.validate(errors);
        }

        for d in self.declarations() {
            d.validate(errors);
        }

        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for ApplyTuple {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        for item in self.items() {
            item.validate(errors);
        }
        if self.close_paren().is_none() {
            errors.push(ParseError::InvalidParenExpr {
                open_paren_range: self.open_paren().map(|op| op.text_range()),
                range: self.syntax().text_range(),
            })
        }

        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for List {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        for item in self.items() {
            item.validate(errors);
        }

        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for Element {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        match self {
            Element::Lit(lit) => lit.validate(errors),
            Element::Block(b) => b.validate(errors),
            Element::List(xs) => xs.validate(errors),
            Element::ParenExpr(pe) => pe.validate(errors),
            Element::Name(n) => n.validate(errors),
            Element::StringPattern(s) => s.validate(errors),
            Element::ApplyTuple(t) => t.validate(errors),
        }
    }
}

// String pattern validation implementations
impl Validatable for StringPattern {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for StringLiteralContent {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for StringInterpolation {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for StringInterpolationTarget {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for StringFormatSpec {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for StringConversionSpec {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for StringEscapedOpen {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        support::no_error_children(self.syntax(), errors);
    }
}

impl Validatable for StringEscapedClose {
    fn validate(&self, errors: &mut Vec<ParseError>) {
        support::no_error_children(self.syntax(), errors);
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use matches::assert_matches;
    use rowan::{ast::AstNode, TextRange};

    use crate::syntax::{kind::SyntaxKind, parse::parse_unit, error::ParseError};

    use super::Validatable;

    fn check<T: AsRef<str>>(text: T) -> Vec<ParseError> {
        let parse = parse_unit(text.as_ref());
        match parse.ok() {
            Ok(p) => {
                println!("{:#?}", p.syntax());
                vec![]
            }
            Err(e) => e.clone(),
        }
    }

    #[test]
    pub fn test_rubbish() {
        let errors = check("B(*&TBG*^£KJH");
        assert_matches!(errors.first(), Some(ParseError::InvalidParenExpr { .. }));
    }

    #[test]
    pub fn test_unexpected_close_braces() {
        let errors = check("}}}");
        assert_matches!(errors.first(), Some(ParseError::SurplusContent { .. }));
    }

    #[test]
    pub fn test_mismatched_brackets() {
        let errors = check("{(})");
        assert_eq!(
            errors,
            vec![
                ParseError::UnexpectedToken {
                    expected: SyntaxKind::CLOSE_PAREN,
                    actual: SyntaxKind::CLOSE_BRACE,
                    range: TextRange::new(2.into(), 3.into())
                },
                ParseError::InvalidParenExpr {
                    open_paren_range: Some(TextRange::new(1.into(), 2.into())),
                    range: TextRange::new(1.into(), 2.into())
                },
                ParseError::EmptyExpression {
                    range: TextRange::new(2.into(), 2.into())
                },
                ParseError::SurplusContent {
                    range: TextRange::new(3.into(), 4.into())
                }
            ]
        );
    }

    #[test]
    pub fn test_exotic_quotes() {
        let errors = check("« a b c »");
        assert_eq!(
            errors,
            vec![
                ParseError::InvalidParenExpr {
                    open_paren_range: None,
                    range: TextRange::new(0.into(), 11.into())
                },
                ParseError::ReservedCharacter {
                    range: TextRange::new(0.into(), 2.into())
                },
                ParseError::ReservedCharacter {
                    range: TextRange::new(9.into(), 11.into())
                }
            ]
        );
    }

    #[test]
    pub fn test_malformed_block() {
        let errors = check("{ foo: (bar }");
        assert_eq!(
            errors,
            vec![
                ParseError::UnexpectedToken {
                    expected: SyntaxKind::CLOSE_PAREN,
                    actual: SyntaxKind::CLOSE_BRACE,
                    range: TextRange::new(12.into(), 13.into())
                },
                ParseError::InvalidParenExpr {
                    open_paren_range: Some(TextRange::new(7.into(), 8.into())),
                    range: TextRange::new(7.into(), 12.into())
                }
            ]
        );
    }

    #[test]
    pub fn test_bad_paren_commas() {
        let errors = check("{ ` x y z foo: (,) }");
        assert_eq!(
            errors,
            vec![
                ParseError::UnexpectedToken {
                    expected: SyntaxKind::CLOSE_PAREN,
                    actual: SyntaxKind::COMMA,
                    range: TextRange::new(16.into(), 17.into())
                },
                ParseError::UnexpectedToken {
                    expected: SyntaxKind::CLOSE_BRACE,
                    actual: SyntaxKind::CLOSE_PAREN,
                    range: TextRange::new(17.into(), 18.into())
                },
                ParseError::InvalidParenExpr {
                    open_paren_range: Some(TextRange::new(15.into(), 16.into())),
                    range: TextRange::new(15.into(), 16.into())
                },
                ParseError::EmptyExpression {
                    range: TextRange::new(16.into(), 16.into())
                },
                ParseError::UnterminatedBlock {
                    range: TextRange::new(0.into(), 17.into()),
                },
                ParseError::SurplusContent {
                    range: TextRange::new(17.into(), 20.into())
                }
            ]
        );
    }

    #[test]
    pub fn test_bad_block_commas() {
        let errors = check("foo: { x: y, z}");
        assert_eq!(
            errors,
            vec![ParseError::SurplusContent {
                range: TextRange::new(12.into(), 14.into())
            }]
        );
    }

    #[test]
    pub fn test_empty_paren() {
        let errors = check(" x: () ");
        assert_eq!(
            errors,
            vec![ParseError::EmptyExpression {
                range: TextRange::new(5.into(), 5.into())
            }]
        );
    }

    #[test]
    pub fn test_full_declaration() {
        let errors = check(
            r#"` :suppress
(l||r): __OR(l,r)"#,
        );
        assert_eq!(errors, vec![]);
    }

    fn check_eucalypt_file(path: &Path) {
        let text = std::fs::read_to_string(path).unwrap();
        let parse = parse_unit(&text);
        let mut errors = parse.errors().clone();
        parse.tree().validate(&mut errors);
        if !errors.is_empty() {
            print!(
                "
{:#?}",
                parse.syntax_node()
            );
            assert_eq!(errors, vec![]);
        }
    }

    #[test]
    pub fn check_harness_eucalypt_files() {
        for entry in std::fs::read_dir(Path::new("harness/test"))
            .unwrap()
            .flatten()
        {
            let path = entry.path();
            if path.is_file() && path.extension().unwrap_or_default() == "eu" {
                println!("checking {}", path.display());
                check_eucalypt_file(&path);
            }
        }
    }
}
