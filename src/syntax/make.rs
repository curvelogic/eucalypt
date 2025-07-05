//! Helpers for constructing synthetic AST nodes

use rowan::ast::AstNode;
use serde_json::Number;

use crate::syntax::{ast::{Literal, Name, Soup, Unit}, parse::parse_expr, kind::EucalyptLanguage};

/// Construct a new AstNode of type T from the supplied text
fn ast_from_text<N: AstNode<Language = EucalyptLanguage>, T: AsRef<str>>(text: T) -> N {
    let soup = parse_expr(text.as_ref()).ok().unwrap();
    let node = match soup.syntax().descendants().find_map(N::cast) {
        Some(it) => it,
        None => {
            panic!(
                "Failed to make ast node `{}` from text {}",
                std::any::type_name::<N>(),
                text.as_ref()
            )
        }
    };
    let node = node.clone_subtree();
    assert_eq!(node.syntax().text_range().start(), 0.into());
    node
}

/// Construct a symbol literal
pub fn sym<T: AsRef<str>>(name: T) -> Literal {
    let text = name.as_ref();
    ast_from_text::<Literal, _>(format!(":{text}"))
}

/// Construct a string literal
pub fn str<T: AsRef<str>>(content: T) -> Literal {
    let text = content.as_ref();
    ast_from_text::<Literal, _>(format!("\"{text}\""))
}

/// Construct a number
pub fn num<T: Into<Number>>(val: T) -> Literal {
    let numval = val.into();
    ast_from_text::<Literal, _>(format!("{numval}"))
}

/// Construct an identifier
pub fn name<T: AsRef<str>>(name: T) -> Name {
    ast_from_text::<Name, _>(name)
}

/// Construct an arbitrary expression
pub fn expression<T: AsRef<str>>(exp: T) -> Soup {
    ast_from_text::<Soup, _>(exp)
}

/// Construct a eucaltyp unit
pub fn unit<T: AsRef<str>>(unit: T) -> Unit {
    ast_from_text::<Unit, _>(unit)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    pub fn test_literals() {
        let s = sym("foo");
        assert_eq!(
            s.value()
                .and_then(|v| v.symbol_name().map(ToString::to_string)),
            Some("foo".to_string())
        );

        let t = str("bar");
        assert_eq!(
            t.value()
                .and_then(|v| v.string_value().map(ToString::to_string)),
            Some("bar".to_string())
        );

        let n = num(988u64);
        assert_eq!(
            n.value().and_then(|v| v.number_value()).unwrap(),
            Number::from(988)
        );
    }
}
