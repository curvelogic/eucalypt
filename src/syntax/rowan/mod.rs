//! Rowan based parsing of eucalypt syntax

pub mod ast;
pub mod error;
pub mod kind;
pub mod lex;
pub mod make;
pub mod parse;
pub mod validate;

pub use error::ParseError;
pub use parse::Parser;
use rowan::{ast::AstNode, GreenNode};
use std::marker::PhantomData;

use self::{
    kind::{EucalyptLanguage, SyntaxNode},
    validate::Validatable,
};

/// A parse result
#[derive(Debug, PartialEq, Eq)]
pub struct Parse<T> {
    /// the resulting green tree
    green_node: GreenNode,
    /// syntax errors found during parse
    errors: Vec<ParseError>,
    /// a destination AST type
    _ty: PhantomData<fn() -> T>,
}

impl<T> Clone for Parse<T> {
    fn clone(&self) -> Parse<T> {
        Parse {
            green_node: self.green_node.clone(),
            errors: self.errors.clone(),
            _ty: PhantomData,
        }
    }
}

impl<T> Parse<T> {
    /// Retrieve a SyntaxNode representing the parsed tree
    pub fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    /// Parse errors
    pub fn errors(&self) -> &Vec<ParseError> {
        &self.errors
    }
}

impl<T: AstNode<Language = EucalyptLanguage>> Parse<T> {
    /// Forget the AstNode type disposition and focus purely on SyntaxNode
    pub fn to_syntax(self) -> Parse<SyntaxNode> {
        Parse {
            green_node: self.green_node,
            errors: self.errors,
            _ty: PhantomData,
        }
    }

    /// The root AST node of the parse
    pub fn tree(&self) -> T {
        T::cast(self.syntax_node()).unwrap()
    }

    /// Result as successful parse or errors
    pub fn ok(self) -> Result<T, Vec<ParseError>> {
        if self.errors.is_empty() {
            Ok(self.tree())
        } else {
            Err(self.errors)
        }
    }
}

impl<T> Parse<T>
where
    T: AstNode<Language = EucalyptLanguage>,
    T: Validatable,
{
    /// Validate the tree if required, adding to `errors`
    pub fn validate(&mut self) {
        self.tree().validate(&mut self.errors);
    }
}

/// Parse and validate and expression
pub fn parse_expr(text: &str) -> Parse<ast::Soup> {
    let mut parse = Parser::new(text).parse_expression();
    parse.validate();
    parse
}

/// Parse and validate an entire unit
pub fn parse_unit(text: &str) -> Parse<ast::Unit> {
    let mut parse = Parser::new(text).parse_unit();
    parse.validate();
    parse
}

#[cfg(test)]
mod tests {
    use super::parse_expr;

    #[test]
    pub fn test_expressions() {
        assert!(parse_expr("x f g h").ok().is_ok());
        assert!(parse_expr(" x f g h ").ok().is_ok());
        assert!(parse_expr("#\n x #\n f #\n g #\n h ").ok().is_ok());
        assert!(parse_expr("[:x,]").ok().is_ok());
        assert!(parse_expr("[:x, ]").ok().is_ok());

        assert!(parse_expr("").ok().is_err());
        assert!(parse_expr(" ").ok().is_err());
        assert!(parse_expr("`").ok().is_err());
    }

    #[test]
    pub fn test_blocks() {
        assert!(parse_expr("{}").ok().is_ok());
        assert!(parse_expr("{ x: 3 f }").ok().is_ok());
        assert!(parse_expr("{ x: 3 f y(x): x * 3}").ok().is_ok());
        assert!(parse_expr("{ \"meta\" x: 3 f g h (x && y): x * 3}")
            .ok()
            .is_ok());
        assert!(parse_expr("{ x: 3, y: 4 f, }").ok().is_ok());
        assert!(parse_expr("{ (! x): 3 + x (y %): y}").ok().is_ok());
        assert!(parse_expr("{ x }").ok().is_ok());
        assert!(parse_expr("{ (    ב    ∗  Ъ  )}").ok().is_ok());
        assert!(parse_expr("{ m n o p q r x: y z: f g h i}").ok().is_ok());
        assert!(parse_expr("{ ` m3 z: 877 f}").ok().is_ok());
        assert!(parse_expr("{ MMMM ` m x: y `m3 z: 877 f}").ok().is_ok());
        assert!(
            parse_expr("{ M M M M M ` m m m m m (x £ y): y1 y2 y3 y4 y5 }")
                .ok()
                .is_ok()
        );
        assert!(parse_expr("{ α: [:x] = [:x, ] }").ok().is_ok());
        assert!(parse_expr("{ α: [:x, :y, :z] = [:x, :y, :z, ] }")
            .ok()
            .is_ok());

        // various error cases
        assert!(parse_expr("{ x: }").ok().is_err());
        assert!(parse_expr("{ x: 3, 3 }").ok().is_err());
        assert!(parse_expr("{ x: 3: 3 }").ok().is_err());
        assert!(parse_expr("{ x: 3").ok().is_err());
        assert!(parse_expr("x: 3}").ok().is_err());
        assert!(parse_expr("{ x: a b c, d e f, y: 3}").ok().is_err());
        assert!(parse_expr("{ (x & &): 3}").ok().is_err());
        assert!(parse_expr("{ (x & y && z): 3}").ok().is_err());
        assert!(parse_expr("{ (x & y z): 3}").ok().is_err());
        assert!(parse_expr("{ ((x) & (y).ok().is_err()): 3}").ok().is_err());
        assert!(parse_expr("{ x: 3 ` }").ok().is_err());
        assert!(parse_expr("{ x: ` 4 }").ok().is_err());
        assert!(parse_expr("{ ` ` x: 4 }").ok().is_err());

        // nested blocks
        assert!(parse_expr("{ x: { y: value } }").ok().is_ok());
        assert!(parse_expr("{ { m: v} x: { y: value } }").ok().is_ok());
        assert!(parse_expr("{ ` { m: v } x: { y: value } }").ok().is_ok());
        assert!(parse_expr("{ { m: v } ` { m: v } x: { y: value } }")
            .ok()
            .is_ok());
        assert!(
            parse_expr("{ { m: v } ` { ` :m m: v } x: { ` \"m\" y: value } }")
                .ok()
                .is_ok()
        );
        assert!(
            parse_expr("{ { m: v } ` { ` :m m: v } x: { ` \"m\" y: value } }")
                .ok()
                .is_ok()
        );
        assert!(parse_expr("{ x: { y: { z: { f(x): x }} } }").ok().is_ok());
        assert!(parse_expr("{ { k: v } : v  } }").ok().is_err());

        // comments
        assert!(parse_expr("{f( #\n x): x}").ok().is_ok());
        assert!(parse_expr("{f(x #\n): x}").ok().is_ok());
        assert!(parse_expr("{f(x): x #\n}").ok().is_ok());
        dbg!(parse_expr("{ #\n f( #\n x #\n )#\n   : x}").errors());
        assert!(parse_expr("{ #\n f( #\n x #\n )#\n   : x}").ok().is_ok());
    }
}
