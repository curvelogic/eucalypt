use crate::syntax::ast::{Block, Expression};
use crate::syntax::error::ParserError;

use codespan_reporting::files::SimpleFiles;

use super::ast::ArgTuple;

/// Parse a unit into an AST Block
pub fn parse_unit<N, T>(files: &SimpleFiles<N, T>, id: usize) -> Result<Block, ParserError>
where
    N: AsRef<str>,
    N: Clone,
    N: std::fmt::Display,
    T: AsRef<str>,
{
    crate::syntax::compat::parse_unit_compat(files, id)
}

/// Parse an expression into an AST block
pub fn parse_expression<N, T>(
    files: &SimpleFiles<N, T>,
    id: usize,
) -> Result<Expression, ParserError>
where
    N: AsRef<str>,
    N: Clone,
    N: std::fmt::Display,
    T: AsRef<str>,
{
    crate::syntax::compat::parse_expression_compat(files, id)
}

/// Parse an expression into an AST block
pub fn parse_embedded_lambda<N, T>(
    files: &SimpleFiles<N, T>,
    id: usize,
) -> Result<(ArgTuple, Expression), ParserError>
where
    N: AsRef<str>,
    N: Clone,
    N: std::fmt::Display,
    T: AsRef<str>,
{
    crate::syntax::compat::parse_embedded_lambda_compat(files, id)
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use codespan_reporting::files::SimpleFiles;

    fn accepts_expr(txt: &str) -> bool {
        let mut files = SimpleFiles::new();
        let file_id = files.add("test".to_string(), txt.to_string());
        parse_expression(&files, file_id).is_ok()
    }

    fn rejects_expr(txt: &str) -> bool {
        let mut files = SimpleFiles::new();
        let file_id = files.add("test".to_string(), txt.to_string());
        parse_expression(&files, file_id).is_err()
    }

    fn accepts_unit(txt: &str) -> bool {
        let mut files = SimpleFiles::new();
        let file_id = files.add("test".to_string(), txt.to_string());
        parse_unit(&files, file_id).is_ok()
    }

    fn rejects_unit(txt: &str) -> bool {
        let mut files = SimpleFiles::new();
        let file_id = files.add("test".to_string(), txt.to_string());
        parse_unit(&files, file_id).is_err()
    }

    #[test]
    fn test_literals() {
        // Symbols
        assert!(accepts_expr(":foo"));
        assert!(accepts_expr(":'bar'"));
        assert!(accepts_expr(":'~*&^%'"));
        
        // Strings
        assert!(accepts_expr("\"foo\""));
        assert!(accepts_expr("\"إ\""));
        
        // String patterns with interpolation
        assert!(accepts_expr("\"{0}\""));
        assert!(accepts_expr("\"{}{}\""));
        
        // Numbers
        assert!(accepts_expr("-1234.1234"));
        assert!(accepts_expr("-1234"));
        assert!(accepts_expr("999"));
    }

    #[test]
    fn test_identifiers() {
        // Normal identifiers
        assert!(accepts_expr("xyz"));
        assert!(accepts_expr("?xyz"));
        assert!(accepts_expr("a-b-c?"));
        assert!(accepts_expr("•1"));
        assert!(accepts_expr("__BIF"));
        assert!(accepts_expr("_1"));
        
        // Operators
        assert!(accepts_expr("∨"));
        assert!(accepts_expr("∘"));
        assert!(accepts_expr("&&"));
        assert!(accepts_expr("-"));
        
        // Quoted identifiers
        assert!(accepts_expr("'asdf'"));
        assert!(accepts_expr("'::||\\t||::'"));
    }

    #[test]
    fn test_expressions() {
        // Simple expressions
        assert!(accepts_expr("x"));
        assert!(accepts_expr("3.3"));
        
        // Parenthesized expressions
        assert!(accepts_expr("(2 + 3)"));
        assert!(accepts_expr("(2+3)"));
        
        // Lists
        assert!(accepts_expr("[x, y, z]"));
        assert!(accepts_expr("[:x,]"));
        assert!(accepts_expr("[:x, ]"));
        
        // Function application
        assert!(accepts_expr("f(x)"));
        
        // Complex expressions
        assert!(accepts_expr("x f g h"));
        assert!(accepts_expr(" x f g h "));
        assert!(accepts_expr("#\n x #\n f #\n g #\n h "));
        
        // Invalid expressions
        assert!(rejects_expr(""));
        assert!(rejects_expr("`"));
    }

    #[test]
    fn test_blocks() {
        // Simple blocks
        assert!(accepts_expr("{}"));
        assert!(accepts_expr("{x: 3}"));
        assert!(accepts_expr("{x: 3 y: 4}"));
        
        // Function declarations
        assert!(accepts_expr("{f(x, y): x + 3}"));
        
        // Operator declarations
        assert!(accepts_expr("{ (x &&& y): y }"));
        assert!(accepts_expr("{(! y): y}"));
        assert!(accepts_expr("{ ( y * ): y }"));
        
        // Complex blocks
        assert!(accepts_expr("{ x: 3 f }"));
        assert!(accepts_expr("{ x: 3 f y(x): x * 3}"));
        assert!(accepts_expr("{ \"meta\" x: 3 f g h (x && y): x * 3}"));
        assert!(accepts_expr("{ x: 3, y: 4 f, }"));
        assert!(accepts_expr("{ (! x): 3 + x (y %): y}"));
        assert!(accepts_expr("{ x }"));
        assert!(accepts_expr("{ (    ב    ∗  Ъ  )}"));
        assert!(accepts_expr("{ m n o p q r x: y z: f g h i}"));
        assert!(accepts_expr("{ ` m3 z: 877 f}"));
        assert!(accepts_expr("{ MMMM ` m x: y `m3 z: 877 f}"));
        assert!(accepts_expr("{ M M M M M ` m m m m m (x £ y): y1 y2 y3 y4 y5 }"));
        assert!(accepts_expr("{ α: [:x] = [:x, ] }"));
        assert!(accepts_expr("{ α: [:x, :y, :z] = [:x, :y, :z, ] }"));

        // Nested blocks
        assert!(accepts_expr("{ x: { y: value } }"));
        assert!(accepts_expr("{ { m: v} x: { y: value } }"));
        assert!(accepts_expr("{ ` { m: v } x: { y: value } }"));
        assert!(accepts_expr("{ { m: v } ` { m: v } x: { y: value } }"));
        assert!(accepts_expr("{ { m: v } ` { ` :m m: v } x: { ` \"m\" y: value } }"));
        assert!(accepts_expr("{ x: { y: { z: { f(x): x }} } }"));

        // Comments
        assert!(accepts_expr("{f( #\n x): x}"));
        assert!(accepts_expr("{f(x #\n): x}"));
        assert!(accepts_expr("{f(x): x #\n}"));
        assert!(accepts_expr("{ #\n f( #\n x #\n )#\n   : x}"));

        // Error cases that should be rejected
        assert!(rejects_expr("{ x: }"));
        assert!(rejects_expr("{ x: 3, 3 }"));
        assert!(rejects_expr("{ x: 3: 3 }"));
        assert!(rejects_expr("{ x: 3"));
        assert!(rejects_expr("x: 3}"));
        assert!(rejects_expr("{ x: a b c, d e f, y: 3}"));
        assert!(rejects_expr("{ (x & &): 3}"));
        assert!(rejects_expr("{ (x & y && z): 3}"));
        assert!(rejects_expr("{ (x & y z): 3}"));
        assert!(rejects_expr("{ ((x) & (y)): 3}"));
        assert!(rejects_expr("{ x: 3 ` }"));
        assert!(rejects_expr("{ x: ` 4 }"));
        assert!(rejects_expr("{ ` ` x: 4 }"));
        assert!(rejects_expr("{ { k: v } : v  } }"));
    }

    #[test]
    fn test_units() {
        // Complex unit
        assert!(accepts_unit(
            r#"#!/usr/bin/env eu

{ doc: "unit meta" }

x: 123

` "docs"
f(y): y + 234

ns: {
  a: b d c e # comment
}

"#
        ));

        // Simple unit
        assert!(accepts_unit("x: y"));
        
        // Error cases
        assert!(rejects_unit("9 ``"));
    }

    #[test]
    fn test_parse_embedded_lambda() {
        let lam = "(x, y) x * y";
        let mut files = SimpleFiles::new();
        let file_id = files.add("test".to_string(), lam.to_string());
        let result = parse_embedded_lambda(&files, file_id);
        // Should return an error since embedded lambda parsing is not yet implemented in Rowan
        assert!(result.is_err());
    }
}