// Re-export the Rowan parser interface as the main parser interface
pub use crate::syntax::rowan::{
    ast, parse_expr, parse_unit as rowan_parse_unit, Parse, ParseError as RowanParseError,
};

// Legacy compatibility functions for existing code - these now return Rowan AST directly
use crate::syntax::{error::ParserError, rowan};
use codespan_reporting::files::SimpleFiles;

/// Parse a unit - returns Rowan AST Unit directly
pub fn parse_unit<N, T>(
    files: &SimpleFiles<N, T>,
    id: usize,
) -> Result<rowan::ast::Unit, ParserError>
where
    N: AsRef<str> + Clone + std::fmt::Display,
    T: AsRef<str>,
{
    let text = files.get(id).unwrap().source().as_ref();
    let parse_result = rowan::parse_unit(text);

    if !parse_result.errors().is_empty() {
        let errors: Vec<String> = parse_result
            .errors()
            .iter()
            .map(|e| format!("{e:?}"))
            .collect();
        return Err(ParserError::Syntax(
            crate::syntax::error::SyntaxError::InvalidInputFormat(
                id,
                format!("Parse errors: {}", errors.join(", ")),
            ),
        ));
    }

    Ok(parse_result.tree())
}

/// Parse an expression - returns Rowan AST Soup directly
pub fn parse_expression<N, T>(
    files: &SimpleFiles<N, T>,
    id: usize,
) -> Result<rowan::ast::Soup, ParserError>
where
    N: AsRef<str> + Clone + std::fmt::Display,
    T: AsRef<str>,
{
    let text = files.get(id).unwrap().source().as_ref();
    let parse_result = rowan::parse_expr(text);

    if !parse_result.errors().is_empty() {
        let errors: Vec<String> = parse_result
            .errors()
            .iter()
            .map(|e| format!("{e:?}"))
            .collect();
        return Err(ParserError::Syntax(
            crate::syntax::error::SyntaxError::InvalidInputFormat(
                id,
                format!("Parse errors: {}", errors.join(", ")),
            ),
        ));
    }

    Ok(parse_result.tree())
}

/// Parse an embedded lambda - parses syntax like "(x, y) x * y" into parameter tuple and body
pub fn parse_embedded_lambda<N, T>(
    files: &SimpleFiles<N, T>,
    id: usize,
) -> Result<(rowan::ast::ApplyTuple, rowan::ast::Soup), ParserError>
where
    N: AsRef<str> + Clone + std::fmt::Display,
    T: AsRef<str>,
{
    let text = files.get(id).unwrap().source().as_ref();

    // Find the closing parenthesis of the parameter tuple
    // The format is "(x, y) body_expression"
    let mut paren_depth = 0;
    let mut closing_paren_pos = None;

    for (i, ch) in text.char_indices() {
        match ch {
            '(' => paren_depth += 1,
            ')' => {
                paren_depth -= 1;
                if paren_depth == 0 {
                    closing_paren_pos = Some(i + 1); // Position after the closing paren
                    break;
                }
            }
            _ => {}
        }
    }

    let (_param_text, body_text) = if let Some(pos) = closing_paren_pos {
        let param_text = &text[..pos - 1]; // Everything up to but not including the closing paren
        let body_text = if pos < text.len() {
            text[pos..].trim()
        } else {
            return Err(ParserError::Syntax(
                crate::syntax::error::SyntaxError::InvalidInputFormat(
                    id,
                    "Lambda body is missing".to_string(),
                ),
            ));
        };
        (param_text, body_text)
    } else {
        return Err(ParserError::Syntax(
            crate::syntax::error::SyntaxError::InvalidInputFormat(
                id,
                "Could not find closing parenthesis for lambda parameters".to_string(),
            ),
        ));
    };

    if body_text.is_empty() {
        return Err(ParserError::Syntax(
            crate::syntax::error::SyntaxError::InvalidInputFormat(
                id,
                "Lambda body is empty".to_string(),
            ),
        ));
    }

    // Parse the parameter tuple by wrapping it in "f" to make it look like function application
    let param_function_text = format!("f{}", text[..closing_paren_pos.unwrap()].trim());
    let param_parse_result = rowan::parse_expr(&param_function_text);

    if !param_parse_result.errors().is_empty() {
        return Err(ParserError::Syntax(
            crate::syntax::error::SyntaxError::InvalidInputFormat(
                id,
                "Failed to parse lambda parameters".to_string(),
            ),
        ));
    }

    let param_soup = param_parse_result.tree();
    let param_elements: Vec<_> = param_soup.elements().collect();

    // Should have exactly two elements: function name and ApplyTuple
    if param_elements.len() != 2 {
        return Err(ParserError::Syntax(
            crate::syntax::error::SyntaxError::InvalidInputFormat(
                id,
                format!(
                    "Invalid parameter parsing result - got {} elements",
                    param_elements.len()
                ),
            ),
        ));
    }

    // Extract the ApplyTuple (second element)
    let apply_tuple = match &param_elements[1] {
        rowan::ast::Element::ApplyTuple(tuple) => tuple.clone(),
        _ => {
            return Err(ParserError::Syntax(
                crate::syntax::error::SyntaxError::InvalidInputFormat(
                    id,
                    "Lambda must start with parameter tuple like (x, y)".to_string(),
                ),
            ));
        }
    };

    // Parse the body as a separate expression
    let body_parse_result = rowan::parse_expr(body_text);
    if !body_parse_result.errors().is_empty() {
        return Err(ParserError::Syntax(
            crate::syntax::error::SyntaxError::InvalidInputFormat(
                id,
                "Failed to parse lambda body".to_string(),
            ),
        ));
    }

    let body_soup = body_parse_result.tree();

    Ok((apply_tuple, body_soup))
}

#[cfg(test)]
pub mod tests {
    use super::*;

    fn accepts_expr(txt: &str) -> bool {
        parse_expr(txt).errors().is_empty()
    }

    fn rejects_expr(txt: &str) -> bool {
        !parse_expr(txt).errors().is_empty()
    }

    fn accepts_unit(txt: &str) -> bool {
        rowan_parse_unit(txt).errors().is_empty()
    }

    fn rejects_unit(txt: &str) -> bool {
        !rowan_parse_unit(txt).errors().is_empty()
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
        assert!(accepts_expr(
            "{ M M M M M ` m m m m m (x £ y): y1 y2 y3 y4 y5 }"
        ));
        assert!(accepts_expr("{ α: [:x] = [:x, ] }"));
        assert!(accepts_expr("{ α: [:x, :y, :z] = [:x, :y, :z, ] }"));

        // Nested blocks
        assert!(accepts_expr("{ x: { y: value } }"));
        assert!(accepts_expr("{ { m: v} x: { y: value } }"));
        assert!(accepts_expr("{ ` { m: v } x: { y: value } }"));
        assert!(accepts_expr("{ { m: v } ` { m: v } x: { y: value } }"));
        assert!(accepts_expr(
            "{ { m: v } ` { ` :m m: v } x: { ` \"m\" y: value } }"
        ));
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
    fn test_whitespace_separated_parentheses() {
        // Test distinguishing between applytuple and paren-expr based on whitespace

        // This should be an applytuple (no whitespace)
        let result = parse_expr("f(g)");
        assert!(result.errors().is_empty());
        let ast_str = format!("{}", result.syntax_node());
        assert!(
            ast_str.contains("ARG_TUPLE"),
            "f(g) should parse as applytuple (ARG_TUPLE)"
        );
        assert!(
            ast_str.contains("OPEN_PAREN_APPLY"),
            "f(g) should use OPEN_PAREN_APPLY"
        );

        // This should be catenation with paren-expr (whitespace)
        let result = parse_expr("f (g)");
        assert!(result.errors().is_empty());
        let ast_str = format!("{}", result.syntax_node());
        assert!(
            ast_str.contains("PAREN_EXPR"),
            "f (g) should parse with paren-expr, not applytuple"
        );
        assert!(
            !ast_str.contains("ARG_TUPLE"),
            "f (g) should not contain applytuple"
        );
        assert!(
            ast_str.contains("OPEN_PAREN"),
            "f (g) should use OPEN_PAREN (not OPEN_PAREN_APPLY)"
        );
        assert!(
            !ast_str.contains("OPEN_PAREN_APPLY"),
            "f (g) should not use OPEN_PAREN_APPLY"
        );

        // Test the matches? case specifically
        let result = parse_expr("match(s, re) (not ∘ nil?)");
        assert!(result.errors().is_empty());
        let ast_str = format!("{}", result.syntax_node());
        assert!(
            ast_str.contains("PAREN_EXPR"),
            "match(s, re) (not ∘ nil?) should have paren-expr"
        );
        assert!(
            ast_str.contains("ARG_TUPLE"),
            "match(s, re) (not ∘ nil?) should have applytuple for match"
        );
        // Should have both types of parens
        assert!(
            ast_str.contains("OPEN_PAREN_APPLY"),
            "match(s, re) should use OPEN_PAREN_APPLY"
        );
        assert!(
            ast_str.contains("OPEN_PAREN"),
            "second part should use OPEN_PAREN"
        );
    }
}
