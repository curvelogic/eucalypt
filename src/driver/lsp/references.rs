//! Find references for the eucalypt LSP.
//!
//! Given a cursor position, finds the symbol name and scans the document
//! for all occurrences of that name, returning their locations.

use crate::syntax::rowan::kind::{SyntaxKind, SyntaxNode, SyntaxToken};
use lsp_types::{Location, Position, Url};

use super::diagnostics::text_range_to_lsp_range;
use super::navigation::find_identifier_at;
use super::selection::position_to_offset;

/// Find all references to the symbol at the given cursor position.
///
/// Returns locations of all identifier tokens in the document that
/// match the name of the symbol under the cursor. If
/// `include_declaration` is true, the declaration site is included.
pub fn find_references(
    source: &str,
    root: &SyntaxNode,
    position: &Position,
    uri: &Url,
    include_declaration: bool,
) -> Option<Vec<Location>> {
    let offset = position_to_offset(source, position);
    let token = find_identifier_at(root, offset)?;
    let name = identifier_text(&token);

    let mut locations = Vec::new();

    // Walk all tokens in the tree
    for descendant in root.descendants_with_tokens() {
        if let Some(tok) = descendant.as_token() {
            if !is_identifier_kind(tok.kind()) {
                continue;
            }
            let tok_name = identifier_text(tok);
            if tok_name != name {
                continue;
            }

            // If not including declarations, skip the declaration head.
            // A token in a declaration head is the one that defines the name.
            // We detect this by checking if the token's parent chain includes
            // a DECL_HEAD node.
            if !include_declaration && is_in_declaration_head(tok) {
                continue;
            }

            let range = text_range_to_lsp_range(source, tok.text_range());
            locations.push(Location {
                uri: uri.clone(),
                range,
            });
        }
    }

    if locations.is_empty() {
        None
    } else {
        Some(locations)
    }
}

/// Check if a syntax kind is an identifier.
fn is_identifier_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::UNQUOTED_IDENTIFIER
            | SyntaxKind::OPERATOR_IDENTIFIER
            | SyntaxKind::SINGLE_QUOTE_IDENTIFIER
    )
}

/// Extract the display name from an identifier token.
fn identifier_text(token: &SyntaxToken) -> String {
    let text = token.text();
    match token.kind() {
        SyntaxKind::SINGLE_QUOTE_IDENTIFIER => text
            .strip_prefix('\'')
            .and_then(|t| t.strip_suffix('\''))
            .unwrap_or(text)
            .to_string(),
        _ => text.to_string(),
    }
}

/// Check if a token is inside a DECL_HEAD node (i.e. it's the declaration name).
fn is_in_declaration_head(token: &SyntaxToken) -> bool {
    let mut node = token.parent();
    while let Some(n) = node {
        if n.kind() == SyntaxKind::DECL_HEAD {
            return true;
        }
        node = n.parent();
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::rowan::parse_unit;

    fn test_uri() -> Url {
        Url::parse("file:///test/file.eu").unwrap()
    }

    #[test]
    fn test_find_references_simple() {
        let source = "x: 1\ny: x\nz: x\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        // Position on `x` in `y: x` (line 1, char 3)
        let pos = Position {
            line: 1,
            character: 3,
        };
        let result = find_references(source, &root, &pos, &test_uri(), false);
        assert!(result.is_some());
        let locs = result.unwrap();
        // Should find the two usage sites (not the declaration)
        assert_eq!(
            locs.len(),
            2,
            "should find 2 references (excluding declaration)"
        );
    }

    #[test]
    fn test_find_references_include_declaration() {
        let source = "x: 1\ny: x\nz: x\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        let pos = Position {
            line: 1,
            character: 3,
        };
        let result = find_references(source, &root, &pos, &test_uri(), true);
        assert!(result.is_some());
        let locs = result.unwrap();
        // Should find declaration + 2 usages = 3
        assert_eq!(
            locs.len(),
            3,
            "should find 3 references (including declaration)"
        );
    }

    #[test]
    fn test_find_references_from_declaration() {
        let source = "x: 1\ny: x\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        // Position on `x` in the declaration `x: 1` (line 0, char 0)
        let pos = Position {
            line: 0,
            character: 0,
        };
        let result = find_references(source, &root, &pos, &test_uri(), false);
        assert!(result.is_some());
        let locs = result.unwrap();
        // Should find the usage in `y: x`
        assert_eq!(locs.len(), 1);
    }

    #[test]
    fn test_find_references_function() {
        let source = "f(x): x\ny: f(1)\nz: f(2)\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        // Position on `f` in `y: f(1)` (line 1, char 3)
        let pos = Position {
            line: 1,
            character: 3,
        };
        let result = find_references(source, &root, &pos, &test_uri(), false);
        assert!(result.is_some());
        let locs = result.unwrap();
        assert_eq!(locs.len(), 2, "should find 2 call sites");
    }

    #[test]
    fn test_find_no_references() {
        let source = "x: 42\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        // Position on the literal — should not find a name
        let pos = Position {
            line: 0,
            character: 3,
        };
        let result = find_references(source, &root, &pos, &test_uri(), false);
        assert!(result.is_none(), "should not find references for a literal");
    }
}
