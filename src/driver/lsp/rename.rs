//! LSP rename provider.
//!
//! Renames a symbol across the current file by finding all references
//! (including the declaration) and generating `TextEdit`s for each
//! occurrence.

use crate::syntax::rowan::kind::{SyntaxKind, SyntaxNode, SyntaxToken};
use lsp_types::{Position, PrepareRenameResponse, TextEdit, Url, WorkspaceEdit};
use std::collections::HashMap;

use super::diagnostics::text_range_to_lsp_range;
use super::navigation::find_identifier_at;
use super::selection::position_to_offset;

/// Prepare a rename operation — validate that the cursor is on a
/// renameable symbol and return its range.
pub fn prepare_rename(
    source: &str,
    root: &SyntaxNode,
    position: &Position,
) -> Option<PrepareRenameResponse> {
    let offset = position_to_offset(source, position);
    let token = find_identifier_at(root, offset)?;
    let range = text_range_to_lsp_range(source, token.text_range());
    let placeholder = identifier_text(&token);
    Some(PrepareRenameResponse::RangeWithPlaceholder { range, placeholder })
}

/// Perform the rename, returning a `WorkspaceEdit` with text edits for
/// all occurrences of the symbol in the file.
pub fn rename(
    source: &str,
    root: &SyntaxNode,
    position: &Position,
    new_name: &str,
    uri: &Url,
) -> Option<WorkspaceEdit> {
    let offset = position_to_offset(source, position);
    let token = find_identifier_at(root, offset)?;
    let name = identifier_text(&token);

    let mut edits = Vec::new();

    // Find all identifier tokens matching the name
    for descendant in root.descendants_with_tokens() {
        if let Some(tok) = descendant.as_token() {
            if !is_identifier_kind(tok.kind()) {
                continue;
            }
            if identifier_text(tok) != name {
                continue;
            }

            let range = text_range_to_lsp_range(source, tok.text_range());
            // For single-quoted identifiers, preserve the quotes
            let replacement = if tok.kind() == SyntaxKind::SINGLE_QUOTE_IDENTIFIER {
                format!("'{new_name}'")
            } else {
                new_name.to_string()
            };

            edits.push(TextEdit {
                range,
                new_text: replacement,
            });
        }
    }

    if edits.is_empty() {
        return None;
    }

    let mut changes = HashMap::new();
    changes.insert(uri.clone(), edits);

    Some(WorkspaceEdit {
        changes: Some(changes),
        ..WorkspaceEdit::default()
    })
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::rowan::parse_unit;

    fn test_uri() -> Url {
        Url::parse("file:///test/file.eu").unwrap()
    }

    #[test]
    fn prepare_rename_on_identifier() {
        let source = "x: 1\ny: x\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let pos = Position {
            line: 0,
            character: 0,
        };
        let result = prepare_rename(source, &root, &pos);
        assert!(result.is_some());
        match result.unwrap() {
            PrepareRenameResponse::RangeWithPlaceholder { placeholder, .. } => {
                assert_eq!(placeholder, "x");
            }
            _ => panic!("expected RangeWithPlaceholder"),
        }
    }

    #[test]
    fn prepare_rename_on_literal_fails() {
        let source = "x: 42\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let pos = Position {
            line: 0,
            character: 3,
        };
        let result = prepare_rename(source, &root, &pos);
        assert!(result.is_none(), "cannot rename a literal");
    }

    #[test]
    fn rename_simple_property() {
        let source = "x: 1\ny: x\nz: x\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let pos = Position {
            line: 0,
            character: 0,
        };
        let result = rename(source, &root, &pos, "foo", &test_uri());
        assert!(result.is_some());
        let edit = result.unwrap();
        let changes = edit.changes.unwrap();
        let file_edits = changes.get(&test_uri()).unwrap();
        // Declaration `x` + 2 usages = 3 edits
        assert_eq!(file_edits.len(), 3, "should rename declaration + 2 usages");
        for e in file_edits {
            assert_eq!(e.new_text, "foo");
        }
    }

    #[test]
    fn rename_function() {
        let source = "f(x): x\ny: f(1)\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        // Position on `f` in the declaration
        let pos = Position {
            line: 0,
            character: 0,
        };
        let result = rename(source, &root, &pos, "g", &test_uri());
        assert!(result.is_some());
        let edit = result.unwrap();
        let changes = edit.changes.unwrap();
        let file_edits = changes.get(&test_uri()).unwrap();
        // Declaration `f` + usage `f` = 2 edits
        assert_eq!(file_edits.len(), 2, "should rename declaration + call site");
        for e in file_edits {
            assert_eq!(e.new_text, "g");
        }
    }

    #[test]
    fn rename_from_usage_site() {
        let source = "x: 1\ny: x\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        // Position on `x` in `y: x` (usage)
        let pos = Position {
            line: 1,
            character: 3,
        };
        let result = rename(source, &root, &pos, "val", &test_uri());
        assert!(result.is_some());
        let edit = result.unwrap();
        let changes = edit.changes.unwrap();
        let file_edits = changes.get(&test_uri()).unwrap();
        assert_eq!(
            file_edits.len(),
            2,
            "should rename both declaration and usage"
        );
    }

    #[test]
    fn rename_literal_returns_none() {
        let source = "x: 42\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let pos = Position {
            line: 0,
            character: 3,
        };
        let result = rename(source, &root, &pos, "y", &test_uri());
        assert!(result.is_none(), "cannot rename a literal");
    }
}
