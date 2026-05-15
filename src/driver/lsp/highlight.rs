//! Document highlight provider for the eucalypt LSP.
//!
//! Provides bracket pair matching via `textDocument/documentHighlight`:
//! when the cursor is on a bracket open or close delimiter, both the open
//! and close tokens of the pair are highlighted.

use crate::syntax::rowan::kind::{SyntaxKind, SyntaxNode};
use lsp_types::{DocumentHighlight, DocumentHighlightKind, Position};

use super::diagnostics::text_range_to_lsp_range;
use super::selection::position_to_offset;

/// Compute document highlights for the given cursor position.
///
/// If the cursor is on a `BRACKET_OPEN` or `BRACKET_CLOSE` token, returns
/// highlights for both the open and close tokens of the bracket pair.
/// Otherwise returns an empty list.
pub fn document_highlights(
    source: &str,
    root: &SyntaxNode,
    position: &Position,
) -> Vec<DocumentHighlight> {
    let offset = position_to_offset(source, position);

    // Try right-biased first so a cursor at the very start of a multi-byte
    // bracket character (token boundary) finds the bracket, not whitespace.
    let candidates = root.token_at_offset(offset);
    let token = match candidates
        .clone()
        .right_biased()
        .or_else(|| candidates.left_biased())
    {
        Some(t) => t,
        None => return vec![],
    };

    if !matches!(
        token.kind(),
        SyntaxKind::BRACKET_OPEN | SyntaxKind::BRACKET_CLOSE
    ) {
        return vec![];
    }

    // Walk up to the BRACKET_EXPR or BRACKET_BLOCK parent to find both delimiters.
    let parent = match token.parent() {
        Some(p) => p,
        None => return vec![],
    };

    if !matches!(
        parent.kind(),
        SyntaxKind::BRACKET_EXPR | SyntaxKind::BRACKET_BLOCK
    ) {
        return vec![];
    }

    // Collect open and close tokens from the parent node.
    let mut open_range = None;
    let mut close_range = None;
    for child in parent.children_with_tokens() {
        if let rowan::NodeOrToken::Token(t) = child {
            match t.kind() {
                SyntaxKind::BRACKET_OPEN => {
                    open_range = Some(t.text_range());
                }
                SyntaxKind::BRACKET_CLOSE => {
                    close_range = Some(t.text_range());
                }
                _ => {}
            }
        }
    }

    let mut highlights = Vec::new();
    if let Some(r) = open_range {
        highlights.push(DocumentHighlight {
            range: text_range_to_lsp_range(source, r),
            kind: Some(DocumentHighlightKind::TEXT),
        });
    }
    if let Some(r) = close_range {
        highlights.push(DocumentHighlight {
            range: text_range_to_lsp_range(source, r),
            kind: Some(DocumentHighlightKind::TEXT),
        });
    }
    highlights
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::rowan::parse_unit;

    #[test]
    fn highlight_bracket_open() {
        let source = "⟦ x ⟧: x\nresult: ⟦ 42 ⟧\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        // Hover at '⟦' on line 1, column 8 (UTF-16)
        let pos = Position {
            line: 1,
            character: 8,
        };
        let highlights = document_highlights(source, &root, &pos);
        assert_eq!(
            highlights.len(),
            2,
            "should highlight both open and close: {highlights:?}"
        );
    }

    #[test]
    fn highlight_bracket_close() {
        let source = "⟦ x ⟧: x\nresult: ⟦ 42 ⟧\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        // Hover at '⟧' — it's at UTF-16 offset 13 on line 1
        // "result: ⟦ 42 ⟧"
        //  r(0)e(1)s(2)u(3)l(4)t(5):(6) (7)⟦(8) (9)4(10)2(11) (12)⟧(13)
        let pos = Position {
            line: 1,
            character: 13,
        };
        let highlights = document_highlights(source, &root, &pos);
        assert_eq!(
            highlights.len(),
            2,
            "should highlight both open and close: {highlights:?}"
        );
    }

    #[test]
    fn no_highlight_on_non_bracket() {
        let source = "x: 42\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        let pos = Position {
            line: 0,
            character: 0,
        };
        let highlights = document_highlights(source, &root, &pos);
        assert!(
            highlights.is_empty(),
            "should return empty for non-bracket token"
        );
    }

    #[test]
    fn highlight_block_mode_bracket() {
        let source = "⟦{}⟧: id\nresult: ⟦ a: 1 ⟧\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        let pos = Position {
            line: 1,
            character: 8,
        };
        let highlights = document_highlights(source, &root, &pos);
        assert_eq!(
            highlights.len(),
            2,
            "block-mode bracket should highlight both delimiters: {highlights:?}"
        );
    }
}
