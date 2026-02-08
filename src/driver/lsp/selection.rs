//! Compute selection ranges from the Rowan syntax tree.
//!
//! Selection ranges implement smart expand/contract selection using
//! Rowan's parent chain. Each syntax node maps to a selection range,
//! with its parent as the next expansion level.

use crate::syntax::rowan::kind::SyntaxNode;
use lsp_types::{Position, SelectionRange};
use rowan::TextSize;

use super::diagnostics::text_range_to_lsp_range;

/// Compute selection ranges for the given cursor positions.
///
/// For each position, walks from the deepest syntax node containing
/// that position up to the root, building a chain of selection ranges.
pub fn selection_ranges(
    source: &str,
    root: &SyntaxNode,
    positions: &[Position],
) -> Vec<SelectionRange> {
    positions
        .iter()
        .map(|pos| selection_range_at(source, root, pos))
        .collect()
}

/// Build the selection range chain for a single cursor position.
fn selection_range_at(source: &str, root: &SyntaxNode, position: &Position) -> SelectionRange {
    let offset = position_to_offset(source, position);

    // Find the deepest node containing this offset
    let mut node = root.clone();
    loop {
        let child = node
            .children()
            .find(|child| child.text_range().contains_inclusive(offset));
        match child {
            Some(c) => node = c,
            None => break,
        }
    }

    // Build the selection range chain from deepest to root
    build_selection_chain(source, &node)
}

/// Build a chain of selection ranges from a node up to the root.
fn build_selection_chain(source: &str, node: &SyntaxNode) -> SelectionRange {
    let range = text_range_to_lsp_range(source, node.text_range());

    let parent = node
        .parent()
        .map(|p| Box::new(build_selection_chain(source, &p)));

    SelectionRange { range, parent }
}

/// Convert an LSP position (line, UTF-16 character) to a byte offset.
///
/// The LSP protocol specifies `Position.character` in UTF-16 code
/// units. This function walks the source counting UTF-16 units to
/// find the correct byte offset.
pub(super) fn position_to_offset(source: &str, position: &Position) -> TextSize {
    let line_index = super::diagnostics::LineIndex::new(source);
    line_index.utf16_position_to_byte_offset(position)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::rowan::parse_unit;

    #[test]
    fn selection_at_identifier() {
        let source = "x: 1\ny: 2\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        let pos = Position {
            line: 0,
            character: 0,
        };
        let ranges = selection_ranges(source, &root, &[pos]);
        assert_eq!(ranges.len(), 1);

        // The innermost range should be small (just the identifier or name node)
        let inner = &ranges[0];
        assert_eq!(inner.range.start.line, 0);
        // There should be a parent chain up to the root
        assert!(inner.parent.is_some());
    }

    #[test]
    fn selection_chain_grows_to_root() {
        let source = "x: { y: 1 }\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        // Position inside the inner block value
        let pos = Position {
            line: 0,
            character: 8,
        };
        let ranges = selection_ranges(source, &root, &[pos]);
        assert_eq!(ranges.len(), 1);

        // Count the chain depth
        let mut depth = 0;
        let mut current = Some(&ranges[0]);
        while let Some(r) = current {
            depth += 1;
            current = r.parent.as_deref();
        }
        // Should have several levels: token -> name/literal -> soup -> body -> decl -> block -> ... -> unit
        assert!(depth >= 3, "expected chain depth >= 3, got {depth}");
    }

    #[test]
    fn multiple_positions() {
        let source = "x: 1\ny: 2\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        let positions = vec![
            Position {
                line: 0,
                character: 0,
            },
            Position {
                line: 1,
                character: 0,
            },
        ];
        let ranges = selection_ranges(source, &root, &positions);
        assert_eq!(ranges.len(), 2);
    }

    #[test]
    fn position_to_offset_basic() {
        let source = "abc\ndef\nghi";
        assert_eq!(
            position_to_offset(
                source,
                &Position {
                    line: 0,
                    character: 0
                }
            ),
            TextSize::from(0)
        );
        assert_eq!(
            position_to_offset(
                source,
                &Position {
                    line: 1,
                    character: 0
                }
            ),
            TextSize::from(4)
        );
        assert_eq!(
            position_to_offset(
                source,
                &Position {
                    line: 2,
                    character: 2
                }
            ),
            TextSize::from(10)
        );
    }

    #[test]
    fn position_to_offset_with_greek_letters() {
        // "α: x" — α is 2 bytes UTF-8, 1 UTF-16 code unit
        // UTF-16 column 3 should map to byte offset 4 ('x')
        let source = "α: x";
        let offset = position_to_offset(
            source,
            &Position {
                line: 0,
                character: 3,
            },
        );
        assert_eq!(offset, TextSize::from(4));
    }
}
