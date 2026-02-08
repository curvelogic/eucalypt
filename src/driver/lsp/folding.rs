//! Compute folding ranges from the Rowan syntax tree.
//!
//! Blocks, lists, and multi-line string patterns produce folding
//! ranges. Only nodes spanning more than one line are included.

use crate::syntax::rowan::kind::SyntaxKind;
use lsp_types::{FoldingRange, FoldingRangeKind};
use rowan::ast::AstNode;

use super::diagnostics::text_range_to_lsp_range;

/// Compute folding ranges for the given source text and syntax tree.
pub fn folding_ranges(source: &str, unit: &crate::syntax::rowan::ast::Unit) -> Vec<FoldingRange> {
    let mut ranges = vec![];
    collect_folding_ranges(source, unit.syntax(), &mut ranges);
    ranges
}

/// Recursively walk the syntax tree collecting foldable nodes.
fn collect_folding_ranges(
    source: &str,
    node: &crate::syntax::rowan::kind::SyntaxNode,
    ranges: &mut Vec<FoldingRange>,
) {
    let kind = node.kind();

    if is_foldable(kind) {
        let lsp_range = text_range_to_lsp_range(source, node.text_range());

        // Only fold nodes spanning multiple lines
        if lsp_range.start.line < lsp_range.end.line {
            ranges.push(FoldingRange {
                start_line: lsp_range.start.line,
                start_character: Some(lsp_range.start.character),
                end_line: lsp_range.end.line,
                end_character: Some(lsp_range.end.character),
                kind: folding_kind(kind),
                collapsed_text: None,
            });
        }
    }

    for child in node.children() {
        collect_folding_ranges(source, &child, ranges);
    }
}

/// Check whether a syntax node kind should produce a folding range.
fn is_foldable(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::BLOCK
            | SyntaxKind::LIST
            | SyntaxKind::STRING_PATTERN
            | SyntaxKind::C_STRING_PATTERN
            | SyntaxKind::RAW_STRING_PATTERN
            | SyntaxKind::DECL_META
            | SyntaxKind::BLOCK_META
    )
}

/// Map a syntax kind to an optional LSP folding range kind.
fn folding_kind(kind: SyntaxKind) -> Option<FoldingRangeKind> {
    match kind {
        SyntaxKind::DECL_META | SyntaxKind::BLOCK_META => Some(FoldingRangeKind::Comment),
        _ => Some(FoldingRangeKind::Region),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::rowan::parse_unit;

    #[test]
    fn no_folds_for_single_line() {
        let source = "x: { y: 1 }\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let folds = folding_ranges(source, &unit);
        assert!(folds.is_empty());
    }

    #[test]
    fn fold_for_multi_line_block() {
        let source = "x: {\n  y: 1\n  z: 2\n}\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let folds = folding_ranges(source, &unit);
        assert!(!folds.is_empty());
        // The block fold should start on line 0 (where { is) and end on line 3 (where } is)
        let block_fold = &folds[0];
        assert_eq!(block_fold.start_line, 0);
        assert!(block_fold.end_line > block_fold.start_line);
    }

    #[test]
    fn fold_for_multi_line_list() {
        let source = "x: [\n  1,\n  2,\n  3\n]\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let folds = folding_ranges(source, &unit);
        assert!(!folds.is_empty());
    }

    #[test]
    fn nested_folds() {
        let source = "outer: {\n  inner: {\n    x: 1\n  }\n}\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let folds = folding_ranges(source, &unit);
        // Should have at least 2 folds: outer block and inner block
        assert!(folds.len() >= 2);
    }
}
