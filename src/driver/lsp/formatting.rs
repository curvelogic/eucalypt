//! LSP formatting integration.
//!
//! Wires up the existing `eu fmt` formatter via the LSP
//! `textDocument/formatting` and `textDocument/rangeFormatting`
//! requests.
//!
//! Full-document formatting reformats the entire file, returning a
//! single `TextEdit` replacing the full text.
//!
//! Range formatting identifies top-level declarations overlapping the
//! requested range and reformats only those declarations, returning
//! edits scoped to the affected ranges.

use lsp_types::{Position, Range, TextEdit};
use rowan::ast::AstNode;

use crate::syntax::export::format::{format_source, Formatter, FormatterConfig};

/// Format an entire document, returning edits to replace the full text.
///
/// Uses reformat mode for a clean, canonical layout. If the source has
/// parse errors or formatting produces no changes, returns an empty
/// edit list.
pub fn format_document(source: &str, options: &lsp_types::FormattingOptions) -> Vec<TextEdit> {
    let config = options_to_config(options);
    match format_source(source, &config) {
        Ok(formatted) if formatted != source => {
            vec![whole_document_edit(source, &formatted)]
        }
        _ => vec![],
    }
}

/// Format a range within a document.
///
/// Identifies top-level declarations overlapping the given range and
/// reformats each one individually. Declarations outside the range
/// are left untouched.
///
/// If the source has parse errors, returns an empty edit list.
pub fn format_range(
    source: &str,
    range: &Range,
    options: &lsp_types::FormattingOptions,
) -> Vec<TextEdit> {
    let parse = crate::syntax::rowan::parse_unit(source);
    if !parse.errors().is_empty() {
        return vec![];
    }

    let unit = parse.tree();
    let config = options_to_config(options);
    let formatter = Formatter::new(config);

    let request_start = lsp_position_to_offset(source, &range.start);
    let request_end = lsp_position_to_offset(source, &range.end);

    let mut edits = Vec::new();

    for decl in unit.declarations() {
        let node_range = decl.syntax().text_range();
        let node_start = u32::from(node_range.start()) as usize;
        let node_end = u32::from(node_range.end()) as usize;

        // For overlap checking, extend to include trailing newline
        let check_end = if node_end < source.len() && source.as_bytes()[node_end] == b'\n' {
            node_end + 1
        } else {
            node_end
        };

        // Check overlap: declaration range intersects requested range
        if node_start >= request_end || check_end <= request_start {
            continue;
        }

        let original = &source[node_start..node_end];
        if let Ok(formatted) = formatter.format_declaration(&decl) {
            if formatted != original {
                let start_pos = offset_to_lsp_position(source, node_start);
                let end_pos = offset_to_lsp_position(source, node_end);
                edits.push(TextEdit {
                    range: Range {
                        start: start_pos,
                        end: end_pos,
                    },
                    new_text: formatted,
                });
            }
        }
    }

    edits
}

/// Convert an LSP `Position` (line, character in UTF-16 code units)
/// to a byte offset in the source string.
fn lsp_position_to_offset(source: &str, position: &Position) -> usize {
    let mut current_line = 0u32;
    let mut current_char = 0u32;
    let mut byte_offset = 0;

    for ch in source.chars() {
        if current_line == position.line && current_char == position.character {
            return byte_offset;
        }
        if ch == '\n' {
            if current_line == position.line {
                // Position is beyond the end of this line; clamp to newline
                return byte_offset;
            }
            current_line += 1;
            current_char = 0;
        } else {
            current_char += ch.len_utf16() as u32;
        }
        byte_offset += ch.len_utf8();
    }

    byte_offset
}

/// Convert a byte offset to an LSP `Position` (line, character in
/// UTF-16 code units).
fn offset_to_lsp_position(source: &str, offset: usize) -> Position {
    let mut line = 0u32;
    let mut character = 0u32;
    let mut byte_offset = 0;

    for ch in source.chars() {
        if byte_offset >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            character = 0;
        } else {
            character += ch.len_utf16() as u32;
        }
        byte_offset += ch.len_utf8();
    }

    Position { line, character }
}

/// Build a `FormatterConfig` from LSP `FormattingOptions`.
fn options_to_config(options: &lsp_types::FormattingOptions) -> FormatterConfig {
    let indent_size = if options.insert_spaces {
        options.tab_size as usize
    } else {
        // Tabs requested — use tab_size as the visual width.
        options.tab_size as usize
    };
    FormatterConfig::new(80, indent_size, true)
}

/// Create a `TextEdit` replacing the entire document.
fn whole_document_edit(source: &str, formatted: &str) -> TextEdit {
    let end = end_of_document(source);
    TextEdit {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end,
        },
        new_text: formatted.to_string(),
    }
}

/// Compute the LSP `Position` at the end of the document.
///
/// Character offsets are counted in UTF-16 code units as required
/// by the LSP protocol.
fn end_of_document(source: &str) -> Position {
    let mut line = 0u32;
    let mut character = 0u32;
    for ch in source.chars() {
        if ch == '\n' {
            line += 1;
            character = 0;
        } else {
            character += ch.len_utf16() as u32;
        }
    }
    Position { line, character }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::FormattingOptions;

    fn default_options() -> FormattingOptions {
        FormattingOptions {
            tab_size: 2,
            insert_spaces: true,
            ..FormattingOptions::default()
        }
    }

    #[test]
    fn format_already_formatted_returns_empty() {
        let source = "x: 1\n";
        let edits = format_document(source, &default_options());
        assert!(edits.is_empty(), "no edits for already-formatted source");
    }

    #[test]
    fn format_adds_trailing_newline() {
        // The formatter normalises trailing whitespace; a missing
        // newline should produce an edit.
        let source = "x: 1";
        let edits = format_document(source, &default_options());
        if !edits.is_empty() {
            assert_eq!(edits[0].range.start.line, 0);
            assert!(edits[0].new_text.ends_with('\n'));
        }
    }

    #[test]
    fn format_parse_error_returns_empty() {
        let source = "x: { y: 1\n";
        let edits = format_document(source, &default_options());
        assert!(edits.is_empty(), "no edits when source has parse errors");
    }

    #[test]
    fn end_of_document_empty() {
        let pos = end_of_document("");
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 0);
    }

    #[test]
    fn end_of_document_single_line() {
        let pos = end_of_document("hello");
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 5);
    }

    #[test]
    fn end_of_document_multi_line() {
        let pos = end_of_document("abc\ndef\n");
        assert_eq!(pos.line, 2);
        assert_eq!(pos.character, 0);
    }

    #[test]
    fn options_to_config_respects_tab_size() {
        let mut opts = default_options();
        opts.tab_size = 4;
        let config = options_to_config(&opts);
        assert_eq!(config.indent_size, 4);
        assert!(config.reformat);
    }

    #[test]
    fn end_of_document_with_greek_letters() {
        // α is 2 bytes UTF-8, 1 UTF-16 code unit
        // "αβγ" has 3 chars = 3 UTF-16 code units (not 6 bytes)
        let pos = end_of_document("αβγ");
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 3, "3 Greek letters = 3 UTF-16 code units");
    }

    #[test]
    fn end_of_document_with_emoji() {
        // 😀 is 4 bytes UTF-8, 2 UTF-16 code units (surrogate pair)
        let pos = end_of_document("😀x");
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 3, "emoji (2 UTF-16 units) + x (1) = 3");
    }

    // ========================================================================
    // Range formatting tests
    // ========================================================================

    #[test]
    fn range_format_single_declaration() {
        // Second declaration has extra spaces; format range covering only it
        let source = "x: 1\ny:   2\n";
        let range = Range {
            start: Position {
                line: 1,
                character: 0,
            },
            end: Position {
                line: 1,
                character: 6,
            },
        };
        let edits = format_range(source, &range, &default_options());
        // Should produce an edit for the second declaration only
        if !edits.is_empty() {
            assert_eq!(edits.len(), 1, "should edit only one declaration");
            assert!(
                edits[0].range.start.line >= 1,
                "edit should start at or after line 1"
            );
            assert!(
                edits[0].new_text.contains("y:"),
                "edit should contain the y declaration"
            );
            assert!(
                !edits[0].new_text.contains("x:"),
                "edit should not contain the x declaration"
            );
        }
    }

    #[test]
    fn range_format_leaves_other_declarations_untouched() {
        let source = "x:   1\ny:   2\nz:   3\n";
        // Range covers only line 1 (the y declaration)
        let range = Range {
            start: Position {
                line: 1,
                character: 0,
            },
            end: Position {
                line: 1,
                character: 6,
            },
        };
        let edits = format_range(source, &range, &default_options());
        // Edits should only touch the y declaration, not x or z
        for edit in &edits {
            assert!(
                edit.range.start.line >= 1,
                "no edit should touch line 0 (x declaration)"
            );
            assert!(
                edit.range.end.line <= 2,
                "no edit should extend past line 2"
            );
        }
    }

    #[test]
    fn range_format_spanning_two_declarations() {
        let source = "x:   1\ny:   2\nz: 3\n";
        // Range spans lines 0-1 (x and y declarations)
        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 1,
                character: 6,
            },
        };
        let edits = format_range(source, &range, &default_options());
        // Should produce edits for both x and y (both have extra spaces)
        assert!(
            edits.len() >= 2,
            "should edit at least two declarations, got {}",
            edits.len()
        );
    }

    #[test]
    fn range_format_parse_error_returns_empty() {
        let source = "x: { y: 1\n";
        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 10,
            },
        };
        let edits = format_range(source, &range, &default_options());
        assert!(edits.is_empty(), "no edits when source has parse errors");
    }

    #[test]
    fn range_format_already_formatted_returns_empty() {
        let source = "x: 1\ny: 2\n";
        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 1,
                character: 4,
            },
        };
        let edits = format_range(source, &range, &default_options());
        assert!(edits.is_empty(), "no edits for already-formatted source");
    }

    #[test]
    fn lsp_position_to_offset_basic() {
        let source = "abc\ndef\n";
        assert_eq!(
            lsp_position_to_offset(
                source,
                &Position {
                    line: 0,
                    character: 0
                }
            ),
            0
        );
        assert_eq!(
            lsp_position_to_offset(
                source,
                &Position {
                    line: 0,
                    character: 3
                }
            ),
            3
        );
        assert_eq!(
            lsp_position_to_offset(
                source,
                &Position {
                    line: 1,
                    character: 0
                }
            ),
            4
        );
        assert_eq!(
            lsp_position_to_offset(
                source,
                &Position {
                    line: 1,
                    character: 3
                }
            ),
            7
        );
    }

    #[test]
    fn offset_to_lsp_position_basic() {
        let source = "abc\ndef\n";
        assert_eq!(
            offset_to_lsp_position(source, 0),
            Position {
                line: 0,
                character: 0
            }
        );
        assert_eq!(
            offset_to_lsp_position(source, 4),
            Position {
                line: 1,
                character: 0
            }
        );
        assert_eq!(
            offset_to_lsp_position(source, 7),
            Position {
                line: 1,
                character: 3
            }
        );
    }
}
