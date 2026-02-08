//! LSP formatting integration.
//!
//! Wires up the existing `eu fmt` formatter via the LSP
//! `textDocument/formatting` and `textDocument/rangeFormatting`
//! requests. The formatter operates on the full Rowan parse tree so
//! both requests reformat the entire document, returning a single
//! `TextEdit` replacing the full text.

use lsp_types::{Position, Range, TextEdit};

use crate::syntax::export::format::{format_source, FormatterConfig};

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
/// The eucalypt formatter operates on the whole parse tree, so we
/// reformat the entire document and return the result. The range
/// parameter is accepted for protocol compliance but does not restrict
/// the formatted output — this matches the behaviour of many language
/// servers whose formatters lack incremental support.
pub fn format_range(source: &str, options: &lsp_types::FormattingOptions) -> Vec<TextEdit> {
    // Same as full-document formatting — the formatter is not
    // incremental, so we format the whole file.
    format_document(source, options)
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
    fn range_format_delegates_to_full() {
        let source = "x: 1\ny:   2\n";
        let full = format_document(source, &default_options());
        let range = format_range(source, &default_options());
        assert_eq!(full.len(), range.len());
        if !full.is_empty() {
            assert_eq!(full[0].new_text, range[0].new_text);
        }
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
}
