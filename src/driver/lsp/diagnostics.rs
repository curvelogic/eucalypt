//! Convert Rowan parse errors to LSP diagnostics.
//!
//! Each `ParseError` variant carries a `TextRange` (byte offset range).
//! We convert these to LSP `Diagnostic` objects with line/column
//! positions by scanning the source text for line breaks.

use crate::syntax::rowan::ParseError;
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use rowan::TextRange;

/// Convert a collection of parse errors into LSP diagnostics.
pub fn diagnostics_from_parse_errors(source: &str, errors: &[ParseError]) -> Vec<Diagnostic> {
    let line_index = LineIndex::new(source);
    errors
        .iter()
        .map(|err| to_diagnostic(&line_index, err))
        .collect()
}

/// Convert a single parse error to an LSP diagnostic.
fn to_diagnostic(line_index: &LineIndex, error: &ParseError) -> Diagnostic {
    let range = error_range(error);
    let lsp_range = line_index.range(range);
    let message = error_message(error);

    Diagnostic {
        range: lsp_range,
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("eucalypt".to_string()),
        message,
        ..Diagnostic::default()
    }
}

/// Extract the primary text range from a parse error.
fn error_range(error: &ParseError) -> TextRange {
    match error {
        ParseError::UnexpectedToken { range, .. }
        | ParseError::UnclosedSingleQuote { range }
        | ParseError::UnclosedDoubleQuote { range }
        | ParseError::InvalidParenExpr { range, .. }
        | ParseError::UnterminatedBlock { range, .. }
        | ParseError::EmptyDeclarationBody { range }
        | ParseError::MalformedDeclarationHead { range }
        | ParseError::InvalidFormalParameter { range, .. }
        | ParseError::InvalidOperatorName { range, .. }
        | ParseError::InvalidPropertyName { range, .. }
        | ParseError::SurplusContent { range }
        | ParseError::ReservedCharacter { range }
        | ParseError::EmptyExpression { range }
        | ParseError::UnclosedStringInterpolation { range }
        | ParseError::InvalidZdtLiteral { range, .. }
        | ParseError::InvalidDoubleColon { range }
        | ParseError::UnclosedBracketExpr { range }
        | ParseError::UnknownBracketPair { range, .. } => *range,
        ParseError::MissingDeclarationColon { head_range } => *head_range,
        ParseError::MismatchedBrackets { open_range, .. } => *open_range,
    }
}

/// Produce a human-readable error message for a parse error.
fn error_message(error: &ParseError) -> String {
    match error {
        ParseError::UnexpectedToken {
            expected, actual, ..
        } => format!("expected {expected:?}, found {actual:?}"),
        ParseError::UnclosedSingleQuote { .. } => "unclosed single quote".to_string(),
        ParseError::UnclosedDoubleQuote { .. } => "unclosed double quote".to_string(),
        ParseError::InvalidParenExpr { .. } => "invalid parenthesised expression".to_string(),
        ParseError::UnterminatedBlock { .. } => {
            "unterminated block (missing closing brace)".to_string()
        }
        ParseError::EmptyDeclarationBody { .. } => "empty declaration body".to_string(),
        ParseError::MissingDeclarationColon { .. } => "missing colon in declaration".to_string(),
        ParseError::MalformedDeclarationHead { .. } => "malformed declaration head".to_string(),
        ParseError::InvalidFormalParameter { .. } => "invalid formal parameter".to_string(),
        ParseError::InvalidOperatorName { .. } => "invalid operator name".to_string(),
        ParseError::InvalidPropertyName { .. } => "invalid property name".to_string(),
        ParseError::SurplusContent { .. } => "unexpected content after expression".to_string(),
        ParseError::ReservedCharacter { .. } => "reserved character".to_string(),
        ParseError::EmptyExpression { .. } => "empty expression".to_string(),
        ParseError::UnclosedStringInterpolation { .. } => {
            "unclosed string interpolation (missing closing brace)".to_string()
        }
        ParseError::InvalidZdtLiteral { .. } => "invalid ZDT literal".to_string(),
        ParseError::InvalidDoubleColon { .. } => {
            "'::' is not valid syntax; use ':' for declarations".to_string()
        }
        ParseError::UnclosedBracketExpr { .. } => {
            "unclosed bracket expression (missing closing bracket)".to_string()
        }
        ParseError::MismatchedBrackets {
            expected_close,
            actual_close,
            ..
        } => format!("mismatched brackets: expected '{expected_close}' but found '{actual_close}'"),
        ParseError::UnknownBracketPair { open_char, .. } => {
            format!("unknown bracket pair starting with '{open_char}'")
        }
    }
}

/// Convert a Rowan `TextRange` to an LSP `Range` using the source text
/// for line/column mapping.
pub fn text_range_to_lsp_range(source: &str, range: TextRange) -> Range {
    let line_index = LineIndex::new(source);
    line_index.range(range)
}

/// Line/column index for converting byte offsets to LSP positions.
///
/// Builds an index of line start offsets on construction, then
/// provides O(log n) lookups via binary search. Column values are
/// returned as UTF-16 code units, as required by the LSP protocol.
pub(super) struct LineIndex<'a> {
    /// The source text, retained for UTF-16 column conversion.
    source: &'a str,
    /// Byte offset of the start of each line (line 0 starts at 0).
    line_starts: Vec<u32>,
}

impl<'a> LineIndex<'a> {
    /// Build a line index from source text.
    pub(super) fn new(source: &'a str) -> Self {
        let mut line_starts = vec![0u32];
        for (offset, ch) in source.char_indices() {
            if ch == '\n' {
                line_starts.push((offset + 1) as u32);
            }
        }
        LineIndex {
            source,
            line_starts,
        }
    }

    /// Convert a byte offset to an LSP `Position` (0-based line and
    /// character in UTF-16 code units).
    pub(super) fn position(&self, offset: rowan::TextSize) -> Position {
        let offset = u32::from(offset);
        let line = match self.line_starts.binary_search(&offset) {
            Ok(line) => line,
            Err(line) => line - 1,
        };
        let line_start = self.line_starts[line];
        let character = byte_col_to_utf16(self.source, line_start, offset);
        Position {
            line: line as u32,
            character,
        }
    }

    /// Convert a `TextRange` to an LSP `Range`.
    pub(super) fn range(&self, range: TextRange) -> Range {
        Range {
            start: self.position(range.start()),
            end: self.position(range.end()),
        }
    }

    /// Convert a byte offset to (line, UTF-16 column), both 0-based.
    pub(super) fn offset_to_line_col(&self, byte_offset: u32) -> (u32, u32) {
        let line = match self.line_starts.binary_search(&byte_offset) {
            Ok(line) => line,
            Err(line) => line - 1,
        };
        let line_start = self.line_starts[line];
        let col = byte_col_to_utf16(self.source, line_start, byte_offset);
        (line as u32, col)
    }

    /// Compute the length of a byte range in UTF-16 code units.
    pub(super) fn utf16_text_len(&self, byte_start: u32, byte_end: u32) -> u32 {
        byte_col_to_utf16(self.source, byte_start, byte_end)
    }

    /// Convert an LSP position (line, UTF-16 character) to a byte offset.
    pub(super) fn utf16_position_to_byte_offset(&self, position: &Position) -> rowan::TextSize {
        let line = position.line as usize;
        let line_start = if line < self.line_starts.len() {
            self.line_starts[line] as usize
        } else {
            self.source.len()
        };

        let mut utf16_col = 0u32;
        let mut byte_offset = line_start;
        for ch in self.source[line_start..].chars() {
            if ch == '\n' || utf16_col >= position.character {
                break;
            }
            utf16_col += ch.len_utf16() as u32;
            byte_offset += ch.len_utf8();
        }

        rowan::TextSize::from(byte_offset as u32)
    }
}

/// Count UTF-16 code units in the source text between two byte offsets.
///
/// Both `byte_start` and `byte_end` are clamped to char boundaries to
/// guard against rowan `TextSize` values that land inside a multi-byte
/// character (which can occur with non-ASCII operator symbols such as
/// `∘`, `•`, and `÷` in the prelude).
fn byte_col_to_utf16(source: &str, byte_start: u32, byte_end: u32) -> u32 {
    let source_len = source.len();
    let start = (byte_start as usize).min(source_len);
    let end = (byte_end as usize).min(source_len);
    // Walk back to the nearest char boundary if needed
    let start = floor_char_boundary(source, start);
    let end = floor_char_boundary(source, end);
    if start >= end {
        return 0;
    }
    let slice = &source[start..end];
    slice.chars().map(|ch| ch.len_utf16() as u32).sum()
}

/// Return the largest index `<= pos` that is a char boundary in `s`.
fn floor_char_boundary(s: &str, pos: usize) -> usize {
    if pos >= s.len() {
        return s.len();
    }
    let mut p = pos;
    while p > 0 && !s.is_char_boundary(p) {
        p -= 1;
    }
    p
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::rowan::parse_unit;

    #[test]
    fn no_errors_for_valid_source() {
        let source = "x: 1\ny: 2\n";
        let parse = parse_unit(source);
        let diags = diagnostics_from_parse_errors(source, parse.errors());
        assert!(diags.is_empty());
    }

    #[test]
    fn error_for_unterminated_block() {
        let source = "x: { y: 1\n";
        let parse = parse_unit(source);
        let diags = diagnostics_from_parse_errors(source, parse.errors());
        assert!(!diags.is_empty());
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diags[0].source.as_deref(), Some("eucalypt"));
    }

    #[test]
    fn error_for_empty_declaration_body() {
        let source = "x:\n";
        let parse = parse_unit(source);
        let diags = diagnostics_from_parse_errors(source, parse.errors());
        assert!(!diags.is_empty());
    }

    #[test]
    fn line_index_single_line() {
        let idx = LineIndex::new("hello");
        let pos = idx.position(rowan::TextSize::from(3));
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 3);
    }

    #[test]
    fn line_index_multi_line() {
        let idx = LineIndex::new("abc\ndef\nghi");
        // 'd' is at byte 4, line 1, character 0
        let pos = idx.position(rowan::TextSize::from(4));
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 0);

        // 'h' is at byte 8, line 2, character 0
        let pos = idx.position(rowan::TextSize::from(8));
        assert_eq!(pos.line, 2);
        assert_eq!(pos.character, 0);

        // 'i' is at byte 10, line 2, character 2
        let pos = idx.position(rowan::TextSize::from(10));
        assert_eq!(pos.line, 2);
        assert_eq!(pos.character, 2);
    }

    #[test]
    fn line_index_at_line_start() {
        let idx = LineIndex::new("abc\ndef\n");
        let pos = idx.position(rowan::TextSize::from(0));
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 0);

        let pos = idx.position(rowan::TextSize::from(4));
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 0);
    }

    #[test]
    fn diagnostics_have_correct_positions() {
        // Second line has the error
        let source = "x: 1\ny:\n";
        let parse = parse_unit(source);
        let diags = diagnostics_from_parse_errors(source, parse.errors());
        assert!(!diags.is_empty());
        // Error should be on line 1 (the "y:" line)
        assert_eq!(diags[0].range.start.line, 1);
    }

    #[test]
    fn line_index_utf16_greek_letters() {
        // Greek letter alpha is 2 bytes in UTF-8 but 1 UTF-16 code unit.
        // In "α: x", byte offsets are: α=0..2, colon=2, space=3, x=4
        // UTF-16 columns should be:    α=0,    colon=1, space=2, x=3
        let source = "α: x";
        let idx = LineIndex::new(source);

        // 'x' is at byte offset 4
        let pos = idx.position(rowan::TextSize::from(4));
        assert_eq!(pos.line, 0);
        assert_eq!(
            pos.character, 3,
            "x should be at UTF-16 column 3, not byte column 4"
        );
    }

    #[test]
    fn line_index_utf16_cjk_character() {
        // CJK character U+4E16 (世) is 3 bytes in UTF-8, 1 UTF-16 code unit.
        // In "世x", byte offsets are: 世=0..3, x=3
        // UTF-16 columns should be:   世=0,    x=1
        let source = "世x";
        let idx = LineIndex::new(source);

        let pos = idx.position(rowan::TextSize::from(3));
        assert_eq!(
            pos.character, 1,
            "x after CJK char should be at UTF-16 column 1"
        );
    }

    #[test]
    fn line_index_utf16_emoji_surrogate_pair() {
        // U+1F600 (grinning face) is 4 bytes in UTF-8, 2 UTF-16 code units.
        // In "😀x", byte offsets are: 😀=0..4, x=4
        // UTF-16 columns should be:   😀=0..2, x=2
        let source = "😀x";
        let idx = LineIndex::new(source);

        let pos = idx.position(rowan::TextSize::from(4));
        assert_eq!(
            pos.character, 2,
            "x after emoji should be at UTF-16 column 2"
        );
    }

    #[test]
    fn utf16_text_len_greek() {
        // "αβ" is 4 bytes UTF-8, 2 UTF-16 code units
        let source = "αβ";
        let idx = LineIndex::new(source);
        assert_eq!(idx.utf16_text_len(0, 4), 2);
    }

    #[test]
    fn utf16_position_to_byte_offset_greek() {
        // In "α: x", UTF-16 column 3 is 'x' at byte offset 4
        let source = "α: x";
        let idx = LineIndex::new(source);
        let pos = Position {
            line: 0,
            character: 3,
        };
        let offset = idx.utf16_position_to_byte_offset(&pos);
        assert_eq!(
            u32::from(offset),
            4,
            "UTF-16 col 3 should map to byte offset 4"
        );
    }

    #[test]
    fn utf16_position_to_byte_offset_multiline_greek() {
        // Line 0: "α: 1\n" (bytes 0..6, α is 2 bytes)
        // Line 1: "β: 2\n" (bytes 6..12, β is 2 bytes)
        // On line 1, UTF-16 col 3 is '2' at byte 10
        let source = "α: 1\nβ: 2\n";
        let idx = LineIndex::new(source);
        let pos = Position {
            line: 1,
            character: 3,
        };
        let offset = idx.utf16_position_to_byte_offset(&pos);
        // Line 1 starts at byte 6 (after "α: 1\n" = 2+1+1+1+1 = 6)
        // On line 1: β(2 bytes, 1 utf16) + :(1,1) + space(1,1) = byte 9, utf16 col 3
        // So '2' is at byte 9... wait:
        // β=byte 6..8 (utf16 col 0..1), :=byte 8 (col 1..2),
        // space=byte 9 (col 2..3), 2=byte 10 (col 3..4)
        assert_eq!(
            u32::from(offset),
            10,
            "UTF-16 col 3 on line 1 should be byte 10"
        );
    }
}
