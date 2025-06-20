//! Lossless string pattern lexer for Rowan

use crate::syntax::rowan::kind::SyntaxKind::{self, *};
use codespan::{ByteIndex, ByteOffset, Span};
use std::{iter::Peekable, str::Chars};

/// String pattern lexer for parsing string interpolation 
/// This lexer preserves all source information for lossless parsing
pub struct StringPatternLexer<'text> {
    /// Input text (without outer quotes)
    text: &'text str,
    /// Character iterator
    chars: Peekable<Chars<'text>>,
    /// Current position
    position: ByteIndex,
    /// Offset to add to positions (for integration with main lexer)
    offset: ByteOffset,
    /// Are we inside an interpolation expression?
    in_interpolation: bool,
    /// Are we in a format specifier?
    in_format_spec: bool,
}

impl<'text> StringPatternLexer<'text> {
    /// Create a new string pattern lexer
    /// `text` should be the content inside the quotes
    /// `offset` is the position offset to add (position of opening quote + 1)
    pub fn new(text: &'text str, offset: ByteOffset) -> Self {
        Self {
            text,
            chars: text.chars().peekable(),
            position: ByteIndex(0),
            offset,
            in_interpolation: false,
            in_format_spec: false,
        }
    }

    /// Peek at the next character
    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    /// Consume the next character
    fn bump(&mut self) -> Option<(ByteIndex, char)> {
        if let Some(ch) = self.chars.next() {
            let pos = self.position;
            self.position += ByteOffset::from_char_len(ch);
            Some((pos, ch))
        } else {
            None
        }
    }

    /// Get a slice of the input text
    fn slice(&self, start: ByteIndex, end: ByteIndex) -> &'text str {
        &self.text[start.to_usize()..end.to_usize()]
    }

    /// Consume characters matching predicate
    fn consume<P>(&mut self, predicate: P) -> ByteIndex
    where
        P: Fn(char) -> bool,
    {
        while let Some(&ch) = self.peek() {
            if !predicate(ch) {
                break;
            }
            self.bump();
        }
        self.position
    }

    /// Create a span with offset applied
    fn make_span(&self, start: ByteIndex, end: ByteIndex) -> Span {
        Span::new(start + self.offset, end + self.offset)
    }

    /// Parse literal content (everything up to { or })
    fn literal_content(&mut self, start: ByteIndex) -> (SyntaxKind, Span, &'text str) {
        let end = self.consume(|c| c != '{' && c != '}');
        let content = self.slice(start, end);
        (STRING_LITERAL_CONTENT, self.make_span(start, end), content)
    }

    /// Parse escaped open brace {{
    fn escaped_open(&mut self, start: ByteIndex) -> (SyntaxKind, Span, &'text str) {
        // We've seen the first {, consume the second
        self.bump(); // consume second {
        let end = self.position;
        (STRING_ESCAPED_OPEN, self.make_span(start, end), "{{")
    }

    /// Parse escaped close brace }}
    fn escaped_close(&mut self, start: ByteIndex) -> (SyntaxKind, Span, &'text str) {
        // We've seen the first }, consume the second
        self.bump(); // consume second }
        let end = self.position;
        (STRING_ESCAPED_CLOSE, self.make_span(start, end), "}}")
    }

    /// Parse interpolation content (identifier, number, or dotted reference)
    fn interpolation_content(&mut self, start: ByteIndex) -> (SyntaxKind, Span, &'text str) {
        let end = self.consume(|c| !c.is_whitespace() && c != '}' && c != ':' && c != '.');
        let content = self.slice(start, end);
        (STRING_INTERPOLATION_TARGET, self.make_span(start, end), content)
    }

    /// Parse format specifier content after :
    fn format_content(&mut self, start: ByteIndex) -> (SyntaxKind, Span, &'text str) {
        let end = self.consume(|c| c != '}' && c != ':');
        let content = self.slice(start, end);
        (STRING_FORMAT_SPEC, self.make_span(start, end), content)
    }

    /// Skip whitespace and return true if any was found
    fn skip_whitespace(&mut self) -> bool {
        let start = self.position;
        self.consume(|c| c.is_whitespace());
        self.position != start
    }
}

impl<'text> Iterator for StringPatternLexer<'text> {
    type Item = (SyntaxKind, Span, &'text str);

    fn next(&mut self) -> Option<Self::Item> {
        // Skip whitespace in interpolation contexts but preserve in literals
        if self.in_interpolation {
            self.skip_whitespace();
        }

        let (start, ch) = self.bump()?;

        match ch {
            '{' => {
                // Check for escaped {{
                if let Some(&'{') = self.peek() {
                    Some(self.escaped_open(start))
                } else {
                    // Start interpolation
                    self.in_interpolation = true;
                    self.in_format_spec = false;
                    Some((OPEN_BRACE, self.make_span(start, self.position), "{"))
                }
            }
            '}' => {
                if self.in_interpolation {
                    // Check for escaped }}
                    if let Some(&'}') = self.peek() {
                        Some(self.escaped_close(start))
                    } else {
                        // End interpolation
                        self.in_interpolation = false;
                        self.in_format_spec = false;
                        Some((CLOSE_BRACE, self.make_span(start, self.position), "}"))
                    }
                } else {
                    // Outside interpolation, check for escaped }}
                    if let Some(&'}') = self.peek() {
                        Some(self.escaped_close(start))
                    } else {
                        // Literal } - include in literal content
                        Some(self.literal_content(start))
                    }
                }
            }
            ':' if self.in_interpolation => {
                self.in_format_spec = true;
                Some((COLON, self.make_span(start, self.position), ":"))
            }
            '.' if self.in_interpolation => {
                Some((OPERATOR_IDENTIFIER, self.make_span(start, self.position), "."))
            }
            _ if self.in_interpolation => {
                if self.in_format_spec {
                    Some(self.format_content(start))
                } else {
                    Some(self.interpolation_content(start))
                }
            }
            _ => {
                // Regular literal content
                Some(self.literal_content(start))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(text: &str) -> Vec<(SyntaxKind, &str)> {
        let lexer = StringPatternLexer::new(text, ByteOffset(0));
        lexer.map(|(kind, _span, text)| (kind, text)).collect()
    }

    #[test]
    fn test_simple_literal() {
        assert_eq!(
            lex("hello world"),
            vec![(STRING_LITERAL_CONTENT, "hello world")]
        );
    }

    #[test]
    fn test_simple_interpolation() {
        assert_eq!(
            lex("hello {name} world"),
            vec![
                (STRING_LITERAL_CONTENT, "hello "),
                (OPEN_BRACE, "{"),
                (STRING_INTERPOLATION_TARGET, "name"),
                (CLOSE_BRACE, "}"),
                (STRING_LITERAL_CONTENT, " world")
            ]
        );
    }

    #[test]
    fn test_escaped_braces() {
        assert_eq!(
            lex("hello {{world}} test"),
            vec![
                (STRING_LITERAL_CONTENT, "hello "),
                (STRING_ESCAPED_OPEN, "{{"),
                (STRING_LITERAL_CONTENT, "world"),
                (STRING_ESCAPED_CLOSE, "}}"),
                (STRING_LITERAL_CONTENT, " test")
            ]
        );
    }

    #[test]
    fn test_format_specifier() {
        assert_eq!(
            lex("value: {num:%03d}"),
            vec![
                (STRING_LITERAL_CONTENT, "value: "),
                (OPEN_BRACE, "{"),
                (STRING_INTERPOLATION_TARGET, "num"),
                (COLON, ":"),
                (STRING_FORMAT_SPEC, "%03d"),
                (CLOSE_BRACE, "}")
            ]
        );
    }

    #[test]
    fn test_dotted_reference() {
        assert_eq!(
            lex("data: {obj.field.value}"),
            vec![
                (STRING_LITERAL_CONTENT, "data: "),
                (OPEN_BRACE, "{"),
                (STRING_INTERPOLATION_TARGET, "obj"),
                (OPERATOR_IDENTIFIER, "."),
                (STRING_INTERPOLATION_TARGET, "field"),
                (OPERATOR_IDENTIFIER, "."),
                (STRING_INTERPOLATION_TARGET, "value"),
                (CLOSE_BRACE, "}")
            ]
        );
    }
}