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
        (
            STRING_INTERPOLATION_TARGET,
            self.make_span(start, end),
            content,
        )
    }

    /// Parse format specifier content after :
    ///
    /// The specifier runs to the next `}` or `:` but stops before any
    /// trailing whitespace, which the next call emits as trivia: in
    /// `"{ s : up }"` the specifier is `up`, not `up `. Whitespace inside
    /// the specifier (the printf space flag in `%  d`) is kept.
    fn format_content(&mut self, start: ByteIndex) -> (SyntaxKind, Span, &'text str) {
        let rest = &self.text[start.to_usize()..];
        let raw_len = rest.find(['}', ':']).unwrap_or(rest.len());
        let end = start + ByteOffset(rest[..raw_len].trim_end().len() as i64);
        while self.position < end {
            self.bump();
        }
        let content = self.slice(start, self.position);
        (
            STRING_FORMAT_SPEC,
            self.make_span(start, self.position),
            content,
        )
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
        // Whitespace inside an interpolation is emitted as a WHITESPACE
        // trivia token so that the syntax tree stays lossless (its text
        // length must equal the source length).
        if self.in_interpolation {
            let start = self.position;
            if self.skip_whitespace() {
                let end = self.position;
                return Some((
                    WHITESPACE,
                    self.make_span(start, end),
                    self.slice(start, end),
                ));
            }
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
                    // Always ends the interpolation: a target cannot contain
                    // '}', so a following '}' belongs to the literal text
                    // after it ("{x}}}" is "{x}" then the "}}" escape).
                    self.in_interpolation = false;
                    self.in_format_spec = false;
                    Some((CLOSE_BRACE, self.make_span(start, self.position), "}"))
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
            '.' if self.in_interpolation => Some((
                OPERATOR_IDENTIFIER,
                self.make_span(start, self.position),
                ".",
            )),
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

/// C-String pattern lexer for parsing c-string interpolation
/// This lexer handles backslash escapes for braces (\{ and \}) instead of {{ and }}
/// Other escape sequences (\n, \t, etc.) are preserved for later processing
pub struct CStringPatternLexer<'text> {
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

impl<'text> CStringPatternLexer<'text> {
    /// Create a new c-string pattern lexer
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

    /// Parse literal content in c-string (stops at unescaped { or })
    fn literal_content(&mut self, start: ByteIndex) -> (SyntaxKind, Span, &'text str) {
        // Consume characters, handling backslash escapes
        loop {
            match self.peek() {
                Some(&'\\') => {
                    // Consume the backslash
                    self.bump();
                    // Consume the next character (the escaped char)
                    self.bump();
                }
                Some(&'{') | Some(&'}') => {
                    // Stop at unescaped braces
                    break;
                }
                Some(_) => {
                    self.bump();
                }
                None => {
                    break;
                }
            }
        }
        let end = self.position;
        let content = self.slice(start, end);
        (STRING_LITERAL_CONTENT, self.make_span(start, end), content)
    }

    /// Parse interpolation content (identifier, number, or dotted reference)
    fn interpolation_content(&mut self, start: ByteIndex) -> (SyntaxKind, Span, &'text str) {
        let end = self.consume(|c| !c.is_whitespace() && c != '}' && c != ':' && c != '.');
        let content = self.slice(start, end);
        (
            STRING_INTERPOLATION_TARGET,
            self.make_span(start, end),
            content,
        )
    }

    /// Parse format specifier content after :
    ///
    /// The specifier runs to the next `}` or `:` but stops before any
    /// trailing whitespace, which the next call emits as trivia: in
    /// `"{ s : up }"` the specifier is `up`, not `up `. Whitespace inside
    /// the specifier (the printf space flag in `%  d`) is kept.
    fn format_content(&mut self, start: ByteIndex) -> (SyntaxKind, Span, &'text str) {
        let rest = &self.text[start.to_usize()..];
        let raw_len = rest.find(['}', ':']).unwrap_or(rest.len());
        let end = start + ByteOffset(rest[..raw_len].trim_end().len() as i64);
        while self.position < end {
            self.bump();
        }
        let content = self.slice(start, self.position);
        (
            STRING_FORMAT_SPEC,
            self.make_span(start, self.position),
            content,
        )
    }

    /// Skip whitespace and return true if any was found
    fn skip_whitespace(&mut self) -> bool {
        let start = self.position;
        self.consume(|c| c.is_whitespace());
        self.position != start
    }
}

impl<'text> Iterator for CStringPatternLexer<'text> {
    type Item = (SyntaxKind, Span, &'text str);

    fn next(&mut self) -> Option<Self::Item> {
        // Whitespace inside an interpolation is emitted as a WHITESPACE
        // trivia token so that the syntax tree stays lossless (its text
        // length must equal the source length).
        if self.in_interpolation {
            let start = self.position;
            if self.skip_whitespace() {
                let end = self.position;
                return Some((
                    WHITESPACE,
                    self.make_span(start, end),
                    self.slice(start, end),
                ));
            }
        }

        let (start, ch) = self.bump()?;

        match ch {
            '{' => {
                // In c-strings, {{ is NOT an escape - it starts interpolation of {
                // Start interpolation
                self.in_interpolation = true;
                self.in_format_spec = false;
                Some((OPEN_BRACE, self.make_span(start, self.position), "{"))
            }
            '}' => {
                if self.in_interpolation {
                    // End interpolation
                    self.in_interpolation = false;
                    self.in_format_spec = false;
                    Some((CLOSE_BRACE, self.make_span(start, self.position), "}"))
                } else {
                    // Literal } outside interpolation - include in literal content
                    Some(self.literal_content(start))
                }
            }
            ':' if self.in_interpolation => {
                self.in_format_spec = true;
                Some((COLON, self.make_span(start, self.position), ":"))
            }
            '.' if self.in_interpolation => Some((
                OPERATOR_IDENTIFIER,
                self.make_span(start, self.position),
                ".",
            )),
            _ if self.in_interpolation => {
                if self.in_format_spec {
                    Some(self.format_content(start))
                } else {
                    Some(self.interpolation_content(start))
                }
            }
            _ => {
                // Regular literal content (includes handling backslash
                // escapes). When the token starts with a backslash, the
                // escaped character belongs to it too: otherwise an escaped
                // brace ("\{" or "\}") at the start of a literal run would
                // be read as an unescaped brace (eu-oaz72.114).
                if ch == '\\' {
                    self.bump();
                }
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

    fn lex_c(text: &str) -> Vec<(SyntaxKind, &str)> {
        let lexer = CStringPatternLexer::new(text, ByteOffset(0));
        lexer.map(|(kind, _span, text)| (kind, text)).collect()
    }

    #[test]
    fn test_cstring_escaped_brace_before_interpolation() {
        // eu-oaz72.114: an escaped brace starting a literal run stays with
        // its backslash rather than being read as an unescaped brace.
        assert_eq!(
            lex_c("\\{ {n}"),
            vec![
                (STRING_LITERAL_CONTENT, "\\{ "),
                (OPEN_BRACE, "{"),
                (STRING_INTERPOLATION_TARGET, "n"),
                (CLOSE_BRACE, "}")
            ]
        );
        assert_eq!(lex_c("\\}"), vec![(STRING_LITERAL_CONTENT, "\\}")]);
    }

    #[test]
    fn test_close_brace_run_after_interpolation() {
        // eu-uhd7d: the first '}' closes the interpolation; what follows is
        // literal text, so "}}" is the escape and a lone '}' is literal.
        assert_eq!(
            lex("{x}}}"),
            vec![
                (OPEN_BRACE, "{"),
                (STRING_INTERPOLATION_TARGET, "x"),
                (CLOSE_BRACE, "}"),
                (STRING_ESCAPED_CLOSE, "}}")
            ]
        );
        assert_eq!(
            lex("{x}}"),
            vec![
                (OPEN_BRACE, "{"),
                (STRING_INTERPOLATION_TARGET, "x"),
                (CLOSE_BRACE, "}"),
                (STRING_LITERAL_CONTENT, "}")
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
    fn test_padded_format_specifier_excludes_trailing_whitespace() {
        let expected = vec![
            (OPEN_BRACE, "{"),
            (WHITESPACE, " "),
            (STRING_INTERPOLATION_TARGET, "s"),
            (WHITESPACE, " "),
            (COLON, ":"),
            (WHITESPACE, " "),
            (STRING_FORMAT_SPEC, "up"),
            (WHITESPACE, "  "),
            (CLOSE_BRACE, "}"),
        ];
        assert_eq!(lex("{ s : up  }"), expected);
        assert_eq!(lex_c("{ s : up  }"), expected);
    }

    #[test]
    fn test_format_specifier_keeps_internal_whitespace() {
        assert_eq!(
            lex("{n:% d }"),
            vec![
                (OPEN_BRACE, "{"),
                (STRING_INTERPOLATION_TARGET, "n"),
                (COLON, ":"),
                (STRING_FORMAT_SPEC, "% d"),
                (WHITESPACE, " "),
                (CLOSE_BRACE, "}"),
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
