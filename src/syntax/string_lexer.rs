//! Lexer for parsing string patterns
use crate::syntax::error::SyntaxError;
use codespan::{ByteIndex, ByteOffset};
use std::iter::Peekable;
use std::str::Chars;

/// Tokens in the eucalypt syntax
#[derive(Clone, PartialEq, Debug)]
pub enum Token<'text> {
    EscapedOpenBrace,
    EscapedCloseBrace,
    OpenBrace,
    CloseBrace,
    Colon,
    Dot,
    Literal(&'text str),
    Number(&'text str),
    InterpolationContent(&'text str),
}

/// Eucalypt lexer
pub struct StringLexer<'text> {
    /// Text content of string literal
    text: &'text str,
    /// wrapped iterator over characters
    chars: Peekable<Chars<'text>>,
    /// track source location as we progress
    location: ByteIndex,
    /// state: in interpolation
    in_interpolation: bool,
    /// state: in format specifier
    in_format_spec: bool,
    /// literal may be offset in larger content, record an offset to
    /// add to spans
    offset: ByteOffset,
}

pub const ONE_BYTE: ByteOffset = ByteOffset(1);

impl<'text> StringLexer<'text> {
    /// Create a lexer for parsing string pattern
    pub fn new(text: &'text str, offset: ByteOffset) -> Self {
        StringLexer {
            text,
            chars: text.chars().peekable(),
            location: ByteIndex(0),
            in_interpolation: false,
            in_format_spec: false,
            offset,
        }
    }

    /// move one character forward, returning index of the read char
    /// and the char
    fn bump(&mut self) -> Option<(ByteIndex, char)> {
        let next = self.chars.next();
        let loc = self.location;

        match next {
            Some(c) => {
                self.location += ByteOffset::from_char_len(c);
                Some((loc, c))
            }
            None => None,
        }
    }

    /// peek at the next character in the stream
    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    /// extract a slice of the input bounded by the specified locations
    fn slice(&self, start: ByteIndex, end: ByteIndex) -> &'text str {
        &self.text[start.to_usize()..end.to_usize()]
    }

    fn open(&mut self, i: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        match self.peek() {
            Some('{') => {
                self.bump();
                (i, Token::EscapedOpenBrace, i + ByteOffset(2))
            }
            _ => {
                self.in_interpolation = true;
                self.in_format_spec = false;
                (i, Token::OpenBrace, i + ONE_BYTE)
            }
        }
    }

    fn close(&mut self, i: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        match self.peek() {
            Some('}') => {
                self.bump();
                (i, Token::EscapedCloseBrace, i + ByteOffset(2))
            }
            _ => {
                self.in_interpolation = false;
                self.in_format_spec = false;
                (i, Token::CloseBrace, i + ONE_BYTE)
            }
        }
    }

    fn i_close(&mut self, i: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        self.in_interpolation = false;
        (i, Token::CloseBrace, i + ONE_BYTE)
    }

    fn literal(&mut self, b: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        let e = self.consume(|c| c != '{' && c != '}');
        (b, Token::Literal(self.slice(b, e)), e)
    }

    fn i_text(&mut self, b: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        let e = self.consume(|c| c != '}' && c != ':' && c != '.');
        (b, Token::InterpolationContent(self.slice(b, e)), e)
    }

    fn f_text(&mut self, b: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        let e = self.consume(|c| c != '}' && c != ':');
        (b, Token::InterpolationContent(self.slice(b, e)), e)
    }

    fn i_number(&mut self, b: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        let e = self.consume(|c| c.is_digit(10));
        (b, Token::Number(self.slice(b, e)), e)
    }

    fn i_colon(&mut self, i: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        self.in_format_spec = true;
        (i, Token::Colon, i + ONE_BYTE)
    }

    fn ws(&mut self) {
        self.consume(|c| c.is_whitespace());
    }

    /// consume characters matching predicate and return the next index
    fn consume<P>(&mut self, predicate: P) -> ByteIndex
    where
        P: Fn(char) -> bool,
    {
        loop {
            match self.peek() {
                Some(&c) if !predicate(c) => return self.location,
                None => return self.location,
                _ => (),
            };
            self.bump();
        }
    }
}

/// The type returned by the lexer iterator as required by lalrpop
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

impl<'text> Iterator for StringLexer<'text> {
    type Item = Spanned<Token<'text>, ByteIndex, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = if self.in_interpolation {
            if self.peek().unwrap_or(&'.').is_whitespace() {
                self.ws();
            }

            if self.in_format_spec {
                match self.bump() {
                    Some((i, ':')) => Some(Ok(self.i_colon(i))),
                    Some((i, '}')) => Some(Ok(self.i_close(i))),
                    Some((i, '{')) => Some(Ok(self.open(i))),
                    Some((i, _)) => Some(Ok(self.f_text(i))),
                    None => None,
                }
            } else {
                match self.bump() {
                    Some((i, '.')) => Some(Ok((i, Token::Dot, i + ONE_BYTE))),
                    Some((i, ':')) => Some(Ok(self.i_colon(i))),
                    Some((i, '}')) => Some(Ok(self.i_close(i))),
                    Some((i, '{')) => Some(Ok(self.open(i))),
                    Some((i, c)) if c.is_digit(10) => Some(Ok(self.i_number(i))),
                    Some((i, _)) => Some(Ok(self.i_text(i))),
                    None => None,
                }
            }
        } else {
            match self.bump() {
                Some((i, '{')) => Some(Ok(self.open(i))),
                Some((i, '}')) => Some(Ok(self.close(i))),
                Some((i, _)) => Some(Ok(self.literal(i))),
                None => None,
            }
        };

        if self.offset != ByteOffset(0) {
            match ret {
                Some(Ok((b, t, e))) => Some(Ok((b + self.offset, t, e + self.offset))),
                r => r,
            }
        } else {
            ret
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    fn lex(text: &'static str) -> Vec<Token<'static>> {
        let lexer = StringLexer::new(text, ByteOffset(0));
        let tokens: Vec<Token> = lexer
            .filter_map(|res| res.ok())
            .map(|(_, t, _)| t)
            .collect();
        tokens
    }

    #[test]
    fn test_pattern() {
        let tokens = lex("foo {x.y.z:f10:int} {1} {} {} bar");
        let expected = vec![
            Token::Literal("foo "),
            Token::OpenBrace,
            Token::InterpolationContent("x"),
            Token::Dot,
            Token::InterpolationContent("y"),
            Token::Dot,
            Token::InterpolationContent("z"),
            Token::Colon,
            Token::InterpolationContent("f10"),
            Token::Colon,
            Token::InterpolationContent("int"),
            Token::CloseBrace,
            Token::Literal(" "),
            Token::OpenBrace,
            Token::Number("1"),
            Token::CloseBrace,
            Token::Literal(" "),
            Token::OpenBrace,
            Token::CloseBrace,
            Token::Literal(" "),
            Token::OpenBrace,
            Token::CloseBrace,
            Token::Literal(" bar"),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_escapes() {
        assert_eq!(
            lex("}}{{{{"),
            vec![
                Token::EscapedCloseBrace,
                Token::EscapedOpenBrace,
                Token::EscapedOpenBrace
            ]
        );
    }

    #[test]
    fn test_escapes_and_interpolation() {
        assert_eq!(
            lex("{{{}}}"),
            vec![
                Token::EscapedOpenBrace,
                Token::OpenBrace,
                Token::CloseBrace,
                Token::EscapedCloseBrace
            ]
        );
    }

    #[test]
    fn test_references() {
        assert_eq!(
            lex("{x}+{y}={z}"),
            vec![
                Token::OpenBrace,
                Token::InterpolationContent("x"),
                Token::CloseBrace,
                Token::Literal("+"),
                Token::OpenBrace,
                Token::InterpolationContent("y"),
                Token::CloseBrace,
                Token::Literal("="),
                Token::OpenBrace,
                Token::InterpolationContent("z"),
                Token::CloseBrace,
            ]
        );
    }

    #[test]
    fn test_single_anaphor() {
        assert_eq!(
            lex("{0}"),
            vec![Token::OpenBrace, Token::Number("0"), Token::CloseBrace]
        );
    }

    #[test]
    fn test_format_specifier_with_dot() {
        assert_eq!(
            lex("{b:%8.2f}"),
            vec![
                Token::OpenBrace,
                Token::InterpolationContent("b"),
                Token::Colon,
                Token::InterpolationContent("%8.2f"),
                Token::CloseBrace
            ]
        )
    }
}
