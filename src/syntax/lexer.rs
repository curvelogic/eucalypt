//! Lexer for Eucalypt
use crate::syntax::error::SyntaxError;
use codespan::{ByteIndex, ByteOffset, Span};
use codespan_reporting::files::{Files, SimpleFiles};
use std::{fmt::Display, iter::Peekable, str::Chars};

use unic_ucd_category::GeneralCategory;

/// Tokens in the eucalypt syntax
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Token<'text> {
    OpenBrace,
    CloseBrace,
    Backtick,
    Colon,
    OpenSquare,
    CloseSquare,
    Comma,
    Whitespace(&'text str),
    Comment(&'text str),
    OpenParen,
    OpenParenApply,
    CloseParen,
    NormalIdentifier(&'text str),
    OperatorIdentifier(&'text str),
    SingleQuoteIdentifier(&'text str),
    Number(&'text str),
    String(&'text str),
    Symbol(&'text str),
}

impl Token<'_> {
    /// return true if this is a character which when preceding a '('
    /// indicates a paren expression rather than an apply tuple
    pub fn new_context(&self) -> bool {
        matches!(
            self,
            Token::OpenParen
                | Token::OpenParenApply
                | Token::OpenSquare
                | Token::OpenBrace
                | Token::Whitespace(_)
                | Token::OperatorIdentifier(_)
        )
    }
}

/// Eucalypt lexer
pub struct Lexer<'text, N, T, C>
where
    N: AsRef<str>,
    N: Clone,
    N: Display,
    T: AsRef<str>,
    C: Iterator<Item = char>,
{
    /// reference to the codespan files
    files: &'text SimpleFiles<N, T>,
    /// usize within Files
    file_id: usize,
    /// wrapped iterator over characters
    chars: Peekable<C>,
    /// track source location as we progress
    location: ByteIndex,
    /// keep last token for disambiguating OpenParen and OpenParenApply
    last_token: Option<Token<'text>>,
}

/// is c a character which can start a normal identifier?
pub fn is_normal_start(c: char) -> bool {
    match c {
        '•' => true,
        '$' => true,
        '?' => true,
        '_' => true,
        _ => c.is_alphabetic(),
    }
}

/// is c a character which can continue a normal identifier?
pub fn is_normal_continuation(c: char) -> bool {
    match c {
        '$' => true,
        '?' => true,
        '_' => true,
        '!' => true,
        '-' => true,
        '*' => true,
        _ => c.is_alphanumeric(),
    }
}

/// is c a character which can start an operator identifier?
fn is_oper_start(c: char) -> bool {
    match c {
        '.' => true,
        '!' => true,
        '@' => true,
        '£' => true,
        '%' => true,
        '^' => true,
        '&' => true,
        '*' => true,
        '|' => true,
        '>' => true,
        '<' => true,
        '/' => true,
        '+' => true,
        '=' => true,
        '-' => true,
        '~' => true,
        ';' => true,
        '"' => false,
        '\'' => false,
        _ => match GeneralCategory::of(c) {
            GeneralCategory::OpenPunctuation => false,
            GeneralCategory::ClosePunctuation => false,
            cat => cat.is_symbol() || cat.is_punctuation(),
        },
    }
}

/// is c a character which can continue an operator identifier?
fn is_oper_continuation(c: char) -> bool {
    match c {
        '.' => true,
        '!' => true,
        '@' => true,
        '£' => true,
        '$' => true,
        '%' => true,
        '^' => true,
        '&' => true,
        '*' => true,
        '|' => true,
        '>' => true,
        '<' => true,
        '/' => true,
        '?' => true,
        '+' => true,
        '=' => true,
        '-' => true,
        '~' => true,
        ';' => true,
        '"' => false,
        '\'' => false,
        ',' => false,
        ':' => false,
        '_' => false,
        _ => match GeneralCategory::of(c) {
            GeneralCategory::OpenPunctuation => false,
            GeneralCategory::ClosePunctuation => false,
            cat => cat.is_symbol() || cat.is_punctuation(),
        },
    }
}

pub const ONE_BYTE: ByteOffset = ByteOffset(1);

impl<'text, N, T> Lexer<'text, N, T, Chars<'text>>
where
    N: AsRef<str>,
    N: Clone,
    N: Display,
    T: AsRef<str>,
{
    /// Construct a Lexer from a codespan::Files database and a
    /// usize
    pub fn from_file_id(files: &'text SimpleFiles<N, T>, file_id: usize) -> Self {
        Lexer {
            files,
            file_id,
            location: ByteIndex(0),
            chars: files.source(file_id).unwrap().chars().peekable(),
            last_token: None,
        }
    }
}

impl<'text, N, T, C> Lexer<'text, N, T, C>
where
    N: AsRef<str>,
    N: Clone,
    N: Display,
    T: AsRef<str>,
    C: Iterator<Item = char>,
{
    /// Access file_id for error reporting etc.
    pub fn file_id(&self) -> usize {
        self.file_id
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
        &self.files.source(self.file_id).unwrap()[start.to_usize()..end.to_usize()]
    }

    /// consume a line comment
    fn comment(&mut self, i: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        let e = self.consume(|c| c != '\n');
        (
            i,
            Token::Comment(self.slice(i + ByteOffset::from_char_len('\n'), e)),
            e,
        )
    }

    /// consume whitespace
    fn whitespace(&mut self, i: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        let e = self.consume(|c| c.is_whitespace());
        (i, Token::Whitespace(self.slice(i, e)), e)
    }

    /// consume a normal identifer
    fn normal(&mut self, i: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        let e = self.consume(is_normal_continuation);
        (i, Token::NormalIdentifier(self.slice(i, e)), e)
    }

    /// consume a normal identifer
    fn squote(
        &mut self,
        i: ByteIndex,
    ) -> Result<(ByteIndex, Token<'text>, ByteIndex), SyntaxError> {
        let e = self.consume_delimited(|c| c != '\'')?;
        self.bump();
        Ok((
            i,
            Token::SingleQuoteIdentifier(self.slice(i + ONE_BYTE, e)),
            e + ONE_BYTE,
        ))
    }

    /// consume an operator identifer
    fn oper(&mut self, i: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        let e = self.consume(is_oper_continuation);
        (i, Token::OperatorIdentifier(self.slice(i, e)), e)
    }

    /// consume a string literal
    fn dquote(
        &mut self,
        i: ByteIndex,
    ) -> Result<(ByteIndex, Token<'text>, ByteIndex), SyntaxError> {
        let e = self.consume_delimited(|c| c != '"')?;
        self.bump();
        Ok((i, Token::String(self.slice(i + ONE_BYTE, e)), e + ONE_BYTE))
    }

    /// handle colon, returning either colon or symbol literal
    fn colon(&mut self, i: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        let c: char = {
            match self.peek() {
                Some(ch) => *ch,
                None => return (i, Token::Colon, i + ONE_BYTE),
            }
        };

        if is_normal_start(c) {
            if let Some((b, _)) = self.bump() {
                let e = self.consume(is_normal_continuation);
                return (i, Token::Symbol(self.slice(b, e)), e);
            } else {
                panic!("peek and next disagree");
            }
        }

        if c == '\'' {
            if let Some((b, _)) = self.bump() {
                let e = self.consume(|c| c != '\'');
                self.bump();
                return (i, Token::Symbol(self.slice(b + ONE_BYTE, e)), e + ONE_BYTE);
            } else {
                panic!("peek and next disagree");
            }
        }

        (i, Token::Colon, i + ONE_BYTE)
    }

    /// '-' can introduce a signed literal or be an operator start
    fn minus(&mut self, i: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        if self.lookahead(|c| c.is_ascii_digit()) {
            self.number(i)
        } else {
            self.oper(i)
        }
    }

    /// read int or float (currently no hex or sci notation)
    fn number(&mut self, i: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        let e = self.consume(|c| c.is_ascii_digit());
        if self.lookahead(|c| c == '.') {
            self.bump();
            let e = self.consume(|c| c.is_ascii_digit());
            (i, Token::Number(self.slice(i, e)), e)
        } else {
            (i, Token::Number(self.slice(i, e)), e)
        }
    }

    fn paren(&mut self, i: ByteIndex) -> (ByteIndex, Token<'text>, ByteIndex) {
        let paren_offset = ByteOffset::from_char_len('\'');
        match self.last_token {
            Some(ref tok) if tok.new_context() => (i, Token::OpenParen, i + paren_offset),
            None => (i, Token::OpenParen, i + paren_offset),
            _ => (i, Token::OpenParenApply, i + paren_offset),
        }
    }

    /// check whether next character matches supplied predicate
    fn lookahead<P>(&mut self, predicate: P) -> bool
    where
        P: Fn(char) -> bool,
    {
        match self.peek() {
            Some(&c) => predicate(c),
            None => false,
        }
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

    /// consume characters matching predicate but return specified
    /// error on EOF
    fn consume_delimited<P>(&mut self, predicate: P) -> Result<ByteIndex, SyntaxError>
    where
        P: Fn(char) -> bool,
    {
        loop {
            match self.peek() {
                Some(&c) if !predicate(c) => return Ok(self.location),
                None => {
                    return Err(SyntaxError::UnexpectedEndOfInput(
                        self.file_id,
                        Span::new(self.location, self.location),
                    ))
                }
                _ => (),
            };
            self.bump();
        }
    }
}

/// The type returned by the lexer iterator as required by lalrpop
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

impl<'text, N, T, C> Iterator for Lexer<'text, N, T, C>
where
    N: AsRef<str>,
    N: Clone,
    N: std::fmt::Display,
    T: AsRef<str>,
    C: Iterator<Item = char>,
{
    type Item = Spanned<Token<'text>, ByteIndex, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = match self.bump() {
            Some((i, '{')) => Some(Ok((i, Token::OpenBrace, i + ONE_BYTE))),
            Some((i, '}')) => Some(Ok((i, Token::CloseBrace, i + ONE_BYTE))),
            Some((i, '[')) => Some(Ok((i, Token::OpenSquare, i + ONE_BYTE))),
            Some((i, ']')) => Some(Ok((i, Token::CloseSquare, i + ONE_BYTE))),
            Some((i, '(')) => Some(Ok(self.paren(i))),
            Some((i, ')')) => Some(Ok((i, Token::CloseParen, i + ONE_BYTE))),
            Some((i, ',')) => Some(Ok((i, Token::Comma, i + ONE_BYTE))),
            Some((i, '`')) => Some(Ok((i, Token::Backtick, i + ONE_BYTE))),
            Some((i, ':')) => Some(Ok(self.colon(i))),
            Some((i, '#')) => Some(Ok(self.comment(i))),
            Some((i, '\'')) => Some(self.squote(i)),
            Some((i, '"')) => Some(self.dquote(i)),
            Some((i, '-')) => Some(Ok(self.minus(i))),
            Some((i, c)) if c.is_ascii_digit() => Some(Ok(self.number(i))),
            Some((i, c)) if is_normal_start(c) => Some(Ok(self.normal(i))),
            Some((i, c)) if is_oper_start(c) => Some(Ok(self.oper(i))),
            Some((i, c)) if c.is_whitespace() => Some(Ok(self.whitespace(i))),
            None => None,
            Some((i, c)) => Some(Err(SyntaxError::UnexpectedCharacter(self.file_id, c, i))),
        };

        if let Some(Ok((_, ref tok, _))) = ret {
            self.last_token = Some(tok.clone());
        };

        ret
    }
}

#[cfg(test)]
pub mod tests {
    use super::Token::*;
    use super::*;

    pub fn test_parse(input: &str, tokens: Vec<Spanned<Token<'_>, ByteIndex, SyntaxError>>) {
        let mut files = SimpleFiles::new();
        let file_id = files.add("test".to_string(), input.to_string());
        assert_eq!(
            Lexer::from_file_id(&files, file_id)
                .collect::<Vec<Spanned<Token<'_>, ByteIndex, SyntaxError>>>(),
            tokens
        );
    }

    pub fn fake_file_id() -> usize {
        SimpleFiles::new().add("", "")
    }

    #[test]
    fn test_atoms() {
        test_parse(
            "a 'b' :c",
            vec![
                Ok((ByteIndex(0), NormalIdentifier("a"), ByteIndex(1))),
                Ok((ByteIndex(1), Whitespace(" "), ByteIndex(2))),
                Ok((ByteIndex(2), SingleQuoteIdentifier("b"), ByteIndex(5))),
                Ok((ByteIndex(5), Whitespace(" "), ByteIndex(6))),
                Ok((ByteIndex(6), Symbol("c"), ByteIndex(8))),
            ],
        );
    }

    #[test]
    fn test_block() {
        test_parse(
            "{ x: :y f(x): :z (l && y): l + y }",
            vec![
                Ok((ByteIndex(0), OpenBrace, ByteIndex(1))),
                Ok((ByteIndex(1), Whitespace(" "), ByteIndex(2))),
                Ok((ByteIndex(2), NormalIdentifier("x"), ByteIndex(3))),
                Ok((ByteIndex(3), Colon, ByteIndex(4))),
                Ok((ByteIndex(4), Whitespace(" "), ByteIndex(5))),
                Ok((ByteIndex(5), Symbol("y"), ByteIndex(7))),
                Ok((ByteIndex(7), Whitespace(" "), ByteIndex(8))),
                Ok((ByteIndex(8), NormalIdentifier("f"), ByteIndex(9))),
                Ok((ByteIndex(9), OpenParenApply, ByteIndex(10))),
                Ok((ByteIndex(10), NormalIdentifier("x"), ByteIndex(11))),
                Ok((ByteIndex(11), CloseParen, ByteIndex(12))),
                Ok((ByteIndex(12), Colon, ByteIndex(13))),
                Ok((ByteIndex(13), Whitespace(" "), ByteIndex(14))),
                Ok((ByteIndex(14), Symbol("z"), ByteIndex(16))),
                Ok((ByteIndex(16), Whitespace(" "), ByteIndex(17))),
                Ok((ByteIndex(17), OpenParen, ByteIndex(18))),
                Ok((ByteIndex(18), NormalIdentifier("l"), ByteIndex(19))),
                Ok((ByteIndex(19), Whitespace(" "), ByteIndex(20))),
                Ok((ByteIndex(20), OperatorIdentifier("&&"), ByteIndex(22))),
                Ok((ByteIndex(22), Whitespace(" "), ByteIndex(23))),
                Ok((ByteIndex(23), NormalIdentifier("y"), ByteIndex(24))),
                Ok((ByteIndex(24), CloseParen, ByteIndex(25))),
                Ok((ByteIndex(25), Colon, ByteIndex(26))),
                Ok((ByteIndex(26), Whitespace(" "), ByteIndex(27))),
                Ok((ByteIndex(27), NormalIdentifier("l"), ByteIndex(28))),
                Ok((ByteIndex(28), Whitespace(" "), ByteIndex(29))),
                Ok((ByteIndex(29), OperatorIdentifier("+"), ByteIndex(30))),
                Ok((ByteIndex(30), Whitespace(" "), ByteIndex(31))),
                Ok((ByteIndex(31), NormalIdentifier("y"), ByteIndex(32))),
                Ok((ByteIndex(32), Whitespace(" "), ByteIndex(33))),
                Ok((ByteIndex(33), CloseBrace, ByteIndex(34))),
            ],
        );
    }

    #[test]
    fn test_various_literals() {
        test_parse(
            r#"["a string literal", 1235, -1233, 13.35, -11.35]"#,
            vec![
                Ok((ByteIndex(0), OpenSquare, ByteIndex(1))),
                Ok((ByteIndex(1), String("a string literal"), ByteIndex(19))),
                Ok((ByteIndex(19), Comma, ByteIndex(20))),
                Ok((ByteIndex(20), Whitespace(" "), ByteIndex(21))),
                Ok((ByteIndex(21), Number("1235"), ByteIndex(25))),
                Ok((ByteIndex(25), Comma, ByteIndex(26))),
                Ok((ByteIndex(26), Whitespace(" "), ByteIndex(27))),
                Ok((ByteIndex(27), Number("-1233"), ByteIndex(32))),
                Ok((ByteIndex(32), Comma, ByteIndex(33))),
                Ok((ByteIndex(33), Whitespace(" "), ByteIndex(34))),
                Ok((ByteIndex(34), Number("13.35"), ByteIndex(39))),
                Ok((ByteIndex(39), Comma, ByteIndex(40))),
                Ok((ByteIndex(40), Whitespace(" "), ByteIndex(41))),
                Ok((ByteIndex(41), Number("-11.35"), ByteIndex(47))),
                Ok((ByteIndex(47), CloseSquare, ByteIndex(48))),
            ],
        );
    }

    #[test]
    fn test_unterminated_squote() {
        test_parse(
            "'abc",
            vec![Err(SyntaxError::UnexpectedEndOfInput(
                fake_file_id(),
                Span::new(ByteIndex(4), ByteIndex(4)),
            ))],
        );
    }

    #[test]
    fn test_bad_chars() {
        test_parse(
            "\0",
            vec![Err(SyntaxError::UnexpectedCharacter(
                fake_file_id(),
                '\0',
                ByteIndex(0),
            ))],
        );
    }

    #[test]
    fn test_tight_commas() {
        test_parse(
            "3,",
            vec![
                Ok((ByteIndex(0), Number("3"), ByteIndex(1))),
                Ok((ByteIndex(1), Comma, ByteIndex(2))),
            ],
        );
    }

    #[test]
    fn test_locations() {
        let ts = r#"
a: 6
b: :foo
c: "bar"
d: {
  e: x + y + z
}
"#;
        let expected = vec![
            Ok((ByteIndex(0), Whitespace("\n"), ByteIndex(1))),
            Ok((ByteIndex(1), NormalIdentifier("a"), ByteIndex(2))),
            Ok((ByteIndex(2), Colon, ByteIndex(3))),
            Ok((ByteIndex(3), Whitespace(" "), ByteIndex(4))),
            Ok((ByteIndex(4), Number("6"), ByteIndex(5))),
            Ok((ByteIndex(5), Whitespace("\n"), ByteIndex(6))),
            Ok((ByteIndex(6), NormalIdentifier("b"), ByteIndex(7))),
            Ok((ByteIndex(7), Colon, ByteIndex(8))),
            Ok((ByteIndex(8), Whitespace(" "), ByteIndex(9))),
            Ok((ByteIndex(9), Symbol("foo"), ByteIndex(13))),
            Ok((ByteIndex(13), Whitespace("\n"), ByteIndex(14))),
            Ok((ByteIndex(14), NormalIdentifier("c"), ByteIndex(15))),
            Ok((ByteIndex(15), Colon, ByteIndex(16))),
            Ok((ByteIndex(16), Whitespace(" "), ByteIndex(17))),
            Ok((ByteIndex(17), String("bar"), ByteIndex(22))),
            Ok((ByteIndex(22), Whitespace("\n"), ByteIndex(23))),
            Ok((ByteIndex(23), NormalIdentifier("d"), ByteIndex(24))),
            Ok((ByteIndex(24), Colon, ByteIndex(25))),
            Ok((ByteIndex(25), Whitespace(" "), ByteIndex(26))),
            Ok((ByteIndex(26), OpenBrace, ByteIndex(27))),
            Ok((ByteIndex(27), Whitespace("\n  "), ByteIndex(30))),
            Ok((ByteIndex(30), NormalIdentifier("e"), ByteIndex(31))),
            Ok((ByteIndex(31), Colon, ByteIndex(32))),
            Ok((ByteIndex(32), Whitespace(" "), ByteIndex(33))),
            Ok((ByteIndex(33), NormalIdentifier("x"), ByteIndex(34))),
            Ok((ByteIndex(34), Whitespace(" "), ByteIndex(35))),
            Ok((ByteIndex(35), OperatorIdentifier("+"), ByteIndex(36))),
            Ok((ByteIndex(36), Whitespace(" "), ByteIndex(37))),
            Ok((ByteIndex(37), NormalIdentifier("y"), ByteIndex(38))),
            Ok((ByteIndex(38), Whitespace(" "), ByteIndex(39))),
            Ok((ByteIndex(39), OperatorIdentifier("+"), ByteIndex(40))),
            Ok((ByteIndex(40), Whitespace(" "), ByteIndex(41))),
            Ok((ByteIndex(41), NormalIdentifier("z"), ByteIndex(42))),
            Ok((ByteIndex(42), Whitespace("\n"), ByteIndex(43))),
            Ok((ByteIndex(43), CloseBrace, ByteIndex(44))),
            Ok((ByteIndex(44), Whitespace("\n"), ByteIndex(45))),
        ];
        test_parse(ts, expected);
    }

    #[test]
    fn test_multibyte_locations() {
        let expected = vec![Ok((ByteIndex(0), NormalIdentifier("•Ἄأ"), ByteIndex(8)))];
        test_parse(r#"•Ἄأ"#, expected);
    }

    #[test]
    fn test_operators_shun_parens() {
        test_parse(
            "%):",
            vec![
                Ok((ByteIndex(0), OperatorIdentifier("%"), ByteIndex(1))),
                Ok((ByteIndex(1), CloseParen, ByteIndex(2))),
                Ok((ByteIndex(2), Colon, ByteIndex(3))),
            ],
        );
    }

    #[test]
    fn test_operators_shun_quotes() {
        test_parse(
            "%\"x\"",
            vec![
                Ok((ByteIndex(0), OperatorIdentifier("%"), ByteIndex(1))),
                Ok((ByteIndex(1), String("x"), ByteIndex(4))),
            ],
        );
    }
}
