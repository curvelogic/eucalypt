//! Lexer for Eucalypt
use codespan::{ByteIndex, ByteOffset, Span};
use std::{iter::Peekable, str::Chars};

use unic_ucd_category::GeneralCategory;

use super::kind::SyntaxKind::{self, *};

/// Eucalypt lexer
pub struct Lexer<C>
where
    C: Iterator<Item = char>,
{
    /// wrapped iterator over characters
    chars: Peekable<C>,
    /// track source location as we progress
    location: ByteIndex,
    /// keep last token for disambiguating OpenParen and OpenParenApply
    last_token: Option<SyntaxKind>,
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
            GeneralCategory::InitialPunctuation => false,
            GeneralCategory::FinalPunctuation => false,
            cat => (cat.is_symbol() || cat.is_punctuation()),
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
            GeneralCategory::InitialPunctuation => false,
            GeneralCategory::FinalPunctuation => false,
            cat => (cat.is_symbol() || cat.is_punctuation()),
        },
    }
}

fn is_reserved_open(c: char) -> bool {
    matches!(
        GeneralCategory::of(c),
        GeneralCategory::OpenPunctuation | GeneralCategory::InitialPunctuation
    )
}

fn is_reserved_close(c: char) -> bool {
    matches!(
        GeneralCategory::of(c),
        GeneralCategory::ClosePunctuation | GeneralCategory::FinalPunctuation
    )
}

pub const ONE_BYTE: ByteOffset = ByteOffset(1);

impl<'text> Lexer<Chars<'text>> {
    /// Construct a Lexer directly from text, with file_id to use in diagnostics
    pub fn from_text(text: &'text str) -> Self {
        Lexer {
            location: ByteIndex(0),
            chars: text.chars().peekable(),
            last_token: None,
        }
    }
}

impl<C> Lexer<C>
where
    C: Iterator<Item = char>,
{
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

    /// consume a line comment
    fn comment(&mut self, i: ByteIndex) -> (SyntaxKind, Span) {
        let e = self.consume(|c| c != '\n');
        (COMMENT, Span::new(i, e))
    }

    /// consume whitespace
    fn whitespace(&mut self, i: ByteIndex) -> (SyntaxKind, Span) {
        let e = self.consume(|c| c.is_whitespace());
        (WHITESPACE, Span::new(i, e))
    }

    /// consume a normal identifer
    fn normal(&mut self, i: ByteIndex) -> (SyntaxKind, Span) {
        let e = self.consume(&is_normal_continuation);
        (UNQUOTED_IDENTIFIER, Span::new(i, e))
    }

    /// consume a normal identifer
    fn squote(&mut self, i: ByteIndex) -> (SyntaxKind, Span) {
        match self.consume_delimited(|c| c != '\'') {
            Ok(e) => {
                self.bump();
                (SINGLE_QUOTE_IDENTIFIER, Span::new(i, e + ONE_BYTE))
            }
            Err(e) => (SINGLE_QUOTE_IDENTIFIER, Span::new(i, e)),
        }
    }

    /// consume an operator identifer
    fn oper(&mut self, i: ByteIndex) -> (SyntaxKind, Span) {
        let e = self.consume(&is_oper_continuation);
        (OPERATOR_IDENTIFIER, Span::new(i, e))
    }

    /// consume a string literal
    fn dquote(&mut self, i: ByteIndex) -> (SyntaxKind, Span) {
        match self.consume_delimited(|c| c != '"') {
            Ok(end) => {
                self.bump();
                (STRING, Span::new(i, end + ONE_BYTE))
            }
            Err(end) => (STRING, Span::new(i, end)),
        }
    }

    /// handle colon, returning either colon or symbol literal
    fn colon(&mut self, i: ByteIndex) -> (SyntaxKind, Span) {
        // colon in declaration
        let c: char = {
            match self.peek() {
                Some(ch) => *ch,
                None => return (COLON, Span::new(i, i + ONE_BYTE)),
            }
        };

        // symbol literal
        if is_normal_start(c) {
            if let Some((_, _)) = self.bump() {
                let e = self.consume(&is_normal_continuation);
                return (SYMBOL, Span::new(i, e));
            } else {
                panic!("peek and next disagree");
            }
        }

        // single quoted symbol literal
        if c == '\'' {
            if let Some((_, _)) = self.bump() {
                match self.consume_delimited(|c| c != '\'') {
                    Ok(e) => {
                        self.bump();
                        return (SYMBOL, Span::new(i, e + ONE_BYTE));
                    }
                    Err(e) => {
                        return (SYMBOL, Span::new(i, e));
                    }
                }
            } else {
                panic!("peek and next disagree");
            }
        }

        (COLON, Span::new(i, i + ONE_BYTE))
    }

    /// '-' can introduce a signed literal or be an operator start
    fn minus(&mut self, i: ByteIndex) -> (SyntaxKind, Span) {
        if self.lookahead(|c| c.is_digit(10)) {
            self.number(i)
        } else {
            self.oper(i)
        }
    }

    /// read int or float (currently no hex or sci notation)
    fn number(&mut self, i: ByteIndex) -> (SyntaxKind, Span) {
        let e = self.consume(|c| c.is_digit(10));
        if self.lookahead(|c| c == '.') {
            self.bump();
            let e = self.consume(|c| c.is_digit(10));
            (NUMBER, Span::new(i, e))
        } else {
            (NUMBER, Span::new(i, e))
        }
    }

    // read paren as either open paren or open an application
    fn paren(&mut self, i: ByteIndex) -> (SyntaxKind, Span) {
        let paren_offset = ByteOffset::from_char_len('\'');
        match self.last_token {
            Some(ref tok) if !tok.is_callable_terminal() => {
                (OPEN_PAREN, Span::new(i, i + paren_offset))
            }
            None => (OPEN_PAREN, Span::new(i, i + paren_offset)),
            _ => (OPEN_PAREN_APPLY, Span::new(i, i + paren_offset)),
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
    fn consume_delimited<P>(&mut self, predicate: P) -> Result<ByteIndex, ByteIndex>
    where
        P: Fn(char) -> bool,
    {
        loop {
            match self.peek() {
                Some(&c) if !predicate(c) => return Ok(self.location),
                None => return Err(self.location),
                _ => (),
            };
            self.bump();
        }
    }
}

impl<C> Iterator for Lexer<C>
where
    C: Iterator<Item = char>,
{
    type Item = (SyntaxKind, Span);

    fn next(&mut self) -> Option<Self::Item> {
        let ret = match self.bump() {
            Some((i, '{')) => Some((OPEN_BRACE, Span::new(i, i + ONE_BYTE))),
            Some((i, '}')) => Some((CLOSE_BRACE, Span::new(i, i + ONE_BYTE))),
            Some((i, '[')) => Some((OPEN_SQUARE, Span::new(i, i + ONE_BYTE))),
            Some((i, ']')) => Some((CLOSE_SQUARE, Span::new(i, i + ONE_BYTE))),
            Some((i, '(')) => Some(self.paren(i)),
            Some((i, ')')) => Some((CLOSE_PAREN, Span::new(i, i + ONE_BYTE))),
            Some((i, ',')) => Some((COMMA, Span::new(i, i + ONE_BYTE))),
            Some((i, '`')) => Some((BACKTICK, Span::new(i, i + ONE_BYTE))),
            Some((i, ':')) => Some(self.colon(i)),
            Some((i, '#')) => Some(self.comment(i)),
            Some((i, '\'')) => Some(self.squote(i)),
            Some((i, '"')) => Some(self.dquote(i)),
            Some((i, '-')) => Some(self.minus(i)),
            Some((i, c)) if c.is_digit(10) => Some(self.number(i)),
            Some((i, c)) if is_normal_start(c) => Some(self.normal(i)),
            Some((i, c)) if is_oper_start(c) => Some(self.oper(i)),
            Some((i, c)) if c.is_whitespace() => Some(self.whitespace(i)),
            Some((i, c)) if is_reserved_open(c) => Some((
                RESERVED_OPEN,
                Span::new(i, i + ByteOffset::from_char_len(c)),
            )),
            Some((i, c)) if is_reserved_close(c) => Some((
                RESERVED_CLOSE,
                Span::new(i, i + ByteOffset::from_char_len(c)),
            )),
            None => None,
            Some((i, c)) => Some((
                ERROR_RESERVED_CHAR,
                Span::new(i, i + ByteOffset::from_char_len(c)),
            )),
        };

        if let Some((tok, _)) = ret {
            self.last_token = Some(tok);
        };

        ret
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    fn test_lex(input: &str, tokens: Vec<(SyntaxKind, Span)>) {
        assert_eq!(
            Lexer::from_text(input).collect::<Vec<(SyntaxKind, Span)>>(),
            tokens
        );
    }

    #[test]
    fn test_atoms() {
        test_lex(
            "a 'b' :c",
            vec![
                (UNQUOTED_IDENTIFIER, Span::new(0, 1)),
                (WHITESPACE, Span::new(1, 2)),
                (SINGLE_QUOTE_IDENTIFIER, Span::new(2, 5)),
                (WHITESPACE, Span::new(5, 6)),
                (SYMBOL, Span::new(6, 8)),
            ],
        );
    }

    #[test]
    fn test_block() {
        test_lex(
            "{ x: :y f(x): :z (l && y): l + y }",
            vec![
                (OPEN_BRACE, Span::new(0, 1)),
                (WHITESPACE, Span::new(1, 2)),
                (UNQUOTED_IDENTIFIER, Span::new(2, 3)),
                (COLON, Span::new(3, 4)),
                (WHITESPACE, Span::new(4, 5)),
                (SYMBOL, Span::new(5, 7)),
                (WHITESPACE, Span::new(7, 8)),
                (UNQUOTED_IDENTIFIER, Span::new(8, 9)),
                (OPEN_PAREN_APPLY, Span::new(9, 10)),
                (UNQUOTED_IDENTIFIER, Span::new(10, 11)),
                (CLOSE_PAREN, Span::new(11, 12)),
                (COLON, Span::new(12, 13)),
                (WHITESPACE, Span::new(13, 14)),
                (SYMBOL, Span::new(14, 16)),
                (WHITESPACE, Span::new(16, 17)),
                (OPEN_PAREN, Span::new(17, 18)),
                (UNQUOTED_IDENTIFIER, Span::new(18, 19)),
                (WHITESPACE, Span::new(19, 20)),
                (OPERATOR_IDENTIFIER, Span::new(20, 22)),
                (WHITESPACE, Span::new(22, 23)),
                (UNQUOTED_IDENTIFIER, Span::new(23, 24)),
                (CLOSE_PAREN, Span::new(24, 25)),
                (COLON, Span::new(25, 26)),
                (WHITESPACE, Span::new(26, 27)),
                (UNQUOTED_IDENTIFIER, Span::new(27, 28)),
                (WHITESPACE, Span::new(28, 29)),
                (OPERATOR_IDENTIFIER, Span::new(29, 30)),
                (WHITESPACE, Span::new(30, 31)),
                (UNQUOTED_IDENTIFIER, Span::new(31, 32)),
                (WHITESPACE, Span::new(32, 33)),
                (CLOSE_BRACE, Span::new(33, 34)),
            ],
        );
    }

    #[test]
    fn test_various_literals() {
        test_lex(
            r#"["a string literal", 1235, -1233, 13.35, -11.35]"#,
            vec![
                (OPEN_SQUARE, Span::new(0, 1)),
                (STRING, Span::new(1, 19)),
                (COMMA, Span::new(19, 20)),
                (WHITESPACE, Span::new(20, 21)),
                (NUMBER, Span::new(21, 25)),
                (COMMA, Span::new(25, 26)),
                (WHITESPACE, Span::new(26, 27)),
                (NUMBER, Span::new(27, 32)),
                (COMMA, Span::new(32, 33)),
                (WHITESPACE, Span::new(33, 34)),
                (NUMBER, Span::new(34, 39)),
                (COMMA, Span::new(39, 40)),
                (WHITESPACE, Span::new(40, 41)),
                (NUMBER, Span::new(41, 47)),
                (CLOSE_SQUARE, Span::new(47, 48)),
            ],
        );
    }

    #[test]
    fn test_unterminated_squote() {
        test_lex("'abc", vec![(SINGLE_QUOTE_IDENTIFIER, Span::new(0, 4))]);
    }

    #[test]
    fn test_bad_chars() {
        test_lex("\0", vec![(ERROR_RESERVED_CHAR, Span::new(0, 1))]);
    }

    #[test]
    fn test_tight_commas() {
        test_lex(
            "3,",
            vec![(NUMBER, Span::new(0, 1)), (COMMA, Span::new(1, 2))],
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
            (WHITESPACE, Span::new(0, 1)),
            (UNQUOTED_IDENTIFIER, Span::new(1, 2)),
            (COLON, Span::new(2, 3)),
            (WHITESPACE, Span::new(3, 4)),
            (NUMBER, Span::new(4, 5)),
            (WHITESPACE, Span::new(5, 6)),
            (UNQUOTED_IDENTIFIER, Span::new(6, 7)),
            (COLON, Span::new(7, 8)),
            (WHITESPACE, Span::new(8, 9)),
            (SYMBOL, Span::new(9, 13)),
            (WHITESPACE, Span::new(13, 14)),
            (UNQUOTED_IDENTIFIER, Span::new(14, 15)),
            (COLON, Span::new(15, 16)),
            (WHITESPACE, Span::new(16, 17)),
            (STRING, Span::new(17, 22)),
            (WHITESPACE, Span::new(22, 23)),
            (UNQUOTED_IDENTIFIER, Span::new(23, 24)),
            (COLON, Span::new(24, 25)),
            (WHITESPACE, Span::new(25, 26)),
            (OPEN_BRACE, Span::new(26, 27)),
            (WHITESPACE, Span::new(27, 30)),
            (UNQUOTED_IDENTIFIER, Span::new(30, 31)),
            (COLON, Span::new(31, 32)),
            (WHITESPACE, Span::new(32, 33)),
            (UNQUOTED_IDENTIFIER, Span::new(33, 34)),
            (WHITESPACE, Span::new(34, 35)),
            (OPERATOR_IDENTIFIER, Span::new(35, 36)),
            (WHITESPACE, Span::new(36, 37)),
            (UNQUOTED_IDENTIFIER, Span::new(37, 38)),
            (WHITESPACE, Span::new(38, 39)),
            (OPERATOR_IDENTIFIER, Span::new(39, 40)),
            (WHITESPACE, Span::new(40, 41)),
            (UNQUOTED_IDENTIFIER, Span::new(41, 42)),
            (WHITESPACE, Span::new(42, 43)),
            (CLOSE_BRACE, Span::new(43, 44)),
            (WHITESPACE, Span::new(44, 45)),
        ];
        test_lex(ts, expected);
    }

    #[test]
    fn test_multibyte_locations() {
        let expected = vec![(UNQUOTED_IDENTIFIER, Span::new(0, 8))];
        test_lex(r#"•Ἄأ"#, expected);
    }

    #[test]
    fn test_operators_shun_parens() {
        test_lex(
            "%):",
            vec![
                (OPERATOR_IDENTIFIER, Span::new(0, 1)),
                (CLOSE_PAREN, Span::new(1, 2)),
                (COLON, Span::new(2, 3)),
            ],
        );
    }

    #[test]
    fn test_operators_shun_quotes() {
        test_lex(
            "%\"x\"",
            vec![
                (OPERATOR_IDENTIFIER, Span::new(0, 1)),
                (STRING, Span::new(1, 4)),
            ],
        );
    }
}
