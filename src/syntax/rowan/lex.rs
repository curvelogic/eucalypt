//! Lexer for Eucalypt
use chrono::{DateTime, FixedOffset, NaiveDate, NaiveDateTime};
use codespan::{ByteIndex, ByteOffset, Span};
use std::{collections::VecDeque, iter::Peekable, str::Chars};

use unicode_general_category::{get_general_category, GeneralCategory};

use super::kind::SyntaxKind::{self, *};

/// Whether a Unicode general category is a symbol category
fn is_symbol_category(cat: GeneralCategory) -> bool {
    matches!(
        cat,
        GeneralCategory::MathSymbol
            | GeneralCategory::CurrencySymbol
            | GeneralCategory::ModifierSymbol
            | GeneralCategory::OtherSymbol
    )
}

/// Whether a Unicode general category is a punctuation category
fn is_punctuation_category(cat: GeneralCategory) -> bool {
    matches!(
        cat,
        GeneralCategory::ConnectorPunctuation
            | GeneralCategory::DashPunctuation
            | GeneralCategory::OpenPunctuation
            | GeneralCategory::ClosePunctuation
            | GeneralCategory::InitialPunctuation
            | GeneralCategory::FinalPunctuation
            | GeneralCategory::OtherPunctuation
    )
}

/// String prefix type for c-strings, r-strings and t-strings
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringPrefix {
    None,
    CString,
    RawString,
    TString,
}

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
    /// track whether we've seen whitespace since last non-whitespace token
    whitespace_since_last_token: bool,
    /// buffer for string pattern tokens
    token_buffer: VecDeque<(SyntaxKind, Span)>,
    /// pending string prefix (c or r) to be absorbed by next string
    pending_string_prefix: Option<(StringPrefix, ByteIndex)>,
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
    // Fast path for ASCII characters (most common case)
    if c.is_ascii() {
        match c {
            '.' | '!' | '@' | '£' | '%' | '^' | '&' | '*' | '|' | '>' | '<' | '/' | '+' | '='
            | '-' | '~' | ';' => true,
            '"' | '\'' => false,
            // Fast rejection for common ASCII non-operators
            'a'..='z'
            | 'A'..='Z'
            | '0'..='9'
            | '_'
            | ' '
            | '\t'
            | '\n'
            | '\r'
            | '('
            | ')'
            | '['
            | ']'
            | '{'
            | '}'
            | ','
            | ':' => false,
            // For other ASCII punctuation, check Unicode categories
            _ => {
                let cat = get_general_category(c);
                !matches!(
                    cat,
                    GeneralCategory::OpenPunctuation
                        | GeneralCategory::ClosePunctuation
                        | GeneralCategory::InitialPunctuation
                        | GeneralCategory::FinalPunctuation
                ) && (is_symbol_category(cat) || is_punctuation_category(cat))
            }
        }
    } else {
        // Slow path for non-ASCII (Unicode) characters
        match get_general_category(c) {
            GeneralCategory::OpenPunctuation => false,
            GeneralCategory::ClosePunctuation => false,
            GeneralCategory::InitialPunctuation => false,
            GeneralCategory::FinalPunctuation => false,
            cat => is_symbol_category(cat) || is_punctuation_category(cat),
        }
    }
}

/// is c a character which can continue an operator identifier?
fn is_oper_continuation(c: char) -> bool {
    // Fast path for ASCII characters (most common case)
    if c.is_ascii() {
        match c {
            '.' | '!' | '@' | '£' | '$' | '%' | '^' | '&' | '*' | '|' | '>' | '<' | '/' | '?'
            | '+' | '=' | '-' | '~' | ';' => true,
            '"' | '\'' | ',' | ':' | '_' => false,
            // Fast rejection for common ASCII non-operators
            'a'..='z'
            | 'A'..='Z'
            | '0'..='9'
            | ' '
            | '\t'
            | '\n'
            | '\r'
            | '('
            | ')'
            | '['
            | ']'
            | '{'
            | '}' => false,
            // For other ASCII punctuation, check Unicode categories
            _ => {
                let cat = get_general_category(c);
                !matches!(
                    cat,
                    GeneralCategory::OpenPunctuation
                        | GeneralCategory::ClosePunctuation
                        | GeneralCategory::InitialPunctuation
                        | GeneralCategory::FinalPunctuation
                ) && (is_symbol_category(cat) || is_punctuation_category(cat))
            }
        }
    } else {
        // Slow path for non-ASCII (Unicode) characters
        match get_general_category(c) {
            GeneralCategory::OpenPunctuation => false,
            GeneralCategory::ClosePunctuation => false,
            GeneralCategory::InitialPunctuation => false,
            GeneralCategory::FinalPunctuation => false,
            cat => is_symbol_category(cat) || is_punctuation_category(cat),
        }
    }
}

fn is_reserved_open(c: char) -> bool {
    // Fast path for ASCII - no ASCII characters are InitialPunctuation
    if c.is_ascii() {
        matches!(c, '(' | '[' | '{')
    } else {
        matches!(
            get_general_category(c),
            GeneralCategory::OpenPunctuation | GeneralCategory::InitialPunctuation
        )
    }
}

fn is_reserved_close(c: char) -> bool {
    // Fast path for ASCII - no ASCII characters are FinalPunctuation
    if c.is_ascii() {
        matches!(c, ')' | ']' | '}')
    } else {
        matches!(
            get_general_category(c),
            GeneralCategory::ClosePunctuation | GeneralCategory::FinalPunctuation
        )
    }
}

pub const ONE_BYTE: ByteOffset = ByteOffset(1);

impl<'text> Lexer<Chars<'text>> {
    /// Construct a Lexer directly from text, with file_id to use in diagnostics
    pub fn from_text(text: &'text str) -> Self {
        Lexer {
            location: ByteIndex(0),
            chars: text.chars().peekable(),
            last_token: None,
            whitespace_since_last_token: false,
            token_buffer: VecDeque::new(),
            pending_string_prefix: None,
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

    /// consume a normal identifer, detecting c-string and r-string prefixes
    fn normal(&mut self, i: ByteIndex, first_char: char) -> (SyntaxKind, Span) {
        let e = self.consume(is_normal_continuation);
        let span = Span::new(i, e);

        // Check if this is a single-char 'c' or 'r' identifier followed immediately by a quote
        if e.to_usize() - i.to_usize() == 1 {
            if let Some(&'"') = self.peek() {
                match first_char {
                    'c' => {
                        // This is a c-string prefix - consume the quote and handle it
                        self.bump(); // consume the opening quote
                        return self.dquote_with_prefix(i, StringPrefix::CString);
                    }
                    'r' => {
                        // This is an r-string prefix - consume the quote and handle it
                        self.bump(); // consume the opening quote
                        return self.dquote_with_prefix(i, StringPrefix::RawString);
                    }
                    't' => {
                        // This is a t-string (ZDT literal) prefix
                        self.bump(); // consume the opening quote
                        return self.dquote_with_prefix(i, StringPrefix::TString);
                    }
                    _ => {}
                }
            }
        }

        (UNQUOTED_IDENTIFIER, span)
    }

    /// consume a string literal or string pattern with a known prefix
    fn dquote_with_prefix(
        &mut self,
        prefix_start: ByteIndex,
        string_prefix: StringPrefix,
    ) -> (SyntaxKind, Span) {
        // Store the prefix so dquote can use it
        self.pending_string_prefix = Some((string_prefix, prefix_start));
        // The quote position is one byte after the prefix
        let quote_pos = self.location - ByteOffset(1);
        self.dquote(quote_pos)
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
        let e = self.consume(is_oper_continuation);
        (OPERATOR_IDENTIFIER, Span::new(i, e))
    }

    /// consume a string literal or string pattern
    fn dquote(&mut self, i: ByteIndex) -> (SyntaxKind, Span) {
        // Check if we have a pending string prefix (c or r)
        let prefix = self.pending_string_prefix.take();
        let (string_prefix, prefix_start) = match prefix {
            Some((p, start)) => (p, start),
            None => (StringPrefix::None, i),
        };

        // Collect the string content to check for interpolation
        let mut content = String::new();

        // Consume characters until closing quote or EOF
        loop {
            match self.peek() {
                Some(&'"') => {
                    self.bump(); // consume closing quote
                    break;
                }
                Some(&c) => {
                    content.push(c);
                    self.bump();
                }
                None => {
                    // Unterminated string - return appropriate kind
                    let kind = match string_prefix {
                        StringPrefix::None => STRING,
                        StringPrefix::CString => C_STRING,
                        StringPrefix::RawString => RAW_STRING,
                        StringPrefix::TString => T_STRING,
                    };
                    return (kind, Span::new(prefix_start, self.location));
                }
            }
        }

        let full_span = Span::new(prefix_start, self.location);

        // T-strings are ZDT literals — no interpolation, just validate and emit
        if string_prefix == StringPrefix::TString {
            return (T_STRING, full_span);
        }

        // Determine the token kinds based on prefix
        let (simple_kind, pattern_start_kind, pattern_end_kind) = match string_prefix {
            StringPrefix::None => (STRING, STRING_PATTERN_START, STRING_PATTERN_END),
            StringPrefix::CString => (C_STRING, C_STRING_PATTERN_START, C_STRING_PATTERN_END),
            StringPrefix::RawString => {
                (RAW_STRING, RAW_STRING_PATTERN_START, RAW_STRING_PATTERN_END)
            }
            StringPrefix::TString => unreachable!(),
        };

        // Check if this string contains interpolation
        // For c-strings, \{ and \} are escape sequences, not interpolation markers
        // For plain and raw strings, {{ and }} are escapes
        let has_interpolation = match string_prefix {
            StringPrefix::CString => {
                // In c-strings, look for unescaped braces
                self.content_has_unescaped_braces_cstring(&content)
            }
            StringPrefix::None | StringPrefix::RawString => {
                // In plain/raw strings, any brace triggers pattern mode
                content.contains('{') || content.contains('}')
            }
            StringPrefix::TString => unreachable!(),
        };

        if has_interpolation {
            // Use appropriate string pattern lexer based on prefix
            use super::string_lex::{CStringPatternLexer, StringPatternLexer};

            // Buffer the opening quote/prefix
            self.token_buffer.push_back((
                pattern_start_kind,
                Span::new(prefix_start, i + ByteOffset(1)),
            ));

            // Buffer all the detailed tokens from the string content
            let content_offset = ByteOffset(i.to_usize() as i64 + 1);
            match string_prefix {
                StringPrefix::CString => {
                    let string_lexer = CStringPatternLexer::new(&content, content_offset);
                    for (token_kind, span, _text) in string_lexer {
                        self.token_buffer.push_back((token_kind, span));
                    }
                }
                StringPrefix::None | StringPrefix::RawString => {
                    let string_lexer = StringPatternLexer::new(&content, content_offset);
                    for (token_kind, span, _text) in string_lexer {
                        self.token_buffer.push_back((token_kind, span));
                    }
                }
                StringPrefix::TString => unreachable!(),
            }

            // Buffer the closing quote
            self.token_buffer.push_back((
                pattern_end_kind,
                Span::new(self.location - ByteOffset(1), self.location),
            ));

            // Return the first buffered token (opening quote)
            self.token_buffer.pop_front().unwrap()
        } else {
            (simple_kind, full_span)
        }
    }

    /// Check if content has unescaped braces for c-string interpolation
    fn content_has_unescaped_braces_cstring(&self, content: &str) -> bool {
        let mut chars = content.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '\\' {
                // Skip the next character (it's escaped)
                chars.next();
            } else if c == '{' || c == '}' {
                return true;
            }
        }
        false
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
                let e = self.consume(is_normal_continuation);
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
        if self.lookahead(|c| c.is_ascii_digit()) {
            self.number(i)
        } else {
            self.oper(i)
        }
    }

    /// read int or float (currently no hex or sci notation)
    fn number(&mut self, i: ByteIndex) -> (SyntaxKind, Span) {
        let e = self.consume(|c| c.is_ascii_digit());
        if self.lookahead(|c| c == '.') {
            self.bump();
            let e = self.consume(|c| c.is_ascii_digit());
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
            _ => {
                // If there was whitespace since the last token, it's not an applytuple
                if self.whitespace_since_last_token {
                    (OPEN_PAREN, Span::new(i, i + paren_offset))
                } else {
                    (OPEN_PAREN_APPLY, Span::new(i, i + paren_offset))
                }
            }
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
        // First check if we have buffered tokens
        if let Some(token) = self.token_buffer.pop_front() {
            // Update tracking for non-trivial tokens
            if !token.0.is_trivial() {
                self.last_token = Some(token.0);
                self.whitespace_since_last_token = false;
            }
            return Some(token);
        }

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
            Some((i, c)) if c.is_ascii_digit() => Some(self.number(i)),
            Some((i, c)) if is_normal_start(c) => Some(self.normal(i, c)),
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
            if tok == WHITESPACE {
                self.whitespace_since_last_token = true;
            } else if !tok.is_trivial() {
                self.last_token = Some(tok);
                self.whitespace_since_last_token = false;
            }
        };

        ret
    }
}

/// Parse a ZDT timestamp string into a `DateTime<FixedOffset>`.
///
/// Supports the same formats as YAML timestamp import:
/// - RFC3339 with Z or offset (2023-01-15T10:30:00Z, 2023-01-15T10:30:00+05:00)
/// - ISO8601 without timezone (assumes UTC)
/// - Space separator instead of T
/// - Fractional seconds
/// - Date only (midnight UTC)
pub fn parse_zdt_literal(text: &str) -> Option<DateTime<FixedOffset>> {
    let utc = FixedOffset::east_opt(0).unwrap();

    // Try full RFC3339 (2023-01-15T10:30:00Z or 2023-01-15T10:30:00+05:00)
    if let Ok(dt) = DateTime::parse_from_rfc3339(text) {
        return Some(dt);
    }

    // Try RFC3339 with space separator (2023-01-15 10:30:00Z)
    if let Ok(dt) = DateTime::parse_from_str(text, "%Y-%m-%d %H:%M:%S%:z") {
        return Some(dt);
    }
    if let Ok(dt) = DateTime::parse_from_str(text, "%Y-%m-%d %H:%M:%SZ") {
        return Some(DateTime::from_naive_utc_and_offset(dt.naive_utc(), utc));
    }

    // Try NaiveDateTime with T separator - assume UTC
    if let Ok(ndt) = NaiveDateTime::parse_from_str(text, "%Y-%m-%dT%H:%M:%S") {
        return Some(DateTime::from_naive_utc_and_offset(ndt, utc));
    }

    // Try NaiveDateTime with T separator and fractional seconds
    if let Ok(ndt) = NaiveDateTime::parse_from_str(text, "%Y-%m-%dT%H:%M:%S%.f") {
        return Some(DateTime::from_naive_utc_and_offset(ndt, utc));
    }

    // Try NaiveDateTime with space separator - assume UTC
    if let Ok(ndt) = NaiveDateTime::parse_from_str(text, "%Y-%m-%d %H:%M:%S") {
        return Some(DateTime::from_naive_utc_and_offset(ndt, utc));
    }

    // Try NaiveDateTime with space separator and fractional seconds
    if let Ok(ndt) = NaiveDateTime::parse_from_str(text, "%Y-%m-%d %H:%M:%S%.f") {
        return Some(DateTime::from_naive_utc_and_offset(ndt, utc));
    }

    // Try date-only format (2023-01-15) - midnight UTC
    if let Ok(nd) = NaiveDate::parse_from_str(text, "%Y-%m-%d") {
        let ndt = nd.and_hms_opt(0, 0, 0)?;
        return Some(DateTime::from_naive_utc_and_offset(ndt, utc));
    }

    None
}

/// Extract the content of a t-string token text (strip `t"` prefix and `"` suffix).
pub fn tstring_content(token_text: &str) -> Option<&str> {
    token_text
        .strip_prefix("t\"")
        .and_then(|s| s.strip_suffix('\"'))
}

/// Normalise a ZDT literal for ZDT.PARSE consumption.
///
/// Converts space-separated timestamps to T-separated, and appends Z
/// for timestamps without timezone. Date-only values are left as-is.
pub fn normalize_zdt_for_parse(text: &str) -> String {
    // Replace space separator with T
    let normalized = if text.contains(' ') && !text.contains('T') {
        text.replacen(' ', "T", 1)
    } else {
        text.to_string()
    };

    // If there's a time component but no timezone, append Z for UTC
    if let Some(t_pos) = normalized.find('T') {
        let after_t = &normalized[t_pos..];
        if !after_t.ends_with('Z') && !after_t.contains('+') && !after_t.contains('-') {
            return format!("{normalized}Z");
        }
    }

    normalized
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

    #[test]
    fn test_nullary_operator_symbols() {
        // ∅ (U+2205 EMPTY SET) is a MathSymbol, should be recognized as operator
        test_lex(
            "(∅):",
            vec![
                (OPEN_PAREN, Span::new(0, 1)),
                (OPERATOR_IDENTIFIER, Span::new(1, 4)), // ∅ is 3 bytes in UTF-8
                (CLOSE_PAREN, Span::new(4, 5)),
                (COLON, Span::new(5, 6)),
            ],
        );
    }

    #[test]
    fn test_cstring_simple() {
        // c"hello" should be lexed as a C_STRING
        test_lex(r#"c"hello""#, vec![(C_STRING, Span::new(0, 8))]);
    }

    #[test]
    fn test_rstring_simple() {
        // r"hello" should be lexed as a RAW_STRING
        test_lex(r#"r"hello""#, vec![(RAW_STRING, Span::new(0, 8))]);
    }

    #[test]
    fn test_cstring_with_space() {
        // c "hello" (with space) should be identifier + string
        test_lex(
            r#"c "hello""#,
            vec![
                (UNQUOTED_IDENTIFIER, Span::new(0, 1)),
                (WHITESPACE, Span::new(1, 2)),
                (STRING, Span::new(2, 9)),
            ],
        );
    }

    #[test]
    fn test_tstring_simple() {
        // t"2023-01-15T10:30:00Z" should be lexed as T_STRING
        test_lex(
            r#"t"2023-01-15T10:30:00Z""#,
            vec![(T_STRING, Span::new(0, 23))],
        );
    }

    #[test]
    fn test_tstring_date_only() {
        // t"2023-01-15" should be lexed as T_STRING
        test_lex(r#"t"2023-01-15""#, vec![(T_STRING, Span::new(0, 13))]);
    }

    #[test]
    fn test_tstring_with_offset() {
        // t"2023-01-15T10:30:00+05:00" should be lexed as T_STRING
        test_lex(
            r#"t"2023-01-15T10:30:00+05:00""#,
            vec![(T_STRING, Span::new(0, 28))],
        );
    }

    #[test]
    fn test_tstring_with_space() {
        // t "2023-01-15" (with space) should be identifier + string
        test_lex(
            r#"t "2023-01-15""#,
            vec![
                (UNQUOTED_IDENTIFIER, Span::new(0, 1)),
                (WHITESPACE, Span::new(1, 2)),
                (STRING, Span::new(2, 14)),
            ],
        );
    }

    #[test]
    fn test_tstring_no_interpolation() {
        // t-strings with braces should NOT trigger interpolation
        test_lex(
            r#"t"2023-01-15T10:30:00Z""#,
            vec![(T_STRING, Span::new(0, 23))],
        );
    }

    #[test]
    fn test_tstring_in_context() {
        // t-string used as a declaration value
        test_lex(
            r#"x: t"2023-01-15""#,
            vec![
                (UNQUOTED_IDENTIFIER, Span::new(0, 1)),
                (COLON, Span::new(1, 2)),
                (WHITESPACE, Span::new(2, 3)),
                (T_STRING, Span::new(3, 16)),
            ],
        );
    }

    #[test]
    fn test_parse_zdt_valid_formats() {
        use super::parse_zdt_literal;

        // RFC3339 with Z
        assert!(parse_zdt_literal("2023-01-15T10:30:00Z").is_some());
        // RFC3339 with offset
        assert!(parse_zdt_literal("2023-01-15T10:30:00+05:00").is_some());
        // No timezone (assumes UTC)
        assert!(parse_zdt_literal("2023-01-15T10:30:00").is_some());
        // Space separator
        assert!(parse_zdt_literal("2023-01-15 10:30:00").is_some());
        // Fractional seconds
        assert!(parse_zdt_literal("2023-01-15T10:30:00.123Z").is_some());
        // Date only
        assert!(parse_zdt_literal("2023-01-15").is_some());
        // Leap year
        assert!(parse_zdt_literal("2024-02-29").is_some());
    }

    #[test]
    fn test_parse_zdt_invalid_formats() {
        use super::parse_zdt_literal;

        // Invalid day
        assert!(parse_zdt_literal("2023-02-30").is_none());
        // Invalid month
        assert!(parse_zdt_literal("2023-13-01").is_none());
        // Non-leap year Feb 29
        assert!(parse_zdt_literal("2023-02-29").is_none());
        // Malformed
        assert!(parse_zdt_literal("not-a-date").is_none());
        // Empty
        assert!(parse_zdt_literal("").is_none());
    }
}
