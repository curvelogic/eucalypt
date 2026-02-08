//! Escape sequence processing for c-strings
//!
//! Processes C-style escape sequences:
//! - `\n`, `\r`, `\t`, `\\`, `\"`, `\0`, `\{`, `\}`
//! - `\xHH` (2 hex digits)
//! - `\uHHHH` (4 hex digits)
//! - `\UHHHHHHHH` (8 hex digits)

use std::fmt;

/// Error type for escape sequence processing
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EscapeError {
    /// Invalid escape sequence (e.g., \q)
    InvalidEscape(char),
    /// Truncated escape sequence (e.g., \x4 instead of \x41)
    TruncatedEscape(String),
    /// Invalid hex digit in escape
    InvalidHexDigit(char),
    /// Invalid Unicode code point (e.g., > 0x10FFFF or surrogate)
    InvalidCodePoint(u32),
    /// Escape at end of string
    UnexpectedEndOfString,
}

impl fmt::Display for EscapeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EscapeError::InvalidEscape(c) => write!(f, "invalid escape sequence: \\{}", c),
            EscapeError::TruncatedEscape(s) => write!(f, "truncated escape sequence: {}", s),
            EscapeError::InvalidHexDigit(c) => write!(f, "invalid hex digit: {}", c),
            EscapeError::InvalidCodePoint(cp) => {
                write!(f, "invalid Unicode code point: U+{:04X}", cp)
            }
            EscapeError::UnexpectedEndOfString => write!(f, "unexpected end of string in escape"),
        }
    }
}

/// Process escape sequences in a c-string
pub fn process_escapes(input: &str) -> Result<String, EscapeError> {
    let mut result = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some('0') => result.push('\0'),
                Some('{') => result.push('{'),
                Some('}') => result.push('}'),
                Some('x') => {
                    // Hex escape: \xHH
                    let hex = read_hex_digits(&mut chars, 2)?;
                    let byte = u8::from_str_radix(&hex, 16)
                        .map_err(|_| EscapeError::TruncatedEscape(format!("\\x{}", hex)))?;
                    result.push(byte as char);
                }
                Some('u') => {
                    // Unicode escape: \uHHHH
                    let hex = read_hex_digits(&mut chars, 4)?;
                    let code_point = u32::from_str_radix(&hex, 16)
                        .map_err(|_| EscapeError::TruncatedEscape(format!("\\u{}", hex)))?;
                    let c = char_from_code_point(code_point)?;
                    result.push(c);
                }
                Some('U') => {
                    // Long Unicode escape: \UHHHHHHHH
                    let hex = read_hex_digits(&mut chars, 8)?;
                    let code_point = u32::from_str_radix(&hex, 16)
                        .map_err(|_| EscapeError::TruncatedEscape(format!("\\U{}", hex)))?;
                    let c = char_from_code_point(code_point)?;
                    result.push(c);
                }
                Some(other) => {
                    return Err(EscapeError::InvalidEscape(other));
                }
                None => {
                    return Err(EscapeError::UnexpectedEndOfString);
                }
            }
        } else {
            result.push(c);
        }
    }

    Ok(result)
}

/// Read exactly `count` hex digits from the iterator
fn read_hex_digits(
    chars: &mut std::iter::Peekable<std::str::Chars>,
    count: usize,
) -> Result<String, EscapeError> {
    let mut hex = String::with_capacity(count);
    for _ in 0..count {
        match chars.next() {
            Some(c) if c.is_ascii_hexdigit() => hex.push(c),
            Some(c) => return Err(EscapeError::InvalidHexDigit(c)),
            None => {
                return Err(EscapeError::TruncatedEscape(format!(
                    "expected {} hex digits, got {}",
                    count,
                    hex.len()
                )))
            }
        }
    }
    Ok(hex)
}

/// Convert a Unicode code point to a char, checking validity
fn char_from_code_point(code_point: u32) -> Result<char, EscapeError> {
    char::from_u32(code_point).ok_or(EscapeError::InvalidCodePoint(code_point))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_escapes() {
        assert_eq!(process_escapes(r"hello\nworld").unwrap(), "hello\nworld");
        assert_eq!(process_escapes(r"hello\tworld").unwrap(), "hello\tworld");
        assert_eq!(process_escapes(r"hello\rworld").unwrap(), "hello\rworld");
        assert_eq!(process_escapes(r"hello\\world").unwrap(), "hello\\world");
        assert_eq!(
            process_escapes(r#"say \"hello\""#).unwrap(),
            "say \"hello\""
        );
        assert_eq!(process_escapes(r"null\0here").unwrap(), "null\0here");
    }

    #[test]
    fn test_brace_escapes() {
        assert_eq!(process_escapes(r"value: \{x\}").unwrap(), "value: {x}");
        assert_eq!(
            process_escapes(r#"JSON: \{\"key\": 1\}"#).unwrap(),
            r#"JSON: {"key": 1}"#
        );
    }

    #[test]
    fn test_hex_escapes() {
        assert_eq!(process_escapes(r"\x41\x42\x43").unwrap(), "ABC");
        assert_eq!(process_escapes(r"\x00").unwrap(), "\0");
        assert_eq!(process_escapes(r"\x7f").unwrap(), "\x7f");
    }

    #[test]
    fn test_unicode_escapes() {
        assert_eq!(process_escapes(r"\u0041").unwrap(), "A");
        assert_eq!(process_escapes(r"\u03B1").unwrap(), "α");
        assert_eq!(process_escapes(r"\U0001F600").unwrap(), "😀");
    }

    #[test]
    fn test_invalid_escape() {
        assert!(matches!(
            process_escapes(r"hello\qworld"),
            Err(EscapeError::InvalidEscape('q'))
        ));
    }

    #[test]
    fn test_truncated_hex() {
        assert!(matches!(
            process_escapes(r"hello\x4"),
            Err(EscapeError::TruncatedEscape(_))
        ));
    }

    #[test]
    fn test_invalid_unicode() {
        // Surrogate code points are invalid
        assert!(matches!(
            process_escapes(r"\uD800"),
            Err(EscapeError::InvalidCodePoint(_))
        ));
        // Code points > 0x10FFFF are invalid
        assert!(matches!(
            process_escapes(r"\UFFFFFFFF"),
            Err(EscapeError::InvalidCodePoint(_))
        ));
    }

    #[test]
    fn test_mixed_content() {
        assert_eq!(
            process_escapes(r"Hello\nWorld\t\u2764").unwrap(),
            "Hello\nWorld\t❤"
        );
    }

    #[test]
    fn test_no_escapes() {
        assert_eq!(process_escapes("hello world").unwrap(), "hello world");
    }

    #[test]
    fn test_empty_string() {
        assert_eq!(process_escapes("").unwrap(), "");
    }
}
