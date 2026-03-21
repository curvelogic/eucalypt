//! Unicode bracket pair detection for idiot brackets.
//!
//! Idiot brackets allow user-defined bracket pair expressions, e.g. `«x»` or `⟦x⟧`.
//!
//! Bracket pair recognition uses Unicode general categories:
//! - **Opening**: any character in Unicode category Ps (OpenPunctuation) or Pi (InitialPunctuation)
//! - **Closing**: any character in Unicode category Pe (ClosePunctuation) or Pf (FinalPunctuation)
//!
//! Matching of open→close uses Unicode Bidi Mirroring Glyph property.
//!
//! The ASCII bracket characters `(`, `)`, `[`, `]`, `{`, `}` are excluded as
//! they are reserved by the eucalypt language.

use std::collections::HashMap;

use unicode_bidi_mirroring::get_mirrored;
use unicode_general_category::{get_general_category, GeneralCategory};

/// How the content of a bracket pair is parsed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BracketContentType {
    /// Block-mode: content is parsed as declarations (like `{}`).
    Block,
    /// Expression-mode: content is parsed as a soup expression.
    Expression,
}

/// Parser-level registry of bracket pairs and their expected content types.
///
/// Populated by pre-scanning the token stream for bracket pair declarations
/// of the form `⟦{}⟧: …` (bare) or `(⟦{}⟧): …` (paren).  Consulted by the
/// parser to decide whether a bracket expression should be parsed as a
/// `BRACKET_BLOCK` or a `BRACKET_EXPR`.
#[derive(Default, Clone, Debug)]
pub struct BracketRegistry {
    pairs: HashMap<char, BracketContentType>,
}

impl BracketRegistry {
    /// Register `open` as having the given content type.
    pub fn register(&mut self, open: char, content_type: BracketContentType) {
        self.pairs.insert(open, content_type);
    }

    /// Return the content type for `open`, or `None` if not registered.
    pub fn content_type(&self, open: char) -> Option<&BracketContentType> {
        self.pairs.get(&open)
    }

    /// Return `true` if `open` is registered as block-mode.
    pub fn is_block_mode(&self, open: char) -> bool {
        matches!(self.content_type(open), Some(BracketContentType::Block))
    }
}

/// Return true if `c` is a Unicode bracket open character eligible for use as
/// an idiot bracket.
///
/// This includes any character in Unicode categories Ps (OpenPunctuation) or
/// Pi (InitialPunctuation), excluding the ASCII brackets reserved by the
/// eucalypt language: `(`, `[`, `{`.
pub fn is_bracket_open(c: char) -> bool {
    if c.is_ascii() {
        return false; // ASCII open brackets are language-reserved
    }
    matches!(
        get_general_category(c),
        GeneralCategory::OpenPunctuation | GeneralCategory::InitialPunctuation
    )
}

/// Return true if `c` is a Unicode bracket close character eligible for use as
/// an idiot bracket.
///
/// This includes any character in Unicode categories Pe (ClosePunctuation) or
/// Pf (FinalPunctuation), excluding the ASCII brackets reserved by the
/// eucalypt language: `)`, `]`, `}`.
pub fn is_bracket_close(c: char) -> bool {
    if c.is_ascii() {
        return false; // ASCII close brackets are language-reserved
    }
    matches!(
        get_general_category(c),
        GeneralCategory::ClosePunctuation | GeneralCategory::FinalPunctuation
    )
}

/// Return the matching closing bracket for an opening bracket character, using
/// the Unicode Bidi Mirroring Glyph property.
///
/// Returns `None` if `open` is not a recognised bracket open character or has
/// no mirroring glyph.
pub fn close_for_open(open: char) -> Option<char> {
    if !is_bracket_open(open) {
        return None;
    }
    get_mirrored(open)
}

/// A Unicode bracket pair consisting of open and close characters.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BracketPair {
    /// The opening bracket character (e.g. `⟦`)
    pub open: char,
    /// The closing bracket character (e.g. `⟧`)
    pub close: char,
}

impl BracketPair {
    /// The canonical name used to refer to this bracket pair in eucalypt.
    ///
    /// This is the string formed by concatenating the open and close chars,
    /// e.g. `"⟦⟧"`.
    pub fn name(self) -> String {
        let mut s = String::with_capacity(self.open.len_utf8() + self.close.len_utf8());
        s.push(self.open);
        s.push(self.close);
        s
    }
}

/// Return the `BracketPair` for an open bracket character, if recognised.
///
/// Uses Unicode category and Bidi Mirroring property to determine the pair.
pub fn pair_for_open(open: char) -> Option<BracketPair> {
    let close = close_for_open(open)?;
    Some(BracketPair { open, close })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_close_for_open_known_pairs() {
        // Pairs that were previously in the hard-coded table
        assert_eq!(close_for_open('⟦'), Some('⟧'));
        assert_eq!(close_for_open('⟨'), Some('⟩'));
        assert_eq!(close_for_open('⟪'), Some('⟫'));
        assert_eq!(close_for_open('⌈'), Some('⌉'));
        assert_eq!(close_for_open('⌊'), Some('⌋'));
        assert_eq!(close_for_open('⦃'), Some('⦄'));
        assert_eq!(close_for_open('⦇'), Some('⦈'));
        assert_eq!(close_for_open('⦉'), Some('⦊'));
        assert_eq!(close_for_open('«'), Some('»'));
        assert_eq!(close_for_open('【'), Some('】'));
        assert_eq!(close_for_open('〔'), Some('〕'));
        assert_eq!(close_for_open('〖'), Some('〗'));
        assert_eq!(close_for_open('〘'), Some('〙'));
        assert_eq!(close_for_open('〚'), Some('〛'));
    }

    #[test]
    fn test_ascii_brackets_excluded() {
        // ASCII brackets are language-reserved and must not be bracket pairs
        assert_eq!(close_for_open('('), None);
        assert_eq!(close_for_open('['), None);
        assert_eq!(close_for_open('{'), None);
        assert!(!is_bracket_open('('));
        assert!(!is_bracket_open('['));
        assert!(!is_bracket_open('{'));
        assert!(!is_bracket_close(')'));
        assert!(!is_bracket_close(']'));
        assert!(!is_bracket_close('}'));
    }

    #[test]
    fn test_non_bracket_chars_excluded() {
        assert_eq!(close_for_open('a'), None);
        assert_eq!(close_for_open('1'), None);
        assert!(!is_bracket_open('a'));
        assert!(!is_bracket_close('b'));
    }

    #[test]
    fn test_is_bracket_open_known() {
        assert!(is_bracket_open('⟦'));
        assert!(is_bracket_open('«'));
        assert!(is_bracket_open('⟨'));
        assert!(is_bracket_open('⌈'));
        assert!(is_bracket_open('⌊'));
    }

    #[test]
    fn test_is_bracket_close_known() {
        assert!(is_bracket_close('⟧'));
        assert!(is_bracket_close('»'));
        assert!(is_bracket_close('⟩'));
        assert!(is_bracket_close('⌉'));
        assert!(is_bracket_close('⌋'));
    }

    #[test]
    fn test_pair_name() {
        let pair = pair_for_open('⟦').unwrap();
        assert_eq!(pair.name(), "⟦⟧");

        let pair2 = pair_for_open('«').unwrap();
        assert_eq!(pair2.name(), "«»");
    }

    // Test with a bracket pair NOT in the old hard-coded table to prove
    // dynamic detection works.
    #[test]
    fn test_dynamic_detection_new_pair() {
        // U+2768 MEDIUM LEFT CURLY BRACKET ORNAMENT /
        // U+2769 MEDIUM RIGHT CURLY BRACKET ORNAMENT
        // These were NOT in the previous hard-coded table.
        let open = '\u{2768}'; // ❨
        let close = '\u{2769}'; // ❩
        assert!(
            is_bracket_open(open),
            "❨ should be detected as bracket open"
        );
        assert!(
            is_bracket_close(close),
            "❩ should be detected as bracket close"
        );
        assert_eq!(close_for_open(open), Some(close), "❨ should mirror to ❩");
        let pair = pair_for_open(open).unwrap();
        assert_eq!(pair.name(), "❨❩");
    }

    #[test]
    fn test_dynamic_detection_ornament_angle_bracket() {
        // U+276C MEDIUM LEFT-POINTING ANGLE BRACKET ORNAMENT
        // U+276D MEDIUM RIGHT-POINTING ANGLE BRACKET ORNAMENT
        // Also not in the old hard-coded table.
        let open = '\u{276c}'; // ❬
        let close = '\u{276d}'; // ❭
        assert!(
            is_bracket_open(open),
            "❬ should be detected as bracket open"
        );
        assert!(
            is_bracket_close(close),
            "❭ should be detected as bracket close"
        );
        assert_eq!(close_for_open(open), Some(close));
    }
}
