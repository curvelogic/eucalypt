//! Bracket pair lookup table for idiom brackets.
//!
//! Idiom brackets allow applicative functor application to be expressed
//! using Unicode bracket pairs, e.g. `⟦ x ⟧` or `⌈ x ⌉`.
//!
//! A bracket pair is declared in a block using the syntax:
//!
//! ```eucalypt
//! (⟦ x ⟧): f(x)
//! ```
//!
//! which defines a function named `⟦⟧` taking one argument and applying `f`.
//!
//! The bracket pair lookup table maps each supported Unicode open bracket
//! character to its corresponding close bracket character.

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

/// All built-in Unicode bracket pairs recognised as idiom brackets.
///
/// Pairs are drawn from common mathematical and typographical Unicode
/// bracket characters.  Additional pairs can be registered at runtime
/// via bracket pair declarations.
pub const BUILTIN_BRACKET_PAIRS: &[BracketPair] = &[
    // Mathematical angle brackets
    BracketPair {
        open: '⟦',
        close: '⟧',
    }, // MATHEMATICAL LEFT WHITE SQUARE BRACKET / RIGHT
    BracketPair {
        open: '⟨',
        close: '⟩',
    }, // MATHEMATICAL LEFT ANGLE BRACKET / RIGHT
    BracketPair {
        open: '⟪',
        close: '⟫',
    }, // MATHEMATICAL LEFT DOUBLE ANGLE BRACKET / RIGHT
    BracketPair {
        open: '⌈',
        close: '⌉',
    }, // LEFT CEILING / RIGHT CEILING
    BracketPair {
        open: '⌊',
        close: '⌋',
    }, // LEFT FLOOR / RIGHT FLOOR
    BracketPair {
        open: '⦃',
        close: '⦄',
    }, // MATHEMATICAL LEFT WHITE CURLY BRACKET / RIGHT
    BracketPair {
        open: '⦇',
        close: '⦈',
    }, // MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET / RIGHT
    BracketPair {
        open: '⦉',
        close: '⦊',
    }, // MATHEMATICAL LEFT FLATTENED PARENTHESIS / RIGHT
    // French quotation marks (guillemets)
    BracketPair {
        open: '«',
        close: '»',
    }, // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK / RIGHT
    // CJK brackets
    BracketPair {
        open: '【',
        close: '】',
    }, // LEFT BLACK LENTICULAR BRACKET / RIGHT
    BracketPair {
        open: '〔',
        close: '〕',
    }, // LEFT TORTOISE SHELL BRACKET / RIGHT
    BracketPair {
        open: '〖',
        close: '〗',
    }, // LEFT WHITE LENTICULAR BRACKET / RIGHT
    BracketPair {
        open: '〘',
        close: '〙',
    }, // LEFT WHITE TORTOISE SHELL BRACKET / RIGHT
    BracketPair {
        open: '〚',
        close: '〛',
    }, // LEFT WHITE SQUARE BRACKET / RIGHT
];

/// Look up the close bracket for a given open bracket character, if it is a
/// known idiom bracket pair.
pub fn close_for_open(open: char) -> Option<char> {
    BUILTIN_BRACKET_PAIRS
        .iter()
        .find(|p| p.open == open)
        .map(|p| p.close)
}

/// Return the `BracketPair` for an open bracket character, if recognised.
pub fn pair_for_open(open: char) -> Option<BracketPair> {
    BUILTIN_BRACKET_PAIRS
        .iter()
        .copied()
        .find(|p| p.open == open)
}

/// Return true if `c` is a recognised idiom bracket open character.
pub fn is_bracket_open(c: char) -> bool {
    BUILTIN_BRACKET_PAIRS.iter().any(|p| p.open == c)
}

/// Return true if `c` is a recognised idiom bracket close character.
pub fn is_bracket_close(c: char) -> bool {
    BUILTIN_BRACKET_PAIRS.iter().any(|p| p.close == c)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_close_for_open() {
        assert_eq!(close_for_open('⟦'), Some('⟧'));
        assert_eq!(close_for_open('⟨'), Some('⟩'));
        assert_eq!(close_for_open('«'), Some('»'));
        assert_eq!(close_for_open('('), None); // ASCII paren, not a bracket pair
        assert_eq!(close_for_open('a'), None);
    }

    #[test]
    fn test_pair_name() {
        let pair = pair_for_open('⟦').unwrap();
        assert_eq!(pair.name(), "⟦⟧");
    }

    #[test]
    fn test_is_bracket_open() {
        assert!(is_bracket_open('⟦'));
        assert!(is_bracket_open('«'));
        assert!(!is_bracket_open('('));
        assert!(!is_bracket_open(')'));
    }

    #[test]
    fn test_is_bracket_close() {
        assert!(is_bracket_close('⟧'));
        assert!(is_bracket_close('»'));
        assert!(!is_bracket_close(')'));
    }
}
