//! Syntax kinds for Rowan-based parser implementation

use crate::syntax::lexer;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum SyntaxKind {
    OPEN_BRACE,              // '{'
    CLOSE_BRACE,             // '}
    BACKTICK,                // '`'`
    COLON,                   // ':'
    OPEN_SQUARE,             // '['
    CLOSE_SQUARE,            // ']'
    COMMA,                   // ','
    WHITESPACE,              // any unicode whitespace
    COMMENT,                 // '#' to EOL
    OPEN_PAREN,              // '('
    OPEN_PAREN_APPLY,        // '(' in application context
    CLOSE_PAREN,             // ')'
    UNQUOTED_IDENTIFIER,     // e.g. x
    OPERATOR_IDENTIFIER,     // e.g. +
    SINGLE_QUOTE_IDENTIFIER, // e.g. 'x'
    NUMBER,                  // e.g. 234
    STRING,                  // e.g. "xyz"
    SYMBOL,                  // e.g. :xyz or :'**'

    /// As yet, unsupported open bracket / quote type, for LSP parses
    /// treat as "bad" OPEN_PAREN
    RESERVED_OPEN,
    /// As yet, unsupported open bracket / quote type, for LSP parses
    /// treat as "bad" CLOSE_PAREN
    RESERVED_CLOSE,

    /// Name containing normal or op identifier
    NAME,
    /// Literal containing number, symbol, string
    LITERAL,
    /// List e.g. [a, b, c]
    LIST,
    /// Block in braces e.g. { x: foo }
    BLOCK,
    /// Block meta embedded at start of block e.g. { meta x: foo }
    BLOCK_META,
    /// A block contains several declarations
    DECLARATION,
    /// A declaration has optional metadata
    DECL_META,
    /// A declaration head (preceding the colon)
    DECL_HEAD,
    /// A declaration body (following the colon)
    DECL_BODY,
    /// Sequence of elements with precedences not yet resolved ("uncooked")
    SOUP,
    /// Parenthesised expression (containing soup)
    PAREN_EXPR,
    /// A unit (file representing block)
    UNIT,
    /// Arguments to direct appliation e.g. (x, y) in f(x, y)
    ARG_TUPLE,

    // String pattern syntax kinds
    /// String pattern containing interpolation e.g. "Hello {name}!"
    STRING_PATTERN,
    /// Literal content within string pattern
    STRING_LITERAL_CONTENT,
    /// Interpolation within string pattern e.g. {name}
    STRING_INTERPOLATION,
    /// Interpolation target (identifier or anaphor)
    STRING_INTERPOLATION_TARGET,
    /// Format specifier in interpolation e.g. :%03d
    STRING_FORMAT_SPEC,
    /// Conversion specifier in interpolation
    STRING_CONVERSION_SPEC,
    /// Dotted reference in interpolation e.g. x.y.z
    STRING_DOTTED_REFERENCE,
    /// Escaped open brace {{
    STRING_ESCAPED_OPEN,
    /// Escaped close brace }}
    STRING_ESCAPED_CLOSE,

    /// Extraneous tokens tagging along for the ride
    ERROR_STOWAWAYS,
    /// Characters (brackets and quotes) reserved for future use
    ERROR_RESERVED_CHAR,
}

use SyntaxKind::*;

impl SyntaxKind {
    pub fn is_callable_terminal(&self) -> bool {
        *self == CLOSE_PAREN || *self == CLOSE_BRACE || *self == UNQUOTED_IDENTIFIER
    }

    pub fn is_literal_terminal(&self) -> bool {
        *self == NUMBER || *self == STRING || *self == SYMBOL
    }

    pub fn is_name_terminal(&self) -> bool {
        *self == UNQUOTED_IDENTIFIER
            || *self == OPERATOR_IDENTIFIER
            || *self == SINGLE_QUOTE_IDENTIFIER
    }

    pub fn is_trivial(&self) -> bool {
        *self == COMMENT || *self == WHITESPACE
    }

    pub fn from_raw(raw: rowan::SyntaxKind) -> Self {
        assert!(raw.0 <= SyntaxKind::ERROR_STOWAWAYS as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
}

impl<'a> From<lexer::Token<'a>> for SyntaxKind {
    fn from(t: lexer::Token<'a>) -> Self {
        match t {
            lexer::Token::OpenBrace => OPEN_BRACE,
            lexer::Token::CloseBrace => CLOSE_BRACE,
            lexer::Token::Backtick => BACKTICK,
            lexer::Token::Colon => COLON,
            lexer::Token::OpenSquare => OPEN_SQUARE,
            lexer::Token::CloseSquare => CLOSE_SQUARE,
            lexer::Token::Comma => COMMA,
            lexer::Token::Whitespace(_) => WHITESPACE,
            lexer::Token::Comment(_) => COMMENT,
            lexer::Token::OpenParen => OPEN_PAREN,
            lexer::Token::OpenParenApply => OPEN_PAREN_APPLY,
            lexer::Token::CloseParen => CLOSE_PAREN,
            lexer::Token::NormalIdentifier(_) => UNQUOTED_IDENTIFIER,
            lexer::Token::OperatorIdentifier(_) => OPERATOR_IDENTIFIER,
            lexer::Token::SingleQuoteIdentifier(_) => SINGLE_QUOTE_IDENTIFIER,
            lexer::Token::Number(_) => NUMBER,
            lexer::Token::String(_) => STRING,
            lexer::Token::Symbol(_) => SYMBOL,
        }
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EucalyptLanguage {}
impl rowan::Language for EucalyptLanguage {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind::from_raw(raw)
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<EucalyptLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<EucalyptLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<EucalyptLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<EucalyptLanguage>;
