use rowan::TextRange;

use super::kind::SyntaxKind;

/// A syntax error detected and recorded during parse
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseError {
    UnexpectedToken {
        expected: SyntaxKind,
        actual: SyntaxKind,
        range: TextRange,
    },
    UnclosedSingleQuote {
        range: TextRange,
    },
    UnclosedDoubleQuote {
        range: TextRange,
    },
    InvalidParenExpr {
        open_paren_range: Option<TextRange>,
        range: TextRange,
    },
    UnterminatedBlock {
        open_brace_range: Option<TextRange>,
        range: TextRange,
    },
    EmptyDeclarationBody {
        range: TextRange,
    },
    MissingDeclarationColon {
        head_range: TextRange,
    },
    MalformedDeclarationHead {
        range: TextRange,
    },
    InvalidFormalParameter {
        head_range: TextRange,
        range: TextRange,
    },
    InvalidOperatorName {
        head_range: TextRange,
        range: TextRange,
    },
    InvalidPropertyName {
        head_range: TextRange,
        range: TextRange,
    },
    SurplusContent {
        range: TextRange,
    },
    ReservedCharacter {
        range: TextRange,
    },
    EmptyExpression {
        range: TextRange,
    },
    UnclosedStringInterpolation {
        range: TextRange,
    },
    InvalidZdtLiteral {
        range: TextRange,
    },
}
