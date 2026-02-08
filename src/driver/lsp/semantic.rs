//! LSP semantic token provider.
//!
//! Walks the Rowan syntax tree and classifies tokens for richer
//! highlighting than TextMate grammars can provide. Uses the symbol
//! table to distinguish prelude/built-in names from local definitions.

use crate::syntax::rowan::kind::{SyntaxKind, SyntaxNode};
use lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensLegend,
};
use rowan::WalkEvent;

use super::diagnostics::LineIndex;
use super::symbol_table::{DeclKind, SymbolSource, SymbolTable};

/// Token type indices — order must match `TOKEN_TYPES`.
mod token_type {
    pub const PROPERTY: u32 = 0;
    pub const FUNCTION: u32 = 1;
    pub const PARAMETER: u32 = 2;
    pub const VARIABLE: u32 = 3;
    pub const NUMBER: u32 = 4;
    pub const STRING: u32 = 5;
    pub const COMMENT: u32 = 6;
    pub const OPERATOR: u32 = 7;
    pub const ENUM_MEMBER: u32 = 8;
    pub const DECORATOR: u32 = 9;
}

/// Token modifier bit flags — order must match `TOKEN_MODIFIERS`.
mod token_modifier {
    pub const DEFAULT_LIBRARY: u32 = 1 << 0;
    pub const DECLARATION: u32 = 1 << 1;
}

/// The token types advertised in the legend.
pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::PROPERTY,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::NUMBER,
    SemanticTokenType::STRING,
    SemanticTokenType::COMMENT,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::ENUM_MEMBER,
    SemanticTokenType::DECORATOR,
];

/// The token modifiers advertised in the legend.
pub const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::DEFAULT_LIBRARY,
    SemanticTokenModifier::DECLARATION,
];

/// Build the semantic tokens legend for capability advertisement.
pub fn legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: TOKEN_TYPES.to_vec(),
        token_modifiers: TOKEN_MODIFIERS.to_vec(),
    }
}

/// Compute semantic tokens for the full document.
pub fn semantic_tokens_full(
    source: &str,
    root: &SyntaxNode,
    table: &SymbolTable,
) -> SemanticTokens {
    let mut tokens = Vec::new();
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    // Build a line index for offset-to-line/col conversion (UTF-16)
    let line_index = LineIndex::new(source);

    for event in root.preorder_with_tokens() {
        let token = match event {
            WalkEvent::Enter(rowan::NodeOrToken::Token(t)) => t,
            _ => continue,
        };

        let (token_type, modifiers) = match classify_token(&token, table) {
            Some(c) => c,
            None => continue,
        };

        let start_offset = u32::from(token.text_range().start());
        let end_offset = u32::from(token.text_range().end());
        let length = line_index.utf16_text_len(start_offset, end_offset);

        let (line, col) = line_index.offset_to_line_col(start_offset);

        let delta_line = line - prev_line;
        let delta_start = if delta_line == 0 {
            col - prev_start
        } else {
            col
        };

        tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type,
            token_modifiers_bitset: modifiers,
        });

        prev_line = line;
        prev_start = col;
    }

    SemanticTokens {
        result_id: None,
        data: tokens,
    }
}

/// Classify a syntax token into a semantic token type and modifier set.
///
/// Returns `None` for tokens that should not be highlighted (whitespace,
/// punctuation).
fn classify_token(
    token: &crate::syntax::rowan::kind::SyntaxToken,
    table: &SymbolTable,
) -> Option<(u32, u32)> {
    match token.kind() {
        SyntaxKind::COMMENT => Some((token_type::COMMENT, 0)),
        SyntaxKind::NUMBER => Some((token_type::NUMBER, 0)),
        SyntaxKind::STRING
        | SyntaxKind::C_STRING
        | SyntaxKind::RAW_STRING
        | SyntaxKind::T_STRING
        | SyntaxKind::STRING_LITERAL_CONTENT
        | SyntaxKind::STRING_ESCAPED_OPEN
        | SyntaxKind::STRING_ESCAPED_CLOSE
        | SyntaxKind::STRING_PATTERN_START
        | SyntaxKind::STRING_PATTERN_END
        | SyntaxKind::C_STRING_PATTERN_START
        | SyntaxKind::C_STRING_PATTERN_END
        | SyntaxKind::RAW_STRING_PATTERN_START
        | SyntaxKind::RAW_STRING_PATTERN_END
        | SyntaxKind::STRING_FORMAT_SPEC
        | SyntaxKind::STRING_CONVERSION_SPEC => Some((token_type::STRING, 0)),
        SyntaxKind::SYMBOL => Some((token_type::ENUM_MEMBER, 0)),
        SyntaxKind::OPERATOR_IDENTIFIER => {
            // Check context: is this in a declaration head (definition)?
            let in_decl_head = token
                .parent()
                .is_some_and(|p| is_in_ancestor(p, SyntaxKind::DECL_HEAD));
            let modifiers = if in_decl_head {
                token_modifier::DECLARATION
            } else {
                0
            };
            Some((token_type::OPERATOR, modifiers))
        }
        SyntaxKind::UNQUOTED_IDENTIFIER | SyntaxKind::SINGLE_QUOTE_IDENTIFIER => {
            classify_identifier(token, table)
        }
        SyntaxKind::STRING_INTERPOLATION_TARGET | SyntaxKind::STRING_DOTTED_REFERENCE => {
            Some((token_type::VARIABLE, 0))
        }
        SyntaxKind::BACKTICK => {
            // Backtick introduces metadata — classify as decorator
            if is_metadata_context(token) {
                Some((token_type::DECORATOR, 0))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Classify an identifier token using its syntactic context and the
/// symbol table.
fn classify_identifier(
    token: &crate::syntax::rowan::kind::SyntaxToken,
    table: &SymbolTable,
) -> Option<(u32, u32)> {
    let text = token.text();
    let name = match token.kind() {
        SyntaxKind::SINGLE_QUOTE_IDENTIFIER => text
            .strip_prefix('\'')
            .and_then(|t| t.strip_suffix('\''))
            .unwrap_or(text),
        _ => text,
    };

    // Check if this is in a declaration head (it's a definition)
    let in_decl_head = token
        .parent()
        .is_some_and(|p| is_in_ancestor(p, SyntaxKind::DECL_HEAD));

    // Check if this is in a metadata context
    if is_metadata_context(token) {
        return Some((token_type::DECORATOR, 0));
    }

    // Look up in symbol table
    let symbols = table.lookup(name);
    if let Some(sym) = symbols.first() {
        let is_prelude = sym.source == SymbolSource::Prelude;
        let mut modifiers = if is_prelude {
            token_modifier::DEFAULT_LIBRARY
        } else {
            0
        };
        if in_decl_head {
            modifiers |= token_modifier::DECLARATION;
        }

        let token_type = match &sym.kind {
            DeclKind::Function { .. } => token_type::FUNCTION,
            DeclKind::Operator { .. } => token_type::OPERATOR,
            DeclKind::Property => {
                if in_decl_head {
                    token_type::PROPERTY
                } else {
                    token_type::VARIABLE
                }
            }
        };

        return Some((token_type, modifiers));
    }

    // No symbol table match — use syntactic context
    if in_decl_head {
        // Check if this is a parameter (appears after the first identifier in the head)
        if is_parameter(token) {
            return Some((token_type::PARAMETER, token_modifier::DECLARATION));
        }
        return Some((token_type::PROPERTY, token_modifier::DECLARATION));
    }

    // Unknown reference — generic variable
    Some((token_type::VARIABLE, 0))
}

/// Check if a token is a parameter in a declaration head.
///
/// In eucalypt, `f(x, y): body` has `f` as the function name and `x`, `y`
/// as parameters. Parameters are identifiers inside an ARG_TUPLE node.
fn is_parameter(token: &crate::syntax::rowan::kind::SyntaxToken) -> bool {
    token
        .parent()
        .is_some_and(|p| is_in_ancestor(p, SyntaxKind::ARG_TUPLE))
}

/// Check if a token is in a metadata annotation context.
fn is_metadata_context(token: &crate::syntax::rowan::kind::SyntaxToken) -> bool {
    token.parent().is_some_and(|p| {
        is_in_ancestor(p.clone(), SyntaxKind::DECL_META)
            || is_in_ancestor(p, SyntaxKind::BLOCK_META)
    })
}

/// Check if a node or any of its ancestors has the given syntax kind.
fn is_in_ancestor(mut node: SyntaxNode, kind: SyntaxKind) -> bool {
    loop {
        if node.kind() == kind {
            return true;
        }
        match node.parent() {
            Some(parent) => node = parent,
            None => return false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::driver::lsp::symbol_table::{prelude_symbols, SymbolSource};
    use crate::syntax::rowan::parse_unit;

    fn make_table(source: &str) -> SymbolTable {
        let uri = lsp_types::Url::parse("file:///test.eu").unwrap();
        let parse = parse_unit(source);
        let unit = parse.tree();
        let prelude_uri = lsp_types::Url::parse("resource:prelude").unwrap();
        let mut table = SymbolTable::new();
        table.add_from_unit(&unit, source, &uri, SymbolSource::Local);
        for sym in prelude_symbols(&prelude_uri).all_symbols() {
            table.add(sym.clone());
        }
        table
    }

    #[test]
    fn empty_source_produces_no_tokens() {
        let source = "";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let table = SymbolTable::new();
        let tokens = semantic_tokens_full(source, &root, &table);
        assert!(tokens.data.is_empty());
    }

    #[test]
    fn comment_classified_correctly() {
        let source = "# a comment\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let table = SymbolTable::new();
        let tokens = semantic_tokens_full(source, &root, &table);
        assert!(!tokens.data.is_empty());
        assert_eq!(tokens.data[0].token_type, token_type::COMMENT);
    }

    #[test]
    fn number_classified_correctly() {
        let source = "x: 42\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let table = make_table(source);
        let tokens = semantic_tokens_full(source, &root, &table);
        let number_tokens: Vec<_> = tokens
            .data
            .iter()
            .filter(|t| t.token_type == token_type::NUMBER)
            .collect();
        assert!(!number_tokens.is_empty(), "should find a number token");
    }

    #[test]
    fn symbol_classified_as_enum_member() {
        let source = "x: :foo\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let table = make_table(source);
        let tokens = semantic_tokens_full(source, &root, &table);
        let symbol_tokens: Vec<_> = tokens
            .data
            .iter()
            .filter(|t| t.token_type == token_type::ENUM_MEMBER)
            .collect();
        assert!(
            !symbol_tokens.is_empty(),
            "should find an enum member token for :foo"
        );
    }

    #[test]
    fn operator_classified_correctly() {
        let source = "x: 1 + 2\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let table = make_table(source);
        let tokens = semantic_tokens_full(source, &root, &table);
        let op_tokens: Vec<_> = tokens
            .data
            .iter()
            .filter(|t| t.token_type == token_type::OPERATOR)
            .collect();
        assert!(!op_tokens.is_empty(), "should find an operator token for +");
    }

    #[test]
    fn string_classified_correctly() {
        let source = "x: \"hello\"\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let table = make_table(source);
        let tokens = semantic_tokens_full(source, &root, &table);
        let string_tokens: Vec<_> = tokens
            .data
            .iter()
            .filter(|t| t.token_type == token_type::STRING)
            .collect();
        assert!(!string_tokens.is_empty(), "should find a string token");
    }

    #[test]
    fn delta_encoding_starts_at_zero() {
        let source = "x: 1\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let table = make_table(source);
        let tokens = semantic_tokens_full(source, &root, &table);
        if !tokens.data.is_empty() {
            // First token should have delta_line and delta_start relative to (0,0)
            assert_eq!(tokens.data[0].delta_line, 0, "first token on first line");
        }
    }

    #[test]
    fn legend_has_expected_types() {
        let leg = legend();
        assert!(leg.token_types.contains(&SemanticTokenType::COMMENT));
        assert!(leg.token_types.contains(&SemanticTokenType::NUMBER));
        assert!(leg.token_types.contains(&SemanticTokenType::STRING));
        assert!(leg.token_types.contains(&SemanticTokenType::FUNCTION));
        assert!(leg.token_types.contains(&SemanticTokenType::PROPERTY));
        assert!(leg.token_types.contains(&SemanticTokenType::OPERATOR));
    }

    #[test]
    fn legend_has_expected_modifiers() {
        let leg = legend();
        assert!(leg
            .token_modifiers
            .contains(&SemanticTokenModifier::DEFAULT_LIBRARY));
        assert!(leg
            .token_modifiers
            .contains(&SemanticTokenModifier::DECLARATION));
    }
}
