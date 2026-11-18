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

        // LSP clients cannot place a token that spans lines, so a
        // multi-line token (a string literal, say) is emitted as one token
        // per line segment, split at `\n` as `LineIndex` counts lines. A
        // segment's line terminator (`\n`, or `\r\n`) is not part of its
        // token, and segments left empty are skipped.
        let token_start = u32::from(token.text_range().start());
        let mut segment_start = token_start;
        for segment in token.text().split_inclusive('\n') {
            let content = segment.trim_end_matches(['\n', '\r']);
            let start_offset = segment_start;
            let end_offset = start_offset + content.len() as u32;
            segment_start += segment.len() as u32;
            if content.is_empty() {
                continue;
            }

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
        | SyntaxKind::S_STRING
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
        SyntaxKind::BRACKET_OPEN | SyntaxKind::BRACKET_CLOSE => {
            // Bracket pair delimiters (e.g. ⟦ ⟧, ‹ ›) — use OPERATOR type.
            // Apply DECLARATION modifier when used in a declaration head.
            let in_decl_head = token
                .parent()
                .and_then(|p| p.parent())
                .is_some_and(|p| is_in_ancestor(p, SyntaxKind::DECL_HEAD));
            let modifiers = if in_decl_head {
                token_modifier::DECLARATION
            } else {
                0
            };
            Some((token_type::OPERATOR, modifiers))
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

    // Identifiers in a declaration's parameter pattern bind parameters,
    // whatever their names and however the pattern is shaped (a block
    // pattern's leading names parse as block metadata). They are
    // classified before metadata and symbol-table lookups.
    if let Some(role) = pattern_role(token) {
        return Some(match role {
            PatternRole::Binding => (token_type::PARAMETER, token_modifier::DECLARATION),
            PatternRole::FieldName => (token_type::PARAMETER, 0),
        });
    }

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
        return Some((token_type::PROPERTY, token_modifier::DECLARATION));
    }

    // Unknown reference — generic variable
    Some((token_type::VARIABLE, 0))
}

/// The role of an identifier inside a declaration's parameter pattern.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PatternRole {
    /// The identifier binds a parameter: `x` in `f(x):`, `f{x y}:`,
    /// `f([x : xs]):`, `(l + x):` or `⟦ x ⟧:`, and `a` in `f({x: a}):`.
    Binding,
    /// The identifier names a field of a block pattern's argument
    /// rather than binding it: `x` in the rename pattern `f({x: a}):`.
    /// It is part of the parameter pattern, so it is a parameter token,
    /// but without the declaration modifier.
    FieldName,
}

/// Find the role of a token inside a declaration's parameter pattern, or
/// `None` when it is not in one.
///
/// A parameter pattern is the argument tuple of a function declaration
/// (`f(x, y):`, `f{x y}:`) or the operand expression of an operator or
/// bracket declaration (`(l +++ r):`, `⟦ xs ⟧:`): an `ARG_TUPLE`,
/// `PAREN_EXPR` or `BRACKET_EXPR` whose parent is a `DECL_HEAD`. An
/// argument tuple elsewhere (a call such as `merge({ a: 1 })`) is not a
/// pattern, so the declarations in a block passed to it are ordinary
/// declarations. Inside a block pattern a rename `{x: a}` parses as a
/// declaration whose head `x` is the field name and whose body `a` is
/// the binding.
fn pattern_role(token: &crate::syntax::rowan::kind::SyntaxToken) -> Option<PatternRole> {
    // The first head or body met on the way up decides the role: a head
    // means the token names a pattern declaration's field, a body that it
    // is the declaration's binding. Declaration metadata inside a pattern
    // (`f({ `{ doc: "d" } x: a }):`) is left to the metadata check.
    let mut role = None;
    for node in token.parent_ancestors() {
        match node.kind() {
            SyntaxKind::DECL_META => return None,
            SyntaxKind::DECL_HEAD if role.is_none() => role = Some(PatternRole::FieldName),
            SyntaxKind::DECL_BODY if role.is_none() => role = Some(PatternRole::Binding),
            SyntaxKind::ARG_TUPLE | SyntaxKind::PAREN_EXPR | SyntaxKind::BRACKET_EXPR
                if node
                    .parent()
                    .is_some_and(|p| p.kind() == SyntaxKind::DECL_HEAD) =>
            {
                return Some(role.unwrap_or(PatternRole::Binding));
            }
            _ => {}
        }
    }
    None
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

    /// Decode the delta-encoded tokens of `source` into
    /// `(line, text, type, modifiers)`, taking text from the source line.
    fn decoded(source: &str) -> Vec<(u32, String, u32, u32)> {
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let table = make_table(source);
        let tokens = semantic_tokens_full(source, &root, &table);
        let lines: Vec<&str> = source.split('\n').collect();
        let (mut line, mut col) = (0u32, 0u32);
        tokens
            .data
            .iter()
            .map(|t| {
                if t.delta_line == 0 {
                    col += t.delta_start;
                } else {
                    line += t.delta_line;
                    col = t.delta_start;
                }
                let utf16: Vec<u16> = lines[line as usize].encode_utf16().collect();
                let text =
                    String::from_utf16(&utf16[col as usize..(col + t.length) as usize]).unwrap();
                (line, text, t.token_type, t.token_modifiers_bitset)
            })
            .collect()
    }

    /// The classifications of every token on `line` with text `text`,
    /// from tokens already decoded by `decoded`.
    fn classes_of(tokens: &[(u32, String, u32, u32)], line: u32, text: &str) -> Vec<(u32, u32)> {
        tokens
            .iter()
            .filter(|(l, t, _, _)| *l == line && t == text)
            .map(|(_, _, ty, m)| (*ty, *m))
            .collect()
    }

    const PARAM_DECL: (u32, u32) = (token_type::PARAMETER, token_modifier::DECLARATION);
    const PROPERTY_DECL: (u32, u32) = (token_type::PROPERTY, token_modifier::DECLARATION);

    #[test]
    fn block_destructuring_parameters_are_parameters() {
        let source = "f{x y}: x\ng({a b}): a\n";
        let tokens = decoded(source);
        assert_eq!(classes_of(&tokens, 0, "x")[0], PARAM_DECL);
        assert_eq!(classes_of(&tokens, 0, "y"), vec![PARAM_DECL]);
        assert_eq!(classes_of(&tokens, 1, "a")[0], PARAM_DECL);
        assert_eq!(classes_of(&tokens, 1, "b"), vec![PARAM_DECL]);
    }

    #[test]
    fn rename_pattern_field_is_parameter_and_binding_is_declaration() {
        let source = "f({x: a  y}): a\n";
        let tokens = decoded(source);
        assert_eq!(
            classes_of(&tokens, 0, "x"),
            vec![(token_type::PARAMETER, 0)]
        );
        assert_eq!(classes_of(&tokens, 0, "a")[0], PARAM_DECL);
        assert_eq!(classes_of(&tokens, 0, "y"), vec![PARAM_DECL]);
    }

    #[test]
    fn list_and_ordinary_parameters_are_parameters() {
        let source = "k([h : t], n): h\n";
        let tokens = decoded(source);
        assert_eq!(classes_of(&tokens, 0, "h")[0], PARAM_DECL);
        assert_eq!(classes_of(&tokens, 0, "t"), vec![PARAM_DECL]);
        assert_eq!(classes_of(&tokens, 0, "n"), vec![PARAM_DECL]);
    }

    #[test]
    fn parameter_named_like_prelude_function_is_parameter() {
        let source = "f(first): first\n";
        let tokens = decoded(source);
        assert_eq!(classes_of(&tokens, 0, "first")[0], PARAM_DECL);
    }

    #[test]
    fn declarations_in_block_call_argument_are_properties() {
        let source = "h: merge({ a: 1 })\n";
        let tokens = decoded(source);
        assert_eq!(classes_of(&tokens, 0, "a"), vec![PROPERTY_DECL]);
    }

    #[test]
    fn operator_and_bracket_declaration_operands_are_parameters() {
        let source = "(l +++ r): l\n⟦ xs ⟧: xs\n(! p): p\n";
        let tokens = decoded(source);
        assert_eq!(classes_of(&tokens, 0, "l")[0], PARAM_DECL);
        assert_eq!(classes_of(&tokens, 0, "r"), vec![PARAM_DECL]);
        assert_eq!(classes_of(&tokens, 1, "xs")[0], PARAM_DECL);
        assert_eq!(classes_of(&tokens, 2, "p")[0], PARAM_DECL);
    }

    #[test]
    fn declared_names_are_unchanged() {
        let source = "f(x): x\nv: 1\n";
        let tokens = decoded(source);
        assert_eq!(
            classes_of(&tokens, 0, "f"),
            vec![(token_type::FUNCTION, token_modifier::DECLARATION)]
        );
        assert_eq!(classes_of(&tokens, 1, "v"), vec![PROPERTY_DECL]);
    }

    #[test]
    fn block_metadata_is_still_decorator() {
        let source = "b: { { doc: \"d\" } a: 1 }\n";
        let tokens = decoded(source);
        assert_eq!(
            classes_of(&tokens, 0, "doc"),
            vec![(token_type::DECORATOR, 0)]
        );
    }

    #[test]
    fn metadata_inside_a_parameter_pattern_is_still_decorator() {
        let source = "f({ `{ doc: \"d\" tag: :t } x: a }): a\n";
        let tokens = decoded(source);
        assert_eq!(
            classes_of(&tokens, 0, "doc"),
            vec![(token_type::DECORATOR, 0)]
        );
        assert_eq!(
            classes_of(&tokens, 0, "tag"),
            vec![(token_type::DECORATOR, 0)]
        );
        assert_eq!(
            classes_of(&tokens, 0, "x"),
            vec![(token_type::PARAMETER, 0)]
        );
        assert_eq!(classes_of(&tokens, 0, "a")[0], PARAM_DECL);
    }

    #[test]
    fn s_string_is_string() {
        let source = "t: s\"foo\"\n";
        let tokens = decoded(source);
        assert_eq!(
            classes_of(&tokens, 0, "s\"foo\""),
            vec![(token_type::STRING, 0)]
        );
    }

    #[test]
    fn multi_line_string_is_one_token_per_line() {
        let source = "m: \"abc\r\n\ndéf\"\nn: 1\n";
        let tokens = decoded(source);
        let strings: Vec<_> = tokens
            .iter()
            .filter(|(_, _, ty, _)| *ty == token_type::STRING)
            .map(|(l, t, _, _)| (*l, t.clone()))
            .collect();
        assert_eq!(
            strings,
            vec![(0, "\"abc".to_string()), (2, "déf\"".to_string())]
        );
        assert_eq!(classes_of(&tokens, 3, "n"), vec![PROPERTY_DECL]);
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
