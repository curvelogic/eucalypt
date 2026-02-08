//! Go-to-definition for the eucalypt LSP.
//!
//! Given a cursor position, finds the identifier at that position and
//! resolves it against the symbol table to return the definition location.

use crate::syntax::rowan::kind::{SyntaxKind, SyntaxNode, SyntaxToken};
use lsp_types::{GotoDefinitionResponse, Location, Position};
use rowan::TextSize;

use super::selection::position_to_offset;
use super::symbol_table::SymbolTable;

/// Find the definition of the symbol at the given cursor position.
///
/// Returns a `GotoDefinitionResponse` if a definition is found, or
/// `None` if there is no identifiable symbol at the position.
pub fn goto_definition(
    source: &str,
    root: &SyntaxNode,
    position: &Position,
    table: &SymbolTable,
) -> Option<GotoDefinitionResponse> {
    let offset = position_to_offset(source, position);
    let token = find_identifier_at(root, offset)?;
    let name = identifier_text(&token);

    // Check for dotted access: is the token preceded by a dot?
    if let Some((prefix, member)) = resolve_dotted(root, &token) {
        let symbols = table.lookup_qualified(&prefix, &member);
        if let Some(sym) = symbols.first() {
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: sym.uri.clone(),
                range: sym.selection_range,
            }));
        }
    }

    // Simple name lookup
    let symbols = table.lookup(&name);
    let sym = symbols.first()?;
    Some(GotoDefinitionResponse::Scalar(Location {
        uri: sym.uri.clone(),
        range: sym.selection_range,
    }))
}

/// Find the identifier token at or near the given offset.
pub(super) fn find_identifier_at(root: &SyntaxNode, offset: TextSize) -> Option<SyntaxToken> {
    let token = root.token_at_offset(offset).right_biased()?;

    if is_identifier_kind(token.kind()) {
        return Some(token);
    }

    // If cursor is just before or after an identifier, try neighbours
    if let Some(prev) = token.prev_token() {
        if is_identifier_kind(prev.kind()) && prev.text_range().end() == offset {
            return Some(prev);
        }
    }

    None
}

/// Check if a syntax kind is an identifier.
fn is_identifier_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::UNQUOTED_IDENTIFIER
            | SyntaxKind::OPERATOR_IDENTIFIER
            | SyntaxKind::SINGLE_QUOTE_IDENTIFIER
    )
}

/// Extract the display name from an identifier token.
fn identifier_text(token: &SyntaxToken) -> String {
    let text = token.text();
    match token.kind() {
        SyntaxKind::SINGLE_QUOTE_IDENTIFIER => text
            .strip_prefix('\'')
            .and_then(|t| t.strip_suffix('\''))
            .unwrap_or(text)
            .to_string(),
        _ => text.to_string(),
    }
}

/// Detect dotted access patterns like `ns.member`.
///
/// In eucalypt, `.` is an operator, so `ns.member` is lexed as
/// `ns` (UNQUOTED_IDENTIFIER), `.` (OPERATOR_IDENTIFIER), `member`
/// (UNQUOTED_IDENTIFIER). If the token is the member part after a
/// dot operator, returns `Some((prefix_name, member_name))`.
pub(super) fn resolve_dotted(_root: &SyntaxNode, token: &SyntaxToken) -> Option<(String, String)> {
    // Walk backwards: previous token should be `.` operator, and before that an identifier
    let dot = token.prev_token()?;
    if dot.kind() != SyntaxKind::OPERATOR_IDENTIFIER || dot.text() != "." {
        return None;
    }
    let prefix_token = dot.prev_token()?;
    if !is_identifier_kind(prefix_token.kind()) {
        return None;
    }

    let prefix = identifier_text(&prefix_token);
    let member = identifier_text(token);
    Some((prefix, member))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::driver::lsp::symbol_table::{SymbolSource, SymbolTable};
    use crate::syntax::rowan::parse_unit;

    fn setup_table(source: &str) -> (SymbolTable, lsp_types::Url) {
        let uri = lsp_types::Url::parse("file:///test/file.eu").unwrap();
        let parse = parse_unit(source);
        let unit = parse.tree();
        let mut table = SymbolTable::new();
        table.add_from_unit(&unit, source, &uri, SymbolSource::Local);
        (table, uri)
    }

    #[test]
    fn test_goto_property_definition() {
        let source = "x: 1\ny: x\n";
        let (table, _) = setup_table(source);
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        // Position on the 'x' reference in line 2 (y: x)
        let pos = Position {
            line: 1,
            character: 3,
        };
        let result = goto_definition(source, &root, &pos, &table);
        assert!(result.is_some(), "should find definition of x");
    }

    #[test]
    fn test_goto_function_definition() {
        let source = "f(x): x\ny: f(1)\n";
        let (table, _) = setup_table(source);
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        // Position on the 'f' call in line 2
        let pos = Position {
            line: 1,
            character: 3,
        };
        let result = goto_definition(source, &root, &pos, &table);
        assert!(result.is_some(), "should find definition of f");
    }

    #[test]
    fn test_goto_no_definition_for_literal() {
        let source = "x: 42\n";
        let (table, _) = setup_table(source);
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        // Position on the number literal '42'
        let pos = Position {
            line: 0,
            character: 3,
        };
        let result = goto_definition(source, &root, &pos, &table);
        assert!(result.is_none(), "should not find definition for a literal");
    }

    #[test]
    fn test_goto_dotted_access() {
        let source = "ns: { inner: 1 }\ny: ns.inner\n";
        let (table, _) = setup_table(source);
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        // Position on 'inner' after 'ns.'
        let pos = Position {
            line: 1,
            character: 6,
        };
        let result = goto_definition(source, &root, &pos, &table);
        assert!(result.is_some(), "should find definition of ns.inner");
    }
}
