//! LSP code action provider.
//!
//! Provides quick-fix code actions for common issues:
//! - Suggest qualified prelude names for unresolved identifiers
//!   (e.g. `split` -> `str.split`)

use crate::syntax::rowan::kind::{SyntaxKind, SyntaxNode, SyntaxToken};
use lsp_types::{CodeAction, CodeActionKind, Range, TextEdit, Url, WorkspaceEdit};
use rowan::WalkEvent;
use std::collections::HashMap;

use super::diagnostics::text_range_to_lsp_range;
use super::symbol_table::SymbolTable;

/// Compute code actions for the given range of a document.
pub fn code_actions(
    source: &str,
    root: &SyntaxNode,
    range: &Range,
    uri: &Url,
    table: &SymbolTable,
) -> Vec<CodeAction> {
    let mut actions = Vec::new();

    // Find identifier tokens within the requested range
    for event in root.preorder_with_tokens() {
        let token = match event {
            WalkEvent::Enter(rowan::NodeOrToken::Token(t)) => t,
            _ => continue,
        };

        if !is_actionable_identifier(&token) {
            continue;
        }

        let token_range = text_range_to_lsp_range(source, token.text_range());
        if !ranges_overlap(&token_range, range) {
            continue;
        }

        collect_qualify_name_actions(&token, source, &token_range, uri, table, &mut actions);
    }

    actions
}

/// Check if a token is an identifier that might need qualification.
fn is_actionable_identifier(token: &SyntaxToken) -> bool {
    matches!(
        token.kind(),
        SyntaxKind::UNQUOTED_IDENTIFIER | SyntaxKind::SINGLE_QUOTE_IDENTIFIER
    )
}

/// If an identifier doesn't resolve as a top-level name but matches a
/// child of a prelude namespace, suggest replacing it with the
/// qualified name.
fn collect_qualify_name_actions(
    token: &SyntaxToken,
    _source: &str,
    token_range: &Range,
    uri: &Url,
    table: &SymbolTable,
    actions: &mut Vec<CodeAction>,
) {
    let name = identifier_text(token);

    // Skip if the name is in a declaration head (it's a definition, not a reference)
    if is_in_decl_head(token) {
        return;
    }

    // Skip if the name resolves directly as a top-level symbol
    let direct = table.lookup(&name);
    if !direct.is_empty() {
        return;
    }

    // Search for qualified matches in namespace children
    let qualified = table.find_qualified_matches(&name);
    let is_unique = qualified.len() == 1;
    for (qualified_name, _sym) in &qualified {
        let edit = TextEdit {
            range: *token_range,
            new_text: qualified_name.clone(),
        };

        let mut changes = HashMap::new();
        changes.insert(uri.clone(), vec![edit]);

        actions.push(CodeAction {
            title: format!("Replace with `{qualified_name}`"),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: None,
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                ..WorkspaceEdit::default()
            }),
            is_preferred: Some(is_unique),
            ..CodeAction::default()
        });
    }
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

/// Check if a token is inside a declaration head.
fn is_in_decl_head(token: &SyntaxToken) -> bool {
    let mut node = match token.parent() {
        Some(n) => n,
        None => return false,
    };
    loop {
        if node.kind() == SyntaxKind::DECL_HEAD {
            return true;
        }
        match node.parent() {
            Some(parent) => node = parent,
            None => return false,
        }
    }
}

/// Check if two LSP ranges overlap.
fn ranges_overlap(a: &Range, b: &Range) -> bool {
    a.start.line <= b.end.line && b.start.line <= a.end.line
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::driver::lsp::symbol_table::{prelude_symbols, SymbolSource};
    use crate::syntax::rowan::parse_unit;
    use lsp_types::Position;

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

    fn full_range() -> Range {
        Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 1000,
                character: 0,
            },
        }
    }

    #[test]
    fn no_actions_for_empty_source() {
        let source = "";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let uri = lsp_types::Url::parse("file:///test.eu").unwrap();
        let table = SymbolTable::new();
        let actions = code_actions(source, &root, &full_range(), &uri, &table);
        assert!(actions.is_empty());
    }

    #[test]
    fn no_actions_for_resolved_name() {
        // `x` resolves directly — no quick fix needed
        let source = "x: 1\ny: x\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let uri = lsp_types::Url::parse("file:///test.eu").unwrap();
        let table = make_table(source);
        let actions = code_actions(source, &root, &full_range(), &uri, &table);
        // Should have no qualify-name actions (x resolves directly)
        let qualify_actions: Vec<_> = actions
            .iter()
            .filter(|a| a.title.starts_with("Replace with"))
            .collect();
        assert!(
            qualify_actions.is_empty(),
            "no qualify actions for directly resolved name"
        );
    }

    #[test]
    fn no_actions_for_declaration_head() {
        // An identifier in the declaration head shouldn't get a qualify action
        let source = "split: 42\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let uri = lsp_types::Url::parse("file:///test.eu").unwrap();
        let table = make_table(source);
        let actions = code_actions(source, &root, &full_range(), &uri, &table);
        let qualify_actions: Vec<_> = actions
            .iter()
            .filter(|a| a.title.starts_with("Replace with"))
            .collect();
        assert!(
            qualify_actions.is_empty(),
            "no qualify actions for declaration heads"
        );
    }

    #[test]
    fn ranges_overlap_works() {
        let a = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 5,
                character: 0,
            },
        };
        let b = Range {
            start: Position {
                line: 3,
                character: 0,
            },
            end: Position {
                line: 10,
                character: 0,
            },
        };
        assert!(ranges_overlap(&a, &b));
        let c = Range {
            start: Position {
                line: 6,
                character: 0,
            },
            end: Position {
                line: 10,
                character: 0,
            },
        };
        assert!(!ranges_overlap(&a, &c));
    }
}
