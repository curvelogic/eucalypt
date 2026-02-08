//! Completion provider for the eucalypt LSP.
//!
//! Offers completion items from the symbol table at the cursor position.
//! Supports dotted access completion (e.g. `str.` triggers completion
//! of `str` block members) and general name completion from all sources.

use crate::syntax::rowan::kind::{SyntaxKind, SyntaxNode};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionResponse, Documentation, MarkupContent,
    MarkupKind, Position,
};
use rowan::TextSize;

use super::selection::position_to_offset;
use super::symbol_table::{DeclKind, SymbolInfo, SymbolTable};

/// Compute completions at the given cursor position.
///
/// If the cursor follows a dot (e.g. `str.`), returns members of the
/// prefix symbol. Otherwise returns all symbols in scope.
pub fn completions(
    source: &str,
    root: &SyntaxNode,
    position: &Position,
    table: &SymbolTable,
) -> CompletionResponse {
    let offset = position_to_offset(source, position);

    // Check for dotted access: is there a `prefix.` immediately before the cursor?
    if let Some(prefix) = detect_dot_prefix(root, offset) {
        let items = complete_members(table, &prefix);
        return CompletionResponse::Array(items);
    }

    // General completion: all symbols in scope
    let items = complete_all(table);
    CompletionResponse::Array(items)
}

/// Detect if cursor is right after `prefix.` and return the prefix name.
///
/// Walks backward from the cursor offset to find a dot operator preceded
/// by an identifier.
fn detect_dot_prefix(root: &SyntaxNode, offset: TextSize) -> Option<String> {
    // Find the token at the cursor position
    let token = root.token_at_offset(offset).left_biased()?;

    // The token immediately before the cursor should be `.` (OPERATOR_IDENTIFIER)
    if token.kind() == SyntaxKind::OPERATOR_IDENTIFIER && token.text() == "." {
        // The token before the dot should be an identifier
        let prefix_token = token.prev_token()?;
        if matches!(
            prefix_token.kind(),
            SyntaxKind::UNQUOTED_IDENTIFIER | SyntaxKind::SINGLE_QUOTE_IDENTIFIER
        ) {
            let text = prefix_token.text();
            let name = match prefix_token.kind() {
                SyntaxKind::SINGLE_QUOTE_IDENTIFIER => text
                    .strip_prefix('\'')
                    .and_then(|t| t.strip_suffix('\''))
                    .unwrap_or(text),
                _ => text,
            };
            return Some(name.to_string());
        }
    }

    // Also handle the case where the user has started typing after the dot,
    // e.g. `str.sp` — the token at cursor is an identifier, and before it
    // is `.`, and before that is the prefix.
    if matches!(
        token.kind(),
        SyntaxKind::UNQUOTED_IDENTIFIER | SyntaxKind::SINGLE_QUOTE_IDENTIFIER
    ) {
        if let Some(dot) = token.prev_token() {
            if dot.kind() == SyntaxKind::OPERATOR_IDENTIFIER && dot.text() == "." {
                if let Some(prefix_token) = dot.prev_token() {
                    if matches!(
                        prefix_token.kind(),
                        SyntaxKind::UNQUOTED_IDENTIFIER | SyntaxKind::SINGLE_QUOTE_IDENTIFIER
                    ) {
                        let text = prefix_token.text();
                        let name = match prefix_token.kind() {
                            SyntaxKind::SINGLE_QUOTE_IDENTIFIER => text
                                .strip_prefix('\'')
                                .and_then(|t| t.strip_suffix('\''))
                                .unwrap_or(text),
                            _ => text,
                        };
                        return Some(name.to_string());
                    }
                }
            }
        }
    }

    None
}

/// Complete members of a namespace block.
fn complete_members(table: &SymbolTable, prefix: &str) -> Vec<CompletionItem> {
    let symbols = table.lookup(prefix);
    symbols
        .into_iter()
        .flat_map(|sym| sym.children.iter())
        .map(symbol_to_completion_item)
        .collect()
}

/// Complete all top-level symbols.
fn complete_all(table: &SymbolTable) -> Vec<CompletionItem> {
    table.all_symbols().map(symbol_to_completion_item).collect()
}

/// Convert a symbol to a completion item.
fn symbol_to_completion_item(sym: &SymbolInfo) -> CompletionItem {
    let kind = match &sym.kind {
        DeclKind::Property => CompletionItemKind::PROPERTY,
        DeclKind::Function { .. } => CompletionItemKind::FUNCTION,
        DeclKind::Operator { .. } => CompletionItemKind::OPERATOR,
    };

    let detail = match &sym.kind {
        DeclKind::Property => None,
        DeclKind::Function { arity } => {
            if sym.parameters.is_empty() {
                Some(format!(
                    "{arity} parameter{}",
                    if *arity == 1 { "" } else { "s" }
                ))
            } else {
                Some(format!("({})", sym.parameters.join(", ")))
            }
        }
        DeclKind::Operator { fixity } => Some(format!("{fixity} operator")),
    };

    let documentation = sym.documentation.as_ref().map(|doc| {
        Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: doc.clone(),
        })
    });

    CompletionItem {
        label: sym.name.clone(),
        kind: Some(kind),
        detail,
        documentation,
        ..CompletionItem::default()
    }
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

    fn items(response: CompletionResponse) -> Vec<CompletionItem> {
        match response {
            CompletionResponse::Array(items) => items,
            CompletionResponse::List(list) => list.items,
        }
    }

    #[test]
    fn test_complete_all_symbols() {
        let source = "x: 1\nf(a): a\ny: 2\n";
        let (table, _) = setup_table(source);
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        // Complete at start of a new line (general completion)
        let pos = Position {
            line: 2,
            character: 0,
        };
        let result = items(completions(source, &root, &pos, &table));
        let names: Vec<&str> = result.iter().map(|i| i.label.as_str()).collect();
        assert!(names.contains(&"x"), "should contain x");
        assert!(names.contains(&"f"), "should contain f");
        assert!(names.contains(&"y"), "should contain y");
    }

    #[test]
    fn test_complete_function_has_detail() {
        let source = "f(a, b): a\n";
        let (table, _) = setup_table(source);
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        let pos = Position {
            line: 0,
            character: 0,
        };
        let result = items(completions(source, &root, &pos, &table));
        let f_item = result.iter().find(|i| i.label == "f").unwrap();
        assert_eq!(f_item.kind, Some(CompletionItemKind::FUNCTION));
        assert_eq!(f_item.detail.as_deref(), Some("(a, b)"));
    }

    #[test]
    fn test_complete_dotted_access() {
        let source = "ns: { inner: 1 f(x): x }\ny: ns.\n";
        let (table, _) = setup_table(source);
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        // Position right after `ns.` — cursor is after the dot (character 6)
        let pos = Position {
            line: 1,
            character: 6,
        };
        let result = items(completions(source, &root, &pos, &table));
        let names: Vec<&str> = result.iter().map(|i| i.label.as_str()).collect();
        assert!(
            names.contains(&"inner"),
            "should complete inner from ns block"
        );
        assert!(names.contains(&"f"), "should complete f from ns block");
        assert!(
            !names.contains(&"ns"),
            "should not include the namespace itself"
        );
    }

    #[test]
    fn test_complete_dotted_with_partial() {
        let source = "ns: { inner: 1 foo: 2 }\ny: ns.in\n";
        let (table, _) = setup_table(source);
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        // Position on `in` after `ns.` — cursor at the end of `in`
        let pos = Position {
            line: 1,
            character: 7,
        };
        let result = items(completions(source, &root, &pos, &table));
        let names: Vec<&str> = result.iter().map(|i| i.label.as_str()).collect();
        // Should return all members (filtering is done by the editor)
        assert!(
            names.contains(&"inner"),
            "should complete inner from ns block"
        );
        assert!(names.contains(&"foo"), "should complete foo from ns block");
    }

    #[test]
    fn test_complete_with_documentation() {
        let source = "` { doc: \"A helper\" }\nf(x): x\n";
        let (table, _) = setup_table(source);
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        let pos = Position {
            line: 0,
            character: 0,
        };
        let result = items(completions(source, &root, &pos, &table));
        let f_item = result.iter().find(|i| i.label == "f").unwrap();
        assert!(f_item.documentation.is_some(), "should have documentation");
    }

    #[test]
    fn test_complete_property_kind() {
        let source = "x: 1\n";
        let (table, _) = setup_table(source);
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        let pos = Position {
            line: 0,
            character: 0,
        };
        let result = items(completions(source, &root, &pos, &table));
        let x_item = result.iter().find(|i| i.label == "x").unwrap();
        assert_eq!(x_item.kind, Some(CompletionItemKind::PROPERTY));
        assert!(x_item.detail.is_none(), "property should have no detail");
    }
}
