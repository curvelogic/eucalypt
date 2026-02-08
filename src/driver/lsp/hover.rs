//! Hover information for the eucalypt LSP.
//!
//! Provides symbol information on hover: declaration kind, parameters,
//! documentation, and source file.

use crate::syntax::rowan::kind::{SyntaxKind, SyntaxNode, SyntaxToken};
use lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};

use super::navigation::{find_identifier_at, resolve_dotted};
use super::symbol_table::{DeclKind, SymbolInfo, SymbolTable};

/// Produce hover information for the symbol at the given cursor position.
pub fn hover(
    source: &str,
    root: &SyntaxNode,
    position: &Position,
    table: &SymbolTable,
) -> Option<Hover> {
    let offset = super::selection::position_to_offset(source, position);
    let token = find_identifier_at(root, offset)?;
    let name = identifier_text(&token);

    // Check for dotted access
    if let Some((prefix, member)) = resolve_dotted(root, &token) {
        let symbols = table.lookup_qualified(&prefix, &member);
        if let Some(sym) = symbols.first() {
            return Some(make_hover(sym, Some(&prefix)));
        }
    }

    // Simple name lookup
    let symbols = table.lookup(&name);
    let sym = symbols.first()?;
    Some(make_hover(sym, None))
}

/// Build an LSP Hover response from a symbol.
fn make_hover(sym: &SymbolInfo, qualifier: Option<&str>) -> Hover {
    let mut lines = Vec::new();

    // Signature line
    let qualified_name = match qualifier {
        Some(q) => format!("{}.{}", q, sym.name),
        None => sym.name.clone(),
    };

    match &sym.kind {
        DeclKind::Property => {
            lines.push(format!("**{}** — property", qualified_name));
        }
        DeclKind::Function { arity } => {
            let params = if sym.parameters.is_empty() {
                format!("{arity} parameter{}", if *arity == 1 { "" } else { "s" })
            } else {
                sym.parameters.join(", ")
            };
            lines.push(format!("**{}**({}) — function", qualified_name, params));
        }
        DeclKind::Operator { fixity } => {
            lines.push(format!("**{}** — {} operator", qualified_name, fixity));
        }
    }

    // Source information
    let uri_str = sym.uri.as_str();
    if uri_str.starts_with("resource:") || uri_str.contains("prelude") {
        lines.push("*prelude*".to_string());
    }

    // Documentation
    if let Some(doc) = &sym.documentation {
        lines.push(String::new());
        lines.push(doc.clone());
    }

    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: lines.join("\n"),
        }),
        range: None,
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
    fn test_hover_property() {
        let source = "x: 1\ny: x\n";
        let (table, _) = setup_table(source);
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        let pos = Position {
            line: 1,
            character: 3,
        };
        let result = hover(source, &root, &pos, &table);
        assert!(result.is_some());
        let h = result.unwrap();
        if let HoverContents::Markup(m) = &h.contents {
            assert!(m.value.contains("property"), "should mention property");
            assert!(m.value.contains("x"), "should mention name");
        } else {
            panic!("expected markup content");
        }
    }

    #[test]
    fn test_hover_function() {
        let source = "f(a, b): a\ny: f(1, 2)\n";
        let (table, _) = setup_table(source);
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        let pos = Position {
            line: 1,
            character: 3,
        };
        let result = hover(source, &root, &pos, &table);
        assert!(result.is_some());
        let h = result.unwrap();
        if let HoverContents::Markup(m) = &h.contents {
            assert!(m.value.contains("function"), "should mention function");
            assert!(m.value.contains("a, b"), "should show parameters");
        } else {
            panic!("expected markup content");
        }
    }

    #[test]
    fn test_hover_with_doc() {
        let source = "` { doc: \"A helper\" }\nf(x): x\ny: f(1)\n";
        let (table, _) = setup_table(source);
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        let pos = Position {
            line: 2,
            character: 3,
        };
        let result = hover(source, &root, &pos, &table);
        assert!(result.is_some());
        let h = result.unwrap();
        if let HoverContents::Markup(m) = &h.contents {
            assert!(m.value.contains("A helper"), "should include documentation");
        } else {
            panic!("expected markup content");
        }
    }

    #[test]
    fn test_hover_no_result_for_literal() {
        let source = "x: 42\n";
        let (table, _) = setup_table(source);
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        let pos = Position {
            line: 0,
            character: 3,
        };
        let result = hover(source, &root, &pos, &table);
        assert!(result.is_none());
    }
}
