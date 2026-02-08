//! Extract document symbols from the Rowan syntax tree.
//!
//! Walks the parsed `Unit` to produce `DocumentSymbol` items for
//! the outline view. Declarations become symbols with appropriate
//! kinds (property, function, operator). Nested blocks produce
//! nested symbol hierarchies.

use crate::syntax::rowan::ast::{
    AstToken, Block, Declaration, DeclarationKind, HasSoup, NormalIdentifier, Unit,
};
use lsp_types::{DocumentSymbol, SymbolKind};
use rowan::ast::AstNode;

use super::diagnostics::text_range_to_lsp_range;

/// Extract document symbols from a parsed unit.
pub fn document_symbols(source: &str, unit: &Unit) -> Vec<DocumentSymbol> {
    unit.declarations()
        .filter_map(|decl| declaration_symbol(source, &decl))
        .collect()
}

/// Convert a declaration to a document symbol, recursing into nested blocks.
fn declaration_symbol(source: &str, decl: &Declaration) -> Option<DocumentSymbol> {
    let head = decl.head()?;
    let kind = head.classify_declaration();

    let (name, symbol_kind, detail) = match &kind {
        DeclarationKind::Property(id) => (normal_id_name(id), SymbolKind::PROPERTY, None),
        DeclarationKind::Function(id, args) => {
            let param_count = args.items().count();
            let detail = format!(
                "{param_count} parameter{}",
                if param_count == 1 { "" } else { "s" }
            );
            (normal_id_name(id), SymbolKind::FUNCTION, Some(detail))
        }
        DeclarationKind::Nullary(_, op) => (
            op.text().to_string(),
            SymbolKind::OPERATOR,
            Some("nullary".to_string()),
        ),
        DeclarationKind::Prefix(_, op, _) => (
            op.text().to_string(),
            SymbolKind::OPERATOR,
            Some("prefix".to_string()),
        ),
        DeclarationKind::Postfix(_, _, op) => (
            op.text().to_string(),
            SymbolKind::OPERATOR,
            Some("postfix".to_string()),
        ),
        DeclarationKind::Binary(_, _, op, _) => (
            op.text().to_string(),
            SymbolKind::OPERATOR,
            Some("binary".to_string()),
        ),
        DeclarationKind::MalformedHead(_) => return None,
    };

    let range = text_range_to_lsp_range(source, decl.syntax().text_range());
    let selection_range = text_range_to_lsp_range(source, head.syntax().text_range());

    // Recurse into nested blocks in the declaration body
    let children = nested_block_symbols(source, decl);

    #[allow(deprecated)]
    Some(DocumentSymbol {
        name,
        detail,
        kind: symbol_kind,
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children: if children.is_empty() {
            None
        } else {
            Some(children)
        },
    })
}

/// Extract the name text from a NormalIdentifier.
fn normal_id_name(id: &NormalIdentifier) -> String {
    match id {
        NormalIdentifier::UnquotedIdentifier(u) => u.text().to_string(),
        NormalIdentifier::SingleQuoteIdentifier(s) => {
            // Strip surrounding quotes for display
            let text = s.text();
            text.strip_prefix('\'')
                .and_then(|t| t.strip_suffix('\''))
                .unwrap_or(text)
                .to_string()
        }
    }
}

/// Find nested block symbols within a declaration's body.
fn nested_block_symbols(source: &str, decl: &Declaration) -> Vec<DocumentSymbol> {
    let body = match decl.body() {
        Some(b) => b,
        None => return vec![],
    };

    let soup = match body.soup() {
        Some(s) => s,
        None => return vec![],
    };

    // Look for block elements in the body soup
    let mut symbols = vec![];
    for element in soup.elements() {
        if let crate::syntax::rowan::ast::Element::Block(block) = element {
            symbols.extend(block_symbols(source, &block));
        }
    }
    symbols
}

/// Extract symbols from a block's declarations.
fn block_symbols(source: &str, block: &Block) -> Vec<DocumentSymbol> {
    block
        .declarations()
        .filter_map(|decl| declaration_symbol(source, &decl))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::rowan::parse_unit;

    #[test]
    fn simple_properties() {
        let source = "x: 1\ny: 2\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let symbols = document_symbols(source, &unit);
        assert_eq!(symbols.len(), 2);
        assert_eq!(symbols[0].name, "x");
        assert_eq!(symbols[0].kind, SymbolKind::PROPERTY);
        assert_eq!(symbols[1].name, "y");
    }

    #[test]
    fn function_declaration() {
        let source = "f(x, y): x\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let symbols = document_symbols(source, &unit);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "f");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[0].detail.as_deref(), Some("2 parameters"));
    }

    #[test]
    fn operator_declaration() {
        let source = "(x + y): x\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let symbols = document_symbols(source, &unit);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "+");
        assert_eq!(symbols[0].kind, SymbolKind::OPERATOR);
        assert_eq!(symbols[0].detail.as_deref(), Some("binary"));
    }

    #[test]
    fn nested_block_produces_children() {
        let source = "ns: { inner: 1 }\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let symbols = document_symbols(source, &unit);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "ns");
        let children = symbols[0].children.as_ref().expect("should have children");
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].name, "inner");
    }

    #[test]
    fn empty_source_produces_no_symbols() {
        let source = "";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let symbols = document_symbols(source, &unit);
        assert!(symbols.is_empty());
    }

    #[test]
    fn single_param_detail() {
        let source = "f(x): x\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let symbols = document_symbols(source, &unit);
        assert_eq!(symbols[0].detail.as_deref(), Some("1 parameter"));
    }
}
