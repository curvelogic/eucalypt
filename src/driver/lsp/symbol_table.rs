//! Symbol table for LSP semantic features.
//!
//! Collects declaration information from parsed eucalypt files to
//! support go-to-definition, hover, completion, and find-references.
//! Symbols are gathered from multiple sources with defined priority:
//!
//! 1. Local scope (same block / file)
//! 2. Imported file declarations
//! 3. lsp-context file declarations
//! 4. Prelude declarations

use crate::syntax::rowan::ast::{
    AstToken, Block, Declaration, DeclarationKind, HasSoup, NormalIdentifier, Unit,
};
use lsp_types::Url;
use rowan::ast::AstNode;
use std::collections::HashMap;

use super::diagnostics::text_range_to_lsp_range;

/// The kind of a symbol declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclKind {
    Property,
    Function { arity: usize },
    Operator { fixity: String },
}

/// The source of a symbol — determines resolution priority.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SymbolSource {
    /// Declaration in the current file
    Local,
    /// Declaration from an imported file
    Import,
    /// Declaration from an lsp-context co-input
    Context,
    /// Declaration from the prelude
    Prelude,
}

/// Information about a single declared symbol.
#[derive(Debug, Clone)]
pub struct SymbolInfo {
    /// The symbol's name
    pub name: String,
    /// What kind of declaration
    pub kind: DeclKind,
    /// Source priority for resolution
    pub source: SymbolSource,
    /// URI of the file containing the declaration
    pub uri: Url,
    /// Full range of the declaration
    pub range: lsp_types::Range,
    /// Range of the declaration name (for selection)
    pub selection_range: lsp_types::Range,
    /// Documentation from `` ` { doc: "..." } `` metadata
    pub documentation: Option<String>,
    /// Parameter names for functions
    pub parameters: Vec<String>,
    /// Child symbols (for namespace blocks)
    pub children: Vec<SymbolInfo>,
}

/// A symbol table collecting declarations from multiple sources.
#[derive(Debug, Default)]
pub struct SymbolTable {
    /// Top-level symbols indexed by name
    symbols: HashMap<String, Vec<SymbolInfo>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a symbol to the table.
    pub fn add(&mut self, symbol: SymbolInfo) {
        self.symbols
            .entry(symbol.name.clone())
            .or_default()
            .push(symbol);
    }

    /// Look up symbols by name, ordered by source priority (local first).
    pub fn lookup(&self, name: &str) -> Vec<&SymbolInfo> {
        let mut results: Vec<&SymbolInfo> = self
            .symbols
            .get(name)
            .map(|v| v.iter().collect())
            .unwrap_or_default();
        results.sort_by_key(|s| s.source.clone());
        results
    }

    /// Look up a qualified name like `str.split`.
    ///
    /// Resolves the first component, then searches its children for
    /// the second component.
    pub fn lookup_qualified(&self, prefix: &str, member: &str) -> Vec<&SymbolInfo> {
        self.symbols
            .get(prefix)
            .into_iter()
            .flat_map(|syms| {
                syms.iter()
                    .flat_map(|s| s.children.iter().filter(|c| c.name == member))
            })
            .collect()
    }

    /// Return all symbols, for completion.
    pub fn all_symbols(&self) -> impl Iterator<Item = &SymbolInfo> {
        self.symbols.values().flat_map(|v| v.iter())
    }

    /// Return all top-level symbol names.
    pub fn all_names(&self) -> impl Iterator<Item = &str> {
        self.symbols.keys().map(|s| s.as_str())
    }

    /// Find qualified names for an unresolved identifier.
    ///
    /// Searches the children of all top-level namespace symbols to
    /// find entries matching `name`. Returns pairs of
    /// `(qualified_name, symbol_info)` like `("str.split", &SymbolInfo)`.
    pub fn find_qualified_matches(&self, name: &str) -> Vec<(String, &SymbolInfo)> {
        let mut matches = Vec::new();
        for syms in self.symbols.values() {
            for sym in syms {
                for child in &sym.children {
                    if child.name == name {
                        matches.push((format!("{}.{}", sym.name, child.name), child));
                    }
                }
            }
        }
        matches
    }

    /// Populate the symbol table from a parsed unit.
    pub fn add_from_unit(
        &mut self,
        unit: &Unit,
        source_text: &str,
        uri: &Url,
        source: SymbolSource,
    ) {
        for decl in unit.declarations() {
            if let Some(sym) = symbol_from_declaration(&decl, source_text, uri, &source) {
                self.add(sym);
            }
        }
    }

    /// Populate from a block's declarations (for namespace traversal).
    pub fn add_from_block(
        &mut self,
        block: &Block,
        source_text: &str,
        uri: &Url,
        source: SymbolSource,
    ) {
        for decl in block.declarations() {
            if let Some(sym) = symbol_from_declaration(&decl, source_text, uri, &source) {
                self.add(sym);
            }
        }
    }
}

/// Extract a symbol from a declaration.
fn symbol_from_declaration(
    decl: &Declaration,
    source_text: &str,
    uri: &Url,
    source: &SymbolSource,
) -> Option<SymbolInfo> {
    let head = decl.head()?;
    let kind = head.classify_declaration();

    let (name, decl_kind, parameters) = match &kind {
        DeclarationKind::Property(id) => (normal_id_name(id), DeclKind::Property, Vec::new()),
        DeclarationKind::Function(id, args) => {
            let params: Vec<String> = args
                .items()
                .flat_map(|soup| {
                    soup.elements().filter_map(|e| {
                        if let crate::syntax::rowan::ast::Element::Name(n) = e {
                            n.identifier().map(|id| id.text().to_string())
                        } else {
                            None
                        }
                    })
                })
                .collect();
            let arity = params.len();
            (normal_id_name(id), DeclKind::Function { arity }, params)
        }
        DeclarationKind::Nullary(_, op) => (
            op.text().to_string(),
            DeclKind::Operator {
                fixity: "nullary".to_string(),
            },
            Vec::new(),
        ),
        DeclarationKind::Prefix(_, op, _) => (
            op.text().to_string(),
            DeclKind::Operator {
                fixity: "prefix".to_string(),
            },
            Vec::new(),
        ),
        DeclarationKind::Postfix(_, _, op) => (
            op.text().to_string(),
            DeclKind::Operator {
                fixity: "postfix".to_string(),
            },
            Vec::new(),
        ),
        DeclarationKind::Binary(_, _, op, _) => (
            op.text().to_string(),
            DeclKind::Operator {
                fixity: "binary".to_string(),
            },
            Vec::new(),
        ),
        DeclarationKind::MalformedHead(_) => return None,
    };

    let range = text_range_to_lsp_range(source_text, decl.syntax().text_range());
    let selection_range = text_range_to_lsp_range(source_text, head.syntax().text_range());
    let documentation = extract_documentation(decl);
    let children = extract_children(decl, source_text, uri, source);

    Some(SymbolInfo {
        name,
        kind: decl_kind,
        source: source.clone(),
        uri: uri.clone(),
        range,
        selection_range,
        documentation,
        parameters,
        children,
    })
}

/// Extract documentation from declaration metadata.
///
/// Looks for `` ` { doc: "..." } `` in the declaration's metadata.
fn extract_documentation(decl: &Declaration) -> Option<String> {
    let meta = decl.meta()?;
    let soup = meta.soup()?;

    for element in soup.elements() {
        match element {
            // Block form: ` { doc: "..." }
            crate::syntax::rowan::ast::Element::Block(block) => {
                for inner_decl in block.declarations() {
                    if let Some(head) = inner_decl.head() {
                        if let DeclarationKind::Property(prop) = head.classify_declaration() {
                            if prop.text() == "doc" {
                                if let Some(body) = inner_decl.body() {
                                    if let Some(body_soup) = body.soup() {
                                        for el in body_soup.elements() {
                                            if let crate::syntax::rowan::ast::Element::Lit(lit) = el
                                            {
                                                if let Some(
                                                    crate::syntax::rowan::ast::LiteralValue::Str(s),
                                                ) = lit.value()
                                                {
                                                    return s.value().map(|v| v.to_string());
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // Bare string form: ` "docstring"
            crate::syntax::rowan::ast::Element::Lit(lit) => {
                if let Some(crate::syntax::rowan::ast::LiteralValue::Str(s)) = lit.value() {
                    return s.value().map(|v| v.to_string());
                }
            }
            _ => {}
        }
    }
    None
}

/// Extract child symbols from a declaration's body block.
///
/// If the declaration body is a single block, extract its declarations
/// as child symbols (used for namespace blocks like `str.` or `io.`).
fn extract_children(
    decl: &Declaration,
    source_text: &str,
    uri: &Url,
    source: &SymbolSource,
) -> Vec<SymbolInfo> {
    let body = match decl.body() {
        Some(b) => b,
        None => return vec![],
    };
    let soup = match body.soup() {
        Some(s) => s,
        None => return vec![],
    };

    let mut children = vec![];
    for element in soup.elements() {
        if let crate::syntax::rowan::ast::Element::Block(block) = element {
            for inner_decl in block.declarations() {
                if let Some(sym) = symbol_from_declaration(&inner_decl, source_text, uri, source) {
                    children.push(sym);
                }
            }
        }
    }
    children
}

/// Extract the name text from a NormalIdentifier.
fn normal_id_name(id: &NormalIdentifier) -> String {
    match id {
        NormalIdentifier::UnquotedIdentifier(u) => u.text().to_string(),
        NormalIdentifier::SingleQuoteIdentifier(s) => {
            let text = s.text();
            text.strip_prefix('\'')
                .and_then(|t| t.strip_suffix('\''))
                .unwrap_or(text)
                .to_string()
        }
    }
}

/// Parse the prelude source and extract symbols.
pub fn prelude_symbols(prelude_uri: &Url) -> SymbolTable {
    let prelude_source = crate::driver::resources::Resources::default()
        .get("prelude")
        .cloned()
        .unwrap_or_default();

    let parse = crate::syntax::rowan::parse_unit(&prelude_source);
    let unit = parse.tree();

    let mut table = SymbolTable::new();
    table.add_from_unit(&unit, &prelude_source, prelude_uri, SymbolSource::Prelude);
    table
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::rowan::parse_unit;

    fn test_uri() -> Url {
        Url::parse("file:///test/file.eu").unwrap()
    }

    #[test]
    fn test_property_symbol() {
        let source = "x: 1\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let mut table = SymbolTable::new();
        table.add_from_unit(&unit, source, &test_uri(), SymbolSource::Local);

        let results = table.lookup("x");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].kind, DeclKind::Property);
        assert_eq!(results[0].source, SymbolSource::Local);
    }

    #[test]
    fn test_function_symbol() {
        let source = "f(x, y): x\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let mut table = SymbolTable::new();
        table.add_from_unit(&unit, source, &test_uri(), SymbolSource::Local);

        let results = table.lookup("f");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].kind, DeclKind::Function { arity: 2 });
        assert_eq!(results[0].parameters, vec!["x", "y"]);
    }

    #[test]
    fn test_operator_symbol() {
        let source = "(x + y): x\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let mut table = SymbolTable::new();
        table.add_from_unit(&unit, source, &test_uri(), SymbolSource::Local);

        let results = table.lookup("+");
        assert_eq!(results.len(), 1);
        assert_eq!(
            results[0].kind,
            DeclKind::Operator {
                fixity: "binary".to_string()
            }
        );
    }

    #[test]
    fn test_source_priority() {
        let source = "x: 1\n";
        let parse = parse_unit(source);
        let unit = parse.tree();

        let mut table = SymbolTable::new();
        table.add_from_unit(&unit, source, &test_uri(), SymbolSource::Prelude);
        table.add_from_unit(&unit, source, &test_uri(), SymbolSource::Local);

        let results = table.lookup("x");
        assert_eq!(results.len(), 2);
        // Local should come first
        assert_eq!(results[0].source, SymbolSource::Local);
        assert_eq!(results[1].source, SymbolSource::Prelude);
    }

    #[test]
    fn test_nested_block_children() {
        let source = "ns: { inner: 1 f(x): x }\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let mut table = SymbolTable::new();
        table.add_from_unit(&unit, source, &test_uri(), SymbolSource::Local);

        let results = table.lookup("ns");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].children.len(), 2);
        assert_eq!(results[0].children[0].name, "inner");
        assert_eq!(results[0].children[1].name, "f");
    }

    #[test]
    fn test_qualified_lookup() {
        let source = "ns: { inner: 1 f(x): x }\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let mut table = SymbolTable::new();
        table.add_from_unit(&unit, source, &test_uri(), SymbolSource::Local);

        let results = table.lookup_qualified("ns", "inner");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "inner");
    }

    #[test]
    fn test_documentation_extraction() {
        let source = "` { doc: \"helper function\" }\nf(x): x\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let mut table = SymbolTable::new();
        table.add_from_unit(&unit, source, &test_uri(), SymbolSource::Local);

        let results = table.lookup("f");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].documentation.as_deref(), Some("helper function"));
    }

    #[test]
    fn test_all_names() {
        let source = "x: 1\ny: 2\nf(a): a\n";
        let parse = parse_unit(source);
        let unit = parse.tree();
        let mut table = SymbolTable::new();
        table.add_from_unit(&unit, source, &test_uri(), SymbolSource::Local);

        let mut names: Vec<&str> = table.all_names().collect();
        names.sort();
        assert_eq!(names, vec!["f", "x", "y"]);
    }

    #[test]
    fn test_prelude_symbols() {
        let uri = Url::parse("file:///prelude").unwrap();
        let table = prelude_symbols(&uri);

        // Prelude should contain common functions
        let map = table.lookup("map");
        assert!(!map.is_empty(), "prelude should contain 'map'");

        let filter = table.lookup("filter");
        assert!(!filter.is_empty(), "prelude should contain 'filter'");
    }
}
