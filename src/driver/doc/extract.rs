//! Data model and AST extraction for `eu doc`.
//!
//! Defines the documentation entry types (`DocEntry`, `DocKind`, etc.) and
//! the functions that walk a parsed `Unit` AST to extract them.

use crate::syntax::rowan::ast::{self, AstToken, DeclarationKind, HasSoup, LiteralValue, Unit};
use rowan::ast::AstNode;

// ── Data model ───────────────────────────────────────────────────────────────

/// Visibility of a binding.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DocVisibility {
    /// Rendered in output, visible to importers (default).
    Normal,
    /// Hidden from output, visible to importers (`:suppress`).
    Suppress,
    /// Hidden from output and importers (`:internal`).
    Internal,
}

/// Deprecation information for a binding.
#[derive(Debug, Clone)]
pub struct DeprecationInfo {
    /// Optional message explaining the deprecation.
    pub message: Option<String>,
    /// Optional replacement name.
    pub replaced_by: Option<String>,
}

/// The kind of a documented binding.
#[derive(Debug, Clone)]
pub enum DocKind {
    /// A plain `name: expr` binding.
    Property,
    /// A function `f(x, y): expr`.
    Function { params: Vec<String> },
    /// An operator `(l op r): expr`.
    Operator { fixity: String },
}

/// A single documented entry extracted from a eucalypt source unit.
#[derive(Debug, Clone)]
pub struct DocEntry {
    /// Binding name (e.g. `"map"`, `"str"`, `"<"`).
    pub name: String,
    /// Namespace prefix if inside a namespace block (e.g. `"str"` for `str.split`).
    pub namespace: Option<String>,
    /// Kind of binding.
    pub kind: DocKind,
    /// Doc string from `` ` "..." `` or `` ` { doc: "..." } `` metadata.
    pub doc: Option<String>,
    /// Type annotation from `` ` { type: "..." } `` metadata (raw string).
    pub type_annotation: Option<String>,
    /// Deprecation status.
    pub deprecated: Option<DeprecationInfo>,
    /// Visibility (public, internal, suppress).
    pub visibility: DocVisibility,
    /// Section heading from a `##` comment immediately above the declaration.
    pub section: Option<String>,
    /// Example code from `` ` { example: "..." } `` metadata.
    pub example: Option<String>,
    /// See-also references from `` ` { see-also: [...] } `` metadata (raw).
    pub see_also: Vec<String>,
    /// Child entries for namespace blocks.
    pub children: Vec<DocEntry>,
}

impl DocEntry {
    /// Fully-qualified name (namespace.name or just name).
    pub fn qualified_name(&self) -> String {
        if let Some(ns) = &self.namespace {
            format!("{ns}.{}", self.name)
        } else {
            self.name.clone()
        }
    }

    /// Whether this entry is visible in documentation (not internal or suppress).
    pub fn is_public(&self) -> bool {
        matches!(self.visibility, DocVisibility::Normal)
    }
}

// ── Extraction ────────────────────────────────────────────────────────────────

/// Extract all documentation entries from a parsed `Unit`.
///
/// Walks the top-level declarations and collects `DocEntry` values.
/// Section headings are extracted from `##` comments, which may appear
/// either at the top level of the unit or inside the unit-metadata BLOCK_META
/// soup (between the unit doc string and the first declaration).
pub fn extract_doc_entries(unit: &Unit) -> Vec<DocEntry> {
    let mut entries = Vec::new();
    let mut current_section: Option<String> = None;

    // The unit-level BLOCK_META soup (unit metadata expression) may contain
    // section headings in comments that precede the first declaration.
    // Walk those tokens first so the section is set before we see declarations.
    if let Some(meta) = unit.meta() {
        for tok in meta.syntax().descendants_with_tokens() {
            use rowan::NodeOrToken;
            if let NodeOrToken::Token(t) = tok {
                if t.kind() == crate::syntax::rowan::kind::SyntaxKind::COMMENT {
                    if let Some(heading) = parse_section_heading(t.text()) {
                        current_section = Some(heading);
                    }
                }
            }
        }
    }

    // Now walk the unit's direct children (declarations and inter-declaration comments).
    for node in unit.syntax().children_with_tokens() {
        use rowan::NodeOrToken;
        match node {
            NodeOrToken::Token(tok) => {
                // Extract `##` section headings from comments.
                if tok.kind() == crate::syntax::rowan::kind::SyntaxKind::COMMENT {
                    let text = tok.text();
                    if let Some(heading) = parse_section_heading(text) {
                        current_section = Some(heading);
                    }
                }
            }
            NodeOrToken::Node(node) => {
                if let Some(decl) = ast::Declaration::cast(node) {
                    if let Some(mut entry) = entry_from_declaration(&decl) {
                        entry.section = current_section.clone();
                        entries.push(entry);
                    }
                }
            }
        }
    }

    entries
}

/// Extract the unit-level documentation from unit metadata.
///
/// Unit metadata is the first expression in a file (before any declarations).
/// Two forms are supported:
/// - Block with `doc:` key: `{ doc: "description" }`
/// - Bare string: `"description"`
pub fn extract_unit_doc(unit: &Unit) -> Option<String> {
    // Unit metadata lives in the BLOCK_META node (accessed via unit.meta()),
    // not in regular declarations.
    let meta = unit.meta()?;
    let soup = meta.soup()?;
    for el in soup.elements() {
        match el {
            ast::Element::Block(block) => {
                if let Some(doc_str) = extract_block_str(&block, "doc") {
                    return Some(collapse_whitespace(&doc_str));
                }
            }
            ast::Element::Lit(lit) => {
                if let Some(ast::LiteralValue::Str(s)) = lit.value() {
                    if let Some(v) = s.value() {
                        return Some(collapse_whitespace(v));
                    }
                }
            }
            _ => {}
        }
    }
    None
}

/// Known single-`#` section names that are treated as section headings.
///
/// These map the same single-hash comment lines that the Python script's
/// `SINGLE_SECTION_TO_CATEGORY` recognises.
const KNOWN_SINGLE_HASH_SECTIONS: &[&str] = &[
    "Utilities",
    "Metadata basics",
    "List library functions, maps and folds",
    "Block library functions",
    "By property alteration of blocks",
];

/// Parse a section heading from a comment token.
///
/// Accepts `## Section Name` (double-hash) at the start of a comment line,
/// as well as single-`#` comments whose text matches a known section name.
/// Returns the trimmed heading text, or `None` if not a section comment.
fn parse_section_heading(comment: &str) -> Option<String> {
    let text = comment.trim();
    // Strip leading `#` characters
    let after_hashes = text.trim_start_matches('#');
    let leading_hashes = text.len() - after_hashes.len();

    if leading_hashes >= 2 && after_hashes.starts_with(' ') {
        // Double (or more) hash — treat as section heading
        let heading = after_hashes.trim().to_string();
        if !heading.is_empty() {
            return Some(heading);
        }
    } else if leading_hashes == 1 && after_hashes.starts_with(' ') {
        // Single hash — only recognised as a section heading for known names
        let heading = after_hashes.trim();
        if KNOWN_SINGLE_HASH_SECTIONS.contains(&heading) {
            return Some(heading.to_string());
        }
    }
    None
}

/// Build a `DocEntry` from a single declaration node.
fn entry_from_declaration(decl: &ast::Declaration) -> Option<DocEntry> {
    let head = decl.head()?;
    let kind = head.classify_declaration();

    let (name, doc_kind) = match &kind {
        DeclarationKind::Property(id) => (id.value().to_string(), DocKind::Property),
        DeclarationKind::Function(id, args) => {
            let params: Vec<String> = args
                .items()
                .flat_map(|soup| {
                    soup.elements().filter_map(|e| {
                        if let ast::Element::Name(n) = e {
                            n.identifier().map(|id| id.value().to_string())
                        } else {
                            None
                        }
                    })
                })
                .collect();
            (id.value().to_string(), DocKind::Function { params })
        }
        DeclarationKind::Binary(_, _, op, _) => (
            op.text().to_string(),
            DocKind::Operator {
                fixity: "binary".to_string(),
            },
        ),
        DeclarationKind::Prefix(_, op, _) => (
            op.text().to_string(),
            DocKind::Operator {
                fixity: "prefix".to_string(),
            },
        ),
        DeclarationKind::Postfix(_, _, op) => (
            op.text().to_string(),
            DocKind::Operator {
                fixity: "postfix".to_string(),
            },
        ),
        DeclarationKind::Nullary(_, op) => (
            op.text().to_string(),
            DocKind::Operator {
                fixity: "nullary".to_string(),
            },
        ),
        DeclarationKind::BracketPair(_, bracket_expr, _) => {
            let name = bracket_expr
                .bracket_pair_name()
                .unwrap_or_else(|| "bracket".to_string());
            (name, DocKind::Function { params: Vec::new() })
        }
        DeclarationKind::BracketBlockDef(_, bracket_expr) => {
            let name = bracket_expr
                .bracket_pair_name()
                .unwrap_or_else(|| "bracket".to_string());
            (name, DocKind::Function { params: Vec::new() })
        }
        DeclarationKind::MalformedHead(_) => return None,
    };

    let doc = extract_doc(decl);
    let type_annotation = extract_meta_str(decl, "type");
    let deprecated = extract_deprecation(decl);
    let visibility = extract_visibility(decl);
    let example = extract_meta_str(decl, "example");
    let see_also = extract_see_also(decl);
    let children = extract_children(decl);

    Some(DocEntry {
        name,
        namespace: None,
        kind: doc_kind,
        doc,
        type_annotation,
        deprecated,
        visibility,
        section: None, // filled in by caller
        example,
        see_also,
        children,
    })
}

/// Extract doc string from a declaration's metadata.
///
/// Handles `` ` "text" `` and `` ` { doc: "text" } `` forms.
fn extract_doc(decl: &ast::Declaration) -> Option<String> {
    let meta = decl.meta()?;
    let soup = meta.soup()?;

    for element in soup.elements() {
        match element {
            ast::Element::Block(block) => {
                if let Some(s) = extract_block_str(&block, "doc") {
                    return Some(s);
                }
            }
            // Bare string form: ` "docstring"
            ast::Element::Lit(lit) => {
                if let Some(s) = lit_to_str(&lit) {
                    return Some(s);
                }
            }
            _ => {}
        }
    }
    None
}

/// Extract a string-valued metadata field by key from a metadata block.
///
/// Handles plain strings, c-strings (string patterns), and s-strings.
fn extract_meta_str(decl: &ast::Declaration, key: &str) -> Option<String> {
    let meta = decl.meta()?;
    let soup = meta.soup()?;
    for element in soup.elements() {
        if let ast::Element::Block(block) = element {
            if let Some(s) = extract_block_str(&block, key) {
                return Some(s);
            }
        }
    }
    None
}

/// Extract a string value for `key` from within a block.
pub(crate) fn extract_block_str(block: &ast::Block, key: &str) -> Option<String> {
    for inner_decl in block.declarations() {
        let head = inner_decl.head()?;
        if let DeclarationKind::Property(prop) = head.classify_declaration() {
            if prop.text() != key {
                continue;
            }
            let body = inner_decl.body()?;
            let body_soup = body.soup()?;
            for el in body_soup.elements() {
                match el {
                    ast::Element::Lit(lit) => {
                        if let Some(s) = lit_to_str(&lit) {
                            return Some(s);
                        }
                    }
                    ast::Element::StringPattern(sp) => {
                        return sp.plain_value();
                    }
                    _ => {}
                }
            }
        }
    }
    None
}

/// Extract a symbol name for `key` from within a block (e.g. `export: :internal`).
fn extract_block_sym(block: &ast::Block, key: &str) -> Option<String> {
    for inner_decl in block.declarations() {
        let head = inner_decl.head()?;
        if let DeclarationKind::Property(prop) = head.classify_declaration() {
            if prop.text() != key {
                continue;
            }
            let body = inner_decl.body()?;
            let body_soup = body.soup()?;
            for el in body_soup.elements() {
                if let ast::Element::Lit(lit) = el {
                    if let Some(LiteralValue::Sym(s)) = lit.value() {
                        return s.value().map(|v| v.to_string());
                    }
                }
            }
        }
    }
    None
}

/// Check if a block has a boolean-true field with the given key.
fn extract_block_bool(block: &ast::Block, key: &str) -> bool {
    for inner_decl in block.declarations() {
        if let Some(head) = inner_decl.head() {
            if let DeclarationKind::Property(prop) = head.classify_declaration() {
                if prop.text() == key {
                    if let Some(body) = inner_decl.body() {
                        if let Some(body_soup) = body.soup() {
                            for el in body_soup.elements() {
                                if let ast::Element::Name(n) = el {
                                    if let Some(id) = n.identifier() {
                                        if id.text() == "true" {
                                            return true;
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
    false
}

/// Extract a string value from a `Literal` node, accepting Str and SStr.
fn lit_to_str(lit: &ast::Literal) -> Option<String> {
    match lit.value()? {
        LiteralValue::Str(s) => s.value().map(|v| v.to_string()),
        // s"..." type-data literals are accepted as annotation strings.
        LiteralValue::SStr(s) => s.value().map(|v| v.to_string()),
        _ => None,
    }
}

/// Extract deprecation information from metadata.
fn extract_deprecation(decl: &ast::Declaration) -> Option<DeprecationInfo> {
    let meta = decl.meta()?;
    let soup = meta.soup()?;

    for element in soup.elements() {
        match element {
            // ` :deprecated shorthand — symbols appear as Lit(Sym), not Name
            ast::Element::Name(_) => {}
            ast::Element::Lit(lit) => {
                if let Some(LiteralValue::Sym(s)) = lit.value() {
                    if s.value() == Some("deprecated") {
                        return Some(DeprecationInfo {
                            message: None,
                            replaced_by: None,
                        });
                    }
                }
            }
            ast::Element::Block(block) => {
                let deprecated_val = extract_block_str(&block, "deprecated");
                if deprecated_val.is_some() || extract_block_bool(&block, "deprecated") {
                    let message = deprecated_val;
                    let replaced_by = extract_block_str(&block, "replaced-by");
                    return Some(DeprecationInfo {
                        message,
                        replaced_by,
                    });
                }
            }
            _ => {}
        }
    }
    None
}

/// Extract visibility from metadata (`:internal`, `:suppress`, `export: :suppress`).
fn extract_visibility(decl: &ast::Declaration) -> DocVisibility {
    let meta = match decl.meta() {
        Some(m) => m,
        None => return DocVisibility::Normal,
    };
    let soup = match meta.soup() {
        Some(s) => s,
        None => return DocVisibility::Normal,
    };

    for element in soup.elements() {
        match element {
            ast::Element::Lit(lit) => {
                if let Some(LiteralValue::Sym(s)) = lit.value() {
                    match s.value() {
                        Some("suppress") => return DocVisibility::Suppress,
                        Some("internal") => return DocVisibility::Internal,
                        _ => {}
                    }
                }
            }
            ast::Element::Block(block) => {
                // `export:` key accepts a symbol value (:internal, :suppress)
                // or a string value ("internal", "suppress").
                if let Some(sym) = extract_block_sym(&block, "export") {
                    match sym.as_str() {
                        "suppress" => return DocVisibility::Suppress,
                        "internal" => return DocVisibility::Internal,
                        _ => {}
                    }
                } else if let Some(s) = extract_block_str(&block, "export") {
                    match s.as_str() {
                        "suppress" => return DocVisibility::Suppress,
                        "internal" => return DocVisibility::Internal,
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
    DocVisibility::Normal
}

/// Extract see-also references from metadata.
///
/// Reads `see-also: [:fn-a, :fn-b]` from backtick block metadata.
/// The value is a eucalypt list (`Element::List`), not a block.
///
/// Uses early returns via helper to keep nesting shallow.
fn extract_see_also(decl: &ast::Declaration) -> Vec<String> {
    let meta = match decl.meta() {
        Some(m) => m,
        None => return vec![],
    };
    let soup = match meta.soup() {
        Some(s) => s,
        None => return vec![],
    };

    for element in soup.elements() {
        if let ast::Element::Block(block) = element {
            if let Some(refs) = extract_see_also_from_block(&block) {
                return refs;
            }
        }
    }
    vec![]
}

/// Extract see-also references from a single metadata block.
fn extract_see_also_from_block(block: &ast::Block) -> Option<Vec<String>> {
    for inner_decl in block.declarations() {
        let head = inner_decl.head()?;
        if let DeclarationKind::Property(prop) = head.classify_declaration() {
            if prop.text() != "see-also" {
                continue;
            }
            let body = inner_decl.body()?;
            let body_soup = body.soup()?;
            for el in body_soup.elements() {
                if let ast::Element::List(list) = el {
                    return Some(collect_symbol_refs(&list));
                }
            }
        }
    }
    None
}

/// Collect symbol names from a list literal.
fn collect_symbol_refs(list: &ast::List) -> Vec<String> {
    let mut refs = vec![];
    for item_soup in list.items() {
        for item_el in item_soup.elements() {
            if let ast::Element::Lit(lit) = item_el {
                if let Some(LiteralValue::Sym(s)) = lit.value() {
                    if let Some(n) = s.value() {
                        refs.push(n.to_string());
                    }
                }
            }
        }
    }
    refs
}

/// Extract child entries for namespace blocks.
///
/// If the declaration body is a single block, its declarations become children.
fn extract_children(decl: &ast::Declaration) -> Vec<DocEntry> {
    let body = match decl.body() {
        Some(b) => b,
        None => return vec![],
    };
    let soup = match body.soup() {
        Some(s) => s,
        None => return vec![],
    };

    let namespace = match decl.head() {
        Some(h) => match h.classify_declaration() {
            DeclarationKind::Property(id) => Some(id.text().to_string()),
            _ => None,
        },
        None => None,
    };

    let mut children = vec![];
    for element in soup.elements() {
        if let ast::Element::Block(block) = element {
            for inner_decl in block.declarations() {
                if let Some(mut child) = entry_from_declaration(&inner_decl) {
                    child.namespace = namespace.clone();
                    children.push(child);
                }
            }
        }
    }
    children
}

/// Collapse runs of whitespace (including newlines from wrapped source
/// strings) into single spaces.
pub(crate) fn collapse_whitespace(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}
