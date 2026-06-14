//! `eu doc` — documentation and schema extraction from eucalypt source files.
//!
//! Runs the front-end pipeline (parse → desugar → cook → eliminate →
//! type-check) and extracts documentation, type annotations, deprecation
//! notices and visibility from backtick metadata.
//!
//! Outputs:
//! - Markdown documentation (`--format md`, default)
//! - JSON Schema (`--format json`)
//! - Coverage report (`--check`)

use std::collections::HashMap;
use std::fs;
use std::path::Path;

use rowan::ast::AstNode;
use serde_json;

use crate::driver::error::EucalyptError;
use crate::driver::options::{DocFormat, EucalyptOptions};
use crate::driver::resources::Resources;
use crate::syntax::rowan::ast::{self, AstToken, DeclarationKind, HasSoup, LiteralValue, Unit};
use crate::syntax::rowan::parse_unit;

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
fn extract_block_str(block: &ast::Block, key: &str) -> Option<String> {
    for inner_decl in block.declarations() {
        if let Some(head) = inner_decl.head() {
            if let DeclarationKind::Property(prop) = head.classify_declaration() {
                if prop.text() == key {
                    if let Some(body) = inner_decl.body() {
                        if let Some(body_soup) = body.soup() {
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
                }
            }
        }
    }
    None
}

/// Extract a symbol name for `key` from within a block (e.g. `export: :internal`).
fn extract_block_sym(block: &ast::Block, key: &str) -> Option<String> {
    for inner_decl in block.declarations() {
        if let Some(head) = inner_decl.head() {
            if let DeclarationKind::Property(prop) = head.classify_declaration() {
                if prop.text() == key {
                    if let Some(body) = inner_decl.body() {
                        if let Some(body_soup) = body.soup() {
                            for el in body_soup.elements() {
                                if let ast::Element::Lit(lit) = el {
                                    if let Some(LiteralValue::Sym(s)) = lit.value() {
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
    None
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
            for inner_decl in block.declarations() {
                if let Some(head) = inner_decl.head() {
                    if let DeclarationKind::Property(prop) = head.classify_declaration() {
                        if prop.text() == "see-also" {
                            if let Some(body) = inner_decl.body() {
                                if let Some(body_soup) = body.soup() {
                                    for el in body_soup.elements() {
                                        // see-also: [:a, :b] — value is a List
                                        if let ast::Element::List(list) = el {
                                            let mut refs = vec![];
                                            for item_soup in list.items() {
                                                for item_el in item_soup.elements() {
                                                    if let ast::Element::Lit(lit) = item_el {
                                                        if let Some(LiteralValue::Sym(s)) =
                                                            lit.value()
                                                        {
                                                            if let Some(n) = s.value() {
                                                                refs.push(n.to_string());
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            return refs;
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
    vec![]
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

// ── Markdown rendering ────────────────────────────────────────────────────────

/// Render a collection of doc entries as Markdown.
///
/// Entries are grouped by section heading and rendered with type annotations,
/// deprecation notices, examples, and see-also links.
pub fn render_markdown(entries: &[DocEntry], title: &str, unit_doc: Option<&str>) -> String {
    let mut out = String::new();

    out.push_str(&format!("# {title}\n\n"));

    if let Some(doc) = unit_doc {
        out.push_str(&format!("> {doc}\n\n"));
    }

    let mut current_section: Option<String> = None;

    for entry in entries {
        // Only render public entries
        if !entry.is_public() {
            continue;
        }

        // Section heading
        if entry.section != current_section {
            if let Some(ref sec) = entry.section {
                out.push_str(&format!("## {sec}\n\n"));
            }
            current_section = entry.section.clone();
        }

        // Namespace children are rendered inline under the namespace heading
        if !entry.children.is_empty() {
            out.push_str(&format!("## `{}`\n\n", entry.name));
            if let Some(ref doc) = entry.doc {
                out.push_str(&format!("{doc}\n\n"));
            }
            for child in &entry.children {
                if child.is_public() {
                    render_entry_markdown(child, &mut out, "###");
                }
            }
            continue;
        }

        render_entry_markdown(entry, &mut out, "###");
    }

    out
}

fn render_entry_markdown(entry: &DocEntry, out: &mut String, heading: &str) {
    // Function signature with params, or bare name
    let signature = match &entry.kind {
        DocKind::Function { params } if !params.is_empty() => {
            format!("`{}({})`", entry.name, params.join(", "))
        }
        _ => format!("`{}`", entry.name),
    };

    out.push_str(&format!("{heading} {signature}\n\n"));

    // Type annotation
    if let Some(ref ty) = entry.type_annotation {
        out.push_str("```\n");
        out.push_str(&format!("type: {ty}\n"));
        out.push_str("```\n\n");
    }

    // Doc string
    if let Some(ref doc) = entry.doc {
        out.push_str(&format!("{doc}\n\n"));
    }

    // Deprecation
    if let Some(ref dep) = entry.deprecated {
        match (&dep.message, &dep.replaced_by) {
            (Some(msg), Some(repl)) => {
                out.push_str(&format!("**Deprecated:** {msg} Use `{repl}` instead.\n\n"));
            }
            (Some(msg), None) => {
                out.push_str(&format!("**Deprecated:** {msg}\n\n"));
            }
            (None, Some(repl)) => {
                out.push_str(&format!("**Deprecated.** Use `{repl}` instead.\n\n"));
            }
            (None, None) => {
                out.push_str("**Deprecated.**\n\n");
            }
        }
    }

    // Example
    if let Some(ref ex) = entry.example {
        out.push_str("**Example:**\n\n");
        out.push_str("```eu\n");
        out.push_str(ex);
        out.push('\n');
        out.push_str("```\n\n");
    }

    // See-also
    if !entry.see_also.is_empty() {
        let refs: Vec<String> = entry.see_also.iter().map(|r| format!("`{r}`")).collect();
        out.push_str(&format!("**See also:** {}\n\n", refs.join(", ")));
    }
}

// ── JSON Schema rendering ─────────────────────────────────────────────────────

/// Render a collection of doc entries as a JSON Schema document.
///
/// Only data-typed bindings (properties whose types describe data shapes)
/// are included. Functions (`T → U`) are omitted.
pub fn render_json_schema(entries: &[DocEntry], title: &str) -> String {
    let mut properties: Vec<String> = Vec::new();

    for entry in entries {
        if !entry.is_public() {
            continue;
        }
        if let Some(ref ty_str) = entry.type_annotation {
            if let Some(schema) = type_str_to_json_schema(ty_str) {
                let name = entry.qualified_name();
                properties.push(format!("    {}: {}", json_str(&name), schema));
            }
        }
        // Recurse into namespace children
        for child in &entry.children {
            if !child.is_public() {
                continue;
            }
            if let Some(ref ty_str) = child.type_annotation {
                if let Some(schema) = type_str_to_json_schema(ty_str) {
                    let name = child.qualified_name();
                    properties.push(format!("    {}: {}", json_str(&name), schema));
                }
            }
        }
    }

    let mut out = String::from("{\n");
    out.push_str("  \"$schema\": \"https://json-schema.org/draft/2020-12/schema\",\n");
    out.push_str(&format!("  \"title\": {},\n", json_str(title)));

    if !properties.is_empty() {
        out.push_str("  \"properties\": {\n");
        out.push_str(&properties.join(",\n"));
        out.push_str("\n  }\n");
    } else {
        out.push_str("  \"type\": \"object\"\n");
    }

    out.push('}');
    out
}

/// Convert a eucalypt type annotation string to a JSON Schema fragment.
///
/// Returns `None` for function types (not representable in JSON Schema).
fn type_str_to_json_schema(ty_str: &str) -> Option<String> {
    let ty = ty_str.trim();
    // Skip function types
    if ty.contains('→') || ty.contains("->") {
        return None;
    }
    Some(match ty {
        "string" => r#"{"type": "string"}"#.to_string(),
        "number" => r#"{"type": "number"}"#.to_string(),
        "bool" | "boolean" => r#"{"type": "boolean"}"#.to_string(),
        "null" => r#"{"type": "null"}"#.to_string(),
        "symbol" => r#"{"type": "string"}"#.to_string(),
        "any" => r#"{}"#.to_string(),
        s if s.starts_with('[') && s.ends_with(']') => {
            // [T] → array
            let inner = &s[1..s.len() - 1];
            if let Some(items) = type_str_to_json_schema(inner) {
                format!(r#"{{"type": "array", "items": {items}}}"#)
            } else {
                r#"{"type": "array"}"#.to_string()
            }
        }
        s if s.starts_with('{') && s.ends_with('}') => {
            // {..} or {x: T, ..} → object
            r#"{"type": "object"}"#.to_string()
        }
        s if s.contains(" | ") => {
            // T | U → oneOf
            let variants: Vec<String> = s
                .split(" | ")
                .filter_map(|v| type_str_to_json_schema(v.trim()))
                .collect();
            if variants.is_empty() {
                return None;
            }
            format!(r#"{{"oneOf": [{}]}}"#, variants.join(", "))
        }
        _ => return None,
    })
}

/// JSON-encode a string value (handles all escape sequences correctly).
fn json_str(s: &str) -> String {
    serde_json::to_string(s).unwrap_or_else(|_| format!("\"{}\"", s.replace('"', "\\\"")))
}

// ── Coverage reporting ────────────────────────────────────────────────────────

/// Coverage report for a set of doc entries.
pub struct Coverage {
    pub documented: usize,
    pub undocumented: usize,
    pub total: usize,
    pub undocumented_names: Vec<String>,
}

/// Compute coverage: how many public bindings have doc strings.
pub fn compute_coverage(entries: &[DocEntry]) -> Coverage {
    let mut documented = 0;
    let mut undocumented = 0;
    let mut undocumented_names = Vec::new();

    let mut check = |entry: &DocEntry| {
        if !entry.is_public() {
            return;
        }
        if entry.doc.is_some() {
            documented += 1;
        } else {
            undocumented += 1;
            undocumented_names.push(entry.qualified_name());
        }
    };

    for entry in entries {
        check(entry);
        for child in &entry.children {
            check(child);
        }
    }

    Coverage {
        documented,
        undocumented,
        total: documented + undocumented,
        undocumented_names,
    }
}

// ── Multi-file prelude rendering ──────────────────────────────────────────────

/// A flat representation of a prelude entry, analogous to the Python script's
/// `PreludeEntry`, used for category-based multi-file output.
#[derive(Debug, Clone)]
struct FlatEntry {
    /// Short name (without namespace prefix).
    name: String,
    /// The display signature: `ns.name(args)` or `(l op r)`.
    signature: String,
    /// Doc string (already stripped of leading signature prefixes).
    description: String,
    /// Section heading from `##` comments in the source.
    section: String,
    /// Namespace (e.g. `"str"`, `"io"`, `"set"`), empty for top-level.
    namespace: String,
    /// Whether this is an operator definition.
    is_operator: bool,
    /// Whether the entry is suppressed from output.
    is_suppressed: bool,
}

/// Known prelude namespaces that open with `name: { ... }`.
const KNOWN_NAMESPACES: &[&str] = &[
    "eu",
    "io",
    "str",
    "cal",
    "set",
    "ch",
    "assertions",
    "_block",
    "pb",
    "random",
];

/// Map `##` section headings to category slugs.
fn section_to_category(section: &str) -> Option<&'static str> {
    match section {
        "Prelude versioning and run metadata" => Some("io"),
        "Random number generation" => Some("random"),
        "Error / debug support" => Some("booleans"),
        "Essentials" => Some("booleans"),
        "List basics" => Some("lists"),
        "Blocks / merge" => Some("blocks"),
        "Deep find — recursive key search" => Some("blocks"),
        "Deep query — pattern-based data querying" => Some("blocks"),
        "Boolean" => Some("booleans"),
        "Polymorphic equality" => Some("booleans"),
        "Arithmetic" => Some("numbers"),
        "Text and regexes" => Some("strings"),
        "Combinators" => Some("combinators"),
        "Sets" => Some("sets"),
        // Single-`#` sections (stored in `section` field after Python script compat)
        "Utilities" => Some("combinators"),
        "Metadata basics" => Some("metadata"),
        "List library functions, maps and folds" => Some("lists"),
        "Block library functions" => Some("blocks"),
        "By property alteration of blocks" => Some("blocks"),
        _ => None,
    }
}

/// Map namespace names to category slugs.
fn namespace_to_category(ns: &str) -> &'static str {
    match ns {
        "eu" => "io",
        "io" => "io",
        "str" => "strings",
        "ch" => "strings",
        "cal" => "calendar",
        "set" => "sets",
        "assertions" => "metadata",
        "random" => "random",
        _ => "io",
    }
}

/// Flatten `DocEntry` trees into `FlatEntry` values with namespace context.
///
/// Top-level entries with children (namespace blocks) are expanded; the
/// namespace entry itself is not included in output.
fn flatten_entries(entries: &[DocEntry]) -> Vec<FlatEntry> {
    let mut flat: Vec<FlatEntry> = Vec::new();

    for entry in entries {
        let ns = entry.namespace.as_deref().unwrap_or("").to_string();
        let is_ns_block =
            !entry.children.is_empty() && KNOWN_NAMESPACES.contains(&entry.name.as_str());

        if is_ns_block {
            // Expand namespace children
            for child in &entry.children {
                let child_ns = child
                    .namespace
                    .as_deref()
                    .unwrap_or(&entry.name)
                    .to_string();
                let fe = flat_entry_from_doc_entry(child, &child_ns);
                flat.push(fe);
            }
        } else {
            let fe = flat_entry_from_doc_entry(entry, &ns);
            flat.push(fe);
        }
    }

    flat
}

/// Build a `FlatEntry` from a `DocEntry` with resolved namespace.
fn flat_entry_from_doc_entry(entry: &DocEntry, namespace: &str) -> FlatEntry {
    let is_operator = matches!(&entry.kind, DocKind::Operator { .. });

    // Build the signature string
    let signature = if is_operator {
        match &entry.kind {
            DocKind::Operator { fixity } => match fixity.as_str() {
                "binary" => format!("({} {} {})", "l", entry.name, "r"),
                "prefix" => format!("({} {})", entry.name, "x"),
                "postfix" => format!("({} {})", "x", entry.name),
                _ => entry.name.clone(),
            },
            _ => entry.name.clone(),
        }
    } else {
        let base = match &entry.kind {
            DocKind::Function { params } if !params.is_empty() => {
                format!("{}({})", entry.name, params.join(", "))
            }
            _ => entry.name.clone(),
        };
        if !namespace.is_empty()
            && namespace != "assertions"
            && namespace != "_block"
            && !is_operator
        {
            format!("{namespace}.{base}")
        } else {
            base
        }
    };

    let description = format_description_for_table(entry.doc.as_deref().unwrap_or(""));

    let section = entry.section.as_deref().unwrap_or("").to_string();

    FlatEntry {
        name: entry.name.clone(),
        signature,
        description,
        section,
        namespace: namespace.to_string(),
        is_operator,
        is_suppressed: !matches!(entry.visibility, DocVisibility::Normal),
    }
}

/// Strip leading signature prefixes from a doc string, as the Python script does.
///
/// Converts the raw doc string into a table-cell description, removing
/// prefixes like `` `name(args)` — `` or `name(args) - `, capitalising
/// the first letter, and stripping trailing periods.
fn format_description_for_table(doc: &str) -> String {
    if doc.is_empty() {
        return String::new();
    }

    let mut s = doc.to_string();

    // Strip single leading backtick if unmatched
    if s.starts_with('`') && s.chars().filter(|&c| c == '`').count() % 2 == 1 {
        s = s[1..].trim_start().to_string();
    }

    // Strip backtick-quoted signature prefix: `name(args)` - description
    let backtick_prefix = regex_strip_backtick_prefix(&s);
    if !backtick_prefix.is_empty() {
        s = backtick_prefix;
    } else {
        // Strip plain signature prefix: name(args) - description or (l op r) - description
        let plain_prefix = regex_strip_plain_prefix(&s);
        if !plain_prefix.is_empty() {
            s = plain_prefix;
        }
    }

    // Strip trailing unmatched backtick
    if s.ends_with('`') && s.chars().filter(|&c| c == '`').count() % 2 == 1 {
        s = s[..s.len() - 1].trim_end().to_string();
    }

    // Capitalise first letter
    if let Some(first) = s.chars().next() {
        if first.is_lowercase() {
            let mut chars = s.chars();
            chars.next();
            s = first.to_uppercase().to_string() + chars.as_str();
        }
    }

    // Escape pipe characters for markdown tables
    s = s.replace('|', "\\|");

    // Strip trailing period
    s = s.trim_end_matches('.').to_string();

    s
}

/// Strip a backtick-quoted signature prefix, e.g. `` `name(args)` — text ``.
fn regex_strip_backtick_prefix(s: &str) -> String {
    // Pattern: `something` followed by [ -—–] and space
    if !s.starts_with('`') {
        return String::new();
    }
    let rest = &s[1..];
    if let Some(close) = rest.find('`') {
        let after = &rest[close + 1..].trim_start();
        if after.starts_with(['-', '\u{2014}', '\u{2013}']) {
            let after = &after[1..].trim_start();
            return after.to_string();
        }
    }
    String::new()
}

/// Strip a plain name/operator signature prefix, e.g. `name(args) - text`.
fn regex_strip_plain_prefix(s: &str) -> String {
    // Try operator prefix: (l op r) - text
    if s.starts_with('(') {
        if let Some(close) = s.find(')') {
            let after = s[close + 1..].trim_start();
            if after.starts_with(['-', '\u{2014}', '\u{2013}']) {
                let after = after[1..].trim_start();
                return after.to_string();
            }
        }
        return String::new();
    }

    // Try name prefix: name or name(args) followed by - or — or –
    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;

    // Consume optional leading quote
    if i < chars.len() && chars[i] == '\'' {
        i += 1;
    }

    // Consume name characters (letters, digits, ?, !, -, ., ,)
    while i < chars.len()
        && (chars[i].is_alphanumeric()
            || matches!(
                chars[i],
                '?' | '!' | '-' | '_' | '.' | ',' | '∅' | '∸' | '¬' | '↑'
            ))
    {
        i += 1;
    }

    if i == 0 {
        return String::new();
    }

    // Consume optional args: (...)
    if i < chars.len() && chars[i] == '(' {
        while i < chars.len() && chars[i] != ')' {
            i += 1;
        }
        if i < chars.len() {
            i += 1; // consume ')'
        }
    }

    // Consume optional trailing backtick
    if i < chars.len() && chars[i] == '`' {
        i += 1;
    }

    // Skip whitespace
    while i < chars.len() && chars[i] == ' ' {
        i += 1;
    }

    // Require a dash separator
    if i < chars.len() && matches!(chars[i], '-' | '\u{2014}' | '\u{2013}') {
        i += 1;
        // Skip whitespace after dash
        while i < chars.len() && chars[i] == ' ' {
            i += 1;
        }
        return chars[i..].iter().collect();
    }

    String::new()
}

/// Determine the category slug for a flat entry.
fn entry_category(entry: &FlatEntry) -> &'static str {
    if !entry.namespace.is_empty() {
        namespace_to_category(&entry.namespace)
    } else if let Some(cat) = section_to_category(&entry.section) {
        cat
    } else {
        "io" // fallback
    }
}

/// Group flat entries by category slug.
fn group_by_category(entries: &[FlatEntry]) -> HashMap<&'static str, Vec<&FlatEntry>> {
    let mut map: HashMap<&'static str, Vec<&FlatEntry>> = HashMap::new();
    for entry in entries {
        let cat = entry_category(entry);
        map.entry(cat).or_default().push(entry);
    }
    map
}

/// Load a supplement file, returning its trimmed content or an empty string.
fn load_supplement(category: &str, slug: &str, supplements_dir: &Path) -> String {
    let path = supplements_dir.join(category).join(format!("{slug}.md"));
    if path.exists() {
        fs::read_to_string(&path)
            .unwrap_or_default()
            .trim()
            .to_string()
    } else {
        String::new()
    }
}

/// Convert a section display name to a slug for supplement lookups.
fn section_slug(display: &str) -> String {
    let mut slug = String::new();
    for c in display.chars() {
        if c.is_ascii_alphanumeric() {
            slug.push(c.to_ascii_lowercase());
        } else {
            if !slug.ends_with('-') {
                slug.push('-');
            }
        }
    }
    slug.trim_matches('-').to_string()
}

/// Generate a markdown table from a slice of flat entries.
fn generate_table(entries: &[&FlatEntry]) -> String {
    if entries.is_empty() {
        return String::new();
    }
    let mut lines = vec![
        "| Function | Description |".to_string(),
        "|----------|-------------|".to_string(),
    ];
    for entry in entries {
        let sig = entry.signature.replace('|', "\\|");
        let desc = &entry.description;
        lines.push(format!("| `{sig}` | {desc} |"));
    }
    lines.join("\n")
}

/// Configuration for sections within a category page.
enum SectionSource<'a> {
    /// Entries whose `section` field matches one of these source headings.
    BySection(&'a [&'a str]),
    /// Entries whose name appears in this set.
    ByName(&'a [&'a str]),
    /// Entries matching a custom predicate (stored as a function).
    Filter(fn(&FlatEntry) -> bool),
    /// All entries in the category.
    All,
}

struct SectionConfig<'a> {
    display: &'a str,
    source: SectionSource<'a>,
}

/// Select entries for a section from the category's entry pool.
fn entries_for_section<'e>(
    all: &[&'e FlatEntry],
    source: &SectionSource<'_>,
) -> Vec<&'e FlatEntry> {
    match source {
        SectionSource::BySection(sections) => all
            .iter()
            .copied()
            .filter(|e| sections.contains(&e.section.as_str()))
            .collect(),
        SectionSource::ByName(names) => all
            .iter()
            .copied()
            .filter(|e| names.contains(&e.name.as_str()))
            .collect(),
        SectionSource::Filter(f) => all.iter().copied().filter(|e| f(e)).collect(),
        SectionSource::All => all.to_vec(),
    }
}

/// Generate a complete markdown page for one category.
fn generate_category_page(
    category: &str,
    all_entries: &[&FlatEntry],
    supplements_dir: &Path,
) -> String {
    // Category page config: title and sections
    let (title, sections): (&str, Vec<SectionConfig>) = match category {
        "lists" => (
            "Lists",
            vec![
                SectionConfig {
                    display: "Basic Operations",
                    source: SectionSource::BySection(&["List basics"]),
                },
                SectionConfig {
                    display: "List Construction",
                    source: SectionSource::ByName(&[
                        "repeat",
                        "ints-from",
                        "range",
                        "cycle",
                        "iterate",
                        "nil",
                    ]),
                },
                SectionConfig {
                    display: "Transformations",
                    source: SectionSource::ByName(&[
                        "map",
                        "<$>",
                        "map2",
                        "filter",
                        "remove",
                        "reverse",
                        "take",
                        "drop",
                        "take-while",
                        "take-until",
                        "drop-while",
                        "drop-until",
                        "cross",
                    ]),
                },
                SectionConfig {
                    display: "Combining Lists",
                    source: SectionSource::ByName(&[
                        "append",
                        "++",
                        "prepend",
                        "concat",
                        "mapcat",
                        "zip",
                        "zip-with",
                        "zip-apply",
                    ]),
                },
                SectionConfig {
                    display: "Splitting Lists",
                    source: SectionSource::ByName(&[
                        "split-at",
                        "split-after",
                        "split-when",
                        "window",
                        "partition",
                        "discriminate",
                    ]),
                },
                SectionConfig {
                    display: "Folds and Scans",
                    source: SectionSource::ByName(&["foldl", "foldr", "scanl", "scanr"]),
                },
                SectionConfig {
                    display: "Predicates",
                    source: SectionSource::ByName(&["all", "all-true?", "any", "any-true?"]),
                },
                SectionConfig {
                    display: "Sorting",
                    source: SectionSource::ByName(&[
                        "qsort",
                        "sort-nums",
                        "sort-strs",
                        "sort-zdts",
                        "sort-by",
                        "sort-by-num",
                        "sort-by-str",
                        "sort-by-zdt",
                        "group-by",
                    ]),
                },
                SectionConfig {
                    display: "Other",
                    source: SectionSource::ByName(&[
                        "over-sliding-pairs",
                        "differences",
                        "count",
                        "last",
                        "nth",
                        "!!",
                    ]),
                },
            ],
        ),
        "blocks" => (
            "Blocks",
            vec![
                SectionConfig {
                    display: "Block Construction and Merging",
                    source: SectionSource::BySection(&["Blocks / merge"]),
                },
                SectionConfig {
                    display: "Block Utilities",
                    source: SectionSource::BySection(&["Block library functions"]),
                },
                SectionConfig {
                    display: "Block Alteration",
                    source: SectionSource::BySection(&["By property alteration of blocks"]),
                },
                SectionConfig {
                    display: "Deep Find and Query",
                    source: SectionSource::BySection(&[
                        "Deep find — recursive key search",
                        "Deep query — pattern-based data querying",
                    ]),
                },
            ],
        ),
        "strings" => (
            "Strings",
            vec![
                SectionConfig {
                    display: "String Processing",
                    source: SectionSource::Filter(|e| e.namespace == "str"),
                },
                SectionConfig {
                    display: "Character Constants",
                    source: SectionSource::Filter(|e| e.namespace == "ch"),
                },
            ],
        ),
        "numbers" => (
            "Numbers and Arithmetic",
            vec![
                SectionConfig {
                    display: "Arithmetic Operators",
                    source: SectionSource::Filter(|e| e.is_operator),
                },
                SectionConfig {
                    display: "Numeric Functions",
                    source: SectionSource::Filter(|e| !e.is_operator),
                },
            ],
        ),
        "booleans" => (
            "Booleans and Comparison",
            vec![
                SectionConfig {
                    display: "Essentials",
                    source: SectionSource::BySection(&["Essentials"]),
                },
                SectionConfig {
                    display: "Error and Debug Support",
                    source: SectionSource::BySection(&["Error / debug support"]),
                },
                SectionConfig {
                    display: "Boolean Logic",
                    source: SectionSource::BySection(&["Boolean"]),
                },
                SectionConfig {
                    display: "Equality and Comparison",
                    source: SectionSource::BySection(&["Polymorphic equality"]),
                },
            ],
        ),
        "combinators" => (
            "Combinators",
            vec![
                SectionConfig {
                    display: "Combinators",
                    source: SectionSource::BySection(&["Combinators"]),
                },
                SectionConfig {
                    display: "Utilities",
                    source: SectionSource::BySection(&["Utilities"]),
                },
            ],
        ),
        "calendar" => (
            "Calendar",
            vec![SectionConfig {
                display: "Date and Time Functions",
                source: SectionSource::All,
            }],
        ),
        "sets" => (
            "Sets",
            vec![SectionConfig {
                display: "Set Operations",
                source: SectionSource::All,
            }],
        ),
        "random" => (
            "Random Numbers",
            vec![SectionConfig {
                display: "Random Number Generation",
                source: SectionSource::All,
            }],
        ),
        "metadata" => (
            "Metadata",
            vec![
                SectionConfig {
                    display: "Metadata Basics",
                    source: SectionSource::BySection(&["Metadata basics"]),
                },
                SectionConfig {
                    display: "Assertions",
                    source: SectionSource::Filter(|e| {
                        e.namespace == "assertions" || e.name.starts_with("//")
                    }),
                },
            ],
        ),
        "io" => (
            "IO",
            vec![
                SectionConfig {
                    display: "Prelude Versioning",
                    source: SectionSource::Filter(|e| e.namespace == "eu"),
                },
                SectionConfig {
                    display: "IO Functions",
                    source: SectionSource::Filter(|e| e.namespace == "io"),
                },
            ],
        ),
        _ => return String::new(),
    };

    let mut parts: Vec<String> = vec![format!("# {title}")];

    // Top-level supplement (preamble)
    let top_supp = load_supplement(category, "top", supplements_dir);
    if !top_supp.is_empty() {
        parts.push(String::new());
        parts.push(top_supp);
    }

    // Track which entries have been used (by pointer identity via index)
    let mut used: std::collections::HashSet<usize> = std::collections::HashSet::new();

    for section_cfg in &sections {
        let mut candidates = entries_for_section(all_entries, &section_cfg.source);

        // Filter to public entries only
        candidates.retain(|e| !e.is_suppressed);

        // Deduplicate and track usage
        let mut seen_names: std::collections::HashSet<&str> = std::collections::HashSet::new();
        let mut unique: Vec<&FlatEntry> = Vec::new();
        for e in candidates {
            let ptr = e as *const FlatEntry as usize;
            if !used.contains(&ptr) && seen_names.insert(&e.name) {
                used.insert(ptr);
                unique.push(e);
            }
        }

        let slug = section_slug(section_cfg.display);
        let supp = load_supplement(category, &slug, supplements_dir);

        if unique.is_empty() && supp.is_empty() {
            continue;
        }

        parts.push(String::new());
        parts.push(format!("## {}", section_cfg.display));
        parts.push(String::new());

        if !unique.is_empty() {
            parts.push(generate_table(&unique));
        }

        if !supp.is_empty() {
            if !unique.is_empty() {
                parts.push(String::new());
            }
            parts.push(supp);
        }
    }

    // Default supplement (bottom of page)
    let default_supp = load_supplement(category, "default", supplements_dir);
    if !default_supp.is_empty() {
        parts.push(String::new());
        parts.push(default_supp);
    }

    // Remaining entries not placed in any section
    let remaining: Vec<&FlatEntry> = all_entries
        .iter()
        .copied()
        .filter(|e| !e.is_suppressed && !used.contains(&(*e as *const FlatEntry as usize)))
        .collect();

    if !remaining.is_empty() {
        parts.push(String::new());
        parts.push("## Other".to_string());
        parts.push(String::new());
        parts.push(generate_table(&remaining));
    }

    parts.push(String::new());
    parts.join("\n")
}

/// Generate the prelude index page.
fn generate_index_page(by_category: &HashMap<&'static str, Vec<&FlatEntry>>) -> String {
    let category_info: &[(&str, &str, &str)] = &[
        (
            "lists",
            "Lists",
            "list construction, transformation, folding, sorting",
        ),
        (
            "blocks",
            "Blocks",
            "block construction, access, merging, transformation",
        ),
        (
            "strings",
            "Strings",
            "string manipulation, regex, formatting",
        ),
        (
            "numbers",
            "Numbers and Arithmetic",
            "numeric operations and predicates",
        ),
        (
            "booleans",
            "Booleans and Comparison",
            "boolean logic and comparison operators",
        ),
        (
            "combinators",
            "Combinators",
            "function composition, application, utilities",
        ),
        ("calendar", "Calendar", "date and time functions"),
        ("sets", "Sets", "set operations"),
        (
            "random",
            "Random Numbers",
            "random number generation, monadic random: namespace",
        ),
        ("metadata", "Metadata", "metadata and assertion functions"),
        (
            "io",
            "IO",
            "environment, time, argument access, and monad utility",
        ),
    ];

    let mut parts = vec![
        "# Prelude Reference".to_string(),
        String::new(),
        "The eucalypt **prelude** is a standard library of functions, operators,".to_string(),
        "and constants that is automatically loaded before your code runs.".to_string(),
        String::new(),
        "You can suppress the prelude with `-Q` if needed, though this leaves".to_string(),
        "a very bare environment (even `true`, `false`, and `if` are defined".to_string(),
        "in the prelude).".to_string(),
        String::new(),
        "## Categories".to_string(),
        String::new(),
    ];

    let mut total = 0usize;
    for (cat_key, cat_title, cat_desc) in category_info {
        let count = by_category
            .get(cat_key)
            .map(|es| es.iter().filter(|e| !e.is_suppressed).count())
            .unwrap_or(0);
        total += count;
        parts.push(format!(
            "- [{cat_title}]({cat_key}.md) -- {cat_desc} ({count} entries)"
        ));
    }

    parts.push(String::new());
    parts.push(format!("*{total} documented entries in total.*"));
    parts.push(String::new());

    parts.join("\n")
}

/// Generate multi-file categorised prelude reference in `output_dir`.
///
/// Supplement files are loaded from `<output_dir>/../supplements/` (i.e. the
/// `supplements/` sibling of the output directory) if it exists, otherwise no
/// supplements are merged.
pub fn render_prelude_multifile(
    entries: &[DocEntry],
    output_dir: &Path,
    supplements_dir: &Path,
) -> Result<i32, EucalyptError> {
    fs::create_dir_all(output_dir).map_err(|e| {
        EucalyptError::FileCouldNotBeRead(output_dir.display().to_string(), Some(e.to_string()))
    })?;

    let flat = flatten_entries(entries);
    let by_category = group_by_category(&flat);

    // All known categories in display order
    const CATEGORIES: &[&str] = &[
        "lists",
        "blocks",
        "strings",
        "numbers",
        "booleans",
        "combinators",
        "calendar",
        "sets",
        "random",
        "metadata",
        "io",
    ];

    for &cat in CATEGORIES {
        let cat_entries: Vec<&FlatEntry> = by_category
            .get(cat)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
            .to_vec();
        let public_count = cat_entries.iter().filter(|e| !e.is_suppressed).count();
        let page = generate_category_page(cat, &cat_entries, supplements_dir);
        let out_path = output_dir.join(format!("{cat}.md"));
        fs::write(&out_path, &page).map_err(|e| {
            EucalyptError::FileCouldNotBeRead(out_path.display().to_string(), Some(e.to_string()))
        })?;
        eprintln!(
            "  Generated {} ({public_count} public entries)",
            out_path.file_name().and_then(|n| n.to_str()).unwrap_or(cat)
        );
    }

    // Generate index
    let index_page = generate_index_page(&by_category);
    let index_path = output_dir.join("index.md");
    fs::write(&index_path, &index_page).map_err(|e| {
        EucalyptError::FileCouldNotBeRead(index_path.display().to_string(), Some(e.to_string()))
    })?;
    eprintln!("  Generated index.md");
    eprintln!("\nDone. Generated pages in {}", output_dir.display());

    Ok(0)
}

// ── Entry point ───────────────────────────────────────────────────────────────

/// Run `eu doc` — extract and render documentation from eucalypt source.
pub fn doc(opt: &EucalyptOptions) -> Result<i32, EucalyptError> {
    if opt.doc_prelude() {
        return doc_prelude(opt);
    }

    let inputs = opt.explicit_inputs();
    if inputs.is_empty() {
        eprintln!("eu doc: no files specified (use --prelude for the prelude reference)");
        return Ok(1);
    }

    let mut exit = 0;
    for input in inputs {
        use crate::syntax::input::Locator;
        let path_str = match input.locator() {
            Locator::Fs(p) => p.to_string_lossy().into_owned(),
            other => {
                eprintln!("eu doc: unsupported input locator {other:?}");
                exit = 1;
                continue;
            }
        };
        let e = doc_file(Path::new(&path_str), opt)?;
        if e != 0 {
            exit = e;
        }
    }
    Ok(exit)
}

/// Process a single file.
fn doc_file(path: &Path, opt: &EucalyptOptions) -> Result<i32, EucalyptError> {
    let source = fs::read_to_string(path).map_err(|e| {
        EucalyptError::FileCouldNotBeRead(path.display().to_string(), Some(e.to_string()))
    })?;

    let (entries, unit_doc) = extract_from_source(&source);

    let title = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("Documentation");

    render_and_output(&entries, title, unit_doc.as_deref(), opt)
}

/// Process the embedded prelude.
fn doc_prelude(opt: &EucalyptOptions) -> Result<i32, EucalyptError> {
    let source = Resources::default()
        .get("prelude")
        .cloned()
        .ok_or_else(|| EucalyptError::UnknownResource("prelude".to_string()))?;

    let (entries, unit_doc) = extract_from_source(&source);

    // Multi-file output when --output-dir is given
    if let Some(output_dir) = opt.doc_output_dir() {
        // Locate supplements directory: <output-dir>/supplements/ if it exists,
        // otherwise a sibling supplements/ directory relative to output-dir.
        let supplements_dir = {
            let sibling = output_dir.join("supplements");
            if sibling.exists() {
                sibling
            } else if let Some(parent) = output_dir.parent() {
                parent.join("supplements")
            } else {
                output_dir.join("supplements")
            }
        };
        return render_prelude_multifile(&entries, output_dir, &supplements_dir);
    }

    render_and_output(&entries, "Prelude Reference", unit_doc.as_deref(), opt)
}

/// Parse source and extract doc entries plus the unit-level doc string.
fn extract_from_source(source: &str) -> (Vec<DocEntry>, Option<String>) {
    let parse_result = parse_unit(source);
    let unit = parse_result.tree();
    let entries = extract_doc_entries(&unit);
    let unit_doc = extract_unit_doc(&unit);
    (entries, unit_doc)
}

/// Extract the unit-level documentation from the unit metadata block.
fn extract_unit_doc(unit: &Unit) -> Option<String> {
    // The first expression in the unit (if it's a block) is unit metadata.
    for decl in unit.declarations() {
        // Look for a bare block expression at the top (unit metadata).
        // In Eucalypt, unit metadata is the first block-expression before declarations.
        // It appears as a declaration with no head (just a body block).
        if decl.head().is_none() {
            if let Some(body) = decl.body() {
                if let Some(soup) = body.soup() {
                    for el in soup.elements() {
                        if let ast::Element::Block(block) = el {
                            if let Some(doc_str) = extract_block_str(&block, "doc") {
                                return Some(doc_str);
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// Render and output documentation in the requested format.
fn render_and_output(
    entries: &[DocEntry],
    title: &str,
    unit_doc: Option<&str>,
    opt: &EucalyptOptions,
) -> Result<i32, EucalyptError> {
    if opt.doc_coverage_check() {
        return render_coverage(entries, title);
    }

    let output = match opt.doc_format() {
        DocFormat::Markdown => render_markdown(entries, title, unit_doc),
        DocFormat::Json => render_json_schema(entries, title),
    };

    println!("{output}");
    Ok(0)
}

/// Print a coverage report and return exit code 0 (always, it's informational).
fn render_coverage(entries: &[DocEntry], title: &str) -> Result<i32, EucalyptError> {
    let cov = compute_coverage(entries);
    let pct = if cov.total == 0 {
        100.0
    } else {
        cov.documented as f64 / cov.total as f64 * 100.0
    };

    println!("# Documentation coverage: {title}");
    println!();
    println!(
        "{}/{} public bindings documented ({:.0}%)",
        cov.documented, cov.total, pct
    );

    if !cov.undocumented_names.is_empty() {
        println!();
        println!("Undocumented:");
        for name in &cov.undocumented_names {
            println!("  - {name}");
        }
    }

    Ok(0)
}
