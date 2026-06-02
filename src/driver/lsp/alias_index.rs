//! Type-alias reference tooling for the eucalypt LSP (§A7).
//!
//! Implements go-to-definition, hover, and rename for type-alias names
//! written inside `type:` annotation strings.  The approach is:
//!
//! 1. **`AliasIndex`** — a per-document map from alias name to every
//!    LSP range where it is *defined* (from `type-def:` metadata and
//!    `types:` blocks).
//!
//! 2. **Cursor detection** — given a cursor position, walk the CST to
//!    find if the cursor is inside a plain `type:` string literal.  If
//!    so, parse it with [`parse_type_with_refs`] to obtain the alias
//!    reference at that byte offset.
//!
//! **Limitation (documented per spec §A7.2)**: this module handles only
//! *plain, unescaped* type strings.  A `type:` value built from a string
//! pattern (e.g. `"{{Person}} | {{Json}}"`) is ignored — tooling
//! degrades gracefully to no alias navigation, with no error.

use std::collections::HashMap;

use lsp_types::{Position, Range, TextEdit, Url, WorkspaceEdit};

use crate::core::typecheck::parse::{parse_type_with_refs, AliasRef};
use crate::syntax::rowan::ast::{
    AstToken, Declaration, DeclarationKind, HasSoup, NormalIdentifier, Unit,
};
use crate::syntax::rowan::kind::{SyntaxKind, SyntaxNode};
use rowan::ast::AstNode;

use super::diagnostics::text_range_to_lsp_range;
use super::selection::position_to_offset;

// ── AliasIndex ───────────────────────────────────────────────────────────────

/// Per-document index mapping alias names to their definition sites.
///
/// An alias may be defined in multiple places (e.g. the same name in a
/// `types:` block and via `type-def:`) — all sites are recorded so that
/// rename can update every one.
#[derive(Debug, Default)]
pub struct AliasIndex {
    /// alias name → all definition-site ranges in this document.
    definitions: HashMap<String, Vec<Range>>,
}

impl AliasIndex {
    /// Return the definition ranges for an alias, if any.
    pub fn definitions(&self, alias: &str) -> &[Range] {
        self.definitions
            .get(alias)
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }

    /// Return the first (primary) definition range for an alias.
    pub fn primary_definition(&self, alias: &str) -> Option<Range> {
        self.definitions(alias).first().copied()
    }

    fn add(&mut self, name: String, range: Range) {
        self.definitions.entry(name).or_default().push(range);
    }
}

// ── Build the alias index ────────────────────────────────────────────────────

/// Walk the parsed unit and build an alias definition index.
///
/// Collects:
/// - `type-def: "Name"` on any declaration → the declaration's name
///   token range is the definition site of `Name`.
/// - `types: { Name: "...", ... }` inside any metadata block → each
///   `Name` key's token range is the definition site of `Name`.
pub fn build_alias_index(source: &str, root: &SyntaxNode, uri: &Url) -> AliasIndex {
    let _ = uri; // reserved for multi-file support
    let mut index = AliasIndex::default();

    // Walk all top-level and nested declarations via the Unit node.
    if let Some(unit) = Unit::cast(root.clone()) {
        for decl in unit.declarations() {
            collect_from_declaration(source, &decl, &mut index);
        }
    }

    index
}

/// Recursively collect alias definitions from a declaration and its children.
fn collect_from_declaration(source: &str, decl: &Declaration, index: &mut AliasIndex) {
    // ── type-def: "Name" ────────────────────────────────────────────────
    //
    // When a binding carries `type-def: "Name"`, the definition site is
    // the binding's own name token (or the declaration's full range if
    // the name is unavailable).
    if let Some(name) = extract_meta_string_value(decl, "type-def") {
        let def_range = decl
            .head()
            .and_then(|h| {
                let kind = h.classify_declaration();
                match kind {
                    DeclarationKind::Property(id) => {
                        Some(text_range_to_lsp_range(source, normal_id_range(&id)))
                    }
                    DeclarationKind::Function(id, _) => {
                        Some(text_range_to_lsp_range(source, normal_id_range(&id)))
                    }
                    _ => None,
                }
            })
            .unwrap_or_else(|| text_range_to_lsp_range(source, decl.syntax().text_range()));
        index.add(name, def_range);
    }

    // ── types: { Name: "...", ... } ─────────────────────────────────────
    //
    // Walk the metadata soup for a `types:` block.  Each key declared
    // inside the block is a type-alias name; its definition site is the
    // range of the key token.
    if let Some(meta) = decl.meta() {
        if let Some(soup) = meta.soup() {
            for element in soup.elements() {
                if let crate::syntax::rowan::ast::Element::Block(block) = element {
                    for inner_decl in block.declarations() {
                        if let Some(head) = inner_decl.head() {
                            if let DeclarationKind::Property(prop) = head.classify_declaration() {
                                if prop.text() == "types" {
                                    // Found `types: { ... }` — walk its body.
                                    if let Some(body) = inner_decl.body() {
                                        if let Some(body_soup) = body.soup() {
                                            for body_el in body_soup.elements() {
                                                if let crate::syntax::rowan::ast::Element::Block(
                                                    types_block,
                                                ) = body_el
                                                {
                                                    for alias_decl in types_block.declarations() {
                                                        collect_from_types_block_decl(
                                                            source,
                                                            &alias_decl,
                                                            index,
                                                        );
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
            }
        }
    }

    // Recurse into block bodies (namespace blocks).
    if let Some(body) = decl.body() {
        if let Some(soup) = body.soup() {
            for element in soup.elements() {
                if let crate::syntax::rowan::ast::Element::Block(block) = element {
                    for child_decl in block.declarations() {
                        collect_from_declaration(source, &child_decl, index);
                    }
                }
            }
        }
    }
}

/// Collect an alias definition from a declaration inside a `types:` block.
///
/// `decl` has the form `Name: "type string"`.  The definition site is the
/// range of the `Name` identifier token.
fn collect_from_types_block_decl(source: &str, decl: &Declaration, index: &mut AliasIndex) {
    let Some(head) = decl.head() else { return };
    let DeclarationKind::Property(id) = head.classify_declaration() else {
        return;
    };
    let alias_name = normal_id_name(&id);
    let def_range = text_range_to_lsp_range(source, normal_id_range(&id));
    index.add(alias_name, def_range);
}

/// Get the text content of a `NormalIdentifier`.
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

/// Get the text range of a `NormalIdentifier` token.
fn normal_id_range(id: &NormalIdentifier) -> rowan::TextRange {
    match id {
        NormalIdentifier::UnquotedIdentifier(u) => u.syntax().text_range(),
        NormalIdentifier::SingleQuoteIdentifier(s) => s.syntax().text_range(),
    }
}

// ── Cursor-in-type-string detection ──────────────────────────────────────────

/// Describes the alias reference the cursor falls on inside a `type:` string.
#[derive(Debug)]
pub struct TypeStringAlias {
    /// The alias name (e.g. `"Person"`).
    pub alias_name: String,
    /// The LSP range of the alias name token inside the `type:` string.
    pub reference_range: Range,
}

/// Find the type-alias reference (if any) at the given cursor position.
///
/// Returns `Some(TypeStringAlias)` if:
/// 1. The cursor is inside a plain string literal that is the value of a
///    `type:` key in metadata.
/// 2. That string parses successfully with [`parse_type_with_refs`].
/// 3. One of the resulting [`AliasRef`]s spans the cursor's byte offset
///    within the string content.
///
/// Returns `None` for escaped/interpolated strings (graceful degradation,
/// §A7.2) and for any cursor not inside a type: annotation.
pub fn alias_at_position(
    source: &str,
    root: &SyntaxNode,
    position: &Position,
) -> Option<TypeStringAlias> {
    let cursor_offset = usize::from(position_to_offset(source, position));

    // Walk every STRING token in the tree.
    for tok in root.descendants_with_tokens() {
        let Some(tok) = tok.as_token() else { continue };
        if tok.kind() != SyntaxKind::STRING {
            continue;
        }

        let tok_range = tok.text_range();
        let tok_start = usize::from(tok_range.start());
        let tok_end = usize::from(tok_range.end());

        // Is the cursor inside this string token?
        if cursor_offset < tok_start || cursor_offset >= tok_end {
            continue;
        }

        // Is this string the value of a `type:` key in metadata?
        if !is_type_annotation_string(tok.parent()) {
            continue;
        }

        // Compute offset within string content (skip the opening `"`).
        let content_start = tok_start + 1;
        if cursor_offset < content_start {
            continue;
        }
        let content_offset = cursor_offset - content_start;

        // Extract the string content.  We only handle plain strings;
        // escape sequences break the linear byte mapping (§A7.2).
        let raw_text = tok.text();
        let content = raw_text
            .strip_prefix('"')
            .and_then(|s| s.strip_suffix('"'))
            .unwrap_or(raw_text);

        // Degrade gracefully if the content contains escape sequences.
        if content.contains('\\') {
            return None;
        }

        // Parse and find the alias ref whose span contains the cursor.
        let (_, alias_refs) = parse_type_with_refs(content).ok()?;
        let alias_ref = find_alias_at_offset(&alias_refs, content_offset)?;

        // Convert the alias ref's byte span back to an LSP range within
        // the source document.
        let ref_start = content_start + alias_ref.span.0;
        let ref_end = content_start + alias_ref.span.1;
        let reference_range = byte_range_to_lsp_range(source, ref_start, ref_end);

        return Some(TypeStringAlias {
            alias_name: alias_ref.name.clone(),
            reference_range,
        });
    }

    None
}

/// Return `true` if `node` (the parent of a STRING token) is the body of a
/// `type:` property declaration inside a metadata block.
///
/// Walks the parent chain looking for the pattern:
///   DECLARATION_BODY → (property named "type") → BLOCK → DECLARATION_METADATA
fn is_type_annotation_string(
    node: Option<rowan::SyntaxNode<crate::syntax::rowan::kind::EucalyptLanguage>>,
) -> bool {
    let node = match node {
        Some(n) => n,
        None => return false,
    };

    // The STRING's immediate parent should be SOUP or similar expression node.
    // Walk upward to find a DECLARATION_BODY.
    let mut cur = Some(node);
    while let Some(n) = cur {
        if n.kind() == SyntaxKind::DECL_BODY {
            // Check the declaration this body belongs to has name "type".
            if let Some(decl_node) = n.parent() {
                if decl_node.kind() == SyntaxKind::DECLARATION {
                    if let Some(decl) = Declaration::cast(decl_node.clone()) {
                        if let Some(head) = decl.head() {
                            if let DeclarationKind::Property(prop) = head.classify_declaration() {
                                if prop.text() == "type" {
                                    // Check that this declaration is inside a metadata block.
                                    return is_inside_metadata(&decl_node);
                                }
                            }
                        }
                    }
                }
            }
            return false;
        }
        cur = n.parent();
    }
    false
}

/// Check whether `node` is inside a `DECLARATION_METADATA` subtree.
fn is_inside_metadata(
    node: &rowan::SyntaxNode<crate::syntax::rowan::kind::EucalyptLanguage>,
) -> bool {
    let mut cur = node.parent();
    while let Some(n) = cur {
        if n.kind() == SyntaxKind::DECL_META {
            return true;
        }
        cur = n.parent();
    }
    false
}

/// Find the [`AliasRef`] whose span contains `offset`, if any.
fn find_alias_at_offset(refs: &[AliasRef], offset: usize) -> Option<&AliasRef> {
    refs.iter()
        .find(|r| offset >= r.span.0 && offset < r.span.1)
}

/// Convert a byte range in `source` to an LSP `Range`.
///
/// Walks the source line by line to find line/column for start and end.
fn byte_range_to_lsp_range(source: &str, start: usize, end: usize) -> Range {
    use lsp_types::Position as LspPos;

    let pos_for = |byte: usize| -> LspPos {
        let mut line: u32 = 0;
        let mut col: u32 = 0;
        for (i, ch) in source.char_indices() {
            if i >= byte {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 0;
            } else {
                col += ch.len_utf16() as u32;
            }
        }
        LspPos {
            line,
            character: col,
        }
    };

    Range {
        start: pos_for(start),
        end: pos_for(end),
    }
}

/// Extract the text content of a plain string metadata field by key.
fn extract_meta_string_value(decl: &Declaration, key: &str) -> Option<String> {
    let meta = decl.meta()?;
    let soup = meta.soup()?;

    for element in soup.elements() {
        if let crate::syntax::rowan::ast::Element::Block(block) = element {
            for inner_decl in block.declarations() {
                if let Some(head) = inner_decl.head() {
                    if let DeclarationKind::Property(prop) = head.classify_declaration() {
                        if prop.text() == key {
                            if let Some(body) = inner_decl.body() {
                                if let Some(body_soup) = body.soup() {
                                    for el in body_soup.elements() {
                                        if let crate::syntax::rowan::ast::Element::Lit(lit) = el {
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
    }
    None
}

// ── Go-to-definition ─────────────────────────────────────────────────────────

/// Go-to-definition for an alias reference inside a `type:` string.
///
/// Returns the LSP location of the alias definition if the cursor is on
/// an alias reference and the alias is indexed.  Returns `None` otherwise
/// (caller falls through to ordinary identifier go-to-def).
pub fn goto_definition_for_alias(
    source: &str,
    root: &SyntaxNode,
    position: &Position,
    alias_index: &AliasIndex,
    uri: &Url,
) -> Option<lsp_types::GotoDefinitionResponse> {
    let at = alias_at_position(source, root, position)?;
    let def_range = alias_index.primary_definition(&at.alias_name)?;
    Some(lsp_types::GotoDefinitionResponse::Scalar(
        lsp_types::Location {
            uri: uri.clone(),
            range: def_range,
        },
    ))
}

// ── Hover ─────────────────────────────────────────────────────────────────────

/// Hover for an alias reference inside a `type:` string.
///
/// Shows the alias name and its resolved type (from the checker's alias
/// map, if available).  Returns `None` when the cursor is not on an alias
/// reference inside a type string.
pub fn hover_for_alias(
    source: &str,
    root: &SyntaxNode,
    position: &Position,
    alias_index: &AliasIndex,
    alias_types: Option<&HashMap<String, crate::core::typecheck::types::Type>>,
) -> Option<lsp_types::Hover> {
    let at = alias_at_position(source, root, position)?;

    // Only produce hover when the alias is indexed (i.e. defined in scope).
    alias_index.primary_definition(&at.alias_name)?;

    let resolved = alias_types
        .and_then(|m| m.get(&at.alias_name))
        .map(|ty| format!("\n\nResolved: `{ty}`"))
        .unwrap_or_default();

    let content = format!("**{}** — type alias{}", at.alias_name, resolved);

    Some(lsp_types::Hover {
        contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: content,
        }),
        range: Some(at.reference_range),
    })
}

// ── Rename ────────────────────────────────────────────────────────────────────

/// Rename a type alias across the document.
///
/// Renames:
/// 1. Every definition site recorded in `alias_index`.
/// 2. Every alias reference in every plain `type:` string in the document.
///
/// **Limitation** (§A7.2): references inside escaped or interpolated type
/// strings are not updated.  The type checker will surface any stale
/// references as "unknown alias" warnings.
pub fn rename_alias(
    source: &str,
    root: &SyntaxNode,
    position: &Position,
    new_name: &str,
    alias_index: &AliasIndex,
    uri: &Url,
) -> Option<WorkspaceEdit> {
    let at = alias_at_position(source, root, position)?;
    let alias_name = &at.alias_name;

    let mut edits: Vec<TextEdit> = Vec::new();

    // 1. Rename all definition sites.
    for &def_range in alias_index.definitions(alias_name) {
        edits.push(TextEdit {
            range: def_range,
            new_text: new_name.to_string(),
        });
    }

    // 2. Rename every alias reference inside every plain type: string.
    for reference_range in collect_alias_references_in_type_strings(source, root, alias_name) {
        edits.push(TextEdit {
            range: reference_range,
            new_text: new_name.to_string(),
        });
    }

    if edits.is_empty() {
        return None;
    }

    let mut changes = HashMap::new();
    changes.insert(uri.clone(), edits);
    Some(WorkspaceEdit {
        changes: Some(changes),
        ..WorkspaceEdit::default()
    })
}

/// Collect the LSP ranges of every occurrence of `alias_name` inside
/// plain `type:` strings in the document.
fn collect_alias_references_in_type_strings(
    source: &str,
    root: &SyntaxNode,
    alias_name: &str,
) -> Vec<Range> {
    let mut ranges = Vec::new();

    for tok in root.descendants_with_tokens() {
        let Some(tok) = tok.as_token() else { continue };
        if tok.kind() != SyntaxKind::STRING {
            continue;
        }
        if !is_type_annotation_string(tok.parent()) {
            continue;
        }

        let raw_text = tok.text();
        let content = match raw_text.strip_prefix('"').and_then(|s| s.strip_suffix('"')) {
            Some(c) => c,
            None => continue,
        };

        // Skip escaped strings (§A7.2).
        if content.contains('\\') {
            continue;
        }

        let content_start = usize::from(tok.text_range().start()) + 1;

        let Ok((_, alias_refs)) = parse_type_with_refs(content) else {
            continue;
        };
        for alias_ref in alias_refs {
            if alias_ref.name == alias_name {
                let start = content_start + alias_ref.span.0;
                let end = content_start + alias_ref.span.1;
                ranges.push(byte_range_to_lsp_range(source, start, end));
            }
        }
    }

    ranges
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::rowan::parse_unit;
    use lsp_types::Url;

    fn test_uri() -> Url {
        Url::parse("file:///test/file.eu").unwrap()
    }

    // ── parse_type_with_refs tests (§A7.1) ──────────────────────────────

    #[test]
    fn parse_type_with_refs_finds_alias_spans() {
        let (_, refs) = parse_type_with_refs("Person -> Json").unwrap();
        assert_eq!(refs.len(), 2);
        assert_eq!(refs[0].name, "Person");
        assert_eq!(refs[0].span, (0, 6));
        assert_eq!(refs[1].name, "Json");
        assert_eq!(refs[1].span, (10, 14));
    }

    #[test]
    fn parse_type_with_refs_ignores_lowercase_vars() {
        let (_, refs) = parse_type_with_refs("a -> b -> string").unwrap();
        assert!(refs.is_empty(), "lowercase vars should not be recorded");
    }

    #[test]
    fn parse_type_with_refs_union_with_alias() {
        let (_, refs) = parse_type_with_refs("number | Person | string").unwrap();
        assert_eq!(refs.len(), 1);
        assert_eq!(refs[0].name, "Person");
        // "number | " = 9 chars; "Person" starts at 9
        assert_eq!(refs[0].span.0, 9);
        assert_eq!(refs[0].span.1, 15);
    }

    #[test]
    fn parse_type_with_refs_error_still_errors() {
        let result = parse_type_with_refs("number |");
        assert!(result.is_err(), "trailing pipe should still be an error");
    }

    // ── AliasIndex build tests ───────────────────────────────────────────

    #[test]
    fn build_alias_index_from_type_def() {
        let source = "` { type-def: \"Point\" }\npoint: { x: 1, y: 2 }\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let uri = test_uri();
        let index = build_alias_index(source, &root, &uri);
        assert!(
            index.primary_definition("Point").is_some(),
            "Point should be indexed from type-def:"
        );
    }

    #[test]
    fn build_alias_index_from_types_block() {
        let source = "` { types: { Person: \"{ name: string }\" } }\npeople: []\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let uri = test_uri();
        let index = build_alias_index(source, &root, &uri);
        assert!(
            index.primary_definition("Person").is_some(),
            "Person should be indexed from types: block"
        );
    }

    // ── Go-to-definition tests ───────────────────────────────────────────

    #[test]
    fn goto_definition_from_alias_reference() {
        // Alias defined via type-def:, referenced in a type: annotation.
        let source = concat!(
            "` { type-def: \"Point\" }\n",
            "point: { x: 1, y: 2 }\n",
            "` { type: \"Point -> number\" }\n",
            "get-x(p): p.x\n",
        );
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let uri = test_uri();
        let index = build_alias_index(source, &root, &uri);

        // Cursor on "Point" inside the `type:` string on line 2.
        // "` { type: \"" = 11 chars, then "Point" starts at col 11.
        let position = Position {
            line: 2,
            character: 11,
        };

        let result = goto_definition_for_alias(source, &root, &position, &index, &uri);
        assert!(
            result.is_some(),
            "should find definition of Point from type: string"
        );
    }

    // ── Rename tests ─────────────────────────────────────────────────────

    #[test]
    fn rename_alias_updates_definition_and_references() {
        let source = concat!(
            "` { type-def: \"Point\" }\n",
            "point: { x: 1, y: 2 }\n",
            "` { type: \"Point -> number\" }\n",
            "get-x(p): p.x\n",
        );
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let uri = test_uri();
        let index = build_alias_index(source, &root, &uri);

        // Cursor on "Point" in the type: string (line 2, col 11).
        let position = Position {
            line: 2,
            character: 11,
        };

        let edit = rename_alias(source, &root, &position, "Coord", &index, &uri);
        assert!(edit.is_some(), "rename should produce edits");

        let changes = edit.unwrap().changes.unwrap();
        let edits = changes.get(&uri).unwrap();
        // Should rename both the type-def: value AND the type: reference.
        assert!(
            edits.len() >= 2,
            "expected at least 2 edits (definition + reference), got {}",
            edits.len()
        );
        for e in edits {
            assert_eq!(e.new_text, "Coord");
        }
    }

    // ── Graceful degradation tests ───────────────────────────────────────

    #[test]
    fn escaped_type_string_degrades_gracefully() {
        // A type string with escape sequences should not crash.
        let source = "` { type: \"number | \\\"literal\\\"\" }\nx: 42\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        // Cursor inside the escaped string.
        let position = Position {
            line: 0,
            character: 13,
        };
        // Should return None gracefully, not panic.
        let result = alias_at_position(source, &root, &position);
        assert!(
            result.is_none(),
            "escaped type strings should degrade gracefully"
        );
    }

    #[test]
    fn byte_range_to_lsp_range_utf16_column() {
        // U+2192 (→) is 3 UTF-8 bytes but 1 UTF-16 code unit.
        // In "number → MyType", MyType starts at byte 11 (6 + 1 + 3 + 1)
        // but at UTF-16 column 9 (6 + 1 + 1 + 1).
        let source = "number \u{2192} MyType";
        let start = source.find("MyType").unwrap();
        let end = start + "MyType".len();
        let range = byte_range_to_lsp_range(source, start, end);
        assert_eq!(range.start.line, 0);
        assert_eq!(
            range.start.character, 9,
            "column must be counted in UTF-16 units, not UTF-8 bytes"
        );
        assert_eq!(range.end.character, 15);
    }
}
