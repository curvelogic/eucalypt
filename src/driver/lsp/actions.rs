//! LSP code action provider.
//!
//! Provides quick-fix code actions for common issues:
//! - Suggest qualified prelude names for unresolved identifiers
//!   (e.g. `split` -> `str.split`)
//!
//! Provides structural editing code actions:
//! - Wrap as namespace (property value → block)
//! - Promote metadata (shortcut → block form)
//! - Add metadata field
//! - Demote metadata (single-field block → shortcut)
//! - Let-block toggle

use crate::syntax::rowan::ast::{
    AstToken, Block, Declaration, DeclarationKind, DeclarationMetadata, Element, HasSoup,
};
use crate::syntax::rowan::kind::{SyntaxKind, SyntaxNode, SyntaxToken};
use lsp_types::{CodeAction, CodeActionKind, Range, TextEdit, Url, WorkspaceEdit};
use rowan::ast::AstNode;
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

    // Structural editing actions: walk AST nodes
    collect_structural_actions(source, root, range, uri, &mut actions);

    actions
}

/// Collect structural editing code actions (wrap-as-namespace,
/// metadata promotion/demotion, add-metadata, let-block toggle).
fn collect_structural_actions(
    source: &str,
    root: &SyntaxNode,
    range: &Range,
    uri: &Url,
    actions: &mut Vec<CodeAction>,
) {
    for event in root.preorder() {
        let node = match event {
            WalkEvent::Enter(n) => n,
            _ => continue,
        };

        let node_range = text_range_to_lsp_range(source, node.text_range());
        if !ranges_overlap(&node_range, range) {
            continue;
        }

        // Declaration-level actions
        if let Some(decl) = Declaration::cast(node.clone()) {
            collect_wrap_as_namespace_actions(source, &decl, uri, actions);
            collect_promote_metadata_actions(source, &decl, uri, actions);
            collect_demote_metadata_actions(source, &decl, uri, actions);
            collect_add_metadata_actions(source, &decl, uri, actions);
        }

        // Block-level actions (let-block toggle)
        if let Some(block) = Block::cast(node.clone()) {
            collect_let_block_toggle_actions(source, &block, uri, actions);
        }
    }
}

// ── Wrap as namespace ──────────────────────────────────────────────────────────

/// If the cursor is on a property declaration, offer to wrap the value
/// into a namespace block.
fn collect_wrap_as_namespace_actions(
    source: &str,
    decl: &Declaration,
    uri: &Url,
    actions: &mut Vec<CodeAction>,
) {
    let head = match decl.head() {
        Some(h) => h,
        None => return,
    };

    // Only for property declarations
    if !matches!(head.classify_declaration(), DeclarationKind::Property(_)) {
        return;
    }

    let colon = match decl.colon() {
        Some(c) => c,
        None => return,
    };

    // Determine the indentation of the declaration for formatting the block
    let decl_start = decl.syntax().text_range().start();
    let decl_offset: usize = decl_start.into();
    let indent = leading_indent(source, decl_offset);
    let inner_indent = format!("{indent}  ");

    let body = decl.body();
    let body_text = body
        .as_ref()
        .map(|b| b.syntax().text().to_string())
        .unwrap_or_default();
    let body_trimmed = body_text.trim();

    // Compute the range from after the colon to end of the declaration body
    let edit_start = colon.text_range().end();
    let edit_end = body
        .as_ref()
        .map(|b| b.syntax().text_range().end())
        .unwrap_or(colon.text_range().end());

    let new_text = if body_trimmed.is_empty() {
        format!(" {{\n{inner_indent}\n{indent}}}")
    } else {
        format!(" {{\n{inner_indent}{body_trimmed}\n{indent}}}")
    };

    let edit_range = text_range_to_lsp_range(source, rowan::TextRange::new(edit_start, edit_end));

    let edit = TextEdit {
        range: edit_range,
        new_text,
    };

    let mut changes = HashMap::new();
    changes.insert(uri.clone(), vec![edit]);

    actions.push(CodeAction {
        title: "Wrap as namespace".to_string(),
        kind: Some(CodeActionKind::REFACTOR_REWRITE),
        diagnostics: None,
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            ..WorkspaceEdit::default()
        }),
        is_preferred: None,
        ..CodeAction::default()
    });
}

// ── Promote metadata ───────────────────────────────────────────────────────────

/// Metadata shortcut classification for promotion/demotion.
enum MetadataShortcut {
    /// A doc string: `` ` "text" ``
    DocString(String),
    /// A symbol shortcut: `` ` :suppress `` or `` ` :main ``
    Symbol(String),
}

/// Classify a declaration's metadata as a shortcut form, if applicable.
fn classify_shortcut_meta(meta: &DeclarationMetadata) -> Option<MetadataShortcut> {
    let soup = meta.soup()?;
    let elements: Vec<Element> = soup.elements().collect();

    // Shortcut metadata has exactly one element (after the backtick)
    if elements.len() != 1 {
        return None;
    }

    match &elements[0] {
        Element::Lit(lit) => {
            let val = lit.value()?;
            if let Some(s) = val.string_value() {
                Some(MetadataShortcut::DocString(s.to_string()))
            } else {
                val.symbol_name()
                    .map(|s| MetadataShortcut::Symbol(s.to_string()))
            }
        }
        _ => None,
    }
}

/// If the declaration has shortcut metadata, offer to promote it to
/// block form.
fn collect_promote_metadata_actions(
    source: &str,
    decl: &Declaration,
    uri: &Url,
    actions: &mut Vec<CodeAction>,
) {
    let meta = match decl.meta() {
        Some(m) => m,
        None => return,
    };

    let shortcut = match classify_shortcut_meta(&meta) {
        Some(s) => s,
        None => return,
    };

    let promoted = match &shortcut {
        MetadataShortcut::DocString(s) => format!("{{ doc: \"{s}\" }}"),
        MetadataShortcut::Symbol(s) => match s.as_str() {
            "suppress" => "{ export: :suppress }".to_string(),
            "target" => {
                // :target uses the declaration's own name
                let decl_name = decl
                    .head()
                    .map(|h| match h.classify_declaration() {
                        DeclarationKind::Property(id) => id.text().to_string(),
                        DeclarationKind::Function(id, _) => id.text().to_string(),
                        _ => "unknown".to_string(),
                    })
                    .unwrap_or_else(|| "unknown".to_string());
                format!("{{ target: :{decl_name} }}")
            }
            // Any other symbol is a target shortcut
            _ => format!("{{ target: :{s} }}"),
        },
    };

    // Replace the metadata soup content (after backtick + space, before the
    // declaration head). The DECL_META contains a SOUP which has
    // BACKTICK + WHITESPACE + content + trailing WHITESPACE.
    let backtick_token = match find_backtick_in_meta(&meta) {
        Some(t) => t,
        None => return,
    };

    // The content to replace: everything from after the backtick to end of
    // DECL_META, but we want to keep the trailing whitespace that leads into
    // the declaration head. We replace "` old-content\n" with "` new-content\n".
    let meta_range = meta.syntax().text_range();
    let after_backtick = backtick_token.text_range().end();

    let trailing_ws = compute_trailing_whitespace(&meta);

    let edit_range = text_range_to_lsp_range(
        source,
        rowan::TextRange::new(after_backtick, meta_range.end()),
    );

    let new_text = format!(" {promoted}{trailing_ws}");

    let edit = TextEdit {
        range: edit_range,
        new_text,
    };

    let mut changes = HashMap::new();
    changes.insert(uri.clone(), vec![edit]);

    actions.push(CodeAction {
        title: "Promote metadata to block form".to_string(),
        kind: Some(CodeActionKind::REFACTOR_REWRITE),
        diagnostics: None,
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            ..WorkspaceEdit::default()
        }),
        is_preferred: None,
        ..CodeAction::default()
    });
}

// ── Demote metadata ────────────────────────────────────────────────────────────

/// If the declaration has single-field block metadata that can be
/// expressed as a shortcut, offer to demote it.
fn collect_demote_metadata_actions(
    source: &str,
    decl: &Declaration,
    uri: &Url,
    actions: &mut Vec<CodeAction>,
) {
    let meta = match decl.meta() {
        Some(m) => m,
        None => return,
    };

    // Must have a block in the metadata soup
    let soup = match meta.soup() {
        Some(s) => s,
        None => return,
    };

    let elements: Vec<Element> = soup.elements().collect();
    if elements.len() != 1 {
        return;
    }

    let block = match &elements[0] {
        Element::Block(b) => b,
        _ => return,
    };

    // Must have exactly one declaration in the block
    let decls: Vec<Declaration> = block.declarations().collect();
    if decls.len() != 1 {
        return;
    }

    let inner_decl = &decls[0];
    let inner_head = match inner_decl.head() {
        Some(h) => h,
        None => return,
    };

    let inner_name = match inner_head.classify_declaration() {
        DeclarationKind::Property(name) => name.text().to_string(),
        _ => return,
    };

    let inner_body = match inner_decl.body() {
        Some(b) => b,
        None => return,
    };

    let body_text = inner_body.syntax().text().to_string();
    let body_trimmed = body_text.trim();

    // Determine the shortcut form
    let shortcut = match inner_name.as_str() {
        "doc" => {
            // Body should be a string literal
            if body_trimmed.starts_with('"') && body_trimmed.ends_with('"') {
                Some(body_trimmed.to_string())
            } else {
                None
            }
        }
        "export" => {
            if body_trimmed == ":suppress" {
                Some(":suppress".to_string())
            } else {
                None
            }
        }
        "target" => {
            // target: :sym → :sym (symbol shortcut)
            // target: :decl-name → :target (self-naming shortcut)
            if let Some(sym_name) = body_trimmed.strip_prefix(':') {
                let decl_name = decl
                    .head()
                    .map(|h| match h.classify_declaration() {
                        DeclarationKind::Property(id) => id.text().to_string(),
                        DeclarationKind::Function(id, _) => id.text().to_string(),
                        _ => String::new(),
                    })
                    .unwrap_or_default();
                if sym_name == decl_name {
                    Some(":target".to_string())
                } else {
                    Some(body_trimmed.to_string())
                }
            } else {
                None
            }
        }
        _ => None,
    };

    let shortcut = match shortcut {
        Some(s) => s,
        None => return,
    };

    // Replace the metadata content
    let backtick_token = match find_backtick_in_meta(&meta) {
        Some(t) => t,
        None => return,
    };

    let meta_range = meta.syntax().text_range();
    let after_backtick = backtick_token.text_range().end();

    let trailing_ws = compute_trailing_whitespace(&meta);

    let edit_range = text_range_to_lsp_range(
        source,
        rowan::TextRange::new(after_backtick, meta_range.end()),
    );

    let new_text = format!(" {shortcut}{trailing_ws}");

    let edit = TextEdit {
        range: edit_range,
        new_text,
    };

    let mut changes = HashMap::new();
    changes.insert(uri.clone(), vec![edit]);

    actions.push(CodeAction {
        title: "Demote metadata to shortcut form".to_string(),
        kind: Some(CodeActionKind::REFACTOR_REWRITE),
        diagnostics: None,
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            ..WorkspaceEdit::default()
        }),
        is_preferred: None,
        ..CodeAction::default()
    });
}

// ── Add metadata field ─────────────────────────────────────────────────────────

/// Offer to add a metadata field to a declaration.
fn collect_add_metadata_actions(
    source: &str,
    decl: &Declaration,
    uri: &Url,
    actions: &mut Vec<CodeAction>,
) {
    // Must have a head (valid declaration)
    if decl.head().is_none() {
        return;
    }

    let fields = ["type", "doc", "target", "export", "monad", "format", "type-def"];

    for field in &fields {
        let (edit_range, new_text) = compute_add_metadata_edit(source, decl, field);
        let edit = TextEdit {
            range: edit_range,
            new_text,
        };
        let mut changes = HashMap::new();
        changes.insert(uri.clone(), vec![edit]);

        actions.push(CodeAction {
            title: format!("Add `{field}:` metadata"),
            kind: Some(CodeActionKind::REFACTOR_REWRITE),
            diagnostics: None,
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                ..WorkspaceEdit::default()
            }),
            is_preferred: None,
            ..CodeAction::default()
        });
    }
}

/// Compute the text edit for adding a metadata field.
///
/// Handles three cases:
/// 1. No existing metadata → insert new backtick + block before head
/// 2. Shortcut metadata → promote and add the field
/// 3. Block metadata → append the field to the existing block
fn compute_add_metadata_edit(source: &str, decl: &Declaration, field: &str) -> (Range, String) {
    let default_value = match field {
        "type" => "\"\"",
        "doc" => "\"\"",
        "target" => ":main",
        "export" => ":suppress",
        "monad" => "true",
        "format" => ":yaml",
        "type-def" => "\"\"",
        _ => "\"\"",
    };

    match decl.meta() {
        None => {
            // No metadata: prefer shortcut form when one exists.
            let head = decl.head().expect("checked above");
            let head_start = head.syntax().text_range().start();
            let head_offset: usize = head_start.into();
            let indent = leading_indent(source, head_offset);

            let insert_pos =
                text_range_to_lsp_range(source, rowan::TextRange::new(head_start, head_start));

            let shortcut = match field {
                "doc" => Some("\"\"".to_string()),
                "target" => Some(":target".to_string()),
                "export" => Some(":suppress".to_string()),
                _ => None,
            };

            let new_text = if let Some(sc) = shortcut {
                format!("` {sc}\n{indent}")
            } else {
                format!("` {{ {field}: {default_value} }}\n{indent}")
            };
            (insert_pos, new_text)
        }
        Some(meta) => {
            // Check if shortcut or block metadata
            let soup = meta.soup();
            let elements: Vec<Element> = soup
                .as_ref()
                .map(|s| s.elements().collect())
                .unwrap_or_default();

            if elements.len() == 1 {
                if let Element::Block(block) = &elements[0] {
                    // Block metadata: insert new field before closing brace
                    let close_brace = match block.close_brace() {
                        Some(b) => b,
                        None => {
                            // Malformed: fall back to inserting before head
                            return compute_add_metadata_no_meta(
                                source,
                                decl,
                                field,
                                default_value,
                            );
                        }
                    };

                    let close_start = close_brace.text_range().start();
                    let insert_pos = text_range_to_lsp_range(
                        source,
                        rowan::TextRange::new(close_start, close_start),
                    );

                    let new_text = format!(", {field}: {default_value} ");
                    (insert_pos, new_text)
                } else if let Some(shortcut) = classify_shortcut_meta(&meta) {
                    // Shortcut metadata: promote to block and add field
                    let promoted_field = match &shortcut {
                        MetadataShortcut::DocString(s) => format!("doc: \"{s}\""),
                        MetadataShortcut::Symbol(s) => match s.as_str() {
                            "suppress" => "export: :suppress".to_string(),
                            "main" => "target: :main".to_string(),
                            _ => format!("tag: :{s}"),
                        },
                    };

                    let backtick_token = match find_backtick_in_meta(&meta) {
                        Some(t) => t,
                        None => {
                            return compute_add_metadata_no_meta(
                                source,
                                decl,
                                field,
                                default_value,
                            );
                        }
                    };

                    let meta_range = meta.syntax().text_range();
                    let after_backtick = backtick_token.text_range().end();

                    let trailing_ws = compute_trailing_whitespace(&meta);

                    let edit_range = text_range_to_lsp_range(
                        source,
                        rowan::TextRange::new(after_backtick, meta_range.end()),
                    );

                    let new_text =
                        format!(" {{ {promoted_field}, {field}: {default_value} }}{trailing_ws}");
                    (edit_range, new_text)
                } else {
                    compute_add_metadata_no_meta(source, decl, field, default_value)
                }
            } else {
                compute_add_metadata_no_meta(source, decl, field, default_value)
            }
        }
    }
}

/// Fallback: insert metadata before the declaration head.
fn compute_add_metadata_no_meta(
    source: &str,
    decl: &Declaration,
    field: &str,
    default_value: &str,
) -> (Range, String) {
    let head = decl.head().expect("checked by caller");
    let head_start = head.syntax().text_range().start();
    let head_offset: usize = head_start.into();
    let indent = leading_indent(source, head_offset);

    let insert_pos = text_range_to_lsp_range(source, rowan::TextRange::new(head_start, head_start));

    let new_text = format!("` {{ {field}: {default_value} }}\n{indent}");
    (insert_pos, new_text)
}

// ── Let-block toggle ───────────────────────────────────────────────────────────

/// If the cursor is on a block, offer to toggle between plain block
/// and `:let` sequential block.
fn collect_let_block_toggle_actions(
    source: &str,
    block: &Block,
    uri: &Url,
    actions: &mut Vec<CodeAction>,
) {
    let open_brace = match block.open_brace() {
        Some(b) => b,
        None => return,
    };

    // Must have at least one declaration to be meaningful
    if block.declarations().next().is_none() {
        return;
    }

    let block_meta = block.meta();

    let has_let = block_meta.as_ref().is_some_and(|bm| {
        bm.soup().is_some_and(|s| {
            s.elements().any(|e| {
                if let Element::Lit(lit) = &e {
                    lit.value()
                        .and_then(|v| v.symbol_name().map(|s| s == "let"))
                        .unwrap_or(false)
                } else {
                    false
                }
            })
        })
    });

    if has_let {
        // Remove :let — replace the BLOCK_META node
        let bm = block_meta.expect("checked above");
        let bm_range = bm.syntax().text_range();

        let edit_range = text_range_to_lsp_range(source, bm_range);
        let edit = TextEdit {
            range: edit_range,
            new_text: String::new(),
        };

        let mut changes = HashMap::new();
        changes.insert(uri.clone(), vec![edit]);

        actions.push(CodeAction {
            title: "Convert to plain block".to_string(),
            kind: Some(CodeActionKind::REFACTOR_REWRITE),
            diagnostics: None,
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                ..WorkspaceEdit::default()
            }),
            is_preferred: None,
            ..CodeAction::default()
        });
    } else {
        // Add :let — insert after the open brace
        let after_brace = open_brace.text_range().end();
        let insert_pos =
            text_range_to_lsp_range(source, rowan::TextRange::new(after_brace, after_brace));

        let edit = TextEdit {
            range: insert_pos,
            new_text: " :let".to_string(),
        };

        let mut changes = HashMap::new();
        changes.insert(uri.clone(), vec![edit]);

        actions.push(CodeAction {
            title: "Convert to sequential let-block".to_string(),
            kind: Some(CodeActionKind::REFACTOR_REWRITE),
            diagnostics: None,
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                ..WorkspaceEdit::default()
            }),
            is_preferred: None,
            ..CodeAction::default()
        });
    }
}

// ── Helpers ────────────────────────────────────────────────────────────────────

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

/// Find the BACKTICK token inside a `DeclarationMetadata` node.
///
/// The backtick lives inside the SOUP child, not as a direct child of
/// the DECL_META node.
fn find_backtick_in_meta(meta: &DeclarationMetadata) -> Option<SyntaxToken> {
    // Search recursively through all descendant tokens
    meta.syntax()
        .descendants_with_tokens()
        .filter_map(|it| it.into_token())
        .find(|t| t.kind() == SyntaxKind::BACKTICK)
}

/// Compute the trailing whitespace in a `DeclarationMetadata` node.
///
/// This is the whitespace between the last non-whitespace content and
/// the end of the DECL_META node (typically `\n` or `\n  `).
fn compute_trailing_whitespace(meta: &DeclarationMetadata) -> String {
    let meta_text = meta.syntax().text().to_string();
    meta_text
        .rfind(|c: char| !c.is_whitespace())
        .map(|i| meta_text[i + meta_text[i..].chars().next().unwrap().len_utf8()..].to_string())
        .unwrap_or_default()
}

/// Extract the leading whitespace (indentation) at the line containing
/// the given byte offset.
fn leading_indent(source: &str, offset: usize) -> String {
    // Find the start of the line
    let line_start = source[..offset].rfind('\n').map(|i| i + 1).unwrap_or(0);
    let line = &source[line_start..];
    let indent_len = line.len() - line.trim_start().len();
    line[..indent_len].to_string()
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
