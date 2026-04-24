//! LSP inlay hint provider.
//!
//! Provides inline hints for:
//! - Parameter names at function call sites (e.g. `f(x: 1, y: 2)`)
//! - Operator fixity on operator declarations

use std::collections::HashMap;

use crate::core::typecheck::types::Type;
use crate::syntax::rowan::ast::{self, AstToken, DeclarationKind, HasSoup};
use crate::syntax::rowan::kind::SyntaxNode;
use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Range};
use rowan::ast::AstNode;

use super::diagnostics::text_range_to_lsp_range;
use super::symbol_table::SymbolTable;

/// Compute inlay hints for the given range of a document.
///
/// When `type_env` is provided, declarations with inferred types show
/// `: <type>` hints after the declaration name.
pub fn inlay_hints(
    source: &str,
    root: &SyntaxNode,
    range: &Range,
    table: &SymbolTable,
    type_env: Option<&HashMap<String, Type>>,
) -> Vec<InlayHint> {
    let mut hints = Vec::new();

    // Walk declarations for operator fixity hints and parameter hints in call sites
    if let Some(unit) = ast::Unit::cast(root.clone()) {
        collect_hints_from_unit(source, &unit, range, table, type_env, &mut hints);
    }

    hints
}

/// Collect hints from a unit's declarations.
fn collect_hints_from_unit(
    source: &str,
    unit: &ast::Unit,
    range: &Range,
    table: &SymbolTable,
    type_env: Option<&HashMap<String, Type>>,
    hints: &mut Vec<InlayHint>,
) {
    for decl in unit.declarations() {
        let decl_range = text_range_to_lsp_range(source, decl.syntax().text_range());
        if !ranges_overlap(&decl_range, range) {
            continue;
        }

        // Operator fixity hints
        if let Some(head) = decl.head() {
            let kind = head.classify_declaration();
            collect_operator_fixity_hint(source, &kind, hints);

            // Type hints from the type environment
            if let Some(env) = type_env {
                collect_type_hint_for_decl(source, &kind, env, hints);
            }
        }

        // Parameter name hints in declaration bodies
        if let Some(body) = decl.body() {
            if let Some(soup) = body.soup() {
                collect_parameter_hints_from_soup(source, &soup, range, table, hints);
            }
        }

        // Monadic block binding type hints in declaration bodies
        if let Some(body) = decl.body() {
            if let Some(soup) = body.soup() {
                for element in soup.elements() {
                    if let ast::Element::Block(block) = element {
                        collect_monadic_binding_hints(source, &block, range, table, hints);
                    }
                }
            }
        }
    }
}

/// Collect inlay hints for monadic block bindings showing the expected type.
///
/// For `{ :for x: [1,2,3] }`, shows `[a]` as a type hint on `x`.
fn collect_monadic_binding_hints(
    source: &str,
    block: &ast::Block,
    range: &Range,
    table: &SymbolTable,
    hints: &mut Vec<InlayHint>,
) {
    let block_range = text_range_to_lsp_range(source, block.syntax().text_range());
    if !ranges_overlap(&block_range, range) {
        return;
    }

    // Detect the monad tag from block metadata
    let monad_name = match extract_monad_tag_from_block(block) {
        Some(name) => name,
        None => return,
    };

    // Look up the monad in the symbol table to get its type.
    // Only show hints for known monad namespaces with a declared type.
    let symbols = table.lookup(&monad_name);
    let monad_sym = match symbols.iter().find(|sym| sym.is_monad) {
        Some(sym) => sym,
        None => return,
    };
    let monad_type = match &monad_sym.monad_type {
        Some(t) => t.clone(),
        None => return,
    };

    // Add type hints on each binding declaration
    for decl in block.declarations() {
        if let Some(head) = decl.head() {
            let kind = head.classify_declaration();
            let name_token = match &kind {
                DeclarationKind::Property(id) => id.syntax().clone(),
                DeclarationKind::Function(id, _) => id.syntax().clone(),
                _ => continue,
            };
            let token_range = text_range_to_lsp_range(source, name_token.text_range());
            hints.push(InlayHint {
                position: token_range.end,
                label: InlayHintLabel::String(format!(": {monad_type}")),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: Some(lsp_types::InlayHintTooltip::String(format!(
                    "Monad binding — value must be {monad_type}",
                ))),
                padding_left: Some(false),
                padding_right: Some(true),
                data: None,
            });
        }
    }
}

/// Extract a symbol tag name from a block's metadata.
///
/// For `{ :for x: ... }`, returns `Some("for")`.  Returns the raw
/// name without filtering — callers check the symbol table to
/// determine whether it is a monad namespace.
fn extract_monad_tag_from_block(block: &ast::Block) -> Option<String> {
    let meta = block.meta()?;
    let soup = meta.soup()?;
    let elements: Vec<ast::Element> = soup.elements().collect();

    if elements.len() == 1 {
        if let ast::Element::Lit(lit) = &elements[0] {
            let text = lit.syntax().text().to_string();
            if let Some(name) = text.strip_prefix(':') {
                return Some(name.to_string());
            }
        }
    }
    None
}

/// Add an inlay hint showing operator fixity for operator declarations.
fn collect_operator_fixity_hint(source: &str, kind: &DeclarationKind, hints: &mut Vec<InlayHint>) {
    let (op_token, fixity) = match kind {
        DeclarationKind::Nullary(_, op) => (op.syntax().clone(), "nullary"),
        DeclarationKind::Prefix(_, op, _) => (op.syntax().clone(), "prefix"),
        DeclarationKind::Postfix(_, _, op) => (op.syntax().clone(), "postfix"),
        DeclarationKind::Binary(_, _, op, _) => (op.syntax().clone(), "binary"),
        _ => return,
    };

    let op_range = text_range_to_lsp_range(source, op_token.text_range());
    hints.push(InlayHint {
        position: op_range.end,
        label: InlayHintLabel::String(format!(": {fixity}")),
        kind: Some(InlayHintKind::TYPE),
        text_edits: None,
        tooltip: None,
        padding_left: Some(false),
        padding_right: Some(true),
        data: None,
    });
}

/// Add an inlay hint showing the inferred type for a declaration.
///
/// Skips `any` types (uninformative) and declarations that already have
/// explicit type annotations (those are shown via hover instead).
fn collect_type_hint_for_decl(
    source: &str,
    kind: &DeclarationKind,
    type_env: &HashMap<String, Type>,
    hints: &mut Vec<InlayHint>,
) {
    let (name, name_token) = match kind {
        DeclarationKind::Property(id) => (id.value().to_string(), id.syntax().clone()),
        DeclarationKind::Function(id, _) => (id.value().to_string(), id.syntax().clone()),
        _ => return,
    };

    if let Some(ty) = type_env.get(&name) {
        if matches!(ty, Type::Any) {
            return;
        }
        let token_range = text_range_to_lsp_range(source, name_token.text_range());
        hints.push(InlayHint {
            position: token_range.end,
            label: InlayHintLabel::String(format!(": {ty}")),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: None,
            padding_left: Some(false),
            padding_right: Some(true),
            data: None,
        });
    }
}

/// Collect parameter name hints from a soup (expression sequence).
fn collect_parameter_hints_from_soup(
    source: &str,
    soup: &ast::Soup,
    range: &Range,
    table: &SymbolTable,
    hints: &mut Vec<InlayHint>,
) {
    let elements: Vec<ast::Element> = soup.elements().collect();

    for (i, element) in elements.iter().enumerate() {
        // Look for Name followed by ApplyTuple: f(x, y, z)
        if let ast::Element::Name(name) = element {
            if let Some(ast::Element::ApplyTuple(args)) = elements.get(i + 1) {
                let tuple_range = text_range_to_lsp_range(source, args.syntax().text_range());
                if !ranges_overlap(&tuple_range, range) {
                    continue;
                }

                // Resolve the function name
                let fn_name = if let Some(id) = name.identifier() {
                    id.text().to_string()
                } else {
                    continue;
                };

                let symbols = table.lookup(&fn_name);
                let sym = match symbols.first() {
                    Some(s) => s,
                    None => continue,
                };

                // Only show hints for functions with 2+ named parameters
                if sym.parameters.len() < 2 {
                    continue;
                }

                // Generate hints for each argument
                for (arg_idx, arg_soup) in args.items().enumerate() {
                    if let Some(param_name) = sym.parameters.get(arg_idx) {
                        let arg_range =
                            text_range_to_lsp_range(source, arg_soup.syntax().text_range());
                        hints.push(InlayHint {
                            position: arg_range.start,
                            label: InlayHintLabel::String(format!("{param_name}: ")),
                            kind: Some(InlayHintKind::PARAMETER),
                            text_edits: None,
                            tooltip: None,
                            padding_left: Some(false),
                            padding_right: Some(false),
                            data: None,
                        });
                    }
                }
            }
        }

        // Recurse into nested blocks
        if let ast::Element::Block(block) = element {
            for inner_decl in block.declarations() {
                if let Some(body) = inner_decl.body() {
                    if let Some(inner_soup) = body.soup() {
                        collect_parameter_hints_from_soup(source, &inner_soup, range, table, hints);
                    }
                }
            }
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
    fn empty_source_no_hints() {
        let source = "";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let table = SymbolTable::new();
        let hints = inlay_hints(source, &root, &full_range(), &table, None);
        assert!(hints.is_empty());
    }

    #[test]
    fn operator_fixity_hint() {
        let source = "(x + y): x\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let table = make_table(source);
        let hints = inlay_hints(source, &root, &full_range(), &table, None);
        let fixity_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::TYPE))
            .collect();
        assert!(!fixity_hints.is_empty(), "should have operator fixity hint");
        match &fixity_hints[0].label {
            InlayHintLabel::String(s) => {
                assert!(s.contains("binary"), "should show binary fixity, got: {s}")
            }
            _ => panic!("expected string label"),
        }
    }

    #[test]
    fn no_param_hints_for_single_param() {
        let source = "f(x): x\nmain: f(1)\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let table = make_table(source);
        let hints = inlay_hints(source, &root, &full_range(), &table, None);
        let param_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::PARAMETER))
            .collect();
        assert!(
            param_hints.is_empty(),
            "no param hints for single-param function"
        );
    }

    #[test]
    fn param_hints_for_multi_param() {
        let source = "f(x, y): x\nmain: f(1, 2)\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();
        let table = make_table(source);
        let hints = inlay_hints(source, &root, &full_range(), &table, None);
        let param_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == Some(InlayHintKind::PARAMETER))
            .collect();
        assert_eq!(
            param_hints.len(),
            2,
            "should have 2 param hints for f(1, 2)"
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

    #[test]
    fn monadic_binding_type_hint() {
        // Set up a table with a typed monad
        let uri = lsp_types::Url::parse("file:///test.eu").unwrap();
        let monad_source = "` { monad: \"[a]\" }\nfor: 1\n";
        let monad_parse = parse_unit(monad_source);
        let monad_unit = monad_parse.tree();
        let mut table = SymbolTable::new();
        table.add_from_unit(&monad_unit, monad_source, &uri, SymbolSource::Local);

        // Now parse a file using the monad
        let source = "main: { :for x: [1,2] }.(x)\n";
        let parse = parse_unit(source);
        let root = parse.syntax_node();

        let hints = inlay_hints(source, &root, &full_range(), &table, None);
        let type_hints: Vec<_> = hints
            .iter()
            .filter(|h| matches!(&h.label, InlayHintLabel::String(s) if s.contains("[a]")))
            .collect();
        assert!(
            !type_hints.is_empty(),
            "should show [a] type hint on monadic binding"
        );
    }
}
