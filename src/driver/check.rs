//! Type annotation check command (`eu check`).
//!
//! Parses eucalypt source files, extracts `type:` metadata annotations from
//! all declarations, and reports any annotation strings that fail to parse.
//!
//! No actual type checking is performed yet — this phase validates that
//! annotations are syntactically well-formed according to the type grammar.

use std::fs;
use std::path::Path;

use crate::core::typecheck::parse;
use crate::driver::options::EucalyptOptions;
use crate::syntax::rowan::ast::{self, Block, Declaration, Element, HasSoup};
use crate::syntax::rowan::parse_unit;
use rowan::ast::AstNode;

/// A single type annotation found in a source file.
struct Annotation {
    /// Byte offset of the annotation string within the source file.
    source_offset: usize,
    /// The annotation string value.
    value: String,
    /// Name of the declaration the annotation was attached to (for error messages).
    decl_name: String,
}

/// Run the `eu check` command.
pub fn check(opt: &EucalyptOptions) -> Result<i32, String> {
    let files: Vec<_> = opt.explicit_inputs().iter().collect();

    if files.is_empty() {
        eprintln!("eu check: no files specified");
        return Ok(1);
    }

    let mut total_errors = 0;

    for input in &files {
        let path_str = input.locator().to_string();
        let path = Path::new(&path_str);

        if !path.exists() {
            eprintln!("eu check: file not found: {path_str}");
            total_errors += 1;
            continue;
        }

        let source = fs::read_to_string(path)
            .map_err(|e| format!("Failed to read '{}': {}", path_str, e))?;

        let file_errors = check_source(&path_str, &source, opt.check_strict());
        total_errors += file_errors;
    }

    if total_errors == 0 {
        Ok(0)
    } else if opt.check_strict() {
        Ok(1)
    } else {
        // Warnings do not cause non-zero exit unless --strict
        Ok(0)
    }
}

/// Check a single source string, reporting type annotation parse errors.
///
/// Returns the number of annotation errors found.
fn check_source(filename: &str, source: &str, strict: bool) -> usize {
    let parse_result = parse_unit(source);

    // Report any eucalypt syntax errors first (these are real errors regardless of --strict).
    for err in parse_result.errors() {
        eprintln!("{filename}: parse error: {err}");
    }

    let unit = parse_result.tree();
    let annotations = collect_annotations_from_unit(&unit, source);

    let mut error_count = 0;
    for ann in annotations {
        if let Err(e) = parse::parse_type(&ann.value) {
            let severity = if strict { "error" } else { "warning" };
            let line_col = byte_offset_to_line_col(source, ann.source_offset);
            eprintln!(
                "{filename}:{line_col}: {severity}: invalid type annotation on '{}': {}",
                ann.decl_name, e
            );
            error_count += 1;
        }
    }

    error_count
}

/// Convert a byte offset to a `line:col` string (1-based).
fn byte_offset_to_line_col(source: &str, offset: usize) -> String {
    let clamped = offset.min(source.len());
    let prefix = &source[..clamped];
    let line = prefix.bytes().filter(|&b| b == b'\n').count() + 1;
    let col = match prefix.rfind('\n') {
        Some(last_nl) => clamped - last_nl,
        None => clamped + 1,
    };
    format!("{line}:{col}")
}

/// Collect all `type:` annotations from a `Unit`.
fn collect_annotations_from_unit(unit: &ast::Unit, source: &str) -> Vec<Annotation> {
    let mut result = Vec::new();
    for decl in unit.declarations() {
        collect_annotations_from_decl(&decl, source, &mut result);
    }
    result
}

/// Collect `type:` annotations from a declaration (and any nested blocks in its body).
fn collect_annotations_from_decl(decl: &Declaration, source: &str, out: &mut Vec<Annotation>) {
    let decl_name = declaration_name(decl);

    // Inspect the declaration's backtick metadata block for a `type:` field.
    if let Some(meta) = decl.meta() {
        if let Some(soup) = meta.soup() {
            let elems: Vec<ast::Element> = soup.elements().collect();
            if let Some(Element::Block(meta_block)) = elems.first() {
                if let Some(ann) = extract_type_annotation(meta_block, &decl_name) {
                    out.push(ann);
                }
            }
        }
    }

    // Recurse into nested block bodies.
    if let Some(body) = decl.body() {
        if let Some(body_soup) = body.soup() {
            for elem in body_soup.elements() {
                if let Element::Block(inner_block) = elem {
                    collect_annotations_from_block(&inner_block, source, out);
                }
            }
        }
    }
}

/// Collect `type:` annotations from all declarations within a block.
fn collect_annotations_from_block(block: &Block, source: &str, out: &mut Vec<Annotation>) {
    for decl in block.declarations() {
        collect_annotations_from_decl(&decl, source, out);
    }
}

/// Try to extract a `type:` string literal from a metadata block.
fn extract_type_annotation(meta_block: &Block, decl_name: &str) -> Option<Annotation> {
    for inner_decl in meta_block.declarations() {
        let Some(head) = inner_decl.head() else {
            continue;
        };
        let ast::DeclarationKind::Property(prop) = head.classify_declaration() else {
            continue;
        };
        if prop.value() != "type" {
            continue;
        }

        // Found a `type:` field — extract its string literal value.
        let body = inner_decl.body()?;
        let body_soup = body.soup()?;
        let body_elems: Vec<ast::Element> = body_soup.elements().collect();

        if let Some(Element::Lit(lit)) = body_elems.first() {
            if let Some(lit_val) = lit.value() {
                if let Some(type_str) = lit_val.string_value() {
                    // Find the byte offset of this literal in the source.
                    let node_offset = usize::from(lit.syntax().text_range().start());
                    return Some(Annotation {
                        source_offset: node_offset,
                        value: type_str.to_string(),
                        decl_name: decl_name.to_string(),
                    });
                }
            }
        }
    }
    None
}

/// Attempt to extract a human-readable name from a declaration head.
fn declaration_name(decl: &Declaration) -> String {
    let Some(head) = decl.head() else {
        return "<unknown>".to_string();
    };
    match head.classify_declaration() {
        ast::DeclarationKind::Property(id) => id.value().to_string(),
        ast::DeclarationKind::Function(name, _) => name.value().to_string(),
        _ => "<unknown>".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_errors_for_valid_annotations() {
        let src = r#"
` { type: "number -> number" }
double(x): x * 2
"#;
        assert_eq!(check_source("<test>", src, false), 0);
    }

    #[test]
    fn reports_invalid_annotation() {
        let src = r#"
` { type: "number ->" }
bad(x): x
"#;
        assert_eq!(check_source("<test>", src, false), 1);
    }

    #[test]
    fn ignores_declarations_without_type_annotation() {
        let src = r#"
` { doc: "just a doc" }
no_type(x): x
"#;
        assert_eq!(check_source("<test>", src, false), 0);
    }

    #[test]
    fn multiple_valid_annotations() {
        let src = r#"
` { type: "(a -> b) -> [a] -> [b]" }
map(f, xs): __MAP

` { type: "(a -> bool) -> [a] -> [a]" }
filter(p, xs): __FILTER
"#;
        assert_eq!(check_source("<test>", src, false), 0);
    }

    #[test]
    fn multiple_annotations_one_bad() {
        let src = r#"
` { type: "(a -> b) -> [a] -> [b]" }
map(f, xs): __MAP

` { type: "number ->" }
bad(x): x
"#;
        assert_eq!(check_source("<test>", src, false), 1);
    }

    #[test]
    fn byte_offset_line_col_first_line() {
        let src = "hello world";
        assert_eq!(byte_offset_to_line_col(src, 0), "1:1");
        assert_eq!(byte_offset_to_line_col(src, 5), "1:6");
    }

    #[test]
    fn byte_offset_line_col_second_line() {
        let src = "line1\nline2";
        assert_eq!(byte_offset_to_line_col(src, 6), "2:1");
    }
}
