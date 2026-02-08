//! Parse `lsp-context` metadata from eucalypt source files.
//!
//! The `lsp-context` metadata declares co-inputs that would normally be
//! provided as CLI arguments at runtime. The LSP uses this to build
//! the symbol table for semantic features.
//!
//! Supported syntax (backtick creates declaration metadata):
//! ```eu
//! ` { lsp-context: ["base.yaml", "helpers.eu"] }
//! ` { lsp-context: "data.json" }
//! ```
//!
//! Or without backtick (unit-level block metadata):
//! ```eu
//! { lsp-context: "data.json" }
//! ```
//!
//! Each entry is parsed with `Input::from_str()`, the same format as
//! CLI arguments. Relative filesystem paths are resolved against the
//! directory containing the file being edited.

use crate::syntax::input::Input;
use crate::syntax::rowan::ast::{
    AstToken, DeclarationKind, Element, HasSoup, LiteralValue, Soup, Unit,
};
use std::path::Path;
use std::str::FromStr;

/// Extract `lsp-context` entries from a parsed eucalypt unit.
///
/// Searches both unit-level block metadata and declaration-level
/// metadata (backtick syntax). Returns a list of `Input` values
/// parsed from the first `lsp-context` metadata found. Relative
/// filesystem paths are resolved against `base_dir` (the directory
/// containing the source file).
pub fn lsp_context_inputs(unit: &Unit, base_dir: &Path) -> Vec<Input> {
    let mut inputs = Vec::new();

    // Check unit-level block metadata (no backtick)
    if let Some(meta) = unit.meta() {
        if let Some(meta_soup) = meta.soup() {
            scrape_lsp_context_from_soup(&meta_soup, &mut inputs, base_dir);
        }
    }

    // Check declaration-level metadata (backtick syntax)
    if inputs.is_empty() {
        for decl in unit.declarations() {
            if let Some(meta) = decl.meta() {
                if let Some(meta_soup) = meta.soup() {
                    scrape_lsp_context_from_soup(&meta_soup, &mut inputs, base_dir);
                    if !inputs.is_empty() {
                        break;
                    }
                }
            }
        }
    }

    inputs
}

/// Walk a soup looking for blocks containing an `lsp-context` property.
fn scrape_lsp_context_from_soup(soup: &Soup, inputs: &mut Vec<Input>, base_dir: &Path) {
    for element in soup.elements() {
        if let Element::Block(block) = element {
            for decl in block.declarations() {
                if let Some(head) = decl.head() {
                    if let DeclarationKind::Property(prop) = head.classify_declaration() {
                        if prop.text() == "lsp-context" {
                            if let Some(body) = decl.body() {
                                if let Some(body_soup) = body.soup() {
                                    scrape_string_inputs(&body_soup, inputs, base_dir);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Extract string values from a soup and parse them as `Input` values.
///
/// Handles both a bare string literal and a list of string literals.
fn scrape_string_inputs(soup: &Soup, inputs: &mut Vec<Input>, base_dir: &Path) {
    for element in soup.elements() {
        match element {
            Element::Lit(literal) => {
                if let Some(LiteralValue::Str(s)) = literal.value() {
                    if let Some(text) = s.value() {
                        if let Ok(input) = Input::from_str(text) {
                            inputs.push(resolve_input(input, base_dir));
                        }
                    }
                }
            }
            Element::List(list) => {
                for item in list.items() {
                    scrape_string_inputs(&item, inputs, base_dir);
                }
            }
            _ => {}
        }
    }
}

/// Resolve a relative filesystem input against the base directory.
///
/// If the input's locator is a relative filesystem path, prepend the
/// base directory. Other locator types (URLs, resources, etc.) pass
/// through unchanged.
fn resolve_input(input: Input, base_dir: &Path) -> Input {
    use crate::syntax::input::Locator;

    match input.locator() {
        Locator::Fs(path) if path.is_relative() => {
            let resolved = base_dir.join(path);
            Input::new(Locator::Fs(resolved), input.name().clone(), input.format())
        }
        _ => input,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::rowan::parse_unit;
    use std::path::PathBuf;

    fn parse_and_extract(source: &str) -> Vec<Input> {
        let parse = parse_unit(source);
        let unit = parse.tree();
        lsp_context_inputs(&unit, Path::new("/test/dir"))
    }

    #[test]
    fn test_no_metadata() {
        let inputs = parse_and_extract("x: 1\ny: 2\n");
        assert!(inputs.is_empty());
    }

    #[test]
    fn test_metadata_without_lsp_context() {
        let inputs = parse_and_extract("` { doc: \"test\" }\nx: 1\n");
        assert!(inputs.is_empty());
    }

    #[test]
    fn test_single_string_backtick() {
        let inputs = parse_and_extract("` { lsp-context: \"data.json\" }\nx: 1\n");
        assert_eq!(inputs.len(), 1);
        assert_eq!(inputs[0].format(), "json");
        assert_eq!(
            *inputs[0].locator(),
            crate::syntax::input::Locator::Fs(PathBuf::from("/test/dir/data.json"))
        );
    }

    #[test]
    fn test_single_string_block_meta() {
        // Without backtick — unit-level block metadata
        let inputs = parse_and_extract("{ lsp-context: \"data.json\" }\nx: 1\n");
        assert_eq!(inputs.len(), 1);
        assert_eq!(inputs[0].format(), "json");
        assert_eq!(
            *inputs[0].locator(),
            crate::syntax::input::Locator::Fs(PathBuf::from("/test/dir/data.json"))
        );
    }

    #[test]
    fn test_list_of_strings() {
        let inputs =
            parse_and_extract("` { lsp-context: [\"base.yaml\", \"helpers.eu\"] }\nx: 1\n");
        assert_eq!(inputs.len(), 2);
        assert_eq!(inputs[0].format(), "yaml");
        assert_eq!(inputs[1].format(), "eu");
    }

    #[test]
    fn test_named_input() {
        let inputs = parse_and_extract("` { lsp-context: \"data=config.yaml\" }\nx: 1\n");
        assert_eq!(inputs.len(), 1);
        assert_eq!(inputs[0].name(), &Some("data".to_string()));
        assert_eq!(inputs[0].format(), "yaml");
    }

    #[test]
    fn test_absolute_path_not_resolved() {
        let inputs = parse_and_extract("` { lsp-context: \"/abs/path/data.json\" }\nx: 1\n");
        assert_eq!(inputs.len(), 1);
        assert_eq!(
            *inputs[0].locator(),
            crate::syntax::input::Locator::Fs(PathBuf::from("/abs/path/data.json"))
        );
    }

    #[test]
    fn test_mixed_formats() {
        let inputs = parse_and_extract(
            "` { lsp-context: [\"config.toml\", \"data=values.json\", \"utils.eu\"] }\nx: 1\n",
        );
        assert_eq!(inputs.len(), 3);
        assert_eq!(inputs[0].format(), "toml");
        assert_eq!(inputs[1].format(), "json");
        assert_eq!(inputs[1].name(), &Some("data".to_string()));
        assert_eq!(inputs[2].format(), "eu");
    }

    #[test]
    fn test_import_not_confused_with_lsp_context() {
        let inputs = parse_and_extract("` { import: \"other.eu\" }\nx: 1\n");
        assert!(inputs.is_empty());
    }
}
