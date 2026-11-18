//! Rendering documentation entries as Markdown, JSON Schema, and coverage reports.

use super::extract::{DocEntry, DocKind};
use crate::core::typecheck::{parse::parse_type, types::Type};

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
            render_children_markdown(&entry.children, &mut out, 3);
            continue;
        }

        render_entry_markdown(entry, &mut out, "###", false);
    }

    out
}

/// Render namespace members; a member that is itself a namespace block is
/// rendered with its own members nested one heading level deeper. `depth`
/// is the heading level these members would take. Markdown has six heading
/// levels, so members nested deeper than that are rendered at level six,
/// under their fully-qualified name so the nesting stays visible.
fn render_children_markdown(children: &[DocEntry], out: &mut String, depth: usize) {
    let heading = "#".repeat(depth.min(6));
    for child in children {
        if !child.is_public() {
            continue;
        }
        render_entry_markdown(child, out, &heading, depth > 6);
        render_children_markdown(&child.children, out, depth + 1);
    }
}

fn render_entry_markdown(entry: &DocEntry, out: &mut String, heading: &str, qualified: bool) {
    let name = if qualified {
        entry.qualified_name()
    } else {
        entry.name.clone()
    };
    // Function signature with params, or bare name
    let signature = match &entry.kind {
        DocKind::Function { params } if !params.is_empty() => {
            format!("`{}({})`", name, params.join(", "))
        }
        _ => format!("`{name}`"),
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

fn collect_child_schemas(children: &[DocEntry], properties: &mut Vec<String>) {
    for child in children {
        if !child.is_public() {
            continue;
        }
        if let Some(ref ty_str) = child.type_annotation {
            if let Some(schema) = type_str_to_json_schema(ty_str) {
                let name = child.qualified_name();
                properties.push(format!("    {}: {}", json_str(&name), schema));
            }
        }
        collect_child_schemas(&child.children, properties);
    }
}

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
        collect_child_schemas(&entry.children, &mut properties);
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
/// Parses the type string using the real type parser and then converts
/// the resulting `Type` to JSON Schema. Returns `None` for function types
/// and other forms not representable in JSON Schema.
fn type_str_to_json_schema(ty_str: &str) -> Option<String> {
    let ty = parse_type(ty_str.trim()).ok()?;
    type_to_json_schema(&ty)
}

/// Convert a parsed `Type` to a JSON Schema fragment string.
///
/// Returns `None` for types that have no JSON Schema representation
/// (e.g. functions, IO actions).
fn type_to_json_schema(ty: &Type) -> Option<String> {
    match ty {
        Type::String => Some(r#"{"type": "string"}"#.to_string()),
        Type::Number => Some(r#"{"type": "number"}"#.to_string()),
        Type::Bool => Some(r#"{"type": "boolean"}"#.to_string()),
        Type::Null => Some(r#"{"type": "null"}"#.to_string()),
        Type::Symbol => Some(r#"{"type": "string"}"#.to_string()),
        Type::DateTime => Some(r#"{"type": "string", "format": "date-time"}"#.to_string()),
        Type::Any | Type::Top => Some(r#"{}"#.to_string()),
        Type::Never => Some(r#"{"not": {}}"#.to_string()),
        Type::LiteralString(s) => {
            Some(format!(r#"{{"type": "string", "const": {}}}"#, json_str(s)))
        }
        Type::LiteralSymbol(s) => {
            Some(format!(r#"{{"type": "string", "const": {}}}"#, json_str(s)))
        }

        // [T] is represented as App(Con("List"), T)
        Type::App(f, inner) if matches!(f.as_ref(), Type::Con(c) if c == "List") => {
            if let Some(items) = type_to_json_schema(inner) {
                Some(format!(r#"{{"type": "array", "items": {items}}}"#))
            } else {
                Some(r#"{"type": "array"}"#.to_string())
            }
        }

        // IO(T) is not representable in JSON Schema
        Type::App(f, _) if matches!(f.as_ref(), Type::Con(c) if c == "IO") => None,

        // Tuple as fixed-length array with prefixItems
        Type::Tuple(elems) => {
            let items: Vec<String> = elems.iter().filter_map(type_to_json_schema).collect();
            if items.is_empty() {
                return Some(r#"{"type": "array"}"#.to_string());
            }
            Some(format!(
                r#"{{"type": "array", "prefixItems": [{}], "items": false}}"#,
                items.join(", ")
            ))
        }

        // Prefix-list — the JSON-Schema 2020-12 tuple-with-rest idiom: fixed
        // `prefixItems` plus an OPEN `items` schema for the homogeneous tail
        // (the tuple case above closes it with `"items": false`).
        Type::PrefixList { prefix, tail } => {
            let items: Vec<String> = prefix.iter().filter_map(type_to_json_schema).collect();
            let tail_schema = type_to_json_schema(tail).unwrap_or_else(|| "{}".to_string());
            Some(format!(
                r#"{{"type": "array", "prefixItems": [{}], "items": {}}}"#,
                items.join(", "),
                tail_schema
            ))
        }

        // Record type → object with properties
        Type::Record { fields, open, .. } => {
            if fields.is_empty() {
                if *open {
                    return Some(r#"{"type": "object"}"#.to_string());
                }
                return Some(r#"{"type": "object", "additionalProperties": false}"#.to_string());
            }
            let props: Vec<String> = fields
                .iter()
                .filter_map(|(k, fp)| {
                    type_to_json_schema(fp.ty())
                        .map(|schema| format!("{}: {}", json_str(k), schema))
                })
                .collect();
            let additional = if *open {
                String::new()
            } else {
                r#", "additionalProperties": false"#.to_string()
            };
            Some(format!(
                r#"{{"type": "object", "properties": {{{}}}{}}}"#,
                props.join(", "),
                additional
            ))
        }

        // Union → oneOf
        Type::Union(variants) => {
            let schemas: Vec<String> = variants.iter().filter_map(type_to_json_schema).collect();
            if schemas.is_empty() {
                return None;
            }
            Some(format!(r#"{{"oneOf": [{}]}}"#, schemas.join(", ")))
        }

        // Functions are not representable
        Type::Function(_, _) => None,

        // Type variables are unconstrained — map to any
        Type::Var(_, _) => Some(r#"{}"#.to_string()),

        // Quantifiers and recursive types — unwrap
        Type::Forall(_, body) | Type::Mu(_, body) => type_to_json_schema(body),

        // Other App/Con/Lam etc. — not representable
        _ => None,
    }
}

/// JSON-encode a string value (handles all escape sequences correctly).
fn json_str(s: &str) -> String {
    serde_json::to_string(s).unwrap_or_else(|_| {
        // serde_json::to_string should never fail for &str, but if it
        // somehow does, fall back to serde_json::Value for correctness.
        serde_json::Value::String(s.to_string()).to_string()
    })
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

    // Coverage counts public bindings at every depth that is rendered. A
    // non-public namespace member is not rendered, nor is anything inside
    // it, so neither is counted. At the top level, as before nested members
    // were counted, the members of a non-public namespace still count: the
    // prelude reference pages publish members of namespaces marked
    // `export: :suppress`.
    fn check(
        entry: &DocEntry,
        documented: &mut usize,
        undocumented: &mut usize,
        names: &mut Vec<String>,
    ) {
        if !entry.is_public() {
            return;
        }
        if entry.doc.is_some() {
            *documented += 1;
        } else {
            *undocumented += 1;
            names.push(entry.qualified_name());
        }
        for child in &entry.children {
            check(child, documented, undocumented, names);
        }
    }

    for entry in entries {
        if entry.is_public() {
            check(
                entry,
                &mut documented,
                &mut undocumented,
                &mut undocumented_names,
            );
        } else {
            for child in &entry.children {
                check(
                    child,
                    &mut documented,
                    &mut undocumented,
                    &mut undocumented_names,
                );
            }
        }
    }

    Coverage {
        documented,
        undocumented,
        total: documented + undocumented,
        undocumented_names,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::driver::doc::extract_doc_entries;
    use crate::syntax::rowan::parse_unit;
    use serde_json::Value;

    #[test]
    fn nested_block_members_are_documented() {
        let src = "` \"Outer ns\"\nouter: {\n  ` \"Mid ns\"\n  inner: {\n    ` \"Deep doc\"\n    g(x): x\n  }\n}\n";
        let unit = parse_unit(src).tree();
        let entries = extract_doc_entries(&unit);
        let md = render_markdown(&entries, "t", None);
        assert!(md.contains("#### `g(x)`"), "{md}");
        assert!(md.contains("Deep doc"), "{md}");
        let cov = compute_coverage(&entries);
        assert_eq!(cov.total, 3);
        assert_eq!(cov.documented, 3);
        assert_eq!(
            entries[0].children[0].children[0].qualified_name(),
            "outer.inner.g"
        );
    }

    #[test]
    fn coverage_counts_members_at_every_depth() {
        let src = "` \"A\"\na: {\n  ` \"B\"\n  b: {\n    ` \"C\"\n    c: {\n      d(x): x\n      ` \"E\"\n      e: 1\n    }\n  }\n}\n";
        let unit = parse_unit(src).tree();
        let cov = compute_coverage(&extract_doc_entries(&unit));
        assert_eq!(cov.total, 5);
        assert_eq!(cov.documented, 4);
        assert_eq!(cov.undocumented_names, vec!["a.b.c.d".to_string()]);
    }

    #[test]
    fn undocumented_sub_namespace_keeps_documented_members() {
        let src = "` \"Outer\"\nouter: {\n  inner: {\n    ` \"Kept doc\"\n    k(x): x\n  }\n  data: { plain: 1 }\n}\n";
        let unit = parse_unit(src).tree();
        let entries = extract_doc_entries(&unit);
        let md = render_markdown(&entries, "t", None);
        assert!(md.contains("#### `k(x)`"), "{md}");
        assert!(md.contains("Kept doc"), "{md}");
        assert!(!md.contains("plain"), "{md}");
        let cov = compute_coverage(&entries);
        assert_eq!(
            cov.undocumented_names,
            vec!["outer.inner".to_string(), "outer.data".to_string()]
        );
    }

    #[test]
    fn coverage_skips_inside_non_public_members() {
        // Extraction already drops the members of a non-public nested
        // block, so mark one non-public after extraction to check that
        // coverage, like rendering, does not look inside it.
        let src = "` \"Outer\"\nouter: {\n  ` \"Hid\"\n  hid: {\n    ` \"Priv\"\n    priv: {\n      ` \"A\"\n      a: 1\n      ` \"B\"\n      b: 2\n    }\n    ` \"Shown\"\n    shown: 3\n  }\n}\n";
        let unit = parse_unit(src).tree();
        let mut entries = extract_doc_entries(&unit);
        let priv_entry = &mut entries[0].children[0].children[0];
        assert_eq!(priv_entry.qualified_name(), "outer.hid.priv");
        assert_eq!(priv_entry.children.len(), 2);
        priv_entry.visibility = crate::driver::doc::DocVisibility::Internal;
        let md = render_markdown(&entries, "t", None);
        assert!(!md.contains("`a`") && !md.contains("`b`"), "{md}");
        assert_eq!(compute_coverage(&entries).total, 3);
    }

    #[test]
    fn non_public_docs_do_not_make_a_namespace() {
        let src = "` \"Outer\"\nouter: {\n  data: {\n    ` { doc: \"Hidden\" export: :internal }\n    p: 1\n  }\n}\n";
        let unit = parse_unit(src).tree();
        let entries = extract_doc_entries(&unit);
        assert!(entries[0].children[0].children.is_empty());
        let md = render_markdown(&entries, "t", None);
        assert!(!md.contains("`p`"), "{md}");
        let cov = compute_coverage(&entries);
        assert_eq!(cov.undocumented_names, vec!["outer.data".to_string()]);
    }

    #[test]
    fn deep_nesting_clamps_heading_level() {
        // Seven nested namespaces: members below level six stay at `######`.
        let mut src = String::new();
        for (i, n) in ["n1", "n2", "n3", "n4", "n5", "n6", "n7"]
            .iter()
            .enumerate()
        {
            let pad = "  ".repeat(i);
            src.push_str(&format!("{pad}` \"{n} doc\"\n{pad}{n}: {{\n"));
        }
        src.push_str(&format!(
            "{}` \"Leaf doc\"\n{}leaf: 1\n",
            "  ".repeat(7),
            "  ".repeat(7)
        ));
        for i in (0..7).rev() {
            src.push_str(&format!("{}}}\n", "  ".repeat(i)));
        }
        let unit = parse_unit(&src).tree();
        let md = render_markdown(&extract_doc_entries(&unit), "t", None);
        for h in [
            "### `n2`",
            "#### `n3`",
            "##### `n4`",
            "###### `n5`",
            "###### `n1.n2.n3.n4.n5.n6`",
            "###### `n1.n2.n3.n4.n5.n6.n7`",
            "###### `n1.n2.n3.n4.n5.n6.n7.leaf`",
        ] {
            assert!(md.contains(&format!("{h}\n")), "missing {h:?}:\n{md}");
        }
        assert!(!md.contains("#######"), "{md}");
        assert!(md.contains("Leaf doc"), "{md}");
    }

    fn schema(ty_str: &str) -> Option<Value> {
        type_str_to_json_schema(ty_str).map(|s| serde_json::from_str(&s).unwrap())
    }

    #[test]
    fn primitives() {
        assert_eq!(
            schema("string"),
            Some(serde_json::json!({"type": "string"}))
        );
        assert_eq!(
            schema("number"),
            Some(serde_json::json!({"type": "number"}))
        );
        assert_eq!(schema("bool"), Some(serde_json::json!({"type": "boolean"})));
        assert_eq!(schema("null"), Some(serde_json::json!({"type": "null"})));
        assert_eq!(
            schema("symbol"),
            Some(serde_json::json!({"type": "string"}))
        );
    }

    #[test]
    fn any_type() {
        assert_eq!(schema("any"), Some(serde_json::json!({})));
    }

    #[test]
    fn list_type() {
        assert_eq!(
            schema("[number]"),
            Some(serde_json::json!({"type": "array", "items": {"type": "number"}}))
        );
    }

    #[test]
    fn union_type() {
        let s = schema("string | number").unwrap();
        let one_of = s["oneOf"].as_array().unwrap();
        assert_eq!(one_of.len(), 2);
    }

    #[test]
    fn function_type_returns_none() {
        assert_eq!(schema("string -> number"), None);
        assert_eq!(schema("string → number"), None);
    }

    #[test]
    fn prefix_list_type() {
        // Prefix-list is the JSON-Schema 2020-12 tuple-with-rest idiom: fixed
        // `prefixItems` plus an OPEN `items` schema for the homogeneous tail.
        let s = schema("[symbol, number, string…]").unwrap();
        assert_eq!(s["type"], "array");
        let prefix = s["prefixItems"].as_array().unwrap();
        assert_eq!(prefix.len(), 2);
        assert_eq!(prefix[0], serde_json::json!({"type": "string"}));
        assert_eq!(prefix[1], serde_json::json!({"type": "number"}));
        // The tail is open (a schema), unlike a tuple which closes with `false`.
        assert_eq!(s["items"], serde_json::json!({"type": "string"}));
    }

    #[test]
    fn tuple_closes_items_prefix_list_opens_them() {
        // A fixed tuple closes the array (`"items": false`); a prefix-list
        // leaves it open with the tail schema.
        let tuple = schema("(symbol, number)").unwrap();
        assert_eq!(tuple["items"], serde_json::json!(false));
        let plist = schema("[symbol, number…]").unwrap();
        assert_eq!(plist["items"], serde_json::json!({"type": "number"}));
    }

    #[test]
    fn record_type() {
        let s = schema("{x: number, y: string}").unwrap();
        assert_eq!(s["type"], "object");
        assert!(s["properties"]["x"].is_object());
        assert!(s["properties"]["y"].is_object());
    }

    #[test]
    fn datetime_type() {
        assert_eq!(
            schema("datetime"),
            Some(serde_json::json!({"type": "string", "format": "date-time"}))
        );
    }

    #[test]
    fn never_type() {
        assert_eq!(schema("never"), Some(serde_json::json!({"not": {}})));
    }

    #[test]
    fn literal_string_type() {
        let s = schema(r#""hello""#).unwrap();
        assert_eq!(s["type"], "string");
        assert_eq!(s["const"], "hello");
    }
}
