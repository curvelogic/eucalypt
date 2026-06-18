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
                .filter_map(|(k, v)| {
                    type_to_json_schema(v).map(|schema| format!("{}: {}", json_str(k), schema))
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

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

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
