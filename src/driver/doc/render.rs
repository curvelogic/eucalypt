//! Rendering documentation entries as Markdown, JSON Schema, and coverage reports.

use super::extract::{DocEntry, DocKind};

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
