//! Multi-file categorised prelude reference generation.
//!
//! Flattens `DocEntry` trees into table-friendly `FlatEntry` values,
//! groups them by category, and renders per-category markdown pages
//! with supplement files and an index page.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

use crate::driver::error::EucalyptError;

use super::extract::{DocEntry, DocKind, DocVisibility};

// ── Flat entry model ─────────────────────────────────────────────────────────

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

// Namespaces are detected dynamically: any entry with children is treated
// as a namespace block. No hard-coded list needed.

// ── Category mapping ─────────────────────────────────────────────────────────

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

// ── Flattening ───────────────────────────────────────────────────────────────

/// Collected documentation for namespace blocks, keyed by namespace name.
///
/// When `flatten_entries` expands a namespace block, the namespace's own
/// docstring is stored here so it can be rendered as a section intro in
/// category pages.
type NamespaceDocs = HashMap<String, String>;

/// Flatten `DocEntry` trees into `FlatEntry` values with namespace context.
///
/// Top-level entries with children (namespace blocks) are expanded.
/// Namespace-level docstrings are collected into `ns_docs` so they can be
/// rendered as introductory text in category pages.
fn flatten_entries(entries: &[DocEntry]) -> (Vec<FlatEntry>, NamespaceDocs) {
    let mut flat: Vec<FlatEntry> = Vec::new();
    let mut ns_docs: NamespaceDocs = HashMap::new();

    for entry in entries {
        let ns = entry.namespace.as_deref().unwrap_or("").to_string();
        let is_ns_block = !entry.children.is_empty();

        if is_ns_block {
            // Preserve the namespace's own docstring
            if let Some(ref doc) = entry.doc {
                if !doc.is_empty() {
                    ns_docs.insert(entry.name.clone(), doc.clone());
                }
            }
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

    (flat, ns_docs)
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

// ── Description formatting ───────────────────────────────────────────────────

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

/// Strip a leading dash separator (`-`, `—`, `–`) from a string, returning
/// the remainder.  Returns `None` if the string does not start with a dash.
///
/// This handles multi-byte em-dash (U+2014) and en-dash (U+2013) correctly
/// by advancing past the full character rather than a single byte.
fn strip_dash_prefix(s: &str) -> Option<&str> {
    let mut chars = s.chars();
    match chars.next()? {
        '-' | '\u{2014}' | '\u{2013}' => Some(chars.as_str()),
        _ => None,
    }
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
        if let Some(after) = strip_dash_prefix(after) {
            return after.trim_start().to_string();
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
            if let Some(after) = strip_dash_prefix(after) {
                let after = after.trim_start();
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

// ── Category / section configuration ─────────────────────────────────────────

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
        } else if !slug.ends_with('-') {
            slug.push('-');
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

// ── Category page data ───────────────────────────────────────────────────────

/// Build the section configuration for a given category.
///
/// Returns `(title, sections)` or `None` for unknown categories.
fn category_sections(category: &str) -> Option<(&'static str, Vec<SectionConfig<'static>>)> {
    Some(match category {
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
        _ => return None,
    })
}

// ── Page generation ──────────────────────────────────────────────────────────

/// Generate a complete markdown page for one category.
fn generate_category_page(
    category: &str,
    all_entries: &[&FlatEntry],
    supplements_dir: &Path,
    ns_docs: &NamespaceDocs,
) -> String {
    let (title, sections) = match category_sections(category) {
        Some(cs) => cs,
        None => return String::new(),
    };

    let mut parts: Vec<String> = vec![format!("# {title}")];

    // Top-level supplement (preamble)
    let top_supp = load_supplement(category, "top", supplements_dir);
    if !top_supp.is_empty() {
        parts.push(String::new());
        parts.push(top_supp);
    }

    // Track which entries have been used (by index in all_entries)
    let mut used: HashSet<usize> = HashSet::new();

    for section_cfg in &sections {
        let mut candidates = entries_for_section(all_entries, &section_cfg.source);

        // Filter to public entries only
        candidates.retain(|e| !e.is_suppressed);

        // Deduplicate and track usage by index (not pointer)
        let mut seen_names: HashSet<&str> = HashSet::new();
        let mut unique: Vec<&FlatEntry> = Vec::new();
        for e in candidates {
            let idx = entry_index(all_entries, e);
            if !used.contains(&idx) && seen_names.insert(&e.name) {
                used.insert(idx);
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

        // If all entries in this section share a single namespace that has
        // a docstring, render it as a section introduction.
        if !unique.is_empty() {
            let first_ns = &unique[0].namespace;
            if !first_ns.is_empty() && unique.iter().all(|e| &e.namespace == first_ns) {
                if let Some(ns_doc) = ns_docs.get(first_ns.as_str()) {
                    parts.push(format!("> {ns_doc}"));
                    parts.push(String::new());
                }
            }
        }

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
        .enumerate()
        .filter(|(i, e)| !e.is_suppressed && !used.contains(i))
        .map(|(_, e)| *e)
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

/// Find the index of an entry in the slice (by pointer identity).
fn entry_index(all: &[&FlatEntry], entry: &FlatEntry) -> usize {
    all.iter()
        .position(|e| std::ptr::eq(*e, entry))
        .unwrap_or(usize::MAX)
}

/// Generate the prelude index page.
///
/// If `unit_doc` is provided it is rendered as the page description;
/// otherwise a default description is used.
fn generate_index_page(
    by_category: &HashMap<&'static str, Vec<&FlatEntry>>,
    unit_doc: Option<&str>,
    supplements_dir: &Path,
) -> String {
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

    let description = if let Some(doc) = unit_doc {
        doc.to_string()
    } else {
        "The eucalypt **prelude** is a standard library of functions, operators,\n\
         and constants that is automatically loaded before your code runs.\n\n\
         You can suppress the prelude with `-Q` if needed, though this leaves\n\
         a very bare environment (even `true`, `false`, and `if` are defined\n\
         in the prelude)."
            .to_string()
    };

    let mut parts = vec![
        "# Prelude Reference".to_string(),
        String::new(),
        description,
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

    // Footer supplement (e.g. links to bundled standard libraries that are not
    // part of the prelude). Lives in `supplements/index/footer.md` so the
    // content survives `eu doc` regeneration and the freshness check.
    let footer = load_supplement("index", "footer", supplements_dir);
    if !footer.is_empty() {
        parts.push(String::new());
        parts.push(footer);
    }

    parts.push(String::new());

    parts.join("\n")
}

// ── Public entry point ───────────────────────────────────────────────────────

/// Preferred display order for categories. Categories not in this list
/// are appended alphabetically after the known ones.
const CATEGORY_ORDER: &[&str] = &[
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

/// Generate multi-file categorised prelude reference in `output_dir`.
///
/// Supplement files are loaded from `<output_dir>/../supplements/` (i.e. the
/// `supplements/` sibling of the output directory) if it exists, otherwise no
/// supplements are merged.
pub fn render_prelude_multifile(
    entries: &[DocEntry],
    output_dir: &Path,
    supplements_dir: &Path,
    unit_doc: Option<&str>,
) -> Result<i32, EucalyptError> {
    fs::create_dir_all(output_dir).map_err(|e| {
        EucalyptError::FileCouldNotBeWritten(output_dir.display().to_string(), Some(e.to_string()))
    })?;

    let (flat, ns_docs) = flatten_entries(entries);
    let by_category = group_by_category(&flat);

    // Build the category list from actual data, ordered by CATEGORY_ORDER
    // with any new categories appended alphabetically.
    let mut categories: Vec<&str> = CATEGORY_ORDER
        .iter()
        .filter(|c| by_category.contains_key(**c))
        .copied()
        .collect();
    let mut extra: Vec<&str> = by_category
        .keys()
        .copied()
        .filter(|k| !CATEGORY_ORDER.contains(k))
        .collect();
    extra.sort();
    categories.extend(extra);

    for cat in &categories {
        let cat_entries: Vec<&FlatEntry> = by_category
            .get(cat)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
            .to_vec();
        let public_count = cat_entries.iter().filter(|e| !e.is_suppressed).count();
        let page = generate_category_page(cat, &cat_entries, supplements_dir, &ns_docs);
        let out_path = output_dir.join(format!("{cat}.md"));
        fs::write(&out_path, &page).map_err(|e| {
            EucalyptError::FileCouldNotBeWritten(
                out_path.display().to_string(),
                Some(e.to_string()),
            )
        })?;
        eprintln!(
            "  Generated {} ({public_count} public entries)",
            out_path.file_name().and_then(|n| n.to_str()).unwrap_or(cat)
        );
    }

    // Generate index
    let index_page = generate_index_page(&by_category, unit_doc, supplements_dir);
    let index_path = output_dir.join("index.md");
    fs::write(&index_path, &index_page).map_err(|e| {
        EucalyptError::FileCouldNotBeWritten(index_path.display().to_string(), Some(e.to_string()))
    })?;
    eprintln!("  Generated index.md");
    eprintln!("\nDone. Generated pages in {}", output_dir.display());

    Ok(0)
}
