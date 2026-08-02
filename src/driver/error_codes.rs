//! `eu error <CODE>` — prints the catalogue entry for a stable error code.
//!
//! The catalogue mirrors `docs/reference/error-codes.md`; when a new code
//! is assigned (see `ExecutionError::code` and its siblings), add an entry
//! both here and in that file so the two stay in sync.

use crate::driver::options::EucalyptOptions;

/// `(code, catalogue entry)`. The entry is plain text, ready to print to a
/// terminal as-is (no markdown rendering).
const CATALOGUE: &[(&str, &str)] = &[
    (
        "EU-EVAL-TYPE",
        "EU-EVAL-TYPE: type mismatch\n\
     \n\
     A value of one type was used where a different type was required — for\n\
     example, a string was passed to an arithmetic operator that expects a\n\
     number, or a number was used where a block was expected. This is a\n\
     runtime error: the mismatch was only discovered when the value was\n\
     evaluated.\n\
     \n\
     Example:\n\
     \n\
     \x20   a: \"3\"\n\
     \x20   result: a + 1\n\
     \n\
     \x20   error[EU-EVAL-TYPE]: type mismatch: expected number, found string \"3\"\n\
     \n\
     How to fix it: convert the value to the type the operator expects\n\
     before using it — for example, `a num + 1` converts the string to a\n\
     number first. See docs/reference/error-codes.md for more detail and\n\
     the prelude reference for the conversion functions available for each\n\
     type (num, str, str.of, and so on).",
    ),
    (
        "EU-EVAL-CONTRACT",
        "EU-EVAL-CONTRACT: contract violation\n\
     \n\
     Data failed a structural contract applied with `ensure`. The contract\n\
     is an ordinary type spec written as an s-string literal, and the\n\
     diagnostic's notes list every position that did not conform, each with\n\
     a path into the data.\n\
     \n\
     Example:\n\
     \n\
     \x20   { import: \"contract.eu\" }\n\
     \x20   schema: s\"{ name: string, port: number }\"\n\
     \x20   config: { name: \"web\", port: \"8080\" } ensure(schema)\n\
     \n\
     \x20   error[EU-EVAL-CONTRACT]: contract violation: 1 violation against \
     {name: string, port: number}\n\
     \x20     = port: expected number, found string\n\
     \n\
     How to fix it: correct the data at each path the notes name, or widen\n\
     the spec if the shape you are receiving is the shape you intended. A\n\
     closed record spec reports surplus keys; add `..` to permit them. To\n\
     inspect the violations as data rather than aborting, call\n\
     `validate(spec, data)` — it returns the same information as a list of\n\
     blocks and never raises.",
    ),
    (
        "EU-RENDER-UNREPRESENTABLE",
        "EU-RENDER-UNREPRESENTABLE: value cannot be represented in the output format\n\
     \n\
     A value survived evaluation intact but the requested output format has\n\
     no way to carry it. The commonest case is an integer above\n\
     9223372036854775807: JSON, EDN and eucalypt itself carry it happily,\n\
     but a YAML integer scalar and a TOML integer are both signed 64-bit.\n\
     TOML also has no null, so a null cannot be rendered as TOML either.\n\
     \n\
     Example:\n\
     \n\
     \x20   # big.json holds {\"n\": 9999999999999999999}\n\
     \x20   main: data.n\n\
     \n\
     \x20   error[EU-RENDER-UNREPRESENTABLE]: cannot represent this value in\n\
     \x20   YAML output: the integer 9999999999999999999 is above\n\
     \x20   9223372036854775807, the largest integer a YAML integer scalar\n\
     \x20   can carry\n\
     \n\
     How to fix it: render to a format that can carry the value (json, edn,\n\
     text and eu output all keep integers of this magnitude), or convert it\n\
     to a string first with `str` so the exact digits are preserved as text.\n\
     For a null being rendered as TOML, give the key a value or drop it from\n\
     the block. eucalypt reports this rather than quietly rounding the value\n\
     or changing its type, so what you export matches what you evaluated.",
    ),
    (
        "EU-RENDER-SHAPE",
        "EU-RENDER-SHAPE: document shape not accepted by the output format\n\
     \n\
     The output format needs the document to have a particular structure,\n\
     and the value being rendered does not have it. Two cases raise it.\n\
     \n\
     Whole-document shape. html output renders hiccup markup - a list\n\
     whose first item is the tag, optionally followed by an attribute\n\
     block and then contents - so rendering a block, or a bare scalar,\n\
     has no meaning as html.\n\
     \n\
     Block keys. json, toml and eu output all need block keys to be\n\
     text: JSON object keys and TOML keys are strings, and eucalypt's\n\
     own syntax needs a name. Block literals always have symbol keys,\n\
     but kv-block and block accept any key at all, so kv-block(1, \"a\")\n\
     - or a boolean, list or block key - produces a block those three\n\
     formats cannot render. yaml and edn both represent such keys (1: a\n\
     and {1 \"a\"} respectively) and render the block unchanged.\n\
     \n\
     Example (document shape):\n\
     \n\
     \x20   ` { target: :main format: :html }\n\
     \x20   main: { a: 1 b: 2 }\n\
     \n\
     \x20   error[EU-RENDER-SHAPE]: cannot render this document as html:\n\
     \x20   the value to render is a block, but markup output needs a\n\
     \x20   hiccup element - a list whose first item is the tag\n\
     \n\
     Example (block key):\n\
     \n\
     \x20   ` { target: :main format: :json }\n\
     \x20   main: kv-block(1, \"a\")\n\
     \n\
     \x20   error[EU-RENDER-SHAPE]: cannot render this document as JSON:\n\
     \x20   a block key is the number 1, but JSON object keys must be text\n\
     \n\
     How to fix it: for a shape mismatch, build the value as hiccup\n\
     markup, e.g. [:div, { id: \"top\" }, \"hello\"], and select it with a\n\
     target or -e so that html is given the markup itself rather than\n\
     the enclosing unit's block. For a block key, give the block symbol\n\
     keys - e.g. kv-block(:one, \"a\") - or convert the key to text with\n\
     str before rendering. In either case you can instead choose a\n\
     format that accepts the document as it stands: yaml, json or text\n\
     for arbitrary shapes, and yaml or edn for keys that are not text.\n\
     \n\
     eucalypt reports this rather than coercing the value - a key\n\
     rendered as \"1\" is not the key that was evaluated, and the\n\
     difference survives into anything that re-reads the output.\n\
     \n\
     This diagnostic often has no source location: the document's root\n\
     events are emitted by the render pipeline rather than by a user\n\
     expression, and a literal block key records no annotation, so\n\
     there is frequently no span to point at.",
    ),
];

/// Look up a code's catalogue entry.
fn catalogue_entry(code: &str) -> Option<&'static str> {
    CATALOGUE
        .iter()
        .find(|(c, _)| *c == code)
        .map(|(_, entry)| *entry)
}

/// Run `eu error <CODE>`: print the catalogue entry for `opt.error_code()`
/// to stdout and return exit code 0, or print an "unknown error code"
/// message and return a non-zero exit code.
pub fn run(opt: &EucalyptOptions) -> Result<i32, String> {
    let code = opt
        .error_code()
        .ok_or_else(|| "eu error: no error code given".to_string())?;

    match catalogue_entry(code) {
        Some(entry) => {
            println!("{entry}");
            Ok(0)
        }
        None => {
            eprintln!("eu error: unknown error code '{code}'");
            eprintln!("see docs/reference/error-codes.md for the full catalogue");
            Ok(1)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn known_code_has_an_entry() {
        let entry = catalogue_entry("EU-EVAL-TYPE").expect("EU-EVAL-TYPE must be catalogued");
        assert!(entry.to_lowercase().contains("type"));
    }

    #[test]
    fn unknown_code_has_no_entry() {
        assert!(catalogue_entry("EU-NOT-A-REAL-CODE").is_none());
    }

    /// This `CATALOGUE` and `docs/reference/error-codes.md` are maintained
    /// by hand, in two separate files, with no compiler check tying them
    /// together — that gap is exactly how `EU-RENDER-SHAPE` drifted (eu-1z503
    /// extended the code to cover non-text block keys but only one of the two
    /// catalogues was updated, eu-1tkk.7.46). This cannot catch stale prose —
    /// the two files intentionally differ in form (docs is markdown with
    /// links and codespan-quoted examples; this one is plain text meant to
    /// print straight to a terminal) — but it does catch the structural half
    /// of the problem: a code documented in one file and not the other. Every
    /// `### \`EU-...\`` heading in the docs file must have a matching
    /// `CATALOGUE` entry, and vice versa.
    #[test]
    fn catalogue_code_set_matches_docs() {
        use std::collections::BTreeSet;

        let docs_path = concat!(env!("CARGO_MANIFEST_DIR"), "/docs/reference/error-codes.md");
        let docs = std::fs::read_to_string(docs_path)
            .unwrap_or_else(|e| panic!("failed to read {docs_path}: {e}"));

        let doc_codes: BTreeSet<&str> = docs
            .lines()
            .filter_map(|line| {
                line.trim()
                    .strip_prefix("### `")
                    .and_then(|rest| rest.strip_suffix('`'))
            })
            .collect();
        assert!(
            !doc_codes.is_empty(),
            "found no '### `EU-...`' headings in {docs_path} — the heading \
             format this test looks for may have changed"
        );

        let catalogue_codes: BTreeSet<&str> = CATALOGUE.iter().map(|(code, _)| *code).collect();

        let docs_only: Vec<_> = doc_codes.difference(&catalogue_codes).collect();
        let catalogue_only: Vec<_> = catalogue_codes.difference(&doc_codes).collect();
        assert!(
            docs_only.is_empty() && catalogue_only.is_empty(),
            "docs/reference/error-codes.md and src/driver/error_codes.rs::CATALOGUE \
             must document the same set of codes.\n\
             In docs but missing from CATALOGUE: {docs_only:?}\n\
             In CATALOGUE but missing from docs: {catalogue_only:?}"
        );
    }
}
