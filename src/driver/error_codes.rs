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
     and the value being rendered does not have it. html output renders\n\
     hiccup markup - a list whose first item is the tag, optionally\n\
     followed by an attribute block and then contents - so rendering a\n\
     block, or a bare scalar, has no meaning as html.\n\
     \n\
     Example:\n\
     \n\
     \x20   ` { target: :main format: :html }\n\
     \x20   main: { a: 1 b: 2 }\n\
     \n\
     \x20   error[EU-RENDER-SHAPE]: cannot render this document as html:\n\
     \x20   the value to render is a block, but markup output needs a\n\
     \x20   hiccup element - a list whose first item is the tag\n\
     \n\
     How to fix it: build the value as hiccup markup, e.g.\n\
     [:div, { id: \"top\" }, \"hello\"], and select it with a target or -e so\n\
     that html is given the markup itself rather than the enclosing unit's\n\
     block. To render arbitrary data instead, choose a format that accepts\n\
     any shape, such as yaml, json or text.\n\
     \n\
     This diagnostic often has no source location: the document's root\n\
     events are emitted by the render pipeline rather than by a user\n\
     expression, so there is frequently no span to point at.",
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
}
