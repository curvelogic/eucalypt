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
     9223372036854775807: JSON and eucalypt itself carry it happily, but a\n\
     YAML integer scalar and a TOML integer are both signed 64-bit.\n\
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
     How to fix it: render to a format that can carry the value (json, text\n\
     and eu output all keep integers of this magnitude), or convert it to a\n\
     string first with `str` so the exact digits are preserved as text.\n\
     eucalypt reports this rather than quietly rounding the value or\n\
     changing its type, so that what you export matches what you evaluated.",
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
