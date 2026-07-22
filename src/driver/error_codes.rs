//! `eu error <CODE>` — prints the catalogue entry for a stable error code.
//!
//! The catalogue mirrors `docs/reference/error-codes.md`; when a new code
//! is assigned (see `ExecutionError::code` and its siblings), add an entry
//! both here and in that file so the two stay in sync.

use crate::driver::options::EucalyptOptions;

/// `(code, catalogue entry)`. The entry is plain text, ready to print to a
/// terminal as-is (no markdown rendering).
const CATALOGUE: &[(&str, &str)] = &[(
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
)];

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
