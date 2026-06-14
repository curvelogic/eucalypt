//! Fuzz target for the eucalypt Rowan parser.
//!
//! Entry point: `syntax::rowan::parse_unit(text)`.
//!
//! The parser is the largest hand-written component and must handle arbitrary
//! input without panicking.  With error recovery in place, every input should
//! produce a `SyntaxTree` (possibly containing error nodes) rather than
//! a panic.  Any panic is a denial-of-service in the LSP and a crash in the
//! CLI.
#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Must not panic — every input must produce a parse result.
        let _ = eucalypt::syntax::rowan::parse_unit(s);
    }
});
