//! Fuzz target for the eucalypt type-DSL parser.
//!
//! Entry point: `typecheck::parse::parse_type(&input)`.
//!
//! The type-DSL parser processes annotation strings written by users in
//! `{ type: "..." }` metadata blocks.  A panic here crashes the CLI on
//! `eu check` and the LSP on hover.  The expected contract is: return
//! `Ok(Type)` or `Err(ParseError)`, never panic.
#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Must not panic — only Ok or Err are acceptable.
        let _ = eucalypt::core::typecheck::parse::parse_type(s);
    }
});
