//! Bytecode opcode definitions.
//!
//! Each opcode is a `u8` constant matching one `HeapSyn` variant.
//! Refs are encoded inline with a tag byte + value.

// ── Opcodes ─────────────────────────────────────────────────────────

pub const ATOM_L: u8 = 0x01;
pub const ATOM_G: u8 = 0x02;
pub const ATOM_V: u8 = 0x03;
pub const APP: u8 = 0x04;
pub const DIRECT_APP: u8 = 0x05;
pub const BIF: u8 = 0x06;
pub const CASE: u8 = 0x07;
pub const CONS: u8 = 0x08;
pub const LET: u8 = 0x09;
pub const LETREC: u8 = 0x0A;
pub const ANN: u8 = 0x0B;
pub const META: u8 = 0x0C;
pub const DEMETA: u8 = 0x0D;
pub const SEQ: u8 = 0x0E;
pub const LOOKUP_LIT: u8 = 0x0F;
pub const BLACKHOLE: u8 = 0x10;

// ── Inline ref tag bytes ────────────────────────────────────────────

pub const REF_L: u8 = 0x00;
pub const REF_G: u8 = 0x01;
pub const REF_V: u8 = 0x02;

// ── Lambda form kind bytes ──────────────────────────────────────────

pub const FORM_LAMBDA: u8 = 0x00;
pub const FORM_THUNK: u8 = 0x01;
pub const FORM_VALUE: u8 = 0x02;
