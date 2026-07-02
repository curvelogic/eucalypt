//! Bytecode opcodes and operand-encoding constants (BV1).
//!
//! One `u8` opcode per `StgSyn` variant (spec §4.2). Unlike the BV0
//! spike — which split `Atom` into `ATOM_L/ATOM_G/ATOM_V` — BV1 uses a
//! single `Op::Atom` followed by an inline `Ref` (tag byte + payload),
//! so the ref-tag vocabulary is shared with every other operand site.

/// A bytecode opcode: one per `StgSyn` variant.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Atom = 0x01,
    Case = 0x02,
    Cons = 0x03,
    App = 0x04,
    DirectApp = 0x05,
    Bif = 0x06,
    Let = 0x07,
    LetRec = 0x08,
    Ann = 0x09,
    Meta = 0x0A,
    DeMeta = 0x0B,
    Seq = 0x0C,
    LookupLit = 0x0D,
    BlackHole = 0x0E,
}

impl Op {
    /// Decode an opcode byte, returning `None` for an unknown byte.
    pub fn from_u8(b: u8) -> Option<Op> {
        match b {
            0x01 => Some(Op::Atom),
            0x02 => Some(Op::Case),
            0x03 => Some(Op::Cons),
            0x04 => Some(Op::App),
            0x05 => Some(Op::DirectApp),
            0x06 => Some(Op::Bif),
            0x07 => Some(Op::Let),
            0x08 => Some(Op::LetRec),
            0x09 => Some(Op::Ann),
            0x0A => Some(Op::Meta),
            0x0B => Some(Op::DeMeta),
            0x0C => Some(Op::Seq),
            0x0D => Some(Op::LookupLit),
            0x0E => Some(Op::BlackHole),
            _ => None,
        }
    }
}

// ── Inline ref tag bytes ────────────────────────────────────────────
//
// A `Ref` operand is a tag byte followed by a `u32` payload: a local
// de Bruijn index (`REF_L`), a global slot index (`REF_G`), or an index
// into the constant pool (`REF_V`).

/// Local (de Bruijn) reference: payload is a `u32` environment index.
pub const REF_L: u8 = 0x00;
/// Global reference: payload is a `u32` global slot index.
pub const REF_G: u8 = 0x01;
/// Value reference: payload is a `u32` constant-pool index.
pub const REF_V: u8 = 0x02;

// ── Flag bits ───────────────────────────────────────────────────────

/// `OP_APP`/`OP_DIRECT_APP` flag bit: resolve `Ref::L` args eagerly
/// (CG3 `eager_args`).
pub const FLAG_EAGER: u8 = 0b0000_0001;

/// Sentinel entry in a densified `OP_CASE` branch table meaning "no
/// branch for this tag". Real branch offsets are always `< code.len()`,
/// so `u32::MAX` never collides with a valid offset.
pub const NO_BRANCH: u32 = u32::MAX;

// ── Lambda-form kind bytes ──────────────────────────────────────────

/// A lambda with arity ≥ 1 (carries an annotation).
pub const FORM_LAMBDA: u8 = 0x00;
/// An updatable thunk (arity 0).
pub const FORM_THUNK: u8 = 0x01;
/// A non-updatable value (arity 0).
pub const FORM_VALUE: u8 = 0x02;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn op_roundtrips_through_u8() {
        let all = [
            Op::Atom,
            Op::Case,
            Op::Cons,
            Op::App,
            Op::DirectApp,
            Op::Bif,
            Op::Let,
            Op::LetRec,
            Op::Ann,
            Op::Meta,
            Op::DeMeta,
            Op::Seq,
            Op::LookupLit,
            Op::BlackHole,
        ];
        for op in all {
            assert_eq!(Op::from_u8(op as u8), Some(op));
        }
    }

    #[test]
    fn unknown_byte_decodes_to_none() {
        assert_eq!(Op::from_u8(0x00), None);
        assert_eq!(Op::from_u8(0xFF), None);
    }
}
