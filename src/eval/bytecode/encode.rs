//! Encode `StgSyn` trees into a flat bytecode stream.
//!
//! Uses post-order encoding: children are emitted first, then the
//! parent references them by absolute `u32` offset.  This means leaf
//! nodes appear at the lowest offsets and the root is emitted last.

use std::rc::Rc;

use crate::{
    common::sourcemap::Smid,
    eval::stg::syntax::{LambdaForm, Native as StgNative, Ref, StgSyn},
};

use super::{opcode, program::BytecodeProgram};

/// Encoder state: accumulates bytes and constants.
struct Encoder {
    code: Vec<u8>,
    constants: Vec<StgNative>,
}

impl Encoder {
    fn new() -> Self {
        Encoder {
            code: Vec::with_capacity(4096),
            constants: Vec::new(),
        }
    }

    // ── Emit primitives ─────────────────────────────────────────────

    fn emit_u8(&mut self, v: u8) {
        self.code.push(v);
    }

    fn emit_u16(&mut self, v: u16) {
        self.code.extend_from_slice(&v.to_le_bytes());
    }

    fn emit_u32(&mut self, v: u32) {
        self.code.extend_from_slice(&v.to_le_bytes());
    }

    /// Add a native constant to the pool and return its index.
    fn add_constant(&mut self, n: &StgNative) -> u32 {
        // Dedup is not critical for the spike
        let idx = self.constants.len() as u32;
        self.constants.push(n.clone());
        idx
    }

    /// Emit an inline ref (tag byte + value).
    fn emit_ref(&mut self, r: &Ref) {
        match r {
            Ref::L(i) => {
                self.emit_u8(opcode::REF_L);
                self.emit_u16(*i as u16);
            }
            Ref::G(i) => {
                self.emit_u8(opcode::REF_G);
                self.emit_u16(*i as u16);
            }
            Ref::V(n) => {
                self.emit_u8(opcode::REF_V);
                let ci = self.add_constant(n);
                self.emit_u32(ci);
            }
        }
    }

    // ── Node encoding (post-order) ──────────────────────────────────

    /// Encode a `StgSyn` node, returning its start offset in the code buffer.
    ///
    /// Children are encoded first (post-order), so offsets are known
    /// before the parent is emitted.
    fn encode_node(&mut self, node: &Rc<StgSyn>) -> u32 {
        match &**node {
            StgSyn::Atom { evaluand } => {
                let offset = self.code.len() as u32;
                match evaluand {
                    Ref::L(i) => {
                        self.emit_u8(opcode::ATOM_L);
                        self.emit_u16(*i as u16);
                    }
                    Ref::G(i) => {
                        self.emit_u8(opcode::ATOM_G);
                        self.emit_u16(*i as u16);
                    }
                    Ref::V(n) => {
                        self.emit_u8(opcode::ATOM_V);
                        let ci = self.add_constant(n);
                        self.emit_u32(ci);
                    }
                }
                offset
            }

            StgSyn::Case {
                scrutinee,
                branches,
                fallback,
            } => {
                // Encode children first
                let scr_off = self.encode_node(scrutinee);
                let branch_data: Vec<(u8, u32)> = branches
                    .iter()
                    .map(|(tag, body)| (*tag, self.encode_node(body)))
                    .collect();
                let fb_off = fallback.as_ref().map(|fb| self.encode_node(fb));

                let offset = self.code.len() as u32;
                self.emit_u8(opcode::CASE);
                self.emit_u32(scr_off);
                self.emit_u8(branch_data.len() as u8);
                for (tag, off) in &branch_data {
                    self.emit_u8(*tag);
                    self.emit_u32(*off);
                }
                match fb_off {
                    Some(off) => {
                        self.emit_u8(1);
                        self.emit_u32(off);
                    }
                    None => {
                        self.emit_u8(0);
                    }
                }
                offset
            }

            StgSyn::Cons { tag, args } => {
                let offset = self.code.len() as u32;
                self.emit_u8(opcode::CONS);
                self.emit_u8(*tag);
                self.emit_u8(args.len() as u8);
                for r in args {
                    self.emit_ref(r);
                }
                offset
            }

            StgSyn::App { callable, args } => {
                let offset = self.code.len() as u32;
                self.emit_u8(opcode::APP);
                self.emit_ref(callable);
                self.emit_u8(args.len() as u8);
                for r in args {
                    self.emit_ref(r);
                }
                offset
            }

            StgSyn::DirectApp {
                smid,
                callable,
                args,
            } => {
                let offset = self.code.len() as u32;
                self.emit_u8(opcode::DIRECT_APP);
                let smid_val: u32 = (*smid).into();
                self.emit_u32(smid_val);
                self.emit_ref(callable);
                self.emit_u8(args.len() as u8);
                for r in args {
                    self.emit_ref(r);
                }
                offset
            }

            StgSyn::Bif { intrinsic, args } => {
                let offset = self.code.len() as u32;
                self.emit_u8(opcode::BIF);
                self.emit_u8(*intrinsic);
                self.emit_u8(args.len() as u8);
                for r in args {
                    self.emit_ref(r);
                }
                offset
            }

            StgSyn::Let { bindings, body } => {
                // Encode binding bodies and the let body first
                let form_data: Vec<(u8, u8, u32, u32)> = bindings
                    .iter()
                    .map(|lf| {
                        let body_off = self.encode_node(lf.body());
                        let (kind, arity, smid) = classify_lambda_form(lf);
                        let smid_val: u32 = smid.into();
                        (kind, arity, smid_val, body_off)
                    })
                    .collect();
                let body_off = self.encode_node(body);

                let offset = self.code.len() as u32;
                self.emit_u8(opcode::LET);
                self.emit_u16(form_data.len() as u16);
                for (kind, arity, smid, boff) in &form_data {
                    self.emit_u8(*kind);
                    self.emit_u8(*arity);
                    self.emit_u32(*smid);
                    self.emit_u32(*boff);
                }
                self.emit_u32(body_off);
                offset
            }

            StgSyn::LetRec { bindings, body } => {
                let form_data: Vec<(u8, u8, u32, u32)> = bindings
                    .iter()
                    .map(|lf| {
                        let body_off = self.encode_node(lf.body());
                        let (kind, arity, smid) = classify_lambda_form(lf);
                        let smid_val: u32 = smid.into();
                        (kind, arity, smid_val, body_off)
                    })
                    .collect();
                let body_off = self.encode_node(body);

                let offset = self.code.len() as u32;
                self.emit_u8(opcode::LETREC);
                self.emit_u16(form_data.len() as u16);
                for (kind, arity, smid, boff) in &form_data {
                    self.emit_u8(*kind);
                    self.emit_u8(*arity);
                    self.emit_u32(*smid);
                    self.emit_u32(*boff);
                }
                self.emit_u32(body_off);
                offset
            }

            StgSyn::Ann { smid, body } => {
                let body_off = self.encode_node(body);
                let offset = self.code.len() as u32;
                self.emit_u8(opcode::ANN);
                let smid_val: u32 = (*smid).into();
                self.emit_u32(smid_val);
                self.emit_u32(body_off);
                offset
            }

            StgSyn::Meta { meta, body } => {
                let offset = self.code.len() as u32;
                self.emit_u8(opcode::META);
                self.emit_ref(meta);
                self.emit_ref(body);
                offset
            }

            StgSyn::DeMeta {
                scrutinee,
                handler,
                or_else,
            } => {
                let scr_off = self.encode_node(scrutinee);
                let handler_off = self.encode_node(handler);
                let or_else_off = self.encode_node(or_else);

                let offset = self.code.len() as u32;
                self.emit_u8(opcode::DEMETA);
                self.emit_u32(scr_off);
                self.emit_u32(handler_off);
                self.emit_u32(or_else_off);
                offset
            }

            StgSyn::Seq { scrutinee, body } => {
                let scr_off = self.encode_node(scrutinee);
                let body_off = self.encode_node(body);

                let offset = self.code.len() as u32;
                self.emit_u8(opcode::SEQ);
                self.emit_u32(scr_off);
                self.emit_u32(body_off);
                offset
            }

            StgSyn::LookupLit {
                smid,
                key,
                obj,
                default,
            } => {
                let offset = self.code.len() as u32;
                self.emit_u8(opcode::LOOKUP_LIT);
                let smid_val: u32 = (*smid).into();
                self.emit_u32(smid_val);
                self.emit_ref(key);
                self.emit_ref(obj);
                self.emit_ref(default);
                offset
            }

            StgSyn::BlackHole => {
                let offset = self.code.len() as u32;
                self.emit_u8(opcode::BLACKHOLE);
                offset
            }
        }
    }

    fn into_program(self) -> BytecodeProgram {
        BytecodeProgram {
            code: self.code,
            constants: self.constants,
            heap_refs: Vec::new(), // populated later by prepare_constants()
        }
    }
}

/// Classify a `LambdaForm` for encoding.
///
/// Returns `(kind, arity, annotation)`.
fn classify_lambda_form(lf: &LambdaForm) -> (u8, u8, Smid) {
    if lf.update() {
        (opcode::FORM_THUNK, 0, lf.annotation())
    } else if lf.arity() == 0 {
        (opcode::FORM_VALUE, 0, lf.annotation())
    } else {
        (opcode::FORM_LAMBDA, lf.arity(), lf.annotation())
    }
}

/// Encoded global form: `(kind, arity, annotation, body_offset)`.
pub type GlobalForm = (u8, u8, Smid, u32);

/// Encode a user program's `StgSyn` root and its global lambda forms
/// into a single `BytecodeProgram`.
///
/// Returns `(program, root_offset, global_offsets)` where
/// `global_offsets[i]` is the bytecode offset for global `i`.
/// Globals that are intrinsic wrappers (containing BIF nodes) are
/// encoded normally — the interpreter handles BIF opcodes by setting
/// `pending_bif`.
pub fn encode(
    root: &Rc<StgSyn>,
    globals: &[LambdaForm],
) -> (BytecodeProgram, u32, Vec<GlobalForm>) {
    let mut enc = Encoder::new();

    // Encode global lambda form bodies
    let global_forms: Vec<GlobalForm> = globals
        .iter()
        .map(|lf| {
            let body_off = enc.encode_node(lf.body());
            let (kind, arity, smid) = classify_lambda_form(lf);
            (kind, arity, smid, body_off)
        })
        .collect();

    // Encode the root program
    let root_off = enc.encode_node(root);

    (enc.into_program(), root_off, global_forms)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::stg::syntax::dsl;

    #[test]
    fn encode_atom_local() {
        let syn = dsl::local(3);
        let mut enc = Encoder::new();
        let off = enc.encode_node(&syn);
        assert_eq!(off, 0);
        let prog = enc.into_program();
        assert_eq!(prog.code[0], opcode::ATOM_L);
        // u16 LE = 3
        assert_eq!(prog.code[1], 3);
        assert_eq!(prog.code[2], 0);
    }

    #[test]
    fn encode_atom_global() {
        let syn = dsl::global(42);
        let mut enc = Encoder::new();
        let off = enc.encode_node(&syn);
        assert_eq!(off, 0);
        let prog = enc.into_program();
        assert_eq!(prog.code[0], opcode::ATOM_G);
        assert_eq!(u16::from_le_bytes([prog.code[1], prog.code[2]]), 42);
    }
}
