//! Encode `StgSyn` trees into a flat BV1 bytecode stream (spec §4.2).
//!
//! Post-order: children are emitted first at low offsets, parents refer
//! to them by absolute `u32` offset, and the root is emitted last. This
//! matches the topological flatten order of `StgArena` (spec §4.2), so a
//! reconstructed arena node ordering is a valid emission order.
//!
//! Ported from the BV0 spike (`spike/bv0-bytecode:.../encode.rs`) but
//! natively typed: a single `Op::Atom` followed by an inline `Ref`
//! (rather than BV0's split `ATOM_L/G/V`), `u32` ref payloads, and the
//! CG3 `eager_args` flag bit on `App`/`DirectApp`.

use std::rc::Rc;

use crate::{
    common::sourcemap::Smid,
    eval::stg::syntax::{LambdaForm, Native as StgNative, Ref, StgSyn},
};

use super::{opcode::*, program::BytecodeProgram, CodeRef};

/// Encoded global form header: everything the machine needs to build a
/// global closure — its kind, arity, source annotation and entry offset.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GlobalForm {
    pub kind: u8,
    pub arity: u8,
    pub smid: Smid,
    pub entry: CodeRef,
}

/// Encoder state: accumulates the byte stream and the constant pool.
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

    fn emit_op(&mut self, op: Op) {
        self.emit_u8(op as u8);
    }

    fn emit_smid(&mut self, smid: Smid) {
        self.emit_u32(smid.into());
    }

    /// Add a native constant to the pool and return its index.
    fn add_constant(&mut self, n: &StgNative) -> u32 {
        // Dedup is not required for correctness; keep it simple.
        let idx = self.constants.len() as u32;
        self.constants.push(n.clone());
        idx
    }

    /// Emit an inline ref: tag byte + `u32` payload (spec §4.2).
    fn emit_ref(&mut self, r: &Ref) {
        match r {
            Ref::L(i) => {
                self.emit_u8(REF_L);
                self.emit_u32(*i as u32);
            }
            Ref::G(i) => {
                self.emit_u8(REF_G);
                self.emit_u32(*i as u32);
            }
            Ref::V(n) => {
                self.emit_u8(REF_V);
                let ci = self.add_constant(n);
                self.emit_u32(ci);
            }
        }
    }

    /// Emit a `u8` arg count followed by that many inline refs.
    fn emit_args(&mut self, args: &[Ref]) {
        self.emit_u8(args.len() as u8);
        for r in args {
            self.emit_ref(r);
        }
    }

    /// Emit a lambda-form header (kind, arity, annotation, body offset).
    fn emit_form_header(&mut self, lf: &LambdaForm, body_off: CodeRef) {
        let (kind, arity, smid) = classify_lambda_form(lf);
        self.emit_u8(kind);
        self.emit_u8(arity);
        self.emit_smid(smid);
        self.emit_u32(body_off);
    }

    // ── Node encoding (post-order) ──────────────────────────────────

    /// Encode a node, returning its start offset in the code buffer.
    /// Children are encoded first so their offsets are known before the
    /// parent is emitted.
    fn encode_node(&mut self, node: &Rc<StgSyn>) -> CodeRef {
        match &**node {
            StgSyn::Atom { evaluand } => {
                let offset = self.here();
                self.emit_op(Op::Atom);
                self.emit_ref(evaluand);
                offset
            }

            StgSyn::Case {
                scrutinee,
                branches,
                fallback,
            } => {
                // Children first (post-order).
                let scr_off = self.encode_node(scrutinee);
                let branch_data: Vec<(u8, CodeRef)> = branches
                    .iter()
                    .map(|(tag, body)| (*tag, self.encode_node(body)))
                    .collect();
                let fb_off = fallback.as_ref().map(|fb| self.encode_node(fb));

                let offset = self.here();
                self.emit_op(Op::Case);
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
                    None => self.emit_u8(0),
                }
                offset
            }

            StgSyn::Cons { tag, args } => {
                let offset = self.here();
                self.emit_op(Op::Cons);
                self.emit_u8(*tag);
                self.emit_args(args);
                offset
            }

            StgSyn::App {
                callable,
                args,
                eager_args,
            } => {
                let offset = self.here();
                self.emit_op(Op::App);
                self.emit_u8(if *eager_args { FLAG_EAGER } else { 0 });
                self.emit_ref(callable);
                self.emit_args(args);
                offset
            }

            StgSyn::DirectApp {
                smid,
                callable,
                args,
                eager_args,
            } => {
                let offset = self.here();
                self.emit_op(Op::DirectApp);
                self.emit_u8(if *eager_args { FLAG_EAGER } else { 0 });
                self.emit_smid(*smid);
                self.emit_ref(callable);
                self.emit_args(args);
                offset
            }

            StgSyn::Bif { intrinsic, args } => {
                let offset = self.here();
                self.emit_op(Op::Bif);
                self.emit_u8(*intrinsic);
                self.emit_args(args);
                offset
            }

            StgSyn::Let { bindings, body } => {
                let body_offs: Vec<CodeRef> = bindings
                    .iter()
                    .map(|lf| self.encode_node(lf.body()))
                    .collect();
                let body_off = self.encode_node(body);

                let offset = self.here();
                self.emit_op(Op::Let);
                self.emit_u16(bindings.len() as u16);
                for (lf, boff) in bindings.iter().zip(&body_offs) {
                    self.emit_form_header(lf, *boff);
                }
                self.emit_u32(body_off);
                offset
            }

            StgSyn::LetRec { bindings, body } => {
                let body_offs: Vec<CodeRef> = bindings
                    .iter()
                    .map(|lf| self.encode_node(lf.body()))
                    .collect();
                let body_off = self.encode_node(body);

                let offset = self.here();
                self.emit_op(Op::LetRec);
                self.emit_u16(bindings.len() as u16);
                for (lf, boff) in bindings.iter().zip(&body_offs) {
                    self.emit_form_header(lf, *boff);
                }
                self.emit_u32(body_off);
                offset
            }

            StgSyn::Ann { smid, body } => {
                let body_off = self.encode_node(body);
                let offset = self.here();
                self.emit_op(Op::Ann);
                self.emit_smid(*smid);
                self.emit_u32(body_off);
                offset
            }

            StgSyn::Meta { meta, body } => {
                let offset = self.here();
                self.emit_op(Op::Meta);
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

                let offset = self.here();
                self.emit_op(Op::DeMeta);
                self.emit_u32(scr_off);
                self.emit_u32(handler_off);
                self.emit_u32(or_else_off);
                offset
            }

            StgSyn::Seq { scrutinee, body } => {
                let scr_off = self.encode_node(scrutinee);
                let body_off = self.encode_node(body);

                let offset = self.here();
                self.emit_op(Op::Seq);
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
                let offset = self.here();
                self.emit_op(Op::LookupLit);
                self.emit_smid(*smid);
                self.emit_ref(key);
                self.emit_ref(obj);
                self.emit_ref(default);
                offset
            }

            StgSyn::BlackHole => {
                let offset = self.here();
                self.emit_op(Op::BlackHole);
                offset
            }
        }
    }

    /// The current end-of-stream offset (where the next byte lands).
    #[inline(always)]
    fn here(&self) -> CodeRef {
        self.code.len() as CodeRef
    }
}

/// Classify a `LambdaForm` for encoding: `(kind, arity, annotation)`.
fn classify_lambda_form(lf: &LambdaForm) -> (u8, u8, Smid) {
    if lf.update() {
        (FORM_THUNK, 0, lf.annotation())
    } else if lf.arity() == 0 {
        (FORM_VALUE, 0, lf.annotation())
    } else {
        (FORM_LAMBDA, lf.arity(), lf.annotation())
    }
}

/// Encode a program root and its global lambda forms into one
/// `BytecodeProgram`.
///
/// Returns `(program, root_offset, global_forms)`; `program.global_entries`
/// is populated with each global's entry offset (spec §5). Globals are
/// encoded before the root so their offsets are stable.
pub fn encode(
    root: &Rc<StgSyn>,
    globals: &[LambdaForm],
) -> (BytecodeProgram, CodeRef, Vec<GlobalForm>) {
    let mut enc = Encoder::new();

    let global_forms: Vec<GlobalForm> = globals
        .iter()
        .map(|lf| {
            let entry = enc.encode_node(lf.body());
            let (kind, arity, smid) = classify_lambda_form(lf);
            GlobalForm {
                kind,
                arity,
                smid,
                entry,
            }
        })
        .collect();

    let root_off = enc.encode_node(root);

    let program = BytecodeProgram {
        code: enc.code,
        constants: enc.constants,
        global_entries: global_forms.iter().map(|f| f.entry).collect(),
    };
    (program, root_off, global_forms)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::stg::syntax::dsl;

    #[test]
    fn encode_atom_v_number() {
        let syn = dsl::atom(dsl::num(42));
        let (prog, root, _) = encode(&syn, &[]);
        let mut pc = root as usize;
        assert_eq!(prog.code[pc], Op::Atom as u8);
        pc += 1;
        assert_eq!(prog.code[pc], REF_V);
        pc += 1;
        let ci = super::super::program::read_u32(&prog.code, &mut pc);
        assert_eq!(prog.constants[ci as usize], StgNative::Num(42.into()));
    }

    #[test]
    fn encode_atom_local_uses_u32_payload() {
        let syn = dsl::local(3);
        let (prog, root, _) = encode(&syn, &[]);
        let mut pc = root as usize;
        assert_eq!(prog.code[pc], Op::Atom as u8);
        pc += 1;
        assert_eq!(prog.code[pc], REF_L);
        pc += 1;
        assert_eq!(super::super::program::read_u32(&prog.code, &mut pc), 3);
    }

    #[test]
    fn encode_case_branches_are_ordered() {
        // case local(0) of tag2 -> 10; tag3 -> 20; default -> 30
        let syn = dsl::case(
            dsl::local(0),
            vec![(2, dsl::atom(dsl::num(10))), (3, dsl::atom(dsl::num(20)))],
            dsl::atom(dsl::num(30)),
        );
        let (prog, root, _) = encode(&syn, &[]);
        let mut pc = root as usize;
        assert_eq!(prog.code[pc], Op::Case as u8);
        pc += 1;
        let scr_off = super::super::program::read_u32(&prog.code, &mut pc);
        // Children precede the parent.
        assert!(scr_off < root, "scrutinee must precede parent");
        let n = super::super::program::read_u8(&prog.code, &mut pc);
        assert_eq!(n, 2);
        for expected_tag in [2u8, 3u8] {
            let tag = super::super::program::read_u8(&prog.code, &mut pc);
            let off = super::super::program::read_u32(&prog.code, &mut pc);
            assert_eq!(tag, expected_tag);
            assert!(off < root, "branch body must precede parent");
            assert!((off as usize) < prog.code.len());
        }
        let has_fb = super::super::program::read_u8(&prog.code, &mut pc);
        assert_eq!(has_fb, 1);
        let fb_off = super::super::program::read_u32(&prog.code, &mut pc);
        assert!(fb_off < root, "fallback body must precede parent");
    }

    #[test]
    fn encode_direct_app_carries_smid_and_eager_flag() {
        let smid = Smid::from(77);
        let syn = dsl::direct_app_eager(smid, dsl::gref(1), vec![dsl::num(5)]);
        let (prog, root, _) = encode(&syn, &[]);
        let mut pc = root as usize;
        assert_eq!(prog.code[pc], Op::DirectApp as u8);
        pc += 1;
        let flags = super::super::program::read_u8(&prog.code, &mut pc);
        assert_eq!(flags & FLAG_EAGER, FLAG_EAGER);
        let decoded_smid = Smid::from(super::super::program::read_u32(&prog.code, &mut pc));
        assert_eq!(decoded_smid, smid);
    }

    #[test]
    fn encode_direct_app_without_eager_clears_flag() {
        let smid = Smid::from(1);
        let syn = dsl::direct_app(smid, dsl::gref(0), vec![]);
        let (prog, root, _) = encode(&syn, &[]);
        let mut pc = root as usize + 1;
        let flags = super::super::program::read_u8(&prog.code, &mut pc);
        assert_eq!(flags & FLAG_EAGER, 0);
    }

    #[test]
    fn encode_globals_recorded_in_entries() {
        // A global whose body is `atom(local(0))` (identity-ish arity-1 lambda).
        let g = dsl::lambda(1, dsl::local(0));
        let root = dsl::atom(dsl::gref(0));
        let (prog, _root_off, forms) = encode(&root, std::slice::from_ref(&g));
        assert_eq!(forms.len(), 1);
        assert_eq!(prog.global_entries.len(), 1);
        assert_eq!(prog.global_entries[0], forms[0].entry);
        assert_eq!(forms[0].kind, FORM_LAMBDA);
        assert_eq!(forms[0].arity, 1);
    }
}
