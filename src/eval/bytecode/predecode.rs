//! Pre-decoded execution IR (lever a, BV2 folded in — bead eu-2sa6.13).
//!
//! Design: `docs/superpowers/specs/2026-07-13-predecoded-execution-ir-design.md`.
//!
//! The byte stream (`BytecodeProgram::code`) is retained as the blob wire
//! format, but for *execution* it is decoded **once**, at machine load, into a
//! flat array of fixed-width [`Instr`] records plus four off-heap side pools
//! (arg-ref lists, App/DirectApp arg-atom ordinals, let/letrec form headers,
//! and densified Case branch tables). Dispatch then reads typed fields instead
//! of re-decoding the byte stream on every tick — removing the per-tick
//! `Op::from_u8`/`read_ref`/`read_arg_offsets`/`read_form_header` envelope.
//!
//! Two representation changes accompany the decode (design §2.3, §3):
//!
//! - **`CodeRef` means an instruction ordinal**, not a byte offset, in every
//!   structure the pre-decoded engine reads (closures, program tables,
//!   continuations, pool entries). A [`DecodedProgram`] therefore carries
//!   ordinal-space copies of every root table (`blackhole`, `templates`, the
//!   apply/meta/producer templates, `pap`, and the per-slot global entries);
//!   the machine remaps `root`/`global_forms` through the same table so the
//!   initial roots dispatch correctly.
//! - **Source annotations live in a `smids` side table keyed by ordinal**, not
//!   inline on every record. `Op::Ann` is *not* eliminated here (that is the
//!   BV2 follow-up, §3); it remains a dispatchable [`Instr::ANN`] whose smid is
//!   read from the side table, exactly reproducing the byte path's behaviour.
//!
//! **GC invariant (design §4):** nothing in these structures is a GC pointer.
//! `Instr` fields are scalars/ordinals/packed refs; the pools hold
//! `DecodedRef`/ordinal/`FormHeader` scalars; `smids` are `Copy`. The collector
//! never scans any of it — code stays off the GC heap, as it does for the byte
//! stream today.

use std::collections::HashMap;

use crate::common::sourcemap::Smid;
use crate::eval::error::ExecutionError;
use crate::eval::stg::tags::Tag;

use super::program::{read_u32, read_u8};
use super::{
    read_arg_offsets, read_form_header, read_ref, BytecodeProgram, CodeRef, DecodedRef, FormHeader,
    GlobalForm, Op, FLAG_EAGER, NO_BRANCH,
};

/// One pre-decoded instruction: a fixed-width, `Copy` record. Exactly 16 bytes
/// (`op` + `flags` + `len` + three `u32`s, no padding), regardless of how many
/// operands the opcode carries — variable-length operand lists live in the
/// side pools of the owning [`DecodedProgram`], addressed by `(c, len)`.
///
/// Field meaning is opcode-dependent; the accessor methods below name each
/// use. A raw `u32` is either an **instruction ordinal** (`CodeRef`, an index
/// into `DecodedProgram::instrs`) or the payload of a packed [`DecodedRef`]
/// whose 2-bit tag is stored in `flags`. `smids[ordinal]` (not a record field)
/// holds any source annotation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Instr {
    /// Opcode (`opcode.rs` `Op`, `#[repr(u8)]`).
    pub op: Op,
    /// Opcode-dependent flag/tag byte: a packed ref tag (Atom/App/DirectApp/
    /// Meta/LookupLit), the `FLAG_EAGER` bit (App/DirectApp), the Case
    /// `min_tag`, or the FusedPrimop `primop_id`. Unused (0) otherwise.
    pub flags: u8,
    /// Side-pool run length (Cons/Bif arity, App/DirectApp arg count, Case
    /// branch count, Let/LetRec binding count); 0 for fixed-arity ops.
    pub len: u16,
    /// Primary operand (scrutinee/body/left ordinal, or first packed ref
    /// payload, or Cons tag / Bif intrinsic index in its low byte).
    pub a: u32,
    /// Secondary operand (fallback/body/right/handler ordinal, or second
    /// packed ref payload).
    pub b: u32,
    /// Tertiary operand (side-pool start index, or or_else/default ordinal).
    pub c: u32,
}

// A compile-time assertion that the record is exactly 16 bytes (design §1.2).
const _: () = assert!(std::mem::size_of::<Instr>() == 16);

// ── DecodedRef packing ──────────────────────────────────────────────
//
// A `DecodedRef` is a 2-bit tag (Local/Global/Value, matching REF_L/G/V) plus
// a `u32` payload. The tag packs into two `flags` bits at a given offset; the
// payload occupies a whole `u32` field. This loses nothing over the byte
// encoding (`read_ref` decodes the same tag byte + `u32`).

#[inline]
fn ref_tag(d: DecodedRef) -> u8 {
    match d {
        DecodedRef::Local(_) => 0,
        DecodedRef::Global(_) => 1,
        DecodedRef::Value(_) => 2,
    }
}

#[inline]
fn ref_payload(d: DecodedRef) -> u32 {
    match d {
        DecodedRef::Local(p) | DecodedRef::Global(p) | DecodedRef::Value(p) => p,
    }
}

#[inline]
fn make_ref(tag: u8, payload: u32) -> DecodedRef {
    match tag & 0b11 {
        0 => DecodedRef::Local(payload),
        1 => DecodedRef::Global(payload),
        _ => DecodedRef::Value(payload),
    }
}

impl Instr {
    /// The packed single ref of an `Atom` (tag in `flags` bits 0-1, payload in
    /// `a`).
    #[inline]
    pub fn atom_ref(&self) -> DecodedRef {
        make_ref(self.flags, self.a)
    }

    /// Whether an App/DirectApp resolves `Local` args eagerly (`FLAG_EAGER`).
    #[inline]
    pub fn eager(&self) -> bool {
        self.flags & FLAG_EAGER != 0
    }

    /// The packed callable ref of an App/DirectApp (tag in `flags` bits 1-2 —
    /// above the eager bit — payload in `a`).
    #[inline]
    pub fn callable_ref(&self) -> DecodedRef {
        make_ref(self.flags >> 1, self.a)
    }

    /// The two packed refs of a `Meta`/`LookupLit` (first tag in `flags` bits
    /// 0-1, payload `a`; second tag in bits 2-3, payload `b`).
    #[inline]
    pub fn first_ref(&self) -> DecodedRef {
        make_ref(self.flags, self.a)
    }
    #[inline]
    pub fn second_ref(&self) -> DecodedRef {
        make_ref(self.flags >> 2, self.b)
    }

    /// The `Cons` data-constructor tag / `Bif` intrinsic index (low byte of `a`).
    #[inline]
    pub fn tag_byte(&self) -> u8 {
        self.a as u8
    }

    /// The `Case` `min_tag` (stored in `flags`, which carries no ref tag).
    #[inline]
    pub fn min_tag(&self) -> Tag {
        self.flags
    }

    /// The `FusedPrimop` intrinsic id (stored in `flags`).
    #[inline]
    pub fn primop_id(&self) -> u8 {
        self.flags
    }

    /// The side-pool run as `(start, len)`.
    #[inline]
    pub fn pool(&self) -> (usize, usize) {
        (self.c as usize, self.len as usize)
    }
}

/// The pre-decoded form of a whole [`BytecodeProgram`]: the flat instruction
/// array, the smid side table, the four operand side pools, and ordinal-space
/// copies of every root table the machine reads. Holds **no GC pointers**.
#[derive(Debug, Default)]
pub struct DecodedProgram {
    /// Instructions indexed by ordinal (`CodeRef`).
    pub instrs: Vec<Instr>,
    /// Source annotation per ordinal (`Smid::default()` where none). Keyed by
    /// ordinal, mirroring `instrs` (design §3).
    pub smids: Vec<Smid>,
    /// Cons/Bif arg refs (design's `refs` pool).
    pub refs: Vec<DecodedRef>,
    /// App/DirectApp arg-atom ordinals (design's `offsets` pool).
    pub offsets: Vec<CodeRef>,
    /// Let/LetRec form headers, `body` remapped to an ordinal (design's
    /// `headers` pool).
    pub headers: Vec<FormHeader>,
    /// Case densified branch tables, entries remapped to ordinals (design's
    /// `branches` pool; `NO_BRANCH` preserved).
    pub branches: Vec<CodeRef>,

    // ── Ordinal-space root tables (design §2.1/§2.3) ────────────────
    /// Program root, as an ordinal.
    pub root: CodeRef,
    /// Shared blackhole node, as an ordinal.
    pub blackhole: CodeRef,
    /// Constructor templates by data tag, as ordinals.
    pub templates: Vec<CodeRef>,
    /// Metadata template, as an ordinal.
    pub meta_template: CodeRef,
    /// Apply-one / apply-two / producer-tail templates, as ordinals.
    pub apply1_template: CodeRef,
    pub apply2_template: CodeRef,
    pub producer_tail_template: CodeRef,
    /// PAP trampoline templates, as ordinals (same indexing as
    /// `BytecodeProgram::pap`).
    pub pap: Vec<CodeRef>,
    /// Per-slot global entry bodies, as ordinals (parallel to the
    /// `global_forms` passed to the machine).
    pub global_entries: Vec<CodeRef>,

    // ── Ordinal ↔ byte-offset maps (for the static-peel helpers) ────────
    /// Ordinal → original byte offset. The io-run driver and intrinsic-support
    /// helpers statically peel a closure's code by reading the byte stream;
    /// under pre-decode a closure carries an ordinal, so `off_of[ord]` recovers
    /// the byte offset those peekers start from. Indexed by ordinal (small —
    /// one entry per instruction, not per byte).
    pub off_of: Vec<CodeRef>,
    /// Byte offset → ordinal (the inverse of `off_of`). The peekers convert a
    /// child byte offset they read from the stream into an ordinal before
    /// building a closure over it, so every closure — synthetic or resolved —
    /// carries an ordinal uniformly.
    pub ord_of: HashMap<CodeRef, CodeRef>,
}

impl DecodedProgram {
    /// The byte offset a closure's `code` ordinal was decoded from (for the
    /// static-peel helpers, which read the byte stream).
    #[inline]
    pub fn byte_off(&self, ordinal: CodeRef) -> usize {
        self.off_of[ordinal as usize] as usize
    }

    /// The ordinal a byte offset was assigned, for building a closure over a
    /// child offset peeled from the byte stream.
    #[inline]
    pub fn ordinal(&self, byte_off: CodeRef) -> CodeRef {
        self.ord_of[&byte_off]
    }
}

/// Worklist decoder: assigns each reachable byte offset an ordinal in
/// discovery order and decodes it into an [`Instr`] + side-pool entries, with
/// every child `CodeRef` operand written directly as an ordinal (the "decode
/// after assignment" interleave of design §2.3 — no separate fixup pass).
struct Decoder<'a> {
    code: &'a [u8],
    /// Byte offset → assigned ordinal.
    ord_of: HashMap<u32, u32>,
    /// Ordinal → byte offset (the inverse of `ord_of`).
    off_of: Vec<u32>,
    /// Decoded instruction per ordinal (`None` until its offset is processed).
    instrs: Vec<Option<Instr>>,
    smids: Vec<Smid>,
    refs: Vec<DecodedRef>,
    offsets: Vec<CodeRef>,
    headers: Vec<FormHeader>,
    branches: Vec<CodeRef>,
    /// Byte offsets awaiting decode, paired with their assigned ordinal.
    queue: Vec<(u32, u32)>,
}

impl<'a> Decoder<'a> {
    fn new(code: &'a [u8]) -> Self {
        Decoder {
            code,
            ord_of: HashMap::new(),
            off_of: Vec::new(),
            instrs: Vec::new(),
            smids: Vec::new(),
            refs: Vec::new(),
            offsets: Vec::new(),
            headers: Vec::new(),
            branches: Vec::new(),
            queue: Vec::new(),
        }
    }

    /// The ordinal for a byte offset, assigning (and enqueueing) a fresh one on
    /// first encounter. Cycles and forward references are safe: the ordinal is
    /// fixed at assignment, before the offset's own body is decoded.
    fn ordinal_for(&mut self, byte_off: CodeRef) -> CodeRef {
        if let Some(&ord) = self.ord_of.get(&byte_off) {
            return ord;
        }
        let ord = self.instrs.len() as u32;
        self.ord_of.insert(byte_off, ord);
        self.off_of.push(byte_off);
        self.instrs.push(None);
        self.smids.push(Smid::default());
        self.queue.push((byte_off, ord));
        ord
    }

    /// Decode the single opcode node at `off` into an [`Instr`], appending any
    /// side-pool entries and recording `smids[ord]` for annotated opcodes.
    fn decode_node(&mut self, off: usize, ord: u32) -> Result<Instr, ExecutionError> {
        let code = self.code;
        let mut pc = off;
        let op = Op::from_u8(read_u8(code, &mut pc)).ok_or_else(|| {
            ExecutionError::Panic(Default::default(), "bytecode: invalid opcode".to_string())
        })?;
        let instr = match op {
            Op::Atom => {
                let dref = read_ref(code, &mut pc)?;
                Instr {
                    op,
                    flags: ref_tag(dref),
                    len: 0,
                    a: ref_payload(dref),
                    b: 0,
                    c: 0,
                }
            }
            Op::Cons => {
                let tag = read_u8(code, &mut pc);
                let arg_offs = read_arg_offsets(code, &mut pc);
                let (start, len) = self.push_refs(&arg_offs)?;
                Instr {
                    op,
                    flags: 0,
                    len,
                    a: tag as u32,
                    b: 0,
                    c: start,
                }
            }
            Op::Bif => {
                let intrinsic = read_u8(code, &mut pc);
                let arg_offs = read_arg_offsets(code, &mut pc);
                let (start, len) = self.push_refs(&arg_offs)?;
                Instr {
                    op,
                    flags: 0,
                    len,
                    a: intrinsic as u32,
                    b: 0,
                    c: start,
                }
            }
            Op::Case => {
                let scr = self.ordinal_for(read_u32(code, &mut pc));
                let min_tag = read_u8(code, &mut pc);
                let len = read_u8(code, &mut pc) as usize;
                let start = self.branches.len() as u32;
                for _ in 0..len {
                    let e = read_u32(code, &mut pc);
                    let mapped = if e == NO_BRANCH {
                        NO_BRANCH
                    } else {
                        self.ordinal_for(e)
                    };
                    self.branches.push(mapped);
                }
                let has_fb = read_u8(code, &mut pc);
                let fallback = if has_fb == 1 {
                    self.ordinal_for(read_u32(code, &mut pc))
                } else {
                    NO_BRANCH
                };
                Instr {
                    op,
                    flags: min_tag,
                    len: len as u16,
                    a: scr,
                    b: fallback,
                    c: start,
                }
            }
            Op::Seq => {
                let scr = self.ordinal_for(read_u32(code, &mut pc));
                let body = self.ordinal_for(read_u32(code, &mut pc));
                Instr {
                    op,
                    flags: 0,
                    len: 0,
                    a: scr,
                    b: body,
                    c: 0,
                }
            }
            Op::Ann => {
                let smid = Smid::from(read_u32(code, &mut pc));
                let body = self.ordinal_for(read_u32(code, &mut pc));
                self.smids[ord as usize] = smid;
                Instr {
                    op,
                    flags: 0,
                    len: 0,
                    a: body,
                    b: 0,
                    c: 0,
                }
            }
            Op::FusedPrimop => {
                let primop_id = read_u8(code, &mut pc);
                let left = self.ordinal_for(read_u32(code, &mut pc));
                let right = self.ordinal_for(read_u32(code, &mut pc));
                Instr {
                    op,
                    flags: primop_id,
                    len: 0,
                    a: left,
                    b: right,
                    c: 0,
                }
            }
            Op::BlackHole => Instr {
                op,
                flags: 0,
                len: 0,
                a: 0,
                b: 0,
                c: 0,
            },
            Op::App => {
                let flags = read_u8(code, &mut pc);
                let eager = flags & FLAG_EAGER;
                let callable = read_ref(code, &mut pc)?;
                let arg_offs = read_arg_offsets(code, &mut pc);
                let (start, len) = self.push_offsets(&arg_offs);
                Instr {
                    op,
                    flags: eager | (ref_tag(callable) << 1),
                    len,
                    a: ref_payload(callable),
                    b: 0,
                    c: start,
                }
            }
            Op::DirectApp => {
                let flags = read_u8(code, &mut pc);
                let eager = flags & FLAG_EAGER;
                let smid = Smid::from(read_u32(code, &mut pc));
                let callable = read_ref(code, &mut pc)?;
                let arg_offs = read_arg_offsets(code, &mut pc);
                let (start, len) = self.push_offsets(&arg_offs);
                self.smids[ord as usize] = smid;
                Instr {
                    op,
                    flags: eager | (ref_tag(callable) << 1),
                    len,
                    a: ref_payload(callable),
                    b: 0,
                    c: start,
                }
            }
            Op::Let | Op::LetRec => {
                let count = read_u32(code, &mut pc) as usize;
                let start = self.headers.len() as u32;
                // Reserve then fill: `read_form_header` borrows `code`, while
                // `ordinal_for` borrows `self` mutably — so read each header
                // first, then remap its body.
                for _ in 0..count {
                    let mut h = read_form_header(code, &mut pc);
                    h.body = self.ordinal_for(h.body);
                    self.headers.push(h);
                }
                let body = self.ordinal_for(read_u32(code, &mut pc));
                Instr {
                    op,
                    flags: 0,
                    len: count as u16,
                    a: body,
                    b: 0,
                    c: start,
                }
            }
            Op::Meta => {
                let meta_off = read_u32(code, &mut pc);
                let body_off = read_u32(code, &mut pc);
                let meta_ref = arg_ref(code, meta_off)?;
                let body_ref = arg_ref(code, body_off)?;
                Instr {
                    op,
                    flags: ref_tag(meta_ref) | (ref_tag(body_ref) << 2),
                    len: 0,
                    a: ref_payload(meta_ref),
                    b: ref_payload(body_ref),
                    c: 0,
                }
            }
            Op::DeMeta => {
                let scr = self.ordinal_for(read_u32(code, &mut pc));
                let handler = self.ordinal_for(read_u32(code, &mut pc));
                let or_else = self.ordinal_for(read_u32(code, &mut pc));
                Instr {
                    op,
                    flags: 0,
                    len: 0,
                    a: scr,
                    b: handler,
                    c: or_else,
                }
            }
            Op::LookupLit => {
                let smid = Smid::from(read_u32(code, &mut pc));
                let key = read_ref(code, &mut pc)?;
                let obj = read_ref(code, &mut pc)?;
                let default = self.ordinal_for(read_u32(code, &mut pc));
                self.smids[ord as usize] = smid;
                Instr {
                    op,
                    flags: ref_tag(key) | (ref_tag(obj) << 2),
                    len: 0,
                    a: ref_payload(key),
                    b: ref_payload(obj),
                    c: default,
                }
            }
        };
        Ok(instr)
    }

    /// Resolve a run of arg-atom offsets to `DecodedRef`s in the `refs` pool
    /// (Cons/Bif). Returns `(start, len)`.
    fn push_refs(&mut self, arg_offs: &[CodeRef]) -> Result<(u32, u16), ExecutionError> {
        let start = self.refs.len() as u32;
        for off in arg_offs {
            self.refs.push(arg_ref(self.code, *off)?);
        }
        Ok((start, arg_offs.len() as u16))
    }

    /// Assign ordinals to a run of arg-atom offsets in the `offsets` pool
    /// (App/DirectApp — the atoms are entered as lazy closures). Returns
    /// `(start, len)`.
    fn push_offsets(&mut self, arg_offs: &[CodeRef]) -> (u32, u16) {
        let start = self.offsets.len() as u32;
        for off in arg_offs {
            let ord = self.ordinal_for(*off);
            self.offsets.push(ord);
        }
        (start, arg_offs.len() as u16)
    }

    /// Drain the worklist, decoding every enqueued offset.
    fn run(&mut self) -> Result<(), ExecutionError> {
        while let Some((off, ord)) = self.queue.pop() {
            let instr = self.decode_node(off as usize, ord)?;
            self.instrs[ord as usize] = Some(instr);
        }
        Ok(())
    }
}

/// Decode the field ref of an arg atom-offset (skip the `OP_ATOM` byte and
/// read the inline ref). Mirrors `machine::arg_ref`.
fn arg_ref(code: &[u8], atom_off: CodeRef) -> Result<DecodedRef, ExecutionError> {
    let mut pc = atom_off as usize + 1;
    read_ref(code, &mut pc)
}

/// Pre-decode a whole program into ordinal space, seeded from every root the
/// machine can reach: the program root, all global-form entries, the
/// constructor/meta/apply/producer templates, the blackhole node, and every
/// PAP trampoline (design §2.2). Decodes eagerly (a full reachability walk);
/// lazy first-touch decode of user code (§2.2) is a deferred startup
/// optimisation and is not required for correctness.
pub fn decode_program(
    prog: &BytecodeProgram,
    root: CodeRef,
    global_forms: &[GlobalForm],
) -> Result<DecodedProgram, ExecutionError> {
    let mut d = Decoder::new(prog.code.as_slice());

    // Seed roots, capturing the ordinal of each so the machine can dispatch
    // from them. Order is irrelevant to correctness (ordinals are stable once
    // assigned); it only fixes the discovery numbering.
    let root_ord = d.ordinal_for(root);
    let global_entries: Vec<CodeRef> = global_forms
        .iter()
        .map(|g| d.ordinal_for(g.entry))
        .collect();
    let templates: Vec<CodeRef> = prog.templates.iter().map(|t| d.ordinal_for(*t)).collect();
    let blackhole = d.ordinal_for(prog.blackhole);
    let meta_template = d.ordinal_for(prog.meta_template);
    let apply1_template = d.ordinal_for(prog.apply1_template);
    let apply2_template = d.ordinal_for(prog.apply2_template);
    let producer_tail_template = d.ordinal_for(prog.producer_tail_template);
    let pap: Vec<CodeRef> = prog.pap.iter().map(|p| d.ordinal_for(*p)).collect();

    d.run()?;

    let instrs = d
        .instrs
        .into_iter()
        .map(|o| o.expect("every enqueued ordinal is decoded"))
        .collect();

    Ok(DecodedProgram {
        instrs,
        smids: d.smids,
        refs: d.refs,
        offsets: d.offsets,
        headers: d.headers,
        branches: d.branches,
        root: root_ord,
        blackhole,
        templates,
        meta_template,
        apply1_template,
        apply2_template,
        producer_tail_template,
        pap,
        global_entries,
        off_of: d.off_of,
        ord_of: d.ord_of,
    })
}
