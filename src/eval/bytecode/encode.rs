//! Encode `StgSyn` trees into a flat BV1 bytecode stream (spec §4.2).
//!
//! Post-order: children are emitted first at low offsets, parents refer
//! to them by absolute `u32` offset, and the root is emitted last. This
//! matches the topological flatten order of `StgArena` (spec §4.2).
//!
//! **Operand model (plan Task 2.x decision).** Every argument operand
//! that the runtime may reify into a closure — App/DirectApp/Cons/Bif
//! args, Meta's meta/body, LookupLit's default — is pre-emitted as its
//! own `OP_ATOM` node and referenced by the parent as a `u32` offset.
//! A lazy arg closure is then `BcClosure::new(atom_off, env)` with **zero
//! per-dispatch code allocation** (the BV0 trap). Because an `OP_ATOM`
//! node is `[Op::Atom][ref]`, the raw ref is recoverable by decoding at
//! `atom_off + 1`, so the eager path and data-arg env-sharing (which need
//! the underlying `L(i)`) still work. Callables stay inline refs (they are
//! always resolved, never held as lazy closures).
//!
//! `Case` branch tables are densified at encode time (mirroring the
//! `StgSyn → HeapSyn` loader): `min_tag` + a dense table of entry offsets,
//! gaps filled with `NO_BRANCH`.

use std::rc::Rc;

use serde::{Deserialize, Serialize};

use crate::{
    common::sourcemap::Smid,
    eval::stg::{
        syntax::{dsl, LambdaForm, Native as StgNative, Ref, StgSyn},
        tags::DataConstructor,
    },
};

use super::{opcode::*, program::BytecodeProgram, CodeRef};

/// Encoded global form header: everything the machine needs to build a
/// global closure — its kind, arity, source annotation and entry offset.
///
/// Derives `Serialize`/`Deserialize` so the prelude's global forms can be
/// embedded in the blob alongside the pre-encoded `BytecodeProgram` (BV5).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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

    /// Emit a standalone `OP_ATOM` node for a ref and return its offset.
    /// Used to reify operand refs into addressable lazy closures.
    fn emit_atom(&mut self, r: &Ref) -> CodeRef {
        let offset = self.here();
        self.emit_op(Op::Atom);
        self.emit_ref(r);
        offset
    }

    /// Emit a `FusedPrimop` node in place of a whitelisted intrinsic's
    /// `binary_wrapper`-compiled `Case`-of-`Case` body (design §4/§5.4).
    /// `binary_wrapper` always builds `lambda(2, body)`, so the wrapper's own
    /// bound args are exactly `local(0)`/`local(1)`; these are pre-emitted as
    /// standalone `Op::Atom` nodes (the same convention every other operand
    /// uses — see the module doc comment) and referenced by offset, mirroring
    /// `Op::Seq`'s scrutinee encoding.
    fn emit_fused_primop(&mut self, primop_id: u8) -> CodeRef {
        let left_off = self.emit_atom(&Ref::L(0));
        let right_off = self.emit_atom(&Ref::L(1));
        let offset = self.here();
        self.emit_op(Op::FusedPrimop);
        self.emit_u8(primop_id);
        self.emit_u32(left_off);
        self.emit_u32(right_off);
        offset
    }

    /// Pre-emit an `OP_ATOM` per arg ref, returning their offsets. Emitted
    /// before the parent node (post-order).
    fn emit_arg_atoms(&mut self, args: &[Ref]) -> Vec<CodeRef> {
        args.iter().map(|r| self.emit_atom(r)).collect()
    }

    /// Emit an arg-offset list: `u8` count then that many `u32` offsets.
    fn emit_arg_offsets(&mut self, offs: &[CodeRef]) {
        self.emit_u8(offs.len() as u8);
        for off in offs {
            self.emit_u32(*off);
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
    fn encode_node(&mut self, node: &Rc<StgSyn>) -> CodeRef {
        match &**node {
            StgSyn::Atom { evaluand } => self.emit_atom(evaluand),

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
                self.emit_dense_branch_table(&branch_data);
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
                let arg_offs = self.emit_arg_atoms(args);
                let offset = self.here();
                self.emit_op(Op::Cons);
                self.emit_u8(*tag);
                self.emit_arg_offsets(&arg_offs);
                offset
            }

            StgSyn::App {
                callable,
                args,
                eager_args,
            } => {
                let arg_offs = self.emit_arg_atoms(args);
                let offset = self.here();
                self.emit_op(Op::App);
                self.emit_u8(if *eager_args { FLAG_EAGER } else { 0 });
                self.emit_ref(callable);
                self.emit_arg_offsets(&arg_offs);
                offset
            }

            StgSyn::DirectApp {
                smid,
                callable,
                args,
                eager_args,
            } => {
                let arg_offs = self.emit_arg_atoms(args);
                let offset = self.here();
                self.emit_op(Op::DirectApp);
                self.emit_u8(if *eager_args { FLAG_EAGER } else { 0 });
                self.emit_smid(*smid);
                self.emit_ref(callable);
                self.emit_arg_offsets(&arg_offs);
                offset
            }

            StgSyn::Bif { intrinsic, args } => {
                let arg_offs = self.emit_arg_atoms(args);
                let offset = self.here();
                self.emit_op(Op::Bif);
                self.emit_u8(*intrinsic);
                self.emit_arg_offsets(&arg_offs);
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
                // Reify meta and body refs as atoms so the runtime can
                // resolve/close over them without per-tick allocation.
                let meta_off = self.emit_atom(meta);
                let body_off = self.emit_atom(body);
                let offset = self.here();
                self.emit_op(Op::Meta);
                self.emit_u32(meta_off);
                self.emit_u32(body_off);
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
                // `default` may be a `Ref::V` reified into a closure, so it
                // is emitted as an atom offset; `key` (always a `V` symbol)
                // and `obj` (a resolvable L/G) stay inline.
                let default_off = self.emit_atom(default);
                let offset = self.here();
                self.emit_op(Op::LookupLit);
                self.emit_smid(*smid);
                self.emit_ref(key);
                self.emit_ref(obj);
                self.emit_u32(default_off);
                offset
            }

            StgSyn::BlackHole => {
                let offset = self.here();
                self.emit_op(Op::BlackHole);
                offset
            }
        }
    }

    /// Emit a densified branch table: `min_tag` (u8), `len` (u8), then
    /// `len` `u32` entries indexed by `tag - min_tag`, gaps = `NO_BRANCH`.
    fn emit_dense_branch_table(&mut self, branch_data: &[(u8, CodeRef)]) {
        if branch_data.is_empty() {
            self.emit_u8(0); // min_tag
            self.emit_u8(0); // len
            return;
        }
        let min_tag = branch_data.iter().map(|(t, _)| *t).min().unwrap();
        let max_tag = branch_data.iter().map(|(t, _)| *t).max().unwrap();
        let len = (max_tag - min_tag) as usize + 1;
        let mut table = vec![NO_BRANCH; len];
        for (tag, off) in branch_data {
            table[(tag - min_tag) as usize] = *off;
        }
        self.emit_u8(min_tag);
        self.emit_u8(len as u8);
        for entry in table {
            self.emit_u32(entry);
        }
    }

    /// Encode one data-constructor template per tag (spec §5.5): an
    /// `OP_CONS tag [L0..L(arity-1)]` node per `DataConstructor`, returning
    /// a table indexed by tag. Runtime data values point at these with a
    /// fresh env holding the field values — no per-construction code alloc.
    fn encode_templates(&mut self) -> Vec<CodeRef> {
        // Tags are contiguous 0..=BoxedTypeData(17).
        const NUM_DATA_TAGS: u8 = 18;
        (0..NUM_DATA_TAGS)
            .map(|tag| {
                let arity = DataConstructor::try_from(tag)
                    .map(|d| d.arity())
                    .unwrap_or(0);
                let args: Vec<Ref> = (0..arity).map(dsl::lref).collect();
                let node = dsl::data(tag, args);
                self.encode_node(&node)
            })
            .collect()
    }

    /// Encode the PAP trampoline table (spec §6.1 / `pap_syn`). For each
    /// `(supplied, pending)` with both in `1..=PAP_MAX_ARITY`, emit
    /// `App(L(pending), [L(pending+1)..L(pending+supplied), L(0)..L(pending-1)])`.
    /// When the partially-applied closure is later saturated with `pending`
    /// args, its env is `[pending args…, f, supplied args…]`, so this applies
    /// `f` to all `supplied + pending` arguments in order.
    fn encode_pap_templates(&mut self) -> Vec<CodeRef> {
        use crate::eval::bytecode::program::PAP_MAX_ARITY as MAX;
        let mut table = vec![0u32; MAX * MAX];
        for supplied in 1..=MAX {
            for pending in 1..=MAX {
                let mut args: Vec<Ref> = Vec::with_capacity(supplied + pending);
                for i in 0..supplied {
                    args.push(dsl::lref(pending + i + 1));
                }
                for i in 0..pending {
                    args.push(dsl::lref(i));
                }
                let node = dsl::app(dsl::lref(pending), args);
                table[(supplied - 1) * MAX + (pending - 1)] = self.encode_node(&node);
            }
        }
        table
    }

    /// The current end-of-stream offset (where the next byte lands).
    #[inline(always)]
    fn here(&self) -> CodeRef {
        self.code.len() as CodeRef
    }
}

/// Intrinsic names whose global-form body is fusible into a single
/// `Op::FusedPrimop` (design §3.1, "first cut" — the `binary_wrapper`
/// family): resolved once to `(name, intrinsic index)` via
/// `intrinsics::index`, never hardcoded magic numbers, mirroring the
/// project's existing convention (e.g. `PRODUCER_NEXT` above).
static FUSIBLE_PRIMOPS: std::sync::LazyLock<[(&'static str, usize); 8]> =
    std::sync::LazyLock::new(|| {
        ["ADD", "SUB", "MUL", "DIV", "GT", "GTE", "LT", "LTE"].map(|n| {
            (
                n,
                crate::eval::intrinsics::index(n).expect("fusible primop registered"),
            )
        })
    });

/// Whether an intrinsic (identified by its global index) is in the fusible-
/// primop whitelist that the encoder intercepts as a single `Op::FusedPrimop`.
///
/// Shared with the STG compiler's inline guard (`compiler.rs`) so that
/// "Option A" — suppressing wrapper inlining at direct call sites so direct
/// arithmetic routes through the fused global form — stays in lockstep with
/// exactly the set the encoder actually fuses. Sharing the single source of
/// truth (`FUSIBLE_PRIMOPS`) keeps the two sites from drifting apart.
pub fn is_fusible_primop_index(index: usize) -> bool {
    FUSIBLE_PRIMOPS.iter().any(|(_, idx)| *idx == index)
}

/// Structural shape guard (design §9.4): confirms a whitelisted intrinsic's
/// `LambdaForm` has `binary_wrapper`'s known shape (`lambda(2,
/// case(local(0), ...))`) before the encoder substitutes the fused body.
///
/// This *gates* fusion rather than asserting it (a deliberate strengthening
/// of §9.4's "panic loudly" mitigation): a mismatch — either a future
/// `arith.rs` refactor that changes the wrapper's shape, or (routinely, in
/// tests — see `differential.rs`'s `assert_engines_agree`, which builds a
/// `StandardRuntime` with only a handful of intrinsics registered, leaving
/// the rest as arity-0 `Unimplemented` stubs) an intrinsic that simply isn't
/// wired up as the real `binary_wrapper` implementation at all — silently
/// falls back to the ordinary `encode_node(lf.body())` path. This can only
/// ever cost a missed fusion (a perf-only regression for that slot), never a
/// wrong-shaped `Op::FusedPrimop`, so it is strictly safer than a hard panic
/// while remaining just as effective at preventing silent miscompilation.
fn is_fusible_shape(lf: &LambdaForm) -> bool {
    lf.arity() == 2
        && matches!(
            &**lf.body(),
            StgSyn::Case { scrutinee, .. }
                if matches!(&**scrutinee, StgSyn::Atom { evaluand: Ref::L(0) })
        )
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

/// The fixed-shape templates and global entry table shared by every
/// program built from a given set of globals. Emitted before any program
/// root so their offsets are stable (spec §4.2 / §5.5).
struct Fixtures {
    templates: Vec<CodeRef>,
    blackhole: CodeRef,
    meta_template: CodeRef,
    pap: Vec<CodeRef>,
    apply1_template: CodeRef,
    apply2_template: CodeRef,
    producer_tail_template: CodeRef,
    global_forms: Vec<GlobalForm>,
}

/// Emit the constructor/PAP/thunk templates and every global lambda-form
/// body into `enc`, returning the resulting `Fixtures`. Shared by `encode`
/// (which then appends a program root) and `encode_prelude` (which stops
/// here, leaving a reusable pre-encoded prelude image).
fn emit_fixtures_and_globals(enc: &mut Encoder, globals: &[LambdaForm]) -> Fixtures {
    // Constructor templates first, so their offsets sit at the low end of
    // the arena and are stable regardless of program size.
    let templates = enc.encode_templates();
    let blackhole = enc.here();
    enc.emit_op(Op::BlackHole);
    // A single `Meta L0 L1` template: a runtime metadata value closes a fresh
    // env `[meta, body]` over it (mirrors the constructor templates).
    let meta_template = enc.encode_node(&dsl::with_meta(dsl::lref(0), dsl::lref(1)));
    let pap = enc.encode_pap_templates();

    // Fixed-shape thunk templates (spec §5.5, arena-growth analysis). Both are
    // pre-encoded once here; runtime construction allocates only the GC-heap
    // env frame over the fixed code, so there is no per-call arena growth.
    //
    // `apply1_template`: `App(L0, [L1])` — a lazy `f(a)` thunk (io-run driver).
    let apply1_template = enc.encode_node(&dsl::app(dsl::lref(0), vec![dsl::lref(1)]));
    // `apply2_template`: `App(L0, [L1, L2])` — a lazy `f(a0, a1)` thunk.
    let apply2_template =
        enc.encode_node(&dsl::app(dsl::lref(0), vec![dsl::lref(1), dsl::lref(2)]));
    // `producer_tail_template`: `AppBif(PRODUCER_NEXT, [L0])` — the updatable
    // producer-tail thunk. The bif index is fixed (a static registry lookup).
    let producer_next_index =
        crate::eval::intrinsics::index("PRODUCER_NEXT").expect("PRODUCER_NEXT intrinsic") as u8;
    let producer_tail_template =
        enc.encode_node(&dsl::app_bif(producer_next_index, vec![dsl::lref(0)]));

    let global_forms: Vec<GlobalForm> = globals
        .iter()
        .enumerate()
        .map(|(i, lf)| {
            // Intercept by intrinsic *identity* (index), not by pattern-
            // matching the compiled `StgSyn` tree (design §4): the fusible
            // shape is exactly and only the compiler-emitted body of this
            // small, known, fixed set of intrinsic global forms, so no tree
            // walk/rewrite is needed — StgSyn/HeapSyn stay entirely untouched.
            let entry = if FUSIBLE_PRIMOPS.iter().any(|(_, idx)| *idx == i) && is_fusible_shape(lf)
            {
                enc.emit_fused_primop(i as u8)
            } else {
                enc.encode_node(lf.body())
            };
            let (kind, arity, smid) = classify_lambda_form(lf);
            GlobalForm {
                kind,
                arity,
                smid,
                entry,
            }
        })
        .collect();

    Fixtures {
        templates,
        blackhole,
        meta_template,
        pap,
        apply1_template,
        apply2_template,
        producer_tail_template,
        global_forms,
    }
}

/// Assemble a `BytecodeProgram` from an encoder's accumulated `code`/
/// `constants` and the shared `Fixtures`.
fn build_program(enc: Encoder, f: &Fixtures) -> BytecodeProgram {
    BytecodeProgram {
        code: enc.code,
        constants: enc.constants,
        global_entries: f.global_forms.iter().map(|g| g.entry).collect(),
        templates: f.templates.clone(),
        blackhole: f.blackhole,
        meta_template: f.meta_template,
        pap: f.pap.clone(),
        apply1_template: f.apply1_template,
        apply2_template: f.apply2_template,
        producer_tail_template: f.producer_tail_template,
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
    let fixtures = emit_fixtures_and_globals(&mut enc, globals);
    let root_off = enc.encode_node(root);
    let program = build_program(enc, &fixtures);
    (program, root_off, fixtures.global_forms)
}

/// Encode only the fixed templates and prelude global bodies — no program
/// root — producing a reusable pre-encoded prelude image (BV5, eu-amp9).
///
/// The returned program's `code` ends immediately after the last global
/// body, so a subsequent [`encode_root_onto`] appends the program root at
/// exactly the offset `encode` would have used, yielding byte-identical
/// output. Embedded in the prelude blob so the loader can run straight off
/// serialised bytecode instead of re-encoding hundreds of prelude globals
/// on every invocation.
pub fn encode_prelude(globals: &[LambdaForm]) -> (BytecodeProgram, Vec<GlobalForm>) {
    let mut enc = Encoder::new();
    let fixtures = emit_fixtures_and_globals(&mut enc, globals);
    let program = build_program(enc, &fixtures);
    (program, fixtures.global_forms)
}

/// Append a program root to a pre-encoded prelude `base`, reusing its
/// `code`/`constants` prefix unchanged, and return the extended program and
/// the root's entry offset (BV5, eu-amp9).
///
/// Because every cross-reference is by global slot (`Ref::G`, resolved via
/// the global entry table) or constant-pool index — never a raw byte offset
/// into another global — appending the root leaves every existing offset
/// valid and produces exactly what a full `encode(root, globals)` would.
pub fn encode_root_onto(base: &BytecodeProgram, root: &Rc<StgSyn>) -> (BytecodeProgram, CodeRef) {
    let mut enc = Encoder {
        code: base.code.clone(),
        constants: base.constants.clone(),
    };
    let root_off = enc.encode_node(root);
    let program = BytecodeProgram {
        code: enc.code,
        constants: enc.constants,
        global_entries: base.global_entries.clone(),
        templates: base.templates.clone(),
        blackhole: base.blackhole,
        meta_template: base.meta_template,
        pap: base.pap.clone(),
        apply1_template: base.apply1_template,
        apply2_template: base.apply2_template,
        producer_tail_template: base.producer_tail_template,
    };
    (program, root_off)
}

/// Re-encode a set of override global forms and a program root onto a
/// pre-encoded prelude `base` (BV5, eu-amp9).
///
/// The prelude blob bakes stale bodies for the `__args` / `__io` globals
/// (their real values depend on the invocation). This mirrors the STG blob
/// path's `set_prelude_slot_override`: each `(slot, form)` body is encoded
/// afresh and its `global_entries[slot]` / `global_forms[slot]` patched to
/// point at the new bytecode. The old (dead) bytecode remains in the buffer
/// but is never referenced. Only these few globals plus the root are
/// re-encoded — the hundreds of ordinary prelude/intrinsic globals are used
/// straight from `base`.
pub fn encode_overrides_and_root(
    base: &BytecodeProgram,
    overrides: &[(usize, LambdaForm)],
    global_forms: &mut [GlobalForm],
    root: &Rc<StgSyn>,
) -> (BytecodeProgram, CodeRef) {
    let mut enc = Encoder {
        code: base.code.clone(),
        constants: base.constants.clone(),
    };
    let mut global_entries = base.global_entries.clone();
    for (slot, lf) in overrides {
        let entry = enc.encode_node(lf.body());
        let (kind, arity, smid) = classify_lambda_form(lf);
        if let Some(e) = global_entries.get_mut(*slot) {
            *e = entry;
        }
        if let Some(g) = global_forms.get_mut(*slot) {
            *g = GlobalForm {
                kind,
                arity,
                smid,
                entry,
            };
        }
    }
    let root_off = enc.encode_node(root);
    let program = BytecodeProgram {
        code: enc.code,
        constants: enc.constants,
        global_entries,
        templates: base.templates.clone(),
        blackhole: base.blackhole,
        meta_template: base.meta_template,
        pap: base.pap.clone(),
        apply1_template: base.apply1_template,
        apply2_template: base.apply2_template,
        producer_tail_template: base.producer_tail_template,
    };
    (program, root_off)
}

#[cfg(test)]
mod tests {
    use super::super::program::{read_u32, read_u8};
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
        let ci = read_u32(&prog.code, &mut pc);
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
        assert_eq!(read_u32(&prog.code, &mut pc), 3);
    }

    #[test]
    fn encode_app_args_are_atom_offsets() {
        // app(gref(0), [num(7)]): the single arg is an offset to an OP_ATOM.
        let syn = dsl::app(dsl::gref(0), vec![dsl::num(7)]);
        let (prog, root, _) = encode(&syn, &[]);
        let mut pc = root as usize;
        assert_eq!(prog.code[pc], Op::App as u8);
        pc += 1;
        let flags = read_u8(&prog.code, &mut pc);
        assert_eq!(flags & FLAG_EAGER, 0);
        // callable inline ref: G(0)
        assert_eq!(read_u8(&prog.code, &mut pc), REF_G);
        assert_eq!(read_u32(&prog.code, &mut pc), 0);
        // one arg, as an offset preceding the parent
        let argc = read_u8(&prog.code, &mut pc);
        assert_eq!(argc, 1);
        let arg_off = read_u32(&prog.code, &mut pc);
        assert!(arg_off < root, "arg atom must precede the App node");
        // and it points at an OP_ATOM holding a V ref
        let mut apc = arg_off as usize;
        assert_eq!(read_u8(&prog.code, &mut apc), Op::Atom as u8);
        assert_eq!(read_u8(&prog.code, &mut apc), REF_V);
    }

    #[test]
    fn encode_case_dense_table() {
        // case local(0) of tag2 -> 10; tag4 -> 20; default -> 30.
        // Gap at tag3 → NO_BRANCH.
        let syn = dsl::case(
            dsl::local(0),
            vec![(2, dsl::atom(dsl::num(10))), (4, dsl::atom(dsl::num(20)))],
            dsl::atom(dsl::num(30)),
        );
        let (prog, root, _) = encode(&syn, &[]);
        let mut pc = root as usize;
        assert_eq!(prog.code[pc], Op::Case as u8);
        pc += 1;
        let scr_off = read_u32(&prog.code, &mut pc);
        assert!(scr_off < root);
        let min_tag = read_u8(&prog.code, &mut pc);
        let len = read_u8(&prog.code, &mut pc);
        assert_eq!(min_tag, 2);
        assert_eq!(len, 3); // tags 2,3,4
        let e2 = read_u32(&prog.code, &mut pc);
        let e3 = read_u32(&prog.code, &mut pc);
        let e4 = read_u32(&prog.code, &mut pc);
        assert!(e2 < root);
        assert_eq!(e3, NO_BRANCH);
        assert!(e4 < root);
        let has_fb = read_u8(&prog.code, &mut pc);
        assert_eq!(has_fb, 1);
        assert!(read_u32(&prog.code, &mut pc) < root);
    }

    #[test]
    fn encode_direct_app_carries_smid_and_eager_flag() {
        let smid = Smid::from(77);
        let syn = dsl::direct_app_eager(smid, dsl::gref(1), vec![dsl::num(5)]);
        let (prog, root, _) = encode(&syn, &[]);
        let mut pc = root as usize;
        assert_eq!(prog.code[pc], Op::DirectApp as u8);
        pc += 1;
        let flags = read_u8(&prog.code, &mut pc);
        assert_eq!(flags & FLAG_EAGER, FLAG_EAGER);
        let decoded_smid = Smid::from(read_u32(&prog.code, &mut pc));
        assert_eq!(decoded_smid, smid);
    }

    #[test]
    fn encode_direct_app_without_eager_clears_flag() {
        let smid = Smid::from(1);
        let syn = dsl::direct_app(smid, dsl::gref(0), vec![]);
        let (prog, root, _) = encode(&syn, &[]);
        let mut pc = root as usize + 1;
        let flags = read_u8(&prog.code, &mut pc);
        assert_eq!(flags & FLAG_EAGER, 0);
    }

    #[test]
    fn templates_encode_cons_shapes() {
        let (prog, _root, _) = encode(&dsl::atom(dsl::num(0)), &[]);
        assert_eq!(prog.templates.len(), 18);

        // ListCons (tag 7): OP_CONS, tag 7, arity 2.
        let mut pc = prog.templates[DataConstructor::ListCons.tag() as usize] as usize;
        assert_eq!(prog.code[pc], Op::Cons as u8);
        pc += 1;
        assert_eq!(
            read_u8(&prog.code, &mut pc),
            DataConstructor::ListCons.tag()
        );
        assert_eq!(read_u8(&prog.code, &mut pc), 2);

        // ListNil (tag 6): OP_CONS, tag 6, arity 0.
        let mut pc = prog.templates[DataConstructor::ListNil.tag() as usize] as usize;
        assert_eq!(prog.code[pc], Op::Cons as u8);
        pc += 1;
        assert_eq!(read_u8(&prog.code, &mut pc), DataConstructor::ListNil.tag());
        assert_eq!(read_u8(&prog.code, &mut pc), 0);

        // BoxedNumber (tag 3): arity 1.
        let mut pc = prog.templates[DataConstructor::BoxedNumber.tag() as usize] as usize + 1;
        assert_eq!(
            read_u8(&prog.code, &mut pc),
            DataConstructor::BoxedNumber.tag()
        );
        assert_eq!(read_u8(&prog.code, &mut pc), 1);
    }

    #[test]
    fn pap_templates_are_apps() {
        let (prog, _root, _) = encode(&dsl::atom(dsl::num(0)), &[]);
        // supplied=1, pending=1 → an App node.
        let off = prog.pap_offset(1, 1).unwrap();
        assert_eq!(prog.code[off as usize], Op::App as u8);
        // Out-of-range indices yield None.
        assert!(prog.pap_offset(0, 1).is_none());
        assert!(prog.pap_offset(1, 99).is_none());
    }

    #[test]
    fn encode_globals_recorded_in_entries() {
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
