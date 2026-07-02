# BV1: Full Bytecode VM Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking. This is a deep translation of intricate existing code, not greenfield — where a step says "translate `vm.rs:NNN-MMM`", read that arm and port its semantics; the differential harness (Phase 2) is the exact correctness gate for every such translation.

> ## STATUS (revised 2026-07-02)
>
> **Phase 0 (scaffolding) and Phase 1 (encoder + core types): COMPLETE** on
> `integration/0.12.0`, all lib tests green (1198) + `clippy --all-targets`
> clean. Commits `9cbb2a0c..10176820` (8). Delivered: `src/eval/bytecode/`
> {`mod`, `opcode`, `program` (incl. a fully-implemented `prepare_constants`,
> not the stub the plan called for), `encode`, `closure`, `cont`,
> `env_builder`}; `env.rs` generalised `EnvironmentFrame`'s `StgObject`/
> `GcScannable` over `C` + added `Closing::set_env` (behaviourally identical on
> the SynClosure path).
>
> **Design decisions settled during Phase 1** (now in spec §4.2 note / §5.5):
> - **Operand encoding:** args are pre-emitted `OP_ATOM` nodes referenced by
>   `u32` offset (lazy arg closure = `BcClosure(atom_off, env)`, zero
>   per-dispatch code alloc; ref recoverable at `atom_off+1`). Encoder rewritten
>   accordingly (commit `10176820`). Case tables densified at encode time.
> - **Deferred to later phases:** `create_arg_array`/`_eager`, `partially_apply`
>   (pap trampoline), `from_let`/`from_letrec` builders — completed when their
>   dispatch arms are written.
>
> **NEW Phase 1.5 (intrinsic layer) inserted before Phase 2.** Implementation
> review found spec §3's "192 intrinsics reused for free" is wrong: the intrinsic
> ABI is `SynClosure`-typed and `support.rs` return helpers synthesise `HeapSyn`
> at runtime. This blocks *every* `Bif` (hence day11) and must be solved before
> the dispatch loop. See the new Phase 1.5 section and spec §5.5. This is real,
> previously-unscoped work — expect Phase 1.5 to be comparable in size to Phase 2.
>
> **Update (later 2026-07-02).** Phase 1.5 progress: neutral scalar + closure
> intrinsic ABI added and `force`/`eq`/`arith`/`string`/`array` migrated
> (HeapSyn byte-identical throughout); **Task 1.5.2 (arena constructor
> templates) done**. A second spec correction surfaced: **the value model is
> `BcValue = Closure | Native`, not `Closing<CodeRef>`** (a runtime native has no
> home in an off-heap code offset). **NEW Phase 1.6** reworks the Phase 1 closure
> types to `BcValue` before the machine. See spec §3 revision.

## Progress Ledger (living — durable source of truth; update every increment)

**Position (2026-07-02):** Phases 0, 1, 1.6 complete; Phase 1.5 (intrinsic ABI)
partially done; **pivoting to Phase 2 (bytecode machine)**, completing the
remaining intrinsic migrations alongside it and verifying via the differential
harness. **Branch:** `integration/0.12.0` (worktree
`eucalypt-worktrees/integration-0.12.0`). **Toolchain:** prepend
`~/.rustup/toolchains/stable-aarch64-apple-darwin/bin` to PATH. Nothing pushed.

### Done (commit → what)
- `9cbb2a0c` Phase 0 scaffold (module + `EU_BYTECODE` flag)
- `31973ca3` opcodes; `2bb05deb` `BytecodeProgram` + LE readers
- `5bc6a387` encoder → `10176820` reworked to **atom-offset operands + dense case table**
- `9eb97f4a` `BcClosure` + GC + **`env.rs` generalised `EnvironmentFrame` `StgObject`/`GcScannable` over `C` + `Closing::set_env`**
- `ba690ad1` `BcContinuation`; `fa965358` `BcEnvBuilder`
- `44b88d03` **spec/plan revision 1** (intrinsics not free → Phase 1.5)
- `cbcc5c3b` Increment A (neutral scalar ABI + `support.rs` scalars)
- `9bb9c0c1` Increment B (set/vec/ndarray → `return_native`)
- `ef5fc399` C1 (`AbiClosure` + neutral closure ABI)
- `a81f4356` migrate force/eq/arith; `d5bdaf50` string/array
- `3e144b15` **Task 1.5.2 constructor templates** (`BytecodeProgram.templates[tag]`)
- `d21ece03` **spec/plan revision 2** (`BcValue` value model → Phase 1.6)
- `372c4b08` **Phase 1.6** `BcValue` rework (closure/cont/env_builder)
- `7c2c665f` `&self` relax + `AbiClosure::arity` + migrate render/expect
- `9f5c952b` this progress ledger (durable tracking)
- `1a1aa3c6` **Phase 2 start** — `machine.rs`: `DecodedRef`, `read_ref`, `resolve_ref`/`resolve_native` (ref → `BcValue`)
- `cf2b3e34` Phase 2 — operand decoders (`read_arg_offsets`/`read_branch_table`/`read_form_header`, round-tripped vs encoder) + `BcMachineState` struct
- `0b66403b` Phase 2 — `return_native` handler (WHNF native → continuation; Branch/Update/DeMeta/SeqBind/LookupLitForce/CaptureEnd), tested. Building return_* handlers as standalone tested units before the full loop.
- `0bd7b7f9` Phase 2 — `return_data` handler (branch match + copy-path `env_from_data_args`; Update/DeMeta/SeqBind/CaptureEnd/IO-yield), tested. STUBBED (need globals + bytecode block navigator): ApplyTo block-application → MERGE, LookupLitForce block-lookup — wire with the machine loop. Also deferred: shared-backing env optimisation (perf); `nearest_stack_annotation` fallback (used `state.annotation` — revisit if differential harness flags error-location diffs).
- `0ddd1075` Phase 2 — `return_fun` handler (ApplyTo exact→saturate / over→saturate+push surplus; Branch fallback→`CannotReturnFunToCase`; Update/DeMeta/SeqBind/LookupLitForce/CaptureEnd), tested. STUBBED: PAP (ApplyTo Less) — needs pap trampoline templates (next).
- `6f005e44` Phase 2 — **`step` dispatch + `handle_op` — FIRST END-TO-END EXECUTION**. `step`: Native→return_native / Closure arity>0→return_fun / arity0→handle_op. `handle_op` arms: Atom(L/G/V), Cons, Case, Seq, Ann, BlackHole. Two end-to-end tests run through `step`: `atom(42)`→Native 42; `case nil→7`.
- `0a010546` Phase 2 — **App arm + thunk memoisation + blackhole template + Let/LetRec**. `make_arg_array` (lazy/eager CG3), `enter_local`/`enter_callable` (blackhole slot + push Update for thunks), blackhole template in the arena (`BytecodeProgram.blackhole`, `state.blackhole`). End-to-end: `let x=5 in x`→5; `let id=\x.x in id(5)`→5.
- `2c48cf2f` Phase 2 — **DirectApp arm** (CG1: inline smid, exact-arity fast-path saturate skipping ApplyTo, thunk/fallback). End-to-end: `id(5)` via DirectApp→5.
- `0b6cb09a` Phase 2 — **Bif arm (capture)**: decode intrinsic index + arg refs into `pending_bif`/`pending_bif_args` (spec §6.3) for deferred dispatch. Tested (capture only; dispatch pending — see below).

### ARCHITECTURAL BOUNDARY (2026-07-02) — handlers need machine/program context
The self-contained handler arms are done (Atom/Cons/Case/Seq/Ann/App/DirectApp/Let/LetRec + all three return_* + thunk memoisation). The machine executes real expressions end-to-end. The REMAINING features all need broader context than the current free-fn handlers `(state, view, code)` take:
- **Bif dispatch**: needs a `BifContext` impl of `IntrinsicMachine` over `BcValue`+templates, the intrinsics slice, symbol pool, and the templates table. (Trait strategy — an implementation call, no user decision needed: the bytecode `BifContext` will impl the neutral methods over `BcValue` and `panic!`/error on the 5 `SynClosure`-typed methods (`nav`/`set_closure`/`env`/`root_env`/`evaluate_to_whnf`); intrinsics still using those on the bytecode path will surface via the differential harness and get migrated — avoids gating on finishing Increment C/D.)
- **PAP** (return_fun Less): needs pap trampoline templates (`App(L(pending),[…])` per (supplied,pending)) in the program → return_fun needs the program.
- **Data construction** (`return_data`/`machine_return_*` on the bytecode path): needs the templates table.
So the next unit is the **machine wiring**: a `BytecodeMachine`/run-context owning the `BytecodeProgram` (code/constants/templates/blackhole/pap/global_entries) + heap + intrinsics + emitter + metrics + symbol pool; build the globals frame of `BcClosure`s from the encoded `GlobalForm`s (intrinsic wrappers + prelude — needs prelude encoded to bytecode); refactor handlers to take/deref this context; then Bif dispatch, PAP, Meta/DeMeta/LookupLit arms, `EU_BYTECODE` flag path in `driver/eval.rs`, and the differential harness → day11 gate.

- `1fda104d` Phase 2 — **PAP trampoline templates** in the arena (`BytecodeProgram.pap` + `pap_offset(supplied,pending)`, `PAP_MAX_ARITY=16`). Ready to wire into `return_fun`'s Less case once handlers take the program.
- `7c4c84d9` **fix(deps)** — bump `quick-xml` 0.25→0.41 (RUSTSEC-2026-0194/0195, newly-published, unrelated to BV1) + migrate `src/import/xml.rs` API. Fixed the PR's Security Audit CI failure. XML harness test byte-identical; `cargo audit` clean.

### Machine-wiring integration — ordered plan + two refinements to settle in-flight
The core machine is complete + tested (all return_* handlers; Atom/Cons/Case/Seq/Ann/App/DirectApp/Let/LetRec; thunk memoisation; Bif capture; data + blackhole + pap templates; value model + GC + decoders). Remaining is one coherent integration:
1. **Refactor** `step`/`handle_op`/`return_fun` to take `&BytecodeProgram` (not `&[u8]`) so handlers reach `templates`/`blackhole`/`pap`. (Mechanical; update tests.)
2. **Wire PAP** (`return_fun` Less) via `prog.pap_offset` + a `partially_apply` building the PAP closure env `[f, supplied…]`.
3. **`BytecodeMachine`** owning heap/intrinsics/emitter/metrics/symbol-pool + the program + `BcMachineState`; `run` loop (500-tick GC poll; `pending_bif` dispatch tail; capture lifecycle).
4. **Bif dispatch** — a `BcBifContext` impl of `IntrinsicMachine`: neutral methods over `BcValue`+templates; the 5 `SynClosure`-typed methods `panic!` (bytecode never calls them; unmigrated intrinsics surface via the harness). Convert `pending_bif_args` (`DecodedRef`) → `&[Ref]` for `execute`.
   - **REFINEMENT A (resolve_closure vs native):** `AbiClosure` is `Heap(SynClosure)|Byte(BcClosure)`, but a bytecode ref can resolve to a bare `Native` (not a closure). Plan: change `AbiClosure::Byte` to hold a `BcValue` (Closure|Native) so `resolve_closure`/`set_result`/`force` round-trip natives too. (My call; implement during Bif dispatch.)
5. **Globals + prelude**: build the globals frame from `GlobalForm`s (intrinsic wrappers) + encode the prelude to bytecode (Task 3.2). 
   - **REFINEMENT B (harness-first):** stand up the differential harness on tiny **synthetic** programs (no prelude) first — validate the core end-to-end through both engines — *before* encoding the full prelude. (My call; do harness-first.)
6. **Meta/DeMeta/LookupLit** arms (+ `return_meta`, bytecode block navigator for LookupLit/MERGE).
7. **`EU_BYTECODE` flag path** in `driver/eval.rs`; full differential harness → day11 gate (§8).

### Phase 2 remaining (updated)
- [ ] PAP trampoline templates in the arena + wire `return_fun` Less case + `partially_apply`.
- [ ] `handle_op` arms (day11): Atom, App, DirectApp, Bif, Case, Cons, Let/LetRec, Seq, LookupLit — each fetches operands (decoders done) and produces/consumes via the return_* handlers (done).
- [ ] value-dispatch `step`: `Native`→`return_native`; `Closure` arity>0→`return_fun`; else fetch opcode→`handle_op`. 500-tick GC poll + `pending_bif` tail.
- [ ] `BytecodeMachine` wiring (heap/intrinsics/emitter/metrics; build globals frame from `GlobalForm`s + `prepare_constants`); neutral `IntrinsicMachine` impl over `BcValue`+templates; `evaluate_to_whnf`.
- [ ] `EU_BYTECODE` flag path (`driver/eval.rs`) + differential harness + day11 gate.

### Phase 2 next steps (fine-grained)
- [ ] `BytecodeMachine` wiring: own/borrow heap+intrinsics+emitter+metrics (mirror `MachineCore`/`Machine`); hold `BcMachineState`; construct globals frame from `GlobalForm`s + `prepare_constants`.
- [ ] `step`: value-dispatch — `current` `Native(n)` → `return_native(n)`; `Closure(c)` with arity>0 → `return_fun`; else fetch opcode at `c.code()` → `handle_op`. Plus 500-tick GC poll, `pending_bif` tail.
- [ ] `handle_op` arms (day11 subset): Atom, App, DirectApp, Bif, Case, Cons, Let/LetRec, Seq, LookupLit; `return_*` for the BcContinuation kinds.
- [ ] neutral `IntrinsicMachine` impl for the bytecode bif context (`set_result`/`resolve_*`/`data_*`/`return_*` over `BcValue`+templates); `evaluate_to_whnf`.
- [ ] `EU_BYTECODE` flag path in `driver/eval.rs`; differential harness; day11 byte-identical gate.

### Neutral `IntrinsicMachine` ABI available (`intrinsic.rs`)
`resolve_native`, `return_native/unit/boxed_num/bool`, `resolve_closure`,
`resolve_callable_closure`, `set_result`, `force`, `data_tag`, `data_field`,
`field_native`; `AbiClosure{Heap,Byte}` + `arity`/`as_heap`/`expect_heap`.
`BcValue = Closure(BcClosure) | Native(Native)`; `BcEnvFrame = EnvironmentFrame<BcValue>`.

### Remaining (checklist)
- [ ] **Phase 2** — `BytecodeMachine` (state over `BcValue`; `run`/`step`/`handle_op`;
  `pending_bif`; `evaluate_to_whnf`); its neutral `IntrinsicMachine` impl (`BcValue`
  over templates); flag path; **differential harness**; day11 byte-identical gate.
- [ ] **Finish Increment C alongside Phase 2** — `return_data` + `machine_return_*_list`
  over templates; `DataIterator`/`StrListIterator` neutral redesign; migrate
  block/debug/list/parse_string/typedata/set/vec/stream_prng/etc.
- [ ] **Increment D** — move `nav`/`set_closure`/`env`/`root_env`/`evaluate_to_whnf`
  to a HeapSyn-only sub-trait; make neutral methods required (no default).
- [ ] **Phase 3** — full opcode coverage + prelude encoded + full-harness differential
  + GC stress + no-regression (acceptance §8).
- [ ] **Phase 4** — collapse: delete HeapSyn machine/loader/flag.

---

**Goal:** Replace the `HeapSyn` tree-walk execution IR with a native flat-bytecode machine so compiled code leaves the GC-scanned heap, built as a parallel engine behind a flag and cross-checked against the HeapSyn engine by differential testing.

**Architecture:** A new `src/eval/bytecode/` module: an encoder (`StgSyn`/`ArenaStgSyn` → flat `Vec<u8>` + constant pool), a `BytecodeMachine` reusing the existing heap/GC/env/intrinsics/emitter with a `match u8` dispatch loop, closures typed `BcClosure = Closing<CodeRef>` (`CodeRef = u32`), and a parallel `BcContinuation`. Selected by a flag; the HeapSyn machine is deleted in Phase 4 once the full harness passes byte-identically through both engines.

**Tech Stack:** Rust; the existing Immix GC (`src/eval/memory/`), STG compiler + arena (`src/eval/stg/`), and STG machine (`src/eval/machine/`). `postcard` (only relevant later for BV5, not here).

## Global Constraints

- **Branch:** all work on `integration/0.12.0` (disposable). Do NOT open PRs to master; commit directly to the integration branch.
- **Spec:** `docs/superpowers/specs/2026-07-01-bv1-bytecode-vm-design.md` — the authority; this plan implements it. Re-read §3–§7 before Phase 1.
- **UK English** in all comments/docs (optimise, colour, behaviour).
- **No clippy warnings** — `cargo clippy --all-targets -- -D warnings` must stay clean at every commit (CLAUDE.md, non-negotiable).
- **`cargo fmt --all`** before every commit.
- **Wrap every `eu` run in `timeout`** (e.g. `timeout 120 ./target/release/eu …`); default heap limit is fine.
- **Rust toolchain:** the pinned stable (`/Users/greg/.rustup/toolchains/stable-aarch64-apple-darwin/bin` — `cargo` is a broken symlink under `~/.cargo/bin`, so ensure that toolchain bin is on PATH).
- **Acceptance bar (spec §8):** correctness + no-regression. Byte-identical corpus via differential testing; `EU_GC_VERIFY=2` clean; code off the scan set; no corpus regression. A dispatch win is a bonus, not required.
- **No new surface syntax, no new intrinsics** — this is a pure execution-engine change; observable output must be byte-identical.

---

## File Structure

**New module `src/eval/bytecode/`:**
- `mod.rs` — module root; `CodeRef` type; `pub use` re-exports; the `bytecode_enabled()` flag reader.
- `opcode.rs` — `Op` (`#[repr(u8)]` enum), ref-tag bytes, flag-bit constants; `u8`↔`Op` helpers.
- `program.rs` — `BytecodeProgram { code, constants, global_entries }`; LE `read_u8/u16/u32` readers; `prepare_constants` (stg `Native` → heap `Ref`, interning/allocating once).
- `encode.rs` — `Encoder` walking `StgSyn`/`ArenaStgSyn` → `BytecodeProgram`; post-order, `u32` offset back-refs; ports the BV0 encoder (`git show spike/bv0-bytecode:src/eval/bytecode/encode.rs`), natively (no HeapSyn stubs).
- `closure.rs` — `BcClosure = Closing<CodeRef>`; `GcScannable for BcClosure` (env only, no code fixup); `BcClosure` constructors mirroring `env.rs:47-73`.
- `cont.rs` — `BcContinuation` (7 variants, `CodeRef` in place of `RefPtr<HeapSyn>`); `match_tag`; `GcScannable` (heap values only).
- `env_builder.rs` — `BcEnvBuilder`: `create_arg_array`/`_eager`, `from_let`/`from_letrec`, `saturate*` for `BcClosure` (mirrors `src/eval/machine/env_builder.rs`).
- `machine.rs` — `BytecodeMachine`: `run`/`step`/`handle_op` dispatch loop, `evaluate_to_whnf`, `pending_bif` capture, the `return_*` handlers; reuses `MachineState` heap/globals/stack/intrinsics.
- `differential.rs` (test support) — run a unit through both engines, compare stdout/stderr/exit.

**Touch points in existing code (minimal until Phase 4):**
- `src/eval/mod.rs` — add `pub mod bytecode;`.
- `src/eval/machine/env.rs` — `EnvironmentFrame<C>` is already generic; confirm `EnvironmentFrame<BcClosure>` instantiates (StgObject/GcScannable). No behaviour change.
- `src/driver/eval.rs` / `src/driver/options.rs` — the flag plumbing (Phase 2) and the encode-and-run-bytecode path.
- Phase 4 only: delete `src/eval/memory/syntax.rs` `HeapSyn` + its `GcScannable`, `src/eval/memory/loader.rs`, the HeapSyn dispatch in `src/eval/machine/vm.rs`, and the flag.

---

## Phase 0 — Scaffolding (module + flag, no behaviour)

**Milestone:** the crate builds with an empty `bytecode` module and a wired-but-inert flag; full existing test suite still green.

### Task 0.1: Create the module skeleton

**Files:**
- Create: `src/eval/bytecode/mod.rs`
- Modify: `src/eval/mod.rs` (add `pub mod bytecode;`)

**Interfaces:**
- Produces: `pub type CodeRef = u32;`, `pub fn bytecode_enabled() -> bool`.

- [ ] **Step 1:** Create `src/eval/bytecode/mod.rs`:

```rust
//! Native flat-bytecode execution engine (BV1). See
//! docs/superpowers/specs/2026-07-01-bv1-bytecode-vm-design.md.

/// A code reference: a byte offset into a `BytecodeProgram.code` buffer.
/// Unlike `RefPtr<HeapSyn>`, a `CodeRef` never moves, so it carries zero
/// GC cost — closures holding one need no `scan_and_update` code fixup.
pub type CodeRef = u32;

/// Whether the bytecode engine is selected for this run. Reads the
/// `EU_BYTECODE` env var; a CLI flag is added in Phase 2.
pub fn bytecode_enabled() -> bool {
    std::env::var("EU_BYTECODE").as_deref() == Ok("1")
}
```

- [ ] **Step 2:** Add `pub mod bytecode;` to `src/eval/mod.rs` (alphabetical with the other `pub mod` lines).

- [ ] **Step 3:** Build.

Run: `cargo build 2>&1 | tail -5`
Expected: `Finished` (a dead-code warning on `bytecode_enabled` is acceptable *only* this task; silence it by `#[allow(dead_code)]` on the fn with a `// removed when wired in Phase 2` comment to keep clippy clean).

- [ ] **Step 4:** Full lint + test to prove no regression.

Run: `cargo clippy --all-targets -- -D warnings 2>&1 | tail -3 && cargo test --lib 2>&1 | grep "test result:"`
Expected: clippy clean; lib tests pass.

- [ ] **Step 5:** Commit.

```bash
git add src/eval/bytecode/mod.rs src/eval/mod.rs
git commit -m "feat(bytecode): scaffold bytecode module + EU_BYTECODE flag reader (eu-vi3a)"
```

---

## Phase 1 — Encoder + core types (no execution)

**Milestone:** `StgSyn` encodes to a `BytecodeProgram`; the closure/continuation types instantiate and GC-scan correctly; round-trip *structural* tests pass. Nothing executes yet.

### Task 1.1: Opcode + operand encoding constants

**Files:**
- Create: `src/eval/bytecode/opcode.rs`
- Modify: `src/eval/bytecode/mod.rs` (`mod opcode; pub use opcode::*;`)

**Interfaces:**
- Produces: `#[repr(u8)] pub enum Op {…}`; `pub const REF_L/REF_G/REF_V: u8`; `pub const FLAG_EAGER: u8`; `pub const FORM_LAMBDA/THUNK/VALUE: u8`; `impl Op { fn from_u8(u8) -> Option<Op> }`.

- [ ] **Step 1:** Write `opcode.rs`. Mirror the spec §4.2 table and the BV0 vocabulary (`git show spike/bv0-bytecode:src/eval/bytecode/opcode.rs`). One `u8` per `StgSyn` variant plus ref-tag and form-kind bytes:

```rust
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Atom = 0x01, Case = 0x02, Cons = 0x03, App = 0x04, DirectApp = 0x05,
    Bif = 0x06, Let = 0x07, LetRec = 0x08, Ann = 0x09, Meta = 0x0A,
    DeMeta = 0x0B, Seq = 0x0C, LookupLit = 0x0D, BlackHole = 0x0E,
}
impl Op {
    pub fn from_u8(b: u8) -> Option<Op> { /* match each discriminant */ }
}
pub const REF_L: u8 = 0x00; pub const REF_G: u8 = 0x01; pub const REF_V: u8 = 0x02;
pub const FLAG_EAGER: u8 = 0b0000_0001;
pub const FORM_LAMBDA: u8 = 0x00; pub const FORM_THUNK: u8 = 0x01; pub const FORM_VALUE: u8 = 0x02;
```

- [ ] **Step 2:** Add a unit test `op_roundtrips_through_u8` asserting `Op::from_u8(op as u8) == Some(op)` for every variant.

- [ ] **Step 3:** `cargo test --lib bytecode::opcode 2>&1 | grep "test result:"` — Expected: PASS.

- [ ] **Step 4:** Commit.

```bash
git add src/eval/bytecode/opcode.rs src/eval/bytecode/mod.rs
git commit -m "feat(bytecode): opcode + operand encoding constants (eu-vi3a)"
```

### Task 1.2: `BytecodeProgram` container + readers

**Files:**
- Create: `src/eval/bytecode/program.rs`
- Modify: `src/eval/bytecode/mod.rs`

**Interfaces:**
- Consumes: `CodeRef`.
- Produces: `pub struct BytecodeProgram { pub code: Vec<u8>, pub constants: Vec<stg::syntax::Native>, pub global_entries: Vec<CodeRef> }`; `#[inline(always)]` `read_u8/read_u16/read_u32(&[u8], &mut usize)`; `pub fn prepare_constants(&self, view) -> Vec<memory::Ref>` (stub returning `vec![]` this task; filled in Task 3.x when constants are executed).

- [ ] **Step 1:** Write the struct + LE readers (port `git show spike/bv0-bytecode:src/eval/bytecode/program.rs`). Use `stg::syntax::Native` as the constant element type (matches the encoder's source; strings/syms are decimal/interned there).

- [ ] **Step 2:** Unit test `readers_are_little_endian`: build a `code` buffer with known bytes, assert `read_u32` returns the expected value and advances `pc` by 4.

- [ ] **Step 3:** `cargo test --lib bytecode::program 2>&1 | grep "test result:"` — Expected: PASS.

- [ ] **Step 4:** Commit.

```bash
git add src/eval/bytecode/program.rs src/eval/bytecode/mod.rs
git commit -m "feat(bytecode): BytecodeProgram container + LE readers (eu-vi3a)"
```

### Task 1.3: The encoder (`StgSyn`/`ArenaStgSyn` → `BytecodeProgram`)

**Files:**
- Create: `src/eval/bytecode/encode.rs`
- Modify: `src/eval/bytecode/mod.rs`

**Interfaces:**
- Consumes: `Op`, ref/form constants, `BytecodeProgram`, `stg::syntax::{StgSyn, Ref, LambdaForm, Native}`, `stg::arena::StgArena`.
- Produces: `pub struct Encoder { … }`; `pub fn encode(root: &Rc<StgSyn>, globals: &[LambdaForm]) -> (BytecodeProgram, CodeRef, Vec<GlobalForm>)` where `pub struct GlobalForm { kind: u8, arity: u8, smid: Smid, entry: CodeRef }`. Post-order: emit children first, parents back-reference by absolute `u32` offset (reuse `StgArena` flatten order, `arena.rs:190-201`).

- [ ] **Step 1:** Port the BV0 encoder body, adapting it to emit the natively-typed operands (no HeapSyn concepts). For each `StgSyn` variant, emit `Op` byte + operands per the spec §4.2 table. Emit `Ref` via a helper `emit_ref(&mut self, &Ref)` → tag byte + payload; push `Native` into `constants` and emit its `REF_V` index. Emit lambda-form headers (kind/arity/annotation/body-offset).

- [ ] **Step 2:** Unit test `encode_atom_v_number`: encode `StgSyn::Atom { evaluand: Ref::V(Native::num(42)) }`; assert `code[root]` is `Op::Atom as u8`, followed by `REF_V` and a constant index whose pool entry is `Native::num(42)`.

- [ ] **Step 3:** Unit test `encode_case_branches_are_ordered`: encode a small `Case` with two branches + fallback; assert the branch table maps `tag → offset` and every referenced offset is `< code.len()` and `< the parent's own offset` (children precede parents).

- [ ] **Step 4:** Unit test `encode_direct_app_carries_smid_and_eager_flag`: encode a `DirectApp { smid, …, eager_args: true }`; assert the inline smid round-trips and `FLAG_EAGER` is set.

- [ ] **Step 5:** `cargo test --lib bytecode::encode 2>&1 | grep "test result:"` — Expected: PASS.

- [ ] **Step 6:** `cargo clippy --all-targets -- -D warnings 2>&1 | tail -3` — Expected: clean. Then commit.

```bash
git add src/eval/bytecode/encode.rs src/eval/bytecode/mod.rs
git commit -m "feat(bytecode): StgSyn -> bytecode encoder (eu-vi3a)"
```

### Task 1.4: `BcClosure` + its GC scanning

**Files:**
- Create: `src/eval/bytecode/closure.rs`
- Modify: `src/eval/bytecode/mod.rs`

**Interfaces:**
- Consumes: `CodeRef`; `Closing<S>` (`env.rs:19`), `InfoTagged` (`infotable.rs:43`), `EnvironmentFrame` (`env.rs:191`).
- Produces: `pub type BcClosure = Closing<CodeRef>;`; constructors `new`, `new_annotated`, `new_annotated_lambda`, `close` (mirror `env.rs:47-73` for `S = CodeRef`); `impl GcScannable for BcClosure` scanning **only the env pointer** (no `forwarded_to(code())` — offsets don't move).

- [ ] **Step 1:** Define `BcClosure = Closing<CodeRef>`. Confirm `Closing<S>`'s constructors are generic (they take `S`); if some are `impl SynClosure`-specific, add the `CodeRef` equivalents in `closure.rs`.

- [ ] **Step 2:** Write `impl GcScannable for BcClosure`: `scan` marks/pushes the env frame (`self.env()`); `scan_and_update` calls `forwarded_to` **only on the env pointer**, never the code field. (Contrast `SynClosure::scan_and_update`, `env.rs:484-491`, which fixes up both.)

- [ ] **Step 3:** Unit test `bcclosure_scan_ignores_code_field`: construct a `BcClosure` with a bogus large `CodeRef` and a real (allocated) env; run `scan_and_update` under a heap view; assert it does not panic / touch the code offset and leaves it unchanged. (Use the existing GC test harness patterns — grep `mod tests` in `env.rs` for the setup.)

- [ ] **Step 4:** `cargo test --lib bytecode::closure 2>&1 | grep "test result:"` — Expected: PASS.

- [ ] **Step 5:** Commit.

```bash
git add src/eval/bytecode/closure.rs src/eval/bytecode/mod.rs
git commit -m "feat(bytecode): BcClosure = Closing<CodeRef> + GC scan (env-only) (eu-vi3a)"
```

### Task 1.5: `BcContinuation`

**Files:**
- Create: `src/eval/bytecode/cont.rs`
- Modify: `src/eval/bytecode/mod.rs`

**Interfaces:**
- Consumes: `CodeRef`, `BcClosure`, `EnvironmentFrame<BcClosure>`, `Tag`, `Smid`, `SymbolId`, `Array`.
- Produces: `pub enum BcContinuation { Branch { min_tag, branch_table: Array<Option<CodeRef>>, fallback: Option<CodeRef>, environment, annotation }, Update { environment, index }, ApplyTo { args: Array<BcClosure>, annotation }, DeMeta { handler: CodeRef, or_else: CodeRef, environment }, SeqBind { body: CodeRef, environment, annotation }, LookupLitForce { key: SymbolId, smid, default_closure: BcClosure }, CaptureEnd }`; `fn match_tag(&self, tag) -> Option<CodeRef>` (mirror `cont.rs:101-111`); `impl GcScannable for BcContinuation` (heap values only — envs, `ApplyTo`/`LookupLitForce` closure envs, `Array` backings; `CodeRef` fields inert).

- [ ] **Step 1:** Define the enum (mirror `src/eval/machine/cont.rs:34-96`, swapping `RefPtr<HeapSyn>` → `CodeRef`).

- [ ] **Step 2:** Implement `match_tag` and the `GcScannable` impl (mirror `cont.rs:173-356` but omit code-pointer marking; keep the `OpaqueHeapBytes` registration for `branch_table`/`args` backings — those are still heap `Array`s).

- [ ] **Step 3:** Unit test `branch_match_tag_indexes_table`: build a `Branch` with `min_tag=5` and a 2-entry table; assert `match_tag(6)` returns the second entry and `match_tag(99)` returns `None`.

- [ ] **Step 4:** `cargo test --lib bytecode::cont 2>&1 | grep "test result:"` — Expected: PASS.

- [ ] **Step 5:** Commit.

```bash
git add src/eval/bytecode/cont.rs src/eval/bytecode/mod.rs
git commit -m "feat(bytecode): BcContinuation (7 kinds, CodeRef bodies) + GC scan (eu-vi3a)"
```

### Task 1.6: `BcEnvBuilder`

**Files:**
- Create: `src/eval/bytecode/env_builder.rs`
- Modify: `src/eval/bytecode/mod.rs`

**Interfaces:**
- Consumes: `BcClosure`, `EnvironmentFrame<BcClosure>`, `BytecodeProgram` (to resolve arg refs), the heap view.
- Produces: `BcEnvBuilder` with `create_arg_array`, `create_arg_array_eager`, `from_let`, `from_letrec`, `from_saturation`, `saturate`, `saturate_with_array`, `partially_apply`, `pap` — the `BcClosure` analogues of `src/eval/machine/env_builder.rs:113-320`. `atom(ref)` builds a `BcClosure` whose code points at an `OP_ATOM` for the ref (or resolves `Ref::L` eagerly in `_eager`).

- [ ] **Step 1:** Port `env_builder.rs`'s builders, typed on `BcClosure`. Note: `create_arg_array` today wraps each arg as `SynClosure::new(atom(syn), env)` (`env_builder.rs:266-280`); the bytecode analogue wraps each as a `BcClosure` whose `CodeRef` points at a pre-encoded `OP_ATOM` for that ref (the encoder must emit these atom stubs for app args, OR the builder synthesises a tiny per-arg atom offset — decide during Task 2.x when the App arm is written; prefer the encoder emitting arg-atoms so no runtime encoding happens).

- [ ] **Step 2:** Unit test `saturate_reuses_code_with_new_env`: build a `BcClosure` with arity 1 and an env; `saturate_with_array` with one arg; assert the result closure's `code()` equals the callee's `code()` and its env is a fresh frame chaining the arg onto the callee env. (Mirror the assertion style of `env_builder.rs` tests if present.)

- [ ] **Step 3:** `cargo test --lib bytecode::env_builder 2>&1 | grep "test result:"` — Expected: PASS.

- [ ] **Step 4:** `cargo clippy --all-targets -- -D warnings` clean; commit.

```bash
git add src/eval/bytecode/env_builder.rs src/eval/bytecode/mod.rs
git commit -m "feat(bytecode): BcEnvBuilder (arg arrays, let/letrec, saturation) (eu-vi3a)"
```

---

## Phase 1.5 — Intrinsic layer (parameterise ABI + constructor templates)

**Added 2026-07-02.** Prerequisite for any `Bif` dispatch (spec §5.5). The
intrinsic ABI (`IntrinsicMachine`, `StgIntrinsic::execute`, `HeapNavigator`) and
the `support.rs` `machine_return_*` helpers are concretely `SynClosure`-typed and
allocate `HeapSyn` at runtime. Goal: make them code-type-agnostic so a `Bif` in
the bytecode engine produces/consumes `BcClosure`s, with runtime data built from
fixed constructor templates in the arena (no runtime code synthesis, no HeapSyn).

**Milestone:** the whole intrinsic layer is generic over the code type; the
HeapSyn engine still passes the full lib+harness suite **byte-identically** (the
generic instantiation must be a no-op refactor for `S = RefPtr<HeapSyn>`); the
bytecode engine has a working `machine_return_*` + navigator producing
`BcClosure`s over templates. No bytecode execution yet (that is Phase 2).

### Task 1.5.1: Audit the intrinsic coupling surface
- [ ] Enumerate every `IntrinsicMachine` method and every `support.rs` helper
  that names `SynClosure`/`EnvFrame`/`HeapSyn`/`HeapNavigator`. Record which
  construct data (need templates) vs. which only move closures (need only the
  type parameter). Output: a checklist of call sites.

### Task 1.5.2: Constructor templates in the arena
- [ ] Extend the encoder/program so the arena carries a fixed template block:
  an `OP_ATOM` scalar-return template, an `OP_CONS` per `(tag, arity)` shape the
  return helpers need (unit, block, list cons/nil, bool via global, etc.), and
  record their offsets in `BytecodeProgram` (e.g. `templates: Templates`).
- [ ] Unit test: entering a cons template over an env `{head, tail}` returns the
  expected data tag/fields (once the dispatch loop exists this becomes a
  differential check; until then assert the encoded template shape).

### Task 1.5.3: Parameterise the ABI over the code type
- [ ] Make `IntrinsicMachine`, `HeapNavigator`, and `MachineBifContext` generic
  over `S: Copy` (or add `BcClosure` siblings), so `set_closure`/`nav`/`env`/
  `evaluate_to_whnf` operate on `Closing<S>`. Keep the HeapSyn engine compiling
  and byte-identical (`S = RefPtr<HeapSyn>`).
- [ ] `cargo test --lib` + full harness on the HeapSyn path: **byte-identical**,
  no regression. This is the gate for the refactor being a no-op on HeapSyn.

### Task 1.5.4: Parameterise `support.rs` return + arg helpers
- [ ] Make `machine_return_*` and the arg extractors (`num_arg`, `str_arg`, …)
  generic; data-constructing returns build `Closing<S>` via the templates rather
  than `view.alloc(HeapSyn::…)`. HeapSyn path stays byte-identical.
- [ ] Commit per coherent group; keep clippy clean.

**Phase 1.5 gate:** HeapSyn engine byte-identical across lib + full harness; the
generic intrinsic layer compiles for both `S = RefPtr<HeapSyn>` and
`S = CodeRef`. Only then start Phase 2.

---

## Phase 1.6 — Value model (`BcValue`) rework

**Added 2026-07-02 (spec §3 revision).** A runtime native cannot live in an
off-heap `CodeRef`, so the bytecode value model is
`BcValue = enum { Closure(BcClosure), Native(Native) }`, with `BcClosure` a
bespoke struct `{ info: InfoTagged<CodeRef>, env: RefPtr<EnvironmentFrame<BcValue>> }`.
Phase 1 built `BcClosure = Closing<CodeRef>` as a first cut; this phase reworks it.

**Milestone:** `BcValue`/`BcClosure` defined; `EnvironmentFrame<BcValue>`,
`BcContinuation`, and `BcEnvBuilder` use them; `GcScannable` for all three; Phase 1
unit tests updated and green. Still no execution.

### Task 1.6.1: Define `BcValue` + bespoke `BcClosure`
- [ ] Replace `pub type BcClosure = Closing<CodeRef>` with a struct carrying
  `InfoTagged<CodeRef>` + `RefPtr<EnvironmentFrame<BcValue>>`; define
  `enum BcValue { Closure(BcClosure), Native(Native) }`. Provide the constructors
  the builders need (`new`, `new_annotated`, `new_annotated_lambda`, `close`,
  `code`, `env`, `set_env`, arity/update/annotation via `InfoTable`).

### Task 1.6.2: GC scanning for `BcValue`/`BcClosure`
- [ ] `GcScannable for BcClosure` (env only; code inert) and `for BcValue`
  (`Closure` → delegate; `Native` → mark heap-pointer natives). Update the
  `closure.rs` `bcclosure_code_field_is_inert` test to the new type.

### Task 1.6.3: Propagate through `cont.rs` / `env_builder.rs`
- [ ] `BcContinuation` env/args fields use `EnvironmentFrame<BcValue>` /
  `Array<BcValue>`; `BcEnvBuilder` builds `BcValue` slots. Update the Phase 1
  unit tests (`branch_match_tag_indexes_table`, `saturate_reuses_code_with_new_env`).

**Phase 1.6 gate:** `cargo test --lib bytecode` green with the new model; clippy
clean. Then the bytecode engine can implement the neutral intrinsic ABI (§5.5)
producing `BcValue`s over templates, unblocking Phase 2.

---

## Phase 2 — Dispatch loop for the day11 subset + differential harness

**Milestone:** `day11-p1` runs end-to-end through the native bytecode engine and produces **byte-identical** output to the HeapSyn engine, GC-verified. The architecture is proven.

The day11-p1 opcode subset (per the BV0 spec): `Atom` (L/G/V), `App`, `DirectApp`, `Bif`, `Case`, `Cons`, `Let`, `LetRec`, `Seq`, `LookupLit` — plus the arity/return path. `Meta`/`DeMeta`/`Ann` may be stubbed as `unimplemented!("BV1 phase 3: <op>")` until Phase 3.

### Task 2.1: `MachineState` reuse + `pending_bif` capture field

**Files:**
- Modify: `src/eval/machine/vm.rs` (add a bytecode-arg capture field to `MachineState`, e.g. `pub bc_pending_args: Option<SmallVec<[Ref; 8]>>`), or hold it in the `BytecodeMachine`. Prefer the latter (no change to `MachineState`) if the deferred-BIF path can read it there.

**Interfaces:**
- Produces: a place to stash the `Bif` opcode's decoded arg `Ref`s so the deferred intrinsic handler reads them (replacing the live-node re-read at `vm.rs:1971`).

- [ ] **Step 1:** Decide the stash location (prefer `BytecodeMachine`). Write the field + a `take_pending_bif_args()` accessor.

- [ ] **Step 2:** No standalone test yet (exercised in 2.3). Build + clippy clean.

- [ ] **Step 3:** Commit.

```bash
git add src/eval/bytecode/machine.rs src/eval/machine/vm.rs
git commit -m "feat(bytecode): pending-bif arg capture slot (eu-vi3a)"
```

### Task 2.2: `BytecodeMachine` skeleton + `handle_op` dispatch shell

**Files:**
- Create: `src/eval/bytecode/machine.rs`
- Modify: `src/eval/bytecode/mod.rs`

**Interfaces:**
- Consumes: `BytecodeProgram`, `BcClosure`, `BcContinuation`, `BcEnvBuilder`, the shared heap/globals/intrinsics/emitter/metrics (reuse `MachineState`'s fields or a parallel struct that borrows them).
- Produces: `pub struct BytecodeMachine { … }`; `pub fn run(&mut self) -> Result<u8, ExecutionError>`; `fn step(&mut self)`; `fn handle_op(&mut self, view) -> Result<(), ExecutionError>` with a `match Op::from_u8(byte)` shell where each arm is `unimplemented!()` for now.

- [ ] **Step 1:** Write `run`/`step` mirroring `Machine::run`/`step` (`vm.rs:2081`/`:1901`): the 500-tick GC poll (`vm.rs:2092-2114`), `metrics.tick()`, the `pending_bif` dispatch tail (`vm.rs:1961-2008`), the capture lifecycle (`vm.rs:2013-2035`). Reuse the existing `facilities()` split.

- [ ] **Step 2:** Write `handle_op`: fetch `closure.code(): u32`, read the opcode byte, `match` on `Op::from_u8`, each arm `unimplemented!("op N")`. Include the arity short-circuit (`remaining_arity > 0 → return_fun`, `vm.rs:423-424`) and the annotation update (`vm.rs:418-421`) before the match.

- [ ] **Step 3:** Build + clippy clean (arms are `unimplemented!`, no dead-code). Commit.

```bash
git add src/eval/bytecode/machine.rs src/eval/bytecode/mod.rs
git commit -m "feat(bytecode): BytecodeMachine run/step + handle_op dispatch shell (eu-vi3a)"
```

### Task 2.3: Implement the day11 dispatch arms

Implement, one arm per sub-step, each as a **translation** of the named `vm.rs` arm with `CodeRef`/`BcContinuation`/`BcEnvBuilder` in place of HeapSyn types. There is no per-arm unit test — the differential harness (Task 2.5) is the correctness gate; commit after each arm builds + clippy-clean, and run the day11 differential check once the subset is complete.

- [ ] **Step 1: `Atom`** — translate `vm.rs:428-457` (L: force/blackhole/Update; G: re-enter global; V: `return_native`). Commit `feat(bytecode): OP_ATOM arm`.
- [ ] **Step 2: `return_native`/`return_data`/`return_fun`/`update`** — translate `vm.rs:758-1263` for the continuation kinds the subset uses (Branch, Update, ApplyTo, SeqBind, LookupLitForce, CaptureEnd). This is the largest single arm-set; split further if a sub-part builds independently. Commit `feat(bytecode): return_* handlers`.
- [ ] **Step 3: `App`** — translate `vm.rs:481-526` (arg array via `BcEnvBuilder`, push `ApplyTo`, callable resolution + thunk blackhole/Update). Commit `feat(bytecode): OP_APP arm`.
- [ ] **Step 4: `DirectApp`** — translate `vm.rs:527-598` (inline smid → annotation, fast-path `saturate_with_array` skipping `ApplyTo`, fallback to App). Commit `feat(bytecode): OP_DIRECT_APP arm`.
- [ ] **Step 5: `Bif`** — translate `vm.rs:599-605`: decode arg refs, stash via `take_pending_bif_args` slot, set `pending_bif`. Verify the deferred tail in `step` reads the stashed args and dispatches through the **Phase 1.5** parameterised intrinsic ABI (the intrinsic returns a `BcClosure` via templates). Commit `feat(bytecode): OP_BIF arm + deferred dispatch`.
- [ ] **Step 6: `Case`** — translate `vm.rs:466-475`: push `Branch { min_tag, branch_table, fallback, env, annotation }`, enter scrutinee. Commit `feat(bytecode): OP_CASE arm`.
- [ ] **Step 7: `Cons`** — translate the `return_data` production path: entering a `Cons` closure returns the constructor to the top continuation. Commit `feat(bytecode): OP_CONS arm`.
- [ ] **Step 8: `Let`/`LetRec`** — translate `vm.rs:606-616` via `BcEnvBuilder::from_let/from_letrec`. Commit `feat(bytecode): OP_LET/LETREC arms`.
- [ ] **Step 9: `Seq`** — translate `vm.rs:639-647` (push `SeqBind`, enter scrutinee). Commit `feat(bytecode): OP_SEQ arm`.
- [ ] **Step 10: `LookupLit`** — translate `vm.rs:684-744` (fast-path block scan; slow path pushes `LookupLitForce`). Commit `feat(bytecode): OP_LOOKUP_LIT arm`.

### Task 2.4: `evaluate_to_whnf` (re-entrant loop) for the subset

**Files:** Modify `src/eval/bytecode/machine.rs`.

- [ ] **Step 1:** Translate `evaluate_to_whnf_impl` (`vm.rs:1541-1629`) to a bytecode version using `handle_op` + the same suspend/stash-for-GC discipline and `NullEmitter`. Needed when a BIF forces a thunk mid-execution.
- [ ] **Step 2:** Build + clippy clean; commit `feat(bytecode): evaluate_to_whnf re-entrant loop`.

### Task 2.5: The flag path + differential harness

**Files:**
- Modify: `src/driver/eval.rs` (a bytecode execution path: encode globals+program → run `BytecodeMachine`), `src/driver/options.rs` (a `--bytecode` global flag OR keep `EU_BYTECODE`).
- Create: `src/eval/bytecode/differential.rs` + a test in `tests/` that runs a unit through both engines.

**Interfaces:**
- Consumes: `bytecode_enabled()`, the encoder, `BytecodeMachine`, the existing HeapSyn execution path.
- Produces: `pub fn run_bytecode(...) -> Result<Output, ExecutionError>`; a differential test entry `assert_engines_agree(path: &Path)`.

- [ ] **Step 1:** Wire `src/driver/eval.rs`: when `bytecode_enabled()`, encode the runtime globals (from the blob's `ArenaStgSyn` via `reconstruct_form`, or source `StgSyn`) + the program into one `BytecodeProgram`, record `global_entries`, `prepare_constants`, and run `BytecodeMachine` instead of the HeapSyn machine. Mirror the existing `make_standard_runtime` / global-slot wiring (`eval.rs:268-299`).

- [ ] **Step 2:** Write `assert_engines_agree`: run a `.eu` unit with the HeapSyn engine and with `EU_BYTECODE=1`, capture stdout/stderr/exit for both, assert equal. (For non-deterministic IO units — unseeded `random`, `io.epoch-time` — compare against the `.expect` sidecar / structural equality instead; see spec §9. Maintain a small skip-strict-identity set.)

- [ ] **Step 3:** Add `test_bytecode_day11_p1_matches`: `assert_engines_agree("examples/aoc25/…/day11-p1…")` (find the exact day11-p1 harness/example path first).

- [ ] **Step 4:** Build release, run the differential test:

Run: `cargo test --release test_bytecode_day11_p1_matches 2>&1 | grep "test result:"`
Expected: PASS (byte-identical output through both engines).

- [ ] **Step 5:** GC-verify the bytecode path on day11:

Run: `EU_BYTECODE=1 EU_GC_VERIFY=2 EU_GC_POISON=1 timeout 120 ./target/release/eu <day11-p1 path>`
Expected: same output, no GC panic.

- [ ] **Step 6:** Commit.

```bash
git add src/driver/eval.rs src/driver/options.rs src/eval/bytecode/differential.rs tests/
git commit -m "feat(bytecode): flag path + differential harness; day11-p1 byte-identical (eu-vi3a)"
```

**Phase 2 gate:** day11-p1 byte-identical through both engines under `EU_GC_VERIFY=2`. If the bytecode path is *slower* than the tree-walk here (0-GC pure dispatch), record the number but continue — the bar is no-regression across the corpus (Phase 3), not day11 alone. If it is dramatically slower, pause and diagnose (the spec's whole thesis is that native dispatch is at worst neutral; a large slowdown means a hidden per-tick allocation crept in — the BV0 trap).

---

## Phase 3 — Full opcode coverage + full harness green

**Milestone:** the entire harness + conformance corpus passes byte-identically through both engines under `EU_GC_VERIFY=2`; no corpus regression. **This is the acceptance gate (spec §8).**

### Task 3.1: Remaining opcodes

For each, translate the named `vm.rs` arm (as in Task 2.3), commit per arm:
- [ ] **`Ann`** — `vm.rs:617-620` (set annotation, enter body).
- [ ] **`Meta`** — the `Meta` return/whnf path (`is_whnf` treats `Meta` as WHNF).
- [ ] **`DeMeta`** — `vm.rs:629-636` + `return_meta` (`vm.rs:778-794`).
- [ ] **`BlackHole`** — the blackhole-encountered error path.
- [ ] **Over/under-application, PAP** — the `return_fun` partial/over-apply branches (`vm.rs:1181-1194`) via `BcEnvBuilder::partially_apply`/`pap`.
- [ ] **Block application / MERGE, `NotCallable`, type-error returns** — the remaining `return_native`/`return_data` continuation mismatches (`vm.rs:823-1263`).
- [ ] **Capture/render** (`render-as` string capture) — `CaptureEnd` + `push_capture_end` (`vm.rs:1399-1401`, `:2024-2035`).
- [ ] **IO** — the IO yield / `io_run` path (BV0 explicitly stubbed this; it must work now). Cross-reference `src/driver/io_run.rs`.

### Task 3.2: Encode the prelude to bytecode

**Files:** Modify `src/driver/eval.rs`, `src/eval/bytecode/encode.rs`.

- [ ] **Step 1:** At machine init on the bytecode path, encode **all** runtime globals into the shared arena (not just the program), populating `global_entries[slot]`. Source: blob `ArenaStgSyn` (`reconstruct_form` → `StgSyn` → encode) or source-prelude `StgSyn`. This removes any fallback-to-HeapSyn for globals (the BV0 hybrid trap).
- [ ] **Step 2:** Differential-test a prelude-heavy unit (e.g. `tests/harness/010_prelude.eu`): `assert_engines_agree`. Commit.

### Task 3.3: Full differential harness run

- [ ] **Step 1:** Extend the differential test to iterate the **entire** `tests/harness/` corpus (respecting `--allow-io` and the non-deterministic skip-strict set). 

Run: `cargo test --release test_bytecode_harness_matches 2>&1 | grep "test result:"`
Expected: PASS for every case (byte-identical or `.expect`-satisfied through both engines).

- [ ] **Step 2:** GC-stress the whole harness on the bytecode path:

Run: `EU_BYTECODE=1 EU_GC_VERIFY=2 EU_GC_POISON=1 EU_GC_STRESS=1 timeout 600 ./target/release/eu test --allow-io tests/harness 2>&1 | tail -20`
Expected: all PASS, no GC panic.

- [ ] **Step 3:** No-regression benchmark: run the AoC-2025 corpus through both engines (A/B on the same binary), confirm no wall-time/alloc regression. Record the deltas in `examples/aoc25/` notes.

- [ ] **Step 4:** Confirm **code off the scan set**: add/enable instrumentation (a GC-scanned-object-kind counter, or assert `GcScannable for HeapSyn` is never invoked on the bytecode path — e.g. a debug assertion / counter). Run a GC-heavy program (day10-p1) and confirm zero code nodes scanned.

- [ ] **Step 5:** Commit the acceptance evidence.

```bash
git add tests/ examples/aoc25/
git commit -m "test(bytecode): full harness byte-identical through both engines; acceptance gate green (eu-vi3a)"
```

**Phase 3 gate = acceptance (spec §8).** If green: the bytecode bet has paid off; proceed to Phase 4. If a class of cases can't be made byte-identical, STOP and report — do not delete the HeapSyn machine.

---

## Phase 4 — Collapse to pure bytecode

**Milestone:** the HeapSyn machine, loader, and flag are gone; `CodeRef` is the only code representation; the tree is pure bytecode. **Do not start until Phase 3's gate is green.**

### Task 4.1: Make bytecode the default, delete the flag

- [ ] **Step 1:** Remove `bytecode_enabled()` and the flag; always take the bytecode path in `src/driver/eval.rs`. Keep the differential harness temporarily (it now compares against a soon-deleted engine — convert it to golden-output tests before deleting the HeapSyn engine, or run it once more then retire).
- [ ] **Step 2:** Full harness green (single engine now). Commit.

### Task 4.2: Delete the HeapSyn machine + loader

- [ ] **Step 1:** Delete `src/eval/memory/loader.rs`; delete `HeapSyn` and its `GcScannable`/`LambdaForm` impls from `src/eval/memory/syntax.rs`; delete the HeapSyn dispatch (`handle_instruction`, the HeapSyn `return_*`) from `src/eval/machine/vm.rs`; delete `SynClosure`-specific code that only served HeapSyn. Fix all resulting compile errors (the reused generic env/closure/heap code stays).
- [ ] **Step 2:** `cargo build && cargo clippy --all-targets -- -D warnings && cargo test --release` — all green.
- [ ] **Step 3:** GC-verify the full harness once more (single engine).
- [ ] **Step 4:** Commit `refactor(bytecode): delete HeapSyn machine + loader; bytecode is the sole engine (eu-vi3a)`.

### Task 4.3: Collapse `CodeRef` scaffolding + close out

- [ ] **Step 1:** Confirm `CodeRef = u32` is the only code type; remove any transitional shims.
- [ ] **Step 2:** Update `bd` bead `eu-vi3a` to closed with a reference to the acceptance evidence; note BV5 (eu-xfxc) is now unblocked (blob stores bytecode directly + startup win).
- [ ] **Step 3:** Final commit + push the integration branch.

---

## Self-Review

**Spec coverage:** §1 (purpose/BV0 reframing) → informs all phases; §2 (decisions) → Phase 2 flag + differential harness (2.5), §8 acceptance (Phase 3); §3 (CodeRef/generic seam) → Tasks 1.4, 1.6; §4 (format/encoder) → Tasks 1.1–1.3, 3.2; §5 (prelude/globals) → Task 3.2; §6 (parallel machine: dispatch/BcContinuation/pending_bif/two loops/annotations) → Tasks 1.5, 2.1–2.4, 3.1; §7 (GC retires) → Task 1.4 (no code fixup) + Task 4.2 (deletion) + Task 3.3 step 4 (code-off-scan-set evidence); §8 (acceptance) → Phase 3 gate; §9 (testing) → Task 2.5 (differential), 3.3 (full + GC stress + benchmarks); §10 (phasing) → the four phases; §11 (risks) → Phase 2 gate note (BV0-trap watch), Task 1.4 (root lifetime via POISON). No uncovered spec requirement.

**Placeholder scan:** dispatch-arm steps intentionally reference exact `vm.rs` line ranges to translate rather than pre-listing hundreds of lines of uncompilable Rust — this is deliberate for a translation-heavy systems rewrite (noted at the top), and every such step is gated by the concrete differential test. Novel code (types, encoder, readers, flag path, tests) has real code/commands. No `TBD`/`TODO`/"handle edge cases" left.

**Type consistency:** `CodeRef` (u32), `BcClosure = Closing<CodeRef>`, `BytecodeProgram { code, constants, global_entries }`, `BcContinuation` variant fields, `Op` names, `take_pending_bif_args` — used consistently across tasks. `assert_engines_agree` / `bytecode_enabled` / `run_bytecode` names stable throughout.
