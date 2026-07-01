# BV1: Full Bytecode VM — Code Out of the GC Heap

- **Bead:** eu-vi3a
- **Pillar:** BV — Bytecode VM (ROADMAP.md §6.4 phase 2, §7 Pillar BV)
- **Release:** 0.12 (epic eu-u9xj, paired with BV5 / eu-xfxc)
- **Integration branch:** `integration/0.12.0` (disposable — if the bytecode bet
  does not pay off, the branch is dropped; nothing lands on master until it does)
- **Date:** 2026-07-01
- **Supersedes/learns-from:** BV0 spike (`spike/bv0-bytecode`, commit `16547a6d`,
  NO-GO)

---

## 1. Purpose and the BV0 reframing

BV1 replaces the `HeapSyn` tree-walk execution IR — and its per-run loader — with
a **flat, non-GC bytecode arena** and an **opcode dispatcher**, so that compiled
code leaves the GC-scanned heap and dispatch runs over a cache-friendly linear
stream. It is the spine of the runtime-performance programme (ROADMAP §4.5): BV5
(startup win), BV2 (annotation side-tables), BV3 (register frames) and BV4
(superinstructions) all build on it.

**BV0 must be read correctly, or BV1 will repeat its mistake.** BV0 was a NO-GO
(1.15× *slower* on day11-p1: 1.780s vs 1.549s median mutator). The confirmed root
cause was **not** that bytecode dispatch is slow — it was a closure-representation
hack. BV0 refused to change `SynClosure`'s `RefPtr<HeapSyn>` code pointer, so it
smuggled each bytecode offset inside a freshly **heap-allocated** `HeapSyn::Atom`
sentinel-`Number` stub (`alloc_bytecode_stub`), plus shadow `Cons`/`Bif` nodes to
satisfy the still-`HeapSyn`-typed continuations, branch tables and `pending_bif`
path. Every control-flow edge therefore performed a GC-heap `view.alloc` **per
dispatch step**, where the tree-walk it replaced allocates nothing. The flat
stream removed pointer-chasing but re-introduced *more* allocation than it saved,
and it remained structurally hybrid — globals/intrinsics/data all fell back to the
tree-walk, each handoff paying a stub allocation.

The lesson, and BV1's thesis: **a real `CodeRef` (a plain integer offset, never a
heap allocation) must run end-to-end through closures, continuations, env frames,
*and* globals, with zero per-dispatch heap allocation.** We therefore build a
*native* bytecode machine, not a bytecode layer draped over a HeapSyn-typed one.

We have **no valid measurement** of native flat-dispatch speed (BV0's number is
contaminated). Per the brainstorming decision, we do **not** spike first: we spec
and build the full BV1 on the integration branch, treating the dispatch win as a
design assumption validated by benchmarks during implementation, with the
keep/drop decision gated on **correctness + no-regression** (§8), not a perf
threshold.

## 2. Decisions settled in brainstorming

1. **Full BV1 directly** — no separate de-risking spike; the integration branch is
   the sandbox.
2. **Parallel bytecode machine behind a flag** — build a fully-native bytecode
   machine alongside the untouched HeapSyn machine, selected by a runtime flag
   (e.g. `EU_BYTECODE=1` / `--bytecode`, to be named during implementation).
   This is motivated not by risk (the branch is disposable) but by two structural
   benefits: **differential testing** (run the whole harness through *both* engines
   and assert byte-identical output — the strongest correctness net for a hot-path
   rewrite) and **A/B perf measurement** on one binary. The flag and the HeapSyn
   machine are deleted once bytecode wins (§10 phase 4).
3. **Success bar = correctness + no-regression only.** Byte-identical corpus via
   differential testing; `EU_GC_VERIFY=2` clean; code demonstrably off the scan
   set; no corpus regression. Any dispatch win is a bonus; startup (BV5) and
   deeper dispatch wins (BV2–4) are judged later.

## 3. The core move — `CodeRef` and the generic seam

The machine's closure/env layer is **already generic over the code type**, which
is what makes a native parallel machine tractable:

- `Closing<S>` (`src/eval/machine/env.rs:19-24`) — a closure is `InfoTagged<S>`
  (static part) + `RefPtr<EnvironmentFrame<Closing<S>>>` (environment). Today
  `SynClosure = Closing<RefPtr<HeapSyn>>` (`env.rs:445`).
- `InfoTagged<L>` (`src/eval/memory/infotable.rs:43`) — `{ info: InfoFlags(u64),
  body: L }`. `L` **is the code pointer**; `InfoFlags` packs update/arity/annotation
  into one `u64`. `body()`/`set_body()` read/write the code reference.
- `EnvironmentFrame<C>` (`src/eval/machine/env.rs:191-209`) — already generic over
  the closure type `C`; the cactus stack, `cell`/`get` de-Bruijn walk
  (`env.rs:333-358`) and shared-backing optimisation are all reused unchanged.

BV1 introduces:

```rust
pub type CodeRef = u32;                    // offset into the bytecode arena
pub type BcClosure = Closing<CodeRef>;     // closure whose code is a bytecode offset
// EnvironmentFrame<BcClosure> comes for free from the generic definition
```

Because a `u32` offset **never moves**, GC gets *simpler*, not harder:
`BcClosure`'s `scan_and_update` no longer calls `heap.forwarded_to(code())` /
`set_body()` (contrast `SynClosure::scan_and_update`, `env.rs:484-491`) — the code
field is inert to the collector. Env frames still hold runtime values and are still
scanned exactly as today.

**Reused for free** (by instantiating the generics with `CodeRef`): the closure
struct, `InfoFlags` arity/update/annotation packing, `EnvironmentFrame` + cactus
stack + `cell`/`get` + shared-backing/remap optimisation, the heap + GC + Immix
allocator, all 192 intrinsics, the emitter, metrics, and the error machinery.

**Genuinely new work** (the bulk of BV1):
- a `BcContinuation` enum (today's 7 continuations hard-code `RefPtr<HeapSyn>`);
- the bytecode encoder (replacing the `StgSyn → HeapSyn` loader);
- the bytecode dispatch loop(s);
- `BcClosure` builders paralleling `env_builder`'s `create_arg_array` etc.;
- `GcScannable` impls for `BcContinuation` and `BcClosure` (simpler — no code
  pointer to trace/fix up).

## 4. Bytecode format and encoder

### 4.1 Program representation

One per-run bytecode program:

```rust
pub struct BytecodeProgram {
    code: Vec<u8>,                 // flat, linearised opcode stream (non-GC)
    constants: Vec<Native>,        // constant pool (see §4.4); registered as GC roots
    global_entries: Vec<CodeRef>,  // global slot index -> entry offset (see §5)
}
```

The `code` buffer is a plain `Vec<u8>` owned outside the GC heap for the whole run
— never allocated per node, never marked, never evacuated, never forwarded.

### 4.2 Encoding shape — linearised, 1:1 with STG nodes

The encoder walks `StgSyn` (or equivalently `ArenaStgSyn`) **post-order** and emits
a linear byte stream: children are emitted first at low offsets, parents refer to
them by absolute `u32` code offset, the root is emitted last. This reuses the
existing topological flatten order of `StgArena` (`src/eval/stg/arena.rs:190-201`:
parent slot pre-allocated before recursing into children, root = index 0), so the
arena's node ordering is a valid emission order.

Opcodes are **≈ 1:1 with `StgSyn` variants** for BV1. Finer micro-ops, stack/register
lowering, and profile-guided superinstructions are explicitly **BV4**, not BV1 —
BV1's job is to prove native flat dispatch is sound and code-off-heap, on the
smallest change that achieves it. The BV0 encoder (`git show
spike/bv0-bytecode:src/eval/bytecode/encode.rs`) and opcode vocabulary port
directly; only the closure/continuation coupling is redone natively.

Opcode set (one `u8` per variant; operands as noted):

| Opcode | StgSyn variant | Operands |
|---|---|---|
| `OP_ATOM` | `Atom { evaluand }` | one `Ref` (tag byte + payload) |
| `OP_CASE` | `Case { scrutinee, branches, fallback }` | scrutinee offset; `min_tag`; branch table (tag → offset); optional fallback offset |
| `OP_CONS` | `Cons { tag, args }` | `tag: u8`; arg count; arg `Ref`s |
| `OP_APP` | `App { callable, args, eager_args }` | callable `Ref`; arg count; arg `Ref`s; **`eager_args` flag bit** |
| `OP_DIRECT_APP` | `DirectApp { smid, callable, args, eager_args }` | inline `Smid`; callable `Ref`; arg count; arg `Ref`s; flag bit |
| `OP_BIF` | `Bif { intrinsic, args }` | `intrinsic: u8`; arg count; arg `Ref`s |
| `OP_LET` | `Let { bindings, body }` | binding count; per-binding form headers + body offsets; let-body offset |
| `OP_LETREC` | `LetRec { bindings, body }` | as `OP_LET` (self-ref via de Bruijn) |
| `OP_ANN` | `Ann { smid, body }` | inline `Smid`; body offset |
| `OP_META` | `Meta { meta, body }` | two `Ref`s |
| `OP_DEMETA` | `DeMeta { scrutinee, handler, or_else }` | three offsets |
| `OP_SEQ` | `Seq { scrutinee, body }` | scrutinee offset; body offset |
| `OP_LOOKUP_LIT` | `LookupLit { smid, key, obj, default }` | inline `Smid`; key const; obj `Ref`; default `Ref` |
| `OP_BLACKHOLE` | `BlackHole` | — |

`Ref` operands encode as a tag byte (`L`/`G`/`V`) + payload: `L(u32)` local index,
`G(u32)` global slot, `V(const_index u32)` into the constant pool. A closure/lambda
"header" carries arity (`u8`), the update flag (is-thunk), value-vs-thunk, and the
annotation `Smid` — the same information `LambdaForm`/`InfoFlags` carries today.

### 4.3 CG1/CG2/CG3 preserved as opcodes

The 0.11 codegen wins are represented directly so no perf is lost in the rewrite:
- **CG1 DirectApp** → `OP_DIRECT_APP`: carries the inline `Smid` (replacing a
  wrapping `Ann`), and on the fast path (callee arity == arg count) saturates
  immediately, **skipping the `ApplyTo` push** — the bytecode analogue of
  `vm.rs:527-598`. Falls back to `OP_APP` semantics on arity mismatch.
- **CG2 LookupLit** → `OP_LOOKUP_LIT`: one-step fast-path block lookup with a
  `default` `Ref` fallback thunk (mirrors `compile_lookup`, `compiler.rs:1716-1794`).
- **CG3 eager_args** → a **flag bit** on `OP_APP`/`OP_DIRECT_APP`; when set, `Ref::L`
  args resolve eagerly (via `env.get(i)`) instead of building lazy `Atom{L}`
  indirections (mirrors `create_arg_array_eager`, `env_builder.rs:282-301`).

### 4.4 Constants — heap literals as roots, not stream bytes

A `Ref::V(Native)` can embed a heap pointer: `Native::Str(RefPtr<HeapString>)`,
`Set`, `NdArray`, `Vec` (`src/eval/memory/syntax.rs:36-58`). These cannot live in
the non-GC byte stream. Following BV0's `prepare_constants`: at load, each such
literal is heap-allocated/interned **once** into `BytecodeProgram.constants`
(symbols interned into the `SymbolPool`, strings allocated as `HeapString`), and
`Ref::V` operands carry a `u32` index into that pool. The constant pool is
registered as a **GC root set** — scanned once per cycle (as roots), never
per-tick, never re-marked as reachable-from-code. Inline scalar constants
(`Num`, `Zdt`, interned `Sym`) need no heap object.

## 5. Prelude, globals, and loading

BV0's fatal structural flaw was leaving globals as `HeapSyn`, forcing every call
into a prelude/intrinsic wrapper to bounce to the tree-walk. **BV1 encodes the
prelude to bytecode too.** At machine init, the bytecode machine encodes both:
- the runtime globals — from the prelude blob's existing `ArenaStgSyn`
  (`src/eval/stg/blob.rs:47-109`, reconstructed via
  `StgArena::reconstruct_form`), or the source-prelude `StgSyn` in the fallback
  path — into the shared arena, recording each global's entry offset in
  `global_entries[slot]`; and
- the program `StgSyn`.

`Ref::G(slot)` resolves through `global_entries` to a bytecode offset, exactly as
`Ref::G` indexes the flat globals frame today (`vm.rs:101-106`). Intrinsic wrappers
occupy slots `0..INTRINSIC_COUNT`; prelude bindings follow.

The prelude **blob still stores `ArenaStgSyn`** in 0.12; making the blob store
bytecode directly — and the 85→~25ms startup collapse — is **BV5 (eu-xfxc)**, out
of scope here. BV1 therefore adds a `StgSyn/ArenaStgSyn → bytecode` encode step at
startup for prelude + program, replacing the `StgSyn → HeapSyn` loader
(`src/eval/memory/loader.rs`) on the bytecode path.

## 6. The parallel bytecode machine

A `BytecodeMachine` reuses `MachineState`'s heap, globals, stack, intrinsics slice,
emitter, metrics and settings, and runs its own dispatch loop.

### 6.1 Dispatch loop

Fetch the current `BcClosure`'s code offset (`closure.code(): u32`), read one
opcode byte, `match` on it, mutate `MachineState` as the corresponding
`handle_instruction` arm does today. A dense `match u8` is the realistic Rust
dispatch technique (LLVM lowers it to a jump table; computed-goto / guaranteed
tail-call dispatch are not stable-Rust-friendly). One tick = one opcode dispatch
(+ at most one deferred BIF + capture bookkeeping), preserving `metrics.tick()`
semantics and the 500-tick GC-poll cadence (`vm.rs:2092-2114`).

The "enter closure" primitive (`saturate`/`saturate_with_array`, reusing a code
reference with a new env — `env_builder.rs:215-240`) carries over directly with
`CodeRef` in place of `RefPtr<HeapSyn>`. `DirectApp` remains the model for the
common "call" case.

### 6.2 `BcContinuation` — the 7 continuations, natively

Today's `Continuation` (`src/eval/machine/cont.rs:34-96`) hard-codes
`RefPtr<HeapSyn>` for branch bodies, fallbacks, handlers and seq bodies. BV1
defines a parallel `BcContinuation` holding `CodeRef` offsets instead:

| Continuation | Change from today |
|---|---|
| `Branch` | `branch_table: Array<Option<CodeRef>>`, `fallback: Option<CodeRef>`; `match_tag` unchanged |
| `Update` | unchanged (`environment`, `index`) — no code pointer |
| `ApplyTo` | `args: Array<BcClosure>` |
| `DeMeta` | `handler`/`or_else`: `CodeRef` |
| `SeqBind` | `body`: `CodeRef` |
| `LookupLitForce` | `default_closure: BcClosure` (offsets, not HeapSyn) |
| `CaptureEnd` | unchanged (holds nothing) |

The stack stays an off-heap `Vec<BcContinuation>` (as `vm.rs:283`). `GcScannable`
for `BcContinuation` mirrors today's (`cont.rs:173-356`) **minus the code-pointer
scanning** — only heap values (env frames, `ApplyTo`/`LookupLitForce` closures'
envs, `Array` backings) are traced; `CodeRef` fields are inert.

### 6.3 `pending_bif` redesign

Today the deferred BIF handler re-reads its arg slice directly from the live
`HeapSyn::Bif` node (`vm.rs:1971`: `match closure.code() { HeapSyn::Bif{args} =>
args.as_slice() }`). In bytecode there is no such node to re-read. Instead,
`OP_BIF` decode **captures the arg `Ref`s** (decoded from the stream) into
`MachineState` (`pending_bif: Option<(u8, SmallVec<[Ref; N]>)>` or equivalent), and
the existing deferred path (`MachineBifContext`, `bif.execute(ctx, view, emitter,
args)`, `vm.rs:1961-2008`) reads from there. The intrinsic ABI is otherwise
unchanged.

### 6.4 Both dispatch loops migrate

There are two dispatch loops today: `Machine::run`/`step` (`vm.rs:2081`/`:1901`) and
the re-entrant `evaluate_to_whnf_impl` (`vm.rs:1541`, used when a BIF must force a
thunk mid-execution). BV1 provides bytecode versions of **both**, plus the
`MachineBifContext` re-entrant path. (Note the `spike/bv0-two-loop` branch exists —
consult it for prior thinking on the two-loop structure, but rebuild natively.)

### 6.5 Annotations stay inline

BV1 keeps annotations inline: `OP_ANN` + inline `Smid` on `OP_DIRECT_APP` /
`OP_LOOKUP_LIT`, exactly as today (annotations flow through `InfoFlags` and inline
node smids — `vm.rs:418-421,534`). Moving `Smid`s out of the instruction stream
into offset-keyed side tables is **BV2 (eu-2sa6.1)**, out of scope. Error source
locations must remain byte-identical.

## 7. GC implications (what retires)

On the bytecode path, `impl GcScannable for HeapSyn` and `impl GcScannable for
LambdaForm` (`src/eval/memory/syntax.rs:318-716`) are **no longer reached** — code
is not a heap object. This retires, for code: per-node `AllocHeader` overhead and
16-byte alignment rounding; per-cycle re-marking and line-marking of every code
node (code is root-reachable for the whole run); and the entire two-level
`Array` + `OpaqueHeapBytes` backing-buffer registration/forwarding dance for
`branch_table`/`args`/`bindings` (`collect.rs:71-121`) — the most intricate,
historically bug-prone part of the code/GC coupling (MEMORY.md "Bug 2/3/5/6").
Runtime **data values** (data-constructor closures = `CodeRef` + heap env) and the
constant pool remain scanned; only *code* leaves the set.

The HeapSyn machine, `HeapSyn`, its `GcScannable` impls, and the `StgSyn → HeapSyn`
loader are physically deleted in phase 4 (§10), once the bytecode path passes the
full harness.

## 8. Acceptance criteria (keep/drop bar)

BV1 is kept iff, on the integration branch:

1. **Byte-identical output** across the full harness and conformance corpus,
   proven by **differential testing** — every case run through both engines with
   asserted-identical stdout/stderr/exit (§9).
2. **GC correctness**: full harness green under `EU_GC_VERIFY=2` +
   `EU_GC_POISON=1` + `EU_GC_STRESS=1` on the bytecode path.
3. **Code off the scan set**: demonstrable that no code node is marked/evacuated on
   the bytecode path (`GcScannable for HeapSyn` unreachable; a GC-scan-object count
   or instrumentation confirms code is absent).
4. **No regression**: the AoC-2025 corpus and benchmarks show no wall-time or
   allocation regression versus the HeapSyn engine (measured A/B on the same
   binary via the flag). A dispatch improvement (esp. day11-p1/p2, the 0-GC
   pure-dispatch benches) is a welcome bonus but **not required** for BV1. Note
   "no regression" does implicitly require native dispatch to be at least *neutral*
   on the 0-GC benches — a BV0-style slowdown there would fail the bar.

Perf beyond "no regression" — the day11 dispatch win, the startup collapse — is
BV2/3/4/5's remit and is not gated here.

## 9. Testing strategy

- **Differential harness (primary):** a mode that runs each harness/conformance
  case through both the HeapSyn and bytecode engines and asserts byte-identical
  results. This is the correctness spine and should exist from phase 2 onward,
  gating every subsequent phase. Non-deterministic IO cases (unseeded `random`,
  `io.epoch-time`, `io.exec` on ambient state) will differ between *any* two runs,
  not just between engines — for these, compare structurally / against the
  existing `.expect` sidecars rather than demanding cross-engine byte-identity, or
  exclude them from the strict-identity set (they still run through both engines
  for crash/error parity).
- **GC stress:** `EU_GC_VERIFY=2`/`POISON=1`/`STRESS=1` on the bytecode path across
  the full harness.
- **Encoder round-trip tests:** encode representative `StgSyn` trees; assert the
  decoded/executed result matches the tree-walk; assert offset back-references and
  branch tables are well-formed.
- **A/B benchmarks:** the AoC-2025 corpus run through both engines on one build for
  the no-regression check and to record the (bonus) dispatch delta.

## 10. Phasing

Each phase is independently landable on `integration/0.12.0` and gated by the
differential harness once it exists.

1. **Encoder + data structures.** `StgSyn/ArenaStgSyn → BytecodeProgram` encoder
   (port + native-ise the BV0 encoder); `CodeRef`, `BcClosure = Closing<CodeRef>`,
   `EnvironmentFrame<BcClosure>` (generic instantiation); `BcContinuation`; the
   constant pool + root registration; `GcScannable` for `BcClosure` /
   `BcContinuation`. No execution yet.
2. **Dispatch loop, day11 subset.** Bytecode `run`/`step` + `evaluate_to_whnf`
   covering the opcode subset day11-p1 exercises; the `pending_bif` capture
   mechanism; the flag; the differential harness. Gate: day11 byte-identical
   through both engines, GC-verified.
3. **Full opcode coverage.** Remaining opcodes (Meta/DeMeta, LookupLit slow path,
   IO, capture/render, over-/under-application, all data constructors); prelude
   encoded to bytecode; full harness green through both engines under
   `EU_GC_VERIFY=2`; no corpus regression. This is the acceptance gate (§8).
4. **Collapse to pure bytecode.** Delete the HeapSyn machine, `HeapSyn`, its
   `GcScannable` impls, the `StgSyn → HeapSyn` loader, and the flag; `CodeRef`
   becomes the only code representation. End state: pure bytecode, no scaffolding.

## 11. Risks

- **High — hot-path rewrite.** BV1 rewrites the hottest, most delicate code
  (dispatch, continuations, env-builder). Mitigated by the parallel-machine
  structure + differential testing (both engines cross-check on every case) and the
  disposable integration branch.
- **Medium — two loops + re-entrancy.** The `evaluate_to_whnf` re-entrant path and
  `MachineBifContext` must be migrated in lockstep with `step`; a subtle divergence
  between the two bytecode loops is a plausible bug class. Differential testing
  under GC stress is the guard.
- **Medium — constant/root lifetime.** Heap literals hoisted into the constant pool
  must be correctly rooted for the whole run; a missed root is a use-after-free.
  `EU_GC_POISON=1` + differential testing guard this.
- **Low — dispatch may not win on day11.** Since day11 is 0-GC, a marginal native
  dispatch improvement may barely move it. Acceptable: BV1's bar is no-regression,
  and its structural payoff (code off-heap, BV5 enablement) stands regardless.
- **Low — encoder mechanical bugs** (offset arithmetic, branch tables). Caught by
  round-trip and differential tests.

## Appendix — coupling map (implementer's reference)

Execution core: `vm.rs` — `run` :2081, `step` :1901, `handle_instruction` :390
(match :427-752, opcode fetch :409), `pending_bif` :1961-2008 (arg re-read :1971),
re-entrant `evaluate_to_whnf_impl` :1541; `App` :481-526, `DirectApp` :527-598,
`return_fun`/saturation :1167.
Closures/env: `env.rs` — `Closing<S>` :19-24, `SynClosure` alias :445, `code()`
:81-83, `scan_and_update` :484-491, `EnvironmentFrame<C>` :191-209, `cell`/`get`
:333-358; `env_builder.rs` — `from_let`/`from_letrec` :175-212,
`create_arg_array` :266-280, `_eager` :282-301, `saturate*` :215-240;
`infotable.rs` — `InfoTagged`/`InfoFlags` :12-49.
Continuations: `cont.rs` — enum :34-96 (7 variants), `scan`/`scan_and_update`
:173-356.
Code-on-heap (retires): `memory/syntax.rs` — `HeapSyn` :226-300, `GcScannable`
:318-716; `loader.rs` :21-216; backing arrays / `OpaqueHeapBytes`
`collect.rs:71-121`, `array.rs:152-158`; `AllocHeader` `header.rs:65-75`.
Source IR + arena: `stg/syntax.rs` — `StgSyn` :117-205, `LambdaForm` :287-300,
`Ref`/`Native` :40-114; `stg/arena.rs` — `StgArena` :178-182, `ArenaStgSyn`
:106-169, flatten :190-201/515-519, reconstruct :372-509; `stg/blob.rs` —
`PreludeBlob` :47-109; embedding `driver/resources.rs:10-11`; CG emission
`stg/compiler.rs` — DirectApp :1190-1298, LookupLit :1716-1794, eager_args
:1134-1137.
BV0 spike (reuse encoder/opcodes; discard stub/hybrid):
`git show spike/bv0-bytecode:src/eval/bytecode/{opcode,encode,program,interp}.rs`;
two-loop prior art: `spike/bv0-two-loop`.
