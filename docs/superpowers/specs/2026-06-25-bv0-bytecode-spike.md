# BV0: Bytecode Encoding + Dispatch-Ceiling Spike

- **Bead:** eu-3mj5
- **Pillar:** BV — Bytecode VM
- **Release:** 0.11 (gate — go/no-go for BV1)
- **Date:** 2026-06-25

---

## 1. Purpose

BV0 is a **gate, not a commitment**. It answers one question: does a
flat bytecode interpreter dispatch significantly faster than the
current `HeapSyn` tree-walk? If ≥~2× on `day11-p1` (the 81%-dispatch
microbench, 66.5 M ticks, 0 GC collections), proceed to BV1. If not,
reassess the bytecode programme.

The spike is designed so that its encoder seeds BV1 — nothing is
thrown away if the gate passes.

## 2. Background: What BV0 Replaces

Today's dispatch path: `StgSyn` (off-heap, `Rc`-based IR) → `load`
(`src/eval/memory/loader.rs:120`) → `HeapSyn` (on-heap, `NonNull`
pointer graph, GC-scanned via `impl GcScannable for HeapSyn`) →
`handle_instruction` tree-walk (`src/eval/machine/vm.rs:389`, match
at `:426`).

**Costs this path pays:**
1. Pointer chasing through a tree of heap-allocated `HeapSyn` nodes.
2. `HeapSyn` is GC-scanned and evacuated like data.
3. `match` on a pointer-dereferenced enum variant — poor cache
   locality.
4. Every `Ann` node is a separate dispatch step (`vm.rs:536`).
5. The loader re-allocates the entire program into the GC heap on
   every run.

BV0 tests whether a **flat, cache-friendly opcode stream** with
`match` on a `u8` opcode eliminates enough of this overhead to
justify the BV programme.

## 3. Design

### 3.1 Code location and branch discipline

All spike code lives in a new module: `src/eval/bytecode/`.

```
src/eval/bytecode/
  mod.rs          — module root, public API
  opcode.rs       — opcode definitions (u8 enum)
  encode.rs       — StgSyn → bytecode encoder
  program.rs      — bytecode program (byte buffer + constant pool)
  interp.rs       — threaded interpreter (dispatch loop)
  bench.rs        — dispatch-only benchmark harness
```

This module is **separate from** `src/eval/machine/` (the existing
VM) and `src/eval/stg/` (the existing STG IR). It does not modify
any existing code. The existing VM continues to be the production
execution path.

**Branch:** work on `spike/bv0-bytecode` branch. If the gate
passes (≥ 2×), merge to master with the measurements. If no-go,
archive the branch and document the measurements in
`docs/development/bv0-results.md`.

### 3.2 Opcode set (minimal for day11-p1)

A `u8` opcode enum covering the `HeapSyn` variants that `day11-p1`
exercises:

| Opcode | Corresponds to | Operands |
|---|---|---|
| `ATOM_L` | `Atom { Ref::L(i) }` | `u16` index |
| `ATOM_G` | `Atom { Ref::G(i) }` | `u16` index |
| `ATOM_V` | `Atom { Ref::V(n) }` | constant-pool index |
| `APP` | `App { callable, args }` | `u8` arg count, callable ref, arg refs |
| `BIF` | `Bif { intrinsic, args }` | `u8` intrinsic id, `u8` arg count, arg refs |
| `CASE` | `Case { scrutinee, branches, fallback }` | branch table offset |
| `CONS` | `Cons { tag, args }` | `u8` tag, `u8` arg count, arg refs |
| `LET` | `Let { bindings, body }` | `u8` binding count, binding offsets |
| `LETREC` | `LetRec { bindings, body }` | `u8` binding count, binding offsets |
| `SEQ` | `Seq { scrutinee, body }` | scrutinee offset, body offset |
| `ANN` | `Ann { smid, body }` | `u32` smid, body offset |
| `META` | `Meta { meta, body }` | meta ref, body ref |
| `DEMETA` | `DeMeta { … }` | handler/or_else offsets |
| `BLACKHOLE` | `BlackHole` | (none) |

Refs are encoded as a tag byte (`L`/`G`/`V`) + value. Lambda forms
in `LET`/`LETREC` bindings carry their `InfoFlags` (arity, update,
annotation) inline.

Opcodes not needed by `day11-p1` can be stubbed as `UNIMPLEMENTED`
with a panic — the spike only needs to complete one program.

### 3.3 Bytecode program structure

```rust
pub struct BytecodeProgram {
    /// Flat opcode stream
    code: Vec<u8>,
    /// Constant pool: native values (numbers, strings, symbols)
    /// referenced by ATOM_V operands
    constants: Vec<stg::syntax::Native>,
    /// Lambda form info for LET/LETREC bindings
    /// (arity, update flag, annotation)
    forms: Vec<InfoFlags>,
}
```

The encoder (`encode.rs`) walks a `StgSyn` tree (or `ArenaStgSyn`
node pool) and emits a linear byte stream. Code offsets replace
`Rc<StgSyn>` / `RefPtr<HeapSyn>` pointers. The constant pool
replaces inline `Native` values.

**This is not the final BV1 format** — it's a minimal encoding to
test the dispatch hypothesis. BV1 will refine the encoding, add
serialisation (postcard), and integrate with the heap.

### 3.4 Interpreter

The interpreter (`interp.rs`) is a loop with `match` on the opcode
byte:

```rust
loop {
    let op = program.code[pc];
    pc += 1;
    match op {
        opcode::ATOM_L => { /* resolve local ref */ }
        opcode::APP => { /* create arg array, push ApplyTo, enter */ }
        opcode::CASE => { /* evaluate scrutinee, branch */ }
        // ...
    }
    ticks += 1;
}
```

**What it reuses from the existing VM:**
- The continuation stack (`Vec<Continuation>`, same six kinds).
- The environment frame machinery (`EnvFrame`, cactus stack).
- The heap and GC (data values still live on the GC heap).
- All 192 intrinsics (via the same `pending_bif` deferral).
- The emitter and error machinery.

**What it replaces:**
- `HeapSyn` dispatch — opcodes are flat bytes, not pointer-chased
  enum variants.
- Code is in a `Vec<u8>` outside the GC heap — never scanned,
  never evacuated.
- `Ann` can be folded into a side-table lookup (preview of BV2)
  rather than a separate dispatch step, but the spike can also
  just handle it as an opcode for simplicity.

**Closures:** a `SynClosure` today pairs a `RefPtr<HeapSyn>` code
pointer with an `EnvFrame`. In the spike, the code pointer becomes
a `u32` offset into the bytecode program. The closure struct needs
a variant or wrapper to hold this. The simplest approach: allocate
a tiny `HeapSyn::Atom { Ref::V(Native::Num(offset)) }` stub as
the code pointer and resolve it in the interpreter — ugly but
avoids touching `SynClosure` for the spike. A cleaner approach
(for BV1) replaces `RefPtr<HeapSyn>` with a `CodeRef` enum.

### 3.5 Benchmark harness

`bench.rs` provides a dispatch-only benchmark:

1. Load `day11-p1` through the normal pipeline (parse → compile →
   STG).
2. Encode the STG to bytecode.
3. Load the program into the existing VM (heap, globals, intrinsics).
4. Run the bytecode interpreter to completion, measuring ticks and
   wall time.
5. Run the same program through the existing `HeapSyn` VM, measuring
   ticks and wall time.
6. Report the ratio.

Both runs use the same heap, same GC, same intrinsics — only the
dispatch differs. The benchmark is run via
`cargo test --test bv0_bench` or a dedicated binary.

### 3.6 The gate

**Go if:** bytecode dispatch throughput is ≥~2× the `HeapSyn`
dispatch on `day11-p1`, measured as wall-time-per-tick (isolating
dispatch from GC, which is 0 collections on this program).

**No-go if:** < 2×. Document the measurements, archive the branch,
and reassess. The roadmap fallback (§6.4): "a flattened-node
interpreter — lower risk, smaller win."

**Measurements to record:**
- Wall time (both interpreters, same program, multiple runs)
- Ticks (should be identical — same program)
- Bytecode size vs HeapSyn allocation size
- Cache miss profile if available (perf stat)

## 4. Scope

### In scope

- Opcode encoding of the `StgSyn` subset `day11-p1` needs.
- Minimal threaded interpreter sharing continuations, env frames,
  heap, GC, and intrinsics with the existing VM.
- Dispatch-only benchmark comparing bytecode vs HeapSyn on
  `day11-p1`.
- Go/no-go decision with documented measurements.
- All code in `src/eval/bytecode/`, no modifications to existing
  modules.

### Out of scope

- Full opcode coverage (BV1).
- Serialisation / embedding (BV5).
- Side tables for annotations (BV2).
- Register frames (BV3).
- Superinstructions (BV4).
- Replacing the production VM — the existing `HeapSyn` VM remains
  the execution path; the bytecode interpreter is a benchmark-only
  prototype.

## 5. Success Criteria

1. **Encoder completes:** `day11-p1`'s STG encodes to a valid
   bytecode program without errors.
2. **Interpreter completes:** the bytecode interpreter runs
   `day11-p1` to completion, producing the same result as the
   `HeapSyn` VM.
3. **Output identity:** bytecode and HeapSyn produce byte-identical
   rendered output.
4. **Measurements recorded:** wall time, ticks, bytecode size,
   and the dispatch ratio are documented.
5. **Gate decision:** go/no-go recorded with rationale.

## 6. Testing

- The benchmark harness itself verifies output identity (criterion
  3).
- `EU_GC_VERIFY=2` on the bytecode run to confirm GC correctness
  (data values are still on the GC heap).
- Multiple runs for timing stability.
- If go: the encoder and interpreter are tested on additional
  harness programs to build confidence before BV1.

## 7. Risks

- **Medium:** the spike requires a working interpreter that
  correctly handles continuations, env frames, and intrinsics
  alongside bytecode dispatch. This is the core complexity —
  getting the interaction right between flat bytecode and the
  existing heap-based machinery.
- **Low:** the encoder is mechanical — walking `StgSyn` and
  emitting bytes. The `ArenaStgSyn` form is already close to
  what's needed.
- **Low:** if the gate fails, the cost is weeks of work on a
  branch that's archived with measurements. The encoder work is
  not wasted — it informs any future flattened-node approach.
- **Medium:** the closure representation hack (§3.4) is the
  ugliest part of the spike. If it causes correctness issues,
  the cleaner `CodeRef` approach should be adopted even for
  the spike.
