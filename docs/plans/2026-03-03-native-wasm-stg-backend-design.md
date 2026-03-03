# Native WASM STG Backend Design

**Bead:** eu-44lh
**Status:** Design approved, implementation plan pending

## Overview

Compile eucalypt's STG intermediate representation directly to WASM
bytecode, producing standalone `.wasm` modules that run in any WASM
3.0 runtime (browsers, Node.js, Cloudflare Workers, wasmtime). No
Rust runtime is shipped — only the compiled program plus a small
host-imported intrinsics shim.

This uses three WASM 3.0 features, all available in all major browsers
since early 2025:

- **WASM GC** — browser-native garbage collection for closures,
  thunks, and data constructors. No custom GC needed.
- **WASM tail calls** — `return_call` / `return_call_ref` map directly
  to STG's enter/apply evaluation model. No trampolining.
- **Typed function references** — `call_ref` for closure dispatch.

**Relationship to eu-raaa:** Fully independent. eu-raaa compiles the
Rust binary to WASM (shipping the full VM, GC, and intrinsics as a
large WASM module). This compiles individual eucalypt programs to
WASM (small per-program output, no interpreter). They can coexist —
eu-raaa for a full-featured browser playground, the native backend for
lightweight embedding.

The front-end pipeline (parse, desugar, cook, verify, simplify,
transform, STG compile) is unchanged. The existing `Rc<StgSyn>` IR is
the input to a new WASM code generator that replaces the Rust-based VM
interpreter.

---

## Section 1: WASM GC Type Mapping

All eucalypt runtime values map to WASM GC struct types under a common
supertype, enabling dynamic typing via runtime casts:

```wasm
;; Abstract base for all eucalypt values
(type $value (struct))

;; Thunk: lazy evaluation cell (mutable — updated on first eval)
(type $thunk (sub $value (struct
  (field $code (mut (ref null $func)))    ;; null when evaluated
  (field $env  (mut (ref null $env)))     ;; captured environment
  (field $result (mut (ref null $value))) ;; filled after evaluation
)))

;; Closure: function with captured environment + known arity
(type $closure (sub $value (struct
  (field $code (ref $func))
  (field $env  (ref $env))
  (field $arity i32)
)))

;; Data constructor: tag + fields (for Case dispatch)
(type $data (sub $value (struct
  (field $tag i32)
  (field $fields (ref $fields))
)))

;; Native values
(type $num_int (sub $value (struct (field $val i64))))
(type $num_flt (sub $value (struct (field $val f64))))
(type $sym (sub $value (struct (field $val (ref extern)))))
(type $str (sub $value (struct (field $val (ref extern)))))

;; Environment: array of value references (de Bruijn indexed)
(type $env (array (mut (ref null $value))))

;; Fields array for data constructors
(type $fields (array (ref null $value))
```

Key points:

- **Two numeric types** (`$num_int` with i64, `$num_flt` with f64)
  mirror the current `serde_json::Number` distinction between integers
  and floats. Arithmetic intrinsics check the runtime type: if both
  operands are `$num_int`, operate on i64 (with overflow promotion to
  f64); if either is `$num_flt`, coerce to f64. The numeric model may
  evolve independently (separate concern); the codegen should localise
  numeric representation so it's swappable.

- **Strings and symbols** use `(ref extern)` to hold JS strings
  directly, avoiding duplication between WASM and host memory. String
  intrinsics call host-imported JS functions operating on these
  references.

- **Thunks** have mutable fields. `$code` is set to null after
  evaluation, `$result` holds the memoised value. Entering a thunk:
  if `$code` is null, return `$result`; otherwise call through `$code`
  and update.

- **Environments** are flat arrays indexed by de Bruijn level,
  matching the current `Ref::L(n)` scheme.

- **Metadata** is not shown in the base types above. The design
  defers the metadata representation decision: either an optional
  field on `$value`, or a wrapper struct `$with_meta` that pairs a
  value with its metadata. The wrapper approach avoids adding overhead
  to non-metadata values.

---

## Section 2: Compilation Strategy

Each STG form maps to WASM instructions. With tail calls, the mapping
is direct:

| STG Form | WASM Translation |
|----------|-----------------|
| `Atom(Ref::L(n))` | `array.get $env n` — load from environment |
| `Atom(Ref::V(native))` | `struct.new $num_int` etc. — inline native |
| `Atom(Ref::G(n))` | Reference to global (intrinsic wrapper) |
| `App(f, args)` | Evaluate `f`, check arity, enter via `return_call_ref` |
| `Case(scrut, branches, fb)` | Non-tail call to evaluate scrutinee, cast, dispatch on tag |
| `Cons(tag, args)` | `struct.new $data` with tag and args array |
| `Let(bindings, body)` | Allocate thunks (`struct.new $thunk`), extend env, compile body |
| `LetRec(bindings, body)` | Allocate with placeholder env, back-patch mutable fields |
| `Bif(intrinsic, args)` | Direct `call $intrinsic_N` |
| `Ann(smid, body)` | Debug mode: record source location. Release: compile body directly |
| `Meta(meta, body)` | Allocate metadata wrapper, attach to value |
| `DeMeta(scrut, h, or)` | Check for metadata, branch accordingly |

### Enter/Apply with tail calls

WASM tail calls (`return_call` / `return_call_ref`) map directly to
STG's "enter" operation. When entering a closure:

1. Check arity vs argument count
2. **Exact match:** `return_call_ref $code` (tail call into closure's
   code)
3. **Under-saturated:** Build partial application closure, return it
4. **Over-saturated:** Apply exact arity args via non-tail call, then
   tail-call result with remaining args

Each compiled lambda form becomes a WASM function that takes an
environment array and returns a `(ref $value)`. The function index is
stored in the closure struct.

### Thunk entry

```
enter_thunk(thunk):
  if thunk.$code is null:       ;; already evaluated
    return thunk.$result
  else:
    result = call thunk.$code(thunk.$env)
    thunk.$code = null
    thunk.$result = result
    return result
```

Black-holing (detecting infinite loops where a thunk enters itself)
uses a sentinel: set `$code` to a "black hole" function before
evaluation. If the thunk is re-entered before completion, the black
hole function raises an error.

---

## Section 3: Intrinsics

The current runtime has 160+ intrinsics. In the WASM backend, these
become WASM functions — some implemented directly in WASM, others
imported from the host.

### Tier 1 — Native WASM (~30 intrinsics)

No host calls needed. Direct WASM instruction emission:

- **Arithmetic:** `ADD`, `SUB`, `MUL`, `DIV`, `MOD`, `NEG`, `ABS` —
  i64/f64 WASM instructions with type dispatch
- **Comparison:** `LT`, `GT`, `EQ`, `LE`, `GE` — WASM comparison
  instructions
- **Logic:** `AND`, `OR`, `NOT` — i32 operations on boolean tags
- **List construction:** `CONS`, `HEAD`, `TAIL`, `NULL` — WASM GC
  struct operations on data constructors

### Tier 2 — Host-imported (~50 intrinsics)

Thin wrappers around host-provided functions:

- **String operations:** `SPLIT`, `MATCH`, `JOIN`, `UPPER`, `LOWER`
  — delegate to JS string/regex APIs via `(ref extern)`
- **I/O:** `EMIT`, `RENDER` — output to host-provided buffer
- **Time:** `EPOCHTIME`, `FMTTIME`, `PARSETIME` — delegate to JS
  `Date` or host clock
- **Random:** `RAND` — delegate to host PRNG

### Tier 3 — Complex structure operations (~80 intrinsics)

Block and set operations that depend on the runtime data structure
representation. See Section 4 for how these are abstracted.

### Host import interface

The host provides imported functions:

```wasm
(import "eu" "str_concat" (func $str_concat
  (param externref externref) (result externref)))
(import "eu" "str_split" (func $str_split
  (param externref externref) (result (ref $value))))
(import "eu" "epoch_time" (func $epoch_time (result f64)))
```

A JS shim (`eu-runtime.js`, ~200 lines) implements these. The shim is
shared across all compiled programs.

---

## Section 4: Block Representation

Blocks are eucalypt's central data structure — ordered key-value pairs
with metadata, merge semantics, and dynamic lookup. The block
representation is currently evolving (move to `im::OrdMap` for
persistent blocks).

**Design decision:** The WASM backend defines a **block operation
interface** without committing to a concrete representation. The
codegen emits calls to block operations; the implementation of those
operations is determined at build time based on the current block
model.

**Required block operations:**

| Operation | Signature |
|-----------|-----------|
| `block_new` | `(keys, values) -> block` |
| `block_lookup` | `(block, key) -> value?` |
| `block_merge` | `(block, block) -> block` |
| `block_keys` | `(block) -> list` |
| `block_values` | `(block) -> list` |
| `block_pairs` | `(block) -> list of (key, value)` |
| `block_has_key` | `(block, key) -> bool` |
| `block_remove` | `(block, key) -> block` |

**Possible implementations** (decided at implementation time):

- **Host-imported (`externref`)** — blocks are opaque JS `Map`
  objects. All operations cross the WASM/host boundary. Simplest
  decoupling, performance depends on boundary-crossing cost.
- **WASM GC flat arrays** — parallel key/value arrays with linear
  scan. Good for small blocks (eucalypt's typical case), poor for
  large ones.
- **WASM GC tree** — balanced tree of WASM GC structs (HAMT or
  similar). Maximum in-WASM performance but significant engineering
  effort.

The choice depends on where persistent blocks (eu-skjg and related
work) have landed when the WASM backend is built.

---

## Section 5: Code Generation Architecture

### Pipeline

```
Source → Parse → Desugar → Cook → Verify → Simplify → Transform
    → STG Compile → Rc<StgSyn>
    → [existing] VM interpreter (native execution)
    → [new]      WASM codegen → .wasm binary
```

### Module structure

```
src/eval/wasm/
  mod.rs            # public API: compile(StgSyn) -> Vec<u8>
  types.rs          # WASM GC type definitions ($value, $thunk, etc.)
  codegen.rs        # StgSyn → WASM instruction sequences
  intrinsics.rs     # intrinsic function table and wrappers
  emit.rs           # WASM binary encoding via wasm-encoder
```

### Dependencies

- **`wasm-encoder`** (Bytecode Alliance) — builder API for WASM
  modules, types, functions, instructions. Supports WASM GC and tail
  call instructions.

### CLI integration

The driver gains a `--target wasm` flag:

```
eu --target wasm input.eu -o output.wasm
```

The JS runtime shim is either bundled as a static asset or generated
alongside the `.wasm` file.

---

## Section 6: Feasibility Assessment

### Straightforward

- Type definitions — mapping STG values to WASM GC structs is
  mechanical
- Tier 1 intrinsics (arithmetic, comparison, logic) — direct WASM
  instruction emission
- Basic compilation of `Atom`, `Cons`, `Let`, `Bif` — predictable
  translations
- `wasm-encoder` crate — mature, well-documented

### Hard but tractable

- **Enter/apply with arity mismatches** — partial application and
  over-saturation require a generic dispatcher function (or a family
  by arg count). Fiddly but well-understood from STG literature.
- **Thunk update and black-holing** — detecting infinite loops
  requires a black-hole marker. Straightforward with mutable WASM GC
  fields but needs careful sequencing.
- **LetRec** — mutual recursion requires allocating thunks with
  placeholder environments, then back-patching. WASM GC mutable
  fields support this.
- **Metadata** — every value can carry metadata. Requires either an
  optional metadata field on all values (overhead) or a wrapper struct
  (indirection). Adds complexity to every value access path.

### Genuinely uncertain

- **Performance vs the Rust VM** — the Rust interpreter is tight code
  with cache-friendly memory layout. WASM GC struct access patterns
  are engine-dependent. The native backend may be slower for some
  workloads due to WASM GC overhead and host-boundary crossing.
  Unknown until benchmarked.
- **Binary size** — compiled programs should be small (KB), but the
  intrinsics shim and apply dispatcher add fixed overhead. Need to
  measure.
- **WASM GC engine maturity** — V8 and SpiderMonkey optimise WASM GC
  well, but edge cases (deeply recursive types, high allocation rates)
  may hit unoptimised paths. Moving target as engines improve.
- **Debugging** — WASM source maps exist but WASM GC tooling is
  young. Debugging compiled eucalypt in the browser may be painful
  initially.

---

## Section 7: Phased Delivery

**Phase 0 (spike):** Compile a trivial eucalypt program (arithmetic,
no blocks) end-to-end. Validates the pipeline: `wasm-encoder`, WASM GC
types, thunk evaluation, a few Tier 1 intrinsics. Success criterion:
`eu --target wasm` produces a `.wasm` that runs in Node.js and
returns the correct numeric result.

**Phase 1:** Add lists, `Case` dispatch, and basic string intrinsics
(Tier 2). A useful subset that handles list processing and pattern
matching.

**Phase 2:** Add blocks (whatever representation is available at the
time), remaining Tier 2 and Tier 3 intrinsic coverage. Full language
support.

**Phase 3:** Optimisation — unboxing (avoid wrapping known-type values
in GC structs), inlining, dead code elimination at the WASM level.

Each phase is independently testable. Phase 0 is the critical
feasibility gate.

---

## Dependencies

- **None blocking.** This is independent of eu-raaa and all other
  0.4.0 work.
- The block representation (Section 4) depends on the state of
  persistent blocks when implementation begins.
- The numeric model may evolve independently; the codegen localises
  numeric representation to ease future changes.

## Out of Scope

- Optimisation passes beyond basic code generation (Phase 3)
- WASM GC tree-based block implementation (deferred decision)
- Numeric model redesign (independent concern)
- WASI target (potential alternative to JS host imports)
- Source-level debugging / source maps
