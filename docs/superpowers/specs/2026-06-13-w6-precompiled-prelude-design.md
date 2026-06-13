# W6: Pre-compiled Prelude & Separate Compilation Floor

- **Date:** 2026-06-13
- **Status:** Draft
- **Bead:** eu-kgsi.1
- **Roadmap:** W6 — Compiled-unit & prelude caching (separate compilation)

---

## 1. Problem

Every `eu` invocation re-parses, desugars, cooks, eliminates, inlines and
STG-compiles the entire prelude from source — a ~500–700 ms fixed tax that
dwarfs the actual work on typical runs. The prelude is embedded as source text
(`lib/prelude.eu` via `include_bytes!`) and its type information is already
cached (`PreludeSummary` / `PRELUDE_CACHE`), but its **compiled STG form** is
not: the prelude source is spliced into one core expression by `rebody`
(`src/core/expr.rs:548`) before STG compilation, and `stg::compile` runs once
on the whole merged evaluand.

This is the most visible performance problem in eucalypt. It makes CLI
scripting and CI usage feel sluggish and dominates LSP restart latency.

## 2. Goal

Compile the prelude **zero times at runtime**. The prelude's compiled STG,
operator table, and type summary are pre-computed at development time, stored
as a checked-in binary blob, and deserialised from `include_bytes!` at
startup. User code is compiled and linked against the prelude's global slots.

Output must be **byte-identical** to the current source-compiled pipeline
across the full test harness.

## 3. Design

### 3.1 The blob

A single binary file (`lib/prelude.blob`) containing four sections serialised
with **postcard** (a compact, no-std-compatible serde format):

```rust
#[derive(Serialize, Deserialize)]
struct PreludeBlob {
    /// SHA-256 of the prelude source (`lib/prelude.eu`) at generation time.
    source_hash: [u8; 32],

    /// Arena-flattened STG for each prelude binding.
    /// Index in this vec is the binding's global slot index
    /// (offset by `INTRINSIC_COUNT` to sit above intrinsic globals).
    bindings: Vec<ArenaLambdaForm>,

    /// Binding name → index into `bindings` vec.  The global slot for
    /// binding `i` is `INTRINSIC_COUNT + i`.  Used by the user-code
    /// STG compiler to resolve prelude names to `Ref::G` slots.
    name_to_slot: HashMap<String, usize>,

    /// Operator definitions for cook seeding.
    operators: HashMap<String, OperatorInfo>,

    /// Type schemes, aliases, branch shapes, operator overloads.
    type_summary: PreludeSummary,
}
```

### 3.2 Arena-flattened STG

`StgSyn` uses `Rc<StgSyn>` for structural sharing. For serialisation, the
tree is flattened into an arena where `Rc` pointers become `u32` indices:

```rust
/// Index into the arena vec.
type NodeIdx = u32;

/// Mirror of StgSyn with Rc<StgSyn> replaced by NodeIdx.
#[derive(Serialize, Deserialize)]
enum ArenaStgSyn {
    Atom { evaluand: ArenaRef },
    Case {
        scrutinee: NodeIdx,
        branches: Vec<(Tag, NodeIdx)>,
        fallback: Option<NodeIdx>,
        suppress_update: bool,
    },
    Cons { tag: Tag, args: Vec<ArenaRef> },
    App { callable: ArenaRef, args: Vec<ArenaRef> },
    Bif { intrinsic: u8, args: Vec<ArenaRef> },
    Let { bindings: Vec<ArenaLambdaForm>, body: NodeIdx },
    LetRec { bindings: Vec<ArenaLambdaForm>, body: NodeIdx },
    Ann { smid: Smid, body: NodeIdx },
    Meta { meta: ArenaRef, body: ArenaRef },
    DeMeta { scrutinee: NodeIdx, handler: NodeIdx, or_else: NodeIdx },
    BlackHole,
}

/// Mirror of Reference<Native> — identical, no Rc involved.
type ArenaRef = Reference<Native>;

/// Mirror of LambdaForm with Rc<StgSyn> replaced by NodeIdx.
#[derive(Serialize, Deserialize)]
enum ArenaLambdaForm {
    Lambda { bound: u8, body: NodeIdx, annotation: Smid },
    Thunk { body: NodeIdx },
    Value { body: NodeIdx },
}
```

**Flattening** (STG → arena): walk the `Rc<StgSyn>` tree, assigning each
unique `Rc` (by pointer identity) a `NodeIdx`. Shared subtrees get one index.
Output is a `Vec<ArenaStgSyn>` plus a root `NodeIdx`.

**Reconstruction** (arena → STG): bottom-up pass through the arena vec,
building `Rc<StgSyn>` nodes. Each `NodeIdx` maps to an `Rc` via a lookup
table, so shared subtrees produce shared `Rc` pointers — structural sharing
is preserved.

### 3.3 Serde support

The following types need `Serialize`/`Deserialize` derives:

- `Native` (already has no `Rc`)
- `Reference<Native>` (no `Rc`)
- `Smid` (`Option<NonZeroU32>`, trivial)
- `Tag` (enum, trivial)
- `OperatorInfo`, `Fixity`, `Precedence`
- `PreludeSummary` (and its constituents: `TypeScheme`, `Type`, `BranchShape`,
  `AliasMap`, `Constraint`)
- The arena types above (new, serde by construction)

`StgSyn` and `LambdaForm` themselves do **not** get serde derives — the arena
types are the serialisation boundary.

### 3.4 Global slot assignment

Currently, `Ref::G` indices are used only for intrinsics (indices 0 through
`INTRINSIC_COUNT - 1`). Prelude bindings are assigned global slots starting at
`INTRINSIC_COUNT`, in a fixed order determined by the blob's `bindings` vec.

The `name_to_slot` map provides the lookup for the user-code STG compiler:
when compiling a free variable reference that matches a prelude name, emit
`Ref::G(slot)` instead of a de Bruijn `Ref::L` index into the merged
let-nest.

The VM's global table (`machine/mod.rs`) is extended to hold prelude globals
alongside intrinsics. At startup, the prelude bindings are loaded into these
slots by interpreting the `ArenaLambdaForm`s.

### 3.5 Cook seeding

The `Distributor` in `src/core/cook/fixity.rs` currently discovers operators
only from `Operator(...)` nodes in the expression it processes. To cook user
code independently of the prelude source:

1. The `UnitInterface.operators` field (already populated by
   `extract_operators_from_expr`) is carried in the blob.
2. Before cooking user code, seed the `Distributor`'s environment (`Frame`)
   with the prelude's operator entries from the blob.
3. User-defined operators override prelude operators in the normal inside-out
   scope resolution.

### 3.6 Prelude pipeline changes

**Current pipeline** (every invocation):
```
load(prelude.eu) → translate → merge_units([prelude, user])
  → extract_operators → cook → eliminate → inline → fuse
  → verify → type_check → STG compile → load to heap → run
```

**New pipeline** (with blob):
```
deserialise(prelude.blob)
  → load prelude globals to VM global table
  → seed cook with prelude operators
  → seed type checker with PreludeSummary

load(user.eu) → translate
  → cook(seeded with prelude operators)
  → eliminate → inline → fuse → verify
  → type_check(seeded with PreludeSummary)
  → STG compile(prelude names → Ref::G)
  → load to heap → run
```

The prelude source is no longer parsed, desugared, cooked, or compiled at
runtime. The user unit is processed independently, with cross-unit information
flowing through the blob's operator table and type summary.

### 3.7 Shadowing and name redefinition

When a user unit (or an intermediate import) redefines a prelude name (e.g.
`map`), the local binding must win. This works naturally:

- The STG compiler resolves names inside-out through scopes. A local `map`
  binding produces a `Ref::L`; the prelude's `Ref::G` is only used when no
  local binding exists.
- Cook seeding: user-defined operators override prelude operators in the
  `Distributor`'s scope chain.
- Type checking: user annotations override `PreludeSummary` entries.

This must be covered by explicit test cases (see §5).

### 3.8 Source map

Pre-compiled prelude `Smid` values are set to `Smid::default()` (synthetic /
opaque). Prelude functions will appear in error traces without file/line
information. The user's call-site `Smid` (which is the useful diagnostic
location) is unaffected.

For debugging, `--source-prelude` (see §3.9) provides full source locations.

LSP features (go-to-definition, hover, completion) are **unaffected** — they
operate on the Rowan parse tree of the embedded prelude source text, not the
STG.

### 3.9 Runtime modes

Three modes, selected by blob availability and flags:

| Blob present? | Flag | Behaviour |
|---|---|---|
| No | — | Source prelude (current behaviour), build warning |
| Yes | — | Pre-compiled prelude (fast path) |
| Yes | `--source-prelude` | Source prelude (for debugging) |
| Yes (stale) | — | Source prelude, build warning |

**Build warnings** (emitted by `build.rs`):
- Blob missing: `"precompiled prelude not found — compiling from source each run. Run cargo xtask prelude-compile to generate."`
- Blob stale (hash mismatch): `"prelude blob is stale — run cargo xtask prelude-compile to regenerate."`

`build.rs` verifies `sha256(lib/prelude.eu)` against the blob's
`source_hash`. On mismatch or missing blob, it sets a cfg flag
(`prelude_blob_available = false`) so the driver knows to fall back.

An environment variable `EU_SOURCE_PRELUDE=1` is also accepted (useful for CI
or scripting without modifying command-line args).

### 3.10 The xtask

A workspace member at `xtask/` providing `cargo xtask prelude-compile`:

1. Loads `lib/prelude.eu` from the workspace root.
2. Runs the full front-end pipeline: parse → desugar → cook → eliminate →
   inline → fuse → verify → type-check.
3. Extracts operators into `OperatorInfo` map.
4. Extracts `PreludeSummary`.
5. STG-compiles the prelude independently.
6. Assigns global slot indices to bindings.
7. Flattens STG to arena form.
8. Serialises `PreludeBlob` with postcard.
9. Writes `lib/prelude.blob`.

The xtask links against the `eucalypt` library crate. It must be run
whenever `lib/prelude.eu` changes. CI release builds run it before
`cargo build`.

### 3.11 What this does NOT do

- **No on-disk cache** of user compilation results (security surface, marginal
  benefit — see roadmap §W6).
- **No general separate compilation** — only the prelude is pre-compiled. The
  arena format sets a precedent for extending this later (roadmap options
  B/C/D are post-1.0).
- **No long-running daemon** — deferred likewise.
- **No crate extraction** — the front-end pipeline stays in one crate. The
  xtask links against it as a dependency.

## 4. Implementation sketch

### Phase 1: Serde foundations
- Add `Serialize`/`Deserialize` to `Native`, `Reference<T>`, `Smid`, `Tag`,
  `OperatorInfo`, `Fixity`, `Precedence`.
- Add serde to `PreludeSummary` and its type system constituents (`Type`,
  `TypeScheme`, `BranchShape`, `AliasMap`, `Constraint`).
- Add `postcard` to dependencies.

### Phase 2: Arena types and flattening
- Define `ArenaStgSyn`, `ArenaLambdaForm`, `ArenaRef` with serde derives.
- Implement `flatten(Rc<StgSyn>) → (Vec<ArenaStgSyn>, NodeIdx)`.
- Implement `reconstruct(Vec<ArenaStgSyn>, NodeIdx) → Rc<StgSyn>`.
- Round-trip property test: flatten → reconstruct → compare.

### Phase 3: Blob generation (xtask)
- Create `xtask/` workspace member.
- Implement `PreludeBlob` struct and serialisation.
- Implement the prelude compilation pipeline in the xtask (reusing library
  functions).
- Assign global slot indices.
- Write `lib/prelude.blob`.

### Phase 4: `build.rs` verification
- Read `lib/prelude.eu`, compute SHA-256.
- If `lib/prelude.blob` exists, deserialise header and compare hashes.
- Set `cfg(prelude_blob_available)` accordingly.
- Emit build warnings on missing/stale blob.

### Phase 5: Runtime loading
- Extend the VM global table to hold prelude bindings above intrinsics.
- Deserialise blob at startup, reconstruct STG, load globals.
- Wire `--source-prelude` / `EU_SOURCE_PRELUDE` flag.
- Fallback path when blob unavailable.

### Phase 6: Cook seeding
- Plumb blob operator table into `Distributor` as a seed frame.
- User operators override prelude operators.

### Phase 7: STG compiler changes
- When compiling a free variable that matches a prelude name, emit
  `Ref::G(name_to_slot[name])`.
- Ensure local bindings shadow prelude globals (inside-out resolution).

### Phase 8: Validation
- Full harness byte-identical to source-compiled output.
- Shadowing tests: user redefines `map`, `+`, etc.
- Round-trip: source-compile vs blob-compile produce identical STG.
- Benchmark: measure cold-start latency reduction.
- `EU_GC_VERIFY=2` + `EU_GC_STRESS=1` + `EU_GC_POISON=1` across harness.
- `--source-prelude` produces identical output.

## 5. Test plan

- **Byte-identical output**: full harness and conformance corpus with
  pre-compiled prelude vs source prelude — rendered output must match.
- **Shadowing**: test files that redefine prelude names (`map`, `filter`, `+`,
  a custom operator) and verify the local definition is used.
- **Operator seeding**: user code using prelude operators (`+`, `*`, `<<`,
  `>>`, pipeline operators) cooks correctly without prelude source.
- **Intermediate imports**: a chain `prelude → lib.eu → user.eu` where
  `lib.eu` redefines a prelude name — user sees lib's version.
- **Round-trip property**: `flatten(stg) |> reconstruct |> flatten` produces
  identical arena.
- **Stale blob**: modify `lib/prelude.eu`, build without regenerating blob,
  verify build warning and source fallback.
- **Missing blob**: build without blob, verify build warning and source
  fallback.
- **`--source-prelude`**: with blob present, flag produces identical output.
- **Latency benchmark**: cold-start `eu -e :ok` with and without blob.
- **GC verification**: full harness under `EU_GC_VERIFY=2` /
  `EU_GC_POISON=1`.

## 6. Risks

- **`PreludeSummary` serialisation surface**: `Type` is a large enum with
  recursive variants. Deriving serde on it is straightforward but the
  serialised size could be significant. Mitigate: measure blob size; postcard
  is compact.
- **Global slot stability**: if the prelude's binding order changes, all slot
  indices shift. This is fine because the blob and the user compiler are
  always in the same binary — there is no cross-binary ABI. But the xtask
  must be re-run.
- **Structural sharing fidelity**: the flatten/reconstruct round-trip must
  preserve sharing exactly, or memory usage regresses. Mitigate: property
  test comparing `Rc::as_ptr` counts before and after.
- **Shadowing edge cases**: prelude names used as operator definitions in user
  code, or as monad-namespace bindings. Mitigate: explicit test cases.

## 7. Success criteria

- **Cold-start latency** on `eu -e :ok` drops by the prelude's compilation
  share (~500–700 ms on current hardware).
- **Output byte-identical** across the full harness and conformance corpus.
- **No GC regression** under verification.
- **Clean fallback** when blob is missing or stale — no build failure, just a
  warning and source-prelude behaviour.
- **LSP unaffected** — go-to-definition, hover, completion work identically.
