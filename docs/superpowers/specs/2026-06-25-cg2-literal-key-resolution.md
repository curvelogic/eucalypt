# CG2: Literal-Key Resolution

- **Bead:** eu-r92e
- **Pillar:** CG — Demand- and type-directed compilation (type-free tier)
- **Release:** 0.11
- **Date:** 2026-06-25

---

## 1. Problem

Every `.key` lookup compiles to a call to the `LookupOr` intrinsic
wrapper (`src/eval/stg/block.rs:477`), which is a multi-step STG
program: switch on the block tag, enter a `letrec` find-loop, case on
the BIF result, and per-element `MatchesKey` comparison. Even though
symbols are already interned to `SymbolId` at load time
(`src/eval/memory/loader.rs:98–100`), the wrapper machinery is
executed for every lookup — the switch, the letrec allocation, the
case branches, the BIF deferral dance in `step()`.

For hot loops that repeatedly look up the same literal key (e.g.
`.grid` in AoC day11-p2), the overhead of the wrapper is pure waste:
the key is statically known, the block destructure is mechanical, and
the index/scan could be done in a single VM step.

## 2. Design

### 2.1 New IR node

A new `LookupLit` variant replaces the `LookupOr` wrapper for
literal-key lookups.

**`StgSyn`** (`src/eval/stg/syntax.rs`):
```rust
LookupLit { smid: Smid, key: Ref, obj: Ref, default: Ref }
```

Where `key` is `Ref::V(Native::Sym("field_name"))` — a literal symbol
known at compile time.

**`HeapSyn`** (`src/eval/memory/syntax.rs`):
```rust
LookupLit { smid: Smid, key: Ref, obj: Ref, default: Ref }
```

After loading, `key` is `Ref::V(Native::Sym(symbol_id))` — the
string has been interned to a `SymbolId` by the loader as today.

**`ArenaStgSyn`** (`src/eval/stg/arena.rs`):
```rust
LookupLit { smid: Smid, key: Ref, obj: Ref, default: Ref }
```

The loader, GC scan, and blob serialisation handle `LookupLit`
following the same patterns as existing nodes. The `key`, `obj`, and
`default` refs are scanned by GC identically to args in `App`.

### 2.2 Compilation (compile_lookup)

In `compile_lookup` (`src/eval/stg/compiler.rs:1530`), the current
code at line 1569:

```rust
let lookup_stg = LookupOr(NativeVariant::Unboxed).global(dsl::sym(key), dft, obj);
```

is replaced with:

```rust
let lookup_stg = Rc::new(StgSyn::LookupLit {
    smid: annotation,
    key: Ref::V(Native::Sym(key.to_string())),
    obj,
    default: dft,
});
```

The wrapping `Ann` node (lines 1572–1576) is no longer needed — the
`Smid` is carried inline, as with `DirectApp` (CG1).

This fires for all `.key` lookups unconditionally — `compile_lookup`
is only called when the key is a literal string from the source.

### 2.3 VM fast path

In `handle_instruction` (`src/eval/machine/vm.rs`),
`HeapSyn::LookupLit` gets a dedicated match arm:

1. **Set `self.annotation = smid`.**
2. **Resolve `obj`** to a closure and check it is a block
   (`DataConstructor::Block`). If not, raise `NoBranchForDataTag`
   with the `smid` (same error as today's wrapper).
3. **Extract the block's list and index** from the `Block` data
   constructor's args.
4. **Resolve `key`** to its `SymbolId`.
5. **Try the block index** (if present): call
   `map.get(&symbol_id)` on the `Native::Index` to get the
   position, then walk the list to that position.
6. **Fall back to linear scan** (if no index or below threshold):
   walk the cons-list comparing each key's `SymbolId` against the
   lookup key.
7. **On hit:** set `self.closure` to the found value and continue.
8. **On miss:** resolve `default` to a closure, set `self.closure`
   to it, and continue. This naturally handles both user-provided
   defaults and `lookup_fail` error thunks.

No new continuation type is needed. Both the hit and miss paths
simply set `self.closure` and continue the main loop.

The block index building logic (for blocks above the threshold)
should be preserved — if the block lacks an index and is large
enough, build one and cache it, as `LookupOr::execute` does today.

### 2.4 What this replaces

For a `.key` lookup, the current path is:

1. `Ann` dispatch step (set annotation)
2. `LookupOr` wrapper: switch on block tag
3. `letrec` allocating the find-loop closure
4. BIF dispatch (`LookupOr::execute` via deferred `pending_bif`)
5. Case on BIF result (ListCons = found, ListNil = scan)
6. If scanning: recursive find-loop with per-element `MatchesKey`
   BIF calls

The new path is:

1. `LookupLit` dispatch step (set annotation + do the lookup)

One VM step replaces ~6+ steps.

## 3. Scope

### In scope

- `.key` lookups via `compile_lookup` — the key is always a literal
  string in this path.
- All three IR representations (`StgSyn`, `HeapSyn`, `ArenaStgSyn`)
  and the loader, GC scan, and blob serialisation.
- Block index lookup (indexed blocks) and linear scan (small blocks)
  in the VM handler.
- Index building and caching for large blocks on first `LookupLit`
  access.

### Out of scope

- `has(:key)` and `lookup(:k, block)` — these are prelude/intrinsic
  function calls. They continue through their existing wrappers and
  benefit from CG1 (direct dispatch) but not from the specialised
  lookup instruction.
- Dynamic (non-literal) key lookups — these remain on the `LookupOr`
  path.
- Pre-resolving to a positional offset (Level 2) — requires shape/type
  knowledge, deferred to CG5 (type-gated tier).

## 4. Success Criteria

1. **Correctness:** full harness green (`cargo test --test harness_test`).
2. **GC safety:** `EU_GC_VERIFY=2` + `EU_GC_POISON=1` passes on the
   full harness.
3. **Output identity:** rendered output byte-identical across the
   conformance corpus.
4. **Diagnostics:** error messages for dot-on-non-block preserve source
   locations. Existing error harness tests pass.
5. **Observability:** `eu dump stg` shows `LookupLit` nodes where
   `LookupOr` wrappers used to appear for `.key` lookups.
6. **Performance:** measurable tick reduction on a lookup-heavy
   benchmark (e.g. `day11-p2` or a targeted harness test) under both
   blob and `--source-prelude` modes.

## 5. Testing

- Existing harness covers correctness (byte-identical output).
- Existing error tests for dot-on-non-block cover diagnostic
  preservation.
- `EU_GC_VERIFY=2` + `EU_GC_POISON=1` on the full harness run.
- Before/after tick comparison on lookup-heavy programs.
- Verify `eu dump stg` output shows `LookupLit` nodes.

## 6. Risks

- **Low:** the change is isolated to `compile_lookup` and a new VM
  match arm. `LookupOr` remains for all dynamic lookups.
- **Medium:** the VM handler must correctly replicate `LookupOr`'s
  semantics — block destructure, index lookup, linear scan, index
  building. The existing `LookupOr::execute` Rust code can be
  largely reused or extracted into a shared function.
- **Blob version:** `ArenaStgSyn::LookupLit` must be handled in the
  postcard codec. The blob version should be bumped (coordinate with
  CG1 if landing in the same release).
