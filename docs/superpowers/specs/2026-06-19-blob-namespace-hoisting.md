# eu-9tah.7: Hoist Namespace Lambdas into Prelude Blob

**Date**: 2026-06-19
**Bead**: eu-9tah.7
**Agent**: Quill
**Parent spec**: `2026-06-19-0.10.0-release-design.md`
**Depends on**: eu-398r (namespace hoisting pass, merged PR #850)

## Motivation

The namespace hoisting pass (eu-398r) extracts namespace members like
`str.upper` to top-level bindings and rewrites `Lookup` nodes to direct
variable references. However, the pass only runs in the user-code pipeline.
With the blob prelude active (the default), namespace blocks aren't in the
core expression — so hoisting is a no-op for prelude namespaces.

Meanwhile, the blob pipeline (`xtask/src/main.rs`) produces one global per
top-level prelude binding. `map`, `filter`, etc. get individual `Ref::G`
slots. But `str.upper`, `cal.now`, `vec.len` etc. are buried inside
namespace block globals — accessing them requires a runtime `LookupOr` into
the block, which is O(1) but still involves a native function call, symbol
comparison, and block traversal on every invocation.

The fix: run the hoisting pass in the blob pipeline so that namespace
members become individual globals, accessible via `Ref::G` like any other
prelude function.

## Design

### Add hoisting to the blob pipeline

In `xtask/src/main.rs`, insert `hoist_namespaces()` after cook and before
the peel step:

```
cook → hoist_namespaces → fuse_destructure → type-check → peel → compile
```

Current pipeline (line 134-143):
```rust
loader.cook().context("cook")?;
loader.fuse_destructure().context("fuse_destructure")?;
```

New pipeline:
```rust
loader.cook().context("cook")?;
loader.hoist_namespaces().context("hoist_namespaces")?;
loader.fuse_destructure().context("fuse_destructure")?;
```

### What happens automatically

After hoisting, the core expression has new top-level `OtherLet` bindings
like `__str_to-upper`, `__cal_zdt`, etc. These flow through the existing
pipeline without further changes:

1. **Peeling** (`peel_all_let_bindings`): the hoisted bindings are peeled
   like any other top-level `Let` binding, producing
   `("__str_to-upper", body)` entries in `binding_bodies`.

2. **`name_to_slot`**: the hoisted names get slots automatically since the
   mapping is built from `binding_bodies` (line 260-264).

3. **`inline_cores` collection** (step 4b): hoisted members that are bare
   intrinsics or simple combinator lambdas will be collected by the
   fixed-point iteration, exactly like existing prelude combinators. This
   means `__str_to-upper` (which is just `Intrinsic("__STR_UPPER")`) will
   be available for user-code inlining via `inject_prelude_inline_cores`.

4. **STG compilation**: each hoisted binding body is compiled individually
   with `prelude_globals` set, producing `Ref::G` references to other
   prelude globals.

5. **User code resolution**: when the user writes `"hello" str.upper`, the
   core expression after desugaring contains `Lookup(Var("str"), "upper")`.
   The user-code hoisting pass rewrites this to `Var("__str_upper")`. The
   compiler's `resolve_free_var` looks up `"__str_upper"` in
   `prelude_globals` and emits `Ref::G(INTRINSIC_COUNT + slot)`.

### Lookup rewriting in user code

The user-code hoisting pass already rewrites `Lookup(Var("str"), "upper")`
→ `Var(Free("__str_upper"))`. For this to resolve, `"__str_upper"` must be
in the blob's `name_to_slot` map. This happens automatically once hoisting
runs in the blob pipeline (step 2 above).

**Important**: the user-code hoisting pass must know which namespace members
were hoisted in the blob. Currently it discovers namespaces by scanning the
core expression for `DefaultBlockLet` blocks. With the blob active, those
blocks aren't present. Two approaches:

**Option A (recommended)**: The blob's `name_to_slot` already lists all
global names. The user-code hoisting pass can check whether `__<ns>_<member>`
exists in `prelude_globals` — if so, rewrite the `Lookup` to
`Var(Free("__<ns>_<member>"))`. No need to scan for namespace blocks at all.

**Option B**: Store the set of hoisted namespace member names in the blob
metadata (`PreludeBlob` struct) and pass it to the user-code pipeline. More
explicit but requires blob format changes.

### Original namespace blocks preserved

The namespace blocks (`str`, `cal`, `vec`, etc.) remain as globals in the
blob. They're still accessible as values (`block?(str)`, dynamic lookups).
The hoisting adds copies, not moves — same as eu-398r.

## Acceptance Criteria

### Precompilation
1. `cargo xtask prelude-compile` succeeds and reports hoisted namespace
   members in its output
2. The blob contains individual globals for namespace members
   (e.g. `__str_to-upper`, `__cal_zdt`)
3. User code `"hello" str.upper` compiles to `Ref::G` (verify via
   `eu dump stg` — should show a global reference, not `LookupOr`)
4. Original namespace blocks remain accessible (`block?(str)` still works)

### Inlining
5. Hoisted namespace intrinsics (e.g. `__str_to-upper`) appear in the
   blob's `inline_cores` — they must be collected by the fixed-point
   `inline_cores` iteration alongside existing combinator lambdas
6. `inject_prelude_inline_cores` delivers them to the user-code inline pass
7. `eu dump inlined` on user code using namespace functions shows the
   intrinsic call directly (e.g. `UPPER("hello")`), not a `LookupOr` or
   a `Var` reference — confirming the inliner has beta-reduced the call

### Parity
8. Namespace functions benefit from precompilation and inlining to the
   same degree as top-level prelude functions like `map`, `filter`, `fold`
   — there must be no performance distinction between `xs map(f)` and
   `s str.upper` at the compiled STG level

### Quality
9. Full test suite passes: `cargo test`
10. `cargo clippy --all-targets -- -D warnings` clean
11. `cargo fmt --all` clean
12. No performance regression on AoC examples

## Out of Scope

- Changing the hoisting pass itself (eu-398r already handles the algorithm)
- User-defined namespace hoisting (already works via eu-398r)
- Blob format versioning (the blob is regenerated on every build)
