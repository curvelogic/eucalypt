# Flat Closures: Eliminating Environment Chain Traversal

**Bead**: eu-9tah.18
**Date**: 2026-06-21
**Status**: Spec v4 — final, ready for implementation

## Problem

CPU profiling shows `EnvironmentFrame::get` at **69% self-time** on
day05-p1 (post HeapNavigator fix). The CPU is spending the majority of
its cycles pointer-chasing through linked environment frames.

The current cactus stack representation (`next` pointer chain) has
been confirmed by assembly analysis as a cache-miss-dominated pointer
chase that LLVM cannot optimise away. Only changing the representation
addresses the root cause.

## Free Variable Data (measured)

| Program | Closures | Avg FV | Max FV | 0 FV | 2 FV |
|---------|----------|--------|--------|------|------|
| 010_prelude | 3,458 | 1.1 | 5 | 28% | 41% |
| day01-p1 | 9,620 | 1.0 | 3 | 50% | 50% |
| day05-p1 | 2,428 | 1.0 | 4 | 50% | 49% |

Average 1.0 FV. Bimodal: 0 or 2. Max 5.

## Design

### Environment frame

```rust
EnvironmentFrame {
    locals: Array<SynClosure>,
    captures: Array<CaptureEntry>,
    remap: [u8; 4],       // retained for case destructuring
    remap_len: u8,
    annotation: Smid,
    // `next` REMOVED
}

struct CaptureEntry {
    frame: RefPtr<EnvFrame>,
    index: u16,            // physical index into frame.locals
}
```

Frames with 0 captures have an empty captures array (no backing
allocation).

### Ref types

```rust
enum Ref {
    Local(u16),      // index into current frame's locals
    Capture(u16),    // index into current frame's captures
    G(usize),        // global (unchanged)
}
```

`Ref::L(depth, offset)` is removed from compiler output. Retained
only for blob deserialisation (source prelude mode makes this
unnecessary in Phase 1).

`handle_instruction` has separate dispatch arms for `Local` and
`Capture`. No runtime type check.

### Access paths

**`Ref::Local(i)`**: `frame.locals[i]` — one load.

**`Ref::Capture(i)`**: `frame.captures[i] → (src, idx) →
src.locals[idx]` — two loads. First load is local (same cache
line as frame). Second is one remote load.

### Capture recipes

Each `LambdaForm` stores a capture recipe:

```rust
enum CaptureInstruction {
    CopyCapture(u16),    // copy enclosing.captures[i]
    CaptureLocal(u16),   // create (enclosing_frame, physical_idx)
}
```

The compiler builds the recipe when compiling a lambda/thunk by
mapping each depth > 0 ref to its origin in the compile-time scope
stack. Remap is applied at recipe creation time (physical index
stored, not logical).

### Closure creation

`from_let`, `from_letrec`, `saturate`, `partially_apply`, and
`from_closure` all follow the same pattern:

1. Create locals array (args or bindings)
2. Read capture recipe from LambdaForm
3. Iterate recipe, populate captures from enclosing env
4. Allocate frame with locals + captures

Cost: O(num_captures) per entry. At avg 1.0, one tuple copy.

### Thunk updates

Updates write to `locals[index]` in the source frame. Captures are
read-only references. All consumers see updates because they point
to the same source slot. Update continuation's `(environment, index)`
always targets a local.

### GC and evacuation

Capture entries contain `RefPtr<EnvFrame>` — traced and updated by
standard GC machinery. Physical index is stable across evacuation.
No interior pointers. No new GC complexity.

### Prelude blob

The blob pipeline (`cargo xtask prelude-compile`) uses the same STG
compiler as user code. Once the compiler emits `Ref::Local`/
`Ref::Capture`, xtask automatically produces a blob with the new
refs. No separate blob pipeline changes needed.

**The blob MUST be regenerated as part of the flat closures PR.**
Run `cargo xtask prelude-compile` after compiler changes and commit
the updated `lib/prelude.blob`. The build.rs staleness check only
hashes `prelude.eu` source — it does NOT detect compiler format
changes. A stale blob with old `Ref::L` refs would cause runtime
failures.

**Verification**: source prelude mode (`EU_SOURCE_PRELUDE=1`) must
also pass all tests. This confirms the compiler produces correct
output independent of the blob. If the blob is somehow wrong, source
prelude mode serves as a fallback for diagnosis.

**Serialisation**: adding `Ref::Local` and `Ref::Capture` to the
`Ref` enum changes the postcard serialisation format. The blob
regeneration handles this. Old blobs are incompatible and must be
regenerated.

**CI**: the prelude freshness CI job compares the checked-in blob
against a fresh `cargo xtask prelude-compile`. This will catch any
mismatch between the compiler and the blob.

### What is removed

- `EnvironmentFrame::next` field
- `EnvironmentFrame::cell()` method
- `Ref::L(depth, offset)` from compiler output
- The reflatten pass (`src/core/reflatten.rs`) — SCC nesting no
  longer affects env depth
- Shared-backing array logic (remap stays for case destructuring)

## Implementation — Two Streams

The work splits into two streams that converge on a single branch.

### Stream A: Runtime (Furnace)

Changes to the VM and memory layer. Can be developed and tested
against hand-constructed STG, without the compiler changes.

**Files:**
- `src/eval/machine/env.rs` — new EnvFrame layout: remove `next`,
  add `captures: Array<CaptureEntry>`. New `get_capture()` method.
  Remove `cell()`. Keep `get()` for locals. Keep remap for case
  destructuring.
- `src/eval/machine/env_builder.rs` — update `from_let`,
  `from_letrec`, `from_closure`, `from_saturation`,
  `partially_apply`, `from_args` to populate captures from recipes.
  Add recipe resolution logic.
- `src/eval/machine/vm.rs` — add `Ref::Local` and `Ref::Capture`
  dispatch arms in `handle_instruction` (return_con, return_fun,
  step). Handle new ref types in HeapNavigator.
- `src/eval/memory/syntax.rs` — add `Ref::Local(u16)`,
  `Ref::Capture(u16)` to Ref enum. Add `CaptureEntry` struct.
  Remove `Ref::L`.
- `src/eval/memory/collect.rs` — update EnvFrame's `GcScannable`
  impl to scan captures array.

**Testing strategy:** Unit tests with hand-built STG using the new
ref types. Verify capture resolution, thunk update visibility,
GC safety under `EU_GC_VERIFY=2`.

### Stream B: Compiler (Quill)

Changes to the STG compiler to emit the new refs and build capture
recipes.

**Files:**
- `src/eval/stg/compiler.rs` — core change. Maintain a capture map
  per closure. Var at depth 0 → `Ref::Local(offset)`, depth > 0 →
  `Ref::Capture(capture_index)`. Build `CaptureInstruction` recipes.
- `src/eval/stg/syntax.rs` — add `CaptureInstruction` enum and
  `capture_recipe` field to `LambdaForm`.
- `src/eval/stg/pretty.rs` — display new ref types.
- `src/eval/stg/arena.rs` — handle new refs.
- `src/eval/stg/embed.rs` — handle new refs.
- `src/eval/stg/optimiser.rs` — update if optimiser inspects refs.
- `src/core/reflatten.rs` — remove (no longer needed).

**Testing strategy:** `eu dump stg` to verify correct ref output.
Full test suite.

### Integration and blob

Both streams converge on one branch. Runtime first — testable with
hand-built STG. Compiler connects when ready.

After both streams are integrated and tests pass:
1. Run `cargo xtask prelude-compile` to regenerate `lib/prelude.blob`
2. Commit updated blob
3. Verify all tests pass in BOTH blob and source prelude modes
4. Profile and measure

**Shared types** (`CaptureEntry`, `CaptureInstruction`, new `Ref`
variants) defined first as an initial commit before both streams
diverge.

## Acceptance Criteria

1. All tests pass (with source prelude mode)
2. `cargo clippy --all-targets -- -D warnings` clean
3. Profiling shows self-time reduction in env access on day05-p1
4. No GC correctness issues under `EU_GC_VERIFY=2`
5. AoC examples complete and produce correct output
6. Before/after wall time comparison on day05-p1, day01-p1, day12-p1
7. If improvement < 10%: revert (git revert the PR)

## Risks

**Allocation pressure**: 16 bytes per CaptureEntry (alignment). At
avg 1.0 captures, 16 extra bytes per frame. Increases heap usage.
Mitigated by: 28-50% of frames have 0 captures. Bump allocation is
fast. GC scan cost roughly equivalent (one RefPtr per capture vs
one RefPtr for next).

**Magnitude uncertainty**: theoretical ~30% improvement. Could be
10-15% if cache misses dominate over loop overhead. Phase 2
measurement is the decision point. Criterion 7 sets the threshold.

**Compiler complexity**: free variable analysis + capture recipe
generation changes the compiler's scope tracking. Mitigated by:
the compiler already tracks scope depth for Ref::L emission — the
information exists, just needs different output.

**Blob staleness**: build.rs only hashes `prelude.eu` source, not
compiler format. A stale blob with old `Ref::L` refs would crash at
runtime. The blob MUST be regenerated (`cargo xtask prelude-compile`)
and committed as part of the PR. CI prelude freshness job catches
any mismatch.
