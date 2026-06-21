# Spec: Render Unification with IoTransparentBranch

**Bead**: eu-ogvr  
**Author**: Quill  
**Date**: 2026-06-21  
**Status**: Implemented

## 1. Problem: Double Evaluation

The pipeline previously evaluated programs twice for non-IO document output:

1. **Headless evaluation**: STG compiled with `RenderType::Headless` (no
   RENDER_DOC wrapper). Machine ran → evaluated program to WHNF → terminated.
2. **Render step**: Driver inspected the result. If no IO yield, built
   `RENDER_DOC(value)` on the existing heap via `BuildRenderDoc` mutator,
   called `machine.resume_for_render()`, and re-ran.

This double evaluation meant every binding in the program was entered at
least once during headless eval, and any binding reachable from the output
block was entered again during RENDER_DOC traversal. AtMostOnce bindings
were thus entered twice, breaking update elision.

A fixup pass (PR #880) forced Multi cardinality on `DefaultBlockLet` scopes
whose body is a `Block` constructor. This was correct but conservative.

## 2. Design: IoTransparentBranch

The solution is to always compile with `RenderType::RenderDoc` and use a
new `IoTransparentBranch` continuation variant that yields IO constructors
to the driver instead of trying to match them.

### Core Idea

RENDER_DOC wraps the user's program. When the STG machine forces the user
expression to WHNF inside RENDER_DOC's wrapper:

- **Pure value** (block, list, string, number): The IoTransparentBranch
  falls through to its fallback, which continues with RENDER_DOC's normal
  rendering pipeline (emit_doc_start → Render → emit_doc_end).
- **IO constructor** (IoBind, IoReturn, etc.): The IoTransparentBranch
  detects the IO tag and yields to the driver, exactly as the empty-stack
  check did previously.
- **IO function** (PAP waiting for world): The IoTransparentBranch yields
  from `return_fun()`, signalling the driver to inject world.

### Why This Works

The previous IO yield mechanism checked for IO constructors only when the
continuation stack was empty. With RENDER_DOC always present, the stack
is never empty when the user expression evaluates — RENDER_DOC's
continuations sit above.

IoTransparentBranch solves this by acting as a transparent window for IO:
it sits where the first `case` continuation would normally be (inside
RENDER_DOC's wrapper), and instead of trying to match IO tags against
render branches, it yields them to the driver.

For non-IO data, it dispatches normally through branch tables or fallback,
so RENDER_DOC's rendering proceeds as before.

## 3. Implementation

### New STG Syntax: `IoTransparentCase`

```rust
StgSyn::IoTransparentCase {
    scrutinee: Rc<StgSyn>,
    branches: Vec<(Tag, Rc<StgSyn>)>,
    fallback: Option<Rc<StgSyn>>,
}
```

Identical in structure to `Case`, but pushes `IoTransparentBranch` instead
of `Branch`. A DSL helper `io_transparent_force(scrutinee, then)` creates
an `IoTransparentCase` with no branches and a fallback — used by
RENDER_DOC's wrapper.

### New Continuation: `IoTransparentBranch`

```rust
Continuation::IoTransparentBranch {
    min_tag: Tag,
    branch_table: Array<Option<RefPtr<HeapSyn>>>,
    fallback: Option<RefPtr<HeapSyn>>,
    environment: RefPtr<EnvFrame>,
    annotation: Smid,
}
```

Same fields as `Branch`. Behaviour differs in three methods:

- **`return_data()`**: If the tag is an IO constructor, yield
  (`terminated = true`, `yielded_io = true`). Otherwise, dispatch normally
  via branch table or fallback.
- **`return_fun()`**: Always yield. A function result means an IO PAP
  waiting for world injection.
- **`return_native()`**: Use fallback, same as `Branch`.

### RENDER_DOC Wrapper Change

The `RenderDoc::wrapper()` method now uses `io_transparent_force`:

```rust
fn wrapper(&self, _annotation: Smid) -> LambdaForm {
    lambda(1,
        io_transparent_force(
            local(0),  // force the renderee
            // fallback for non-IO: render normally
            force(
                call::bif::emit_doc_start(),
                force(Render.global(lref(1)), call::bif::emit_doc_end()),
            ),
        ),
    )
}
```

### Driver Change

The driver now always compiles with `RenderType::RenderDoc` (except when
`RenderFragment` is already set for test mode). After running the machine:

- **IO yielded + function**: Inject world token and run `io_run_and_render`.
- **IO yielded + data**: Run `io_run_and_render`.
- **Normal termination**: Done. Exit code passes through from the machine.

This replaces the previous multi-step dispatch (headless eval → check IO →
inject world → check IO again → `render_headless_result`).

### Files Modified

| File | Change |
|------|--------|
| `src/eval/stg/syntax.rs` | `StgSyn::IoTransparentCase` variant + `io_transparent_force` DSL |
| `src/eval/memory/syntax.rs` | `HeapSyn::IoTransparentCase` + GC scanning |
| `src/eval/memory/loader.rs` | Load `StgSyn::IoTransparentCase` → `HeapSyn::IoTransparentCase` |
| `src/eval/machine/cont.rs` | `Continuation::IoTransparentBranch` + Display + GC |
| `src/eval/machine/vm.rs` | Enter IoTransparentCase; handle IoTransparentBranch in return_data/fun/native |
| `src/eval/stg/render.rs` | `RenderDoc::wrapper()` uses `io_transparent_force` |
| `src/driver/eval.rs` | Always `RenderDoc`; simplified post-run dispatch |
| `src/eval/stg/optimiser.rs` | Handle `IoTransparentCase` in optimiser |
| `src/eval/stg/pretty.rs` | Pretty-print `IoTransparentCase` |
| `src/eval/stg/arena.rs` | Arena variant for `IoTransparentCase` |
| `src/eval/stg/embed.rs` | Embed `IoTransparentCase` as `[:s-io-case ...]` |

## 4. Acceptance Criteria

1. All 451 harness tests pass
2. IO programs (`io.shell`, `io.exec`, `io.bind`) work unchanged
3. IO error tests (`tests/harness/errors/`) pass
4. `cargo clippy --all-targets -- -D warnings` clean
5. `echo '{ io.shell("echo hello") }' | timeout 10 ./target/release/eu --allow-io -` works
6. No regression on 010_prelude allocations (≤ 119K)
7. Single-pass evaluation: no `render_headless_result()` or `resume_for_render()` in the hot path

## 5. Future Work

- **Remove the rendered-block fixup** from PR #880's demand analysis. Since
  RENDER_DOC now renders in a single pass, the fixup that forces Multi on
  `DefaultBlockLet` scopes with Block body should no longer be needed.
  AtMostOnce should be sound for all scopes.
- **Remove `BuildRenderDoc` mutator and `resume_for_render()`** if they
  become dead code after this change.
