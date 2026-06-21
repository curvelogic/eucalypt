# Spec: Render Unification — Eliminate Double Evaluation

**Bead**: eu-ogvr  
**Author**: Quill  
**Date**: 2026-06-21  
**Status**: Analysis — architectural blocker identified

## 1. Problem: Double Evaluation

The current pipeline evaluates programs twice for non-IO document output:

1. **Headless evaluation**: STG compiles with `RenderType::Headless` (no
   RENDER_DOC wrapper). Machine runs → evaluates program to WHNF → terminates.
2. **Render step**: Driver inspects the result. If no IO yield, builds
   `RENDER_DOC(value)` on the existing heap via `BuildRenderDoc` mutator,
   calls `machine.resume_for_render()`, and re-runs.

This double evaluation means every binding in the program is entered at least
once during headless eval, and any binding reachable from the output block is
entered again during RENDER_DOC traversal. AtMostOnce bindings are thus
entered twice, breaking update elision.

We currently work around this with a fixup pass (PR #880) that forces Multi
cardinality on `DefaultBlockLet` scopes whose body is a `Block` constructor.
This is correct but conservative — it prevents update elision for all
rendered block bindings, even those that genuinely are single-use within the
render traversal.

### Current Code Flow (`eval.rs` lines 280–416)

```
compile(Headless) → machine.run()
  ├── io_yielded? → io_run_and_render()     [IO path]
  ├── inject_world_and_run()
  │     ├── io_yielded? → io_run_and_render() [IO function path]
  │     └── no yield → render_headless_result() [pure document path]
  └── error → propagate
```

### Current Code Flow (`io_run.rs`)

- `render_headless_result()` (line 1602): Takes `machine.current_closure()`,
  builds `RENDER_DOC(value)` via `BuildRenderDoc` mutator, calls
  `machine.resume_for_render()`, runs machine.
- `io_run_and_render()` (line 1629): Runs `io_run()` to get pure value from
  IO monad, then builds `RENDER_DOC(value)` and re-runs machine.
- `BuildRenderDoc` mutator (line 1029): Builds `App { callable: G(RENDER_DOC),
  args: [L(0)] }` on the heap.

## 2. Proposed Design: Always Compile with RENDER_DOC

The idea: compile `RENDER_DOC(user_expr)` from the start instead of
`user_expr` alone. Single evaluation + single render in one pass.

### Pure Value Path (No IO)

1. Compile: `RENDER_DOC(user_expr)`
2. Machine runs → RENDER_DOC forces `user_expr` to WHNF → renders it
3. Single evaluation. Done.

### IO Path (the complication)

1. Compile: `RENDER_DOC(user_expr)`
2. Machine runs → RENDER_DOC forces `user_expr`
3. `user_expr` evaluates to IO constructor (e.g. `IoBind(world, cont, action)`)
4. **BLOCKER**: The IO constructor cannot yield to the driver because
   RENDER_DOC's continuations are on the stack

## 3. Architectural Blocker: IO Yield vs RENDER_DOC Continuations

### How IO Yield Works

The machine's `return_con()` method (vm.rs line 889) checks for IO
constructors **only when the continuation stack is empty**:

```rust
} else if DataConstructor::is_io_constructor(tag) {
    // IO constructor at the top level with nothing left to
    // consume it — yield to the io-run driver loop
    self.terminated = true;
    self.yielded_io = true;
} else {
    self.terminated = true
}
```

This is an `else if` — it only fires when there is no continuation to match.

### What Happens with RENDER_DOC Wrapping

When `RENDER_DOC(user_expr)` is compiled, the execution sequence is:

1. Enter RENDER_DOC → push `force` continuation for `emit_doc_start()`
2. `emit_doc_start()` emits → push continuation for `force(Render.global(...), emit_doc_end())`
3. Enter `Render.global(user_expr)` → pushes `case` continuation matching
   data constructor tags (Block, ListCons, BoxedString, etc.)
4. `user_expr` evaluates to WHNF
5. If WHNF is an IO constructor → `return_con()` finds the `case` continuation
   from step 3 on the stack → tries to match the IO tag → **no branch matches**
   → falls through to default → `EmitNative.global(lref(0))` → error

The IO constructor never reaches the empty-stack check. It hits RENDER_DOC's
case continuation first and fails.

### Why the Continuation Stack Insight Matters

The coordinator's design assumed: "RENDER_DOC's continuation survives the
yield. After IO completes, the machine resumes naturally into RENDER_DOC."

This would work **if** the IO constructor could yield despite continuations
on the stack. But the current design specifically uses an empty stack as the
yield signal. Changing this would require either:

1. **Modifying the yield mechanism**: Check for IO constructors *before*
   checking continuations in `return_con()`. This would break the semantics
   of `case` — an IO constructor inside a `case` branch should be matched
   by the case, not yielded.

2. **Adding an IO-transparent wrapper**: A special continuation type that
   passes IO constructors through to yield while catching non-IO constructors
   for rendering. This is a significant VM change.

3. **Compile-time detection**: If the compiler knows the expression will
   produce IO, compile differently. But IO-ness is a runtime property in
   eucalypt (programs can conditionally produce IO or pure values).

## 4. World Injection Complication

Even setting aside the continuation stack issue, world injection presents
another challenge. The current flow:

1. Compile headless → run → check if result is a function (arity > 0)
2. If function: inject world token, re-run → IO constructor yields
3. Run io-run loop

With RENDER_DOC always present:
- RENDER_DOC wraps the expression and tries to render it
- If the expression is an IO function (PAP waiting for world), RENDER_DOC
  would try to render a lambda → `Saturated` check returns false → suppressed
- No output would be produced; the driver wouldn't know it needs to inject world

This is actually handled correctly by the `Saturated` check in `Render.wrapper()`
which suppresses rendering of unsaturated closures. But the driver would then
need a different signal to detect "this was an IO function, try world injection".

## 5. Alternative Approaches

### 5A. Remove the Rendered-Block Fixup (status quo improvement)

Keep the current two-phase pipeline but make the demand analysis smarter:
- Track which bindings are transitively reachable from the output position
- Only force Multi on truly-rendered bindings
- This is complex analysis but doesn't change the evaluation architecture

### 5B. Mark Rendered Bindings as Updated After First Phase

After the headless evaluation, before the render phase, walk the output value
and mark all thunks as "already evaluated" (they have been — they're in WHNF
already). This would prevent the render traversal from entering them as if
they were fresh thunks.

**Problem**: Thunks that were evaluated during headless eval have already been
updated (BlackHole → value). The re-evaluation by RENDER_DOC enters the
*updated* value, not a thunk. The issue is that skip_update() prevents
memoisation, not that RENDER_DOC re-enters thunks.

### 5C. Two-Phase with Shared Memoisation (current approach)

The current approach (PR #878 + #880) is actually correct and performant:
- Phase 1 (headless eval): All thunks are evaluated and updated (memoised)
- Phase 2 (RENDER_DOC render): Re-entering a binding that was updated in
  phase 1 is a cheap no-op (returns the memoised value)
- The only cost of Thunk vs Value is the Update continuation push/pop
- AtMostOnce works for non-rendered scopes (generalised lookups, foldl bodies)

The 40% allocation reduction on AoC day01 from PR #880 shows this is already
effective for the cases that matter (compute-heavy non-rendered scopes).

### 5D. Lazy RENDER_DOC Injection (Alternative Single-Phase)

Instead of wrapping at compile time, inject RENDER_DOC as the first
continuation on the stack before running. The machine runs the user expression;
when it terminates normally (stack empty), instead of truly terminating, pop
the injected RENDER_DOC continuation which then renders.

This would require a new continuation type and changes to the termination logic.
For IO programs, the IO constructor would still yield before the RENDER_DOC
continuation is reached (since IO yield fires on empty stack — and with only
the RENDER_DOC continuation, the stack isn't empty).

This has the same fundamental issue as the main proposal.

## 6. RENDER_DOC and Thunk Re-Entry

To clarify why the double evaluation doesn't cause correctness issues:

After headless eval, all demanded thunks have been updated (memoised).
When RENDER_DOC traverses the output:
- **With Thunk (Multi/Unknown)**: The thunk's update frame has already been
  popped; the value is memoised. Re-entry returns the cached value immediately.
  Cost: one stack push/pop for the Update continuation (negligible).
- **With Value (AtMostOnce via skip_update)**: The binding is re-evaluated
  from scratch. For pure bindings this is semantically equivalent but wastes
  work. For bindings with side effects (there are none — eucalypt is pure),
  it could be incorrect.

The rendered-block fixup ensures all rendered bindings use Thunk, so the
render phase is essentially free (just pointer chasing through memoised values).

## 7. Recommendation

**Do not pursue render unification at this time.** The architectural blocker
(IO yield vs continuation stack) requires significant VM changes that are
disproportionate to the benefit.

The current approach (PR #878 + #880) is:
- **Correct**: Rendered bindings use Thunk (memoised); re-entry is free
- **Effective**: AtMostOnce works for non-rendered scopes (40% alloc reduction)
- **Simple**: ~15 lines of fixup code in demand analysis
- **Sound**: The fixup is conservative; it never produces incorrect behaviour

The remaining performance opportunity (skip update frame push/pop for rendered
bindings) is marginal — it saves one continuation frame per binding per render
traversal, which is negligible compared to the computation cost.

## 8. Acceptance Criteria (if proceeding despite blocker)

If the decision is to proceed anyway, these would need to be met:

1. IO programs (`io.shell`, `io.exec`, `io.bind`) must work unchanged
2. All 451 harness tests pass
3. `cargo clippy --all-targets -- -D warnings` clean
4. 010_prelude allocations ≤ 119K
5. No regression on AoC examples
6. IO error tests (`tests/harness/errors/`) pass
7. The rendered-block fixup (PR #880) can be removed
8. `echo '{ io.shell("echo hello") }' | timeout 10 ./target/release/eu --allow-io -` works

## 9. Risks

- **IO yield mechanism**: Any change to the yield-on-empty-stack invariant
  affects the entire IO monad system
- **World injection detection**: With RENDER_DOC present, detecting IO
  functions becomes harder
- **RENDER_DOC encountering IO constructors**: Would need new case branches
  or a transparent-to-IO wrapper
- **Test coverage**: IO + render interactions are under-tested
