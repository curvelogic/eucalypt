# Declaration Trace Metadata

**Bead:** eu-yhk0.4
**Target:** 0.8.0
**Status:** Design

## Problem

Debugging eucalypt programs is difficult. The existing `dbg{}` and `▶`
operator require manual insertion around expressions and don't
naturally show what arguments a declaration receives. When a function
produces unexpected results, there is no quick way to confirm what
flowed in — you must manually wrap each argument in debug calls, which
is tedious and error-prone.

## Solution

A `trace` metadata key on declarations that instruments them to print
entry (and optionally exit) information to stderr. The user annotates
a declaration, re-runs, and sees what arrives.

## Metadata Forms

| Annotation                    | Entry | Exit | Args forced? |
|-------------------------------|-------|------|--------------|
| `` ` :trace ``                | yes   | no   | no           |
| `` ` { trace: :lazy } ``     | yes   | no   | no           |
| `` ` { trace: :strict } ``   | yes   | no   | yes          |
| `` ` { trace: :exit } ``     | yes   | yes  | no           |
| `` ` { trace: :strict-exit } `` | yes | yes  | yes          |

`` ` :trace `` and `` ` { trace: :lazy } `` are equivalent — the bare
symbol form is shorthand for the block form.

**Lazy mode** (`:trace`/`:lazy`, `:exit`): arguments that have already been
evaluated are rendered; unevaluated thunks display as `<thunk>`.
No forcing occurs — evaluation semantics are unchanged.

**Strict mode** (`:strict`, `:strict-exit`): arguments are forced to
WHNF before rendering. This changes evaluation order; the user opts in
deliberately.

**Exit tracing** (`:exit`, `:strict-exit`): the result is rendered
after the body evaluates. In lazy exit mode the result is rendered at
whatever depth it has reached; in strict exit mode it is forced to
WHNF first. Exit tracing changes evaluation semantics — it is opt-in.

## Output Format

```
→ f(x: <thunk>, y: <thunk>)
→ f(x: 42, y: [1, 2, 3])
← f: <thunk>
← f: {result: "hello"}
```

- `→` for entry, `←` for exit.
- Flat output, no nesting or indentation.
- Output goes to stderr.
- Rendering reuses the existing `__DBG_REPR` infrastructure.

## Implementation

### 1. Metadata extraction (desugar phase)

**File:** `src/core/metadata.rs`

Add `trace` as a recognised declaration metadata key. Define:

```rust
pub enum TraceSpec {
    Lazy,       // ` :trace
    Strict,     // ` { trace: :strict }
    Exit,       // ` { trace: :exit }
    StrictExit, // ` { trace: :strict-exit }
}
```

Normalisation rules in `normalise_metadata()`:
- Add `:trace` as a named case alongside `:suppress`, `:internal`,
  and `:target` — before the wildcard `_ if decl_name.is_some()`
  fallback that converts unrecognised symbols to targets. Normalise
  to `{ trace: :lazy }`.
- `{ trace: :lazy }`, `{ trace: :strict }`, `{ trace: :exit }`,
  `{ trace: :strict-exit }` pass through as-is.

Read the `trace` key via `ReadMetadata` / `Extract` traits, producing
`Option<TraceSpec>`. Strip the key after extraction (same pattern as
fixity, target, import).

### 2. Body wrapping (desugar phase)

**File:** `src/core/desugar/rowan_ast.rs`

When a declaration has `TraceSpec`, the desugarer wraps the body
during declaration processing.

For entry-only (`:trace` / `:strict`), conceptually:

```
f(x, y): body
  becomes
f(x, y): __TRACE_ENTRY("f", ["x", x, "y", y], false) seq body
```

For entry+exit (`:exit` / `:strict-exit`):

```
f(x, y): body
  becomes
f(x, y): __TRACE_EXIT("f", (__TRACE_ENTRY("f", ["x", x, "y", y], false) seq body), false)
```

The boolean flag controls strict vs lazy mode.

The wrapping is done at the core expression level — it emits
`Expr::App` calls to the new intrinsics. `seq` is the existing
sequencing primitive that forces its first argument then returns the
second.

**Argument names** are available as string literals from the
declaration's parameter list at desugar time.

### 3. New intrinsics (STG runtime)

**File:** `src/eval/stg/debug.rs`

Two new intrinsics registered in `src/eval/stg/intrinsics.rs`:

#### `__TRACE_ENTRY(name, args_array, strict)`

- `name`: string — the declaration name.
- `args_array`: a list alternating `[name_str, value, name_str, value, ...]`.
- `strict`: bool — if true, force each value to WHNF before rendering;
  if false, peek (render if already evaluated, else `<thunk>`).

Behaviour:
1. For each (name, value) pair, render the value using
   `render_debug_repr()` (strict) or a new `peek_debug_repr()` (lazy).
2. Format: `→ name(arg1: rendered1, arg2: rendered2)`.
3. Print to stderr.
4. Return unit (null / empty value for `seq` to discard).

#### `__TRACE_EXIT(name, value, strict)`

- `name`: string — the declaration name.
- `value`: the body result.
- `strict`: bool — if true, force to WHNF before rendering.

Behaviour:
1. If strict, force `value` to WHNF.
2. Render using `render_debug_repr()` or `peek_debug_repr()`.
3. Format: `← name: rendered`.
4. Print to stderr.
5. Return `value` (pass through transparently).

#### `peek_debug_repr()` (new helper)

A variant of `render_debug_repr()` that does not force evaluation.
It inspects the heap cell:
- If it is a Native value, render it.
- If it is a data constructor (list, block), render the outer structure
  tag without forcing contents (e.g. `[...]`, `{...}`).
- If it is a closure/thunk (unevaluated), render `<thunk>`.

This requires reading the heap cell's tag without entering evaluation,
which is possible via the existing `HeapSyn` discriminant inspection.

### 4. Metadata normalisation edge case

The existing `normalise_metadata()` converts bare symbols on
declarations into `{ target: <symbol> }`. The `:trace` symbol must be
intercepted before this fallback. Add a check:

```rust
// In normalise_metadata(), add alongside "suppress" | "internal" | "target":
"trace" => core::block(
    *smid,
    [("trace".to_string(), core::sym(*smid, "lazy"))].iter().cloned(),
),
```

This follows the same pattern as `:suppress` → `{ export: :suppress }`
and `:target` → `{ target: <name> }`.

## Scope Exclusions

These are explicitly out of scope for this iteration:

- No call-depth tracking or nested indentation.
- No CLI flag to activate tracing without source edits.
- No per-argument strict/lazy control.
- No STG structure dumping.
- No integration with `eu dump` commands.
- No tooling/editor integration.

All could be added later without breaking this design.

## Acceptance Criteria

1. `` ` :trace f(x, y): x + y `` — calling `f(1, 2)` prints
   `→ f(x: <thunk>, y: <thunk>)` to stderr. The result is `3`
   (trace does not affect output).
2. `` ` { trace: :lazy } `` behaves identically to `` ` :trace ``.
3. `` ` { trace: :strict } f(x, y): x + y `` — calling `f(1, 2)`
   prints `→ f(x: 1, y: 2)` to stderr (args forced and rendered).
4. `` ` { trace: :exit } f(x, y): x + y `` — calling `f(1, 2)`
   prints both `→ f(x: <thunk>, y: <thunk>)` and `← f: <thunk>`
   (or the evaluated value if it has been forced by the caller).
5. `` ` { trace: :strict-exit } f(x, y): x + y `` — calling
   `f(1, 2)` prints `→ f(x: 1, y: 2)` and `← f: 3`.
6. Lazy mode does not force thunks: a traced function with a
   side-effecting argument that is never used by the body must NOT
   trigger the side effect.
7. Property declarations (no args): `` ` :trace x: 42 `` prints
   `→ x()` (entry with no arguments).
8. Multiple traced declarations print in evaluation order.
9. `:trace` in metadata normalisation does not become
   `{ target: :trace }` — it becomes `{ trace: :lazy }`.
10. The existing harness passes unchanged (no regression).

**Test file:** `tests/harness/NNN_trace.eu` with `.expect` sidecar
using `stderr:` regex patterns to verify trace output.
