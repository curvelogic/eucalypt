# Record Type Propagation Through Block Application

**Status**: Spec  
**Bead**: eu-z9zz.6  
**Date**: 2026-05-02

## 1. Overview

Blocks in eucalypt are callable: applying a block to another block
merges them, with the function-position block's fields winning on
conflicts.  The type checker should understand this and propagate
record types through block application, rather than returning `any`.

This is not a special case for "catenation" — blocks being callable
is a fundamental property of eucalypt.  Catenation `a b` is simply
the RHS block being applied to the LHS, and juxtaposed call syntax
`{a}{b}` is the LHS applied to the RHS as argument.

## 2. Current Behaviour

When `synthesise_app` encounters a `Record` type in function
position, it doesn't match `Type::Function` so it falls through
to `Type::Any`.  All field type information is lost.

## 3. Proposed Behaviour

In `synthesise_app`, when the function type is `Record` and the
argument type is also `Record`, produce the merged record type:

- All fields from both sides are included in the result
- Function-position fields win on conflicts (matching runtime)
- If either side has a row variable, the result is open
- No warning on field override (intentional by design)

When the function type is `Record` and the argument type is:

- `Any` — return `Any` (gradual boundary, can't infer)
- A non-record concrete type (number, string, etc.) — return
  `Any` (gradual; blocks applied to non-blocks silently ignore
  the argument at runtime)

## 4. Implementation

### 4.1 synthesise_app change

Add a branch in `synthesise_app` (or `apply_one_with_subst`) that
handles `Record` in function position:

```rust
Type::Record { fields: func_fields, open: func_open, row: func_row } => {
    let arg_type = self.synthesise(arg);
    match arg_type {
        Type::Record { fields: arg_fields, open: arg_open, row: arg_row } => {
            // Merge: start with arg fields, overlay func fields
            let mut merged = arg_fields.clone();
            for (k, v) in &func_fields {
                merged.insert(k.clone(), v.clone());
            }
            let open = func_open || arg_open;
            let row = func_row.or(arg_row);
            Type::Record { fields: merged, open, row }
        }
        _ => Type::Any, // gradual boundary
    }
}
```

### 4.2 Row variable interaction

When both sides have row variables, the result should have a row
variable (the result is open with unknown extra fields).  The
precise semantics of merging two distinct row variables is deferred
to row union constraints — for now, pick one or generate a fresh
one.

When one side has a row variable and the other is closed, the
result has the row variable from the open side.

### 4.3 Substitution propagation

Row variable bindings from unification should be applied before
the merge, so that `{x: number, ..r}` where `r` is bound to
`{y: string}` expands to `{x: number, y: string}` before merging.

## 5. Interaction with Catenation

Catenation `a b` desugars to `App(b_block, [a_block])` — the RHS
is in function position.  So the RHS fields win on conflicts,
which matches the runtime semantics.

Juxtaposed call `{a}{b}` desugars to `App(a_block, [b_block])` —
the LHS is in function position.  So the LHS fields win.

The type checker handles both uniformly: function-position record
wins on conflicts.

## 6. Acceptance Criteria

### Must pass (no warnings)

1. `{x: 1}({y: 2}).y` — merged result has field `y`
2. `({a: 1} {b: 2}).b` — catenation merges fields
3. `{x: "new"}({x: "old"}).x` — function-position wins, type is
   `string`
4. Typed blocks `a: {x: 1}` and `b: {y: "hi"}` with annotations —
   `(a b).y` checker knows `y: string`
5. Row variable preserved: `add-z: {x: number, ..r} → {x: number,
   z: number, ..r}` — calling with `{x: 1, y: 2}` and accessing
   `.y` produces no warning
6. Block applied to untyped value — no warning (gradual)
7. Nested merge preserves deep field types

### Must warn

8. Access field absent from both sides (both closed) — warn
9. Wrong type for merged field propagates correctly

### Gradual boundary (no warning)

10. Block applied to non-block (e.g. number) — silent, returns `any`
11. Untyped block in either position — no warning

### Integration

12. `eu check lib/prelude.eu` — zero warnings
13. All existing harness tests pass

## 7. Files Changed

| File | Change |
|------|--------|
| `src/core/typecheck/check.rs` | Handle `Record` in function position in `synthesise_app` or `apply_one_with_subst` |
| `tests/harness/typecheck/` | New tests for block application typing |

## 8. Dependencies

- eu-z9zz.5 (row variables) — for row variable propagation through
  merges.  The basic record merge (without row variables) can be
  implemented independently.

## 9. Out of Scope

- Row union constraints (merging two distinct row variables into
  a combined row)
- Warning on block applied to non-block (currently gradual/silent)
- Override warnings for conflicting field types
