# Monadic Block Bound Variable Type Hinting (LSP)

**Status**: Spec  
**Bead**: eu-z9zz.10  
**Date**: 2026-05-03

## 1. Overview

In `{ :for x: [1,2,3] }.(x * 2)`, the bound variable `x` should
have type `number` — the unwrapped element type from the monad's
wrapper.  The type checker path gets this for free once monad
namespace record types (eu-meir) and record type propagation
(eu-z9zz.6) land, because the desugared `for.bind([1,2,3], λx. ...)`
naturally propagates `a = number` through unification.

This bead covers the **LSP path**: using the `monad:` metadata
directly to infer bound variable types during editing, when the
expression may be incomplete or invalid and desugaring may not
succeed.

## 2. The Metadata Path

The LSP already knows:
- The monad tag (`:for`, `:io`, etc.) from block metadata
- The wrapper type (`[a]`, `IO(a)`, etc.) from the symbol table's
  `monad_type` field
- Each binding declaration and its value expression

The algorithm:

1. Detect monadic block (already done for inlay hints)
2. Look up the wrapper type string (already available)
3. For each binding `name: value_expr`:
   a. Parse the wrapper type (e.g. `[a]`) to get a `Type`
   b. Synthesise the value expression's type (e.g. `[number]`)
   c. Unify the wrapper type against the value type
   d. If unification succeeds, extract the binding for `a`
   e. The bound variable's type is the solved `a`
4. Make this type available for:
   - **Hover** on the bound variable name → shows inferred type
   - **Inlay hints** on the binding → shows `: number` not `: [a]`
   - **Completion** inside the block body → field suggestions if
     the bound variable has a record type
   - **Type checking** inside the body → `x + "hello"` warns

## 3. Fallback Behaviour

- If the value expression can't be synthesised (incomplete code),
  the bound variable type is `any` — no hint shown
- If unification fails (value doesn't match wrapper), the bound
  variable type is `any` — the existing `__type_hint` warning
  already covers this case
- If the wrapper type has no type variable (e.g. `monad: true`),
  the bound variable type is `any`
- For monads like `random` where the wrapper is
  `stream → {{value: a, rest: stream}}`, extracting `a` requires
  matching the return type structure — defer to `any` if the
  wrapper is too complex to extract from

## 4. Implementation

### 4.1 Type extraction helper

Add a function that, given a wrapper type and a concrete value
type, solves for the element type variable:

```rust
/// Given a monad wrapper type (e.g. `[a]`) and a concrete value
/// type (e.g. `[number]`), unify them and return the solved
/// element type (e.g. `number`).
fn extract_element_type(
    wrapper_type: &str,
    value_type: &Type,
) -> Option<Type>
```

Parse the wrapper string, freshen its variables, unify with the
value type, and look up the first type variable in the resulting
substitution.

### 4.2 LSP hover

In the hover provider, when the cursor is on a bound variable
name inside a monadic block:

1. Identify the enclosing monadic block and its tag
2. Find the binding declaration for this variable
3. Extract the element type using the helper
4. Show the inferred type in hover

### 4.3 LSP inlay hints

Update `collect_monadic_binding_hints` to show the solved element
type instead of the raw wrapper type.  Currently it shows
`: [a]` on `x:` — it should show `: number` when the value is
`[1,2,3]`.

This requires synthesising the value expression's type, which
means the inlay hint provider needs access to a type checker
instance or a pre-computed type environment.

### 4.4 Scope for body type checking

When the LSP runs type checking on a monadic block body, push
the bound variables with their inferred types into the scope.
This enables warnings like `x + "hello"` when `x: number`.

## 5. Acceptance Criteria

### LSP features

1. Hover on `x` in `{ :for x: [1,2,3] }.(x)` shows `number`
2. Hover on `cmd` in `{ :io cmd: io.shell("ls") }.(cmd)` shows
   `{{..}}` (the IO result type)
3. Inlay hint on `x:` in `{ :for x: [1,2,3] }` shows `: number`
   (not `: [a]`)
4. In `{ :for x: [1,2,3] }.(x ++ "!")` the LSP reports a type
   warning (number vs string in ++)
5. In `{ :let x: 42 }.(x)` — `x` has type `any` (let is untyped),
   no inlay hint for element type
6. When value expression is incomplete (mid-edit), no crash,
   falls back to `any`

### Type checker path (covered by eu-meir + eu-z9zz.6)

7. `eu check --strict` on `{ :for x: [1,2,3] }.(x ++ "!")` warns
   (once monad namespace record types and record propagation land)
8. `eu check --strict` on `{ :for x: [1,2,3] }.(x * 2)` — no
   warning

### Integration

9. `eu check lib/prelude.eu` — zero warnings
10. All existing harness tests pass

## 6. Files Changed

| File | Change |
|------|--------|
| `src/core/typecheck/check.rs` or new module | `extract_element_type` helper |
| `src/driver/lsp/hover.rs` | Hover on bound variables in monadic blocks |
| `src/driver/lsp/inlay_hints.rs` | Show solved element type instead of wrapper |
| `src/driver/lsp/symbol_table.rs` | Possibly cache element types per binding |

## 7. Dependencies

- eu-ggr9 (typed monad metadata) — already landed, provides
  `monad_type` in symbol table
- For acceptance criteria 7-8: eu-meir + eu-z9zz.6

## 8. Out of Scope

- Complex wrapper types (random, state) — element type extraction
  deferred to `any` for these; can be improved later
- Completion suggestions based on the bound variable's record type
  (deferred to a future bead)
