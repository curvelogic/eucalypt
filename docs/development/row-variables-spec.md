# Named Row Variables in Type Annotations

**Status**: Spec  
**Bead**: eu-z9zz.5  
**Date**: 2026-04-27

## 1. Overview

Add named row variables to the type system so that type annotations
can express "a record with at least these fields, plus unknown extra
fields that are preserved through a transformation".

Syntax: `{k: T, ..r}` — the existing `..` open-record marker gets an
optional name.  Bare `..` remains valid as an anonymous open record.

## 2. Syntax

| Annotation | Meaning |
|------------|---------|
| `{x: number}` | Closed record — exactly field `x` |
| `{x: number, ..}` | Open record — at least field `x` |
| `{x: number, ..r}` | Named row — at least field `x`, extras captured by `r` |

Row variable names are lowercase identifiers (like type variables
`a`, `b`).  They share the same namespace as type variables — `a` in
`{x: a, ..r}` is a type variable and `r` is a row variable.
Syntactically they are distinguished by position: after `..` is
always a row variable; elsewhere is a type variable.

## 3. Type Representation

Add a `Row` variant to hold the row variable in the `Record` type:

```rust
Type::Record {
    fields: BTreeMap<String, Type>,
    open: bool,
    row: Option<TypeVarId>,  // NEW — named row variable
}
```

When `row` is `Some(id)`, `open` is implicitly `true`.  When `row`
is `None` and `open` is `true`, the record is open but the extra
fields are anonymous (not tracked through unification).

## 4. Parser Changes

Extend the record type parser to recognise `..name` after the
last field:

```
record_type := '{' field_list (',' '..' IDENT?)? '}'
```

When `..` is followed by a lowercase identifier, store it as the
row variable.  When `..` appears alone, the record is open with
no named row.

## 5. Unification

### 5.1 Record vs Record

Current behaviour: common fields are unified; extra fields in
open records are ignored.

New behaviour with row variables:

- **Both have row vars** (`{x: A, ..r}` vs `{y: B, ..s}`):
  Unify common fields.  Bind `r` to `{y: B, ..s}` minus the
  fields already in the LHS, and vice versa.  In practice, for
  the gradual system, bind each row var to the other side's
  extra fields.

- **One has row var, other is concrete** (`{x: A, ..r}` vs
  `{x: B, y: C}`): Unify `x: A` with `x: B`.  Bind `r` to
  `{y: C}` (greedy absorption — all unmatched fields are
  captured by the row variable).

- **One has row var, other is open** (`{x: A, ..r}` vs
  `{x: B, ..}`): Unify `x: A` with `x: B`.  `r` unifies
  with `any` (gradual boundary — we don't know the extra
  fields).

- **Neither has row var**: Current behaviour unchanged.

### 5.2 Substitution

Row variables participate in the substitution map alongside
type variables.  When a row variable `r` is bound to
`{y: string}`, applying the substitution to `{x: number, ..r}`
produces `{x: number, y: string}` (closed, or open if `r` was
bound to an open record).

### 5.3 Freshening

Row variables in polymorphic schemes are freshened alongside
type variables.  Each use of a function with `..r` in its type
gets a fresh row variable.

## 6. Catenation Typing

When the checker sees a block catenation `a b` where both sides
have known record types, produce the merged record type:

- Fields from both sides are included
- RHS wins on conflicts (matching runtime semantics)
- No warning on field override (catenation is intentional)
- If either side has a row variable, the result is open

Example: `{x: number, ..r}` catenated with `{y: string}` produces
`{x: number, y: string, ..r}`.

## 7. Gradual Boundary

Row variables follow the gradual typing philosophy:

- `{x: number, ..r}` unifying with `any` succeeds silently
- `{x: number, ..r}` unifying with `{..}` succeeds — `r` gets `any`
- Only concrete mismatches produce warnings (wrong field type,
  missing required field in closed record)

## 8. What This Enables

### State monad threading

```
state-ret: a → {..r} → {value: a, state: {..r}}
state-get: {..r} → {value: {..r}, state: {..r}}
state-bind: ({..r} → {value: a, state: {..r}})
          → (a → {..r} → {value: b, state: {..r}})
          → {..r} → {value: b, state: {..r}}
```

### Structure-preserving user functions

```
greet: {name: string, ..r} → {name: string, greeting: string, ..r}
inc-x: {x: number, ..r} → {x: number, ..r}
```

### Block manipulation functions (medium benefit)

```
alter-value: symbol → any → {..r} → {..r}
update-value: symbol → (any → any) → {..r} → {..r}
set-value: symbol → any → {..r} → {..r}
```

These express "structure is preserved" but can't track which
specific field is altered (the key is a runtime symbol).

### Functions that stay `{..}` (low benefit)

`merge`, `map-values`, `map-keys` — these combine or transform
all fields and can't express the result in terms of input row
variables without row union constraints (PureScript-style `Union`),
which we defer.

## 9. Acceptance Criteria

### Must pass (no warnings)

1. `{name: string, ..r} → {name: string, greeting: string, ..r}` —
   calling with `{name: "Alice", age: 30}` and accessing `.age` on
   the result produces no warning (row variable preserves `age`)

2. `{x: number, ..r} → {x: number, ..r}` — calling with
   `{x: 1, y: 2}` and accessing `.y` on the result: no warning

3. `a → {..r} → {value: a, state: {..r}}` — state-ret pattern:
   state fields are preserved in the result

4. `{..r} → {value: {..r}, state: {..r}}` — state-get pattern:
   same row var in multiple positions

5. `{x: number, ..r} → number` — calling with `{x: 42}` (empty
   row): no warning

6. `{x: number, ..r} → {x: number, ..r}` — calling with result
   of untyped function: no warning (gradual boundary)

7. Nested row vars: `{inner: {x: number, ..r}, ..s}` parses and
   unifies correctly

### Must warn

8. `{name: string, ..r} → string` — calling with `{age: 30}`:
   warns missing field `name`

9. `{x: number, ..r} → number` — calling with `{x: "hello"}`:
   warns wrong type for field `x`

10. `{x: number} → number` (closed, no row var) — calling with
    `{x: 1, y: 2}`: warns extra field `y` in closed record

### Parser

11. `{x: number, ..r}` parses to Record with row var `r`
12. `{x: number, ..}` parses to open Record with no row var
    (backward compatible)
13. `{x: number}` parses to closed Record (backward compatible)
14. `{..r}` parses to open Record with only a row var (no fields)
15. Row var name freshened independently per use

### Prelude check

16. `eu check lib/prelude.eu` — zero warnings after updating
    annotations to use row variables
17. All existing harness tests pass

## 10. Files Changed

| File | Change |
|------|--------|
| `src/core/typecheck/types.rs` | Add `row: Option<TypeVarId>` to `Record` variant |
| `src/core/typecheck/parse.rs` | Parse `..name` in record types |
| `src/core/typecheck/unify.rs` | Row variable unification and greedy absorption |
| `src/core/typecheck/check.rs` | Substitution application for row vars; catenation typing |
| `src/core/typecheck/scheme.rs` | Include row vars in freshening |
| `lib/prelude.eu` | Update `alter-value`, `update-value`, `set-value` annotations |
| `lib/state.eu` | Add row-typed annotations to state monad functions |
| `tests/harness/typecheck/` | New test files for row variable scenarios |

## 11. Out of Scope

- Row union constraints (`Union r1 r2 r3`) — needed for typing
  `merge` precisely; deferred to future work
- Mapped types (same keys, transformed values) — needed for
  `map-values`; deferred
- Higher-kinded type variables — needed for `monad()`; separate
  bead (eu-crbc)
