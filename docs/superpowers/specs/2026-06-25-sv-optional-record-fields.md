# SV: Optional (Presence-Annotated) Record Fields

- **Bead:** eu-luyx
- **Pillar:** SV — The type-value surface (type-vocabulary enrichment)
- **Release:** 0.11
- **Date:** 2026-06-25

---

## 1. Problem

Record types in the type system are required-keys-only. The `Record`
type (`src/core/typecheck/types.rs:242`) has
`fields: BTreeMap<String, Type>` — every field is required. A
missing required field is a type warning.

Real config — and downstream work like eucalypt-grove — needs fields
that *may be absent*: a key that is legitimately not present in some
instances. Today, the only workaround is `T?` / `Partial(T)`, but
that is a *value-side* annotation meaning "the key is present but
its value may be null." The distinction matters:

- `name?: String` — the key `name` may be absent entirely
- `name: String?` — the key `name` is present, its value may be
  null

These are different shapes with different subtyping and different
validation semantics. Only the first is missing.

## 2. Design

### 2.1 Type-DSL syntax

Add `?` suffix on the key name in record types:

```
s"{ name: String, age?: Number }"
```

`name` is required, `age` is optional. The `?` attaches to the
key, not the type — `age?: Number` means "the key `age` may be
absent; if present, its value is `Number`." Compare with
`age: Number?` meaning "the key `age` is required; its value is
`Number | Null`."

### 2.2 Type representation

Enrich the `Record` type in `src/core/typecheck/types.rs`:

**Current:**
```rust
Record {
    fields: BTreeMap<String, Type>,
    open: bool,
    rows: Vec<Type>,
}
```

**After:**
```rust
Record {
    fields: BTreeMap<String, FieldPresence>,
    open: bool,
    rows: Vec<Type>,
}

enum FieldPresence {
    Required(Type),
    Optional(Type),
}
```

Or equivalently, a `BTreeMap<String, (Type, bool)>` where the bool
indicates optionality. The `FieldPresence` enum is more readable.

### 2.3 Type-DSL parser

In `src/core/typecheck/parse.rs`, when parsing a record field, check
for a trailing `?` on the key name before the `:`. If present, mark
the field as optional.

### 2.4 Subtyping

Presence subtyping in `src/core/typecheck/subtype.rs`:

- A record with a **required** field `k: T` is a subtype of a
  record with an **optional** field `k?: T`. (Having the key is
  more specific than maybe-having it.)
- A record with an **optional** field `k?: T` is NOT a subtype of
  a record with a **required** field `k: T`. (The key might be
  absent.)
- Two records with the same field, both optional: standard
  covariant subtyping on the field type.
- A record **missing** a key is a subtype of a record where that
  key is optional (the absence is one valid state of "optional").

### 2.5 Type checking

In `src/core/typecheck/check.rs`, when checking a record value
against a record type:

- **Required field missing:** warning (as today).
- **Optional field missing:** no warning (new — this is the point).
- **Optional field present:** check the value against the field's
  type (same as required).
- **Extra field present (open record):** no warning (as today for
  open records).

### 2.6 `t-*` tagged-list projection

Extend the `t-*` vocabulary (SV1) to represent optional fields:

```
s"{ name: String, age?: Number }" to-data
→ [:t-record {name: [:t-prim :string], age: [:t-optional [:t-prim :number]]}]
```

The `[:t-optional T]` wrapper distinguishes optional from required
in the tagged-list form. Alternatively, the record projection could
use a different structure:

```
→ [:t-record {name: [:t-field :required [:t-prim :string]],
              age:  [:t-field :optional [:t-prim :number]]}]
```

The `[:t-field presence type]` form is more explicit and extensible
(could carry defaults later). Recommend this form.

### 2.7 `to-spec` / `match?` integration (SV2)

When `to-spec` encounters an optional field in a record spec:

- The generated `match?` pattern omits the optional key entirely
  (open matching — `match?` already ignores extra keys, and a
  missing optional key should not cause failure).
- If the optional key IS present, its value must match the type
  spec.

Implementation: `me?` in `match?` (`lib/prelude.eu:556`) currently
checks `t has(k) ∧ mv?(v, t lookup(k))`. For optional fields,
the pattern should be: `¬(t has(k)) ∨ mv?(v, t lookup(k))` — the
key is absent OR the value matches.

### 2.8 Display / rendering

In the type-DSL display (`types.rs` `Display` impl), render optional
fields with the `?` suffix: `{ name: String, age?: Number }`.

## 3. Scope

### In scope

- `?` suffix on record field keys in the type-DSL syntax.
- `FieldPresence` (or equivalent) in the `Record` type.
- Type-DSL parser support.
- Subtyping rules for optional fields.
- Type-checker handling (optional absent = no warning).
- `t-*` projection with `[:t-field presence type]`.
- `to-spec` handling for optional fields in `match?` patterns.
- Display rendering.

### Out of scope

- Presence *inference* — the checker does not infer which fields
  are optional. That is a follow-on item (roadmap: "presence
  inference follows").
- Default-fill — a spec supplying values for absent optional fields.
  That is part of the SV aspiration but not 0.11.
- Schema import — CRDs and JSON Schema need optional fields but
  SV4 (schema interop) is 0.15+.

## 4. Success Criteria

1. **Parsing:** `s"{ name: String, age?: Number }"` parses
   successfully.
2. **Subtyping:** `{ name: String, age: Number }` is a subtype of
   `{ name: String, age?: Number }`.
3. **Checking (present):** `{name: "Alice", age: 30}` against
   `{ name: String, age?: Number }` — no warning.
4. **Checking (absent):** `{name: "Alice"}` against
   `{ name: String, age?: Number }` — no warning.
5. **Checking (wrong type):** `{name: "Alice", age: "thirty"}`
   against `{ name: String, age?: Number }` — type warning on
   `age`.
6. **Checking (required missing):** `{age: 30}` against
   `{ name: String, age?: Number }` — warning on missing `name`.
7. **Display:** `{ name: String, age?: Number }` renders with the
   `?` suffix.
8. **`to-data`:** projection includes `[:t-field :optional ...]`
   for optional fields.
9. **`match?` via `to-spec`:** validates correctly with optional
   fields present, absent, and wrong-typed.
10. **Full harness green.**

## 5. Testing

- Type-DSL parser tests for optional field syntax.
- Subtyping tests for presence subtyping rules.
- Type-checker tests for all four cases (present/absent ×
  correct/wrong type).
- `to-data` round-trip tests for optional fields.
- `to-spec` + `match?` integration tests.
- Existing harness passes (no existing code uses `?` on keys).

## 6. Risks

- **Low:** the change is additive — no existing record types have
  optional fields, so no backward compatibility concern.
- **Medium:** the subtyping rules must be carefully implemented to
  avoid unsoundness. The key invariant: required ⊆ optional, never
  the reverse.
- **Low:** the `t-*` vocabulary extension (`[:t-field ...]`) is
  additive and does not break existing projections (existing fields
  without the wrapper can be treated as `[:t-field :required ...]`
  for backward compatibility).
