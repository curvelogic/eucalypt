# SV2: as-spec / to-spec

- **Bead:** eu-amjn
- **Pillar:** SV — The type-value surface
- **Release:** 0.11
- **Depends on:** SV1 (eu-3a9w)
- **Date:** 2026-06-25

---

## 1. Problem

SV1 gives `s"…"` type-data value semantics and a `to-data` projection
to `t-*` tagged lists. But there is no bridge from type-data to
runtime validation. The prelude's `match?` is a structural predicate
that already checks values against patterns — blocks, lists,
predicates, literals — but there is no way to derive a `match?`
pattern from a type annotation.

The gap: a user can write `s"{ name: String, age: Number }"` but
cannot use it to validate an incoming block without manually
constructing the equivalent `{ name: string?, age: number? }` pattern.

## 2. Design

### 2.1 `to-spec` — type-data to match? pattern

A prelude function that takes type-data (a `BoxedTypeData` value)
and produces a `match?`-compatible pattern:

1. Call `to-data` to get the `t-*` tagged-list projection.
2. Recursively walk the tagged list and build a pattern.

**Mapping from `t-*` tags to patterns:**

| Type form | Tagged list | Pattern |
|---|---|---|
| `Number` | `[:t-prim :number]` | `number?` |
| `String` | `[:t-prim :string]` | `string?` |
| `Symbol` | `[:t-prim :symbol]` | `symbol?` |
| `Bool` | `[:t-prim :bool]` | `bool?` |
| `Null` | `[:t-prim :null]` | `null?` (or `(_ = null)`) |
| `DateTime` | `[:t-prim :datetime]` | `datetime?` |
| `Any` | `[:t-prim :any]` | `any?` |
| `List(T)` | `[:t-list T']` | A predicate: `list? ∧ all(T-spec)` |
| `Record({…})` | `[:t-record {k: T'}]` | `{k: T-spec, …}` — a block pattern for `match?` |
| `Union(A, B)` | `[:t-union A' B']` | A predicate: `_(A-spec match?(•) \|\| B-spec match?(•))` |
| `Fun(A, B)` | `[:t-fn A' B']` | `__SATURATED` — can only check it is a function |
| `Partial(T)` | `[:t-partial T']` | `_(null? \|\| T-spec match?(•))` |
| `Tuple(…)` | `[:t-tuple T1' T2' …]` | A list pattern with positional specs |
| `Forall(…)` | `[:t-forall …]` | Erase the quantifier, spec the body |
| `Var(…)` | `[:t-var …]` | `any?` — type variables are unconstrained at runtime |

This is pure prelude code — no new intrinsics needed. All the
type-checking predicates (`number?`, `string?`, `bool?`, `block?`,
`list?`, `symbol?`) already exist in the prelude
(`lib/prelude.eu:378–400`). `match?` already handles blocks, lists,
predicates, and literals (`lib/prelude.eu:544–562`).

### 2.2 `as-spec` — convenience wrapper

```
as-spec: to-spec
```

A synonym. The roadmap uses both names; `as-spec` reads naturally
in pipeline position: `s"{ name: String }" as-spec`. If the names
should diverge semantically later (e.g. `to-spec` returns a
reusable spec object vs `as-spec` applies inline), the alias can
be split. For 0.11 they are identical.

### 2.3 Usage pattern

```eucalypt
schema: s"{ name: String, age: Number }" as-spec

valid:   {name: "Alice", age: 30} match?(schema)    # true
invalid: {name: "Alice", age: "thirty"} match?(schema)  # false
```

The spec is an ordinary eucalypt value — a block/predicate structure
that `match?` consumes. No special runtime machinery.

### 2.4 Missing predicates

Some type forms need predicates that don't yet exist in the prelude:

- **`null?`** — check if a value is null. May need adding if not
  present (check `__ISNULL` or equivalent).
- **`datetime?`** — check if a value is a zoned datetime. May need
  a new intrinsic `__ISDATETIME` or equivalent.

These are small additions if needed.

## 3. Scope

### In scope

- `to-spec` prelude function: `t-*` tagged list → `match?` pattern.
- `as-spec` alias.
- Coverage of all `t-*` type forms from SV1.
- Any missing type-checking predicates (`null?`, `datetime?`).

### Out of scope

- Structural contracts with located blame (SV3 / W16) — that builds
  on specs but adds error reporting and ingress enforcement.
- Schema export / JSON Schema (SV4).
- Default-fill from specs.
- Generation from specs (property-test seeds).

## 4. Success Criteria

1. **Primitive specs:**
   `s"Number" as-spec` produces a spec that `42 match?` returns
   true and `"hello" match?` returns false.
2. **Record specs:**
   `s"{ name: String, age: Number }" as-spec` validates
   `{name: "Alice", age: 30}` (true) and rejects
   `{name: "Alice", age: "thirty"}` (false).
3. **List specs:**
   `s"[Number]" as-spec` validates `[1, 2, 3]` (true) and rejects
   `[1, "two", 3]` (false).
4. **Union specs:**
   `s"String | Number" as-spec` validates both `"hello"` and `42`.
5. **Partial specs:**
   `s"String?" as-spec` validates `"hello"`, `null`, and rejects
   `42`.
6. **Nested specs:**
   `s"{ users: [{ name: String }] }" as-spec` validates nested
   structures.
7. **Full harness green.**

## 5. Testing

- Targeted harness tests for each type form → spec → `match?`.
- Round-trip: verify that `to-spec` produces specs that correctly
  distinguish valid and invalid values for each type form.
- Edge cases: empty records, empty lists, `Any`, `Never`,
  type variables, function types.

## 6. Risks

- **Low:** this is pure prelude work consuming SV1's `to-data`
  output. No compiler or runtime changes beyond possibly adding
  one or two missing type-test predicates.
- **Low:** `match?` already handles all the pattern forms needed.
  The only complexity is the recursive walk of the `t-*` tagged
  list, which is straightforward functional code.
- **Medium:** union types produce predicate-based specs (not
  block/list patterns), so `match?` dispatches to the predicate
  path. This works today but should be tested carefully for
  nested unions.
