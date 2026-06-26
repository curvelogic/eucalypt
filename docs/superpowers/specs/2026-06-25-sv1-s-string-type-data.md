# SV1: s-string Type-Data Value Semantics

- **Bead:** eu-3a9w
- **Pillar:** SV — The type-value surface
- **Release:** 0.11
- **Date:** 2026-06-25

---

## 1. Problem

`s"…"` is lexed and compiled but has minimal semantics. It tokenises
as `S_STRING` (no interpolation, no escapes), desugars to
`Primitive::TypeData(String)`, and compiles to a `BoxedTypeData` data
constructor (tag 17) wrapping the raw string. But the string content
is never validated — a typo like `s"nubmer"` silently produces a
string value. There is no way to inspect the type structure at
runtime (`to-data`), and no way to construct type-data from
structured data (`from-data`).

`s"…"` is the keystone of the type-value surface (Pillar SV): the
bridge between the type-annotation world (`type:` metadata, checked
and erased) and the value world (validation, generation, schema
export). Without value semantics, `s"…"` is just a string with a
funny prefix.

## 2. Existing Infrastructure

The following already exists and is preserved:

- **Lexer:** `S_STRING` token kind (`src/syntax/rowan/lex.rs`). No
  interpolation or escape processing — `{` and `}` inside s-strings
  are literal characters. This means `s"{ name: String }"` already
  uses clean single braces with no `{{…}}` escaping needed.
- **Desugarer:** `Primitive::TypeData(String)` variant in core
  expressions (`src/core/expr.rs:54`).
- **Compiler:** `Primitive::TypeData(s)` compiles to
  `dsl::box_type_data(s)` — `BoxedTypeData` data constructor, tag
  17, arity 1, wrapping a string
  (`src/eval/stg/compiler.rs:1215`).
- **Type checker:** `TypeData(s)` synthesises as
  `Type::LiteralString(s)` (`src/core/typecheck/check.rs:3043`).
- **Type-DSL parser:** `parse_type` in
  `src/core/typecheck/parse.rs:1172` — public, already used by
  `eu doc` (`src/driver/doc/render.rs:169`).
- **Monad hints:** s-string raw text feeds `monad_type_registry`
  (`src/core/desugar/rowan_ast.rs:717–722`).

## 3. Design

### 3.1 Compile-time validation

In the desugarer (`src/core/desugar/literal.rs`), when processing an
s-string, call `parse_type` on the content string. If parsing fails,
report a compile error with the s-string's source location. If
parsing succeeds, continue to produce `Primitive::TypeData(s)` with
the **canonical** string (re-rendered from the parsed `Type` to
normalise whitespace and formatting).

This ensures:
- Type errors in s-strings are caught at compile time.
- The runtime string is always valid and canonical.
- The single-brace mode for records works naturally (the type-DSL
  parser already handles `{ name: String }` syntax).

### 3.2 Runtime representation (unchanged)

`BoxedTypeData` (tag 17) wrapping the canonical type-DSL string.
No new `Native` variant needed. The existing representation is
already distinguished from plain strings at the data-constructor
level.

**Rendering:** when the emitter encounters a `BoxedTypeData` value,
it outputs the wrapped string. This is the same as today —
s-strings render as their source text. Exporters (JSON, YAML, etc.)
render it as a plain string value.

### 3.3 `to-data` projection

A new intrinsic `TYPE_TO_DATA` that:

1. Takes a `BoxedTypeData` value (tag 17).
2. Parses the wrapped string via `parse_type`.
3. Projects the resulting `Type` to a tagged-list structure using
   the `t-*` vocabulary.

**The `t-*` tagged-list vocabulary** mirrors the `Type` enum
(`src/core/typecheck/types.rs`) 1:1:

| Type constructor | Tagged list |
|---|---|
| `Number` | `[:t-prim :number]` |
| `String` | `[:t-prim :string]` |
| `Bool` | `[:t-prim :bool]` |
| `List(T)` | `[:t-list T']` |
| `Fun(A, B)` | `[:t-fn A' B']` |
| `Record({...})` | `[:t-record {k1: T1', ...}]` |
| `Union(A, B)` | `[:t-union A' B']` |
| `Forall(v, T)` | `[:t-forall :v T']` |
| `App(F, X)` | `[:t-app F' X']` |
| `Con(name)` | `[:t-con :name]` |
| `Var(name)` | `[:t-var :name]` |
| `Mu(v, T)` | `[:t-mu :v T']` |
| `Partial(T)` | `[:t-partial T']` |
| `Any` | `[:t-prim :any]` |
| `Tuple(...)` | `[:t-tuple T1' T2' ...]` |

Where `T'` denotes recursive projection of sub-types.

A prelude wrapper `to-data` dispatches on the data tag:
existing embeddings (AST `a-*`, Core `c-*`, STG `s-*`) already
follow this pattern. `to-data` on a `BoxedTypeData` calls
`TYPE_TO_DATA`; on other values, existing behaviour is preserved.

### 3.4 `from-data` builder

A prelude function `from-data` that takes a `t-*` tagged list and
produces a `BoxedTypeData` value. Implementation:

1. Walk the tagged-list structure, matching `t-*` tags.
2. Reconstruct the canonical type-DSL string.
3. Wrap in `BoxedTypeData`.

This is pure prelude-level code — no intrinsic needed. The
tagged-list → string rendering is straightforward pattern matching.

### 3.5 Round-trip guarantee

`s"T" to-data from-data` produces a value that renders identically
to `s"T"`. The canonical string normalisation in §3.1 ensures this.

## 4. Scope

### In scope

- Compile-time validation of s-string content via `parse_type`.
- Canonical string normalisation.
- `TYPE_TO_DATA` intrinsic for `to-data` projection.
- `t-*` tagged-list vocabulary definition.
- `from-data` prelude builder function.
- Round-trip guarantee.

### Out of scope

- `as-spec` / `to-spec` — that is SV2 (eu-amjn), which consumes
  the `to-data` projection.
- Optional record fields — that is a separate SV bead (eu-luyx),
  which enriches the type vocabulary.
- Prefix-list type — 0.13.
- Runtime `typeof(value)` reflection — aspirational end-state.
- Schema export / JSON Schema — SV4.

## 5. Success Criteria

1. **Compile-time validation:** `s"nubmer"` produces a compile
   error. `s"{ name: String }"` compiles successfully.
2. **Rendering:** `s"number"` renders as `"number"` in YAML/JSON
   output. `s"{ name: String, age: Number }"` renders as its
   canonical string.
3. **`to-data` projection:** `s"[String]" to-data` produces
   `[:t-list [:t-prim :string]]`.
4. **`from-data` round-trip:**
   `s"Number -> String" to-data from-data` renders identically to
   `s"Number -> String"`.
5. **Existing uses unaffected:** `monad:` hints continue to work
   with s-strings.
6. **Full harness green.**

## 6. Testing

- Targeted harness tests for s-string validation (valid and invalid
  type strings).
- Targeted harness tests for `to-data` projection of each `t-*`
  type form.
- Round-trip test: `to-data from-data` identity.
- Existing harness and monad tests pass unchanged.

## 7. Risks

- **Low:** the lexer, desugarer, and runtime representation are
  already in place. The changes are additive.
- **Medium:** the `t-*` vocabulary is a new versioned surface. Its
  schema should be documented and kept stable once shipped. Changes
  to the `Type` enum in future would need corresponding `t-*`
  updates.
- **Low:** compile-time validation could reject s-strings that
  previously compiled (if they contained invalid type syntax). This
  is intentional — those strings were silently wrong.
- **Medium:** the canonical string normalisation must be carefully
  implemented to preserve semantic equivalence (e.g. whitespace,
  parenthesisation). The round-trip test is the safety net.
