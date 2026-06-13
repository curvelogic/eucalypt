# s-string: Type-Data String Literal

- **Date:** 2026-06-13
- **Status:** Draft
- **Bead:** eu-kgsi.5
- **Roadmap:** §4.3 — prerequisite for W8 (`eu doc`) and W16 (contracts)

---

## 1. Problem

Type annotations are written as strings in metadata: `{ type: "number -> string" }`.
Record types require escaped braces: `c"\"{{x: number}}\" -> number"`. There is
no way to reference a type as a **value** — types exist only inside annotation
strings, invisible to the runtime.

W8 (`eu doc`) needs to extract types as values for schema generation. W16
(contracts) needs to transform types into runtime specs via `as-spec`. Both
require a value-level representation of types, distinct from plain strings.

## 2. Goal

A new string prefix `s"..."` that produces a `Native::TypeData` value —
a first-class type-data literal. Braces are literal (no interpolation, no
escaping), making record types natural to write: `s"{x: number, y: string}"`.

## 3. Design

### 3.1 Surface syntax

`s"..."` — a string prefixed with `s`. No interpolation, no escape
processing. Braces, quotes, and all other characters are literal content.
The only special character is `"` which terminates the string.

```eucalypt
# Type-data values
t1: s"number -> string"
t2: s"{x: number, y: string} -> symbol"
t3: s"\"block\" | \"log\""

# Compare with current c-string annotation style
` { type: c"\"block\" | \"log\"" }
f(a): :ok

# With s-string in type: metadata (future — not required for 0.9)
` { type: s"\"block\" | \"log\"" }
f(a): :ok
```

### 3.2 Lexer

Add `SString` to `StringPrefix` in `src/syntax/rowan/lex.rs`:

```rust
pub enum StringPrefix {
    None,
    CString,
    RawString,
    TString,
    SString,   // new
}
```

In the character-dispatch logic (around line 271), recognise `s"` as a
prefixed string:

```rust
's' if self.peek_is('"') => {
    return self.dquote_with_prefix(i, StringPrefix::SString);
}
```

Add a new `SyntaxKind::S_STRING` token. No interpolation pattern tokens
needed — s-strings do not interpolate (like t-strings; note that r-strings
*do* support interpolation despite being "raw").

### 3.3 Parser

The parser handles `S_STRING` like `T_STRING` (a simple string, no pattern
children). The Rowan AST gains an `SStringPattern` or similar, but since
there is no interpolation it is just a leaf token containing the literal
text.

### 3.4 Desugarer

`s"..."` desugars to:

```rust
Expr::Literal(smid, Primitive::TypeData(content))
```

where `content` is the raw string content with no escape processing (like
`RawString`). This requires adding `TypeData(String)` to `Primitive` in
the core expression types.

### 3.5 Runtime representation

Add `TypeData` to `Native` in `src/eval/stg/syntax.rs`:

```rust
pub enum Native {
    Sym(String),
    Str(String),
    Num(Number),
    Zdt(DateTime<FixedOffset>),
    TypeData(String),   // new
}
```

And to `Primitive` in the core expression types.

### 3.6 Type checking

`s"..."` synthesises as a new `Type::TypeData` base type (or simply
`Type::String` if we don't want to distinguish at the type level yet).

For 0.9, `Type::String` is sufficient — type-data is distinguished at
the runtime level by its `Native` variant, not at the type-checker level.

### 3.7 Export / rendering

Type-data values render as their string content in all export formats:

| Format | `s"number"` renders as |
|--------|----------------------|
| JSON   | `"number"` |
| YAML   | `number` |
| TOML   | `"number"` |
| Text   | `number` |

This makes type-data transparent in output — it looks like a string. The
distinction is only meaningful at runtime for intrinsics like `as-spec`
(W16) that dispatch on the `Native` tag.

### 3.8 Intrinsic support

A `type-data?` predicate intrinsic (or check via the existing type
introspection) to test whether a value is type-data at runtime. This
enables W16's `as-spec` to accept both `s"..."` values and (for
backwards compatibility) plain strings.

### 3.9 What this does NOT do

- **No interpolation**: braces and all characters are literal.
- **No escape processing**: like r-strings.
- **No type-level distinction**: the type checker sees `string` (or a
  future `type-data` type), not the parsed type structure.
- **No runtime type parsing**: `s"..."` stores the DSL string, it does
  not parse it into a `Type` value at runtime. Parsing happens when
  consumers like `as-spec` (W16) or `eu doc` (W8) need it.
- **No changes to `type:` metadata handling**: existing `{ type: "..." }`
  and `{ type: c"..." }` annotations continue to work as before.

## 4. Implementation sketch

### Phase 1: Lexer
- Add `SString` to `StringPrefix`.
- Add `S_STRING` to `SyntaxKind`.
- Recognise `s"` prefix in the lexer dispatch.
- No interpolation tokens — s-strings are simple like t-strings.

### Phase 2: Parser and AST
- Handle `S_STRING` tokens in the parser.
- Add to the Rowan AST element types.

### Phase 3: Core types
- Add `TypeData(String)` to `Primitive` in `src/core/expr.rs`.
- Add `TypeData(String)` to `Native` in `src/eval/stg/syntax.rs`.

### Phase 4: Desugarer
- Desugar `S_STRING` to `Expr::Literal(smid, Primitive::TypeData(text))`.
- No escape processing.

### Phase 5: STG compiler
- Handle `Primitive::TypeData` → `Native::TypeData` in the literal
  compilation path.

### Phase 6: Export
- Add `TypeData` rendering to each exporter (JSON, YAML, TOML, text,
  EDN, HTML, markup) — render as the string content.

### Phase 7: Intrinsics
- Add `type-data?` predicate or extend `type-of` to recognise type-data.

### Phase 8: Validation
- Harness tests: `s"number"` evaluates and renders correctly.
- Round-trip: `s"..."` content survives through the pipeline unchanged.
- Braces literal: `s"{x: number}"` produces the string `{x: number}`
  with no escaping needed.

## 5. Test plan

- **Basic rendering**: `s"number -> string"` renders as `"number -> string"`
  in JSON output.
- **Braces are literal**: `s"{x: number}"` renders as `{x: number}` — no
  doubling or escaping.
- **Quotes in content**: `s"\"block\" | \"log\""` renders correctly (the
  only escape is `\"` to embed a literal double-quote, inherited from the
  lexer's string termination).
- **Not a plain string**: `s"number" = "number"` is false (different
  `Native` variants).
- **Type-data predicate**: `s"number" type-data?` returns true; `"number"
  type-data?` returns false.
- **All export formats**: type-data renders as string content in JSON,
  YAML, TOML, text.
- **No interpolation**: `s"hi {name}"` produces the literal string
  `hi {name}`, not an interpolation.

## 6. Risks

- **Escaping double quotes**: the only character that needs escaping in
  s-strings is `"` itself (to terminate the string). For type DSL content
  containing literal string types like `"block" | "log"`, users write
  `s"\"block\" | \"log\""`. This is still better than the c-string
  alternative but not perfect. Mitigate: document clearly.
- **Equality semantics**: `s"number" ≠ "number"` because they are different
  `Native` variants. This is deliberate (§4.3: "kept deliberately distinct
  from a bare symbol") but may surprise users expecting string equivalence.
  Mitigate: document; provide explicit conversion functions if needed.

## 7. Success criteria

- `s"..."` lexes, parses, desugars, compiles, and renders correctly.
- Braces are literal — record types are natural to write.
- Type-data is distinguishable from plain strings at runtime.
- All existing string prefix behaviour unchanged.
