# Spec: First-Class Alias References in the Type DSL (Bead A7)

**Status**: Specification — ready to implement.
**Date**: 2026-05-18
**Branch**: `type-system-exploration`
**Companions**: [type-system-evolution.md](./type-system-evolution.md) (H12a),
[type-system-bead-plan.md](./type-system-bead-plan.md) (TS-A7).

A7 is **tooling, not type theory**. Type aliases (`Person`, `Json`)
referenced inside `type:` strings already *resolve* correctly — the DSL
treats a capitalised identifier as an alias reference against the
checker's alias map. What is missing is editor support: go-to-definition,
hover, and rename do not work for an alias name written inside a type
string. A7 adds it, with no change to where aliases are defined (they
stay in `types:` / `type-def:` metadata — no new binding form).

## Background — what exists today

- The type-DSL lexer (parse.rs) tracks a byte position per token but
  **never materialises spans**: identifiers become `Token::Ident(String)`
  with no attached position, and `parse_type` returns a bare `Type`.
- A capitalised identifier and a lowercase type variable are **both**
  parsed to `Type::Var(TypeVarId(name))` — no AST distinction. The
  checker resolves capitalised names against the alias map afterwards.
- Aliases are defined in two places, both already in the AST and fully
  visible to tooling:
  - a `types: { Name: "type string", … }` sub-block in a metadata
    expression (`register_aliases_from_meta`, check.rs);
  - a `type-def: "Name"` entry in a binding's metadata
    (`extract_type_def_name`, check.rs).
- An existing LSP lives in `src/driver/lsp/` (`hover.rs`,
  `completion.rs`, `diagnostics.rs`, `inlay_hints.rs`).

## A7.1 Parser — record alias-reference spans

Add a tooling-only entry point alongside `parse_type`, leaving the
existing checker callers untouched:

```rust
struct AliasRef { name: String, span: (usize, usize) }   // byte offsets in the input
fn parse_type_with_refs(s: &str) -> Result<(Type, Vec<AliasRef>), ParseError>
```

The lexer already has the start offset of each token; record the end
offset too (`start + name.len()`). During parsing, whenever a
**capitalised** identifier is turned into a `Type::Var`, push an
`AliasRef { name, span }`. Lowercase identifiers (type variables) are
*not* recorded. The result is a flat list of every alias reference in
the string with its byte range. `parse_type` itself is unchanged —
internally it can be `parse_type_with_refs(s).map(|(t, _)| t)`.

## A7.2 Span resolution — string offset to source position

An `AliasRef` span is a byte range *within the type-string content*. To
turn it into a source position the LSP needs the string literal's
origin:

- The `type:` value is a string-literal AST node carrying a `Smid`; the
  `SourceMap` resolves that `Smid` to a source range.
- The content begins one byte after the opening `"`. For a plain string
  literal, content offset *N* maps linearly to source position
  `content_start + N`.
- **Edge cases**: a `type:` string with escape sequences breaks the
  linear map (the source is longer than the content); a string built by
  `{{…}}` interpolation is desugared to a `JOIN` and has no single
  literal span. A7 handles **plain, unescaped** type strings — the
  overwhelming common case — and degrades gracefully (no
  alias-reference tooling, no error) for escaped/interpolated ones.
  Note this limitation in the LSP code.

**Rename safety.** If a user renames an alias via go-to-definition on a
plain-string occurrence, interpolated type strings referencing the same
alias are **not updated** (the LSP cannot see inside them). The type
checker would then fail with an "unknown alias" warning on the stale
reference. This is acceptable — interpolated type strings are extremely
rare in practice (type annotations are almost always plain literals),
and the checker warning immediately surfaces the stale reference. The
alternative — forbidding interpolated type strings entirely — would
reject valid code for a theoretical edge case. Document this in the
LSP rename handler: "Rename updates plain type-string references only;
interpolated or escaped type strings may contain stale references."

## A7.3 LSP — index alias definitions

Build, per checked document, an **alias-name → definition-site** map by
walking the AST (the LSP already has it):

- Each `types: { Name: "…", … }` block: `Name` → the source span of the
  `Name` key.
- Each `type-def: "Name"` entry: `Name` → the source span of the
  declared binding (or of the `type-def` string).

A name defined in both places, or twice, follows the checker's own
last-wins resolution; record all definition sites so rename can update
every one. The map is rebuilt on document change alongside the existing
diagnostics pass — no persistence (that is Phase B, TS-B7).

## A7.4 Wire the three features

For a cursor inside a `type:` string: parse it with
`parse_type_with_refs`, find the `AliasRef` whose span contains the
cursor (via §A7.2), and:

- **Go-to-definition** — jump to the alias's definition site from the
  §A7.3 map.
- **Hover** — show the alias name and its *resolved* type (the checker's
  alias map already holds the resolved `Type`; `humanise` it).
- **Rename** — rewrite the definition-site key *and* every alias
  reference (across all `type:` strings in scope) whose name matches.
  Renaming edits *inside* string literals — the LSP must produce text
  edits at sub-string offsets; reuse the §A7.2 mapping in reverse.

These hook into the existing `src/driver/lsp/` handlers. Go-to-def and
hover are read-only and low-risk; rename writes and should be guarded to
the plain-string case (§A7.2).

## Scope

A7 is independent of A1/A2/A3 — it touches the parser's offset tracking
and the LSP only, never the type lattice or the checker's inference. It
is P2 and can land any time. It does **not** make aliases first-class
*eucalypt* values (importable, usable in expressions) — that is H12c,
explicitly out of scope.

## Test plan

- **Parser** — unit tests: `parse_type_with_refs("Person -> Json")`
  returns both refs with correct byte spans; lowercase vars are not
  recorded; a malformed string still errors as before.
- **LSP** — go-to-definition from an alias reference in a `type:` string
  lands on its `types:`/`type-def:` site; hover shows the resolved
  type; rename updates the definition and all references; an
  escaped/interpolated type string yields no alias tooling and no crash.
- **Regression**: existing LSP behaviour (diagnostics, hover on
  ordinary bindings, completion) unchanged; `cargo test` green; clippy
  clean.

## File-by-file change summary

| File | Change |
|------|--------|
| `src/core/typecheck/parse.rs` | track token end offsets; `AliasRef`; `parse_type_with_refs` entry point |
| `src/driver/lsp/` | string-offset ↔ source-position mapping; alias-definition index; go-to-def / hover / rename for alias references |
| `tests/` | parser span unit tests; LSP alias-reference tests |
