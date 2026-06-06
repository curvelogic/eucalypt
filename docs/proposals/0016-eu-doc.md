# 0016 — `eu doc` — documentation & schema extraction

- **Status:** Draft proposal for review
- **Track:** D — tooling
- **Classification:** Whitespace
- **Suggested horizon:** 0.9
- **Related:** sibling proposals [0003](0003-conformance-testing-fuzzing.md) (conformance & doctest),
  [0009](0009-structural-contracts-validation.md) (structural contracts & schema),
  [0013](0013-type-dsl-embedding.md) (the `s"…"` type-DSL surface it reads),
  [0019](0019-host-language-interop.md) (host-language & schema interop)

---

## Summary

Eucalypt's declarations already carry first-class `doc:` metadata (345 instances
in `lib/prelude.eu`), `type:` annotations (327 instances), and in-language
assertion examples via `//=>`. The tree-sitter grammar aliases doc-string
literals to a named `docstring` node for syntax highlighting
(`editors/tree-sitter-eucalypt/grammar.js:66–68`). The LSP already extracts
`doc:` and `type:` into hover cards and completion tooltips
(`src/driver/lsp/symbol_table.rs:66–69`). Yet no command exists to harvest this
structural information into external documentation or machine-readable output: the
`docs/reference/prelude/` reference is hand-authored and can drift. This proposal
adds `eu doc` — a subcommand that walks a unit's declarations, collects `doc:`,
`type:`, and embedded examples, and emits Markdown, HTML, or JSON. It also
proposes a doctest mode that executes embedded examples, building on the
`scripts/test-doc-examples.py` infrastructure and `//=>` operators. For a
language that treats documentation as structured metadata rather than prose
comments, extraction is the natural completion of that bet.

---

## Motivation

### The gap: rich metadata, zero extraction

Eucalypt's documentation story has two levels that do not yet connect.

**Level 1 — in-source metadata, very complete.** Every prelude declaration of
any significance carries a `doc:` string and a `type:` annotation in a backtick
metadata block (`lib/prelude.eu:250–252`):

```eu
` { doc: "`if(c, t, f)` - if `c` is `true`, return `t` else `f`."
    type: "bool → a → b → (a | b)" }
if: __IF
```

This is not a comment; it is structural data — a block value attached to the
declaration by the parser, preserved through the pipeline, and already read by
the LSP server (`src/driver/lsp/symbol_table.rs:334–376`). The tree-sitter
grammar distinguishes `docstring` as a named node type distinct from ordinary
string literals (`editors/tree-sitter-eucalypt/grammar.js:66`), and the Emacs
mode highlights it accordingly. The metadata is designed to be machine-readable.

**Level 2 — published reference, hand-maintained.** The `docs/reference/prelude/`
directory contains fifteen hand-written Markdown files (arrays, blocks, booleans,
calendar, combinators, io, lists, metadata, numbers, random, sets, strings, …).
These are tabular summaries (`docs/reference/prelude/strings.md:1–40`):

```markdown
| `str.split` | Split string `s` on separators matching regex `re` |
| `str.trim`  | Trim leading and trailing whitespace                |
```

These descriptions are paraphrases of the `doc:` strings in `lib/prelude.eu`;
they are not generated from them. Every time a function's signature or doc string
changes, someone must remember to update both places. Since the LSP hover text is
driven from the live source and the published reference is static, they can — and
eventually will — diverge.

**The partial workaround.** `scripts/test-doc-examples.py` already extracts
`eu` code blocks from `docs/` markdown files and runs them, catching regressions
in the *published* docs (`scripts/test-doc-examples.py:1–23`). This is a partial
doctest facility for documentation-as-prose, but it is the inverse of what is
needed: it tests whether the docs agree with the runtime, but does not generate
docs from the runtime.

**The missing operator.** The `eu` CLI (`src/driver/options.rs:85–108`) has
subcommands `run`, `test`, `dump`, `check`, `fmt`, `lsp`, `version` — but no
`doc`. There is no way for a user or CI job to ask: "given this `.eu` file, what
does it export and document?"

### Why this matters for 1.0

A 1.0 stability commitment (see [0001](0001-v1-charter.md)) means the prelude
API is stable. Stable APIs need canonical, versioned reference documentation.
Canonical means *generated from the source of truth*, not hand-copied. The
machinery to do this is almost entirely present; what is missing is the
subcommand that ties it together.

---

## Prior art & landscape

The principle is old and consistent: documentation should be extracted from the
source, not maintained separately.

**Rustdoc** (Rust's standard tool) extracts `///` doc comments from source,
renders them as HTML with cross-links, and runs embedded code examples as tests
(`cargo test --doc`). The key design decision: doc comments are not stripped
before compilation — they are first-class attributes visible to `rustdoc` as
structured tokens. Eucalypt's `doc:` metadata is structurally equivalent, except
it is an explicit key-value field in a block rather than a comment convention.
Eucalypt's approach is in some respects *cleaner*: the metadata is already parsed
and typed, not scraped from freeform comment strings.

**Pkldoc** (Apple's Pkl configuration language) generates navigable HTML from
Pkl module doc comments, with hyperlinks between modules, classes, and
properties. Pkldoc is offered as a CLI, a Gradle plugin, and a Java library.
Pkl is an especially apt comparison because, like eucalypt, it is a
configuration-and-data language rather than a general-purpose one, and its doc
generator has first-class JSON Schema generation as a closely related feature
(`pkl-lang.org/main/current/pkl-doc/index.html`). Eucalypt's `type:` annotations
are analogous to Pkl's class definitions; the same extraction pass that produces
documentation could produce a JSON Schema, as discussed below.

**ExDoc** (Elixir/Erlang) extracts from compiled beam modules, not source tokens.
The practical consequence is that documentation can reference types and
specifications that are resolved at compile time. Eucalypt cannot do this today
— it evaluates lazily and type information is erased before the VM runs — but the
simpler approach (source-level extraction) is sufficient for the prelude and
works with the existing pipeline.

**Haddock** (Haskell) introduced the pattern of annotated type signatures as the
primary documentation anchor. Eucalypt's `type:` metadata mirrors this: the
signature is the key documentation artefact; the prose fills in the semantics.

**The shared principle:** in every case, docs are extracted from the same source
the compiler reads. Divergence between docs and implementation is structurally
impossible when extraction is automated. For eucalypt, where `doc:` is literal
program data rather than a comment convention, this principle applies with
particular force.

---

## Proposed design

### 4.1 The `eu doc` subcommand

```
eu doc [OPTIONS] [FILES...]
```

The subcommand accepts the same file/input syntax as `eu run`. It **parses
the unit and reads declaration metadata directly off the syntax tree** — the
same source-level extraction the LSP performs (`extract_documentation` walks
the parsed `Declaration`, no evaluation, `symbol_table.rs:334`) — walking the
top-level declarations in declaration order, and for each extracts:

| Field | Source |
|-------|--------|
| Name | declaration head |
| `doc:` | `extract_documentation()` — already implemented in `src/driver/lsp/symbol_table.rs:334` |
| `type:` | `extract_type_annotation()` — already implemented at line 381 |
| `export:` | to determine visibility (`:suppress` hides internal declarations) |
| `precedence:` / `associates:` | for operator documentation |
| `example:` | optional new metadata key (see §4.3) |

The extraction logic already exists and is tested in
`src/driver/lsp/symbol_table.rs:656–727`. `eu doc` reuses it directly.

### 4.2 Output formats

`--format` accepts three values:

**`markdown`** (default): one file per namespace, with a table of declarations,
their type signatures, and their doc strings, followed by a fenced example block
for each declaration that has one. This is structurally equivalent to the
hand-written `docs/reference/prelude/*.md` files, so they can be regenerated
automatically:

```
eu doc --format markdown lib/prelude.eu --out-dir docs/reference/prelude/
```

**`json`**: a machine-readable array of declaration objects:

```json
[
  {
    "name": "if",
    "type": "bool → a → b → (a | b)",
    "doc": "`if(c, t, f)` - if `c` is `true`, return `t` else `f`.",
    "namespace": null,
    "examples": []
  }
]
```

This is a stable, queryable representation for tooling (IDE integrations, doc
site generators, schema validators).

**`html`**: a thin renderer over the Markdown output. Keep it simple: a static
single-page HTML file with anchor links per declaration. No JavaScript framework,
no build system dependency. The JSON output is the authoritative structured form;
HTML is a convenience. A future web renderer or external tool can consume the
JSON.

### 4.3 Example metadata and doctest mode

The existing `//=>` operator asserts equality of expressions within a running
eucalypt program (`lib/prelude.eu:1260–1269`). Doc examples should be embedded
in `doc:` strings as fenced code blocks (already the convention in the existing
prelude strings), or via a new optional `example:` key in the metadata block:

```eu
` { doc: "`take(n, l)` - return initial segment of `n` elements from list `l`."
    type: "number → [a] → [a]"
    example: "[1, 2, 3, 4, 5] take(3) //=> [1, 2, 3]" }
take: __TAKE
```

When `--doctest` is passed, `eu doc` extracts the `example:` strings (and fenced
`eu` blocks in `doc:` strings), writes each to a temporary file, and executes
them via the existing `eu` binary **in test mode** (as `eu test` runs). Failure
is a non-zero exit code. This is the same pattern as
`scripts/test-doc-examples.py` but driven from the source rather than from prose
docs, and using the `//=>` assert-and-pass-through operator rather than testing
exit codes alone.

The `//=>` operator already satisfies the assertion semantics, and its
behaviour is **mode-dependent** (`__EXPECT`, `lib/prelude.eu:1256-1266`): it
returns the left-hand value on success, and on failure it **emits a clean
diagnostic to stderr and returns `false` in test mode** — only *normal* mode
panics. The doctest runner should therefore run examples in test mode, so a
failing example produces a structured diagnostic and a non-zero result rather
than a Rust panic (consistent with the project's panic policy). The runner
needs only to write each example to a file and run it in test mode.

This connects directly to [0003 — conformance suite](0003-conformance-testing-fuzzing.md):
doctest examples in the prelude become part of the conformance corpus, so any
alternative implementation must pass them.

### 4.4 Prelude reference regeneration

The immediate high-value application:

```
eu doc --format markdown lib/prelude.eu --out-dir docs/reference/prelude/
```

This replaces the fifteen hand-written files with generated output. To keep the
generated files readable and navigable, the generator should:

1. Group by namespace (top-level block names: `io`, `str`, `random`, `math`, …).
2. Suppress declarations with `export: :suppress` from public output.
3. Preserve declaration order (meaningful in eucalypt — later declarations can
   shadow earlier ones in the merge pipeline).
4. Include the `type:` annotation in a code block immediately below the name, so
   generated output matches the existing hand-written style.

The CI job for prelude documentation becomes:

```sh
eu doc --format markdown lib/prelude.eu --out-dir docs/reference/prelude/
git diff --exit-code docs/reference/prelude/
```

Failing this check means the prelude changed but the docs were not regenerated —
the same pattern as `cargo fmt --check`.

### 4.5 Schema angle (connection to 0009 and 0019)

A typed block shape in eucalypt — a block where all fields carry `type:`
annotations — is structurally equivalent to a JSON Schema object definition.
The `eu doc --format json` output for such a block contains enough information
to emit a JSON Schema fragment:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "properties": {
    "name": { "type": "string", "description": "..." },
    "count": { "type": "integer", "description": "..." }
  }
}
```

This is not a full schema compiler, but it is a natural extension of the JSON
output format. The `type:` DSL parser already lives in
`src/core/typecheck/parse.rs`; the JSON output pass can call it to map primitive
type expressions (`string`, `number`, `bool`, `[a]`, record types) to JSON
Schema vocabulary. Complex polymorphic types (`a → b → c`) are rendered as `{}`
(any) with a note, rather than blocking the output. (Per [0013], type
annotations are written as `s"…"` literals — so record types read as
`s"{host: string}"` without the `{{..}}` doubling; `eu doc` reads the same DSL
through `parse.rs` either way.)

This shares machinery with [0019 — host-language interop](0019-host-language-interop.md)
(which proposes ingesting JSON Schema and emitting eucalypt types) and complements
[0009 — structural contracts](0009-structural-contracts-validation.md) (which
proposes runtime schema validation). The three proposals form a coherent loop:
types are written once (as `type:` metadata), validated at runtime (0009),
exported as schemas (0016), and imported from external schemas (0019).

---

## Interaction with the existing roadmap

`eu doc` is not on the type-system roadmap (Stages A/B/C) because it is
orthogonal to the type checker: it operates on the *source representation* of
types, not the inferred or checked representation. This means it is available
even before the type system is complete, and it degrades gracefully when `type:`
annotations are absent.

Dependencies:

- **0015 — parser error recovery**: `eu doc` benefits from resilient parsing
  (it should report doc for valid declarations even in a file with errors), but
  it is not blocked on 0015.
- **0003 — conformance suite**: doctest mode makes prelude examples part of the
  conformance corpus. This is additive, not a dependency.
- **0004 — compiled-unit caching**: `eu doc` only **parses** (it reads
  metadata off the tree, not evaluated values), so it is largely independent of
  0004; the parse-result query in [0014] would reduce its latency, but neither
  is required.
- **0009, 0019**: the JSON Schema output is an optional extension, sequenced
  after the core extraction.

The proposal supersedes the hand-maintained `docs/reference/prelude/` files in
the sense that, once `eu doc` is working, those files should be generated rather
than hand-edited. It does not supersede any roadmap item.

---

## Implementation sketch

### Phase 1 — Core extraction and JSON output (small)

**Components touched:** `src/driver/options.rs`, `src/driver/` (new
`doc.rs`), `src/driver/lsp/symbol_table.rs` (refactor `extract_documentation`
and `extract_type_annotation` into a shared library function).

The extraction functions already exist and are unit-tested
(`symbol_table.rs:656–727`). Phase 1 is:
1. Add `Doc(DocArgs)` variant to the `Commands` enum in `options.rs`.
2. Move extraction functions from `symbol_table.rs` into a new
   `src/driver/doc_extract.rs` module, shared by LSP and `eu doc`.
3. Walk top-level `SymbolInfo` records, filter by `export: :suppress`, serialise
   to JSON via `serde_json`.

Estimated size: ~300 lines of new code, no new dependencies beyond `serde_json`
(already a dependency).

### Phase 2 — Markdown output and prelude regeneration (small-medium)

Write a Markdown renderer over the JSON output. Group by namespace, emit tables
and type blocks. Wire up the CI check. Replace the fifteen hand-written files
with a `make docs` or `cargo xtask docs` step that runs `eu doc` and commits the
diff.

Estimated size: ~200 lines. The main cost is deciding the grouping heuristic and
the exact Markdown template, which requires aesthetic choices.

### Phase 3 — Doctest mode (medium)

Add `--doctest` flag. Extract `example:` strings and fenced `eu` blocks from
`doc:` strings. For each, write to a temp file and invoke the `eu` binary (the
same strategy as `scripts/test-doc-examples.py`). Report pass/fail.

Estimated size: ~200 lines. Risk: the fragment-detection heuristic in
`scripts/test-doc-examples.py:107–140` may need to become more principled when
applied to `doc:` strings (which can be briefer than standalone `.md` code
blocks).

### Phase 4 — JSON Schema emission (medium, sequenced after 0009)

Extend the JSON output with a `json-schema` key when all fields of a block carry
`type:` annotations. Reuse the `type:` DSL parser from
`src/core/typecheck/parse.rs`. Emit draft-2020-12 schema fragments.

Estimated size: ~300 lines including tests. Risk: the type DSL includes
polymorphic types that have no JSON Schema equivalent; the mapping is partial and
must document its limitations honestly.

### Phase 5 — HTML output (small, lowest priority)

A thin static-HTML renderer over the Markdown. Single file, no build deps.
Lowest priority; the Markdown output covers most use cases.

---

## Alternatives considered

**Keep hand-writing the reference.** The status quo. Rejected because divergence
is inevitable at 1.0 scale, and the extraction infrastructure is 80 % present.

**Generate from the LSP hover protocol.** The LSP already renders `doc:` and
`type:` into Markdown for hover cards. One could drive an LSP client against
every identifier and collect the hover responses. Rejected: it is indirect,
requires a running LSP server, and the hover format is designed for small
snippets, not batch output.

**Embedded DSL for examples (new syntax).** Introduce a dedicated `examples:`
block with a list of eucalypt expressions. Rejected under the syntactic
conservatism non-negotiable: the existing `example:` string key (containing a
valid `eu` snippet) is sufficient, requires no new syntax, and can be adopted
incrementally. An `examples:` list of strings is a natural extension if needed.

**Full documentation site generator.** Generate a multi-page website with search,
navigation, and versioning. Explicitly out of scope: this is a thin
extraction-and-rendering pass, not a documentation platform. The JSON output is
the hook for external site generators.

---

## Risks & what would kill this

**The `export: :suppress` filter may be incorrect.** Declarations marked
`:suppress` are internal (`lib/prelude.eu:99`, `369`, and ~20 more operator
definitions). If the filter is too broad it omits public API; too narrow and it
exposes internal scaffolding. Resolution: start with a conservative filter
(suppress only explicit `:suppress`), compare output against the existing
hand-written files, and adjust.

**`doc:` strings are inconsistently formatted.** A quick scan of `lib/prelude.eu`
shows most `doc:` strings open with a function signature in backticks and then
prose. But some are bare prose and some are multi-paragraph. The Markdown
renderer must handle this gracefully; it cannot assume a rigid format. Mitigation:
treat `doc:` strings as Markdown and render them verbatim.

**Doctest coverage may be low initially.** The prelude currently embeds
assertions in separate test harness files, not in `doc:` strings. Phase 3 adds
value only if `example:` metadata is populated; that requires a sweep of the
prelude which is editorial work, not engineering. Mitigation: treat Phase 3 as
opt-in and add examples incrementally, starting with the most-used functions.

**The JSON Schema mapping is necessarily partial.** Polymorphic types, function
types, and union types in the `type:` DSL have no direct JSON Schema equivalent.
The output must document what it cannot represent, and not silently drop
information. Mitigation: emit an `x-eucalypt-type` extension key with the raw
type string alongside the best-effort JSON Schema translation.

---

## Success criteria

1. `eu doc --format json lib/prelude.eu` runs without error and emits a JSON
   array containing an entry for every non-suppressed prelude declaration.
2. `eu doc --format markdown lib/prelude.eu --out-dir /tmp/ref/` produces output
   that, when diffed against the current `docs/reference/prelude/`, covers at
   least 90 % of the functions described in the hand-written files.
3. A CI step `eu doc --format markdown lib/prelude.eu | diff - docs/reference/prelude/index.md`
   (or equivalent) passes after the prelude reference is regenerated.
4. `eu doc --doctest lib/prelude.eu` passes for all `example:` entries added in
   Phase 3, and is added to the test matrix.
5. The LSP hover text and `eu doc` JSON output agree on `doc:` and `type:` for
   a random sample of 20 prelude functions (spot-checked after implementation).

---

## References

- `lib/prelude.eu` — 2,264 lines; 345 `doc:` annotations, 327 `type:`
  annotations (verified by grep).
- `src/driver/lsp/symbol_table.rs:66–69` — `SymbolInfo` fields for
  `documentation` and `type_annotation`.
- `src/driver/lsp/symbol_table.rs:334–376` — `extract_documentation()`
  implementation.
- `src/driver/lsp/symbol_table.rs:381–382` — `extract_type_annotation()`
  implementation.
- `src/driver/options.rs:85–108` — current `Commands` enum (no `Doc` variant).
- `editors/tree-sitter-eucalypt/grammar.js:66–68` — `docstring` node alias.
- `scripts/test-doc-examples.py` — existing doc-example extraction and test runner.
- `lib/prelude.eu:1260–1269` — `//=>` assert-and-pass-through operator.
- `docs/reference/prelude/` — fifteen hand-maintained reference files.
- Rustdoc documentation tests: <https://doc.rust-lang.org/rustdoc/documentation-tests.html>
- Pkldoc generator: <https://pkl-lang.org/main/current/pkl-doc/index.html>
- ExDoc (Elixir): <https://github.com/elixir-lang/ex_doc>
