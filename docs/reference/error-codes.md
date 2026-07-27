# Error Codes

Some eucalypt diagnostics carry a stable code such as `EU-EVAL-TYPE`,
shown in the diagnostic header (`error[EU-EVAL-TYPE]: ...`) and in the
`code` field of `--error-format json` output. A code is stable across
releases: once assigned, it identifies the same category of problem even
if the wording of the message changes, so it is safe to match on in
scripts, editor integrations, or your own notes.

Not every diagnostic has a code yet — codes are being added
incrementally, category by category. A diagnostic without a code is
still fully described by its message, source location, and notes; see
[Error Messages Guide](error-messages.md) for how to read those.

Codes are namespaced by area: `EU-<AREA>-<SLUG>`, e.g. `EU-EVAL-TYPE`
for a runtime evaluation error, `EU-PARSE-*` for a parser error,
`EU-NAME-*` for name resolution, `EU-TYPE-*` for a type-checker warning.

Look up a code from the command line with `eu error <CODE>`, e.g.
`eu error EU-EVAL-TYPE` prints the catalogue entry below. This is
independent of `eu explain <FILES>`, which explains what a run would do
given input files rather than looking up a code.

## Catalogue

### `EU-EVAL-TYPE`

**What it means:** a value of one type was used where a different type
was required — for example, a string was passed to an arithmetic
operator that expects a number, or a number was used where a block was
expected. This is a runtime error: the mismatch was only discovered when
the value was evaluated.

**Example:**

```eucalypt
a: "3"
result: a + 1
```

```text
error[EU-EVAL-TYPE]: type mismatch: expected number, found string "3"
  ┌─ example.eu:2:11
  │
2 │ result: a + 1
  │           ^
  │
  = to convert a string to a number, use 'num', e.g. 's num'
  = to concatenate strings, use string interpolation or 'str.join-on' instead of '+'
```

**How to fix it:** convert the value to the type the operator expects
before using it — for example, `a num + 1` converts the string to a
number first. See the [prelude reference](prelude/) for the conversion
functions available for each type (`num`, `str`, `str.of`, and so on).

### `EU-EVAL-CONTRACT`

**What it means:** data failed a structural contract applied with `ensure`.
The contract is an ordinary type spec written as an `s"…"` literal, and the
diagnostic's notes list every position that did not conform, each with a
path into the data. The source location is the `ensure` call site — the
contract that failed — while the paths in the notes say where in the data
to look.

**Example:**

```eucalypt
{ import: "contract.eu" }
schema: s"{ name: string, port: number }"
config: { name: "web", port: "8080", debug: true } ensure(schema)
```

```text
error[EU-EVAL-CONTRACT]: contract violation: 2 violations against {name: string, port: number}
  ┌─ example.eu:3:51
  │
3 │ config: { name: "web", port: "8080", debug: true } ensure(schema)
  │                                                   ^^^^^^^^^^^^^^
  │
  = port: expected number, found string
  = (root): unexpected keys: debug
```

**How to fix it:** correct the data at each path the notes name, or widen
the spec if the shape you are receiving is the shape you intended — a
closed record spec such as `{name: string}` reports surplus keys, and
adding `..` (`{name: string, ..}`) permits them. To inspect the violations
as data rather than aborting, call `validate(spec, data)` instead: it
returns the same information as a list of blocks and never raises. See the
[Contracts guide](../guide/contracts.md).

### `EU-RENDER-UNREPRESENTABLE`

**What it means:** a value survived evaluation intact, but the output
format you asked for has no way to carry it. The commonest case is an
integer above `9223372036854775807` (the signed 64-bit maximum): JSON,
EDN and eucalypt itself carry it happily, but a YAML integer scalar and
a TOML integer are both signed 64-bit. TOML also has no null, so a null
value cannot be rendered as TOML either.

**Example:** given `big.json` holding `{"n": 9999999999999999999}`,

```eucalypt
` { target: :main output: :yaml import: "data=big.json" }
main: data.n
```

```text
error[EU-RENDER-UNREPRESENTABLE]: cannot represent this value in YAML output: the integer 9999999999999999999 is above 9223372036854775807, the largest integer a YAML integer scalar can carry
  ┌─ example.eu:2:7
  │
2 │ main: data.n
  │       ^^^^^^
  │
  = render to a format that can carry the value — 'json', 'edn', 'text' and 'eu' output all keep integers of this magnitude
  = to keep the exact digits in this format, convert the value to a string first with 'str', e.g. 'n str'
```

**How to fix it:** render to a format that can carry the value — `-x
json`, `-x edn`, `-x text` and `-x eu` all keep integers of this
magnitude — or convert it to a string first with `str`, so the exact
digits are preserved as text. For a null being rendered as TOML, give
the key a value or drop it from the block before rendering. eucalypt
reports this rather than quietly rounding the value or changing its
type, so that what you export matches what you evaluated.

### `EU-RENDER-SHAPE`

**What it means:** the output format needs the document to have a
particular structure, and the value being rendered does not have it.
`html` output renders *hiccup* markup — a list whose first item is the
tag, optionally followed by an attribute block and then contents — so
rendering a block, or a bare scalar, has no meaning as html.

**Example:**

```eucalypt
` { target: :main format: :html }
main: { a: 1 b: 2 }
```

```text
error[EU-RENDER-SHAPE]: cannot render this document as html: the value to render is a block, but markup output needs a hiccup element — a list whose first item is the tag
  = html output renders hiccup markup: a list of tag, attribute block and contents, e.g. '[:div, { id: "top" }, "hello"]'
  = select the markup value with a target or '-e', e.g. 'eu -x html -e page.eu'; rendering a whole unit gives html the unit's block, which is not markup
  = to render arbitrary data instead, choose a format that accepts any shape, such as 'yaml', 'json' or 'text'
```

**How to fix it:** build the value as hiccup markup, e.g. `[:div, { id:
"top" }, "hello"]`, and select it with a target or `-e` so that html is
given the markup itself rather than the enclosing unit's block. To
render arbitrary data instead, choose a format that accepts any shape,
such as `yaml`, `json` or `text`.

This diagnostic often has no source location: the document's root events
are emitted by the render pipeline rather than by a user expression, so
there is frequently no span to point at.

<!--
To add a new code: append a `### EU-<AREA>-<SLUG>` section here with
"What it means", "Example", and "How to fix it" — positive guidance only,
no anti-pattern enumeration. Then map the variant to the code in the
relevant `code()` method (`ExecutionError::code`, and analogous methods
as they are added for `CoreError`, `ParserError`, `TypeWarning`), and add
a matching entry to the `CATALOGUE` in `src/driver/error_codes.rs` so
`eu error <CODE>` can look it up too.
-->
