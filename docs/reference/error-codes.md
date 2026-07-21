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

<!--
To add a new code: append a `### EU-<AREA>-<SLUG>` section here with
"What it means", "Example", and "How to fix it" — positive guidance only,
no anti-pattern enumeration. Then map the variant to the code in the
relevant `code()` method (`ExecutionError::code`, and analogous methods
as they are added for `CoreError`, `ParserError`, `TypeWarning`).
-->
