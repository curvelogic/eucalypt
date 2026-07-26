# Structural Contracts

Eucalypt's type checker runs before evaluation and is erased afterwards.
That is the right trade for code you wrote yourself — but it says nothing
about data that arrives from outside the program. An imported YAML file, a
CSV, a JSON payload: none of it was checked by anything, and by the time a
missing field surfaces as a lookup error you are several transformations
away from the thing that was actually wrong.

**Structural contracts** guard that boundary. You write a spec as an
ordinary `s"…"` type literal — the same type language as `type:`
annotations — apply it where the data comes in, and get back a precise
account of what does not conform.

```eu,notest
{ import: ["contract.eu", "cfg=config.yaml"] }

schema: s"{ name: string, port: number, tags?: [string] }"

config: cfg ensure(schema)
```

Contracts are **explicit**: nothing is checked unless you write the check,
and nothing is forced unless the spec names it.

## Two functions

| Function | Returns | Raises |
|---|---|---|
| `validate(spec, data)` | a list of violations; `[]` means conformant | never, for any data |
| `ensure(spec, data)` | `data`, unchanged | when the data does not conform |

`validate` is for when you want to *look* at the problems — count them,
group them, render them into a report, decide for yourself what to do.
`ensure` is for when there is nothing sensible to do but stop.

Both take the receiver last, so both read naturally in a pipeline:
`data validate(spec)`, `data ensure(spec)`.

Import them with:

```eu,notest
{ import: "contract.eu" }
```

## The report

Given `servers.yaml`:

```yaml
servers:
  - host: "a.example.com"
    port: 8080
  - host: "b.example.com"
    port: 9090
  - host: "c.example.com"
    port: "7070"
  - port: 6060
debug: true
```

and the spec:

```eu,notest
{ import: ["contract.eu", "data=servers.yaml"] }

schema: s"{ servers: [{ host: string, port: number }] }"

report: data validate(schema)
```

`report` is:

```yaml
- path: "servers[2].port"
  kind: type-mismatch
  expected: "number"
  found: "string"
- path: "servers[3]"
  kind: missing
  expected: "host"
  found: absent
- path: ""
  kind: unexpected
  expected: closed
  found: [debug]
```

Three separate faults, three separate entries — every violation is
collected, not just the first, because a boundary check exists to let you
fix the data in one pass rather than N.

The report is a plain list of plain blocks. It can be filtered, grouped,
counted, rendered to a table, or emitted as YAML like any other eucalypt
data. That is the point of returning data rather than a string.

### The four kinds

| `kind` | Raised when | `path` points at | `expected` | `found` |
|---|---|---|---|---|
| `:type-mismatch` | a value is present but does not match its spec | the **value** | the rendered type expected there | the runtime type name of the value |
| `:missing` | a required record field is absent | the **containing record** | the missing field's key | `:absent` |
| `:unexpected` | a **closed** record has surplus keys | the **record** | `:closed` | the surplus keys |
| `:length` | a tuple or prefix-list has the wrong arity | the **list** | the required length | the actual length |

The runtime type names in `found` are the predicate vocabulary the language
already exposes: `"number"`, `"string"`, `"symbol"`, `"bool"`, `"null"`,
`"datetime"`, `"list"`, `"block"`, `"function"`, `"type-data"`.

### Paths

`path` is a string, and it is the primary locator — format-independent,
always available, and meaningful for every input format:

| Step | Rendering |
|---|---|
| the root | `""` |
| a record field | `.name`, with the leading `.` elided at the root — so `port`, `server.port` |
| a field whose key is not identifier-shaped | `.'my key'` |
| a list or tuple index | `[3]`, appended with no separator — `servers[2].port` |

A rendered path is a valid eucalypt lookup expression relative to the
validated value, so it can be pasted into a `.`-chain or a lens path to
inspect the offending value.

## `ensure` and the error it raises

`ensure` returns the data **unchanged** on success, so it drops into a
pipeline without restructuring the code around it — the binding it feeds
looks the same with or without the check.

When the data does not conform it raises:

```text
error[EU-EVAL-CONTRACT]: contract violation: 2 violations against {name: string, port: number}
  ┌─ deploy.eu:7:39
  │
7 │ config: { name: "web", port: "8080" } ensure(schema)
  │                                       ^^^^^^^^^^^^^^
  │
  = port: expected number, found string
  = (root): unexpected keys: debug
```

Two locators, doing two different jobs. The **source location** is the
`ensure` call site: it answers "which contract failed". The **paths in the
notes** answer "which field to fix". Pointing at a line in the data file
itself is a separate matter, discussed under *What is not covered* below.

`ensure` on conforming data costs exactly one `validate` — the failure
branch is lazy, so nothing is rendered when there is nothing to report.

## Open and closed records

A spec literal means the **same thing** statically and at runtime. The
checker treats `s"{a: number}"` as a closed record, so the validator does
too:

```eu,notest
{ import: "contract.eu" }

data: { a: 1, b: 2 }

closed: data validate(s"{a: number}")       # one :unexpected entry for b
open:   data validate(s"{a: number, ..}")   # []
```

Adding `..` — or a named row variable, `..r`, which means the same thing to
a validator — permits surplus keys. This matters most for external data,
where a payload routinely carries fields you do not care about: reach for
the open form unless you genuinely want to be told about extras.

## Optional fields

`k?: T` means *absent is fine, present must match*:

```eu,notest
{ import: "contract.eu" }

schema: s"{ host: string, port?: number }"

a: { host: "x" }              validate(schema)   # [] — absence is conformant
b: { host: "x", port: 80 }    validate(schema)   # [] — present and correct
c: { host: "x", port: "80" }  validate(schema)   # one :type-mismatch at "port"
```

## Cost is paid only where written

The interpreter forces **only the paths the spec names**:

- A record spec forces the value far enough to know it is a block and
  enumerates its keys. Key enumeration walks the block's spine and forces
  **no** value. Only the values under keys the spec *names* are forced —
  so detecting surplus keys is a key-set comparison, never a value forcing,
  and even a closed spec is cheap.
- A list, tuple or prefix-list spec forces the spine, and forces each
  element only as far as its element spec demands.
- A primitive spec forces its value just far enough to name its type.
- `any`, `top`, type variables and unrecognised tags force **nothing**.

Three consequences worth relying on:

- `s"{ .. }"` against a 10 000-key imported block forces the spine and
  nothing else.
- `s"{name: string, ..}"` against that block forces exactly `name`.
- A spec is safe to apply to a value with expensive or diverging subtrees,
  as long as it does not name them.

`ensure` inherits this discipline exactly.

## A malformed spec is a different problem

A typo in a *schema* is a program bug, not a data problem, so it must not
appear in the report — otherwise it would send you to inspect the wrong
file. `validate` raises when its first argument is not a type spec at all:

```eu,notest
{ import: "contract.eu" }
r: { a: 1 } validate({ not: "a spec" })
# error: panic: validate: not a type spec
```

That is a different error from a contract violation, distinguishable both
on stderr and in an error test's `.expect` sidecar.

An unrecognised *tag inside a well-formed spec* is not malformed — it is
valid type data this version cannot check, so it accepts.

## Contracts and `as-spec`

`as-spec` and `match?` are unchanged and still available. The difference is
what you get back:

```eu,notest
{ import: "contract.eu" }

data match?(spec as-spec)   # a bare boolean
data validate(spec)         # a located account of what is wrong
```

`match?` is the right tool for a branch; `validate` is the right tool for a
boundary, where "something is wrong" is not actionable. Note that `match?`
matches records **openly** by design, so it agrees with `validate` on open
specs and deliberately differs on closed ones.

## What is not covered

- **`Dict(T)`, `NonEmpty([T])` and recursive types are accepted without
  checking**, matching `as-spec`, so the two never disagree about what
  conforms. Extending them is future work, and will extend both together.
- **Data-file blame** — reporting `config.yaml:12` rather than the `ensure`
  call site — needs every importer to carry per-value source provenance.
  Today only the YAML reader (which also serves JSON) does; offering it for
  those two alone would make the quality of an error message depend on the
  file extension. The `path` in the report is the locator that works
  everywhere.
- **Default-fill** — a spec supplying a value for an absent optional field
  — is a *transformer*, not a check, so it is not folded into `ensure`.

## Reference

- [Type Checking](type-checking.md) — the type language the specs are
  written in, and `to-data` / `from-data` / `type-str`
- [Navigating Nested Data](navigating-nested-data.md) — `match?` and safe
  navigation
- [Error Codes](../reference/error-codes.md) — `EU-EVAL-CONTRACT`
