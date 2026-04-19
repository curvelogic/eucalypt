# Type Checking

Eucalypt includes an optional, advisory type checker. It never prevents
your code from running — all existing code continues to work exactly as
before. Think of it as a second pair of eyes that can catch common
mistakes before they become confusing runtime errors.

```sh
eu check file.eu              # type-check, report warnings
eu check file.eu --strict     # treat type warnings as errors
eu --type-check file.eu       # check then evaluate
```

Type issues are reported as **warnings**, not errors. Your code runs
regardless.

---

## What type checking catches

The checker is most useful for catching a handful of mistakes that are
easy to make and hard to debug:

- **Passing an IO action where a plain value is expected.** `io.shell`
  returns an `IO({..})`, not a block. Accessing `.stdout` on the action
  itself is a common mistake the checker flags immediately.
- **Accessing a missing or misspelled field on a block.** If you have
  a block typed as `{name: string, age: number}`, accessing `.naem` is
  a warning.
- **Type mismatches in pipelines.** Piping a `string` into a function
  expecting `number` produces a clear warning with a source location.
- **Wrong lens composition.** `Lens(a, b)` composed with
  `Lens(c, d)` requires `b = c`; the checker catches the mismatch.

The checker works through inference — it does not require any
annotations in your code. Annotated prelude functions (like `map`,
`filter`, `str.of`, `io.shell`, `at`, `view`) propagate types through
your pipelines automatically.

## What type checking does not catch

- **Runtime-dependent values.** If a function returns `any` (because
  its type depends on a runtime value, like `lookup`), the checker
  makes no assumptions about the result. This is correct — eucalypt
  is gradual, not fully statically typed.
- **Unannotated functions.** Code without `type:` annotations is
  treated as `any -> any -> ... -> any`. The checker stays silent
  rather than guessing.
- **Dynamic block construction.** `block(kvs)` constructs a block
  from a list of pairs at runtime; the checker cannot know its shape.

---

## Adding type annotations

Annotations live in declaration metadata alongside doc strings:

```eu,notest
` { doc: "`double(x)` - double a number."
    type: "number -> number" }
double(x): x * 2
```

The `type:` value is a string containing a type expression. You can
annotate as much or as little as you like. Unannotated declarations
default to `any`.

### Simple annotations

```eu,notest
` { type: "number -> number" }
square(x): x * x

` { type: "string -> string -> string" }
greet(greeting, name): "{greeting}, {name}!"

` { type: "[number] -> number" }
total: fold(+, 0)
```

### Operators

Operators use the same metadata syntax:

```eu,notest
` { doc: "`x |> f` - pipe x into f."
    type: "a -> (a -> b) -> b"
    associates: :left
    precedence: 20 }
(x |> f): f(x)
```

---

## The type language

### Primitives

| Type       | Values                             |
|------------|------------------------------------|
| `number`   | integers and floats                |
| `string`   | strings                            |
| `symbol`   | symbolic atoms (`:key`, `:name`)   |
| `bool`     | `true`, `false`                    |
| `null`     | `null`                             |
| `datetime` | zoned date-time values             |
| `any`      | unknown — consistent with all types|

### Lists

`[T]` is a homogeneous list of `T`:

```eu,notest
` { type: "[number] -> [string]" }
to-strings: map(str.of)

` { type: "[a] -> a" }
first: head
```

### Tuples

Tuples use parentheses with commas:

| Syntax    | Meaning          |
|-----------|------------------|
| `(A, B)`  | pair             |
| `(A, B, C)` | triple         |
| `(A,)`    | 1-tuple          |

Tuples appear naturally in lens element types and in functions like
`discriminate` that return pairs:

```eu,notest
` { type: "(a -> bool) -> [a] -> ([a], [a])" }
discriminate: ...
```

### Records

Records describe the shape of blocks:

| Syntax           | Meaning                                           |
|------------------|---------------------------------------------------|
| `{k: T}`         | closed record — exactly the key `k` of type `T`  |
| `{k: T, ..}`     | open record — at least `k: T`, may have more      |
| `block`          | any block (no known shape)                        |

```eu,notest
` { type: "{name: string, age: number, ..} -> string" }
greet-person(p): "Hello, {p.name}!"
```

Open records are more commonly useful — most block-processing functions
don't require a specific shape.

### Functions

Arrow `->` is right-associative. Multi-argument functions are curried:

```eu,notest
# a -> b -> c  means  a -> (b -> c)
` { type: "a -> b -> a" }
const(k, _): k
```

### Unions

`|` expresses "either type". Useful for overloaded functions:

```eu,notest
` { type: "number -> number | string -> string" }
to-upper-or-double: ...
```

Overloaded operators use union types:
```
+   : number -> number -> number | [a] -> [a] -> [a] | array -> array -> array
<   : number -> number -> bool | string -> string -> bool | symbol -> symbol -> bool
```

### Type variables

Lowercase identifiers are type variables, universally quantified at
the declaration level:

```eu,notest
` { type: "(a -> b) -> [a] -> [b]" }
map: ...
```

At call sites, type variables are instantiated from the argument types.
`map(str.of, [1, 2])` instantiates `a = number, b = string`, giving
result type `[string]`.

### Special types

| Type    | Meaning                                               |
|---------|-------------------------------------------------------|
| `any`   | gradual/dynamic — no errors involving `any`           |
| `top`   | supertype of everything — nothing useful can be done with it |
| `never` | bottom type — unreachable code, empty collections     |

---

## IO actions

`IO(T)` is a proper type constructor, not a block with metadata.
IO actions have dedicated runtime representations — they are
genuinely distinct from the values they produce.

This is the type checker's most valuable capability: catching the
common mistake of treating an IO action as its result:

```eu,notest
# WRONG: io.shell returns IO(block), not block
result: io.shell("echo hello")
name: result.stdout              # warning: IO(block) has no field .stdout

# RIGHT: extract fields inside the monad
name: io.shell("echo hello") io.map(_.stdout)  # IO(string) ✓
```

The prelude IO functions are typed:

| Function         | Type                              |
|------------------|-----------------------------------|
| `io.shell(cmd)`  | `string -> IO(block)`             |
| `io.return(a)`   | `a -> IO(a)`                      |
| `io.bind(a, c)`  | `IO(a) -> (a -> IO(b)) -> IO(b)` |
| `io.map(f, a)`   | `(a -> b) -> IO(a) -> IO(b)`     |
| `io.fail(msg)`   | `string -> IO(a)`                 |

`IO(T)` is opaque. The only way to work with the value inside is
through `io.bind`, `io.map`, or the IO runner. This prevents IO
values from leaking into pure data.

---

## Lens and traversal types

The lens library is typed with two opaque type constructors:

| Type              | Meaning                                      |
|-------------------|----------------------------------------------|
| `Lens(a, b)`      | focuses on a single `b` within an `a`        |
| `Traversal(a, b)` | focuses on zero or more `b`s within an `a`   |

`Lens(a, b)` is a subtype of `Traversal(a, b)`.

```eu,notest
` { type: "symbol -> Lens(block, any)" }
at(key, k): ...

` { type: "number -> Lens([a], a)" }
ix(n, k): ...

` { type: "Lens(a, b) -> a -> b" }
view(lens, data): ...

` { type: "Lens(a, b) | Traversal(a, b) -> (b -> b) -> a -> a" }
over(lens, fn, data): ...
```

Composition of lenses via `∘` chains optic types:
- `Lens(a, b) ∘ Lens(b, c)` → `Lens(a, c)`
- `Lens(a, b) ∘ Traversal(b, c)` → `Traversal(a, c)`

The checker catches broken compositions:

```eu,notest
# Works: a block → string → number (hypothetical)
path: at(:name) ∘ at(:length)

# Warning: Lens(block, any) ∘ ix(0) — ix expects [a], not any
bad-path: at(:name) ∘ ix(0)
```

---

## Type aliases

For complex or repeated types, define aliases at the top of your file
using unit metadata:

```eu,notest
{ import: "data.eu"
  types: { Person: "{name: string, age: number, email: string | null, ..}"
           Response: "{status: number, body: string | null}" } }

` { type: "[Person] -> [string]" }
names: map(_.name)

` { type: "Response -> bool" }
success?(r): r.status < 400
```

Alternatively, use `type-def:` on a declaration to name the type of
a canonical value:

```eu,notest
` { type-def: "Point" }
origin: { x: 0, y: 0 }
# Defines: Point = {x: number, y: number, ..}
# And: origin : Point

` { type: "Point -> Point -> number" }
distance(a, b): ((b.x - a.x)^2 + (b.y - a.y)^2) abs
```

Combine `type:` and `type-def:` to override inference:

```eu,notest
` { type-def: "Person"
    type: "{name: string, age: number, email: string | null, ..}" }
nobody: { name: "", age: 0, email: null }
```

---

## A worked example

Consider a small data processing pipeline with type annotations:

```eu,notest
{ types: { Record: "{id: number, name: string, active: bool, ..}" } }

` { type: "[Record] -> [Record]" }
active-records: filter(_.active)

` { type: "[Record] -> block" }
index-by-id: group-by(_.id)

` { type: "Record -> string" }
summary(r): "[{r.id}] {r.name}"
```

With these annotations in place, the checker can verify downstream
code:

```eu,notest
# Warning: filter expects (a -> bool), but str.of has type any -> string
bad: records filter(str.of)

# Fine: map over active records, produce strings
ok: records active-records map(summary)
```

---

## Open vs. closed records

An **open record** `{k: T, ..}` says "has at least this key", allowing
additional fields. A **closed record** `{k: T}` says "has exactly this
key".

In practice, open records are more useful because eucalypt blocks
typically have variable shapes:

```eu,notest
# Open: works with any block that has a .name field
` { type: "{name: string, ..} -> string" }
get-name(b): b.name

# Closed: only works with blocks of exactly this shape
` { type: "{name: string}" }
exact-block: { name: "Alice" }
```

Width subtyping means `{x: number, y: number}` is a subtype of
`{x: number, ..}` — a more specific block satisfies a less specific
requirement.

---

## Namespace typing

Functions within namespace blocks (`str`, `arr`, `set`, etc.) are
annotated in the same way:

```eu,notest
str: {
  ` { type: "any -> string" }
  of: __STR

  ` { type: "string -> number" }
  len: __STR_LEN
}
```

The checker synthesises the block's record type from its members, so
`str.len` resolves to `string -> number` via standard record field
lookup.

---

## Summary

| What                     | How                                   |
|--------------------------|---------------------------------------|
| Run the checker          | `eu check file.eu`                    |
| Treat warnings as errors | `eu check file.eu --strict`           |
| Annotate a declaration   | `` ` { type: "number -> number" } ``  |
| Any block                | `block`                               |
| Open record              | `{k: T, ..}`                          |
| Closed record            | `{k: T}`                              |
| IO action                | `IO(T)`                               |
| Lens                     | `Lens(a, b)`                          |
| Traversal                | `Traversal(a, b)`                     |
| Type variable            | lowercase identifier: `a`, `b`, `s`   |
| Union type               | `A \| B`                              |
| Type alias (unit meta)   | `{ types: { Name: "..." } }`          |
| Type alias (declaration) | `` ` { type-def: "Name" } ``          |
