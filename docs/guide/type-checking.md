# Type Checking

Eucalypt includes an advisory type checker that runs **unconditionally**
on every `eu` invocation (evaluate, dump, test). It never prevents your
code from running by default — all existing code continues to work
exactly as before. Think of it as a second pair of eyes that can catch
common mistakes before they become confusing runtime errors.

```sh
eu check file.eu              # check-only: report warnings, don't evaluate
eu check file.eu --strict     # treat type warnings as errors (exit 1)
eu file.eu --strict           # evaluate, but abort first if there are warnings
eu file.eu --suppress-type-warnings  # evaluate, silencing warning output
```

Type issues are reported as **warnings**, not errors, and only abort
evaluation when `--strict` is passed. `--type-check` is a deprecated
no-op flag kept for backwards compatibility — type checking already
runs whether or not it is passed.

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

### Literal types

A **literal type** represents exactly one value. Literal types are
subtypes of their corresponding primitive:

| Type syntax     | Meaning                           | Subtype of |
|-----------------|-----------------------------------|------------|
| `"value"`       | the specific string `"value"`     | `string`   |
| `:name`         | the specific symbol `:name`       | `symbol`   |

Literal types are most useful in **discriminated unions** — type aliases
where a tag field distinguishes variants:

```eu,notest
{ types: { Shape: c"{{type: \"circle\", radius: number, ..}} | {{type: \"rect\", w: number, h: number, ..}}" } }
```

Because writing complex union types inline is verbose, defining a `types:`
alias is the recommended approach. (Note the `c"..."` **c-string** — see
*Writing literal types in annotation strings* below.)

**Subtyping rules**:
- `"foo"` is consistent with `string` — a literal satisfies a string parameter
- `"foo" | string` simplifies to `string` — the specific literal is absorbed
- `:active` is consistent with `symbol` — same for symbols

**Writing literal types in annotation strings**: a literal string type is
written `"value"`, so its quotes need escaping inside the eucalypt
annotation string — use **c-string escapes**: `c"\"value\""`. (A plain
`"..."` string does not process `\"`.) Literal *symbol* types (`:name`)
have no quotes, so need no escaping.

Mind the precedence of `|` and `->`: as in most type languages `->` binds
tighter, so a union in *argument* position needs brackets.
`"circle" | "rect" -> bool` is the overload `"circle" | ("rect" -> bool)`
(see *Unions* below); the one-argument reading is
`("circle" | "rect") -> bool`.

```eu,notest
# Literal string union as an argument: c-string escapes, bracketed union.
` { type: c"(\"circle\" | \"rect\") -> bool" }
is-shape-name(s): s = "circle" or s = "rect"

# Literal symbol union: no escaping needed, still bracketed.
` { type: "(:ok | :err) -> bool" }
ok?(status): status = :ok
```

Most often you name the union in a `types:` alias, which avoids the
bracketing at the use site — the alias name is a single token:

```eu,notest
{ types: { Status: c"\"ok\" | \"err\"" } }

` { type: "Status -> bool" }
ok?(s): s = "ok"
```

### Lists

`[T]` is a homogeneous list of `T`:

```eu,notest
` { type: "[number] -> [string]" }
to-strings: map(str.of)

` { type: "[a] -> a" }
first: head
```

`NonEmpty([T])` is a non-empty list guaranteed to have at least one element.
`head` and `tail` are safe on `NonEmpty([T])` without type warnings.

#### List literal synthesis

The checker assigns precise types to list literals:

| Literal         | Synthesised type      | Notes                                  |
|-----------------|-----------------------|----------------------------------------|
| `[]`            | `List(Never)`         | Empty — `head` on this warns           |
| `[42]`          | `Tuple([number])`     | 1–16 elements → Tuple with element types |
| `[42, "hello"]` | `Tuple([number, string])` | Heterogeneous; `head` gives `number` |
| `[e1..e17, ...]`| `NonEmpty([T])`       | >16 elements → non-empty homogeneous   |

`cons(h, t)` and `h ‖ t` both return `NonEmpty([a])`.

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

`head` on a `Tuple([A, B, ...])` returns the precise first-element type `A`
(not `A | B | ...`). `tail` returns `Tuple([B, ...])`:

```eu,notest
[42, "hello"] head       # type: number
[42, "hello"] tail head  # type: string
```

The named pair accessors `first`/`key` (index 0) and `second`/`value` (index 1)
are also precise on `Tuple` — the checker recognises their `head ∘ tail^n`
composition as a **projection** and returns the exact element type:

```eu,notest
pair: ["alice", 42]
pair key    # type: string  (index 0)
pair value  # type: number  (index 1)
second(pair) # type: number  (index 1)
```

Any user-defined function whose body is `xs tail head` (or `xs tail tail head`,
etc.) acquires the same precise projection typing automatically.

### Records

Records describe the shape of blocks:

| Syntax           | Meaning                                             |
|------------------|-----------------------------------------------------|
| `{k: T}`         | closed record — exactly the key `k` of type `T`    |
| `{k: T, ..}`     | open record — at least `k: T`, may have more        |
| `{k: T, ..r}`    | named row variable — `r` captures the extra fields  |
| `{..r, ..s}`     | row concatenation — union of row `r` and row `s`    |
| `block`          | any block (no known shape)                          |

```eu,notest
` { type: "{{name: string, age: number, ..}} -> string" }
greet-person(p): "Hello, {p.name}!"
```

Open records are more commonly useful — most block-processing functions
don't require a specific shape.

#### `lookup` with a literal key

When `lookup(:key, block)` is called with a **literal symbol** key, the checker
resolves the field type precisely:

```eu,notest
` { type: "!{{name: string, age: number}}" }
person: { name: "Alice", age: 30 }

lookup(:name, person)   # type: string  (precise)
lookup(:age, person)    # type: number  (precise)
lookup(:naem, person)   # warning: unknown record key :naem (key typo in closed record)
```

- **Closed record + known key** → exact field type.
- **Closed record + absent key** → `any` + a static warning (key typo caught before runtime).
- **Open record + absent key** → `any`, no warning (the key may be present at runtime).
- **`Dict(V)` + any literal key** → `V`.
- **Non-literal key** → `any` (key value unknown at check time; may fail at runtime).

### Dict

`Dict(T)` represents a homogeneous block — a block where every value has
the same type `T`. Use this when you know what the values are but not what
the keys are:

```eu,notest
` { type: "Dict(number)" }
scores: { alice: 95 bob: 87 carol: 92 }
```

Key prelude functions are annotated with `Dict` types:

| Function     | Type                              |
|--------------|-----------------------------------|
| `map-values` | `(a -> b) -> Dict(a) -> Dict(b)`  |
| `values`     | `Dict(a) -> [a]`                  |
| `keys`       | `Dict(a) -> [symbol]`             |
| `group-by`   | `(a -> any) -> [a] -> Dict([a])`  |

A closed record is a subtype of the corresponding `Dict` type when all
its field values share a common type. An open or annotated-Dict record is
consistent with `Dict(T)` for gradual typing purposes.

### Row polymorphism

**Named row variables** allow functions that preserve the shape of records
they do not inspect. The `merge` function is annotated:

```eu,notest
` { type: "{{..r}} -> {{..s}} -> {{..r, ..s}}" }
merge: __MERGE
```

Here `r` and `s` are row variables that capture the fields from each
input. The result contains both sets of fields. In a type annotation
string, `{{` and `}}` are literal braces (eucalypt string escaping);
the parser sees `{..r}` and `{..r, ..s}`.

Row variables propagate through function applications: if a function
with a named row in its type is applied to a concrete record, the
checker unifies the known fields and binds the row variable to the
remaining fields.

#### Inference for unannotated block combinators (B9)

When a lambda parameter is **used as a block** — projected (`.field`),
merged (`merge`/`over`), or passed to a block-typed function — the
checker allocates a fresh row variable for it automatically, even
without an explicit `type:` annotation.

```eu,notest
# No annotation needed — f infers {..r} -> {..s} -> {..r, ..s}
f(a, b): a merge(b)

base: { x: 1 }
ext: { y: "hello" }
result: base f(ext)   # result : {x: number, y: string}
```

The checker uses a **use-driven** approach: only parameters that appear
in a block position acquire row variables. Parameters used arithmetically
(`x + y`) stay `any`, avoiding spurious warnings.

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

### Recursive types

A **recursive alias** is an alias whose definition refers back to itself.
The type checker resolves it to an equirecursive `μ`-type automatically —
no special syntax is needed. Define recursive aliases with the `types:`
metadata key:

```eu,notest
{ types: { Json: "number | string | bool | null | [Json] | Dict(Json)" } }

` { type: "Json → string" }
describe: str.from
```

`Json` unfolds to `number | string | bool | null | [Json] | Dict(Json)`,
and any value whose type is a subtype of any variant passes without
warning.  The type always *displays* as its alias name (`Json`) — never
as the (infinite) unfolded form.

**Unit metadata** (the bare block at the top of a file, no `` ` ``) is
the natural place for type aliases shared across a whole file. Declaration
metadata (with `` ` ``) can also carry a `types:` block to scope aliases
to a single binding:

```eu,notest
` { types: { Tree: "{{value: number, children: [Tree]}}" } }
root: { value: 1, children: [] }
```

**Mutual recursion** — aliases that reference each other are handled:
the first alias expanded in an annotation becomes the outer `μ` binder.

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

### Higher-kinded type variables

Type variables can range over *type constructors* (types that take
arguments), not just ordinary types.  A **kind** describes the arity
of a type constructor:

- `*` — an ordinary type (e.g. `number`, `[string]`)
- `* -> *` — a unary type constructor (e.g. `List`, `IO`)
- `* -> * -> *` — a binary type constructor (e.g. `Lens`, `Dict`)

Declare a constructor variable with a kind annotation using `::`:

```eu,notest
` { type: "forall (m :: * -> *) a. m a -> m a" }
hk-id: ...
```

Without a kind annotation, type variables default to kind `*` (ordinary
types).  The `forall` keyword makes quantification explicit and allows
kind annotations.  Implicit quantification (writing `a` directly in
the type string) still works for kind-`*` variables.

**Constructor application** uses juxtaposition:

| Annotation          | Meaning                                   |
|---------------------|-------------------------------------------|
| `m a`               | Apply constructor variable `m` to type `a` |
| `forall (m :: * -> *) a. m a -> m a` | Identity for any unary functor |
| `forall a. a -> a`  | Polymorphic identity                       |

The built-in constructors `List`, `IO`, `Dict`, `NonEmpty`, `Random`,
and `State` all have kind `* -> *`.  `Lens` and `Traversal` have kind
`* -> * -> *`.

### Special types

| Type             | Meaning                                               |
|------------------|-------------------------------------------------------|
| `any`            | gradual/dynamic — no errors involving `any`           |
| `top`            | supertype of everything — nothing useful can be done with it |
| `never`          | bottom type — unreachable code, empty collections     |
| `ExecutionError` | the type of a raised runtime error (see Partial types below) |

### Partial types

Some functions can fail at runtime — for example `nth` raises an error
when the index is out of range, and `parse-as` raises an error when the
string is not valid in the given format.  These are called **partial
functions**.

Partial functions use the postfix `?` sugar in their type signatures:

```
nth      : number → [a] → a?         # may raise an error if index out of range
parse-as : symbol → string → any?    # may raise an error if string is not valid
lookup   : symbol → Dict(a) → a?     # may raise an error if key not found
```

`T?` is display sugar for `T | ExecutionError`.  The two forms are
completely interchangeable in type annotations:

```eu,notest
` { type: "number → [a] → a?" }
safe-nth(n, l): l nth(n)       # explicitly partial

` { type: "number → [a] → a | ExecutionError" }
also-safe-nth(n, l): l nth(n)  # identical — same type, different spelling
```

**Gradual self-limiting**: a partial result flowing into unannotated
(`any`) code is **silent** — `ExecutionError <: any`.  A warning fires
only when a partial result is used in a position that has been explicitly
annotated as total:

```eu,notest
` { type: "number" }
result: [1, 2, 3] nth(0)   # warning: expected number, found number?
```

Functions that A6 made total by input refinement (`head`/`tail` require
`NonEmpty([a])`) are **not** partial — they keep their total types.
`T?` documents the *residual* partiality that refinement cannot remove.

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

## Monadic block binding types

When `eu check` encounters a monadic block, it uses the monad's element
type to infer the types of bound variables. Each binding in a `{ :ns
... }` block should be an action in that monad; the checker unwraps the
monad layer and gives the bound name the result type.

### The list monad: `{ :for ... }`

The `for` namespace is the list monad. Binding a name draws elements
from a list, so the bound variable gets the list's element type:

```eu,notest
{ :for x: [1, 2, 3] }.(x * 2)   # x : number  ✓
{ :for s: ["a", "b"] }.(s)       # s : string  ✓
```

If you bind a non-list, `eu check` warns:

```eu,notest
{ :for x: 42 }.(x)   # warning: binding in :for block must be [a], got number
```

### The IO monad: `{ :io ... }`

The `io` namespace is typed with `IO(a)`. Each binding must be an IO
action; the bound variable gets the inner type:

```eu,notest
{ :io
  r: io.shell("echo hello")   # r : block  (IO(block) unwrapped)
  _: io.check(r)
}.(r.stdout)                  # stdout lookup on block  ✓
```

The most common mistake the checker catches is binding a plain value
instead of an IO action:

```eu,notest
{ :io
  cmd: 42                     # warning: expected IO(a), got number
}.(cmd)
```

### The state monad: `{ :random ... }` and `{ :state ... }`

State monads wrap actions as functions of a state value. The checker
validates that each binding is a valid monadic action for that namespace.
Bound variables receive the result type (the `value` component):

```eu,notest
{ :random
  d6:  random.int(6)     # d6  : number
  d20: random.int(20)    # d20 : number
}.[d6, d20]
```

### The identity monad: `{ :let ... }`

`{ :let ... }` blocks use the identity monad — every binding is a plain
value, so bound variables keep their inferred types directly:

```eu,notest
{ :let
  x: 1 + 1               # x : number
  s: str.of(x)           # s : string
}.(x + 1)                # number + number  ✓
```

### User-defined monads: `monad()`

`monad({ bind(m, f): ..., return(v): ... })` derives the standard
monad combinators (`map`, `then`, `and-then`, `join`, `sequence`,
`map-m`, `filter-m`) and annotates them with higher-kinded types using
`forall (m :: * -> *)`.

When `monad()` is called with a concrete `bind` and `return`, the type
checker instantiates `m :: * → *` as a fresh higher-kinded variable and
runs higher-order pattern unification: it unifies `m a` against the
concrete first-argument type of `bind`.  For named constructors such as
`List` (`[a]`) this decomposes via the first-order rule; for anonymous
type constructors (e.g. `stream → {value: a, rest: stream}`) the
Miller pattern fragment fires and binds `m` to a type-level lambda.
No annotation is required — the binding is inferred entirely from the
`bind` implementation.

For example, a list monad:

```eu,notest
my-for: monad({bind(m, f): m mapcat(f), return(v): [v]})
```

The checker unifies `m a` against `[a]`, infers `m = List`, and gives
`my-for.map` type `forall a b. (a → b) → [a] → [b]`.  Passing a
non-function triggers a type warning:

```eu,notest
[1, 2, 3] my-for.map(true)   # warning: expected a → b, found bool
```

#### How combinator types are inferred

Monad combinator types are inferred automatically via higher-order
pattern unification — no `monad:` annotation is needed for type
checking.  When `monad()` is called with a concrete `bind`
implementation, the checker unifies the `bind` argument type against
the polymorphic `m a → (a → m b) → m b` signature.  If `m` is applied
to a variable (the Miller pattern fragment), the unifier constructs a
type-level lambda: for example, a `bind` whose first argument has type
`stream → {value: a, rest: stream}` causes the checker to infer
`m = λa. stream → {value: a, rest: stream}` and propagate that
instantiation through all nine derived combinators.

Named constructors (`List`, `IO`) are handled by the existing
first-order rule: `m a` against `[a]` decomposes as `m = List`.

No annotation is required:

```eu,notest
my-for: monad({bind(m, f): m mapcat(f), return(v): [v]})
```

`my-for.bind` is inferred as `[a] → (a → [b]) → [b]`, `my-for.map`
as `(a → b) → [a] → [b]`, and so on.

#### `monad:` metadata for LSP hints

The `monad:` metadata key is **not** used for type checking.  It is
retained solely for A10 LSP element-type hints: when the language
server displays inlay hints inside a monadic block, `monad:` tells it
which element type to show next to each bound variable (e.g.
`x: number` instead of `x: [number]`).

```eu,notest
` { monad: "[a]" }
my-for: monad({bind(m, f): m mapcat(f), return(v): [v]})
```

Omitting `monad:` does not affect type checking; it only suppresses
the LSP element-type inlay hints.

### What the checker validates

`eu check` reports a warning when:

- A binding's RHS type is inconsistent with the monad's expected action
  type (e.g. `number` in an `:io` block instead of `IO(a)`)
- A bound variable is used in a way inconsistent with its unwrapped type
  (e.g. calling `.stdout` on a `number`)

The checker does **not** require annotations — it infers binding types
from the monad's known type signature and the RHS expression type.

When using an LSP-enabled editor, inlay hints display the inferred
element type next to each bound variable name, e.g. `x: number` inside
a `{ :for x: [...] }` block.

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
  types: { Person: "{{name: string, age: number, email: string | null, ..}}"
           Response: "{{status: number, body: string | null}}" } }

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
    type: "{{name: string, age: number, email: string | null, ..}}" }
nobody: { name: "", age: 0, email: null }
```

---

## A worked example

Consider a small data processing pipeline with type annotations:

```eu,notest
{ types: { Record: "{{id: number, name: string, active: bool, ..}}" } }

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
` { type: "{{name: string, ..}} -> string" }
get-name(b): b.name

# Closed: only works with blocks of exactly this shape
` { type: "{{name: string}}" }
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

## Flow-sensitive narrowing

After a recognised branch on a type predicate, the checker narrows the
tested variable's type within each branch:

```eu,notest
# x : number | string | null
# In the true branch, x is narrowed to `null`.
# In the false branch, x is narrowed to `number | string` (null subtracted).
` { type: "(number | string | null) -> string" }
describe(x): if(x null?, "nothing", if(x string?, x, str(x)))
```

Recognised predicates: `number?`, `string?`, `symbol?`, `bool?`, `list?`,
`block?`, `nil?`, `null?`, `not-nil?`.

`nil?` narrows a `[T]` to `NonEmpty([T])` in the **false** branch (the
list is non-empty), enabling `head` without a type warning:

```eu,notest
` { type: "[number] -> number" }
safe-head(xs): if(xs nil?, 0, xs head)   # no warning — xs is NonEmpty([number]) in false branch
```

Recognised branchers: `if` (three-argument), `and`, `or`, `cond`.
A user-defined brancher that aliases or wraps one of these inherits its
narrowing — but only if the prelude's `if`/`and`/`or`/`cond` is used,
not a local rebinding.

## `cond` — multi-way conditional

`cond` dispatches a list of `condition => result` clauses, returning the
first result whose condition is true. A trailing bare expression is the
default:

```eu,notest
classify(n): cond[n < 0 => "negative", n > 100 => "huge", "normal"]
```

The `=>` operator (precedence 15, right-associative) builds a clause pair.
The Unicode alias `⇒` is equivalent. Both forms are recognised by the
checker for flow-sensitive narrowing through each clause.

---

## Prelude type cache

The checker builds the prelude's type information once per process and
caches it.  Every subsequent file is checked in **standalone mode** —
only the user file is loaded, and the cached prelude summary (binding
types, type aliases, branch-shape classification) is seeded into the
checker before it begins.

This means:

- The prelude is **fully checked** on the first call — every prelude
  binding is a root in standalone mode, unlike the merged-then-pruned
  path which only ever checks the subset reachable from the user file.
- Each subsequent LSP re-check or `eu check` invocation **skips the
  ~2 200-line prelude entirely**, giving a measurably faster response.
- Results are identical to the merged check: prelude references resolve
  to the same types, predicate narrowing fires on `number?`/`string?`/
  etc., and user-defined brancher wrappers around prelude combinators
  are recognised.

The cache is keyed on the embedded prelude source and lives in process
memory — it persists for the lifetime of the `eu` or LSP server process
but is not written to disk.

---

## Summary

| What                     | How                                              |
|--------------------------|--------------------------------------------------|
| Run the checker          | `eu check file.eu`                               |
| Treat warnings as errors | `eu check file.eu --strict`                      |
| Annotate a declaration   | `` ` { type: "number -> number" } ``             |
| Any block                | `block`                                          |
| Open record              | `{k: T, ..}`                                     |
| Closed record            | `{k: T}`                                         |
| Named row variable       | `{k: T, ..r}` (`{{k: T, ..r}}` in string)       |
| Row concatenation        | `{..r, ..s}` (`{{..r, ..s}}` in string)          |
| Homogeneous dict         | `Dict(T)`                                        |
| IO action                | `IO(T)`                                          |
| Lens                     | `Lens(a, b)`                                     |
| Traversal                | `Traversal(a, b)`                                |
| Literal string type      | `"value"` — c-string escapes: `c"\"value\""`     |
| Literal symbol type      | `:name`                                           |
| Type variable            | lowercase identifier: `a`, `b`, `s`              |
| Explicit quantification  | `forall a. T` or `forall (m :: * -> *) a. T`    |
| Constructor variable     | `m :: * -> *` in a `forall` binder               |
| Constructor application  | `m a` (juxtaposition in type expressions)        |
| Union type               | `A \| B`                              |
| NonEmpty list            | `NonEmpty([a])`                       |
| Empty list literal       | `[]` synthesises as `List(Never)`     |
| Short list literal       | `[a, b]` synthesises as `Tuple([A, B])` (1–16 elements) |
| `nil?` false-branch      | `[T]` narrowed to `NonEmpty([T])`     |
| Type alias (unit meta)   | `{ types: { Name: "..." } }`          |
| Type alias (declaration) | `` ` { type-def: "Name" } ``          |
| Cond clause              | `condition => result`                 |
| Multi-way conditional    | `cond[c1 => r1, c2 => r2, default]`   |
