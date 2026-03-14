# Monads and the monad() Utility

Eucalypt has first-class support for monadic programming. Two built-in
namespaces — `io` and `random` — are monads, and the `monad()` utility
lets you build additional ones from just `bind` and `return`.

## What is a monad in eucalypt?

A *monad* is just two primitives, which are conventionally grouped in
a namespace.

| Primitive | Role |
|-----------|------|
| `return(v)` | Wrap a pure value as a monadic action |
| `bind(action, f)` | Run an action, pass its result to `f`, return a new action |

With these two primitives available an alternative interpretation of the block
structure, _monadic blocks_ can be used.

Other monadic functions — `map`, `then`, `join`, `sequence`, `map-m`,
`filter-m` — are typically provided in the same namespace and can be
derived automatically with the aid of the `monad` function.

Monads are required for performing [IO from eucalypt code](io.md) and
may be used to simplify random number access. Users may define monads
themselves and assign unicode brackets to them if they so wish.

---

## Monadic blocks

Eucalypt provides syntactic sugar for chaining monadic actions. A
block tagged with a monad namespace name desugars into nested `bind`
calls automatically.

The most common form tags the block with `:io`:

```eu,notest
{ :io
  r: io.shell("echo hello")
  _: io.check(r)
}.(r.stdout)
```

This desugars to:

```eu,notest
io.bind(io.shell("echo hello"),
  λr. io.bind(io.check(r),
    λ_. io.return(r.stdout)))
```

Each field becomes a `bind` step. The bound name is available in all
subsequent steps. The `.()` expression after the closing brace is the
return expression, wrapped in the monad's `return`.

### Key constraint: sequential binding

**Unlike normal blocks, names in a monadic block can only refer to
names bound in earlier steps.** Normal eucalypt blocks are
declarative — bindings can refer to each other in any order. Monadic
blocks are sequential — each step can only see what came before it,
because the desugaring nests each continuation inside the previous
one.

```eu,notest
# WRONG — b is not yet bound when a is evaluated
{ :io
  a: io.map(inc, b)
  b: io.shell("echo 1")
}.(a)

# RIGHT — b is bound before a uses it
{ :io
  b: io.shell("echo 1")
  a: io.map(inc, b)
}.(a)
```

### Block metadata forms

Several syntax forms are available for monadic blocks:

| Form | Syntax | Monad source |
|------|--------|-------------|
| 1 | `{ :name decls }.expr` | Namespace `name` in scope |
| 2 | `{ { monad: name } decls }.expr` | Namespace `name` in scope |
| 3 | `{ { :monad namespace: name } decls }.expr` | Namespace `name` in scope |
| 4 | `{ { :monad bind: f return: r } decls }.expr` | Explicit `f`/`r` functions |

Form 1 is the most common — `{ :io ... }` tags a block with the `io`
namespace. The desugarer looks up `io.bind` and `io.return`
automatically.

### Custom bracket pairs

You can define bracket pairs for monadic notation using the `:monad`
metadata:

```eu,notest
⟦{}⟧: { :monad bind: my-bind  return: my-return }

result: ⟦ x: some-action  y: other-action(x) ⟧.(x + y)
```

See the [syntax reference](../reference/syntax.md) for full details on
bracket pair definitions.

---

## The monad() utility

`monad(m)` takes a block with `bind` and `return` fields and returns a
block of derived combinators:

```eu,notest
my-monad: monad{bind: my-bind, return: my-return}
```

The returned block provides:

| Combinator | Description |
|-----------|-------------|
| `bind(action, f)` | Passed through from `m.bind` |
| `return(v)` | Passed through from `m.return` |
| `map(f, action)` | Apply pure function `f` to the result of an action (fmap) |
| `then(a, b)` | Sequence two actions, discarding the result of the first |
| `join(mm)` | Flatten a nested monadic value |
| `sequence(ms)` | Run a list of actions in order, collecting results |
| `map-m(f, xs)` | Apply `f` to each element of `xs`, then sequence |
| `filter-m(p, xs)` | Monadic filter: keep elements where `p` returns a truthy action |

### Building a monadic namespace

The typical pattern is to use `monad()` to produce the derived
operations and then **catenate** (merge) domain-specific operations on
top:

```eu,notest
my-ns: monad{bind: my-bind, return: my-return} {
  some-extra-op(x): ...
}
```

Since `monad()` includes `bind` and `return` in its result, there is
no need to repeat them in the right-hand block.

**Why catenation (not `<<`)?**  Catenation is shallow merge — the
right-hand block's fields override the left-hand block's fields at the
top level. `<<` is deep merge and is not appropriate here because the
derived combinators are not nested blocks.

**Overriding a derived combinator** — put the specialised
implementation in the right-hand block; it shadows the derived one:

```eu,notest
my-ns: monad{bind: my-bind, return: my-return} {
  # override map with a more efficient implementation
  map(f, action): my-efficient-map(f, action)
}
```

---

## The IO monad

The `io` namespace is a monad built around effect execution. IO
operations cause side effects (shell commands, reads, writes). The
eucalypt runtime sequences them strictly.

**IO operations require `--allow-io` / `-I` at the command line.**

### Primitives

```eu,notest
io.return(v)             # wrap a pure value; no side effects
io.bind(action, f)       # run action, pass result to f
```

### Practical example

```eu,notest
result: { :io
  r: io.shell("git rev-parse HEAD")
  _: io.check(r)
}.(r.stdout)
```

### Extending the IO monad

You can derive additional combinators or extend with domain-specific
operations via catenation:

```eu,notest
app-io: monad{bind: io.bind, return: io.return} {
  read-file(path): io.shell("cat {path}")
  git-hash: io.shell("git rev-parse HEAD")
}
```

### IO combinators reference

| Function | Description |
|----------|-------------|
| `io.return(a)` | Wrap a pure value in the IO monad |
| `io.bind(action, f)` | Sequence two IO actions |
| `io.map(f, action)` | Apply pure function `f` to the result of an IO action |
| `io.check(result)` | Fail if `exit-code` is non-zero; otherwise return result |

See [IO and Shell Commands](io.md) for practical usage and the
[IO reference](../reference/prelude/io.md) for the full API.

---

## The random state monad

The `random` namespace implements a *state monad* over a PRNG stream.
A random stream is provided at startup as `io.random` (seeded from
the system or from `--seed`). Each random operation consumes part of
the stream and returns a `{value, rest}` block — `value` is the
result and `rest` is the remaining stream for subsequent operations.

### The pitfall: forgetting to propagate the stream

For a single random value, passing `io.random` directly is simple:

```eu,notest
roll: random.int(6, io.random).value + 1
```

But when you need multiple random values, you must thread the `.rest`
stream from each call into the next:

```eu,notest
r1: random.int(6, io.random)
r2: random.int(6, r1.rest)       # must use r1.rest, not io.random!
total: r1.value + r2.value + 2
```

If you accidentally pass `io.random` to both calls, you get the
*same* random value twice. This manual threading is error-prone and
verbose — which is exactly what the random monad solves.

### With the monad: automatic threading

A `{ :random ... }` monadic block threads the stream automatically:

```eu,notest
triple: { :random
  d6: random.int(6)
  d20: random.int(20)
  d100: random.int(100)
}.[d6, d20, d100]

result: triple(io.random).value  # e.g. [3, 14, 57]
```

Each step receives the stream left over from the previous step —
no manual `.rest` threading needed. The return expression
(`.[d6, d20, d100]`) collects the results into a list.

The monadic block produces an *action* — a function that takes a
stream — so you run it by passing `io.random` and extracting
`.value`.

For common patterns, `sequence` and `map-m` are even more concise:

```eu,notest
# Sequence a list of actions
two-dice: random.sequence[random.int(6), random.int(6)]
result: two-dice(io.random).value  # list of two rolls

# Map an action over a list of die sizes
dice: random.map-m(random.int, [4, 6, 8, 10, 20])
rolls: dice(io.random).value   # e.g. [2, 4, 1, 7, 15]
```

**Important:** always extract `.value` before rendering — the `.rest`
field is an infinite stream and will hang if you try to print it.

### Deterministic seeds

For reproducible output (useful in tests), pass `--seed` on the
command line or create a stream from a fixed seed:

```eu,notest
rolls: random.map-m(random.int, [6, 6])(random.stream(42)).value
# always produces the same result for seed 42
```

### How the state monad works

Under the hood, `random.return` and `random.bind` are:

```eu,notest
# return: wrap a pure value as a do-nothing action
random-ret(v, stream): { value: v, rest: stream }

# bind: run m, pass result to f, run f's action with remaining stream
random-bind(m, f, stream): {
  r: m(stream)
  run: f(r.value)(r.rest)
  value: run.value
  rest: run.rest
}
```

`random-bind(m, f)` is a 3-arg function; calling it with only two
arguments returns an action (function of `stream`). This is the curried
partial application pattern used throughout eucalypt.

### Random combinators reference

| Function | Description |
|----------|-------------|
| `random.bind(m, f)` | State monad bind |
| `random.return(v)` | State monad return |
| `random.float` | Action: random float in [0,1) |
| `random.int(n)` | Action: random integer in [0,n) |
| `random.choice(list)` | Action: random element from list |
| `random.shuffle(list)` | Action: shuffled copy of list |
| `random.sample(n, list)` | Action: n elements sampled without replacement |

Derived combinators (`map`, `then`, `join`, `sequence`, `map-m`,
`filter-m`) are also available — see
[Random Numbers reference](../reference/prelude/random.md).

---

## Writing your own monad

Any pair of functions satisfying the monad laws can be wrapped with
`monad()`. Here is a minimal example — the identity monad, where
"wrapping" a value is a no-op:

```eu
` :suppress
id-bind(m, f): f(m)
` :suppress
id-return(a): a
` :suppress
id-monad: monad{bind: id-bind, return: id-return}

` { target: :example }
example: id-monad.map-m(inc, [1, 2, 3])
# => [2, 3, 4]
```

A more useful example — a "maybe" monad where `null` short-circuits
the computation:

```eu,notest
` :suppress
maybe-return(v): v

` :suppress
maybe-bind(m, f): if(m = null, null, f(m))

` :suppress
maybe: monad{bind: maybe-bind, return: maybe-return}

safe-head(xs): if(xs nil?, null, xs head)

# Chain operations that may return null — short-circuits on first null
result: maybe.bind(safe-head([]), inc)   # => null (list was empty)
ok:     maybe.bind(safe-head([42]), inc) # => 43
```

---

## Key concepts

- `monad(m)` takes `{bind, return}` and returns a block with `bind`,
  `return`, and six derived combinators
- Use **catenation** (juxtaposition) to merge derived and specialised
  operations — NOT `<<`
- Monadic blocks (`{ :io ... }`, `{ :name ... }`) desugar into nested
  `bind` calls — names are bound **sequentially**, not declaratively
- The `random:` namespace is a state monad — actions are functions of a
  stream, `bind` threads the stream automatically
- When running random actions, always extract `.value` before
  rendering; the `.rest` field is an infinite stream
