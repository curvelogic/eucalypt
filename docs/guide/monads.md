# Monads and the monad() Utility

Eucalypt has first-class support for monadic programming. Two built-in
namespaces — `io` and `random` — are monads, and the `monad()` utility
lets you build additional ones from just `bind` and `return`.

## What is a monad in eucalypt?

A *monad* is a namespace (block) that provides two primitives:

| Primitive | Role |
|-----------|------|
| `return(v)` | Wrap a pure value as a monadic action |
| `bind(action, f)` | Run an action, pass its result to `f`, return a new action |

Everything else — `map`, `then`, `join`, `sequence`, `map-m`,
`filter-m` — can be derived automatically from those two.

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

---

## Building a monadic namespace

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
operations are not called for their return value alone — they cause
side effects (shell commands, reads, writes). The eucalypt runtime
sequences them strictly.

### Primitives

```eu,notest
io.return(v)             # wrap a pure value; no side effects
io.bind(action, f)       # run action, pass result to f
```

### The { :io ... } monadic block syntax

Eucalypt provides syntactic sugar for chaining IO actions. A block
tagged `:io` is desugared into nested `io.bind` calls automatically:

```eu,notest
{ :io
  r: io.shell("ls -la")
  _: io.check(r)
}.r.stdout
```

desugars to:

```eu,notest
io.bind(io.shell("ls -la"),
  λr. io.bind(io.check(r),
    λ_. io.return(r.stdout)))
```

The `.expr` after the closing brace is the return expression, wrapped
in `io.return`. Each field in the block becomes a `bind` step; the
bound name is the lambda parameter for all subsequent steps.

**IO operations require `--allow-io` / `-I` at the command line.**
Test targets that use IO should include `requires-io: true` in their
target metadata so the test runner skips them gracefully in
environments without IO permission.

### Practical IO example

```eu,notest
result: { :io
  r: io.shell("git rev-parse HEAD")
  _: io.check(r)
}.(r.stdout)
```

### Using monad() with the IO monad

You can derive the full set of combinators for the IO monad:

```eu,notest
` :suppress
io-m: monad{bind: io.bind, return: io.return}

# sequence two IO actions and collect results
both: io-m.sequence([io.shell("echo a"), io.shell("echo b")])
```

Or extend the IO monad with domain-specific operations via catenation:

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

See [IO reference](../reference/prelude/io.md) for the full API
including `io.shell`, `io.exec`, and related operations.

---

## The random state monad

The `random` namespace implements a *state monad* over a PRNG stream.
Each operation is a *function* from stream to a result-plus-new-stream
block — the state threads through automatically via `bind`.

### Without the monad: manual threading

```eu,notest
# Thread the stream manually through each step
r1: random.int(6)(io.random)
r2: random.int(6)(r1.rest)
total: r1.value + r2.value + 2
```

Every call must pass the `.rest` from the previous call. This is
error-prone and verbose.

### With the monad: automatic threading

```eu,notest
# random.sequence handles the threading
two-dice: random.sequence([random.int(6), random.int(6)])
result: two-dice(random.stream(42)).value  # list of two rolls
```

### Running an action

An action is a function from stream to `{value, rest}`. Call it with
`random.stream(seed)` to run it:

```eu,notest
roll: random.int(6)(random.stream(42)).value   # integer in [0,6)
```

Only extract `.value` — rendering the full result block would try to
print the infinite `.rest` stream.

### Chaining with map-m

Roll five dice of different sizes:

```eu,notest
dice: random.map-m(random.int, [4, 6, 8, 10, 20])
rolls: dice(random.stream(42)).value   # e.g. [2, 4, 1, 7, 15]
```

### Implementing the state monad pattern

The state monad return and bind are:

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
| `random.stream(seed)` | Create a PRNG stream from an integer seed |
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
- The `{ :io ... }` syntax is syntactic sugar for nested `io.bind`
  calls; it is currently IO-specific
- The `random:` namespace is a state monad — actions are functions of a
  stream, `bind` threads the stream automatically
- When running random actions, always extract `.value` before
  rendering; the `.rest` field is an infinite stream
