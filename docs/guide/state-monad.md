# The State Monad

Eucalypt provides an optional state monad library for threading
block-valued state through a sequence of operations. Import it with:

```eu,notest
{ import: "state.eu" }
```

## The Problem

Eucalypt blocks are immutable. To "update" a value deep in a
structure, you must reconstruct the entire surrounding structure.
When several updates depend on each other, manually threading the
modified state through each step is verbose and error-prone — the
same problem that `{ :random ... }` solves for PRNG streams.

```eu
s0: { count: 0, name: "init" }
s1: s0 merge({count: 1})
s2: s1 merge({name: "updated"})
```

```yaml
s0:
  count: 0
  name: init
s1:
  count: 1
  name: init
s2:
  count: 1
  name: updated
```

The state monad automates this threading.

## State Actions

A *state action* is a function from a state block to a block with
`value` and `state` fields — the same pattern as the random monad's
`value`/`rest` blocks:

```eu,notest
# An action that reads the count and increments it
count-and-inc(s): { value: s.count, state: s merge({count: s.count + 1}) }
```

The state monad provides primitives for building these actions
without manually deconstructing and reconstructing blocks.

## Monadic Blocks with `{ :state ... }`

Tag a block with `:state` to get automatic bind threading — each
declaration becomes a monadic step:

```eu
{ import: "state.eu" }

action: { :state
  n: state.query(_.count)
  _: state.put(:count, n + 1)
  _: state.put(:name, "step-2")
}

result: state.exec(action, {count: 0, name: "init"})
```

```yaml
result:
  count: 1
  name: step-2
```

Each step sees the state left by the previous step:

- `state.get` returns the whole state as the value — then `.count`
  navigates it
- `state.put(:count, n + 1)` sets a key in the state
- The implicit return synthesises `{n: n}` from the non-underscore
  bindings (or use `.(expr)` for an explicit return)

Run the action by passing an initial state to `state.run`,
`state.eval` (value only), or `state.exec` (final state only).

## Primitives

| Function | Description |
|----------|-------------|
| `state.get` | Action: return the whole state as the value |
| `state.put(k, v)` | Action: set key `k` to value `v` |
| `state.lift(f)` | Action: apply `f` to transform the state, return null |
| `state.query(f)` | Action: apply `f` to read from the state, state unchanged |
| `state.modify(k, f)` | Action: update key `k` by applying `f` to its value |

### Running Actions

| Function | Description |
|----------|-------------|
| `state.run(action, s)` | Run action from state `s`, return block with `value` and `state` fields |
| `state.eval(action, s)` | Run action, return only the value |
| `state.exec(action, s)` | Run action, return only the final state |

## Using Existing Pipeline Functions

The `lift` and `query` primitives let you reuse existing eucalypt
functions as state actions:

```eu
{ import: "state.eu" }

action: { :state
  _: :count %! (+ 1)
  n: state.query(_.count)
}.(n)

result: state.eval(action, {count: 0})
```

```yaml
result: 1
```

With the lens library imported, you can also use `state.lift` with
lens functions like `over(at(:key), f)` for deep updates. Since
`state.eu` imports `lens.eu` internally, the lens functions are
available without a separate import.

## Lens Operators

The `=!` and `%!` operators provide concise syntax for lens-based
state updates. When the left operand is a symbol, it is
automatically wrapped in `at()`:

```eu
{ import: "state.eu" }

action: { :state
  _: :count =! 0
  _: :count %! (+ 1)
  _: :count %! (+ 1)
}

result: state.exec(action, {count: 99}).count
```

```yaml
result: 2
```

| Operator | Meaning | Equivalent to |
|----------|---------|---------------|
| `lens =! val` | Set at lens | `state.lift(over(lens, -> val))` |
| `lens %! fn` | Modify at lens | `state.lift(over(lens, fn))` |

## Comparison with the Random Monad

The state monad follows the same pattern as `{ :random ... }`:

| | Random | State |
|---|---|---|
| State type | PRNG stream | Block |
| Import | Built-in (prelude) | `{ import: "state.eu" }` |
| Block tag | `{ :random ... }` | `{ :state ... }` |
| Run | `random.run(action, stream)` | `state.run(action, initial)` |
| Read state | `random.float`, `random.int(n)` | `state.get`, `state.query(f)` |
| Modify state | Automatic (stream advances) | `state.put`, `state.modify`, `=!`, `%!` |

## Example: Accumulating Counts

```eu
{ import: "state.eu" }

count-item(item): { :state
  current: state.query(~ item)
  _: state.put(item, current fnil(+ 1, 0))
}

count-items(s, item): state.exec(count-item(item), s)

result: [:a, :b, :a, :c, :a, :b] foldl(count-items, {})
```

```yaml
result:
  a: 3
  b: 2
  c: 1
```

## How It Works

Under the hood, `state-bind` and `state-ret` are:

```eu,notest
state-ret(v, s): { value: v, state: s }

state-bind(action, f, s): {
  r: action(s)
  run: f(r.value)(r.state)
  value: run.value
  state: run.state
}
```

The `state` namespace is registered with `monad: true` metadata, so
the desugarer recognises `{ :state ... }` blocks and chains
declarations through `state.bind` / `state.return` automatically.
