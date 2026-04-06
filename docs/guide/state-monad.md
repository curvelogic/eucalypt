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

```eu,notest
# Manual threading — tedious and fragile
s0: { count: 0, name: "init" }
s1: s0 merge({count: 1})
s2: s1 merge({name: "updated"})
# Every step must reference the previous one by name
```

The state monad automates this threading.

## State Actions

A *state action* is a function from a state block to a
`{value, state}` block — the same pattern as the random monad's
`{value, rest}`:

```eu,notest
# An action that reads the count and increments it
count-and-inc(s): { value: s.count, state: s merge({count: s.count + 1}) }
```

The state monad provides primitives for building these actions
without manually deconstructing and reconstructing pairs.

## Monadic Blocks with `{ :state ... }`

Tag a block with `:state` to get automatic bind threading — each
declaration becomes a monadic step:

```eu,notest
{ import: "state.eu" }

action: { :state
  n: state.get.count
  _: state.put(:count, n + 1)
  _: state.put(:name, "step-2")
}

result: state.run(action, {count: 0, name: "init"})
# => [{count: 1, name: "step-2"}, {count: 1, name: "step-2"}]
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
| `state.run(action, s)` | Run action from state `s`, return `{value, state}` block |
| `state.eval(action, s)` | Run action, return only the value |
| `state.exec(action, s)` | Run action, return only the final state |

## Using Existing Pipeline Functions

The `lift` and `query` primitives let you reuse existing eucalypt
functions as state actions:

```eu,notest
{ import: "state.eu" }
{ import: "lens.eu" }

action: { :state
  # Use merge to add keys
  _: state.lift(merge({new-key: "added"}))

  # Use lenses for deep updates
  _: state.lift(over(at(:count), + 1))

  # Use safe navigation to read
  host: state.query(~ :server ~ :host)

  # Filter a list-valued key
  _: state.modify(:items, filter(match?{active: true}))

  # Read a derived value
  total: state.query(.items length)
}
```

## Lens Operators

The `=!` and `%!` operators provide concise syntax for lens-based
state updates. When the left operand is a symbol, it is
automatically wrapped in `at()`:

```eu,notest
{ import: "state.eu" }

action: { :state
  # Set a key (symbol auto-wrapped in at())
  _: :count =! 0

  # Modify a key with a function
  _: :count %! (+ 1)

  # Use composed lenses for deep access
  _: at(:server) =! {host: "localhost", port: 8080}
}
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
| Run | `random.run(action, io.random)` | `state.run(action, initial)` |
| Read state | `random.float`, `random.int(n)` | `state.get`, `state.query(f)` |
| Modify state | Automatic (stream advances) | `state.put`, `state.modify`, `=!`, `%!` |

## Example: Accumulating Counts

```eu,notest
{ import: "state.eu" }

# Count occurrences of each item
count-item(item): { :state
  current: state.get ~ item
  _: state.put(item, if(current null?, 1, current + 1))
}

# Process a list of items
count-all(items): items foldl(count-items, {})

count-items(s, item): state.exec(count-item(item sym), s)
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
