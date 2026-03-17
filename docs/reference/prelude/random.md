# Random Numbers

The `random` namespace provides pseudo-random number generation as a
state monad over a PRNG stream.

## The random stream

A random stream is provided at startup as `io.random`, seeded from
system entropy. Each run produces different values unless you supply
a fixed seed with `--seed`:

```sh
eu --seed 42 example.eu
```

You can also create a deterministic stream directly:

```eu,notest
stream: random.stream(12345)
```

## Using random operations

Each random operation takes a stream and returns a `{value, rest}`
block. For a single value, pass `io.random`:

```eu,notest
roll: random.int(6, io.random).value + 1
```

For multiple values, you must propagate `.rest` into the next call —
reusing `io.random` gives the same value each time. Use a
`{ :random ... }` monadic block or combinators like `sequence` to
handle this automatically:

```eu,notest
dice: { :random
  a: random.int(6)
  b: random.int(6)
}.[a + b + 2]

result: dice(io.random).value
```

Always extract `.value` before rendering — the `.rest` field is an
infinite stream.

## Reference

| Function | Description |
|----------|-------------|
| `random.stream(seed)` | Create a PRNG stream from an integer seed |
| `random.bind(m, f)` | State monad bind: run action m, pass result to f, thread stream |
| `random.return(v)` | State monad return: wrap a pure value as an action |
| `random.float` | Action returning a random float in [0,1) |
| `random.int(n)` | Action returning a random integer in [0,n) |
| `random.choice(list)` | Action returning a random element from list |
| `random.shuffle(list)` | Action returning a shuffled copy of list |
| `random.sample(n, list)` | Action returning n elements sampled without replacement |
| `random.map(f, action)` | Apply pure function f to the result of an action (derived) |
| `random.then(b, a)` | Sequence two actions, discard first result (derived). Pipeline: `a random.then(b)` |
| `random.join(mm)` | Flatten a nested action (derived) |
| `random.sequence(ms)` | Sequence a list of actions, collect results (derived) |
| `random.map-m(f, xs)` | Map f over list producing actions, then sequence (derived) |
| `random.filter-m(p, xs)` | Monadic filter over a list of actions (derived) |
