# Random Numbers

Eucalypt provides pseudo-random number generation through the `io.random`
stream and a set of prelude functions.

## The Random Stream

The `io.random` binding is an opaque PRNG stream, seeded from system
entropy or the `--seed` command-line flag. Use `random.*` actions to
draw values from it, or `random.as-list` to obtain a lazy cons-list of
floats in `[0, 1)`.

```eu
first-random: random.float(io.random).value
```

Because `io.random` is seeded from the system clock by default, it
produces different values on each run. Use `--seed` for reproducible
results:

```sh
eu --seed 42 example.eu
```

## Random Number Generation

| Function | Description |
|----------|-------------|
| `random-stream(seed)` |  |
| `stream-advance(n, s)` |  |
| `random-ret(v, stream)` |  |
| `random-bind(m, f, stream)` |  |
| `random.stream(seed)` | Create an opaque PRNG stream from integer seed |
| `random.as-list(s)` | Convert an opaque random stream to a lazy cons-list of floats |
| `random.float(s)` | State-monad action returning a random float in [0,1) |
| `random.int(n, s)` | State-monad action returning a random integer in [0,n) |
| `random.split(s)` | State-monad action that splits the stream into two independent streams. Returns a value/rest block where value and rest are each independent streams |
| `random.choice(lst, stream)` | State-monad action returning a random element from list |
| `random.shuffle(lst)` | State-monad action returning a shuffled copy of list |
| `random.sample(n, lst, stream)` | State-monad action returning n elements sampled without replacement |
| `random.run(action, stream)` | Run a random action on a stream; returns a value/rest block |
| `random.eval(action, stream)` | Run a random action and return only the value |
| `random.exec(action, stream)` | Run a random action and return only the remaining stream |

## Usage Pattern

The `random` namespace is a state monad. A random stream is provided
at startup as `io.random`. Each operation takes a stream and returns
a `{value, rest}` block:

```eu,notest
result: random.int(6, io.random)
die-roll: result.value    # a number from 0 to 5
```

For multiple random values, propagate `.rest` into the next call — or
use the random monad to handle threading automatically:

```eu,notest
# Monadic block — no manual threading
dice: { :random
  a: random.int(6)
  b: random.int(6)
}.[a + b + 2]

result: dice(io.random).value
```

`random.sequence` and `random.map-m` also handle threading:

```eu,notest
two-dice: random.sequence([random.int(6), random.int(6)], io.random).value
```

## Shuffling and Sampling

```eu,notest
deck: range(1, 53)
hand: random.shuffle(deck, io.random).value take(5)
```

```eu,notest
colours: ["red", "green", "blue", "yellow", "purple"]
two-colours: random.sample(2, colours, io.random).value
```

## Deterministic Seeds

For reproducible output (useful in tests), use `--seed` on the
command line or create a stream from a fixed seed:

```sh
eu --seed 42 my-template.eu
```

```eu,notest
stream: random.stream(12345)
x: random.int(100, stream)
# x.value is always the same for seed 12345
```
