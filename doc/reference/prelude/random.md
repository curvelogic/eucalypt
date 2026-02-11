# Random Numbers

Eucalypt provides pseudo-random number generation through the `io.random`
stream and a set of prelude functions.

## The Random Stream

The `io.random` binding is an infinite lazy list of random floats in
`[0, 1)`, seeded from system entropy or the `--seed` command-line flag.

```eu
first-random: io.random head
```

Because `io.random` is seeded from the system clock by default, it
produces different values on each run. Use `--seed` for reproducible
results:

```sh
eu --seed 42 example.eu
```

## Core Functions

| Function | Description |
|----------|-------------|
| `random-stream(seed)` | Infinite lazy list of floats in `[0, 1)` from integer seed |
| `random-int(n, stream)` | Random integer in `[0, n)` from stream. Returns `{ value, rest }` |
| `random-choice(list, stream)` | Pick a random element from list. Returns `{ value, rest }` |
| `shuffle(list, stream)` | Randomly reorder list. Returns `{ value, rest }` |
| `sample(n, list, stream)` | Pick `n` elements without replacement. Returns `{ value, rest }` |

## Usage Pattern

The random functions use a functional random stream pattern. Each
function consumes some random values and returns both a result and the
remaining stream in a block with `value` and `rest` keys:

```eu,notest
result: random-int(6, io.random)
die-roll: result.value    # a number from 0 to 5
remaining: result.rest    # unconsumed stream for further use
```

To chain multiple random operations, thread the `rest` through:

```eu,notest
rolls: {
  first: random-int(6, io.random)
  second: random-int(6, first.rest)
  value: [first.value + 1, second.value + 1]
}
two-dice: rolls.value
```

## Shuffling and Sampling

```eu,notest
deck: range(1, 53)
shuffled: shuffle(deck, io.random)
hand: shuffled.value take(5)
```

```eu,notest
colours: ["red", "green", "blue", "yellow", "purple"]
picked: sample(2, colours, io.random)
two-colours: picked.value
```

## Deterministic Seeds

For reproducible output (useful in tests), pass a fixed seed:

```eu,notest
stream: random-stream(12345)
x: random-int(100, stream)
# x.value is always the same for seed 12345
```

Or use `--seed` on the command line, which sets `io.RANDOM_SEED`:

```sh
eu --seed 42 my-template.eu
```
