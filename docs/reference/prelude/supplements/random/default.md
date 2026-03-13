## Usage Pattern

The `random` namespace is a state monad. Each operation is an *action*
— a function from a PRNG stream to a `{value, rest}` block:

```eu,notest
result: random.int(6)(io.random)
die-roll: result.value    # a number from 0 to 5
```

Use `random.sequence` or `random.map-m` to compose multiple actions
without manually threading the stream:

```eu,notest
two-dice: random.sequence([random.int(6), random.int(6)])(io.random).value
```

## Shuffling and Sampling

```eu,notest
deck: range(1, 53)
hand: random.shuffle(deck)(io.random).value take(5)
```

```eu,notest
colours: ["red", "green", "blue", "yellow", "purple"]
two-colours: random.sample(2, colours)(io.random).value
```

## Deterministic Seeds

For reproducible output (useful in tests), pass a fixed seed:

```eu,notest
stream: random.stream(12345)
x: random.int(100)(stream)
# x.value is always the same for seed 12345
```

Or use `--seed` on the command line, which sets `io.RANDOM_SEED`:

```sh
eu --seed 42 my-template.eu
```
