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
