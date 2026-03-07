## Usage Pattern

The random functions use a functional random stream pattern. Each
function consumes some random values and returns both a result and the
remaining stream in a block with `value` and `rest` keys:

```eu
result: random-int(6, io.random)
die-roll: result.value    # a number from 0 to 5
remaining: result.rest    # unconsumed stream for further use
```

To chain multiple random operations, thread the `rest` through:

```eu
rolls: {
  first: random-int(6, io.random)
  second: random-int(6, first.rest)
  value: [first.value + 1, second.value + 1]
}
two-dice: rolls.value
```

## Shuffling and Sampling

```eu
deck: range(1, 53)
shuffled: shuffle(deck, io.random)
hand: shuffled.value take(5)
```

```eu
colours: ["red", "green", "blue", "yellow", "purple"]
picked: sample(2, colours, io.random)
two-colours: picked.value
```

## Deterministic Seeds

For reproducible output (useful in tests), pass a fixed seed:

```eu
stream: random-stream(12345)
x: random-int(100, stream)
# x.value is always the same for seed 12345
```

Or use `--seed` on the command line, which sets `io.RANDOM_SEED`:

```sh
eu --seed 42 my-template.eu
```
