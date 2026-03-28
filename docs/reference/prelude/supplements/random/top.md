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
