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
