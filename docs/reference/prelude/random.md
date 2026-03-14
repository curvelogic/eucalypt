# Random Numbers

Eucalypt provides pseudo-random number generation through the `io.random`
stream and a set of prelude functions. There are two APIs: the legacy
functional threading API and the newer monadic `random:` namespace.

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

---

## Monadic API — `random:` namespace

The `random:` namespace provides a state-monad interface where each
random operation is a *function* from a PRNG stream to a result block.
Use `random.bind` and `random.return` (plus the derived combinators from
`monad()`) to compose operations without manually threading the stream.

### Creating the stream

```eu,notest
my-stream: random.stream(42)   # deterministic, seeded stream
```

### Running an action

An action is called with a stream; it returns a block with `value` and
`rest`:

```eu,notest
r: random.int(6, random.stream(42))
die-roll: r.value    # integer in [0,6)
next-stream: r.rest  # stream for further operations
```

### Composing actions with sequence

```eu,notest
two-dice: random.sequence([random.int(6), random.int(6)], random.stream(42)).value
# => e.g. [4, 0]
```

### Deriving a monad variant

Use `monad()` catenation to override or extend the namespace:

```eu,notest
dice: monad{bind: random.bind, return: random.return} {
  d6: random.int(6)
  d20: random.int(20)
}
roll: dice.sequence([dice.d6, dice.d20], random.stream(42)).value
```

### Reference

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
| `random.then(a, b)` | Sequence two actions, discard first result (derived) |
| `random.join(mm)` | Flatten a nested action (derived) |
| `random.sequence(ms)` | Sequence a list of actions, collect results (derived) |
| `random.map-m(f, xs)` | Map f over list producing actions, then sequence (derived) |
| `random.filter-m(p, xs)` | Monadic filter over a list of actions (derived) |

---

## Deterministic Seeds

For reproducible output, pass a fixed seed to `random.stream` or use
`--seed` on the command line (which sets `io.RANDOM_SEED`):

```sh
eu --seed 42 my-template.eu
```

```eu,notest
stream: random.stream(12345)
x: random.int(100, stream).value
# x is always the same for seed 12345
```
