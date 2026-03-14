# Date, Time, and Random Numbers

## Zoned Date-Time (ZDT) Values

Eucalypt has native support for date-time values through the ZDT
(Zoned Date-Time) type. ZDT values represent a point in time with
timezone information.

### ZDT Literals

Use the `t"..."` prefix to write date-time literals directly in
eucalypt source:

```eu
today: t"2024-03-15"
meeting: t"2024-03-15T14:30:00Z"
local: t"2024-03-15T14:30:00+01:00"
```

The `t"..."` syntax accepts ISO 8601 formats:

| Format | Example | Notes |
|--------|---------|-------|
| Date only | `t"2024-03-15"` | Midnight UTC |
| UTC | `t"2024-03-15T14:30:00Z"` | |
| With offset | `t"2024-03-15T14:30:00+05:00"` | |
| Fractional seconds | `t"2024-03-15T14:30:00.123Z"` | |

### Parsing and Formatting

The `cal` namespace provides functions for working with date-time
values:

```eu,notest
# Parse from a string
d: cal.parse("2024-03-15T14:30:00Z")

# Format to a custom string
label: t"2024-03-15" cal.format("%Y-%m-%d")  # "2024-03-15"
```

### Date-Time Arithmetic

ZDT values support comparison operators:

```eu
before: t"2024-01-01" < t"2024-12-31"   # true
same: t"2024-03-15" = t"2024-03-15"     # true
```

### Sorting Date-Times

```eu
dates: [t"2024-12-25", t"2024-01-01", t"2024-07-04"]
sorted: dates sort-zdts  # [Jan 1, Jul 4, Dec 25]
```

### YAML Timestamps

When importing YAML files, unquoted timestamp values are automatically
converted to ZDT values:

```yaml
created: 2024-03-15
updated: 2024-03-15T14:30:00Z
```

Quote the value to keep it as a string: `created: "2024-03-15"`.

See [Import Formats](../reference/import-formats.md) for full details.

### Current Time

The `io.epoch-time` binding provides the current Unix epoch time in
seconds:

```eu
now: io.epoch-time
```

## Random Numbers

Eucalypt provides pseudo-random number generation via the `random`
namespace, which is a state monad over a PRNG stream.

### The random stream

A random stream is provided at startup as `io.random`. Each run
produces different values unless you supply a seed:

```sh
eu --seed 42 example.eu
```

### Single random values

For a single random value, pass `io.random` as the stream:

```eu,notest
roll: random.int(6, io.random).value + 1
colour: random.choice(["red", "green", "blue"], io.random).value
```

Each operation returns a `{value, rest}` block — extract `.value`
to get the result.

### Multiple random values

When you need several random values, you must propagate the stream
from each call to the next. Reusing `io.random` would give the same
value each time:

```eu,notest
# WRONG — both use the same stream, so d1 = d2
d1: random.int(6, io.random).value
d2: random.int(6, io.random).value

# RIGHT — thread the stream manually
r1: random.int(6, io.random)
r2: random.int(6, r1.rest)
total: r1.value + r2.value + 2
```

This manual threading is error-prone. The `random` monad solves it —
use a `{ :random ... }` monadic block or combinators like `sequence`
and `map-m`:

```eu,notest
# Monadic block — stream threads automatically
dice: { :random
  d6: random.int(6)
  d20: random.int(20)
}.[d6, d20]

result: dice(io.random).value  # e.g. [3, 14]

# Or use sequence for a list of actions
two-dice: random.sequence([random.int(6), random.int(6)], io.random).value
```

### Other operations

```eu,notest
shuffled: random.shuffle(["a", "b", "c", "d"], io.random).value
picked: random.sample(3, range(1, 50), io.random).value
```

See the [Random Numbers reference](../reference/prelude/random.md) for
the full API and the [Monads guide](monads.md) for details on the
state monad pattern.
