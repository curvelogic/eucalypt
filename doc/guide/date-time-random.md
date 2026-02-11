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

```eu
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

Eucalypt provides pseudo-random number generation using a functional
stream pattern.

### The Random Stream

The `io.random` binding is an infinite lazy list of random floats in
`[0, 1)`:

```eu
first-value: io.random head
```

Each run produces different values unless you supply a seed:

```sh
eu --seed 42 example.eu
```

### Generating Random Values

Random functions consume part of the stream and return both a result
and the remaining stream:

```eu
result: random-int(100, io.random)
value: result.value   # a number from 0 to 99
rest: result.rest     # remaining stream
```

### Practical Examples

**Rolling dice:**

```eu
roll: random-int(6, io.random)
die: roll.value + 1
```

**Picking a random element:**

```eu
colours: ["red", "green", "blue"]
pick: random-choice(colours, io.random)
colour: pick.value
```

**Shuffling a list:**

```eu
items: ["a", "b", "c", "d"]
shuffled: shuffle(items, io.random)
result: shuffled.value
```

**Sampling without replacement:**

```eu
pool: range(1, 50)
drawn: sample(6, pool, io.random)
lottery: drawn.value
```

See the [Random Numbers reference](../reference/prelude/random.md) for
the full API.
