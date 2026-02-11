# Eucalypt by Example

This page walks through real-world problems solved in eucalypt. Each
example shows the problem, the eucalypt code, and the output.

---

## 1. Merging configuration files

**Problem:** Combine a base configuration with environment-specific
overrides.

```eu,notest
{ import: ["base=config/base.yaml", "env=config/prod.yaml"] }

config: base << env
db-url: "postgres://{config.db.host}:{config.db.port}/{config.db.name}"
```

```sh
eu base.yaml prod.yaml -e 'base << prod'
```

The `<<` operator deep-merges blocks, preserving nested keys that are
not overridden.

---

## 2. Filtering and transforming a list

**Problem:** From a list of people, find developers over 30 and
produce a summary.

```eu
is-senior-dev(p): p.age > 30 && p.role = "dev"

people: [
  { name: "Alice", age: 35, role: "dev" },
  { name: "Bob", age: 25, role: "ops" },
  { name: "Carol", age: 40, role: "dev" },
  { name: "Dave", age: 28, role: "dev" }
]

senior-devs: people filter(is-senior-dev) map(.name)
result: senior-devs //=> ["Alice", "Carol"]
```

`filter` keeps elements matching the predicate. `map(.name)` extracts
a single field using lookup syntax.

---

## 3. Aggregating CSV data

**Problem:** Compute total revenue from a CSV of transactions.

```sh
eu -e 'data map(.amount) map(num) foldl(+, 0)' data=transactions.csv
```

CSV rows become blocks with column headers as keys. `num` converts
string values to numbers before summing.

---

## 4. Building JSON output

**Problem:** Transform a flat list into structured JSON.

```eu
pairs: [["x", 1], ["y", 2], ["z", 3]]

to-entry(pair): { key: head(pair), value: last(pair) }

result: pairs map(to-entry)
```

```sh
eu transform.eu -j
```

Output:

```json
[
  {"key": "x", "value": 1},
  {"key": "y", "value": 2},
  {"key": "z", "value": 3}
]
```

---

## 5. String processing pipeline

**Problem:** Clean and normalise a list of email addresses.

```eu,notest
normalise(email): email str.to-lower str.trim

emails: ["  Alice@Example.com ", "BOB@test.COM", " carol@DEMO.org"]
result: emails map(normalise)
```

Each email is trimmed of whitespace and converted to lower case.

---

## 6. Grouping and counting

**Problem:** Group log entries by level and count each group.

```eu,notest
{ import: "entries=log.csv" }

is-error(e): e.level = "ERROR"
is-warn(e): e.level = "WARN"

summary: {
  errors: entries filter(is-error) count
  warnings: entries filter(is-warn) count
  total: entries count
}
```

The `count` function returns the number of elements in a list.

---

## 7. Recursive data processing

**Problem:** Extract all values for a given key from a deeply nested
structure.

```eu,notest
data: {
  servers: {
    web: { host: "web1", port: 80 },
    api: { host: "api1", port: 8080 }
  },
  databases: {
    main: { host: "db1", port: 5432 }
  }
}

all-hosts: data deep-find("host")
# => ["web1", "api1", "db1"]
```

`deep-find` searches recursively through all nested blocks for the
specified key.

---

## 8. Format conversion one-liner

**Problem:** Convert YAML to JSON from the command line.

```sh
eu config.yaml -j
```

Or pipe from stdin:

```sh
cat data.yaml | eu -j
```

Eucalypt reads YAML by default and `-j` switches output to JSON. No
code needed.

---

## 9. Data pipeline with multiple steps

**Problem:** Process a list of products -- filter, sort, compute
totals.

```eu
revenue(item): item.qty * item.price

items: [
  { name: "Widget", qty: 100, price: 10 },
  { name: "Gadget", qty: 50, price: 25 },
  { name: "Gizmo", qty: 75, price: 15 }
]

sorted: items sort-by-str(.name)
names: sorted map(.name)         //=> ["Gadget", "Gizmo", "Widget"]
total: items map(revenue) foldl(+, 0) //=> 3375
```

Pipelines read left to right: start with the data, apply each
transformation in turn.

---

## 10. Parameterised scripts

**Problem:** Write a reusable script that accepts arguments.

```eu,notest
name: io.args head-or("World")
greeting: "Hello, {name}!"
```

```sh
eu greet.eu -e greeting -- Alice
# => Hello, Alice!

eu greet.eu -e greeting
# => Hello, World!
```

Arguments after `--` are available via `io.args`. `head-or` provides
a default when no arguments are given.

---

## 11. Sorting with custom comparisons

**Problem:** Sort a list of records by a computed value.

```eu
items: [
  { name: "cherry", price: 3 },
  { name: "apple", price: 1 },
  { name: "banana", price: 2 }
]

by-name: items sort-by-str(.name) map(.name)
result: by-name //=> ["apple", "banana", "cherry"]
```

`sort-by-str` sorts by a string-valued key function. Use
`sort-by-num` for numeric keys.

---

## 12. Merging block values

**Problem:** Extract and transform values from a block.

```eu
config: {
  db: { host: "localhost", port: 5432 },
  cache: { host: "redis", port: 6379 }
}

hosts: config map-values(.host) //=> { db: "localhost", cache: "redis" }
```

`map-values` applies a function to every value in a block, keeping
the keys unchanged.

---

## 13. Working with dates

**Problem:** Generate a schedule with formatted dates.

```eu,notest
today: io.now
formatted: zdt.format("yyyy-MM-dd", today)
```

See [Date, Time, and Random Numbers](../guide/date-time-random.md) for
the full date/time API.

---

## 14. Reproducible random output

**Problem:** Generate random test data that is reproducible.

```eu,notest
pick(xs): xs nth(io.random-nat(count(xs)))
colours: ["red", "green", "blue"]
chosen: pick(colours)
```

```sh
eu --seed 42 template.eu
```

The `--seed` flag ensures the same random sequence each run.

---

## 15. Stream processing large files

**Problem:** Process a large JSON Lines file without loading it all
into memory.

```sh
eu -e 'data filter(_.level = "ERROR") take(10)' \
   data=jsonl-stream@events.jsonl -j
```

Streaming formats (`jsonl-stream`, `csv-stream`, `text-stream`) read
lazily, processing one record at a time.

---

## What next?

- [The Eucalypt Guide](../guide/blocks-and-declarations.md) -- a
  progressive tutorial from basics to advanced features
- [Syntax Cheat Sheet](../appendices/cheat-sheet.md) -- quick syntax
  reference
- [CLI Reference](../reference/cli.md) -- full command-line
  documentation
