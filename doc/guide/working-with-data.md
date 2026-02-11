# Working with Data

Eucalypt is designed for processing structured data. This chapter
shows how to load, transform, and output data in practical scenarios.

## Format conversion

Convert between formats by specifying input and output:

```sh
# YAML to JSON (default output is YAML, use -j for JSON)
eu config.yaml -j

# TOML to YAML
eu config.toml

# JSON to TOML
eu data.json -t
```

## Merging multiple inputs

When multiple inputs are given, they are merged left to right:

```sh
eu base.yaml overrides.yaml
```

This is equivalent to `base overrides` (catenation merge). Later
values override earlier ones.

## Evaluands: inline expressions

Use `-e` to evaluate an expression against the loaded data:

```sh
# Extract a specific field
eu config.yaml -e 'db.host'

# Transform data
eu people.csv -e '_ map(.name)'
```

## Pipeline processing

Combine inputs, expressions, and output format:

```sh
# Load CSV, filter, output as JSON
eu -e 'data filter(_.age > 30) map(.name)' data=people.csv -j
```

## List processing patterns

### Filter and transform

```eu
data: [
  { name: "Alice", age: 30, role: "dev" },
  { name: "Bob", age: 25, role: "ops" },
  { name: "Carol", age: 35, role: "dev" }
]

devs: data filter(_.role = "dev") map(.name) //=> ["Alice", "Carol"]
```

### Aggregation

```eu
scores: [85, 92, 78, 96, 88]
total: scores foldl(+, 0)     //=> 439
cnt: count(scores)             //=> 5
```

### Sorting

```eu
items: [
  { name: "cherry", price: 3 },
  { name: "apple", price: 1 },
  { name: "banana", price: 2 }
]
sorted: items sort-by-str(.name)
result: sorted map(.name) //=> ["apple", "banana", "cherry"]
```

## Working with blocks

### Extracting structure

```eu
config: {
  db: { host: "localhost" port: 5432 }
  cache: { host: "redis" port: 6379 }
}

hosts: config map-values(.host) //=> { db: "localhost" cache: "redis" }
```

### Building output

```eu,notest
people: [
  { first: "Alice" last: "Smith" }
  { first: "Bob" last: "Jones" }
]

result: people map(p: {
  full-name: "{p.first} {p.last}"
  initials: "{p.first str.take(1)}{p.last str.take(1)}"
})
```

## Deep querying

Search through nested structures:

```eu,notest
data: {
  servers: {
    web: { host: "web1" port: 80 }
    api: { host: "api1" port: 8080 }
  }
  databases: {
    main: { host: "db1" port: 5432 }
  }
}

all-hosts: data deep-find("host")
# => ["web1", "api1", "db1"]
```

## Render targets

Mark a declaration as the render target to control what gets output:

```eu,notest
` :target
summary: {
  count: count(data)
  names: data map(.name)
}

data: [
  { name: "Alice" age: 30 }
  { name: "Bob" age: 25 }
]
```

Only `summary` is rendered. Without `:target`, all visible
declarations are output.

## Text output

Use `-T` for plain text output:

```sh
eu -T -e '"hello, world"'
```

The text format renders strings without quotes, making it suitable
for generating plain text, scripts, or configuration files.

## Piping with other tools

Eucalypt works well in shell pipelines:

```sh
# Process JSON from curl
curl -s https://api.example.com/data | eu -e '_ map(.name)' -j

# Generate config and pipe to a tool
eu base.yaml prod.yaml -T -e 'render-config'
```

## Practical example: data report

```eu
sales: [
  { product: "Widget" qty: 100 price: 10 }
  { product: "Gadget" qty: 50 price: 25 }
  { product: "Gizmo" qty: 75 price: 15 }
]

report: {
  total-revenue: sales map(s: s.qty * s.price) foldl(+, 0)
  top-seller: sales sort-by-num(s: 0 - s.qty * s.price) head
  product-count: count(sales)
}

revenue: report.total-revenue //=> 3375
count: report.product-count   //=> 3
```

## Next steps

- [The Command Line](command-line.md) -- full CLI reference
- [Advanced Topics](advanced-topics.md) -- metadata, sets, and more
