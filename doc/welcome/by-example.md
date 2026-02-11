# Eucalypt by Example

This page presents a collection of worked examples showing how
eucalypt solves real-world problems. Each example includes the
problem, the eucalypt code, and the expected output.

## 1. Format Conversion: JSON to YAML

**Problem:** Convert a JSON configuration file to YAML.

```sh
echo '{"database": {"host": "db.example.com", "port": 5432}}' | eu
```

**Output:**

```yaml
database:
  host: db.example.com
  port: 5432
```

Eucalypt reads JSON natively and defaults to YAML output. No code
needed.

## 2. Extracting Fields from API Data

**Problem:** Given a list of users in JSON, extract just their names.

```sh
eu -e 'map(_.name)' <<'JSON'
[
  {"name": "Alice", "role": "admin"},
  {"name": "Bob", "role": "user"},
  {"name": "Charlie", "role": "user"}
]
JSON
```

**Output:**

```yaml
- Alice
- Bob
- Charlie
```

The `_` is an expression anaphor -- `_.name` means "look up `name`
in whatever the argument is".

## 3. Filtering and Transforming Data

**Problem:** From a list of products, find those over a price
threshold and format them.

```eu
# products.eu
products: [
  { name: "Widget" price: 9.99 }
  { name: "Gadget" price: 24.99 }
  { name: "Gizmo" price: 49.99 }
  { name: "Doohickey" price: 4.99 }
]

expensive: products
  filter(_.price > 20)
  map({name: •}.(name str.to-upper))
```

```sh
eu products.eu -e expensive
```

**Output:**

```yaml
- GADGET
- GIZMO
```

## 4. Generating Configuration with Shared Defaults

**Problem:** Generate environment-specific configs that share common
defaults.

```eu
# config.eu
base: {
  app: "my-service"
  port: 8080
  log-level: "info"
  db: { host: "localhost" port: 5432 }
}

production: base << {
  log-level: "warn"
  db: { host: "prod-db.internal" }
}

staging: base << {
  db: { host: "staging-db.internal" }
}
```

```sh
eu config.eu -e production -j
```

**Output:**

```json
{
  "app": "my-service",
  "port": 8080,
  "log-level": "warn",
  "db": {
    "host": "prod-db.internal",
    "port": 5432
  }
}
```

The `<<` operator deep-merges blocks, so nested keys like `db.port`
are preserved while `db.host` is overridden.

## 5. CSV to JSON Conversion

**Problem:** Read a CSV file and output as a JSON array.

Given `people.csv`:
```csv
name,age,city
Alice,30,London
Bob,25,Manchester
Charlie,35,Edinburgh
```

```sh
eu rows=people.csv -j -e 'rows map({name: •}.(name))'
```

**Output:**

```json
["Alice", "Bob", "Charlie"]
```

Or to transform the data:

```sh
eu rows=people.csv -j -e 'rows map({ n: • }.({name: n.name age: n.age num}))'
```

This converts age from string to number (CSV values are always
strings).

## 6. Generating Availability Zone Names

**Problem:** Generate AWS availability zone names from a list of zone
letters.

```sh
eu -e '["a", "b", "c"] map("eu-west-2{}")'
```

**Output:**

```yaml
- eu-west-2a
- eu-west-2b
- eu-west-2c
```

The string `"eu-west-2{}"` is a function: `{}` is a string anaphor
that takes one argument.

## 7. Merging Multiple YAML Files

**Problem:** Combine values from several YAML files into a single
output.

Given `defaults.yaml`:
```yaml
timeout: 30
retries: 3
```

Given `overrides.yaml`:
```yaml
timeout: 60
debug: true
```

```sh
eu defaults.yaml overrides.yaml
```

**Output:**

```yaml
timeout: 60
retries: 3
debug: true
```

Later inputs override earlier ones. For a merged view with both
available, use named inputs:

```sh
eu d=defaults.yaml o=overrides.yaml -e 'd << o'
```

## 8. Data Aggregation Pipeline

**Problem:** Compute summary statistics from structured data.

```eu
# sales.eu
sales: [
  { region: "North" amount: 1200 }
  { region: "South" amount: 800 }
  { region: "North" amount: 600 }
  { region: "South" amount: 1500 }
  { region: "East" amount: 900 }
]

` :suppress
amounts: sales map(_.amount)

summary: {
  total: amounts foldl(+, 0)
  count: sales count
  average: summary.total / summary.count
  max: amounts max-of
  min: amounts min-of
}
```

```sh
eu sales.eu -e summary
```

**Output:**

```yaml
total: 5000
count: 5
average: 1000
max: 1500
min: 600
```

## 9. Querying Deeply Nested Configuration

**Problem:** Find all port numbers in a complex configuration.

```sh
eu -e 'deep-query("port", {
  web: { host: "0.0.0.0" port: 80 }
  api: { host: "0.0.0.0" port: 8080 }
  db: { host: "localhost" port: 5432 }
  cache: { host: "localhost" port: 6379 }
})'
```

**Output:**

```yaml
- 80
- 8080
- 5432
- 6379
```

`deep-query` recursively searches nested blocks. You can also use
wildcards: `deep-query("*.port", data)` matches ports one level deep,
while `deep-query("**.port", data)` matches at any depth.

## 10. String Processing: Parsing Log Lines

**Problem:** Extract timestamps and levels from log lines.

```eu
# logs.eu
lines: [
  "2024-03-15 10:30:00 ERROR Connection timeout"
  "2024-03-15 10:30:05 INFO Retry attempt 1"
  "2024-03-15 10:30:10 ERROR Connection timeout"
  "2024-03-15 10:30:15 INFO Connected"
]

` :suppress
parse(line): line str.match-with("(\S+ \S+) (\w+) (.*)") tail

parsed: lines map(parse) map({parts: •}.({
  timestamp: parts first
  level: parts second
  message: parts nth(2)
}))

errors: parsed filter(_.level = "ERROR")
```

```sh
eu logs.eu -e errors
```

**Output:**

```yaml
- timestamp: '2024-03-15 10:30:00'
  level: ERROR
  message: Connection timeout
- timestamp: '2024-03-15 10:30:10'
  level: ERROR
  message: Connection timeout
```

## 11. Templating CloudFormation Resources

**Problem:** Generate YAML with custom tags for CloudFormation.

```eu
# cfn.eu
resource(name, type, props): {
  'Type': type
  'Properties': props
}

resources: {
  MyBucket: resource("bucket", "AWS::S3::Bucket", {
    'BucketName': :my-bucket // { tag: "!Ref AccountId" }
  })
  MyQueue: resource("queue", "AWS::SQS::Queue", {
    'QueueName': "my-queue"
  })
}
```

This demonstrates using single-quote identifiers for keys with
special characters and the metadata `tag` key for YAML tags.

## 12. Working with Dates

**Problem:** Filter events by date and format the output.

```eu
# events.eu
events: [
  { name: "Launch" date: t"2024-01-15" }
  { name: "Review" date: t"2024-06-01" }
  { name: "Release" date: t"2024-09-30" }
]

cutoff: t"2024-06-01"

upcoming: events
  filter(_.date >= cutoff)
  map(_.name)
```

```sh
eu events.eu -e upcoming
```

**Output:**

```yaml
- Review
- Release
```

The `t"..."` syntax creates date-time literals that support
comparison operators.

## 13. Generating a Lookup Table

**Problem:** Build a key-value mapping from two parallel lists.

```sh
eu -e 'zip-kv([:name, :age, :city], ["Alice", 30, "London"])'
```

**Output:**

```yaml
name: Alice
age: 30
city: London
```

`zip-kv` pairs up symbols as keys with values to produce a block.

## 14. Parameterised Scripts

**Problem:** Write a reusable script that accepts command-line
arguments.

```eu
# greet.eu
name: io.args head-or("World")
times: io.args tail head-or("1") num

greetings: repeat("Hello, {name}!") take(times)
```

```sh
eu greet.eu -e greetings -- Alice 3
```

**Output:**

```yaml
- Hello, Alice!
- Hello, Alice!
- Hello, Alice!
```

Arguments after `--` are available via `io.args` as a list of
strings. Use `num` to convert to numbers.

## 15. Set Operations: Finding Unique Values

**Problem:** Find the unique tags across multiple items and compute
overlaps.

```eu
items: [
  { name: "A" tags: ["fast", "reliable", "cheap"] }
  { name: "B" tags: ["fast", "expensive"] }
  { name: "C" tags: ["reliable", "cheap", "slow"] }
]

` :suppress
tag-sets: items map(_.tags set.from-list)

all-tags: tag-sets foldl(set.union, ∅) set.to-list
common-tags: tag-sets foldl(set.intersect, tag-sets head) set.to-list

result: {
  all: all-tags
  common: common-tags
}
```

```sh
eu tags.eu -e result
```

**Output:**

```yaml
all:
- cheap
- expensive
- fast
- reliable
- slow
common: []
```

## Next Steps

- Work through [The Eucalypt Guide](../guide/blocks-and-declarations.md)
  for a progressive tutorial
- Browse the [Prelude Reference](../reference/prelude/index.md) for
  the full standard library
- See the [CLI Reference](../reference/cli.md) for all command-line
  options
