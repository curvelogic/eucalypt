# Block Manipulation

In this chapter you will learn:

- How to merge blocks with catenation and `merge`
- How to inspect, transform, and restructure blocks
- Key prelude functions for working with blocks
- Patterns for building and modifying configuration data

## Block Merge by Catenation

When two blocks appear next to each other, the result is a shallow
merge. Later values override earlier ones:

```sh
eu -e '{ a: 1 } { b: 2 }'
```

```yaml
a: 1
b: 2
```

```sh
eu -e '{ a: 1 } { a: 2 }'
```

```yaml
a: 2
```

This is the same as calling the `merge` function:

```sh
eu -e 'merge({ a: 1 }, { b: 2 })'
```

```yaml
a: 1
b: 2
```

## Deep Merge

Use `deep-merge` or the `<<` operator for recursive merging of nested
blocks:

```eu
base: { server: { host: "localhost" port: 8080 } }
override: { server: { port: 9090 debug: true } }
config: base << override
```

```yaml
base:
  server:
    host: localhost
    port: 8080
override:
  server:
    port: 9090
    debug: true
config:
  server:
    host: localhost
    port: 9090
    debug: true
```

Note that `<<` merges nested blocks but replaces lists entirely.

## Inspecting Blocks

### `elements`

Break a block into its list of key-value pairs:

```sh
eu -e '{ a: 1 b: 2 } elements'
```

```yaml
- - a
  - 1
- - b
  - 2
```

Each element is a two-element list: `[key, value]`.

### `keys` and `values`

```sh
eu -e '{ a: 1 b: 2 c: 3 } keys'
```

```yaml
- a
- b
- c
```

```sh
eu -e '{ a: 1 b: 2 c: 3 } values'
```

```yaml
- 1
- 2
- 3
```

### `has`

Check whether a block contains a key:

```sh
eu -e '{ a: 1 b: 2 } has(:a)'
```

```yaml
true
```

### `lookup` and `lookup-or`

Look up a value by symbol key:

```sh
eu -e '{ a: 1 b: 2 } lookup(:b)'
```

```yaml
2
```

With a default for missing keys:

```sh
eu -e '{ a: 1 } lookup-or(:z, 99)'
```

```yaml
99
```

## Reconstructing Blocks

### `block`

Build a block from a list of key-value pairs:

```sh
eu -e '[[:a, 1], [:b, 2], [:c, 3]] block'
```

```yaml
a: 1
b: 2
c: 3
```

### `zip-kv`

Build a block from separate key and value lists:

```sh
eu -e 'zip-kv([:x, :y, :z], [1, 2, 3])'
```

```yaml
x: 1
y: 2
z: 3
```

### `merge-all`

Merge a list of blocks into one:

```sh
eu -e '[{a: 1}, {b: 2}, {c: 3}] merge-all'
```

```yaml
a: 1
b: 2
c: 3
```

## Transforming Blocks

### `map-values`

Apply a function to every value, keeping keys:

```sh
eu -e '{ a: 1 b: 2 c: 3 } map-values(* 10)'
```

```yaml
a: 10
b: 20
c: 30
```

### `map-keys`

Transform the keys of a block:

```sh
eu -e '{ a: 1 b: 2 } map-keys(sym ∘ str.prefix("x-") ∘ str.of)'
```

```yaml
x-a: 1
x-b: 2
```

### `filter-values`

Keep only entries whose values satisfy a predicate:

```sh
eu -e '{ a: 1 b: 20 c: 3 d: 40 } filter-values(> 10)'
```

```yaml
b: 20
d: 40
```

### `map-kv`

Transform key-value pairs. The function receives a `[key, value]`
pair and should return a `[key, value]` pair:

```sh
eu -e '{ a: 1 b: 2 } map-kv(pair) block'
```

```yaml
a: 1
b: 2
```

## Modifying Individual Values

### `alter-value`

Replace the value at a specific key:

```eu
config: { host: "localhost" port: 8080 }
updated: config alter-value(:port, 9090)
```

```yaml
config:
  host: localhost
  port: 8080
updated:
  host: localhost
  port: 9090
```

### `update-value`

Apply a function to the value at a specific key:

```eu
counters: { hits: 10 errors: 3 }
result: counters update-value(:hits, inc)
```

```yaml
counters:
  hits: 10
  errors: 3
result:
  hits: 11
  errors: 3
```

### `set-value`

Set a value, creating the key if it does not exist:

```sh
eu -e '{} set-value(:x, 42)'
```

```yaml
x: 42
```

### `alter` and `update` (nested)

Modify values deep in nested blocks using a key path:

```eu
config: { server: { db: { port: 5432 } } }
changed: config alter([:server, :db, :port], 3306)
bumped: config update([:server, :db, :port], inc)
```

```yaml
config:
  server:
    db:
      port: 5432
changed:
  server:
    db:
      port: 3306
bumped:
  server:
    db:
      port: 5433
```

### `merge-at`

Merge additional keys into a nested block:

```eu
config: { server: { db: { port: 5432 } } }
extended: config merge-at([:server, :db], { host: "10.0.0.1" })
```

```yaml
config:
  server:
    db:
      port: 5432
extended:
  server:
    db:
      port: 5432
      host: 10.0.0.1
```

## Patterns: Configuration Templating

A common pattern is to define a base configuration and layer
environment-specific overrides on top:

```eu
base: {
  server: {
    host: "0.0.0.0"
    port: 8080
    workers: 4
  }
  logging: {
    level: "info"
    format: "json"
  }
}

production: base << {
  server: { workers: 16 }
  logging: { level: "warn" }
}

development: base << {
  server: { host: "localhost" }
  logging: { level: "debug" format: "text" }
}
```

## Key Concepts

- **Block catenation** merges two blocks; later keys override earlier
  ones
- **Deep merge** (`<<`) recursively merges nested blocks
- `elements`, `keys`, `values` decompose blocks; `block` and
  `zip-kv` reconstruct them
- `map-values`, `map-keys`, `filter-values` transform blocks
- `alter-value`, `update-value`, `set-value` modify individual
  entries
- `alter`, `update`, `merge-at` modify deeply nested values
