# Blocks

## Construction

| Function | Description |
|----------|-------------|
| `block(kvs)` | Construct block from list of `[key, value]` pairs |
| `pair(k, v)` | Create a `[key, value]` pair |
| `sym(s)` | Create symbol from string `s` |
| `tongue(ks, v)` | Create nested block from key path to value |
| `zip-kv(ks, vs)` | Create block by zipping keys and values |
| `with-keys(ks)` | Alias for `zip-kv` |
| `map-as-block(f, syms)` | Map symbols and create block |

## Access

| Function | Description |
|----------|-------------|
| `lookup(s, b)` | Look up symbol `s` in block (error if missing) |
| `lookup-in(b, s)` | Same as `lookup` with swapped args |
| `lookup-or(s, d, b)` | Look up with default `d` if missing |
| `lookup-or-in(b, s, d)` | Same with swapped args |
| `lookup-alts(syms, d, b)` | Try symbols in order until found |
| `lookup-across(s, d, bs)` | Look up in sequence of blocks |
| `lookup-path(ks, b)` | Look up nested key path |
| `has(s, b)` | True if block has key `s` |
| `elements(b)` | List of `[key, value]` pairs |
| `keys(b)` | List of keys |
| `values(b)` | List of values |
| `key(pr)` | Key from a pair |
| `value(pr)` | Value from a pair |

## Merging

| Function | Description |
|----------|-------------|
| `merge(b1, b2)` | Shallow merge `b2` onto `b1` |
| `deep-merge(b1, b2)` | Deep merge (nested blocks) |
| `l << r` | Operator for deep merge |
| `merge-all(bs)` | Merge list of blocks |
| `merge-at(ks, v, b)` | Merge `v` at key path `ks` |

## Transformation

| Function | Description |
|----------|-------------|
| `map-values(f, b)` | Apply `f` to each value |
| `map-keys(f, b)` | Apply `f` to each key |
| `map-kv(f, b)` | Apply `f(k, v)` to each pair, return list |
| `filter-items(f, b)` | Filter items by predicate on pairs |
| `filter-values(p?, b)` | Values matching predicate |
| `match-filter-values(re, b)` | Values with keys matching regex |

## Item Predicates

| Function | Description |
|----------|-------------|
| `by-key(p?)` | Predicate on key |
| `by-key-name(p?)` | Predicate on key as string |
| `by-key-match(re)` | Predicate matching key against regex |
| `by-value(p?)` | Predicate on value |

## Deep Find and Query

These functions search recursively through nested block structures.

| Function | Description |
|----------|-------------|
| `deep-find(k, b)` | All values for key `k` at any depth, depth-first |
| `deep-find-first(k, d, b)` | First value for key `k`, or default `d` |
| `deep-find-paths(k, b)` | Key paths to all occurrences of key `k` |
| `deep-query(pattern, b)` | Query using dot-separated pattern string |
| `deep-query-first(pattern, d, b)` | First match for pattern, or default `d` |
| `deep-query-paths(pattern, b)` | Key paths matching pattern |

### Deep Find

Searches for a key at any nesting level:

```eu
config: {
  server: { host: "localhost" port: 8080 }
  db: { host: "db.local" port: 5432 }
}

hosts: config deep-find("host")  # ["localhost", "db.local"]
first-host: config deep-find-first("host", "unknown")  # "localhost"
```

### Deep Query

Queries using dot-separated patterns with wildcards:

- Bare name `foo` is sugar for `**.foo` (find at any depth)
- `*` matches one level
- `**` matches any depth

```eu
data: {
  us: { config: { host: "us.example.com" } }
  eu: { config: { host: "eu.example.com" } }
}

# Find all hosts under any config
hosts: data deep-query("config.host")  # ["us.example.com", "eu.example.com"]

# Wildcard: any key at one level, then host
hosts: data deep-query("*.config.host")
```

## Mutation

| Function | Description |
|----------|-------------|
| `alter-value(k, v, b)` | Set `b.k` to `v` |
| `update-value(k, f, b)` | Apply `f` to `b.k` |
| `alter(ks, v, b)` | Set value at nested key path |
| `update(ks, f, b)` | Apply `f` at nested key path |
| `update-value-or(k, f, d, b)` | Update or add with default |
| `set-value(k, v, b)` | Set value, adding if absent |
