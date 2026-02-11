# Blocks

## Block Construction and Merging

| Function | Description |
|----------|-------------|
| `sym` | Create symbol with name given by string `s` |
| `merge` | Shallow merge block `b2` on top of `b1` |
| `deep-merge` | Deep merge block `b2` on top of `b1`, merges nested blocks but not lists |
| `block?` | True if and only if `v` is a block |
| `list?` | True if and only if `v` is a list |
| `elements` | Expose list of elements of block `b` |
| `block` | (re)construct block from list `kvs` of elements |
| `has(s, b)` | True if and only if block `b` has key (symbol) `s` |
| `lookup(s, b)` | Look up symbol `s` in block `b`, error if not found |
| `lookup-in(b, s)` | Look up symbol `s` in block `b`, error if not found |
| `lookup-or(s, d, b)` | Look up symbol `s` in block `b`, default `d` if not found |
| `lookup-or-in(b, s, d)` | Look up symbol `s` in block `b`, default `d` if not found |
| `lookup-alts(syms, d, b)` | Look up symbols `syms` in turn in block `b` until a value is found, default `d` if none |
| `lookup-across(s, d, bs)` | Look up symbol `s` in turn in each of blocks `bs` until a value is found, default `d` if none |
| `lookup-path(ks, b)` | Look up value at key path `ks` in block `b` |

## Block Utilities

| Function | Description |
|----------|-------------|
| `merge-all(bs)` | Merge all blocks in list `bs` together, later overriding earlier |
| `key` | Return key in a block element / pair |
| `value` | Return key in a block element / pair |
| `keys(b)` | Return keys of block |
| `values(b)` | Return values of block |
| `sort-keys(b)` | Return block `b` with keys sorted alphabetically |
| `bimap(f, g, pr)` | Apply f to first item of pair and g to second, return pair |
| `map-first(f, prs)` | Apply f to first elements of all pairs in list of pairs `prs` |
| `map-second(f, prs)` | Apply f to second elements of all pairs in list of pairs `prs` |
| `map-kv(f, b)` | Apply `f(k, v)` to each key / value pair in block `b`, returning list |
| `map-as-block(f, syms)` | Map each symbol in `syms` and create block mapping `syms` to mapped values |
| `pair(k, v)` | Form a block element from key (symbol) `k` and value `v` |
| `zip-kv(ks, vs)` | Create a block by zipping together keys `ks` and values `vs` |
| `with-keys` | Create block from list of values by assigning list of keys `ks` against them |
| `map-values(f, b)` | Apply `f(v)` to each value in block `b` |
| `map-keys(f, b)` | Apply `f(k)` to each key in block `b` |
| `filter-items(f, b)` | Return items from block `b` which match item match function `f` |
| `by-key(p?)` | Return item match function that checks predicate `p?` against the (symbol) key |
| `by-key-name(p?)` | Return item match function that checks predicate `p?` against string representation of the key |
| `by-key-match(re)` | Return item match function that checks string representation of the key matches regex `re` |
| `by-value(p?)` | Return item match runction that checks predicate `p?` against the item value |
| `match-filter-values(re, b)` | Return list of values from block `b` with keys matching regex `re` |
| `filter-values(p?, b)` | Return items from block `b` where values match predicate `p?` |

## Block Alteration

| Function | Description |
|----------|-------------|
| `alter-value(k, v, b)` | Alter `b.k` to value `v` |
| `update-value(k, f, b)` | Update  `b.k` to `f(b.k)` |
| `alter(ks, v, b)` | In nested block `b` alter value to value `v` at path-of-keys `ks` |
| `update(ks, f, b)` | In nested block `b` applying `f` to value at path-of-keys `ks` |
| `update-value-or(k, f, d, b)` | Set `b.k` to `f(v)` where v is current value, otherwise add with default value `d` |
| `set-value(k, v)` | Set `b.k` to `v`, adding if absent |
| `tongue(ks, v)` | Construct block with a single nested path-of-keys `ks` down to value `v` |
| `merge-at(ks, v, b)` | Shallow merge block `v` into block value at path-of-keys `ks` |

## Deep Find and Query

| Function | Description |
|----------|-------------|
| `deep-find(k, b)` | Return list of all values for key `k` at any depth in block `b`, depth-first |
| `deep-find-first(k, d, b)` | Return first value for key `k` at any depth in block `b`, or default `d` |
| `deep-find-paths(k, b)` | Return list of key paths to all occurrences of key `k` at any depth in block `b` |
| `deep-query(pattern, b)` | Query block `b` using dot-separated pattern string. `*` matches one level, `**` matches any depth. Bare `foo` is sugar for `**.foo` |
| `deep-query-first(pattern, d, b)` | Return first match for `pattern` in block `b`, or default `d` |
| `deep-query-paths(pattern, b)` | Return list of key paths matching `pattern` in block `b` |

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
