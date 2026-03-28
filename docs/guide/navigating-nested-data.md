# Navigating Nested Data

In this chapter you will learn:

- How to safely access keys that may not exist using `~`
- How to check data shape with `match?`
- How to use lenses and traversals for deep get/set operations
- When to use each approach

## The Problem

Structured data from YAML, JSON, or TOML is often deeply nested,
with optional fields that may or may not be present. Eucalypt
provides several complementary tools for working with this data,
from simple safe navigation to powerful lens-based transformations.

## Safe Navigation with `~`

The dot operator (`.`) looks up a key in a block but errors if
the key is missing or the value is not a block. The `~` operator
is the safe alternative: it returns `null` instead of erroring.

```eu
config: { server: { host: "localhost", port: 5432 } }

# Dot lookup — errors if key missing
host: config.server.host

# Safe navigation — returns null if any step fails
host2: config ~ :server ~ :host
missing: config ~ :cache ~ :host
```

```yaml
config:
  server:
    host: localhost
    port: 5432
host: localhost
host2: localhost
missing: ~
```

`~` takes a symbol on the right and returns the value at that key
if the left-hand side is a block containing the key, or `null`
otherwise. Crucially, `null ~ :anything` returns `null`, so chains
propagate failure naturally.

### Sections for Pipelines

Because `~` is a regular operator (left-associative, precedence
90), sections work:

```eu
people: [
  { name: "Alice", email: "alice@example.com" }
  { name: "Bob" }
  { name: "Charlie", email: "charlie@example.com" }
]

emails: people map(~ :email)
have-email: people filter(_ ~ :email ✓)
```

```yaml
people:
- name: Alice
  email: alice@example.com
- name: Bob
- name: Charlie
  email: charlie@example.com
emails:
- alice@example.com
- ~
- charlie@example.com
have-email:
- name: Alice
  email: alice@example.com
- name: Charlie
  email: charlie@example.com
```

### Mixing `~` and `.`

Both `~` and `.` have the same precedence (90). Use `.` for keys
you know exist and `~` for keys that might not:

```eu,notest
# Known structure, optional leaf
config.server.db ~ :ssl-cert

# Entirely optional path
config ~ :cache ~ :host
```

If `~` returns `null` and you then use `.` on it, you get an error
— `.` is not safe. Use `~` for the entire uncertain portion of the
path.

## Structural Matching with `match?`

`match?(pattern, target)` checks whether a value conforms to a structural
pattern, returning `true` or `false`. With a single block or list
argument, use juxtaposed call syntax: `match?{...}` or `match?[...]`.

### Pattern Language

Pattern values are interpreted by type:

| Pattern value | Interpretation |
|---------------|----------------|
| Block | Sub-pattern: keys must exist, values matched recursively |
| List | Positional sub-pattern: length must match, elements matched |
| Function | Applied as predicate |
| Literal | Exact equality check |

```eu
data: { host: "10.0.0.1", port: 8080, name: "api" }

# Key existence (any? matches any value)
has-host: data match?{host: any?}

# Value predicates
high-port: data match?{port: (> 1000)}

# Type checking
typed: data match?{host: string?, port: number?}

# Exact literal
specific: data match?{port: 8080}

# Nested sub-patterns
nested: {server: data} match?{server: {host: any?}}
```

```yaml
data:
  host: 10.0.0.1
  port: 8080
  name: api
has-host: true
high-port: true
typed: true
specific: true
nested: true
```

### Open Matching

Block patterns are *open*: extra keys in the target are ignored.
`{host: any?}` matches any block that has a `:host` key, regardless
of what other keys it contains.

### List Patterns

List patterns check length and match elements positionally:

```eu
pair: [1, "hello"] match?[number?, string?]
wrong-len: [1, 2, 3] match?[any?, any?]
```

```yaml
pair: true
wrong-len: false
```

### Composing `match?` with `when` and `filter`

`match?` is pattern-first, so `match?{pattern}` is a partially
applied predicate — perfect for `filter` and `when`:

```eu
servers: [
  { host: "10.0.0.1", port: 8080 }
  { name: "cache" }
  { host: "10.0.0.2", port: 5432 }
]

with-host: servers filter(match?{host: any?})
```

```yaml
servers:
- host: 10.0.0.1
  port: 8080
- name: cache
- host: 10.0.0.2
  port: 5432
with-host:
- host: 10.0.0.1
  port: 8080
- host: 10.0.0.2
  port: 5432
```

Use `when` for conditional transformation — apply a function only
when the data matches a pattern, otherwise pass through unchanged:

```eu,notest
data when(match?{host: any?, port: any?}, .("{host}:{port}"))
```

### Deep Structural Queries

Combine `match?` with `deep-fold` to find matching nodes at any
depth:

```eu,notest
# Find all blocks with both host and port keys
find-endpoints(data): {
  emit(s, v): if(v match?{host: any?, port: any?}, [v], [])
  next(s, k): null
}.(deep-fold(emit, next, null, data))
```

## Lenses and Traversals

With `~` and `.` you can *read* nested data easily. But what if you
need to *update* a value deep inside a structure and get the whole
structure back? Dot-lookup gives you the value but loses the
surrounding context. To change the title of the second item in a
list nested three levels deep, you'd have to manually reconstruct
every layer of the structure around the changed value.

Lenses solve this. A lens is a reusable description of a position
within a data structure. Once defined, the same lens can *get*
the value at that position, *set* it to a new value, or *modify*
it with a function — always returning the complete updated structure.

```eu,notest
{ import: "lens.eu" }
```

### Define Once, Use for Get and Set

```eu,notest
{ import: "lens.eu" }

# Define a lens once — it describes WHERE to look, not WHAT to do
db-host: ‹:server :db :host›

config: { server: { db: { host: "localhost", port: 5432 }, cache: { host: "redis" } } }

# Read with view
current: config view(db-host)
# => "localhost"

# Update with over (returns the WHOLE config, not just the changed part)
migrated: config over(db-host, -> "10.0.0.5")
# => { server: { db: { host: "10.0.0.5", port: 5432 }, cache: { host: "redis" } } }
```

The key insight: `db-host` is defined once and describes the path
`:server` → `:db` → `:host`. You can use it with `view` to read or
`over` to modify. The surrounding structure (`:port`, `:cache`, etc.)
is preserved automatically.

### Lens Constructors

`at(key)` focuses on a block key; `ix(n)` focuses on a list index.
Compose with `∘` (read right to left) or use the `‹›` bracket
shorthand:

```eu,notest
{ import: "lens.eu" }

# These are equivalent:
at(:server) ∘ at(:db) ∘ at(:host)
‹:server :db :host›

# Mix keys and indices:
‹:items 0 :meta :title›    # items[0].meta.title
```

### Traversals

Traversals extend lenses to focus on *multiple* positions. `each`
targets all list elements; `filtered(pred)` targets only matching
ones. Crucially, they compose with lenses — so you can target a
specific field across every element:

```eu,notest
{ import: "lens.eu" }

records: [{name: "a", score: 10}, {name: "b", score: 20}, {name: "c", score: 30}]

# Define a traversal: the score of every record
all-scores: each ∘ at(:score)

# Collect all scores
scores: records to-list-of(all-scores)
# => [10, 20, 30]

# Double all scores (returns the whole list with scores updated)
boosted: records over(all-scores, * 2)
# => [{name: "a", score: 20}, {name: "b", score: 40}, {name: "c", score: 60}]

# Cap high scores only
high-scores: filtered(_.score > 15) ∘ at(:score)
capped: records over(high-scores, -> 15)
# => [{name: "a", score: 10}, {name: "b", score: 15}, {name: "c", score: 15}]
```

Again, the traversal is defined once and reused. The surrounding
structure (`:name` fields, list positions, non-matching elements)
is preserved through every transformation.

### Tip: `->` for Setting Values

`over` takes a function to apply to the focused value. When you
want to *replace* rather than *transform*, use the `->` const
operator — `-> value` ignores its argument and returns `value`:

```eu,notest
# Set db host to a fixed value (-> discards the old value)
config over(‹:server :db :host›, -> "10.0.0.5")

# Clear all scores
records over(each ∘ at(:score), -> 0)

# Also useful with when — replace matching data entirely
data when(match?{status: "draft"}, -> {status: "published"})
```

### Lens Consumers

| Function | Description |
|----------|-------------|
| `view(lens, data)` | Extract the focused value (single-focus lenses only) |
| `over(lens, fn, data)` | Apply `fn` at each focus, return whole structure |
| `to-list-of(traversal, data)` | Collect all foci into a list |

### Lens and Traversal Constructors

| Constructor | Description |
|-------------|-------------|
| `at(key)` | Focus on block value at symbol key |
| `ix(n)` | Focus on list element at index n |
| `item(pred)` | Focus on first list element matching predicate |
| `element(pred)` | Focus on first `[key, value]` pair matching predicate |
| `each` | Traverse all list elements |
| `filtered(pred)` | Traverse list elements matching predicate |
| `_value` | Focus on value of a `[key, value]` pair |
| `_key` | Focus on key of a `[key, value]` pair |

## Choosing the Right Tool

| Task | Tool | Example |
|------|------|---------|
| Access an optional key | `~` | `data ~ :host` |
| Chain through optional keys | `~` chain | `data ~ :server ~ :host` |
| Check if data has a shape | `match?` | `data match?{host: any?}` |
| Filter by shape | `match?` + `filter` | `items filter(match?{host: any?})` |
| Conditional transform | `match?` + `when` | `data when(match?{host: any?}, f)` |
| Find by key at any depth | `deep-find` | `data deep-find(:host)` |
| Get a value deep in a known structure | `view` + lens | `data view(‹:server :host›)` |
| Modify a value deep in a structure | `over` + lens | `data over(‹:server :port›, + 1)` |
| Transform all elements | `over` + `each` | `items over(each ∘ at(:name), str.upper)` |
| Collect values from all elements | `to-list-of` + `each` | `items to-list-of(each ∘ at(:name))` |

**Rules of thumb:**

- Use `~` for quick, safe extraction where you don't need to
  modify the structure.
- Use `match?` when you need to check shape before acting.
- Use lenses when you need to *modify* values deep inside a
  structure and get the whole structure back.
- Use `deep-find` / `deep-fold` when you need to search at
  arbitrary depth.
