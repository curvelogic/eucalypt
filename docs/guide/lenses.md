# Lenses

In this chapter you will learn:

- What a lens is and what problem it solves
- How to read, update, and transform nested data with `view` and `over`
- What a traversal is and how it generalises lenses to multiple foci
- How to compose lenses and traversals with `∘` and the `‹›` shorthand
- How to operate on all foci as a group with `parts-of`

## The Problem: Updating Nested Data

Reading a value deep inside a structure is easy with dot-lookup:

```eu,notest
config.server.db.host   # => "localhost"
```

But modifying it is painful. If you want to change just that one field
and keep the rest of the structure intact, you must manually reconstruct
every layer:

```eu,notest
config { server: config.server { db: config.server.db { host: "10.0.0.5" } } }
```

This is verbose, error-prone, and breaks whenever the structure changes.
Lenses solve this.

## Importing the Lens Library

Lenses are not built into the prelude — they live in `lib/lens.eu`:

```eu,notest
{ import: "lens.eu" }
```

All functions described in this chapter come from that import.

## What Is a Lens?

A **lens** is a reusable description of a *position* inside a data structure.
Once you have a lens, you can:

- **Read** the value at that position with `view`
- **Replace** it with a new value using `over` and the `->` const operator
- **Transform** it with a function using `over`

Crucially, `over` always returns the *complete* updated structure — you never
lose the surrounding data.

```eu,notest
{ import: "lens.eu" }

config: { server: { db: { host: "localhost", port: 5432 } } }

# Read with view
host: config view(‹:server :db :host›)
# => "localhost"

# Replace with over (note: -> discards the old value)
migrated: config over(‹:server :db :host›, -> "10.0.0.5")
# => { server: { db: { host: "10.0.0.5", port: 5432 } } }

# Transform with over
bumped: config over(‹:server :db :port›, + 1)
# => { server: { db: { host: "localhost", port: 5433 } } }
```

The lens `‹:server :db :host›` is defined *once* and can be reused for any
combination of reading and modifying. Everything else in `config` — the
`:port`, other sibling keys — is preserved automatically.

## Building Lenses

### The `at` Constructor

`at(key)` creates a lens that focuses on a block value at the given symbol key:

```eu,notest
{ import: "lens.eu" }

data: { name: "Alice", score: 95 }

data view(at(:name))            # => "Alice"
data over(at(:score), + 5)      # => { name: "Alice", score: 100 }
```

### The `ix` Constructor

`ix(n)` creates a lens that focuses on a list element at index `n` (zero-based):

```eu,notest
{ import: "lens.eu" }

items: ["a", "b", "c"]

items view(ix(1))               # => "b"
items over(ix(0), str.to-upper) # => ["A", "b", "c"]
```

### The `item` Constructor

`item(pred)` focuses on the *first* list element matching a predicate:

```eu,notest
{ import: "lens.eu" }

records: [{id: 1, value: "a"}, {id: 2, value: "b"}]

records view(item(_.id = 2))    # => {id: 2, value: "b"}
records over(item(_.id = 1) ∘ at(:value), str.to-upper)
# => [{id: 1, value: "A"}, {id: 2, value: "b"}]
```

## Composing Lenses

Compose lenses with `∘` (right-to-left) to navigate deeper into a structure.
A lens composed with another lens is still a lens:

```eu,notest
{ import: "lens.eu" }

# Navigate to a nested field step by step
at(:server) ∘ at(:db) ∘ at(:host)

# Shorter: the ‹› bracket shorthand
# Symbols become at(), numbers become ix()
‹:server :db :host›
‹:items 0 :meta :title›    # at(:items) ∘ ix(0) ∘ at(:meta) ∘ at(:title)
```

The `‹›` shorthand accepts:
- **Symbols** — converted to `at(sym)`
- **Numbers** — converted to `ix(n)`
- **Lens values** — used directly (e.g. `item(pred)`, `element(pred)`)

Mixed paths are fine:

```eu,notest
# Lens function inside a path
‹element(by-key(_ = :b)) _value›
# => element(by-key(_ = :b)) ∘ _value
```

## Traversals

A **traversal** generalises a lens to focus on *multiple* positions.
`over` applies a function at each focus; `to-list-of` collects all
foci into a list.

### The `each` Traversal

`each` traverses all elements of a list:

```eu,notest
{ import: "lens.eu" }

items: [1, 2, 3, 4, 5]

items to-list-of(each)          # => [1, 2, 3, 4, 5]
items over(each, * 2)           # => [2, 4, 6, 8, 10]
```

Compose `each` with `at` to reach a field inside every element:

```eu,notest
{ import: "lens.eu" }

records: [{name: "alice", score: 10}, {name: "bob", score: 20}]

# Collect all scores
records to-list-of(each ∘ at(:score))     # => [10, 20]

# Double all scores (whole list returned, :name untouched)
records over(each ∘ at(:score), * 2)
# => [{name: "alice", score: 20}, {name: "bob", score: 40}]
```

### The `filtered` Traversal

`filtered(pred)` traverses only the list elements that satisfy the predicate.
Non-matching elements are left untouched by `over`:

```eu,notest
{ import: "lens.eu" }

[1, 2, 3, 4, 5] over(filtered(_ > 3), negate)
# => [1, 2, 3, -4, -5]

[1, 2, 3, 4, 5] to-list-of(filtered(_ > 3))
# => [4, 5]
```

### Block Traversals

For blocks, `each-element` traverses all `[key, value]` pairs, and
`filtered-elements(pred)` traverses only those matching the predicate.
Pair with `_value` (focus on index 1) or `_key` (focus on index 0):

```eu,notest
{ import: "lens.eu" }

data: {a: 1, b: 2, c: 3}

# Collect all values
data to-list-of(each-element ∘ _value)         # => [1, 2, 3]

# Double all values
data over(each-element ∘ _value, * 10)          # => {a: 10, b: 20, c: 30}

# Only values greater than 1
data over(filtered-elements(by-value(_ > 1)) ∘ _value, negate)
# => {a: 1, b: -2, c: -3}
```

## Composing Lenses and Traversals

A lens composed with a traversal yields a traversal. This lets you
navigate into a specific part of a structure and then traverse *within*
that part:

```eu,notest
{ import: "lens.eu" }

data: {x: [{y: 1}, {y: 2}, {y: 3}]}

# Lens into :x, then traverse each element, then lens into :y
data to-list-of(at(:x) ∘ each ∘ at(:y))
# => [1, 2, 3]

data over(at(:x) ∘ each ∘ at(:y), * 10)
# => {x: [{y: 10}, {y: 20}, {y: 30}]}
```

The same applies to `filtered`:

```eu,notest
{ import: "lens.eu" }

data: {x: [{y: 1}, {y: 2}, {y: 3}]}

# Only y values greater than 1
data to-list-of(at(:x) ∘ filtered(_.y > 1) ∘ at(:y))
# => [2, 3]
```

## Operating on All Foci as a Group: `parts-of`

Sometimes you want to treat all the traversal foci as a *single list*
and apply a list-level operation — sorting, reversing, replacing with a
given list. `parts-of(traversal)` turns a traversal into a lens that
focuses on the list of all foci.

```eu,notest
{ import: "lens.eu" }

# Sort all elements
[3, 1, 4, 1, 5] over(parts-of(each), sort-nums)
# => [1, 1, 3, 4, 5]

# Reverse only the filtered elements; non-matching elements stay put
[1, 4, 2, 5, 3] over(parts-of(filtered(> 2)), reverse)
# => [1, 3, 2, 5, 4]

# Collect: view on parts-of is the same as to-list-of
[1, 2, 3] view(parts-of(each))    # => [1, 2, 3]

# Replace all foci with a new list
[1, 2, 3] over(parts-of(each), -> [10, 20, 30])   # => [10, 20, 30]
```

Works equally well on deep compositions:

```eu,notest
{ import: "lens.eu" }

data: {x: [{y: 1}, {y: 2}, {y: 3}]}

data over(parts-of(at(:x) ∘ each ∘ at(:y)), reverse)
# => {x: [{y: 3}, {y: 2}, {y: 1}]}
```

## Quick Reference

| Function | Description |
|----------|-------------|
| `view(lens, data)` | Read the focused value (lens only, not traversal) |
| `over(optic, fn, data)` | Apply `fn` at each focus; return whole structure |
| `to-list-of(optic, data)` | Collect all foci into a list |
| `parts-of(traversal)` | Lens on the list of all foci |

| Constructor | Type | Description |
|-------------|------|-------------|
| `at(key)` | Lens | Block value at symbol key |
| `ix(n)` | Lens | List element at index `n` |
| `item(p?)` | Lens | First list element matching predicate |
| `element(p?)` | Lens | First block `[key, value]` pair matching predicate |
| `_value` | Lens | Value (index 1) of a `[key, value]` pair |
| `_key` | Lens | Key (index 0) of a `[key, value]` pair |
| `each` | Traversal | All list elements |
| `filtered(p?)` | Traversal | List elements matching predicate |
| `each-element` | Traversal | All block `[key, value]` pairs |
| `filtered-elements(p?)` | Traversal | Block pairs matching predicate |

**Composition**: `∘` (right-to-left). `‹sym num ...›` shorthand converts
symbols to `at()` and numbers to `ix()`.

## Type Checker Integration

The type checker understands `Lens(a, b)` and `Traversal(a, b)` as
opaque types. Passing a traversal to `view` triggers a type warning —
use `to-list-of` instead. Composing optics with incompatible inner and
outer types also produces a warning.

```eu,notest
` { type: "Lens(a, b) → a → b" }
view: ...

# Type warning: each is Traversal([a], a), not Lens
[1, 2, 3] view(each)       # warning: use to-list-of for traversals
```

See [Navigating Nested Data](navigating-nested-data.md) for a tutorial
that combines lenses with `~` safe navigation and `match?` pattern
matching, and the
[Agent Reference Section 11](../reference/agent-reference.md#11-lens-library-liblenseu)
for a terse all-in-one summary.
