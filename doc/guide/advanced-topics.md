# Advanced Topics

In this chapter you will learn:

- How the metadata system works
- How to use sets for unique collections
- How to search deep structures with `deep-find` and `deep-query`
- Format specifiers and formatting techniques
- Type predicates and other utilities

## The Metadata System

Every value in eucalypt can carry **metadata**: a block of additional
information that does not appear in the rendered output but can be
inspected and used programmatically.

### Attaching Metadata

Use the `//` operator to attach metadata to a value:

```eu
answer: 42 // { note: "the answer to everything" }
```

The value `42` is rendered normally; the metadata is hidden:

```yaml
answer: 42
```

### Reading Metadata

Use `meta` to retrieve the metadata block:

```eu
answer: 42 // { note: "the answer to everything" }
note: meta(answer).note
```

```yaml
answer: 42
note: the answer to everything
```

### Deep Merge Metadata

Use `//<< ` to deep-merge additional metadata onto existing metadata:

```eu
x: 1 // { a: 1 }
y: x //<< { b: 2 }
result: meta(y)
```

```yaml
x: 1
y: 1
result:
  a: 1
  b: 2
```

### Declaration Metadata

Declaration metadata (written with the backtick syntax) is separate
from value metadata. It controls how eucalypt processes the
declaration:

```eu
` { doc: "A helper" export: :suppress }
helper(x): x + 1
```

Key metadata properties:
- `doc` -- documentation string
- `export: :suppress` -- hide from output
- `target` -- mark as an export target
- `import` -- import other files
- `associates` / `precedence` -- operator fixity (see
  [Operators](operators.md) for details)

### YAML Tags

Metadata can carry a `tag` key which renders as a YAML tag:

```eu
ref: :my-resource // { tag: "!Ref" }
```

```yaml
ref: !Ref my-resource
```

This is useful for generating CloudFormation, Kubernetes, and other
tagged YAML formats.

### Assertions with `//=` and `//=>`

The `//=` operator asserts equality at runtime:

```eu
result: 2 + 2 //= 4  # panics if not equal
```

The `//=>` operator additionally stores the assertion as metadata:

```eu
checked: 2 + 2 //=> 4
m: meta(checked)  # contains the assertion
```

## Sets

Sets are unordered collections of unique values, provided by the
`set` namespace.

### Creating Sets

```sh
eu -e '[1, 2, 2, 3, 3, 3] set.from-list set.to-list'
```

```yaml
- 1
- 2
- 3
```

Duplicates are removed and elements are sorted.

The empty set is written as `∅`:

```sh
eu -e '∅ set.to-list'
```

```yaml
[]
```

### Membership and Size

```sh
eu -e '[1, 2, 3] set.from-list set.contains?(2)'
```

```yaml
true
```

```sh
eu -e '[1, 2, 3] set.from-list set.size'
```

```yaml
3
```

```sh
eu -e '∅ set.empty?'
```

```yaml
true
```

### Adding and Removing

```sh
eu -e '∅ set.add(1) set.add(2) set.add(1) set.to-list'
```

```yaml
- 1
- 2
```

```sh
eu -e '[1, 2, 3] set.from-list set.remove(2) set.to-list'
```

```yaml
- 1
- 3
```

### Set Algebra

**Union:**

```eu
a: [1, 2] set.from-list
b: [2, 3] set.from-list
result: a set.union(b) set.to-list
```

```yaml
result:
- 1
- 2
- 3
```

**Intersection:**

```eu
a: [1, 2, 3] set.from-list
b: [2, 3, 4] set.from-list
result: a set.intersect(b) set.to-list
```

```yaml
result:
- 2
- 3
```

**Difference:**

```eu
a: [1, 2, 3] set.from-list
b: [2, 3] set.from-list
result: a set.diff(b) set.to-list
```

```yaml
result:
- 1
```

## Deep Find

`deep-find` recursively searches a nested block structure for all
values associated with a given key name:

```sh
eu -e 'deep-find("host", { server: { host: "10.0.0.1" db: { host: "10.0.0.2" } } })'
```

```yaml
- 10.0.0.1
- 10.0.0.2
```

### `deep-find-first`

Return just the first match, or a default:

```sh
eu -e 'deep-find-first("host", "unknown", { server: { host: "10.0.0.1" } })'
```

```yaml
10.0.0.1
```

### `deep-find-paths`

Return the key paths to each match:

```sh
eu -e 'deep-find-paths("host", { server: { host: "a" db: { host: "b" } } })'
```

```yaml
- - server
  - host
- - server
  - db
  - host
```

## Deep Query

`deep-query` provides a more powerful pattern-based search using
dot-separated patterns with wildcards.

### Bare Key (Recursive Search)

A bare key name searches recursively (equivalent to `**.key`):

```sh
eu -e 'deep-query("port", { web: { port: 80 } db: { port: 5432 } })'
```

```yaml
- 80
- 5432
```

### Dotted Path

A dotted path matches a specific path:

```sh
eu -e 'deep-query("server.host", { server: { host: "10.0.0.1" port: 80 } })'
```

```yaml
- 10.0.0.1
```

### Wildcard `*`

`*` matches exactly one level:

```sh
eu -e 'deep-query("*.port", { web: { port: 80 } db: { port: 5432 } name: "app" })'
```

```yaml
- 80
- 5432
```

### Double Wildcard `**`

`**` matches any depth:

```sh
eu -e 'deep-query("config.**.port", { config: { port: 9090 nested: { deep: { port: 3000 } } } })'
```

```yaml
- 9090
- 3000
```

### Variants

- `deep-query-first(pattern, default, block)` -- first match or
  default
- `deep-query-paths(pattern, block)` -- key paths of matches

## Type Predicates

Test the type of a value:

```sh
eu -e '{ a: 1 } block?'
```

```yaml
true
```

```sh
eu -e '[1, 2] list?'
```

```yaml
true
```

## Sorting

### `qsort`

Sort with a custom comparison:

```sh
eu -e '["banana", "apple", "cherry"] qsort(<)'
```

```yaml
- apple
- banana
- cherry
```

Sort by a derived key:

```eu
words: ["one", "two", "three", "four", "five", "six"]
by-length: words qsort({
  lhs: • rhs: •
}.( (lhs str.letters count) < (rhs str.letters count) ))
```

```yaml
words:
- one
- two
- three
- four
- five
- six
by-length:
- one
- two
- six
- four
- five
- three
```

### `sort-nums`

Sort numbers in ascending order:

```sh
eu -e '[30, 10, 20] sort-nums'
```

```yaml
- 10
- 20
- 30
```

## Grouping

### `group-by`

Group list elements by a key function:

```eu
items: [
  { type: "fruit" name: "apple" }
  { type: "veg" name: "carrot" }
  { type: "fruit" name: "banana" }
]

grouped: items group-by(_.type)
```

```yaml
items:
- type: fruit
  name: apple
- type: veg
  name: carrot
- type: fruit
  name: banana
grouped:
  fruit:
  - type: fruit
    name: apple
  - type: fruit
    name: banana
  veg:
  - type: veg
    name: carrot
```

## Format Specifiers

Format specifiers in interpolation control output formatting. They use
printf-style codes after a colon inside the interpolation braces.

```eu
results: {
  padded: "{42:%06d}"
  float: "{3.14159:%.2f}"
  hex: "{255:%x}"
}
```

```yaml
results:
  padded: '000042'
  float: '3.14'
  hex: ff
```

### Available Format Codes

| Code | Description | Example |
|------|-------------|---------|
| `%d`, `%i` | Signed decimal integer | `{42:%d}` → `42` |
| `%u` | Unsigned decimal integer | `{42:%u}` → `42` |
| `%o` | Octal | `{255:%o}` → `377` |
| `%x`, `%X` | Hexadecimal (lower/upper) | `{255:%x}` → `ff` |
| `%f`, `%F` | Decimal notation | `{3.14:%.1f}` → `3.1` |
| `%e`, `%E` | Scientific notation | `{1000:%e}` → `1e3` |
| `%g`, `%G` | Auto (decimal or scientific) | `{0.001:%g}` → `0.001` |
| `%s` | String | `{:hello:%s}` → `hello` |

### Flags and Modifiers

- `%-` — left-align
- `%+` — prepend `+` for positive numbers
- `%0` — zero-padding (e.g. `%06d`)
- `%#` — alternate form (e.g. `0x` prefix for hex)
- Width: `%10s` — minimum field width
- Precision: `%.2f` — decimal places for floats

See also the [Strings and Text](string-interpolation.md) chapter for
more on string interpolation.

## Version Assertions

Assert a minimum version of `eu`:

```eu
_ : eu.requires(">=0.3.0")
```

Access build metadata:

```eu
info: {
  version: eu.build.version
  prelude: eu.prelude.version
}
```

## Key Concepts

- **Metadata** (`//`) attaches hidden information to values; `meta`
  retrieves it
- **Sets** (`set.*`) provide unique collections with union,
  intersection, and difference
- **Deep find** recursively searches nested blocks by key name
- **Deep query** supports pattern-based search with `*` and `**`
  wildcards
- **Type predicates** (`block?`, `list?`) test value types
- `qsort` sorts with custom comparators; format specifiers control
  numeric output
- `eu.requires` asserts a minimum version for compatibility
