# Frequently Asked Questions

## Getting Started

### How do I install eucalypt?

On macOS, use Homebrew:

```sh
brew install curvelogic/homebrew-tap/eucalypt
```

On other platforms, download a binary from the
[GitHub releases](https://github.com/curvelogic/eucalypt/releases)
page, or build from source with `cargo install --path .`.

Verify installation with:

```sh
eu version
```

### How do I convert between data formats?

Pass a file in one format and specify the output format with `-x` or
`-j`:

```sh
# YAML to JSON
eu data.yaml -j

# JSON to YAML (default output)
eu data.json

# YAML to TOML
eu data.yaml -x toml
```

### What data formats does eucalypt support?

**Input formats**: YAML, JSON, JSON Lines (jsonl), TOML, EDN, XML,
CSV, plain text, and eucalypt's own `.eu` syntax.

**Output formats**: YAML (default), JSON, TOML, EDN, and plain text.

**Streaming input formats** (for large files): `jsonl-stream`,
`csv-stream`, `text-stream`.

### How do I use eucalypt in a pipeline?

`eu` reads from stdin by default when used in a pipe and writes to
stdout:

```sh
# Filter JSON from an API
curl -s https://api.example.com/data | eu -e 'items filter(_.active)'

# Transform and re-export
cat data.yaml | eu transform.eu -j > output.json
```

Use `-e` to specify an expression to evaluate against the input data.

### How do I pass arguments to a eucalypt program?

Use `--` to separate `eu` flags from program arguments:

```sh
eu program.eu -- arg1 arg2 arg3
```

Inside your program, access them via `io.args`:

```eu
name: io.args head-or("World")
greeting: "Hello, {name}!"
```

## Language

### How do functions work in eucalypt?

Define functions with a parameter list after the name:

```eu
double(x): x * 2
result: double(21) //=> 42
```

Functions are curried -- applying fewer arguments than expected returns
a partially applied function:

```eu
add(x, y): x + y
increment: add(1)
result: increment(9) //=> 10
```

### What is catenation?

Catenation is eucalypt's pipeline syntax. Writing `x f` applies `f` to
`x` as a single argument:

```eu
add-one(x): x + 1
result: 5 add-one //=> 6
```

Chain multiple transforms by writing them in sequence:

```eu
double(x): x * 2
add-one(x): x + 1
result: 5 double add-one //=> 11
```

This reads left to right: start with 5, double it (10), add one (11).

### What are anaphora and when should I use them?

Anaphora are implicit parameters that let you define simple functions
without naming them. There are three kinds:

**Expression anaphora** (`_`, `_0`, `_1`): turn an expression into a
function.

```eu
squares: [1, 2, 3] map(_0 * _0) //=> [1, 4, 9]
```

**String anaphora** (`{}`, `{0}`, `{1}`): turn a string template into
a function.

```eu
labels: [1, 2, 3] map("item-{}") //=> ["item-1", "item-2", "item-3"]
```

**Block anaphora** (`•`, `•0`, `•1`): turn a block into a function.

Use anaphora for simple, readable cases. For anything more complex,
prefer a named function. See [Anaphora](guide/anaphora.md) for details.

### Why is there no lambda syntax?

Eucalypt deliberately omits lambda expressions. Instead, use:

1. **Named functions** for anything non-trivial
2. **Anaphora** (`_`, `{}`) for simple one-liners
3. **Sections** (`(+ 1)`, `(* 2)`) for operator-based functions
4. **Partial application** (`add(1)`) for curried functions

```eu
# All equivalent ways to add one:
add-one(x): x + 1
result1: [1, 2, 3] map(add-one) //=> [2, 3, 4]
result2: [1, 2, 3] map(_ + 1) //=> [2, 3, 4]
result3: [1, 2, 3] map(+ 1) //=> [2, 3, 4]
```

### How does block merging work?

When you write one block after another (catenation), they merge:

```eu
base: { a: 1 b: 2 }
overlay: { b: 3 c: 4 }
merged: base overlay //=> { a: 1 b: 3 c: 4 }
```

The second block's values override the first. This is a **shallow**
merge. For recursive deep merge, use the `<<` operator:

```eu
base: { x: { a: 1 b: 2 } }
extra: { x: { c: 3 } }
result: base << extra
```

### How do I handle the lookup precedence gotcha?

The `.` (lookup) operator has higher precedence than catenation, so
`xs head.id` parses as `xs (head.id)`, not `(xs head).id`.

Use explicit parentheses:

```eu
data: [{ id: 1 }, { id: 2 }]
first-id: (data head).id //=> 1
```

See [Syntax Gotchas](appendices/syntax-gotchas.md) for more.

## Data Processing

### How do I filter and transform lists?

Use `map` to transform and `filter` to select:

```eu
numbers: [1, 2, 3, 4, 5, 6]
small: numbers filter(< 4) //=> [1, 2, 3]
doubled: numbers map(* 2) //=> [2, 4, 6, 8, 10, 12]
```

Combine them in a pipeline:

```eu
result: [1, 2, 3, 4, 5, 6] filter(> 3) map(* 10) //=> [40, 50, 60]
```

### How do I look up values in nested blocks?

Use chained `.` lookups for known paths:

```eu
config: { db: { host: "localhost" port: 5432 } }
host: config.db.host //=> "localhost"
```

For dynamic key lookup, use `lookup` with a symbol:

```eu
data: { name: "Alice" age: 30 }
field: data lookup(:name) //=> "Alice"
```

Use `lookup-or` to provide a default:

```eu
data: { name: "Alice" }
age: data lookup-or(:age, 0) //=> 0
```

### How do I search deeply nested data?

Use `deep-find` for recursive key search:

```eu,notest
# Finds all values for key :id at any depth
ids: data deep-find(:id)
```

Use `lookup-path` for a known sequence of keys:

```eu
data: { a: { b: { c: 42 } } }
result: data lookup-path([:a, :b, :c]) //=> 42
```

### How do I sort data?

Sort lists with `sort-nums` or `sort-strs`:

```eu
names: ["Charlie", "Alice", "Bob"]
sorted: names sort-strs //=> ["Alice", "Bob", "Charlie"]
```

```eu
nums: [5, 1, 3, 2, 4]
sorted: nums sort-nums //=> [1, 2, 3, 4, 5]
```

For sorting by a key, use `sort-by-str` or `sort-by-num`:

```eu
people: [{ name: "Zoe" age: 25 }, { name: "Amy" age: 30 }]
by-name: people sort-by-str(_.name)
youngest: (by-name head).name //=> "Amy"
```

### How do I work with dates?

Use `t"..."` literals for date-time values:

```eu
meeting: t"2024-03-15T14:30:00Z"
date-only: t"2024-03-15"
before: t"2024-01-01" < t"2024-12-31" //=> true
```

See [Date, Time, and Random Numbers](guide/date-time-random.md) for
parsing, formatting, and arithmetic.

## Advanced

### How do I attach metadata to declarations?

Use the backtick (`` ` ``) prefix:

```eu
` "Compute the square of a number"
square(x): x * x

result: square(5) //=> 25
```

Metadata can be a string (documentation) or a block with structured
data:

```eu,notest
` { doc: "Custom operator" associates: :left precedence: 75 }
(l <+> r): l + r
```

### How do imports work?

Imports are specified in declaration metadata using the `import` key:

```eu,notest
{ import: "helpers.eu" }

result: helper-function(42)
```

For named imports (scoped access):

```eu,notest
{ import: "cfg=config.eu" }

host: cfg.host
```

See [Import Formats](reference/import-formats.md) for the full syntax
including git imports.

### How do I write tests?

Use the `//=>` assertion operator to check values inline:

```eu
double(x): x * 2
result: double(21) //=> 42
```

If the assertion fails, eucalypt panics with a non-zero exit code.
Other assertion operators:

```eu
x: 5
check1: (x > 3) //!
check2: (x = 0) //!!
check3: x //=? pos?
```

### How do I generate random values?

Use `io.random` for a stream of random floats, or pass `--seed` for
reproducible output:

```eu,notest
roll: random-int(6, io.random)
die: roll.value + 1
```

```sh
eu --seed 42 game.eu
```

See [Random Numbers](reference/prelude/random.md) for the full API.

### What are sets and how do I use them?

The `set` namespace provides set operations. Convert lists to sets
with `set.from-list`:

```eu
sa: set.from-list([1, 2, 3, 4])
sb: set.from-list([3, 4, 5, 6])
common: sa set.intersect(sb) set.to-list //=> [3, 4]
combined: sa set.union(sb) set.to-list sort-nums //=> [1, 2, 3, 4, 5, 6]
diff: sa set.diff(sb) set.to-list sort-nums //=> [1, 2]
```
