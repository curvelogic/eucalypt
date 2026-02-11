# Agent Reference

Dense, example-heavy reference for AI coding agents working with
Eucalypt. All content verified against source. See the
[guide chapters](../guide/blocks-and-declarations.md) for narrative
explanations.

---

## 1. Syntax Reference

### 1.1 Primitives

| Type | Syntax | Examples |
|------|--------|----------|
| Integer | digits | `42`, `-7`, `0` |
| Float | digits with `.` | `3.14`, `-0.5` |
| String | `"..."` | `"hello"` |
| Raw string | `r"..."` | `r"C:\path"`, `r"^\d+"` |
| C-string | `c"..."` | `c"line\nbreak"`, `c"tab\there"` |
| T-string (ZDT) | `t"..."` | `t"2024-03-15"`, `t"2024-03-15T14:30:00Z"` |
| Symbol | `:name` | `:key`, `:active` |
| Boolean | keywords | `true`, `false` |
| Null | keyword | `null` |

**String escape sequences (c-strings only):** `\n` newline, `\t` tab,
`\r` carriage return, `\\` backslash, `\"` quote, `\{` `\}` literal
braces, `\xHH` hex byte, `\uHHHH` unicode, `\UHHHHHHHH` extended
unicode.

**String interpolation** (all string types except t-strings): embed
expressions with `{expr}`. Literal braces via `{{` and `}}`. Format
specifiers: `{value:%.2f}`, `{n:%06d}`.

Raw strings treat backslashes literally but still support `{}`
interpolation.

### 1.2 Collections

```eu,notest
# List (commas required)
[1, 2, 3]
[]
[1, "two", :three, true]

# Block (commas optional)
{ a: 1 b: 2 c: 3 }
{ a: 1, b: 2, c: 3 }
```

### 1.3 Declarations

| Form | Syntax | Notes |
|------|--------|-------|
| Property | `name: expr` | Defines a named value, rendered in output |
| Function | `f(x, y): expr` | Not rendered in output |
| Binary operator | `(l op r): expr` | Symbolic name required |
| Prefix operator | `(op x): expr` | Unary prefix |
| Postfix operator | `(x op): expr` | Unary postfix |

**Top-level unit**: the file itself is an implicit block without braces.

### 1.4 Comments

```eu,notest
# Line comment (to end of line)
x: 42  # inline comment
```

### 1.5 Metadata Annotations

Placed between a leading backtick and the declaration:

```eu,notest
# Documentation shorthand (string = doc metadata)
` "Adds two numbers"
add(x, y): x + y

# Structured metadata
` { doc: "Deep merge operator"
    associates: :left
    precedence: :append }
(l << r): deep-merge(l, r)

# Suppress from output
` :suppress
helper(x): x + 1

# Export target
` { target: :my-output }
output: { result: 42 }

# Mark as main (default) target
` :main
main: { result: 42 }
```

**Unit-level metadata**: if the first item in a file is an expression
(not a declaration), it becomes metadata for the entire unit:

```eu,notest
{ import: "helpers.eu" }
result: helper(42)
```

### 1.6 Function Application

```eu,notest
# Standard call (NO whitespace before paren)
f(x, y)

# Catenation / pipeline (single argument, becomes LAST param)
x f                    # = f(x)
x f g h                # = h(g(f(x)))
[1,2,3] map(inc)       # map(inc, [1,2,3])

# Partial application (all functions are curried)
add(1)                 # returns a function that adds 1
[1,2,3] map(+ 10)     # section: adds 10 to each

# Sections (operator with gaps filled by implicit params)
(+ 1)                  # function: _ + 1
(* 2)                  # function: _ * 2
(> 3)                  # function: _ > 3
(/)                    # function: _ / _ (two params)
```

### 1.7 Lookup and Generalised Lookup

```eu,notest
# Simple property lookup (dot, precedence 90)
block.key
config.db.host

# Generalised lookup (evaluate RHS in block scope)
{ a: 3 b: 4 }.(a + b)        # 7
{ a: 3 b: 4 }.[a, b]         # [3, 4]
{ a: 3 b: 4 }."{a} and {b}"  # "3 and 4"
```

**Warning:** `.` binds very tightly (precedence 90). `list head.name`
parses as `list (head.name)`, not `(list head).name`. Use parentheses:
`(list head).name`.

### 1.8 Anaphora (Implicit Parameters)

| Type | Numbered | Unnumbered | Scope |
|------|----------|------------|-------|
| Expression | `_0`, `_1`, `_2` | `_` (each `_` = new param) | Expression |
| Block | `•0`, `•1`, `•2` | `•` (each `•` = new param) | Block |
| String | `{0}`, `{1}`, `{2}` | `{}` (each `{}` = new param) | String literal |

```eu,notest
# Expression anaphora
[1,2,3] map(_0 * _0)          # square: [1,4,9]
[1,2,3] filter(_ > 1)         # [2,3]

# Block anaphora (• = bullet, Option-8 on Mac)
{ x: • y: • }                 # two-param function returning block
[[1,2],[3,4]] map({ x: • y: • } uncurry)

# Pseudo-lambda via block anaphora + generalised lookup
{ x: • y: • }.(x + y)         # anonymous two-param function

# String anaphora
[1,2,3] map("item: {}")       # ["item: 1", "item: 2", "item: 3"]
"{1},{0}"                      # two-param function, reversed order
```

**Important:** anaphora cannot be nested. For complex cases, use named
functions.

### 1.9 Imports

```eu,notest
# Unit-level import (available everywhere in file)
{ import: "lib.eu" }

# Named import (access as namespace)
{ import: "cfg=config.yaml" }
host: cfg.host

# Multiple imports
{ import: ["helpers.eu", "cfg=config.eu"] }

# Format override
{ import: "data=yaml@records.txt" }

# Scoped import (on specific declaration)
` { import: "math.eu" }
calculations: { result: advanced-calc(10) }

# Git import
{ import: { git: "https://github.com/user/lib"
            commit: "abc123def456"
            import: "helpers.eu" } }

# Streaming imports (lazy, for large files)
{ import: "events=jsonl-stream@events.jsonl" }
{ import: "rows=csv-stream@big.csv" }
{ import: "lines=text-stream@log.txt" }
```

### 1.10 Quoted Identifiers

Single quotes turn any character sequence into a normal identifier:

```eu,notest
home: {
  '.bashrc': false
  'notes.txt': true
}
z: home.'notes.txt'
```

---

## 2. Operator Precedence Table

All values verified against `src/core/metadata.rs` (`named_precedence`)
and `lib/prelude.eu` operator metadata.

From highest (tightest) to lowest binding:

| Prec | Name | Assoc | Operators | Description |
|------|------|-------|-----------|-------------|
| 95 | -- | prefix | `↑` | Head (tight prefix) |
| 90 | lookup | left | `.` (built-in) | Property lookup |
| 90 | call | left | (built-in) | Function call |
| 88 | bool-unary | prefix | `!`, `¬` | Boolean negation |
| 88 | -- | -- | `∘`, `;` | Composition (actual prec 88 from prelude) |
| 85 | exp | -- | `!!` (nth) | Indexing |
| 80 | prod | left | `*`, `/`, `%` | Multiplication, division, modulo |
| 75 | sum | left | `+`, `-` | Addition, subtraction |
| 60 | shift | -- | (shift ops) | Reserved |
| 55 | bitwise | -- | (bitwise ops) | Reserved |
| 50 | cmp | left | `<`, `>`, `<=`, `>=` | Comparison |
| 45 | append | left | `++` | List concatenation |
| 45 | append | left | `<<` | Deep merge |
| 42 | map | left | `<$>` | Functor map |
| 40 | eq | left | `=`, `!=` | Equality |
| 35 | bool-prod | left | `&&`, `∧` | Logical AND |
| 30 | bool-sum | left | `\|\|`, `∨` | Logical OR |
| 20 | cat | left | *(catenation)* | Juxtaposition / pipeline |
| 10 | apply | right | `@` | Function application |
| 5 | meta | left | `//`, `//<<`, `//=`, `//=>`, `//=?`, `//!?`, `//!`, `//!!` | Metadata and assertions |

**Named precedence levels** for use in operator metadata: `:lookup`,
`:call`, `:bool-unary`, `:exp`, `:prod`, `:sum`, `:shift`, `:bitwise`,
`:cmp`, `:append`, `:map`, `:eq`, `:bool-prod`, `:bool-sum`, `:cat`,
`:apply`, `:meta`.

**User-defined operators** default to left-associative, precedence 50
(`:cmp` level). Set custom values via metadata:

```eu,notest
` { associates: :right precedence: :sum }
(x +++ y): x + y
```

**Composition operators** `∘` and `;` are defined at precedence 88 in
the prelude (between bool-unary and exp):
- `f ∘ g` — compose right-to-left (`g` then `f`), right-associative
- `f ; g` — compose left-to-right (`f` then `g`), left-associative

---

## 3. Top 30 Prelude Functions

All signatures verified against `lib/prelude.eu`. Pipeline style shown
where idiomatic (catenated argument is always the **last** parameter).

### 3.1 List Functions

#### `map(f, l)` — transform each element

```
[1, 2, 3] map(inc)           # [2, 3, 4]
[1, 2, 3] map(* 10)          # [10, 20, 30]
["a","b"] map(str.to-upper)  # ["A", "B"]
```

#### `filter(p?, l)` — keep elements satisfying predicate

```
[1,2,3,4,5] filter(> 3)      # [4, 5]
[1,2,3,4,5] filter(pos?)     # [1, 2, 3, 4, 5]
```

#### `foldl(op, i, l)` — left fold

```
foldl(+, 0, [1,2,3,4,5])     # 15
[1,2,3] foldl(+, 0)          # ERROR: use foldl(+, 0, [1,2,3])
```

Note: `foldl` takes 3 args. In pipeline: the list is the last arg, so
`[1,2,3,4,5] foldl(+, 0)` does NOT work — you must call
`foldl(+, 0, [1,2,3,4,5])` directly, or partially apply:

```
sum: foldl(+, 0)
[1,2,3,4,5] sum              # 15
```

#### `foldr(op, i, l)` — right fold

```
foldr(cons, [], [1,2,3])      # [1, 2, 3] (identity)
foldr(++, [], [[1,2],[3,4]])  # [1, 2, 3, 4]
```

#### `head(xs)` — first element (panics if empty)

```
[10, 20, 30] head            # 10
```

#### `tail(xs)` — all but first (panics if empty)

```
[10, 20, 30] tail            # [20, 30]
```

#### `cons(h, t)` — prepend element to list

```
cons(0, [1, 2, 3])           # [0, 1, 2, 3]
```

#### `reverse(l)` — reverse a list

```
[1, 2, 3] reverse            # [3, 2, 1]
```

#### `count(l)` — number of elements

```
[10, 20, 30] count           # 3
```

#### `zip(l1, l2)` — pair elements from two lists

```
zip([:a,:b,:c], [1,2,3])     # [[:a,1], [:b,2], [:c,3]]
```

#### `concat(ls)` — flatten one level of nesting

```
concat([[1,2], [3], [4,5]])   # [1, 2, 3, 4, 5]
```

#### `mapcat(f, l)` — map then concatenate (flatMap)

```
["ab","cd"] mapcat(str.letters)  # ["a","b","c","d"]
```

#### `take(n, l)` — first n elements

```
[1,2,3,4,5] take(3)          # [1, 2, 3]
```

#### `drop(n, l)` — remove first n elements

```
[1,2,3,4,5] drop(3)          # [4, 5]
```

#### `any(p?, l)` — true if any element satisfies p?

```
[1, 2, 3] any(> 2)           # true
[1, 2, 3] any(> 5)           # false
```

#### `all(p?, l)` — true if all elements satisfy p?

```
[2, 4, 6] all(> 0)           # true
[2, 4, 6] all(> 3)           # false
```

### 3.2 Block Functions

#### `keys(b)` — list of keys (as symbols)

```
{ a: 1 b: 2 c: 3 } keys     # [:a, :b, :c]
```

#### `values(b)` — list of values

```
{ a: 1 b: 2 c: 3 } values   # [1, 2, 3]
```

#### `elements(b)` — list of [key, value] pairs

```
{ a: 1 b: 2 } elements       # [[:a, 1], [:b, 2]]
```

#### `merge(b1, b2)` — shallow merge (b2 overrides b1)

```
merge({ a: 1 }, { b: 2 })    # { a: 1 b: 2 }
{ a: 1 } { a: 2 }            # { a: 2 } (catenation = merge)
```

#### `has(s, b)` — does block have key s? (s is a symbol)

```
{ a: 1 b: 2 } has(:a)        # true
{ a: 1 b: 2 } has(:z)        # false
```

#### `lookup(s, b)` — look up key s (panics if missing)

```
{ a: 1 b: 2 } lookup(:b)     # 2
```

#### `lookup-or(s, d, b)` — look up key s with default d

```
{ a: 1 } lookup-or(:z, 99)   # 99
{ a: 1 } lookup-or(:a, 99)   # 1
```

#### `deep-find(k, b)` — find all values for key k at any depth

```
{ a: { x: 1 } b: { x: 2 } } deep-find("x")  # [1, 2]
```

#### `deep-query(pattern, b)` — query with dot-separated glob pattern

Patterns: bare `foo` = `**.foo`; `*` = one level; `**` = any depth.

```
{ a: { b: { c: 1 } } } deep-query("a.b.c")    # [1]
{ a: { x: 1 } b: { x: 2 } } deep-query("*.x") # [1, 2]
```

### 3.3 String Functions (str namespace)

#### `str.split-on(re, s)` — split string on regex

```
"one-two-three" str.split-on("-")      # ["one", "two", "three"]
"a.b.c" str.split-on("[.]")           # ["a", "b", "c"]
```

#### `str.join-on(sep, l)` — join list with separator

```
["a", "b", "c"] str.join-on(", ")     # "a, b, c"
```

#### `str.to-upper(s)` — convert to upper case

```
"hello" str.to-upper                   # "HELLO"
```

#### `str.to-lower(s)` — convert to lower case

```
"GOODBYE" str.to-lower                 # "goodbye"
```

#### `str.matches?(re, s)` — does regex match full string?

```
"hello" str.matches?("^h.*o$")        # true
"hello" str.matches?("^H")            # false
```

### 3.4 Combinators

See also: `identity(v)`, `const(k, _)`, `compose(f, g, x)`,
`flip(f, x, y)`, `complement(p?)`, `curry(f, x, y)`,
`uncurry(f, l)`, `cond(l, d)`.

---

## 4. Pipeline Patterns and Idioms

### 4.1 Basic Pipeline

Read left to right: data flows through transformations.

```eu,notest
[1, 2, 3, 4, 5]
  filter(> 2)
  map(* 10)
  reverse
# Result: [50, 40, 30]
```

### 4.2 Pipeline with Named Stages

Use `:suppress` to hide intermediate values from output:

```eu,notest
` :suppress
raw-data: [
  { name: "alice" score: 85 }
  { name: "bob" score: 92 }
  { name: "charlie" score: 78 }
]

result: raw-data
  filter(_.score >= 90)
  map(_.name)
  map(str.to-upper)
```

### 4.3 Conditional Pipeline

Use `then(true-val, false-val)` at the end of a pipeline:

```eu,notest
[1, 2, 3] count (> 2) then("many", "few")   # "many"
```

Or use `when(p?, f, x)` to conditionally transform:

```eu,notest
5 when(> 3, * 10)   # 50 (condition met, apply * 10)
2 when(> 3, * 10)   # 2  (condition not met, pass through)
```

### 4.4 Configuration Layering with Deep Merge

```eu,notest
base: {
  server: { host: "0.0.0.0" port: 8080 workers: 4 }
  logging: { level: "info" format: "json" }
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

### 4.5 Building Blocks from Lists

```eu,notest
# Zip keys and values into a block
zip-kv([:x, :y, :z], [1, 2, 3])
# Result: { x: 1 y: 2 z: 3 }

# Reconstruct from element pairs
[[:a, 1], [:b, 2]] block
# Result: { a: 1 b: 2 }

# Merge a list of blocks
[{a: 1}, {b: 2}, {c: 3}] merge-all
# Result: { a: 1 b: 2 c: 3 }
```

### 4.6 Transforming Block Values

```eu,notest
# Apply function to all values
{ a: 1 b: 2 c: 3 } map-values(* 10)
# Result: { a: 10 b: 20 c: 30 }

# Transform keys
{ a: 1 b: 2 } map-keys(sym ∘ str.prefix("x-") ∘ str.of)
# Result: { x-a: 1 x-b: 2 }
```

### 4.7 Nested Block Modification

```eu,notest
config: { server: { db: { port: 5432 } } }

# Set a nested value
config alter([:server, :db, :port], 3306)

# Apply a function to a nested value
config update([:server, :db, :port], inc)

# Merge into a nested block
config merge-at([:server, :db], { host: "10.0.0.1" })
```

### 4.8 Grouping and Sorting

```eu,notest
# Sort numbers
[5, 3, 1, 4, 2] qsort(<)           # [1, 2, 3, 4, 5]
[30, 10, 20] sort-nums              # [10, 20, 30]

# Sort by extracted key
people sort-by-num(_.age)

# Group by key function
items group-by(_.category)
```

### 4.9 Composition

```eu,notest
# Right-to-left composition (g then f)
shout: str.to-upper ∘ str.suffix("!")
"hello" shout   # "HELLO!"

# Left-to-right composition (f then g)
process: filter(> 0) ; map(* 2)
[-1, 2, -3, 4] process   # [4, 8]
```

### 4.10 Handling Data from External Sources

```eu,notest
# CSV (values are always strings, use num to convert)
{ import: "rows=transactions.csv" }
total: rows map(_.amount num) foldl(+, 0)

# Command-line arguments
name: io.args head-or("World")
greeting: "Hello, {name}!"

# Environment variables
home: io.env lookup-or(:HOME, "/tmp")
```

### 4.11 Infinite Lists

```eu,notest
# Infinite repetition
repeat(:x) take(4)                   # [:x, :x, :x, :x]

# Infinite sequence of integers
ints-from(1) take(5)                 # [1, 2, 3, 4, 5]

# Range (finite)
range(1, 6)                          # [1, 2, 3, 4, 5]

# Infinite iteration
iterate(* 2, 1) take(6)             # [1, 2, 4, 8, 16, 32]
```

---

## 5. Common Pitfalls

### 5.1 Catenation Applies the LAST Argument

When using pipeline style, the catenated value becomes the **last**
parameter. `[1,2,3] map(inc)` means `map(inc, [1,2,3])` because the
list is the last argument.

For `foldl(op, init, list)`, you cannot write
`[1,2,3] foldl(+, 0)` — you need either `foldl(+, 0, [1,2,3])` or
define a partial: `sum: foldl(+, 0)` then `[1,2,3] sum`.

### 5.2 Dot Binds Tighter Than Catenation

`.` has precedence 90 vs catenation at 20. So:

```eu,notest
list head.name    # WRONG: parses as list (head.name)
(list head).name  # RIGHT: get head, then lookup .name
```

The `↑` prefix operator (precedence 95) binds even tighter: `↑xs.name`
= `(↑xs).name`.

### 5.3 No Whitespace Before Parentheses in Calls

```eu,notest
f(x)    # function call
f (x)   # catenation: applies f to (x) as pipeline
```

### 5.4 map vs mapcat

- `map(f, l)` — applies `f` to each element, preserving list structure
- `mapcat(f, l)` — applies `f` (which must return a list) and
  concatenates all results

```eu,notest
["ab","cd"] map(str.letters)     # [["a","b"], ["c","d"]]
["ab","cd"] mapcat(str.letters)  # ["a", "b", "c", "d"]
```

### 5.5 lookup vs Dot Lookup

- `block.key` — compile-time property access (key must be a literal
  name known at compile time)
- `lookup(:key, block)` / `block lookup(:key)` — runtime dynamic
  lookup using a symbol value

Use `lookup` / `lookup-or` when the key is computed or stored in a
variable.

### 5.6 merge vs deep-merge (<<)

- `merge(a, b)` — shallow merge; nested blocks in `b` completely
  replace those in `a`
- `deep-merge(a, b)` / `a << b` — recursively merges nested blocks,
  but lists are still replaced entirely

### 5.7 has Takes a Symbol, Not a String

```eu,notest
{ a: 1 } has(:a)       # true
{ a: 1 } has("a")      # WRONG: "a" is a string, not symbol :a
```

Use `sym("a")` to convert a string to a symbol if needed.

### 5.8 deep-find Takes a String, Not a Symbol

```eu,notest
{ a: { x: 1 } } deep-find("x")    # [1] — correct
{ a: { x: 1 } } deep-find(:x)     # WRONG
```

`deep-find` internally converts the string to a symbol via `sym(k)`.

### 5.9 Self-Reference Creates Infinite Recursion

```eu,notest
name: "foo"
x: { name: name }    # INFINITE RECURSION: inner name refers to itself
```

The inner `name` shadows the outer one. Use a different name or
generalised lookup.

### 5.10 Anaphora Scope Limits

- Expression anaphora (`_`, `_0`) do not cross commas, catenation
  boundaries, or function argument lists
- Block anaphora (`•`, `•0`) are scoped to the enclosing block
- String anaphora (`{}`, `{0}`) are scoped to the enclosing string
- None of the anaphora types can be nested

### 5.11 Functions That Do NOT Exist

The following are commonly assumed but are **not** in the prelude:

- `str.replace` — does not exist
- `str.trim` — does not exist
- `str.starts-with?` — does not exist
- `str.ends-with?` — does not exist
- `str.contains?` — does not exist
- `flatten` — use `concat` (flattens one level)
- `unique` — does not exist in prelude
- `abs` — does not exist (use `if(x < 0, negate(x), x)`)
- `even?` / `odd?` — do not exist (use `x % 2 = 0`)
- `round` / `ceil` — use `floor` and `ceiling`
- `select` / `dissoc` — do not exist (use `filter-items` with
  `by-key`)

### 5.12 str.split-on Uses Regex, Not Literal Strings

```eu,notest
"a.b.c" str.split-on(".")     # WRONG: "." matches any char
"a.b.c" str.split-on("[.]")   # RIGHT: escaped dot in regex
```

All `str.match`, `str.split`, `str.matches?` functions use regex
patterns.

---

## 6. Quick CLI Reference

```sh
eu file.eu                  # Evaluate, output YAML
eu -j file.eu               # Output JSON
eu -x toml file.eu          # Output TOML
eu -x text file.eu          # Output plain text
eu -e 'expression'          # Evaluate inline expression
eu data.yaml transform.eu   # Merge inputs (left to right)
eu name=data.json app.eu    # Named input
eu -t target file.eu        # Render specific target
eu list-targets file.eu     # List available targets
eu test file.eu             # Run embedded tests
eu fmt file.eu              # Format source
eu fmt --write file.eu      # Format in place
eu -o output.json file.eu   # Write to file (format inferred)
eu -Q file.eu               # Suppress prelude
eu -B file.eu               # Batch mode (no ergonomic features)
eu --seed 42 file.eu        # Deterministic random
eu -e 'io.args' -- arg1 arg2  # Pass arguments
```

---

## 7. Complete str Namespace Reference

All functions verified against `lib/prelude.eu`:

| Function | Signature | Description |
|----------|-----------|-------------|
| `str.of` | `of(e)` | Convert any value to string |
| `str.split` | `split(s, re)` | Split string `s` on regex `re` |
| `str.split-on` | `split-on(re, s)` | Split `s` on regex `re` (pipeline-friendly) |
| `str.join` | `join(l, sep)` | Join list `l` with separator `sep` |
| `str.join-on` | `join-on(sep, l)` | Join `l` with `sep` (pipeline-friendly) |
| `str.match` | `match(s, re)` | Match `s` against regex, return [full, groups...] |
| `str.match-with` | `match-with(re, s)` | Match `s` against `re` (pipeline-friendly) |
| `str.extract` | `extract(re, s)` | Extract first capture group or error |
| `str.extract-or` | `extract-or(re, d, s)` | Extract first capture group or default `d` |
| `str.matches` | `matches(s, re)` | All matches of `re` in `s` |
| `str.matches-of` | `matches-of(re, s)` | All matches (pipeline-friendly) |
| `str.matches?` | `matches?(re, s)` | True if `re` matches full string `s` |
| `str.letters` | `letters(s)` | List of individual characters |
| `str.len` | `len(s)` | String length in characters |
| `str.fmt` | `fmt(x, spec)` | Printf-style format |
| `str.to-upper` | `to-upper(s)` | Convert to upper case |
| `str.to-lower` | `to-lower(s)` | Convert to lower case |
| `str.prefix` | `prefix(b, a)` | Prepend `b` onto `a` |
| `str.suffix` | `suffix(b, a)` | Append `b` onto `a` |
| `str.base64-encode` | `base64-encode(s)` | Base64 encode |
| `str.base64-decode` | `base64-decode(s)` | Base64 decode |
| `str.sha256` | `sha256(s)` | SHA-256 hash (lowercase hex) |

---

## 8. Assertion Operators

For testing and debugging. All at precedence 5 (`:meta`).

| Operator | Description |
|----------|-------------|
| `e //=> v` | Assert `e` equals `v`, panic if not, return `e` |
| `e //= v` | Check `e` equals `v`, return boolean |
| `e //!` | Assert `e` is `true` |
| `e //!!` | Assert `e` is `false` |
| `e //=? f` | Assert `f(e)` is `true`, return `e` |
| `e //!? f` | Assert `f(e)` is `false`, return `e` |
| `e // m` | Attach metadata block `m` to value `e` |
| `e //<< m` | Merge `m` into existing metadata of `e` |
