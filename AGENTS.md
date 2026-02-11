# Agent Instructions

This project uses **bd** (beads) for issue tracking. Run `bd onboard` to get started.

## Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --status in_progress  # Claim work
bd close <id>         # Complete work
bd sync               # Sync with git
```

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds

---

## Eucalypt Language Reference

Eucalypt is a functional language for generating, templating, and processing
structured data (YAML, JSON, TOML). This reference covers what agents need
to read and write `.eu` files in `harness/test/` and `lib/`.

### Primitives

| Type | Syntax | Examples |
|------|--------|----------|
| Integer | digits | `42`, `-7`, `0` |
| Float | digits with `.` | `3.14`, `-0.5` |
| String | double quotes | `"hello"`, `"line\nbreak"` |
| Symbol | colon prefix | `:key`, `:name` |
| Boolean | keywords | `true`, `false` |
| Null | keyword | `null` |
| DateTime | `t"..."` prefix | `t"2024-03-15"`, `t"2024-03-15T14:30:00Z"` |

### Blocks (Objects/Maps)

A block is a collection of declarations enclosed in braces. The top-level
file is an implicit block (no braces needed).

```eu
{ a: 1 b: 2 c: 3 }
```

Commas are optional. Line endings are not significant.

```eu
a: 1
b: 2
c: 3
```

### Lists

```eu
list: [1, 2, :a, "boo"]
empty: []
```

### Declarations

```eu
# Property declaration
name: "value"

# Function declaration (with parameters)
f(x, y): x + y

# Binary operator declaration
(l ^|^ r): "{l} v {r}"

# Prefix / postfix unary operator
(! x): not(x)
(x ******): "maybe {x}"
```

### Function Application

```eu
# Parenthesised application (NO whitespace before paren)
f(x, y)

# Catenation: single-argument pipeline style
# x f  is equivalent to  f(x)
# x f g h  is equivalent to  h(g(f(x)))
```

Functions are curried -- fewer arguments than expected returns a partial:

```eu
add(x, y): x + y
increment: add(1)
result: increment(9) //=> 10
```

### Sections (Operator Gaps)

An operator with a "gap" becomes a function:

```eu
increment: + 1
result: [1, 2, 3] map(+ 1) //=> [2, 3, 4]
double: * 2
halved: [10, 20] map(/ 2) //=> [5, 10]
```

### Lookup (Field Access)

```eu
config: { db: { host: "localhost" port: 5432 } }
host: config.db.host
```

Generalised lookup evaluates the RHS in the block's scope:

```eu
point: { x: 3 y: 4 }
sum: point.(x + y) //=> 7
```

### Anaphora (Implicit Parameters)

Anaphora create anonymous functions without lambda syntax.

| Type | Numbered | Unnumbered |
|------|----------|------------|
| Expression | `_0`, `_1`, `_2` | `_` (each use = new param) |
| Block | `•0`, `•1`, `•2` | `•` (each use = new param) |
| String | `{0}`, `{1}`, `{2}` | `{}` (each use = new param) |

```eu
squares: [1, 2, 3] map(_0 * _0) //=> [1, 4, 9]
labels: [1, 2, 3] map("item-{}") //=> ["item-1", "item-2", "item-3"]
```

**No lambda syntax exists.** Use anaphora or named functions instead.

### String Interpolation

Expressions in braces inside strings are interpolated:

```eu
name: "world"
greeting: "Hello, {name}!"
```

### Metadata Annotations

Backtick (`` ` ``) before a declaration attaches metadata:

```eu
` "Compute the square of a number"
square(x): x * x

result: square(5) //=> 25
```

Special metadata keys: `:target`, `:suppress`, `:main`, `associates`,
`precedence`, `import`.

### Imports

```eu
# In declaration metadata:
{ import: "helpers.eu" }       # unqualified import
{ import: "cfg=config.eu" }    # named/qualified import
{ import: ["a.eu", "b.eu"] }   # multiple imports
```

### Assertion Operators (Used in Tests)

| Operator | Description |
|----------|-------------|
| `e //=> v` | Assert `e` equals `v` (panic if not) |
| `e //= v` | Assert equals (silent, returns `e`) |
| `e //!` | Assert `e` is `true` |
| `e //!!` | Assert `e` is `false` |
| `e //=? f` | Assert `f(e)` is `true` |
| `e //!? f` | Assert `f(e)` is `false` |

### Operator Precedence (Highest to Lowest)

| Prec | Operators | Description |
|------|-----------|-------------|
| 95 | `↑` (prefix) | Tight prefix (head) |
| 90 | `.` | Field access / lookup |
| 88 | `!`, `¬` (prefix) | Boolean negation |
| 88 | `∘`, `;` | Composition |
| 80 | `*`, `/`, `%` | Multiply, divide, modulo |
| 75 | `+`, `-` | Add, subtract |
| 50 | `<`, `>`, `<=`, `>=` | Comparison |
| 45 | `++`, `<<` | List append, deep merge |
| 42 | `<$>` | Functor map |
| 40 | `=`, `!=` | Equality |
| 35 | `&&`, `∧` | Logical AND |
| 30 | `||`, `∨` | Logical OR |
| 20 | *(catenation)* | Juxtaposition / pipeline |
| 10 | `@` | Function application |
| 5 | `//`, `//=>`, etc. | Metadata / assertions |

User-defined operators default to precedence 50, left-associative.

---

## Top 20 Common Patterns

### 1. Map and Filter

```eu
doubled: [1, 2, 3] map(* 2) //=> [2, 4, 6]
small: [1, 2, 3, 4, 5] filter(< 4) //=> [1, 2, 3]
```

### 2. Pipeline Chaining

```eu
result: [1, 2, 3, 4, 5, 6] filter(> 3) map(* 10) //=> [40, 50, 60]
```

### 3. Block Merge (Shallow)

```eu
base: { a: 1 b: 2 }
overlay: { b: 3 c: 4 }
merged: base overlay //=> { a: 1 b: 3 c: 4 }
```

### 4. Deep Merge

```eu
base: { x: { a: 1 b: 2 } }
extra: { x: { c: 3 } }
result: base << extra
```

### 5. Nested Lookup

```eu
data: { a: { b: { c: 42 } } }
result: data.a.b.c //=> 42
```

### 6. Dynamic Key Lookup

```eu
data: { name: "Alice" age: 30 }
field: data lookup(:name) //=> "Alice"
safe: data lookup-or(:missing, "default") //=> "default"
```

### 7. List Destructuring

```eu
xs: [10, 20, 30]
first: xs head //=> 10
rest: xs tail //=> [20, 30]
```

### 8. Fold / Reduce

```eu
total: [1, 2, 3, 4] foldl((+), 0) //=> 10
```

### 9. Sorting

```eu
names: ["Charlie", "Alice", "Bob"]
sorted: names sort-strs //=> ["Alice", "Bob", "Charlie"]
nums: [5, 1, 3, 2, 4]
ordered: nums sort-nums //=> [1, 2, 3, 4, 5]
```

### 10. Sort by Key

```eu
people: [{ name: "Zoe" age: 25 }, { name: "Amy" age: 30 }]
by-name: people sort-by-str(_.name)
youngest: (by-name head).name //=> "Amy"
```

### 11. String Operations

```eu
upper: "hello" str.to-upper //=> "HELLO"
parts: "a,b,c" str.split-on(",") //=> ["a", "b", "c"]
joined: ["x", "y", "z"] str.join-on("-") //=> "x-y-z"
```

### 12. Conditional Logic

```eu
classify(x): if(x > 0, "positive", "non-positive")
result: classify(5) //=> "positive"
```

### 13. Block Keys and Values

```eu
data: { a: 1 b: 2 c: 3 }
ks: data keys //=> [:a, :b, :c]
vs: data values //=> [1, 2, 3]
```

### 14. Block Elements (Key-Value Pairs)

`elements` returns a list of `[key, value]` lists (not blocks):

```eu
data: { x: 10 y: 20 }
pairs: data elements
# pairs is [[:x, 10], [:y, 20]]
```

### 15. Type Predicates

```eu
check1: [1, 2] list? //=> true
check2: { a: 1 } block? //=> true
check3: 42 pos? //=> true
```

### 16. Partial Application

```eu
add(x, y): x + y
add5: add(5)
result: add5(3) //=> 8
```

### 17. Function Composition

```eu
double(x): x * 2
add-one(x): x + 1
transform: double ∘ add-one
result: transform(3) //=> 8
```

### 18. Cartesian Product Style

```eu
pair(x): range(1, 4) map([x, _])
pairs: range(1, 4) mapcat(pair)
```

### 19. Set Operations

```eu
sa: set.from-list([1, 2, 3, 4])
sb: set.from-list([3, 4, 5, 6])
common: sa set.intersect(sb) set.to-list //=> [3, 4]
combined: sa set.union(sb) set.to-list sort-nums //=> [1, 2, 3, 4, 5, 6]
```

### 20. Assertions in Tests

```eu
double(x): x * 2
test1: double(21) //=> 42
test2: (3 > 2) //!
test3: (1 > 2) //!!
```

---

## Common Pitfalls

### 1. Lookup vs Catenation Precedence

`.` (precedence 90) binds tighter than catenation (precedence 20):

```
xs head.id    -->  xs (head.id)     WRONG
(xs head).id  -->  correct          RIGHT
```

Always parenthesise the expression before `.` when combining with catenation.

### 2. No Lambda Syntax

There are no lambda expressions. Use:
- **Anaphora**: `map(_ + 1)`, `map(_0 * _0)`
- **Sections**: `map(+ 1)`, `filter(> 3)`
- **Named functions**: `f(x): x + 1` then `map(f)`
- **Partial application**: `map(add(1))`

### 3. Single Quotes Are Identifiers, Not Strings

`'name'` creates an identifier, not a string. Use `"name"` for strings.

```
'hello'   -->  identifier reference (like a variable name)
"hello"   -->  string literal
```

### 4. Whitespace Before Parens Changes Meaning

```
f(x)      -->  function application (call f with x)
f (x)     -->  catenation (apply x to f as single argument)
```

No whitespace before `(` means function call. Whitespace means catenation.

### 5. Sorting Functions Are Type-Specific

There is no generic `sort`. Use:
- `sort-nums` for numbers
- `sort-strs` for strings
- `sort-zdts` for date-times
- `sort-by-num(key-fn)` for sorting by numeric key
- `sort-by-str(key-fn)` for sorting by string key
- `qsort(lt)` for custom comparator

### 6. Set Operations Require Set Values

`set.intersect`, `set.union`, `set.diff` operate on set values, not lists.
Convert with `set.from-list(xs)` first, and `set.to-list` to get back a list.

### 7. Shadowing Can Cause Infinite Recursion

```
name: "foo"
x: { name: name }   # infinite recursion! inner 'name' refers to itself
```

The inner `name` shadows the outer one. Use a different name or lookup.

### 8. `even?` and `odd?` Do Not Exist

The prelude does not have `even?` or `odd?`. Use modulo:

```eu
is-even(x): x % 2 = 0
check: is-even(4) //=> true
```

---

## Key Prelude Functions

### Lists

| Function | Description |
|----------|-------------|
| `head` / `tail` | First element / all but first |
| `cons(x, xs)` | Prepend element |
| `map(f)` | Transform each element |
| `filter(p?)` | Keep matching elements |
| `foldl(f, init)` / `foldr(f, init)` | Left/right fold |
| `sort-nums` / `sort-strs` | Sort numbers / strings |
| `sort-by-num(f)` / `sort-by-str(f)` | Sort by extracted key |
| `qsort(lt)` | Sort with custom comparator |
| `take(n)` / `drop(n)` | First n / remove first n |
| `zip` / `zip-with(f)` | Pair / combine elements |
| `concat` | Flatten one level (list of lists) |
| `reverse` | Reverse list |
| `count` | Number of elements |
| `range(a, b)` | Integers from a to b-1 |
| `any(p?)` / `all(p?)` | Any/all match predicate |
| `nil?` | Is list empty? |

### Blocks

| Function | Description |
|----------|-------------|
| `lookup(key)` | Look up key (symbol) |
| `lookup-or(key, default)` | Look up with default |
| `has(key)` | Does block contain key? |
| `keys` / `values` | List of keys / values |
| `elements` | List of `[key, value]` lists |
| `map-keys(f)` / `map-values(f)` | Transform keys / values (returns list) |
| `merge(b)` / `deep-merge(b)` | Shallow / deep merge |
| `sort-keys` | Sort by key name |

### Strings (str namespace)

| Function | Description |
|----------|-------------|
| `str.of(e)` | Convert to string |
| `str.len(s)` | String length (in characters) |
| `str.to-upper(s)` / `str.to-lower(s)` | Case conversion |
| `str.split-on(sep)` / `str.join-on(sep)` | Split / join |
| `str.prefix(p)` / `str.suffix(s)` | Prepend / append string |
| `str.letters(s)` | Split into list of characters |
| `str.matches?(regex)` | Matches regex? |
| `str.match-with(regex)` | Extract match + capture groups |
| `str.matches-of(regex)` | All occurrences of pattern |
| `str.base64-encode(s)` / `str.sha256(s)` | Encoding / hashing |

### Numbers

| Function | Description |
|----------|-------------|
| `num` | Parse string to number |
| `abs` / `negate` | Absolute value / negate |
| `inc` / `dec` | Increment / decrement |
| `max(a, b)` / `min(a, b)` | Maximum / minimum |
| `floor` / `ceil` / `round` | Rounding |
| `block?` / `list?` | Type predicates |
| `zero?` / `pos?` / `neg?` | Sign predicates |

### Combinators

| Function | Description |
|----------|-------------|
| `identity` | Returns argument unchanged |
| `const(k)` | Always returns k |
| `compose(f, g)` or `f ∘ g` | Compose functions |
| `flip(f)` | Swap argument order |
| `complement(p?)` | Negate a predicate |

### IO

| Binding | Description |
|---------|-------------|
| `io.env` | Block of environment variables |
| `io.epoch-time` | Unix timestamp at launch |
| `io.args` | Command-line arguments (after `--`) |
| `io.random` | Infinite lazy stream of random floats |

---

## Harness Test Patterns

Test files live in `harness/test/`. Each `.eu` file is a test case.

**Basic test structure:**

```eu
# Simple value assertion
result: 2 + 2 //=> 4

# Boolean assertion
check: (3 > 2) //!

# Predicate assertion
value: 42 //=? pos?
```

**Running tests:**

```bash
cargo test --test harness_test                  # all harness tests
cargo test test_harness_001                     # specific test
cargo test -- --nocapture test_harness_001      # with output
```

**Error tests** in `harness/test/errors/` verify that specific inputs
produce expected error messages. Each `.eu` file has a matching `.eu.expect`
file containing the expected error output.

---

## CLI Quick Reference

```bash
eu file.eu                  # Evaluate, output YAML
eu -j file.eu               # Output JSON
eu -x toml file.eu          # Output TOML
eu -x text file.eu          # Output plain text
eu -e 'expression'          # Evaluate expression
eu a.yaml b.eu              # Merge inputs
eu -t target file.eu        # Render specific target
eu list-targets file.eu     # List available targets
eu --seed 42 file.eu        # Deterministic random
eu -Q file.eu               # Suppress prelude
eu fmt file.eu              # Format source
eu dump stg file.eu         # Dump STG syntax
eu -- arg1 arg2             # Pass arguments (io.args)
```
