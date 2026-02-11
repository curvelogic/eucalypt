# Syntax Cheat Sheet

A dense single-page reference covering all syntax forms, operators,
common patterns, and key prelude functions.

## Primitives

| Type | Syntax | Examples |
|------|--------|----------|
| Integer | digits | `42`, `-7`, `0` |
| Float | digits with `.` | `3.14`, `-0.5` |
| String | double quotes | `"hello"`, `"line\nbreak"` |
| Symbol | colon prefix | `:key`, `:name` |
| Boolean | keywords | `true`, `false` |
| Null | keyword | `null` |
| ZDT | `t"..."` prefix | `t"2024-03-15"`, `t"2024-03-15T14:30:00Z"` |

## Blocks

```eu,notest
# Property declaration
name: expression

# Function declaration
f(x, y): expression

# Operator declaration (binary)
(l ++ r): expression

# Operator declaration (prefix / postfix)
(! x): expression
(x ******): expression

# Block literal
{ a: 1 b: 2 c: 3 }

# Commas are optional
{ a: 1, b: 2, c: 3 }

# Nested blocks
{ outer: { inner: "value" } }
```

**Top-level unit**: the file itself is an implicit block (no braces needed).

## Lists

```eu,notest
# List literal
[1, 2, 3]

# Empty list
[]

# Mixed types
[1, "two", :three, true]
```

## String Interpolation

```eu,notest
# Insert expressions with {braces}
"Hello, {name}!"

# String anaphora (defines a function)
"#{}"           # one-parameter function
"{0} and {1}"   # two-parameter function
```

## Comments

```eu,notest
# Line comment (to end of line)
x: 42 # inline comment
```

## Declarations

| Form | Syntax | Notes |
|------|--------|-------|
| Property | `name: expr` | Defines a named value |
| Function | `f(x, y): expr` | Named function with parameters |
| Binary operator | `(l op r): expr` | Infix operator |
| Prefix operator | `(op x): expr` | Unary prefix |
| Postfix operator | `(x op): expr` | Unary postfix |

## Metadata Annotations

```eu,notest
# Declaration metadata (backtick prefix)
` "Documentation string"
name: value

# Structured metadata
` { doc: "description" associates: :left precedence: 50 }
(l op r): expr

# Unit-level metadata (first expression in file)
{ :doc "Unit description" }
a: 1
```

**Special metadata keys**: `:target`, `:suppress`, `:main`,
`associates`, `precedence`, `import`.

## Function Application

```eu,notest
# Parenthesised application (no whitespace before paren)
f(x, y)

# Catenation (pipeline style, single argument)
x f              # equivalent to f(x)
x f g h          # equivalent to h(g(f(x)))

# Partial application (curried)
add(1)           # returns a function adding 1

# Sections (operator with gaps)
(+ 1)            # function: add 1
(* 2)            # function: multiply by 2
(/)              # function: divide (two params)
```

## Lookup and Generalised Lookup

```eu,notest
# Simple lookup
block.key

# Generalised lookup (evaluate RHS in block's scope)
{ a: 3 b: 4 }.(a + b)        # 7
{ a: 3 b: 4 }.[a, b]         # [3, 4]
{ a: 3 b: 4 }."{a} and {b}"  # "3 and 4"
```

## Anaphora (Implicit Parameters)

| Type | Numbered | Unnumbered | Scope |
|------|----------|------------|-------|
| Expression | `_0`, `_1`, `_2` | `_` (each use = new param) | Expression |
| Block | `•0`, `•1`, `•2` | `•` (each use = new param) | Block |
| String | `{0}`, `{1}`, `{2}` | `{}` (each use = new param) | String |

```eu,notest
# Expression anaphora
map(_0 * _0)        # square each element
map(_ + 1)          # increment (each _ is a new param)

# Block anaphora (bullet = Option-8 on Mac)
{ x: •0 y: •1 }    # two-parameter block function

# String anaphora
map("item: {}")     # format each element
```

## Operator Precedence Table

From highest to lowest binding:

| Prec | Name | Assoc | Operators | Description |
|------|------|-------|-----------|-------------|
| 95 | -- | prefix | `↑` | Tight prefix (head) |
| 90 | lookup | left | `.` | Field access / lookup |
| 88 | bool-unary | prefix | `!`, `¬` | Boolean negation |
| 85 | exp | right | `∘`, `;` | Composition |
| 80 | prod | left | `*`, `/`, `%` | Multiplication, division, modulo |
| 75 | sum | left | `+`, `-` | Addition, subtraction |
| 50 | cmp | left | `<`, `>`, `<=`, `>=` | Comparison |
| 45 | append | right | `++`, `<<` | List append, deep merge |
| 42 | map | left | `<$>` | Functor map |
| 40 | eq | left | `=`, `!=` | Equality |
| 35 | bool-prod | left | `&&`, `∧` | Logical AND |
| 30 | bool-sum | left | `\|\|`, `∨` | Logical OR |
| 20 | cat | left | *(catenation)* | Juxtaposition / pipeline |
| 10 | apply | right | `@` | Function application |
| 5 | meta | right | `//`, `//<< `, `//=`, `//=>` | Metadata / assertions |

**User-defined operators** default to left-associative, precedence 50.
Set custom values via metadata: `` ` { precedence: 75 associates: :right } ``

**Named precedence levels** for use in metadata: `lookup`, `call`,
`bool-unary`, `exp`, `prod`, `sum`, `shift`, `bitwise`, `cmp`,
`append`, `map`, `eq`, `bool-prod`, `bool-sum`, `cat`, `apply`, `meta`.

## Block Merge

```eu,notest
# Catenation of blocks performs a shallow merge
{ a: 1 } { b: 2 }       # { a: 1 b: 2 }
{ a: 1 } { a: 2 }       # { a: 2 }

# Deep merge operator
{ a: { x: 1 } } << { a: { y: 2 } }  # { a: { x: 1 y: 2 } }
```

## Imports

```eu,notest
# Unit-level import
{ import: "lib.eu" }

# Named import
{ import: "cfg=config.eu" }

# Multiple imports
{ import: ["dep-a.eu", "dep-b.eu"] }

# Format override
{ import: "yaml@data.txt" }

# Git import
{ import: { git: "https://..." commit: "sha..." import: "file.eu" } }
```

## Key Prelude Functions

### Lists

| Function | Description |
|----------|-------------|
| `head` | First element |
| `tail` | All but first |
| `cons(x, xs)` | Prepend element |
| `map(f)` | Transform each element |
| `filter(p?)` | Keep elements matching predicate |
| `foldl(f, init)` | Left fold |
| `foldr(f, init)` | Right fold |
| `sort-nums` / `sort-strs` | Sort numbers / strings |
| `sort-by-num(f)` / `sort-by-str(f)` | Sort by extracted key |
| `qsort(lt)` | Sort with custom comparator |
| `take(n)` | First n elements |
| `drop(n)` | Remove first n |
| `zip` | Pair elements from two lists |
| `zip-with(f)` | Combine elements with function |
| `flatten` | Flatten nested lists one level |
| `reverse` | Reverse a list |
| `count` | Number of elements |
| `range(a, b)` | Integers from a to b-1 |
| `nil?` | Is the list empty? |
| `any?(p?)` | Does any element match? |
| `all?(p?)` | Do all elements match? |
| `unique` | Remove duplicates |

### Blocks

| Function | Description |
|----------|-------------|
| `lookup(key)` | Look up a key (symbol) |
| `lookup-or(key, default)` | Look up with default |
| `has(key)` | Does block contain key? |
| `keys` | List of keys (as symbols) |
| `values` | List of values |
| `elements` | List of `{key, value}` pairs |
| `map-keys(f)` | Transform keys |
| `map-values(f)` | Transform values |
| `select(keys)` | Keep only listed keys |
| `dissoc(keys)` | Remove listed keys |
| `merge(b)` | Shallow merge |
| `deep-merge(b)` | Deep recursive merge |
| `sort-keys` | Sort by key name |

### Strings (`str` namespace)

| Function | Description |
|----------|-------------|
| `str.len(s)` | String length |
| `str.upper(s)` | Upper case |
| `str.lower(s)` | Lower case |
| `str.starts-with?(prefix)` | Starts with prefix? |
| `str.ends-with?(suffix)` | Ends with suffix? |
| `str.contains?(sub)` | Contains substring? |
| `str.matches?(regex)` | Matches regex? |
| `str.split(sep)` | Split by separator |
| `str.join(sep)` | Join list with separator |
| `str.replace(from, to)` | Replace occurrences |
| `str.trim` | Remove surrounding whitespace |

### Combinators

| Function | Description |
|----------|-------------|
| `identity` | Returns its argument unchanged |
| `const(k)` | Always returns k |
| `compose(f, g)` or `f ∘ g` | Compose functions |
| `flip(f)` | Swap argument order |
| `complement(p?)` | Negate a predicate |
| `curry(f)` | Curry a function taking a pair |
| `uncurry(f)` | Uncurry to take a pair |

### Numbers

| Function | Description |
|----------|-------------|
| `num` | Parse string to number |
| `abs` | Absolute value |
| `negate` | Negate number |
| `inc` / `dec` | Increment / decrement |
| `max(a, b)` / `min(a, b)` | Maximum / minimum |
| `num?` / `str?` | Type predicates |
| `zero?` / `pos?` / `neg?` | Sign predicates |
| `floor` / `ceil` / `round` | Rounding |

### IO

| Binding | Description |
|---------|-------------|
| `io.env` | Block of environment variables |
| `io.epoch-time` | Unix timestamp at launch |
| `io.args` | Command-line arguments (after `--`) |
| `io.random` | Infinite lazy stream of random floats |
| `io.RANDOM_SEED` | Current random seed |

## Assertion Operators

| Operator | Description |
|----------|-------------|
| `e //=> v` | Assert `e` equals `v` (panic if not) |
| `e //= v` | Assert equals (silent, returns `e`) |
| `e //!` | Assert `e` is `true` |
| `e //!!` | Assert `e` is `false` |
| `e //=? f` | Assert `f(e)` is `true` |
| `e //!? f` | Assert `f(e)` is `false` |

## Command Line Quick Reference

```sh
eu file.eu                  # Evaluate file, output YAML
eu -j file.eu               # Output JSON
eu -x text file.eu          # Output plain text
eu -e 'expression'          # Evaluate expression
eu a.yaml b.eu              # Merge inputs
eu -t target file.eu        # Render specific target
eu list-targets file.eu     # List targets
eu --seed 42 file.eu        # Deterministic random
eu -Q file.eu               # Suppress prelude
eu fmt file.eu              # Format source
eu dump stg file.eu         # Dump STG syntax
eu -- arg1 arg2             # Pass arguments (io.args)
```
