# Agent Reference

Dense, example-heavy reference for AI coding agents working with
eucalypt. For human-readable guides, see
[The Eucalypt Guide](../guide/blocks-and-declarations.md).

## Syntax Quick Reference

### Primitives

```
42         number (integer)
3.14       number (float)
"hello"    string (interpolation with {expr})
true       boolean
null       null
:keyword   symbol
```

### Blocks (key-value mappings)

```eu,notest
{ x: 1, y: 2 }              # block literal
{ x: 1 y: 2 }               # commas optional
point: { x: 3, y: 4 }       # declaration
config: { db: { port: 5432 } }  # nested
```

### Lists

```eu,notest
[1, 2, 3]                   # list literal
[[1, 2], [3, 4]]            # nested
[]                           # empty
```

### Declarations

```eu,notest
x: 42                        # property
double(x): x * 2             # function (curried)
add(x, y): x + y             # multi-param function
(l <+> r): l + r             # operator
```

### Function application

```eu,notest
double(21)                   # parenthesised: 42
21 double                    # catenation: 42
3 add(5)                     # partial + catenation: 8
data filter(> 3) map(* 10)  # pipeline style
```

CRITICAL: `f(x)` is parenthesised application. `f (x)` (with space)
is catenation of `f` and `(x)`.

### Lookup

```eu,notest
point.x                      # dot lookup
point.(x + y)                # generalised lookup
config.db.port               # chained lookup
```

### String interpolation

```eu,notest
"hello, {name}"              # interpolate variable
"({point.x}, {point.y})"    # interpolate lookup
```

String anaphora: `{0}`, `{1}`, `{}` (unnumbered).
Expression anaphora: `_0`, `_1`, `_` (unnumbered, each `_` is different).

### Metadata

```eu,notest
` "documentation string"
square(x): x * x

` { doc: "info", associates: :left, precedence: 75 }
(l <+> r): l + r

` :target                    # render target
` :suppress                  # hide from output
` :main                      # default target
```

### Imports

```eu,notest
{ import: "helpers.eu" }             # basic
{ import: "cfg=config.eu" }          # named
{ import: ["a.eu", "cfg=b.yaml"] }   # multiple
```

### Assertions (testing)

```eu,notest
result: 2 + 2 //=> 4                # value assertion
bad: 1 / 0 //! ERR                  # error assertion
```

## Operators (by precedence, highest first)

| Prec | Op | Example | Notes |
|------|-----|---------|-------|
| 90 | `.` | `block.key` | Lookup |
| 88 | `not` | `not(true)` | Boolean negation |
| 80 | `*` `/` `%` | `6 * 7` `15 / 4` `15 % 4` | Product (`/` is integer div) |
| 75 | `+` `-` | `3 + 4` `10 - 3` | Sum |
| 50 | `<` `>` `<=` `>=` | `3 < 4` | Comparison |
| 45 | `++` | `[1] ++ [2]` | List append |
| 40 | `=` | `x = 3` | Equality (NOT `==`) |
| 35 | `&&` | `a && b` | Boolean and |
| 30 | `\|\|` | `a \|\| b` | Boolean or |
| 20 | (juxtaposition) | `x f` | Catenation |
| 10 | `@` | `f @ x` | Apply |
| 5 | `` ` `` | metadata | Meta |

CRITICAL: Equality is `=` not `==`. There is no `!=` operator.
Integer division: `15 / 4` is `3`. Use `15.0 / 4` for `3.75`.

## Top 30 Prelude Functions

### Lists

| Function | Signature | Example | Result |
|----------|-----------|---------|--------|
| `head` | `list -> a` | `head([1,2,3])` | `1` |
| `tail` | `list -> list` | `tail([1,2,3])` | `[2,3]` |
| `last` | `list -> a` | `last([1,2,3])` | `3` |
| `cons` | `(a, list) -> list` | `cons(0,[1,2])` | `[0,1,2]` |
| `count` | `list -> num` | `count([1,2,3])` | `3` |
| `nil?` | `list -> bool` | `nil?([])` | `true` |
| `map` | `(fn, list) -> list` | `[1,2] map(* 2)` | `[2,4]` |
| `filter` | `(fn, list) -> list` | `[1,2,3] filter(> 1)` | `[2,3]` |
| `foldl` | `(fn, init, list) -> a` | `[1,2,3] foldl(+, 0)` | `6` |
| `mapcat` | `(fn, list) -> list` | `[1,2] mapcat(x: [x, x])` | `[1,1,2,2]` |
| `sort-nums` | `list -> list` | `[3,1,2] sort-nums` | `[1,2,3]` |
| `sort-strs` | `list -> list` | `["b","a"] sort-strs` | `["a","b"]` |
| `sort-by-num` | `(fn, list) -> list` | `items sort-by-num(.age)` | sorted |
| `sort-by-str` | `(fn, list) -> list` | `items sort-by-str(.name)` | sorted |
| `take` | `(n, list) -> list` | `[1,2,3] take(2)` | `[1,2]` |
| `drop` | `(n, list) -> list` | `[1,2,3] drop(1)` | `[2,3]` |
| `reverse` | `list -> list` | `reverse([1,2,3])` | `[3,2,1]` |
| `concat` | `list[list] -> list` | `concat([[1],[2]])` | `[1,2]` |
| `zip` | `(list, list) -> list` | `zip([1,2],["a","b"])` | `[[1,"a"],[2,"b"]]` |
| `range` | `(from, to) -> list` | `range(0, 3)` | `[0,1,2]` |
| `all` | `(fn, list) -> bool` | `[2,4] all(is-even)` | `true` |
| `any` | `(fn, list) -> bool` | `[1,2] any(> 1)` | `true` |
| `nth` | `(n, list) -> a` | `nth(1, [10,20])` | `20` |
| `unique` | `list -> list` | `unique([1,1,2])` | `[1,2]` |

### Blocks

| Function | Signature | Example | Result |
|----------|-----------|---------|--------|
| `keys` | `block -> list` | `keys({x:1})` | `[:x]` |
| `values` | `block -> list` | `values({x:1})` | `[1]` |
| `elements` | `block -> list` | `elements({x:1})` | `[[:x,1]]` |
| `lookup` | `(sym, block) -> a` | `lookup(:x, {x:1})` | `1` |
| `lookup-or` | `(sym, default, block) -> a` | `lookup-or(:z, 0, {x:1})` | `0` |
| `has` | `(sym, block) -> bool` | `has(:x, {x:1})` | `true` |
| `map-values` | `(fn, block) -> block` | `{x:1} map-values(* 2)` | `{x:2}` |
| `select` | `(syms, block) -> block` | `{x:1,y:2} select([:x])` | `{x:1}` |
| `dissoc` | `(syms, block) -> block` | `{x:1,y:2} dissoc([:y])` | `{x:1}` |

### Strings

| Function | Signature | Example | Result |
|----------|-----------|---------|--------|
| `str.to-upper` | `str -> str` | `"hi" str.to-upper` | `"HI"` |
| `str.to-lower` | `str -> str` | `"HI" str.to-lower` | `"hi"` |
| `str.split-on` | `(delim, str) -> list` | `"a,b" str.split-on(",")` | `["a","b"]` |
| `str.join-on` | `(delim, list) -> str` | `["a","b"] str.join-on(",")` | `"a,b"` |
| `str.len` | `str -> num` | `"hello" str.len` | `5` |
| `str.of` | `a -> str` | `str.of(42)` | `"42"` |
| `str.trim` | `str -> str` | `"  hi  " str.trim` | `"hi"` |

### Combinators

| Function | Signature | Example | Result |
|----------|-----------|---------|--------|
| `identity` | `a -> a` | `identity(42)` | `42` |
| `const` | `(a, b) -> a` | `const(1, 2)` | `1` |
| `flip` | `((a,b)->c) -> (b,a)->c` | `flip(sub)` | swapped args |
| `complement` | `pred -> pred` | `complement(nil?)` | negated |
| `;` | compose L-to-R | `double ; negate` | fn |
| `num` | `str -> num` | `num("42")` | `42` |
| `if` | `(bool, a, a) -> a` | `if(x > 0, x, 0)` | conditional |

### Type predicates

`number?`, `string?`, `list?`, `block?`, `nil?`, `bool?`, `sym?`

## Pipeline Patterns

```eu,notest
# Filter then transform
data filter(> 3) map(* 10)

# Extract field from list of blocks
people map(.name)

# Sort by field
items sort-by-str(.name)

# Aggregate
scores foldl(+, 0)

# Chain multiple operations
data filter(_.active) sort-by-str(.name) map(.email)
```

## Common Pitfalls

1. **Equality is `=` not `==`**: `x = 3` not `x == 3`
2. **Integer division**: `15 / 4` is `3`, use `15.0 / 4` for `3.75`
3. **No `!=` operator**: use `not(x = y)`
4. **Whitespace in application**: `f(x)` is call, `f (x)` is catenation
5. **Shadowing causes recursion**: `name: name` in inner block loops
6. **`count` not `len`**: use `count` for list/block length
7. **`nil?` not `null?`**: use `nil?` for empty list check
8. **`identity` not `id`**: the identity function is called `identity`
9. **`mapcat` not `flat-map`**: map-then-flatten is called `mapcat`
10. **Keys are symbols**: `keys({x:1})` returns `[:x]` not `["x"]`
11. **Named functions for predicates**: prefer `is-even(n): n % 2 = 0` over inline lambdas
12. **Sections have no space**: `map(* 2)` not `map (* 2)`

## Disambiguation

| Want | Use | Not |
|------|-----|-----|
| Equality test | `=` | `==` |
| List length | `count(xs)` | `len(xs)` |
| Empty list? | `nil?(xs)` | `null?(xs)` |
| Identity function | `identity` | `id` |
| Map + flatten | `mapcat` | `flat-map` |
| String length | `str.len` | `count` on string |
| Block length | `b keys count` | `count(b)` |
| String upper | `str.to-upper` | `str.upper` |
| String lower | `str.to-lower` | `str.lower` |
| Split string | `str.split-on(",")` | `str.split(",")` |
| Join strings | `str.join-on(",")` | `str.join(",")` |
| Sort numbers | `sort-nums` | `sort` |
| Sort strings | `sort-strs` | `sort` |

## CLI Quick Reference

```sh
eu file.eu                 # run, YAML output
eu file.eu -j              # JSON output
eu -e 'expression'         # evaluate inline
eu a.yaml b.eu             # merge inputs
eu data=file.csv -e 'data' # named input
eu -c all *.yaml           # collect inputs
eu --seed 42 file.eu       # reproducible random
eu fmt --write file.eu     # format in place
eu test file.eu            # run tests
eu dump ast file.eu        # dump syntax tree
```
