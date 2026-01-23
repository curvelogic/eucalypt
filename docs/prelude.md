# Prelude Reference

The eucalypt **prelude** is a standard library of functions, operators,
and constants that is automatically loaded before your code runs.

You can suppress the prelude with `-Q` if needed, though this leaves
a very bare environment (even `true`, `false`, and `if` are defined
in the prelude).

## Metadata and IO

### `eu` namespace

- `eu.prelude.version` - Version of the standard prelude
- `eu.build` - Build metadata for the eucalypt executable

### `io` namespace

- `io.env` - Block of environment variables at launch time
- `io.epoch-time` - Unix timestamp at launch time

## Essentials

### Constants

- `null` - Null value (exports as `null` in JSON, `~` in YAML)
- `true` - Boolean true
- `false` - Boolean false
- `nil` - Empty list `[]`

### Control Flow

| Function | Description |
|----------|-------------|
| `if(c, t, f)` | If `c` is true return `t`, else `f` |
| `then(t, f, c)` | Pipeline-friendly if: `x? then(t, f)` |
| `when(p?, f, x)` | When `x` satisfies `p?`, apply `f`, else pass through |
| `cond(l, d)` | Select first true condition from list of `[condition, value]` pairs, else default `d` |

### Error Handling

| Function | Description |
|----------|-------------|
| `panic(s)` | Raise runtime error with message `s` |
| `assert(c, s, v)` | If `c` is true return `v`, else error with message `s` |

## Lists

### Basic Operations

| Function | Description |
|----------|-------------|
| `cons(h, t)` | Prepend item `h` to list `t` |
| `head(xs)` | First item of list (error if empty) |
| `↑xs` | Tight-binding prefix form of `head` (prec 95) |
| `head-or(d, xs)` | First item or default `d` if empty |
| `tail(xs)` | List without first item (error if empty) |
| `tail-or(d, xs)` | List without first item or `d` if empty |
| `first(xs)` | Alias for `head` |
| `second(xs)` | Second item of list |
| `second-or(d, xs)` | Second item or default `d` |
| `last(l)` | Last element of list |
| `nil?(xs)` | True if list is empty |
| `nth(n, l)` | Return `n`th item (0-indexed) |
| `l !! n` | Operator form of `nth` |
| `count(l)` | Number of items in list |

### List Construction

| Function | Description |
|----------|-------------|
| `repeat(i)` | Infinite list of item `i` |
| `ints-from(n)` | Infinite list of integers from `n` upwards |
| `range(b, e)` | List of integers from `b` to `e` (exclusive) |
| `cycle(l)` | Infinite list cycling elements of `l` |
| `iterate(f, i)` | List of `i`, `f(i)`, `f(f(i))`, ... |

### Transformations

| Function | Description |
|----------|-------------|
| `map(f, l)` | Apply `f` to each element |
| `f <$> l` | Operator form of `map` |
| `map2(f, l1, l2)` | Map `f` over two lists in parallel |
| `filter(p?, l)` | Keep elements satisfying predicate `p?` |
| `remove(p?, l)` | Remove elements satisfying predicate `p?` |
| `reverse(l)` | Reverse list |
| `take(n, l)` | First `n` elements |
| `drop(n, l)` | List after dropping `n` elements |
| `take-while(p?, l)` | Initial elements while `p?` is true |
| `take-until(p?, l)` | Initial elements while `p?` is false |
| `drop-while(p?, l)` | Skip elements while `p?` is true |
| `drop-until(p?, l)` | Skip elements while `p?` is false |

### Combining Lists

| Function | Description |
|----------|-------------|
| `append(l1, l2)` | Concatenate two lists |
| `l1 ++ l2` | Operator form of `append` |
| `prepend(l1, l2)` | Concatenate with `l1` after `l2` |
| `concat(ls)` | Concatenate list of lists |
| `mapcat(f, l)` | Map then concatenate results |
| `zip(l1, l2)` | List of pairs from two lists |
| `zip-with(f, l1, l2)` | Apply `f` to parallel elements |
| `zip-apply(fs, vs)` | Apply functions to corresponding values |

### Splitting Lists

| Function | Description |
|----------|-------------|
| `split-at(n, l)` | Split at index `n`, return pair |
| `split-after(p?, l)` | Split where `p?` becomes false |
| `split-when(p?, l)` | Split where `p?` becomes true |
| `window(n, step, l)` | Sliding windows of size `n` with offset `step` |
| `partition(n, l)` | Non-overlapping segments of size `n` |
| `discriminate(pred, xs)` | Split into [matches, non-matches] |

### Folds and Scans

| Function | Description |
|----------|-------------|
| `foldl(op, i, l)` | Left fold with initial value `i` |
| `foldr(op, i, l)` | Right fold with final value `i` |
| `scanl(op, i, l)` | Left scan (intermediate fold values) |
| `scanr(op, i, l)` | Right scan |

### Predicates

| Function | Description |
|----------|-------------|
| `all(p?, l)` | True if all elements satisfy `p?` |
| `all-true?(l)` | True if all elements are true |
| `any(p?, l)` | True if any element satisfies `p?` |
| `any-true?(l)` | True if any element is true |

### Sorting

| Function | Description |
|----------|-------------|
| `qsort(lt, xs)` | Sort using less-than function `lt` |
| `group-by(k, xs)` | Group by key function, returns block |

### Other

| Function | Description |
|----------|-------------|
| `over-sliding-pairs(f, l)` | Apply binary `f` to overlapping pairs |
| `differences(l)` | Differences between adjacent numbers |

## Blocks

### Construction

| Function | Description |
|----------|-------------|
| `block(kvs)` | Construct block from list of `[key, value]` pairs |
| `pair(k, v)` | Create a `[key, value]` pair |
| `sym(s)` | Create symbol from string `s` |
| `tongue(ks, v)` | Create nested block from key path to value |
| `zip-kv(ks, vs)` | Create block by zipping keys and values |
| `with-keys(ks)` | Alias for `zip-kv` |
| `map-as-block(f, syms)` | Map symbols and create block |

### Access

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

### Merging

| Function | Description |
|----------|-------------|
| `merge(b1, b2)` | Shallow merge `b2` onto `b1` |
| `deep-merge(b1, b2)` | Deep merge (nested blocks) |
| `l << r` | Operator for deep merge |
| `merge-all(bs)` | Merge list of blocks |
| `merge-at(ks, v, b)` | Merge `v` at key path `ks` |

### Transformation

| Function | Description |
|----------|-------------|
| `map-values(f, b)` | Apply `f` to each value |
| `map-keys(f, b)` | Apply `f` to each key |
| `map-kv(f, b)` | Apply `f(k, v)` to each pair, return list |
| `filter-items(f, b)` | Filter items by predicate on pairs |
| `filter-values(p?, b)` | Values matching predicate |
| `match-filter-values(re, b)` | Values with keys matching regex |

### Item Predicates

| Function | Description |
|----------|-------------|
| `by-key(p?)` | Predicate on key |
| `by-key-name(p?)` | Predicate on key as string |
| `by-key-match(re)` | Predicate matching key against regex |
| `by-value(p?)` | Predicate on value |

### Mutation

| Function | Description |
|----------|-------------|
| `alter-value(k, v, b)` | Set `b.k` to `v` |
| `update-value(k, f, b)` | Apply `f` to `b.k` |
| `alter(ks, v, b)` | Set value at nested key path |
| `update(ks, f, b)` | Apply `f` at nested key path |
| `update-value-or(k, f, d, b)` | Update or add with default |
| `set-value(k, v, b)` | Set value, adding if absent |

## Booleans

### Functions

| Function | Description |
|----------|-------------|
| `not(b)` | Toggle boolean |
| `and(l, r)` | Logical and |
| `or(l, r)` | Logical or |

### Operators

| Operator | Description |
|----------|-------------|
| `!x` or `¬x` | Not (prefix) |
| `l && r` or `l ∧ r` | And |
| `l \|\| r` or `l ∨ r` | Or |

## Equality and Comparison

| Operator | Description |
|----------|-------------|
| `l = r` | Equality |
| `l != r` | Inequality |
| `l < r` | Less than |
| `l > r` | Greater than |
| `l <= r` | Less than or equal |
| `l >= r` | Greater than or equal |

## Arithmetic

### Operators

| Operator | Description |
|----------|-------------|
| `l + r` | Addition |
| `l - r` | Subtraction |
| `l * r` | Multiplication |
| `l / r` | Division |
| `l % r` | Modulus |
| `∸ n` | Unary minus (negate) |

### Functions

| Function | Description |
|----------|-------------|
| `inc(x)` | Increment by 1 |
| `dec(x)` | Decrement by 1 |
| `negate(n)` | Negate number |
| `num(s)` | Parse number from string |
| `floor(n)` | Round down to integer |
| `ceiling(n)` | Round up to integer |
| `max(l, r)` | Maximum of two numbers |
| `min(l, r)` | Minimum of two numbers |
| `max-of(l)` | Maximum in list |
| `min-of(l)` | Minimum in list |

### Predicates

| Function | Description |
|----------|-------------|
| `zero?(n)` | True if `n` is 0 |
| `pos?(n)` | True if `n` is positive |
| `neg?(n)` | True if `n` is negative |

## Strings

The `str` namespace contains string functions:

| Function | Description |
|----------|-------------|
| `str.of(e)` | Convert to string |
| `str.split(s, re)` | Split string on regex |
| `str.split-on(re, s)` | Split (pipeline-friendly) |
| `str.join(l, s)` | Join list with separator |
| `str.join-on(s, l)` | Join (pipeline-friendly) |
| `str.match(s, re)` | Match regex, return captures |
| `str.match-with(re, s)` | Match (pipeline-friendly) |
| `str.matches(s, re)` | All matches of regex |
| `str.matches-of(re, s)` | All matches (pipeline-friendly) |
| `str.matches?(re, s)` | True if regex matches full string |
| `str.extract(re, s)` | Extract single capture |
| `str.extract-or(re, d, s)` | Extract with default |
| `str.suffix(b, a)` | Suffix `b` onto `a` |
| `str.prefix(b, a)` | Prefix `b` onto `a` |
| `str.letters(s)` | List of characters |
| `str.len(s)` | String length |
| `str.fmt(x, spec)` | Printf-style formatting |
| `str.to-upper(s)` | Convert to upper case |
| `str.to-lower(s)` | Convert to lower case |

### Character Constants

The `ch` namespace provides special characters:

- `ch.n` - Newline
- `ch.t` - Tab
- `ch.dq` - Double quote

## Function Combinators

| Function | Description |
|----------|-------------|
| `identity(v)` | Return `v` unchanged |
| `const(k)` | Function that always returns `k` |
| `-> k` | Operator form of `const` |
| `compose(f, g, x)` | Apply `f` to `g(x)` |
| `f ∘ g` | Composition: `g` then `f` |
| `f ; g` | Composition: `f` then `g` |
| `l @ r` | Application: `l(r)` |
| `apply(f, xs)` | Apply `f` to args in list |
| `flip(f)` | Swap argument order |
| `complement(p?)` | Invert predicate |
| `curry(f)` | Convert `f([x,y])` to `f(x,y)` |
| `uncurry(f)` | Convert `f(x,y)` to `f([x,y])` |
| `juxt(f, g, x)` | Return `[f(x), g(x)]` |
| `fnil(f, v, x)` | Replace null with `v` before applying `f` |

## Pairs

| Function | Description |
|----------|-------------|
| `pair(k, v)` | Create pair `[k, v]` |
| `bimap(f, g, pr)` | Apply `f` to first, `g` to second |
| `map-first(f, prs)` | Apply `f` to first elements |
| `map-second(f, prs)` | Apply `f` to second elements |

## Metadata

| Function | Description |
|----------|-------------|
| `with-meta(m, e)` | Add metadata block `m` to expression `e` |
| `e // m` | Operator form of `with-meta` |
| `meta(e)` | Retrieve metadata from expression |
| `merge-meta(m, e)` | Merge into existing metadata |
| `e //<< m` | Operator form of `merge-meta` |

### Assertions

| Operator | Description |
|----------|-------------|
| `e //= v` | Check if `e` equals `v`, return boolean |
| `e //=> v` | Assert `e` equals `v`, return `e` or panic |
| `e //=? f` | Assert `e` satisfies predicate `f` |
| `e //!? f` | Assert `e` does not satisfy `f` |
| `e //!` | Assert `e` is true |
| `e //!!` | Assert `e` is false |

## Calendar / Time

The `cal` namespace provides date/time functions:

| Function | Description |
|----------|-------------|
| `cal.now` | Current time as fields block |
| `cal.epoch` | Unix epoch as fields block |
| `cal.zdt(y,m,d,H,M,S,Z)` | Create zoned datetime |
| `cal.datetime(b)` | Create from block with defaults |
| `cal.parse(s)` | Parse ISO8601 string |
| `cal.format(t)` | Format as ISO8601 |
| `cal.fields(t)` | Decompose to `{y,m,d,H,M,S,Z}` block |

## IOSM (Insert-Ordered Symbol Maps)

The `iosm` namespace provides optimized block operations:

| Function | Description |
|----------|-------------|
| `iosm.empty` | Empty IOSM |
| `iosm.insert(k, v, m)` | Insert value |
| `iosm.from-elements(xs)` | Create from key-value pairs |
| `iosm.from(b)` | Create from block |
| `iosm.lookup(k, m)` | Look up value |
| `iosm.lookup-or(k, d, m)` | Look up with default |
| `iosm.merge(l, r)` | Shallow merge |
| `iosm.deep-merge(l, r)` | Deep merge |
| `iosm.elements(m)` | Extract as pairs |
