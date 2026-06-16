# Lists

## Basic Operations

| Function | Description |
|----------|-------------|
| `cons` | Construct new list by prepending item `h` to list `t` |
| `snoc(x, l)` | Append element `x` to the end of list `l` |
| `(l ‖ r)` | Prepend element `x` to list `xs`. Right-associative cons operator |
| `head` | Return the head item of list `xs`, panic if empty |
| `(↑ x)` | Return first element of list `xs`. Tight-binding prefix operator |
| `nil?` | `true` if list `xs` is empty, `false` otherwise |
| `non-nil?` | `true` if list `xs` is non-empty, `false` otherwise |
| `null?` | `true` if `x` is null, `false` otherwise |
| `(x ✓)` | `true` if `x` is not null, `false` otherwise. Postfix not-null check operator |
| `coalesce(xs)` | Return the first non-null element from list `xs`, or null if all are null |
| `head-or(d, xs)` | Return the head item of list `xs` or default `d` if empty |
| `tail` | Return list `xs` without the head item. [] causes error |
| `tail-or(d, xs)` | Return list `xs` without the head item or `d` for empty list |
| `nil` | Identical to `[]`, the empty list |
| `first` | Return first item of list `xs` - error if the list is empty |
| `second(xs)` | Return second item of list - error if there is none |
| `second-or(d, xs)` | Return second item of list - default `d` if there is none |

## List Construction

| Function | Description |
|----------|-------------|
| `repeat(i)` | Return infinite list of instances of item `i` |
| `iterate(f, i)` | Return list of `i` with subsequent repeated applications of `f` to `i` |
| `ints-from` | Return infinite list of integers from `n` upwards, alias `iota` for backwards compatibility |
| `range(b, e)` | Return list of ints from `b` to `e` (not including `e`) |
| `cycle(l)` | Create infinite list by cycling elements of list `l` |

## Transformations

| Function | Description |
|----------|-------------|
| `take(n, l)` | Return initial segment of integer `n` elements from list `l` |
| `drop(n, l)` | Return result of dropping integer `n` elements from list `l` |
| `take-until(p?)` | Initial elements of list `l` while `p?` is false |
| `drop-while(p?, l)` | Skip initial elements of list `l` while `p?` is true |
| `drop-until(p?)` | Skip initial elements of list `l` while `p?` is false |
| `map(f, l)` | Map function `f` over list `l` |
| `map2(f, l1, l2)` | Map function `f` over lists `l1` and `l2`, until the shorter is exhausted |
| `cross(f, xs, ys)` | Apply `f` to every combination of elements from `xs` and `ys` (cartesian product) |
| `filter(p?, l)` | Return list of elements of list `l` that satisfy predicate `p?` |
| `remove(p?, l)` | Return list of elements of list `l` that do not satisfy predicate `p?` |
| `reverse(l)` | Reverse list `l` |

## Combining Lists

| Function | Description |
|----------|-------------|
| `zip-with` | Map function `f` over lists `l1` and `l2`, until the shorter is exhausted |
| `zip` | List of pairs of elements  `l1` and `l2`, until the shorter is exhausted |
| `append(l1, l2)` | Concatenate two lists `l1` and `l2` |
| `prepend` | Concatenate two lists with `l1` after `l2` |
| `concat(ls)` | Concatenate all lists in `ls` together |
| `mapcat(f)` | Map items in l with `f` and concatenate the resulting lists |
| `zip-apply(fs, vs)` | Apply fns in list `fs` to corresponding values in list `vs`, until shorter is exhausted |

## Splitting Lists

| Function | Description |
|----------|-------------|
| `split-when(p?, l)` | Split list where `p?` becomes true and return pair |
| `partition(n)` | List of lists of non-overlapping segments of list `l` of size `n` |

## Folds and Scans

| Function | Description |
|----------|-------------|
| `foldl(op, i, l)` | Left fold operator `op` over list `l` starting from value `i` |
| `foldr(op, i, l)` | Right fold operator `op` over list `l` ending with value `i` |
| `scanl(op, i, l)` | Left scan operator `op` over list `l` starting from value `i` |
| `scanr(op, i, l)` | Right scan operator `op` over list `l` ending with value `i` |

## Predicates

| Function | Description |
|----------|-------------|
| `all-true?(l)` | True if and only if all items in list `l` are true |
| `all(p?, l)` | True if and only if all items in list `l` satisfy predicate `p?` |
| `any-true?(l)` | True if and only if any items in list `l` are true |
| `any(p?, l)` | True if and only if any items in list `l` satisfy predicate `p?` |

## Sorting

| Function | Description |
|----------|-------------|
| `qsort(lt, xs)` | Sort `xs` using 'less-than' function `lt` |
| `sort-nums` | Sort list of numbers ascending (Rust-level intrinsic) |
| `sort-strs(xs)` | Sort list of strings or symbols ascending |
| `sort-zdts(xs)` | Sort list of zoned date-times ascending |
| `sort-by-num(key-fn)` | Sort list `xs` ascending by numeric key extracted with `key-fn` |
| `sort-by-str(key-fn)` | Sort list `xs` ascending by string key extracted with `key-fn` |
| `sort-by-zdt(key-fn)` | Sort list `xs` ascending by zoned date-time key extracted with `key-fn` |

### Sorting Examples

```eu
nums: [3, 1, 4, 1, 5] sort-nums          # [1, 1, 3, 4, 5]
words: ["banana", "apple", "cherry"] sort-strs  # ["apple", "banana", "cherry"]

people: [{name: "Zara" age: 30}, {name: "Alice" age: 25}]
by-name: people sort-by-str(.name)       # sorted by name
by-age: people sort-by-num(.age)         # sorted by age
```

## Other

| Function | Description |
|----------|-------------|
| `nth(n, l)` | Return `n`th item of list if it exists, otherwise error |
| `(l !! r)` | Return `n`th item of list `l` if it exists, otherwise error. For arrays, `n` must be a coordinate list (e.g. `[row, col]`) and !! delegates to `arr.get` |
| `count(l)` | Return count of items in list `l` |
| `last` | Return last element of list `l` |
| `over-sliding-pairs(f, l)` | Apply binary fn `f` to each overlapping pair in `l` to form new list |
| `differences` | Calculate difference between each overlapping pair in list of numbers `l` |

## Other

| Function | Description |
|----------|-------------|
| `reduce(op, l)` | Left fold with no initial value; uses first element as seed. Panics on empty list |
| `tails(l)` | Return list of successive tails of `l`: `[l, tail(l), tail(tail(l)), ...]` |
| `iota(n)` | Return infinite list of integers from `n` upwards |
| `ℕ` | The natural numbers: `[0, 1, 2, ...]` |
| `butlast(l)` | Return all elements of list `l` except the last |
| `unzip(pairs)` | List of pairs to pair of lists. Inverse of zip |
| `interleave(a, b)` | Alternate elements from lists `a` and `b`. When one is exhausted, the remainder of the other is appended |
| `partition-all(n)` | List of lists of non-overlapping segments of `l`, including any final short chunk |
| `group-consecutive-by(f, xs)` | Group consecutive elements where `f` returns equal values into sublists |
| `group-consecutive` | Group consecutive equal elements into sublists |
| `uniq(xs)` | Remove consecutive duplicates from a list |
| `running-max` | Running maximum over a number list.
`[3, 1, 4, 1]` → `[3, 3, 4, 4]`. (Rust-level intrinsic) |
| `running-min` | Running minimum over a number list.
`[3, 1, 4, 1]` → `[3, 1, 1, 1]`. (Rust-level intrinsic) |
| `running-sum` | Cumulative sum over a number list.
`[3, 1, 4, 1]` → `[3, 4, 8, 9]`. (Rust-level intrinsic) |
