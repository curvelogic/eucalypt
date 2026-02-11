# Lists

## Basic Operations

| Function | Description |
|----------|-------------|
| `cons` | Construct new list by prepending item `h` to list `t` |
| `head` | Return the head item of list `xs`, panic if empty |
| `(↑ xs)` | Return first element of list `xs`. Tight-binding prefix operator |
| `nil?` | `true` if list `xs` is empty, `false` otherwise |
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
| `ints-from(n)` | Return infinite list of integers from `n` upwards |
| `range(b, e)` | Return list of ints from `b` to `e` (not including `e`) |
| `cycle(l)` | Create infinite list by cycling elements of list `l` |

## Transformations

| Function | Description |
|----------|-------------|
| `take(n, l)` | Return initial segment of integer `n` elements from list `l` |
| `drop(n, l)` | Return result of dropping integer `n` elements from list `l` |
| `take-while(p?, l)` | Initial elements of list `l` while `p?` is true |
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
| `split-at(n, l)` | Split list in to at `n`th item and return pair |
| `split-after(p?, l)` | Split list where `p?` becomes false and return pair |
| `split-when(p?, l)` | Split list where `p?` becomes true and return pair |
| `window(n, step, l)` | List of lists of sliding windows over list `l` of size `n` and offest `step` |
| `partition(n)` | List of lists of non-overlapping segments of list `l` of size `n` |
| `discriminate(pred, xs)` | Return pair of `xs` for which `pred(_)` is true and `xs` for which `pred(_)` is false |

## Folds and Scans

| Function | Description |
|----------|-------------|
| `foldl(op, i, l)` | Left fold operator `op` over list `l` starting from value `i`  |
| `foldr(op, i, l)` | Right fold operator `op` over list `l` ending with value `i`  |
| `scanl(op, i, l)` | Left scan operator `op` over list `l` starting from value `i`  |
| `scanr(op, i, l)` | Right scan operator `op` over list `l` ending with value `i`  |

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
| `group-by(k, xs)` | Group xs by key function returning block of key to subgroups, maintains order |
| `qsort(lt, xs)` | Sort `xs` using 'less-than' function `lt` |
| `sort-nums(xs)` | Sort list of numbers ascending |
| `sort-strs(xs)` | Sort list of strings or symbols ascending |
| `sort-zdts(xs)` | Sort list of zoned date-times ascending |
| `sort-by(key-fn, cmp, xs)` | Sort list `xs` by key extracted with `key-fn` using comparator `cmp` |
| `sort-by-num(key-fn)` | Sort list `xs` ascending by numeric key extracted with `key-fn` |
| `sort-by-str(key-fn)` | Sort list `xs` ascending by string key extracted with `key-fn` |
| `sort-by-zdt(key-fn)` | Sort list `xs` ascending by zoned date-time key extracted with `key-fn` |

### Sorting Examples

```eu
nums: [3, 1, 4, 1, 5] sort-nums          # [1, 1, 3, 4, 5]
words: ["banana", "apple", "cherry"] sort-strs  # ["apple", "banana", "cherry"]

people: [{name: "Zara" age: 30}, {name: "Alice" age: 25}]
by-name: people sort-by-str(_.name)       # sorted by name
by-age: people sort-by-num(_.age)         # sorted by age
```

## Other

| Function | Description |
|----------|-------------|
| `nth(n, l)` | Return `n`th item of list if it exists, otherwise panic |
| `(l !! n)` | Return `n`th item of list if it exists, otherwise error |
| `count(l)` | Return count of items in list `l` |
| `last` | Return last element of list `l` |
| `over-sliding-pairs(f, l)` | Apply binary fn `f` to each overlapping pair in `l` to form new list |
| `differences` | Calculate difference between each overlapping pair in list of numbers `l` |
