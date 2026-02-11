# Lists

## Basic Operations

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

## List Construction

| Function | Description |
|----------|-------------|
| `repeat(i)` | Infinite list of item `i` |
| `ints-from(n)` | Infinite list of integers from `n` upwards |
| `range(b, e)` | List of integers from `b` to `e` (exclusive) |
| `cycle(l)` | Infinite list cycling elements of `l` |
| `iterate(f, i)` | List of `i`, `f(i)`, `f(f(i))`, ... |

## Transformations

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

## Combining Lists

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

## Splitting Lists

| Function | Description |
|----------|-------------|
| `split-at(n, l)` | Split at index `n`, return pair |
| `split-after(p?, l)` | Split where `p?` becomes false |
| `split-when(p?, l)` | Split where `p?` becomes true |
| `window(n, step, l)` | Sliding windows of size `n` with offset `step` |
| `partition(n, l)` | Non-overlapping segments of size `n` |
| `discriminate(pred, xs)` | Split into [matches, non-matches] |

## Folds and Scans

| Function | Description |
|----------|-------------|
| `foldl(op, i, l)` | Left fold with initial value `i` |
| `foldr(op, i, l)` | Right fold with final value `i` |
| `scanl(op, i, l)` | Left scan (intermediate fold values) |
| `scanr(op, i, l)` | Right scan |

## Predicates

| Function | Description |
|----------|-------------|
| `all(p?, l)` | True if all elements satisfy `p?` |
| `all-true?(l)` | True if all elements are true |
| `any(p?, l)` | True if any element satisfies `p?` |
| `any-true?(l)` | True if any element is true |

## Sorting

| Function | Description |
|----------|-------------|
| `qsort(lt, xs)` | Sort using less-than function `lt` |
| `group-by(k, xs)` | Group by key function, returns block |

## Other

| Function | Description |
|----------|-------------|
| `over-sliding-pairs(f, l)` | Apply binary `f` to overlapping pairs |
| `differences(l)` | Differences between adjacent numbers |
