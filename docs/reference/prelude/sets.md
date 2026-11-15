# Sets

> **Sets hold primitives only.** Elements must be numbers, strings or
> symbols. A list, block or other structured value is an error
> (`expected a primitive value but found non-boxed constructor`).
> To keep coordinates or other tuples in a set, encode each one as a
> single number or string first (see below).

## Cost

| Operation | Cost |
|-----------|------|
| `set.from-list` | O(n log n) |
| `set.contains?`, `∈`, `∉` | O(log n) |
| `set.add`, `set.remove` | O(n) — each returns a fresh copy of the set |
| `set.union`, `set.intersect`, `set.diff` | O(n + m) |
| `set.to-list`, `set.size` | O(n), O(1) |

Because `set.add` copies, building a large set by repeated `set.add` in
a fold is quadratic. Collect the elements in a list and call
`set.from-list` once instead.

## Coordinate encoding

Encode a coordinate pair as one number, choosing a multiplier larger
than any coordinate:

```eu
key([x, y]): x * 100000 + y

visited: [[0, 0], [3, 4], [3, 4]] map(key) set.from-list

seen: ([3, 4] key) ∈ visited        //=> true
unseen: ([9, 9] key) ∈ visited      //=> false
count: visited set.size             //=> 2
```

## Set Operations

| Function | Description |
|----------|-------------|
| `set.from-list(xs)` | Create a set from list `xs` of primitive values |
| `set.to-list` | Return sorted list of elements in set `s` |
| `set.add` | Add element `e` to set `s` |
| `set.remove` | Remove element `e` from set `s` |
| `set.contains?` | True if set `s` contains element `e` |
| `set.size` | Return number of elements in set `s` |
| `set.empty?(s)` | True if set `s` has no elements |
| `set.union` | Return union of sets `a` and `b` |
| `set.intersect` | Return intersection of sets `a` and `b` |
| `set.diff` | Return elements in set `a` that are not in set `b` |
| `set.sample(k, s, stream)` | Random monad action: pick `n` random elements from set `s`. Returns value/rest block |
| `∅` | The empty set |
| `(l ∈ r)` | True if element `v` is a member of set `s` |
| `(l ∉ r)` | True if element `v` is not a member of set `s` |

```eu
s: set.from-list([1, 2, 3, 2, 1])
# s contains {1, 2, 3} (duplicates removed)
```

## Set Algebra

```eu
a: set.from-list([1, 2, 3])
b: set.from-list([2, 3, 4])
u: set.union(a, b) set.to-list       # [1, 2, 3, 4]
i: set.intersect(a, b) set.to-list   # [2, 3]
d: set.diff(a, b) set.to-list        # [1]
```
