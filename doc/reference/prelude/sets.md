# Sets

The `set` namespace provides operations on sets of primitive values
(numbers, strings, symbols). Sets are unordered collections of unique
elements.

## Creating Sets

| Expression | Description |
|------------|-------------|
| `set.from-list(xs)` | Create a set from a list of values |
| `∅` | The empty set (Option-O on Mac, or use `set.from-list([])`) |

```eu
s: set.from-list([1, 2, 3, 2, 1])
# s contains {1, 2, 3} (duplicates removed)
```

## Operations

| Function | Description |
|----------|-------------|
| `set.add(e, s)` | Add element `e` to set `s` |
| `set.remove(e, s)` | Remove element `e` from set `s` |
| `set.contains?(e, s)` | True if set `s` contains element `e` |
| `set.size(s)` | Number of elements in set `s` |
| `set.empty?(s)` | True if set `s` has no elements |
| `set.to-list(s)` | Sorted list of elements in set `s` |

```eu
s: set.from-list([3, 1, 4, 1, 5])
has-three: s set.contains?(3)        # true
count: s set.size                     # 4 (duplicates removed)
elems: s set.to-list                  # [1, 3, 4, 5]
```

## Set Algebra

| Function | Description |
|----------|-------------|
| `set.union(a, b)` | Elements in either set |
| `set.intersect(a, b)` | Elements in both sets |
| `set.diff(a, b)` | Elements in `a` but not in `b` |

```eu
a: set.from-list([1, 2, 3])
b: set.from-list([2, 3, 4])
u: set.union(a, b) set.to-list       # [1, 2, 3, 4]
i: set.intersect(a, b) set.to-list   # [2, 3]
d: set.diff(a, b) set.to-list        # [1]
```
