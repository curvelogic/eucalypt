# Sets

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
| `(∅)` | The empty set |

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
