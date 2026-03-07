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
