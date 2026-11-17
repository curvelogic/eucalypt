```eu
s: set.of([1, 2, 3, 2, 1])
# s contains {1, 2, 3} (duplicates removed)
```

## Set Algebra

```eu
a: set.from-list([1, 2, 3])
b: set.from-list([2, 3, 4])
u: a set.union(b) set.to-list      //=> [1, 2, 3, 4]
i: a set.intersect(b) set.to-list  //=> [2, 3]
d: a set.diff(b) set.to-list       //=> [1]
d2: set.diff(b, a) set.to-list     //=> [1]
```
