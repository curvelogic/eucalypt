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
