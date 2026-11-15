## Cost

Lists are singly linked: a list is consumed from the front.

| Operation | Cost |
|-----------|------|
| `head`, `tail`, `cons` | O(1) |
| `nth`, `!!`, `take(n)`, `drop(n)` | O(n) in the index on lists (`!!` is O(1) on arrays) — convert to a vec (`vec.of`) for O(1) indexed access |
| `count`, `last`, `reverse` | O(n) |
| `xs ++ ys` | O(length of `xs`) |
| `sort-nums`, `sort-strs`, `sort-by*` | O(n log n) |

Repeated `!!` over the same list in a loop is quadratic.
