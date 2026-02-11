# Metadata

| Function | Description |
|----------|-------------|
| `with-meta(m, e)` | Add metadata block `m` to expression `e` |
| `e // m` | Operator form of `with-meta` |
| `meta(e)` | Retrieve metadata from expression |
| `merge-meta(m, e)` | Merge into existing metadata |
| `e //<< m` | Operator form of `merge-meta` |

## Assertions

| Operator | Description |
|----------|-------------|
| `e //= v` | Check if `e` equals `v`, return boolean |
| `e //=> v` | Assert `e` equals `v`, return `e` or panic |
| `e //=? f` | Assert `e` satisfies predicate `f` |
| `e //!? f` | Assert `e` does not satisfy `f` |
| `e //!` | Assert `e` is true |
| `e //!!` | Assert `e` is false |
