## Cost

A block is searched linearly by key; there is no index. `lookup`,
`lookup-or` and `has` are all O(n) in the number of keys, and `merge` is linear in the size of
both operands. A block is the right shape for records and small configuration, but not for a
large lookup table queried in a loop: use a `dict` (see the [dicts reference](dicts.md)) for
large keyed tables, a set for membership, and a vec (`vec.of`) for indexed access to numbers.
