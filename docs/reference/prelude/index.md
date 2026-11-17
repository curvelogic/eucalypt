# Prelude Reference

The eucalypt **prelude** is a standard library of functions, operators, and constants that is automatically loaded before your code runs. You can suppress the prelude with `-Q` if needed, though this leaves a very bare environment (even `true`, `false`, and `if` are defined in the prelude).

## Categories

- [Lists](lists.md) -- list construction, transformation, folding, sorting (99 entries)
- [Blocks](blocks.md) -- block construction, access, merging, transformation (66 entries)
- [Strings](strings.md) -- string manipulation, regex, formatting (44 entries)
- [Numbers and Arithmetic](numbers.md) -- numeric operations and predicates (38 entries)
- [Booleans and Comparison](booleans.md) -- boolean logic and comparison operators (13 entries)
- [Combinators](combinators.md) -- function composition, application, utilities (14 entries)
- [Calendar](calendar.md) -- date and time functions (8 entries)
- [Sets](sets.md) -- set operations (15 entries)
- [Dicts](dicts.md) -- persistent ordered dictionaries (24 entries)
- [Random Numbers](random.md) -- random number generation, monadic random: namespace (15 entries)
- [Vecs and Arrays](arrays.md) -- vecs (O(1) indexed access) and n-dimensional arrays (32 entries)
- [Graph Algorithms](graphs.md) -- union-find, topological sort, Kruskal (3 entries)
- [Bitwise Operations](bits.md) -- bitwise and, or, xor, shifts, bit counts (9 entries)
- [Process Parallelism](parallel.md) -- experimental par-* combinators (5 entries)
- [Metadata](metadata.md) -- metadata and assertion functions (4 entries)
- [IO](io.md) -- environment, time, argument access, and monad utility (37 entries)

*426 documented entries in total.*

## Standard Libraries

These are not part of the prelude (they require an explicit import) but
ship with eucalypt:

- **[Lens Library](../../guide/lenses.md)** (`{ import: "lens.eu" }`) — composable
  lenses and traversals for reading and updating nested data: `view`, `over`,
  `to-list-of`, `parts-of`, `at`, `ix`, `item`, `element`, `each`, `filtered`,
  `each-element`, `filtered-elements`, `_value`, `_key`, `‹›` path shorthand.
