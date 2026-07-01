# Prelude Reference

The eucalypt **prelude** is a standard library of functions, operators, and constants that is automatically loaded before your code runs. You can suppress the prelude with `-Q` if needed, though this leaves a very bare environment (even `true`, `false`, and `if` are defined in the prelude).

## Categories

- [Lists](lists.md) -- list construction, transformation, folding, sorting (91 entries)
- [Blocks](blocks.md) -- block construction, access, merging, transformation (66 entries)
- [Strings](strings.md) -- string manipulation, regex, formatting (36 entries)
- [Numbers and Arithmetic](numbers.md) -- numeric operations and predicates (30 entries)
- [Booleans and Comparison](booleans.md) -- boolean logic and comparison operators (12 entries)
- [Combinators](combinators.md) -- function composition, application, utilities (14 entries)
- [Calendar](calendar.md) -- date and time functions (8 entries)
- [Sets](sets.md) -- set operations (14 entries)
- [Random Numbers](random.md) -- random number generation, monadic random: namespace (15 entries)
- [Metadata](metadata.md) -- metadata and assertion functions (4 entries)
- [IO](io.md) -- environment, time, argument access, and monad utility (80 entries)

*370 documented entries in total.*

## Standard Libraries

These are not part of the prelude (they require an explicit import) but
ship with eucalypt:

- **[Lens Library](../../guide/lenses.md)** (`{ import: "lens.eu" }`) — composable
  lenses and traversals for reading and updating nested data: `view`, `over`,
  `to-list-of`, `parts-of`, `at`, `ix`, `item`, `element`, `each`, `filtered`,
  `each-element`, `filtered-elements`, `_value`, `_key`, `‹›` path shorthand.
