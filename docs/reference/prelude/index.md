# Prelude Reference

The eucalypt **prelude** is a standard library of functions, operators,
and constants that is automatically loaded before your code runs.

You can suppress the prelude with `-Q` if needed, though this leaves
a very bare environment (even `true`, `false`, and `if` are defined
in the prelude).

## Categories

- [Lists](lists.md) -- list construction, transformation, folding, sorting (64 entries)
- [Blocks](blocks.md) -- block construction, access, merging, transformation (52 entries)
- [Strings](strings.md) -- string manipulation, regex, formatting (26 entries)
- [Numbers and Arithmetic](numbers.md) -- numeric operations and predicates (14 entries)
- [Booleans and Comparison](booleans.md) -- boolean logic and comparison operators (13 entries)
- [Combinators](combinators.md) -- function composition, application, utilities (12 entries)
- [Calendar](calendar.md) -- date and time functions (5 entries)
- [Sets](sets.md) -- set operations (11 entries)
- [Random Numbers](random.md) -- random number generation, monadic random: namespace (20 entries)
- [Metadata](metadata.md) -- metadata and assertion functions (7 entries)
- [IO](io.md) -- environment, time, argument access, and monad utility (16 entries)

*240 documented entries in total.*
