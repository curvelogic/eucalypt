# Prelude Reference

The eucalypt **prelude** is a standard library of functions, operators,
and constants that is automatically loaded before your code runs. It
provides around 250 documented functions and operators across 11
categories.

You can suppress the prelude with `-Q` if needed, though this leaves
a very bare environment (even `true`, `false`, and `if` are defined
in the prelude).

## Categories

- [Lists](lists.md) -- list construction, transformation, folding, sorting
- [Blocks](blocks.md) -- block construction, access, merging, transformation
- [Strings](strings.md) -- string manipulation, regex, formatting
- [Numbers and Arithmetic](numbers.md) -- numeric operations and predicates
- [Booleans and Comparison](booleans.md) -- boolean logic and comparison operators
- [Combinators](combinators.md) -- function composition, application, utilities
- [Calendar](calendar.md) -- date and time functions
- [Sets](sets.md) -- set operations
- [Random Numbers](random.md) -- random number generation
- [Metadata](metadata.md) -- metadata and assertion functions
- [IO](io.md) -- environment, time, and argument access

> **Maintainer note:** Run `python3 scripts/extract-prelude-docs.py --check`
> to verify that all documented prelude functions are covered by these
> reference pages. The script parses `lib/prelude.eu` backtick doc
> strings and reports any undocumented entries.
