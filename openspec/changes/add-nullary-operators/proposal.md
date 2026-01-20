# Change: Add Nullary (0-arity) Operators

## Why
Eucalypt supports unary (arity 1) and binary (arity 2) operators but not nullary (arity 0) operators. This prevents using operator symbols as constants, limiting mathematical notation like `∅` for empty set or `π` for pi. The syntax layer already parses nullary operator declarations but the core expression system ignores them.

## What Changes
- Extend `Fixity` enum with `Nullary` variant
- Update shunting yard algorithm to handle 0-arity operators
- Update gap-filling logic to recognise nullary operators
- Wire through nullary operator declarations from syntax to core

## Impact
- Affected specs: `operators` (new capability)
- Affected code:
  - `src/core/expr.rs` - Add `Nullary` to `Fixity` enum
  - `src/core/cook/shunt.rs` - Handle nullary in shunting algorithm
  - `src/core/cook/fill.rs` - Update gap-filling for nullary
  - `src/core/cook/fixity.rs` - Distribute nullary operator metadata
  - `src/core/desugar/` - Wire nullary declarations through desugaring

## Success Criteria
- `(∅): []` declares a nullary operator
- Using `∅` in expressions evaluates to `[]`
- Nullary operators work alongside unary and binary operators
- Existing operator tests continue to pass
