# Sorting Blocks — Design

## Problem

Users want blocks to render with keys in alphabetical order for clean,
deterministic JSON/YAML output. Currently blocks preserve insertion
order.

## Scope

This feature is purely about **output ordering** — reordering block
pairs by key for presentation. Performance improvements (O(1) lookup
via indexmap) are deferred to eu-brj.

## Solution

A prelude-level `sort-keys` function that reorders block pairs
alphabetically. This requires string/symbol comparison intrinsics
which don't currently exist.

## String Comparison Intrinsics

New intrinsics for lexicographic string comparison:

- `STR_LT` — string less-than
- `STR_GT` — string greater-than
- `STR_LTE` — string less-than-or-equal
- `STR_GTE` — string greater-than-or-equal

These accept both strings and symbols, comparing by their underlying
text. Implemented in `src/eval/stg/string.rs` following the pattern of
numeric comparison intrinsics.

## Prelude Functions

Exposed inside the `str` block (consistent with `str.split`,
`str.upper`, `str.base64-encode` etc.):

- `str.lt(a, b)` — true if a < b lexicographically
- `str.gt(a, b)` — true if a > b lexicographically
- `str.lte(a, b)` — true if a <= b
- `str.gte(a, b)` — true if a >= b

And the main feature:

- `sort-keys(b)` — returns block with pairs reordered by key
  alphabetically

## Implementation

### String comparison intrinsics (`src/eval/stg/string.rs`)

Follow the pattern of `LT`/`GT` in `arith.rs`. Accept two arguments,
extract as strings or symbols, use Rust's `str::cmp()` for
lexicographic ordering, return boolean.

```rust
// Pseudocode
fn execute(&self, ..., args: &[Ref]) -> Result<(), ExecutionError> {
    let a = str_or_sym_arg(machine, view, &args[0])?;
    let b = str_or_sym_arg(machine, view, &args[1])?;
    let result = a.as_str() < b.as_str();  // or >, <=, >=
    machine_return_bool(machine, view, result)
}
```

### Prelude functions (`lib/prelude.eu`)

Inside the existing `str` block in the prelude:

```eu
str: {
  # ... existing functions ...
  lt(a, b): a '__STR_LT(b)
  gt(a, b): a '__STR_GT(b)
  lte(a, b): a '__STR_LTE(b)
  gte(a, b): a '__STR_GTE(b)
}

sort-keys(b): b elements qsort(pair-key-lt) block
  pair-key-lt(p1, p2): first(p1) str.lt(first(p2))
```

Note: `str.lt` as a higher-order argument (`qsort(str.lt)`) is a static
dot access — DCE correctly tracks it and can still eliminate unused `str`
members.

## Testing

- Harness tests for string comparison: empty strings, single chars,
  unicode, mixed case
- Harness tests for symbol comparison: `:a` vs `:b`, `:foo` vs `:bar`
- Harness tests for `sort-keys`: empty block, single key, multiple
  keys, already sorted

**Note**: With polymorphic `lt`/`gt` (eu-u1m), `sort-keys` could also
use `lt` directly. The `str.lt` variants remain as explicit typed
alternatives and are used in `sort-keys` for clarity.

## Out of Scope

- Performance improvements for block lookup (eu-brj)
- Custom sort order / locale-aware comparison
