# Set Implementation — Design

## Problem

Eucalypt lacks a native set data type. Users must use lists and manually
handle deduplication and membership testing, which is inefficient and
verbose.

## Solution

A native Set type with O(1) operations, stored as `HashSet<Primitive>` in
Rust, with prelude functions for construction and set algebra.

## Key Properties

- **Native type** — new `Native::Set` variant
- **Primitive elements** — numbers, strings, symbols only
- **O(1) operations** — hash-based membership and mutation
- **Set algebra** — union, intersection, difference
- **Serializes as array** — JSON/YAML output is `[1, 2, 3]`

## Syntax Examples

```eu
empty: ∅                              # empty set (0-arity operator)
nums: [1, 2, 2, 3] set.from-list      # {1, 2, 3}
has-two: nums set.contains?(2)        # true

evens: [2, 4, 6] set.from-list
common: nums set.intersect(evens)     # {2}
all: nums set.union(evens)            # {1, 2, 3, 4, 6}
```

## Native Type

**New Native variant** in `src/eval/memory/syntax.rs`:

```rust
pub enum Native {
    Sym(RefPtr<HeapString>),  // becomes Sym(SymbolId) after eu-4af
    Str(RefPtr<HeapString>),
    Num(Number),
    Zdt(DateTime<FixedOffset>),
    Set(RefPtr<HeapSet>),  // NEW
}
```

**Ordering**: eu-4af (symbol interning) lands first, so `Sym` is
`Sym(SymbolId)` when sets are implemented.

**HeapSet structure** in `src/eval/memory/set.rs`:

```rust
pub struct HeapSet {
    elements: HashSet<Primitive>,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Primitive {
    Num(OrderedNumber),  // wrapper for hashable number
    Str(String),
    Sym(SymbolId),       // interned symbol ID (eu-4af)
}
```

Set intrinsics access the symbol pool via `IntrinsicMachine::symbol_pool()`
for display/construction.

**Note on numbers**: Rust's `f64` isn't `Hash`. An `OrderedNumber` wrapper
handles this by implementing Hash for the numeric representation.

**GC integration**: `HeapSet` allocated on heap. GC scans don't trace into
set contents since primitives contain no heap references.

## Intrinsics

New module `src/eval/stg/set.rs`:

| Intrinsic | Args | Description |
|-----------|------|-------------|
| `SET.EMPTY` | 0 | Returns empty set |
| `SET.FROM_LIST` | 1 | List → Set |
| `SET.TO_LIST` | 1 | Set → List |
| `SET.ADD` | 2 | Add element to set |
| `SET.REMOVE` | 2 | Remove element from set |
| `SET.CONTAINS` | 2 | Check membership → bool |
| `SET.SIZE` | 1 | Number of elements |
| `SET.UNION` | 2 | Union of two sets |
| `SET.INTERSECT` | 2 | Intersection of two sets |
| `SET.DIFF` | 2 | Difference (set1 - set2) |

## Prelude API

```eu
` { doc: "Set operations" }
set: {
  from-list: __SET.FROM_LIST
  to-list: __SET.TO_LIST
  add: __SET.ADD
  remove: __SET.REMOVE
  contains?: __SET.CONTAINS
  size: __SET.SIZE
  empty?: s -> s size eq(0)
  union: __SET.UNION
  intersect: __SET.INTERSECT
  diff: __SET.DIFF
}

` { doc: "Empty set" }
∅: __SET.EMPTY
```

## Rendering

Sets render as JSON/YAML arrays, sorted for deterministic output.
Numbers sort numerically, strings and symbols sort lexicographically.
Numbers sort before strings, strings before symbols.

```eu
[3, 1, 2, 2] set.from-list   # renders as [1, 2, 3]
["c", "a"] set.from-list     # renders as ["a", "c"]
```

The renderer handles `Native::Set` by collecting elements, sorting via
`Primitive::cmp()`, and emitting as a list.

## Testing

- **Construction**: `set.from-list` deduplicates, `∅` is empty
- **Membership**: `set.contains?` returns true/false correctly
- **Mutation**: `set.add` adds new, ignores existing; `set.remove` removes
- **Algebra**: `union`, `intersect`, `diff` produce correct results
- **Round-trip**: `set.from-list(set.to-list(s))` equals original
- **Rendering**: Set outputs as valid JSON array
- **Edge cases**: Empty sets, single element, duplicate adds

## Error Handling

- Adding non-primitive to set → runtime error
- Set operations on non-sets → runtime error

## Out of Scope

- Block/nested structure membership (complex equality)
- Set literal syntax `{1, 2, 3}` (future enhancement)
- Subset/disjoint predicates (future enhancement)
- Ordered sets (future enhancement)

## Dependencies

- eu-7kc (0-arity operators) ✓ — needed for `∅`
- eu-4af (symbol interning) — `Primitive::Sym` uses `SymbolId`
