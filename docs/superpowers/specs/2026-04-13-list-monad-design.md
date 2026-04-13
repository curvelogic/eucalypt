# List Monad — `:for` Blocks

**Bead**: eu-6287
**Status**: Design
**Date**: 2026-04-13

## Overview

A list monad enabling list comprehension via monadic block syntax.
Pure prelude implementation — no Rust changes needed.

## Semantics

The list monad's bind is `mapcat` (flatMap) and return is singleton
list wrapping:

```eu,notest
for-bind(m, f): m mapcat(f)
for-ret(v): [v]
```

A `{ :for ... }` block desugars into nested `for.bind` calls,
exactly like `{ :let ... }` or `{ :random ... }`.

## Examples

### Simple mapping

```eu,notest
{ :for x: [1, 2, 3] }.(x * 2)
# desugars to: [1,2,3] mapcat(λx. [x * 2])
# => [2, 4, 6]
```

### Cartesian product

```eu,notest
{ :for x: [1, 2], y: [10, 20] }.(x + y)
# desugars to: [1,2] mapcat(λx. [10,20] mapcat(λy. [x + y]))
# => [11, 21, 12, 22]
```

### Dependent binding

```eu,notest
{ :for x: [1, 2, 3], y: [x, x * 10] }.(y)
# each y draws from a list that depends on x
# => [1, 10, 2, 20, 3, 30]
```

### Filtering with guard

```eu,notest
{ :for x: [1, 2, 3, 4, 5], _: for.guard(x > 3) }.(x)
# guard(true) => [null], guard(false) => []
# [] causes mapcat to eliminate that branch
# => [4, 5]
```

### Filtering with when

```eu,notest
{ :for x: [1, "two", 3, "four", 5], n: for.when(number?, x) }.(n * 10)
# when(number?, 1) => [1], when(number?, "two") => []
# filters and binds the passing value in one step
# => [10, 30, 50]
```

### Implicit return

```eu,notest
{ :for x: [1, 2, 3] }
# implicit return synthesises { x: x } for each x
# => [{ x: 1 }, { x: 2 }, { x: 3 }]

{ :for x: [:a, :b], y: [1, 2] }
# => [{ x: :a, y: 1 }, { x: :a, y: 2 }, { x: :b, y: 1 }, { x: :b, y: 2 }]
```

### In generalised lookup position

```eu,notest
{ items: [1, 2, 3] }.{ :for x: items }.(x * 2)
# items visible from LHS via generalised lookup
# => [2, 4, 6]
```

## Implementation

### Prelude additions

```eu,notest
# List monad bind and return
for-bind(m, f): m mapcat(f)
for-ret(v): [v]

# Register as monad namespace
` { monad: true }
for: monad{bind: for-bind, return: for-ret} {

  ` "for.guard(cond) - filter: continues when cond is true, eliminates when false."
  guard(cond): if(cond, [null], [])

  ` "for.when(pred?, v) - filter: continues when v satisfies pred?, eliminates otherwise."
  when(pred?, v): if(v pred?, [v], [])
}
```

### Why `for` not `list`

`list` could conflict with expectations of a list utility namespace.
`for` mirrors the list comprehension `for x in xs` pattern familiar
from Python/Haskell/Scala. The `:for` block tag reads naturally:
"for x drawn from xs, for y drawn from ys, yield expr."

### Guard helpers

| Function | Description |
|----------|-------------|
| `for.guard(cond)` | `[null]` when true, `[]` when false — boolean filter |
| `for.when(pred?, v)` | `[v]` when `v` satisfies `pred?`, `[]` otherwise — predicate filter preserving value |

`for.guard` is assigned to `_` (discarded). `for.when` returns the
filtered value for subsequent bindings.

## Files

| File | Change |
|------|--------|
| `lib/prelude.eu` | `for-bind`, `for-ret`, `for` namespace with `guard` and `when` |

No Rust changes. No new intrinsics. No `build.rs` changes (prelude
already tracked).

## Testing

Harness test covering:
- Simple mapping
- Cartesian product
- Dependent binding
- Guard filtering
- `for.when` predicate filtering
- Implicit return (single and multi-binding)
- In generalised lookup position
- Empty list input
- Nested `:for` within `:for` (if meaningful)

## Interaction with other monads

`:for` blocks work in generalised lookup position like any other
monad (per eu-461s fix). They can appear in chains:

```eu,notest
{ data: [1, 2, 3] }.{ :for x: data, _: for.guard(x > 1) }.(x * 10)
# => [20, 30]
```

The result of a `:for` block is always a list. Subsequent `.name`
would be a key lookup on a list (likely error). Use `.(expr)` or
pipeline functions to process the result.
