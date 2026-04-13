# parts-of Lens Combinator

**Bead**: eu-fa2x
**Status**: Design (brainstorming)
**Date**: 2026-04-13

## Overview

`parts-of(traversal)` turns a traversal into a lens that focuses on
the list of all traversed values. Assigning a list replaces the
traversed elements in order.

## Semantics

```eu,notest
view(parts-of(each), [1, 2, 3])                    # => [1, 2, 3]
over(parts-of(each), reverse, [1, 2, 3])            # => [3, 2, 1]
over(parts-of(each), sort-nums, [3, 1, 2])          # => [1, 2, 3]

# Deep composition
data: {x: [{y: 1}, {y: 2}, {y: 3}]}
view(parts-of(at(:x) ∘ each ∘ at(:y)), data)        # => [1, 2, 3]
over(parts-of(at(:x) ∘ each ∘ at(:y)), reverse, data)
  # => {x: [{y: 3}, {y: 2}, {y: 1}]}
```

## How Eucalypt Lenses Work

A lens/traversal is a function `(b -> fb) -> (a -> fa)` where the
behaviour is controlled by metadata on `k`:

| Consumer | `fmap` | `pure` | `ap` | Result |
|----------|--------|--------|------|--------|
| `view` | `flip(const)` | — | — | Extract focus |
| `over` | `identity` | `identity` | `identity` | Apply fn, rebuild |
| `to-list-of` | `flip(const)` | `-> []` | `++` | Collect foci |

## Approach: State-Based Distribution

`parts-of(traversal)` is a lens. When applied with continuation `k`:

### View (collect)

Same as `to-list-of(traversal, data)` — collect all foci into a
list, then pass to `k`.

### Over (distribute)

1. Collect all foci: `collected = to-list-of(traversal, data)`
2. Apply `k` to the collected list (via the lens protocol, `k`
   transforms the list)
3. Run the traversal again with a stateful functor that replaces
   each focus by consuming from the transformed list

The stateful distribution functor:

```eu,notest
# Each "value" is a state action: remaining-list -> [result, rest]
dist-fmap(f, action, s): {
  r: action(s)
}.([f(r head), r tail head])

dist-pure(v, s): [v, s]

dist-ap(f-action, x-action, s): {
  r1: f-action(s)
  r2: x-action(r1 tail head)
}.([r1 head(r2 head), r2 tail head])
```

The traversal is run with:
- `fmap: dist-fmap` — wraps each focus replacement in a state action
- `pure: dist-pure` — non-focused positions pass through unchanged
- `ap: dist-ap` — sequences the state actions

The "replace" function passed to each focus is `(old-val) -> pop
next from remaining list` — ignoring the old value and consuming
from the state.

The result is a state action `remaining-list -> [rebuilt-structure,
rest]`. Apply it to the transformed list to get the final structure.

## Implementation Sketch

```eu,notest
parts-of(traversal, k): {
  fmap: meta(k).fmap

  # Collect all foci
  collected: to-list-of(traversal)

  # Distribution: run traversal with stateful functor
  dist-fmap(f, action, s): {
    r: action(s)
  }.([f(r head), r tail head])

  dist-pure(v, s): [v, s]

  dist-ap(f-action, x-action, s): {
    r1: f-action(s)
    r2: x-action(r1 tail head)
  }.([r1 head(r2 head), r2 tail head])

  # The "replace" function: ignore old value, pop from list
  pop(old, remaining): [remaining head, remaining tail]

  # Run traversal with distribution functor, seeded with new values
  distribute(new-vals, data):
    (data traversal(pop // { fmap: dist-fmap, pure: dist-pure, ap: dist-ap })
      (new-vals)) head

  # Lens body: fmap over the collected list, then distribute
  s(data): fmap(distribute(data), k(data collected))
}.(s // meta(k))
```

## Open Questions

1. **Does the distribution functor compose correctly through
   lens ∘ traversal ∘ lens chains?** The `at` lens uses `fmap` but
   not `pure`/`ap`. When `parts-of` wraps a composition, the inner
   lenses need to thread state correctly.

2. **What happens if the transformed list has a different length?**
   Haskell's `parts-of` silently drops excess or fills with the
   original. We should probably error or document the requirement
   that lengths must match.

3. **Performance**: two full traversals (collect + distribute).
   Acceptable for the use cases but worth noting.

## Files

| File | Change |
|------|--------|
| `lib/lens.eu` | `parts-of` definition + tests |

No Rust changes needed.

## Testing

- `view(parts-of(each), [1, 2, 3])` => `[1, 2, 3]`
- `over(parts-of(each), reverse, [1, 2, 3])` => `[3, 2, 1]`
- `over(parts-of(each), sort-nums, [3, 1, 2])` => `[1, 2, 3]`
- `over(parts-of(filtered(> 2)), reverse, [1, 4, 2, 5, 3])` =>
  `[1, 5, 2, 4, 3]` (only filtered positions swapped)
- Deep: `over(parts-of(at(:x) ∘ each ∘ at(:y)), reverse, data)`
- `view(parts-of(at(:x) ∘ each ∘ at(:y)), data)` => list of y values
