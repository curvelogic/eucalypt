# Combinators

| Function | Description |
|----------|-------------|
| `identity(v)` | Return `v` unchanged |
| `const(k)` | Function that always returns `k` |
| `-> k` | Operator form of `const` |
| `compose(f, g, x)` | Apply `f` to `g(x)` |
| `f ∘ g` | Composition: `g` then `f` |
| `f ; g` | Composition: `f` then `g` |
| `l @ r` | Application: `l(r)` |
| `apply(f, xs)` | Apply `f` to args in list |
| `flip(f)` | Swap argument order |
| `complement(p?)` | Invert predicate |
| `curry(f)` | Convert `f([x,y])` to `f(x,y)` |
| `uncurry(f)` | Convert `f(x,y)` to `f([x,y])` |
| `juxt(f, g, x)` | Return `[f(x), g(x)]` |
| `fnil(f, v, x)` | Replace null with `v` before applying `f` |

## Pairs

| Function | Description |
|----------|-------------|
| `pair(k, v)` | Create pair `[k, v]` |
| `bimap(f, g, pr)` | Apply `f` to first, `g` to second |
| `map-first(f, prs)` | Apply `f` to first elements |
| `map-second(f, prs)` | Apply `f` to second elements |
