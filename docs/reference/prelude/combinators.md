# Combinators

## Combinators

| Function | Description |
|----------|-------------|
| `identity(v)` | Identity function, return value `v` |
| `const(k, _)` | Return single arg function that always returns `k` |
| `(-> x)` | Const; return single arg function that always returns `k` |
| `compose(f, g, x)` | Apply function `f` to `g(x)` |
| `apply(f, xs)` | Apply function `f` to arguments in list `xs` |
| `flip(f, x, y)` | Flip arguments of function `f`, flip(f)(x, y) == f(y, x) |
| `complement(p?)` | Invert truth value of predicate function |
| `curry(f, x, y)` | Turn f((x, y)) into f(x, y) |
| `uncurry(f, l)` | Turn f(x, y) into f([x, y]). Semantically: (a → b → c) → (a, b) → c |
| `cond` | Multi-way conditional: evaluate each `cond => result` clause in turn, returning the first matching result, or the last element as a default |
| `(l => r)` | Build a cond clause: condition `c` paired with result `r`. Use inside `cond([...])` lists |
| `(l ⇒ r)` | Unicode alias for `c => r` |
| `juxt(f, g, x)` | Apply both `f` and `g` to `x`, returning pair (f(x), g(x)) |

## Utilities

| Function | Description |
|----------|-------------|
| `fnil(f, v, x)` | Return a function equivalent to f except it sees `x` instead of `null` when null is passed |
