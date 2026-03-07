# Combinators

## Combinators

| Function | Description |
|----------|-------------|
| `identity(v)` | Identity function, return value `v` |
| `const(k, _)` | Return single arg function that always returns `k` |
| `(-> k)` | Const; return single arg function that always returns `k` |
| `compose(f, g, x)` | Apply function `f` to `g(x)` |
| `apply(f, xs)` | Apply function `f` to arguments in list `xs` |
| `flip(f, x, y)` | Flip arguments of function `f`, flip(f)(x, y) == f(y, x) |
| `complement(p?)` | Invert truth value of predicate function |
| `curry(f, x, y)` | Turn f([x, y]) into f' of two parameters (x, y) |
| `uncurry(f, l)` | Turn f(x, y) into f' that expects [x, y] as a list |
| `cond(l, d)` | In list `l` of [condition, value] select first true condition, returning value, else default `d` |
| `juxt(f, g, x)` | `juxt(f, g) - return function of `x` returning list of `f(x)` and g(x)` |

## Utilities

| Function | Description |
|----------|-------------|
| `fnil(f, v, x)` | Return a function equivalent to f except it sees `x` instead of `null` when null is passed |
