# Process Parallelism

## Parallel Combinators

| Function | Description |
|----------|-------------|
| `par-map(f, xs)` | EXPERIMENTAL (macOS/Linux only, semantics may change without notice). `par-map(f, xs)` - parallel map, order-preserving. Same value as `xs map(f)` but STRICT: it forces the spine of `xs` and every result, so it is unusable on an infinite or partially-consumed list and raises an error that `map` would have left unevaluated. `f` must be pure and return serialisable data |
| `par-sum(f, xs)` | EXPERIMENTAL (macOS/Linux only, semantics may change without notice). `par-sum(f, xs)` - parallel sum of `f(x)` over `xs`. Same value as `xs map(f) sum`; strict in the same way as par-map |
| `par-max(f, xs)` | EXPERIMENTAL (macOS/Linux only, semantics may change without notice). `par-max(f, xs)` - parallel max of `f(x)` over `xs`. Same value as `xs map(f) max-of`; strict in the same way as par-map |
| `par-min(f, xs)` | EXPERIMENTAL (macOS/Linux only, semantics may change without notice). `par-min(f, xs)` - parallel min of `f(x)` over `xs`. Same value as `xs map(f) min-of`; strict in the same way as par-map |
| `par-concat(f, xs)` | EXPERIMENTAL (macOS/Linux only, semantics may change without notice). `par-concat(f, xs)` - parallel map-then-concat. Same value as `xs map(f) concat`; strict in the same way as par-map |
