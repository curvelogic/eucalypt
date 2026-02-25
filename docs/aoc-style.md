# Eucalypt AoC Style Guide

Idioms and conventions for AoC solutions in eucalypt.

## List access

- **`head`/`tail`** for list decomposition (variable-length, processing elements sequentially)
- **`first`/`second`/`!! n`** for positional access into fixed-size tuples or records

Don't mix idioms on the same value. If you use `second(xs)`, use `first(xs)` not `head(xs)`. If you use `!! 2`, use `!! 0` and `!! 1` not `first` and `second`.

## Conditionals

- Prefer `<bool> then(a, b)` over `if(cond, a, b)` for simple conditionals.
- Never combine booleans with `∨` before `then` — it causes runtime errors when a branch returns a function.
- Avoid nesting `then` forms — they become hard to read quickly. If you absolutely must have nested conditionals, use `if(cond, a, b)` for the inner branches.
- Prefer restructuring to eliminate conditionals altogether: use `nil?` guards, `max`, `min`, default values (`min-of-or`, `max-of-or`), etc.

## Pipelines

- Prefer pipeline (catenation) style: `xs head` not `head(xs)`, `xs map(f) filter(g)` not `filter(g, map(f, xs))`.
- Use `;` (compose) for point-free function definitions and embedding in pipelines: `abs ; (+ 1)`.
- Partial application for pipeline steps: `map(area(p))`, `filter(v-spans(y))`.

## Naming

- Predicates end with `?`: `vertical?`, `nil?`, `at-y`.
- Use descriptive names: `make-edges` not `mk-edges`.

## Aggregation

- Prefer prelude functions over explicit recursion: `max-of`, `min-of`, `max-of-by`, `sum`, `product`, `foldl`, `foldr`.
- Prefer `map(f) max-of` or `max-of-by(f)` over recursive max-finding.
- Prefer folds, scans, or specific prelude algorithms over explicit recursion where possible.

## Documentation

- Use backtick string metadata (`` ` "..." ``) not `#` comments for documenting declarations.
- `#` comments are for disabling code, not documentation.
- Doc metadata should be markdown: use backquotes for `param` and `function` names.

## Blocks

- Use blocks for intermediate bindings: `{ x: ... y: ... }.(x + y)`.
- Keep block results in `.(...)` concise.
