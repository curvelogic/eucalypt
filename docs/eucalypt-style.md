# Eucalypt Style Guide

Eucalypt is very flexible and allows you to write extremely ugly code.

Here is some general style guidance for writing eucalypt idiomatically.

## List access

- **`head`/`tail`** for list decomposition (variable-length, processing elements sequentially)
- **`first`/`second`/`!! n`** for positional access into fixed-size tuples or records

Don't mix idioms on the same context. If you use `second(xs)`, use `first(xs)` not `head(xs)`. If you use `!! 2`, use `!! 0` and `!! 1` not `first` and `second`.

## Conditionals

- Prefer simple (unnested) conditionals.
- Use `<bool> then(a, b)` over `if(cond, a, b)` for simple
  conditionals.
- If nesting is unavoidable, use `if`, nested `then`s are confusing.
- Never mix `if()` and `then()` in the same expression — it looks like they go together and is confusing. 
- Prefer restructuring to eliminate conditionals altogether: use `nil?` guards, `max`, `min`, default values (`min-of-or`, `max-of-or`), etc.
- **`then(false, x)` and `then(true, x)` are antipatterns.** Refactor to
  disjunctions and conjunctions: `cond ∧ x`, `cond ∨ x`, etc.
  Use block bindings or parentheses to keep `∧`/`∨` separated from
  catenation pipelines:
  ```
  { ok: xs non-nil?
    result: xs map(f) sum
  }.(ok ∧ result > 0)
  ```

## Catenation precedence

Catenation (juxtaposition / pipeline application) has the **lowest** operator precedence (20). ALL infix operators bind tighter, including `∧` (35), `∨` (30), `=` (40), `+` (75), etc. This means infix operators steal adjacent atoms from catenation pipelines:

- `xs f(a) + 1` parses as `xs(f(a) + 1)` — `+` grabs `f(a)` and `1`
- `(k > 0) ∧ xs non-nil?` parses as `((k > 0) ∧ xs) non-nil?` — `∧` grabs `xs`
- `xs tail ++ [0]` parses as `xs(tail ++ [0])` — `++` grabs `tail` and `[0]`

Fix with parens around the catenation: `(k > 0) ∧ (xs non-nil?)`, `(xs tail) ++ [0]`.

## Pipelines

- The clearest function definition is a straight pipeline. Structure programs to maximise them.
- Prefer pipeline (catenation) style: `xs head` not `head(xs)`, `xs map(f) filter(g)` not `filter(g, map(f, xs))`.
- Use `;` (compose) for point-free function definitions and embedding
  in pipelines: `abs ; (+ 1)`. 
- `∘` is also acceptable, particularly in mathematical of strongly FP contexts
- Partial application for pipeline steps: `map(area(p))`, `filter(v-spans(y))`.

## Parameter order

Choose parameter order to support partial application and pipeline style:

- Put the "data" or "collection" parameter **last** so that partially applied functions slot into pipelines: `g remove-node("dac") count-paths("svr")` reads as a pipeline of transformations.
- Put "configuration" or "small" parameters **first** so they can be fixed early: `map(lookup-count(table))`, `foldl(dp-step(g), {}, order)`.
- If a function will be used as a `foldl` accumulator, match the `(acc, elem)` signature: `dfs-topo(g, state, node)` allows `foldl(dfs-topo(g), init, nodes)`.

When in doubt, ask: "how will this function most commonly be called?" and put the varying argument last.

## Naming

- Predicates end with `?`: `vertical?`, `nil?`, `at-y`.
- Use descriptive names: `make-edges` not `mk-edges`.

## Recursion

- Prefer folds, scans, or specific prelude algorithms over explicit recursion where possible.

## Documentation

- Use backtick string metadata (`` ` "..." ``) for documenting declarations. Backtick metadata attaches to the next declaration, so only use it when the comment is specific to that declaration.
- Use `#` comments for inline explanatory notes within blocks (e.g. section separators, notes that apply to a group of bindings rather than one declaration) and for disabling code.
- Doc metadata should be markdown: use backquotes for `param` and `function` names.

## Blocks

- Use blocks for local bindings: `{ x: ... y: ... }.(x + y)`, but limit to one block, do not stack this construct
- Keep block "results"" in `.(...)` concise, preferably simple pipelines or expressions, or even just `{...}.result`
- **Dynamic generalised lookup**: a function can return a block whose names are then used as a namespace for subsequent pipelines, e.g. `prepare(data).( edges take(k) ... )`. Use very sparingly — it can defeat static analysis. Never nest or stack dynamic lookups.
