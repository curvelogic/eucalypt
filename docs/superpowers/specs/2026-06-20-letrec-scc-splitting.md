# eu-9tah.10: Split LetRec into Let + LetRec via SCC Decomposition

**Date**: 2026-06-20
**Bead**: eu-9tah.10
**Agent**: Quill
**Parent spec**: `2026-06-19-0.10.0-release-design.md`

## Motivation

Eucalypt desugars all block scopes as `LetRec` because the syntax allows any
binding to reference any other. But in practice, the vast majority of bindings
are non-recursive — `x: 1 + 2` doesn't reference `x` in its own RHS.

This has two consequences:

1. **Demand analysis is crippled.** The LetRec fixed-point iteration
   conservatively clamps most bindings to `Multi` because they share a
   recursive scope. A non-recursive binding `x` used once should get
   `AtMostOnce`, but inside a `LetRec` the analysis can't prove it's safe
   without expensive fixed-point iteration — and even then, the taint
   propagation from genuinely recursive siblings forces many non-recursive
   bindings to `Multi`.

2. **The STG compiler emits Thunks for everything.** Without `AtMostOnce`,
   every binding gets an Update frame. The demand analysis pass runs but
   produces no improvement over the prune pass's heuristics.

3. **Inlining opportunities are missed.** The inline pass is conservative
   about bindings in recursive scopes.

## Prior Art

GHC performs dependency analysis immediately after desugaring, splitting
`LetRec` into minimal SCCs (strongly connected components). Non-recursive
bindings become `Let`, genuinely mutually recursive bindings stay as `LetRec`.
This is standard in functional compilers and is one of the highest-leverage
early passes.

## Design

### New core pass: `src/core/dependency.rs`

A pass that runs **early in the pipeline, after desugaring** (before cook or
immediately after cook). For each `LetRec` scope in the core expression:

1. **Build a dependency graph.** For each binding `(name, rhs)`, compute the
   set of free variables in `rhs` that reference other bindings in the same
   scope. Edge: `name → referenced_name`.

2. **Compute SCCs.** Use Tarjan's algorithm or Kosaraju's algorithm to find
   strongly connected components. A single binding with no self-reference is
   a trivial SCC (non-recursive). A group of mutually recursive bindings
   forms a non-trivial SCC.

3. **Topological sort** the SCCs. Non-recursive bindings that depend on
   nothing come first; bindings that depend on earlier ones come next;
   genuinely recursive groups come where their dependencies are satisfied.

4. **Emit `Let` / `LetRec`.** For each SCC in topological order:
   - Trivial SCC (single binding, no self-edge) → `Let`
   - Non-trivial SCC (multiple bindings or self-referential) → `LetRec`

5. **Nest the scopes.** The result is a chain of nested `Let` and `LetRec`
   expressions, innermost body is the original `LetRec`'s body.

### Example

Input (all in one `LetRec`):
```
LetRec {
  x = 1 + 2          -- depends on nothing
  y = x + 3          -- depends on x
  f = \n -> if(n=0, 0, f(n-1))  -- self-recursive
  z = f(y)           -- depends on f, y
} in z
```

After SCC splitting:
```
Let { x = 1 + 2 } in
Let { y = x + 3 } in
LetRec { f = \n -> if(n=0, 0, f(n-1)) } in
Let { z = f(y) } in
z
```

Now demand analysis can assign `AtMostOnce` to `x`, `y`, `z` (if each is used
once) without being dragged to `Multi` by sharing a scope with `f`.

### Pipeline placement

After desugaring, **before cook**. Or after cook — the pass doesn't depend on
operator precedence resolution. The key requirement is that it runs before
the demand analysis pass and before the inline pass, so both benefit from
the refined scope structure.

```
desugar → **dependency split** → cook → hoist → inline → ... → demand → stg
```

### Interaction with existing passes

- **Demand analysis**: directly benefits — `Let` bindings get precise demands
  without LetRec clamping.
- **Inline pass** (`distribute`): may benefit — non-recursive `Let` bindings
  are easier to inline.
- **Prune pass**: already handles both `Let` and `LetRec` — no changes needed.
- **STG compiler**: already compiles both `Let` and `LetRec` — `Let` bindings
  can use the more efficient non-recursive compilation path.

### LetType preservation

The existing `LetType` enum distinguishes `DefaultBlockLet`, `OtherLet`, etc.
When splitting a `LetRec`:
- The inner `LetRec` (for recursive SCCs) keeps the original `LetType`
- Extracted `Let` bindings use `LetType::OtherLet` (they're compiler-generated)
- `DefaultBlockLet` blocks should NOT be split — they represent user-visible
  block values where all bindings must be in scope together. Only split
  `LetType::LetRec` scopes (if such a variant exists) or scopes that are
  genuinely recursive function definitions.

**Important**: study how block scopes vs function scopes are represented.
Not all `LetRec` scopes should be split — only those where the bindings are
independent definitions, not block members that form a record value.

### Handling `DefaultBlockLet`

`DefaultBlockLet` scopes represent block literals `{ x: 1, y: x + 1 }`. These
should **not** be split because:
- The block value needs all bindings in scope to construct the block map
- Splitting would change the semantics (the block body references all bindings)
- The namespace hoisting pass already handles the optimisation case for these

Only split `OtherLet` / non-block `LetRec` scopes — function-level let
bindings where the scope is recursive only because the desugarer emits
`LetRec` by default.

## Acceptance Criteria

1. New pass in `src/core/dependency.rs` splits `LetRec` into `Let` + `LetRec`
2. SCC algorithm correctly identifies recursive vs non-recursive bindings
3. `DefaultBlockLet` scopes are NOT split
4. Topological ordering is correct — bindings only reference earlier bindings
5. `eu dump desugared` (or a new `eu dump split`) shows the refined scope
   structure
6. Demand analysis produces `AtMostOnce` for genuinely single-use non-recursive
   bindings (verify with `eu dump demands`)
7. No behavioural change: full test suite passes
8. `cargo clippy --all-targets -- -D warnings` clean
9. `cargo fmt --all` clean

## Risks

- **Block semantics**: splitting a `DefaultBlockLet` would break block value
  construction. The pass must be conservative about which scopes to split.
- **Performance of the pass**: unlike GHC, eucalypt compiles on every
  execution — pass cost is user-visible latency. Tarjan's SCC is O(V+E),
  and the pass only runs on user code (blob prelude is pre-compiled). User
  programs typically have tens to hundreds of bindings. Current pipeline
  phases (cook, hoist, inline) each take 0.1-0.5ms — the SCC pass must
  stay in that range. Profile with `--statistics` after implementation.
- **Interaction with the prune pass**: the prune pass eliminates dead bindings
  from `LetRec`. After splitting, some dead bindings may be in `Let` scopes
  instead — verify the prune pass handles this.

## Out of Scope

- Splitting at the desugarer level (would require the desugarer to do
  dependency analysis during construction — cleaner as a separate pass)
- Recursive binding group optimisation (e.g. lambda lifting within SCCs)
- Cross-scope dependency analysis (only within individual `LetRec` scopes)
