# Replace Moniker Dependency with Custom Binding Implementation

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove the `moniker` git dependency (unmaintained, pinned to curvelogic fork) and replace it with a minimal, purpose-built binding module that preserves the same scoping semantics with simpler types.

**Bead:** eu-uex5

**Motivation:** `moniker` is abandoned upstream (last release 2018, Rust 2015 edition). The eucalypt fork pins a specific rev. The crate provides generic binding machinery, but eucalypt only uses it with `String` names. The wrapper types (`Binder`, `Embed`, `Rec`) and the `BoundTerm` derive macro add complexity without benefit beyond what a focused implementation provides.

**Strategy:** Keep the `Scope` concept and Free/Bound variable distinction (they do real work), but strip away the generic machinery and newtype wrappers. The binding semantics are preserved; only the representation changes.

**Tech Stack:** Rust

---

## Type Mapping (before → after)

| Moniker type | Replacement | Rationale |
|---|---|---|
| `FreeVar<String>` | `String` | Unique ID is unused; name is the source of truth |
| `BoundVar<String>` | `BoundVar` (own type: `scope: u32, binder: u32, name: Option<String>`) | Keep pretty name for debugging, drop generic |
| `Var<String>` | `Var` (own enum: `Free(String)`, `Bound(BoundVar)`) | Same semantics, no generic |
| `Binder<String>` | `String` | Just a newtype marker for moniker's traversal — unnecessary without `BoundTerm` |
| `Embed<T>` | `T` | Same — marker for moniker's traversal |
| `Rec<T>` | `T` | Same — all lets are recursive by convention; no need to mark |
| `Scope<P, B>` | `Scope<P, B>` (own struct: `pattern: P`, `body: B`) | Keep the concept, drop `unsafe_` prefix, implement `new()` and `unbind()` ourselves |
| `LetScope<T>` | `Scope<Vec<(String, T)>, T>` | Was `Scope<Rec<Vec<(Binder<String>, Embed<T>)>>, T>` — three wrappers removed |
| `LamScope<T>` | `Scope<Vec<String>, T>` | Was `Scope<Vec<Binder<String>>, T>` — one wrapper removed |
| `ScopeOffset` | `u32` | Plain integer |
| `BinderIndex` | `u32` | Plain integer |
| `BoundTerm<String>` trait | Removed | Replace derived traversals with explicit `close`/`open` functions |
| `assert_term_eq!` macro | `assert_eq!` | With deterministic names, structural equality = binding equality |
| `impl_bound_term_ignore!` macro | Removed | No longer needed |

---

## Chunk 1: Create the `binding` Module

### Task 1: Implement custom binding types and scope operations

**Files:**
- Create: `src/core/binding.rs`
- Modify: `src/core/mod.rs` (register module)

- [ ] **Step 1: Define core types**

```rust
/// A variable reference — either free (by name) or bound (de Bruijn indexed)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Var {
    Free(String),
    Bound(BoundVar),
}

/// A bound variable with de Bruijn indices
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BoundVar {
    /// How many enclosing scopes to skip (0 = innermost)
    pub scope: u32,
    /// Which binding within that scope (0-indexed)
    pub binder: u32,
    /// Original name, preserved for debugging and pretty-printing
    pub name: Option<String>,
}

/// A scope binding pattern `P` over body `B`
///
/// Construction via `Scope::new()` converts matching free variables
/// in the body to bound variables. Access via `.pattern` and `.body`
/// is direct — no "unsafe" prefix.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope<P, B> {
    pub pattern: P,
    pub body: B,
}
```

- [ ] **Step 2: Implement `Scope::new()` for `LetScope`**

`Scope::new(bindings: Vec<(String, T)>, body: T) -> Scope<Vec<(String, T)>, T>` where `T` is the expression type.

This must:
1. Collect the binding names from the pattern
2. Walk the body expression, converting any `Var::Free(name)` that matches a binding name into `Var::Bound(BoundVar { scope: 0, binder: index, name: Some(name) })`
3. Also walk the binding values themselves (since lets are recursive, binding values can reference other bindings in the same let)
4. Increment the `scope` field of any existing `BoundVar` references (they now have one more enclosing scope)

This is a recursive tree walk over `Expr<RcExpr>`. Implement as a method or free function taking an `RcExpr`.

- [ ] **Step 3: Implement `Scope::new()` for `LamScope`**

Similar but simpler — the pattern is just `Vec<String>` (no embedded values to walk).

- [ ] **Step 4: Implement `unbind()`**

`scope.unbind() -> (P, B)` — the inverse of `new()`:
1. Walk the body, converting any `Var::Bound` with `scope == 0` back to `Var::Free(name)` using the pattern names
2. Decrement the `scope` of any `BoundVar` with `scope > 0`
3. Return `(pattern, body)` with free variables restored

Only 5 call sites use this, so it can be straightforward.

- [ ] **Step 5: Implement helper functions**

- `free_vars(expr: &RcExpr) -> HashSet<String>` — collect all `Var::Free` names (replaces moniker's `free_vars()` method on `BoundTerm`)
- `succ(expr: &RcExpr) -> RcExpr` — increment all `BoundVar` scope offsets by 1 (replaces `transform/succ.rs` which currently does this via moniker's visitor)

- [ ] **Step 6: Unit tests**

Test round-tripping: `Scope::new()` then `unbind()` should recover the original names. Test that binding converts the right variables and leaves others free. Test nested scopes.

**Verification:** `cargo test --lib` for the new module.

---

## Chunk 2: Update `expr.rs` — Core Expression Types

### Task 2: Replace moniker types in expression definitions

**Files:**
- Modify: `src/core/expr.rs`

This is the largest single change — expr.rs has 51 `unsafe_pattern`/`unsafe_body` accesses and defines the type aliases.

- [ ] **Step 1: Replace imports and type aliases**

Remove `use moniker::*;` and the `impl_bound_term_ignore!` macro.

Replace type aliases:
```rust
// Before
pub type LetScope<T> = Scope<Rec<Vec<(Binder<String>, Embed<T>)>>, T>;
pub type LamScope<T> = Scope<Vec<Binder<String>>, T>;

// After
pub type LetScope<T> = Scope<Vec<(String, T)>, T>;
pub type LamScope<T> = Scope<Vec<String>, T>;
```

- [ ] **Step 2: Remove `BoundTerm` constraint from `Expr<T>`**

Change `Expr<T> where T: BoundTerm<String>` to just `Expr<T>` (or `Expr<T: Clone>` if needed). Remove `#[derive(BoundTerm)]` from `Expr`, `RcExpr`, `RcFatExpr`, `Fixity`.

- [ ] **Step 3: Replace all `unsafe_pattern` → `pattern` and `unsafe_body` → `body`**

This is a bulk find-and-replace across the file. Also replace:
- `Rec::new(bindings)` → `bindings`
- `.unrec()` → remove (it was just unwrapping)
- `Rec { unsafe_pattern: ... }` → just the vec directly
- `(Binder(name), Embed(value))` → `(name, value)` in patterns
- `Binder(fv)` → `fv` (just a string now)

- [ ] **Step 4: Update constructor helpers (`core::` and `acore::` modules)**

Update `lam()`, `inline()`, `default_let()`, `let_()` etc. to use the new `Scope::new()` from binding.rs. The signatures change: `Vec<FreeVar<String>>` → `Vec<String>`.

- [ ] **Step 5: Replace `assert_term_eq!` with `assert_eq!`**

In expr.rs tests. With deterministic names, structural equality works.

- [ ] **Step 6: Update the `bound()` function**

This function (line 1744) wraps free vars in a lambda for alpha-equivalence testing. With name-based identity, this can be simplified or removed if `assert_eq!` suffices.

**Verification:** `cargo check` — types should all align. Tests deferred until consumer files are also updated.

---

## Chunk 3: Update Core Pipeline Passes

### Task 3: Update desugaring

**Files:**
- Modify: `src/core/desugar/rowan_ast.rs`
- Modify: `src/core/desugar/desugarer.rs`

- [ ] **Step 1: Replace moniker imports with `crate::core::binding::*`**

- [ ] **Step 2: Replace `FreeVar::fresh_named("x")` with just `"x".to_string()`**

All variable creation becomes plain string construction. This is a significant simplification — no more unique ID generation.

- [ ] **Step 3: Replace pattern matching on `Binder`/`Embed`/`Rec`**

`(Binder(fv), Embed(value))` → `(name, value)` throughout.

- [ ] **Step 4: Replace `Scope::new()` calls**

Use the new `Scope::new()` from binding.rs. The call sites should be nearly identical, just with simpler argument types.

**Verification:** `cargo check`

### Task 4: Update cook (fixity resolution)

**Files:**
- Modify: `src/core/cook/fixity.rs`
- Modify: `src/core/cook/mod.rs`
- Modify: `src/core/cook/fill.rs`

- [ ] **Step 1: Same pattern as Task 3** — replace imports, unwrap newtypes, update `Scope::new()` and `.unbind()` calls.

- [ ] **Step 2: Update tests** — `assert_term_eq!` → `assert_eq!`, `FreeVar::fresh_named()` → string literals.

**Verification:** `cargo test` for cook module.

### Task 5: Update simplification

**Files:**
- Modify: `src/core/simplify/compress.rs`
- Modify: `src/core/simplify/prune.rs`

- [ ] **Step 1: Replace moniker types**

These files are the heaviest `unsafe_pattern`/`unsafe_body` users. The replacements are mechanical: drop "unsafe_" prefix, unwrap `Rec`/`Binder`/`Embed`.

- [ ] **Step 2: Replace `BoundVar` field access**

`bound_var.scope.0` → `bound_var.scope`, `bound_var.binder.to_usize()` → `bound_var.binder as usize`.

- [ ] **Step 3: Replace `OnFreeFn`/`OnBoundFn`/`ScopeState`** in prune.rs

These moniker traits are used for custom binding traversals. Replace with direct pattern matching on the expression tree — likely cleaner than the visitor-based approach.

- [ ] **Step 4: Remove `impl_bound_term_ignore!(Tracker)`**

The `Tracker` type in prune.rs won't need this.

**Verification:** `cargo test` for simplify module.

### Task 6: Update inline/reduce and inline/tag

**Files:**
- Modify: `src/core/inline/reduce.rs`
- Modify: `src/core/inline/tag.rs`

- [ ] **Step 1: Replace moniker types, same mechanical pattern**

- [ ] **Step 2: Update `substs_depth()`** in reduce.rs

This function does depth-aware substitution using de Bruijn indices. The logic stays the same, but `ScopeOffset` → `u32` and `BinderIndex` → `u32`.

**Verification:** `cargo test` for inline module.

### Task 7: Update transforms, verify, export

**Files:**
- Modify: `src/core/transform/succ.rs`
- Modify: `src/core/transform/dynamise.rs`
- Modify: `src/core/verify/binding.rs`
- Modify: `src/core/export/pretty.rs`
- Modify: `src/core/export/embed.rs`
- Modify: `src/core/metadata.rs`
- Modify: `src/common/sourcemap.rs`

- [ ] **Step 1: Replace moniker types in all files**

Same mechanical pattern. `succ.rs` is particularly simple — it already does manual de Bruijn increment; just change the types.

- [ ] **Step 2: Remove `BoundTerm` impl for `Smid`** in sourcemap.rs

Smid doesn't participate in binding — just remove the impl entirely.

- [ ] **Step 3: Update verify/binding.rs**

Replace `Binder(fv)` patterns with plain string access. Replace `bound_var.scope.0` with `bound_var.scope`. The verifier logic is unchanged.

**Verification:** `cargo check`

### Task 8: Update STG compiler and imports

**Files:**
- Modify: `src/eval/stg/compiler.rs`
- Modify: `src/import/yaml.rs`
- Modify: `src/import/jsonl.rs`
- Modify: `src/core/anaphora.rs`

- [ ] **Step 1: Replace moniker types in compiler.rs**

The compiler accesses `unsafe_pattern`/`unsafe_body` and matches on `Var::Bound`/`Var::Free`. Mechanical replacement.

- [ ] **Step 2: Update import files**

yaml.rs and jsonl.rs use moniker types in test helpers. Replace `FreeVar::fresh_named()` with strings.

- [ ] **Step 3: Update anaphora.rs**

Replace `FreeVar<String>` with `String`, `Binder<String>` with `String`. The `to_binding_pattern` function simplifies significantly.

**Verification:** `cargo check`

---

## Chunk 4: Remove Moniker Dependency

### Task 9: Clean up

**Files:**
- Modify: `Cargo.toml`
- Modify: `Cargo.lock`

- [ ] **Step 1: Remove `moniker` from `Cargo.toml`**

Delete the `moniker = { git = "..." }` line.

- [ ] **Step 2: Verify no remaining moniker imports**

`grep -r "moniker" src/` should return nothing.

- [ ] **Step 3: Full build and test**

`cargo build && cargo test` — everything should pass with zero moniker references.

- [ ] **Step 4: Run clippy**

`cargo clippy --all-targets -- -D warnings` — ensure no new warnings from the refactor.

**Verification:** Full CI-equivalent check passes. `cargo tree` no longer shows moniker.

---

## Implementation Order

1. **Chunk 1** — create binding module (can be developed independently, no existing code changes)
2. **Chunk 2** — update expr.rs (foundational — all other files depend on these types)
3. **Chunk 3** — update all consumer files (can be split across sub-tasks but must all land together with Chunk 2)
4. **Chunk 4** — remove dependency (final cleanup)

**Note:** Chunks 2 and 3 are effectively one atomic change — the type aliases in expr.rs are used everywhere, so all consumers must be updated simultaneously. The split into tasks is for organising the work, not for incremental merging. This should be a single PR.

## Risk Assessment

- **Low risk:** The refactor is mechanical — same semantics, simpler types. Each `unsafe_pattern` → `pattern` replacement is trivial.
- **Medium risk:** The `Scope::new()` reimplementation must produce identical de Bruijn indices to moniker's version. Mitigation: the binding verifier (`verify/binding.rs`) will catch any discrepancy, and the full test suite exercises complex binding scenarios.
- **Low risk:** Dropping `FreeVar`'s unique ID could cause name collisions if two different variables share a name. Mitigation: the desugarer already generates unique names (e.g. `__ana_0`, `__destructure_1`), and `Scope::new()` only binds names that match the pattern — shadowing is handled correctly by de Bruijn indexing.

## Opportunities

- **Simpler type signatures:** `LetScope<T>` shrinks from 4 nested generics to 2. Every pattern match becomes cleaner. This improves readability across all 22 files.
- **Faster compilation:** Removing the `moniker` proc macro (`#[derive(BoundTerm)]`) eliminates a proc-macro dependency and its compile-time cost.
- **No git dependency:** Removes the only git-pinned dependency in Cargo.toml, simplifying builds and making `cargo publish` possible in future.
- **Foundation for future work:** A purpose-built binding module is easier to extend (e.g. adding binding metadata, optimising traversals) than forking an abandoned generic library.
